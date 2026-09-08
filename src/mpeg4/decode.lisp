;;;; mpeg4/decode.lisp — start codes in, pictures out.
;;;;
;;;; MPEG-4's reorder is MPEG-2's: a reference picture is held until the next reference picture is
;;;; finished, and every B picture is handed out at once.  What is different is that a picture may
;;;; not be there at all — `vop_coded' of zero means "show the last one again", which is how a
;;;; static shot costs a handful of bits per frame — and a decoder that treats that as an error, or
;;;; as a picture, gets the frame count wrong in either direction.

(in-package #:reel.mpeg4)

(defstruct (decoder (:conc-name d-))
  vol
  last next
  (out '())
  (frames 0 :type fixnum)
  (display 0 :type fixnum))

(defun decoder-width (d) (and (d-vol d) (vol-width (d-vol d))))
(defun decoder-height (d) (and (d-vol d) (vol-height (d-vol d))))

(defun %emit (d frame)
  (when frame
    (incf (d-display d))
    (push frame (d-out d))))

(defun %retire (d frame)
  "Keep the picture just decoded if it is a reference, and hand out whatever its arrival released."
  (incf (d-frames d))
  (if (= (fr-coding-type frame) +vop-b+)
      (%emit d frame)
      (progn (%emit d (d-next d))
             (setf (d-last d) (d-next d) (d-next d) frame))))

(defun flush-decoder (d)
  "Hand out everything still held: a stream always ends with one reference picture undisplayed."
  (%emit d (d-next d))
  (setf (d-next d) nil (d-last d) nil)
  (let ((o (nreverse (d-out d)))) (setf (d-out d) '()) o))

(defun feed-bytes (d bytes &key (start 0) (end (length bytes)))
  "Decode every start code in BYTES.  Returns the pictures completed, in display order."
  (declare (type octets bytes))
  (let ((i (find-start-code bytes start)))
    (loop while (and i (< (+ i 4) end))
          do (let* ((code (aref bytes (+ i 3)))
                    (next (or (find-start-code bytes (+ i 4)) end))
                    (payload (+ i 4)))
               (cond
                 ((<= #x20 code #x2f)
                  (setf (d-vol d) (parse-vol-header (make-br bytes :start payload :end next))))
                 ((= code +sc-vop+)
                  (unless (d-vol d) (%err "a picture arrived before any video object layer"))
                  (let* ((br (make-br bytes :start payload :end next))
                         (p (parse-vop-header br (d-vol d))))
                    (cond
                      ((not (vop-coded p))
                       ;; NOT CODED means show the previous picture again.  It is a picture in the
                       ;; output and no bits at all in the stream.
                       (when (d-next d)
                         (%retire d (%repeat-frame (d-next d) (vop-coding-type p)))))
                      (t
                       (let* ((cur (make-frame-for (d-vol d)))
                              (fwd (if (= (vop-coding-type p) +vop-b+) (d-last d) (d-next d)))
                              (bwd (and (= (vop-coding-type p) +vop-b+) (d-next d))))
                         (when (= (vop-coding-type p) +vop-b+)
                           (%err "B-VOPs are not supported yet"))
                         (when (= (vop-coding-type p) +vop-s+)
                           (%err "sprite VOPs are not supported"))
                         (setf (fr-coding-type cur) (vop-coding-type p))
                         (let ((st (make-state (d-vol d) p cur fwd bwd)))
                           (setf (st-br st) br)
                           (decode-vop st))
                         (%retire d cur))))))
                 (t nil))
               (setf i (if (< next end) next nil))))
    (let ((o (nreverse (d-out d)))) (setf (d-out d) '()) o)))

(defun %repeat-frame (f type)
  "A picture the stream declined to code: the last one, again, as a picture of its own."
  (let ((c (copy-frame f)))
    (setf (fr-coding-type c) type)
    c))

(defun decode-elementary-stream (bytes)
  "Decode a whole MPEG-4 Part 2 elementary stream into a list of pictures, in display order."
  (let* ((d (make-decoder))
         (a (feed-bytes d (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
         (b (flush-decoder d)))
    (append a b)))

;;; ---- output --------------------------------------------------------------------------------

(defun picture->yuv420 (f)
  "One frame as planar I420 at its displayed size."
  (let* ((w (fr-width f)) (h (fr-height f))
         (cw (ceiling w 2)) (ch (ceiling h 2))
         (out (make-array (+ (* w h) (* 2 cw ch)) :element-type '(unsigned-byte 8)))
         (o 0))
    (dotimes (y h)
      (replace out (fr-y f) :start1 o :start2 (* y (fr-ystride f)) :end2 (+ (* y (fr-ystride f)) w))
      (incf o w))
    (dolist (p (list (fr-u f) (fr-v f)))
      (dotimes (y ch)
        (replace out p :start1 o :start2 (* y (fr-cstride f)) :end2 (+ (* y (fr-cstride f)) cw))
        (incf o cw)))
    out))

(defun as-picture (f)
  "This decoder's frame as a REEL.DECODE:PICTURE, sharing planes rather than copying them."
  (reel.decode::%make-shared-picture
   :width (fr-width f) :height (fr-height f)
   :y (fr-y f) :u (fr-u f) :v (fr-v f)
   :y-stride (fr-ystride f) :uv-stride (fr-cstride f)
   :y-offset 0 :uv-offset 0))

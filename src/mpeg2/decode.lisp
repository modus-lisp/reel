;;;; mpeg2/decode.lisp — start codes in, pictures out.
;;;;
;;;; THE REORDER IS THE WHOLE OF THE TOP LEVEL.  MPEG-2 codes pictures out of display order — a
;;;; reference picture is transmitted BEFORE the B pictures that sit in front of it on screen — and a
;;;; decoder that hands pictures out as it finishes them plays the sequence with every group of
;;;; pictures shuffled.  The rule is one line long: a reference picture is held until the NEXT
;;;; reference picture is finished, and every B picture is handed out at once.
;;;;
;;;; There is no picture order count here and none is needed.  H.264 has one because its reordering
;;;; can be arbitrarily deep; MPEG-2's is exactly one reference deep by construction, so two slots
;;;; and a rule replace the whole reorder buffer.

(in-package #:reel.mpeg2)

(defstruct (decoder (:conc-name d-))
  seq ph
  cur                                           ; the picture being decoded
  last next                                     ; the two reference pictures
  ss                                            ; the slice state, reused
  (out '())                                     ; finished pictures, in DISPLAY order
  (frames 0 :type fixnum)
  (display 0 :type fixnum))

(defun decoder-width (d) (and (d-seq d) (seq-width (d-seq d))))
(defun decoder-height (d) (and (d-seq d) (seq-height (d-seq d))))

(defun %emit (d frame)
  (when frame
    (setf (fr-timestamp frame)
          (let ((r (and (d-seq d) (seq-frame-rate (d-seq d)))))
            (if (and r (plusp r)) (/ (d-display d) r) nil)))
    (incf (d-display d))
    (push frame (d-out d))))

(defun %finish-picture (d)
  "Retire the picture just decoded: keep it if it is a reference, and hand out whatever its arrival
   released."
  (let ((cur (d-cur d)))
    (when cur
      (incf (d-frames d))
      (if (= (fr-coding-type cur) +pic-b+)
          (%emit d cur)                          ; B pictures are never references and never wait
          (progn (%emit d (d-next d))            ; the reference this one displaces
                 (setf (d-last d) (d-next d)
                       (d-next d) cur))))
    (setf (d-cur d) nil (d-ph d) nil)))

(defun flush-decoder (d)
  "Hand out everything still held.  A stream ends with one reference picture undisplayed, always."
  (%finish-picture d)
  (%emit d (d-next d))
  (setf (d-next d) nil (d-last d) nil)
  (let ((o (nreverse (d-out d)))) (setf (d-out d) '()) o))

(defun %start-picture (d)
  "Allocate the frame this picture will be decoded into, now that its extensions have arrived."
  (let* ((seq (d-seq d)) (ph (d-ph d)))
    (unless seq (%err "a picture arrived before any sequence header"))
    (unless (= 3 (ph-structure ph))
      (%err "field pictures are not supported (picture_structure ~d)" (ph-structure ph)))
    (let ((f (make-frame-for seq)))
      (setf (fr-coding-type f) (ph-coding-type ph)
            (fr-temporal-reference f) (ph-temporal-reference ph))
      (setf (d-cur d) f)
      (setf (d-ss d)
            (make-slice-state :seq seq :ph ph :cur f
                              :forward (if (= (ph-coding-type ph) +pic-b+) (d-last d) (d-next d))
                              :backward (and (= (ph-coding-type ph) +pic-b+) (d-next d))))
      f)))

(defun feed-bytes (d bytes &key (start 0) (end (length bytes)))
  "Decode every start code in BYTES.  Returns the pictures completed, in display order.

   The unit here is a whole buffer rather than one NAL as in H.264, because MPEG-2 has no framing
   above the start code: a picture is however many slices lie between one picture_start_code and
   the next, and finding that out means scanning forwards regardless."
  (declare (type octets bytes))
  (let ((i (find-start-code bytes start)))
    (loop while (and i (< (+ i 4) end))
          do (let* ((code (aref bytes (+ i 3)))
                    (next (or (find-start-code bytes (+ i 4)) end))
                    (payload (+ i 4)))
               (cond
                 ((= code +sc-sequence+)
                  (setf (d-seq d) (parse-sequence-header (make-br bytes :start payload :end next))))
                 ((= code +sc-extension+)
                  (let* ((br (make-br bytes :start payload :end next))
                         (id (read-bits br 4)))
                    (case id
                      (1 (when (d-seq d) (parse-sequence-extension br (d-seq d))))
                      (3 (when (d-seq d) (parse-quant-matrix-extension br (d-seq d))))
                      (8 (when (d-ph d) (parse-picture-coding-extension br (d-ph d))))
                      (t nil))))                 ; display, copyright, scalability: nothing to do
                 ((= code +sc-picture+)
                  (%finish-picture d)
                  (setf (d-ph d) (parse-picture-header
                                  (make-br bytes :start payload :end next))))
                 ((= code +sc-group+) nil)       ; the time code, which nothing here needs
                 ((= code +sc-sequence-end+) (%finish-picture d))
                 ((slice-start-code-p code)
                  (unless (d-cur d) (%start-picture d))
                  (decode-slice (d-ss d) (1- code) bytes payload next))
                 (t nil))
               (setf i (if (< next end) next nil))))
    (let ((o (nreverse (d-out d)))) (setf (d-out d) '()) o)))

(defun decode-elementary-stream (bytes)
  "Decode a whole MPEG-1 or MPEG-2 video elementary stream into a list of pictures, in display
   order.  This is what a `.m2v' or `.mpv' file is, and what a demuxer hands over."
  (let* ((d (make-decoder))
         (a (feed-bytes d bytes))
         (b (flush-decoder d)))
    (append a b)))

;;; ---- output --------------------------------------------------------------------------------

(defun picture->yuv420 (f)
  "One frame as planar I420 at its DISPLAYED size, which is what ffmpeg writes for `-f rawvideo'."
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
  "This decoder's frame as a REEL.DECODE:PICTURE, so a caller has one picture type whichever codec
   produced it.  The planes are SHARED, not copied — the same lifetime rule VP8 and H.264 use."
  (reel.decode::%make-shared-picture
   :width (fr-width f) :height (fr-height f)
   :y (fr-y f) :u (fr-u f) :v (fr-v f)
   :y-stride (fr-ystride f) :uv-stride (fr-cstride f)
   :y-offset 0 :uv-offset 0))

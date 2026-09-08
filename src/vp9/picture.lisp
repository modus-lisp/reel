;;;; vp9/picture.lisp — the decoded picture.
;;;;
;;;; Ahead of everything that reads it, for the reason the H.264 decoder learned the hard way: a
;;;; caller compiled before the DEFSTRUCT is seen cannot inline a slot access and does not know what
;;;; type comes back, and every sample the decoder writes goes through one of these.
;;;;
;;;; The planes are aligned up to whole SUPERBLOCKS rather than to the picture, so that a 64x64 block
;;;; at the right or bottom edge always has room and no part of the decoder needs a special case for
;;;; the last column.  The samples in that margin are never read: the edge gatherer knows how many of
;;;; the row above are real and repeats the last one rather than reading past it.

(in-package #:reel.vp9)

(defstruct (frame (:conc-name fr-))
  (width 0 :type fixnum) (height 0 :type fixnum)
  (planes (vector (%o 0) (%o 0) (%o 0)) :type simple-vector)
  (stride (make-array 3 :element-type 'fixnum) :type (simple-array fixnum (3)))
  (pheight (make-array 3 :element-type 'fixnum) :type (simple-array fixnum (3)))
  (timestamp nil))

(defun make-frame-for (h)
  "A picture buffer aligned up to whole superblocks, so that a 64x64 block at the edge always fits
   and no part of the decoder needs an out-of-line case for the last column."
  (let* ((sbw (ash (+ (h-width h) 63) -6)) (sbh (ash (+ (h-height h) 63) -6))
         (yw (* 64 sbw)) (yh (* 64 sbh))
         (f (make-frame :width (h-width h) :height (h-height h))))
    (setf (aref (fr-planes f) 0) (make-array (* yw yh) :element-type '(unsigned-byte 8)
                                                       :initial-element 0)
          (aref (fr-stride f) 0) yw (aref (fr-pheight f) 0) yh)
    (dotimes (p 2)
      (setf (aref (fr-planes f) (1+ p))
            (make-array (* (ash yw -1) (ash yh -1)) :element-type '(unsigned-byte 8)
                                                    :initial-element 128)
            (aref (fr-stride f) (1+ p)) (ash yw -1)
            (aref (fr-pheight f) (1+ p)) (ash yh -1)))
    f))

(defun picture->yuv420 (f)
  "The picture as tightly packed I420, cropped to the size the frame header declared.

   The crop happens here and nowhere else: the planes are superblock-aligned throughout decoding
   because prediction and the loop filter both need the margin, and the caller wants the picture the
   file says it is."
  (declare (type frame f))
  (let* ((w (fr-width f)) (h (fr-height f))
         (cw (ceiling w 2)) (ch (ceiling h 2))
         (out (make-array (+ (* w h) (* 2 cw ch)) :element-type '(unsigned-byte 8)))
         (o 0))
    (declare (type fixnum w h cw ch o))
    (let ((y (the octets (aref (fr-planes f) 0))) (ys (aref (fr-stride f) 0)))
      (dotimes (r h)
        (replace out y :start1 o :end1 (+ o w) :start2 (* r ys) :end2 (+ (* r ys) w))
        (incf o w)))
    (dotimes (p 2)
      (let ((c (the octets (aref (fr-planes f) (1+ p)))) (cs (aref (fr-stride f) (1+ p))))
        (dotimes (r ch)
          (replace out c :start1 o :end1 (+ o cw) :start2 (* r cs) :end2 (+ (* r cs) cw))
          (incf o cw))))
    out))

(defun as-picture (f)
  "This decoder's frame as a REEL.DECODE:PICTURE, sharing planes rather than copying them.

   The picture carries the DISPLAY size and the plane's own stride, so the superblock alignment the
   decoder needs never leaves this file — a caller sees a 176x144 picture whose rows happen to be
   192 samples apart."
  (declare (type frame f))
  (reel.decode::%make-shared-picture
   :width (fr-width f) :height (fr-height f)
   :y (aref (fr-planes f) 0) :u (aref (fr-planes f) 1) :v (aref (fr-planes f) 2)
   :y-stride (aref (fr-stride f) 0) :uv-stride (aref (fr-stride f) 1)
   :y-offset 0 :uv-offset 0))

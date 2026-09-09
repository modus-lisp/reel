;;;; hevc/mc.lisp — motion compensation: the fractional-sample interpolation filters (8.5.3.3.3).
;;;;
;;;; Luma is quarter-sample with EIGHT taps, against H.264's half-sample six-tap plus a bilinear
;;;; step to reach quarter.  That is the single biggest arithmetic change between the two codecs
;;;; and most of where the compression went: H.264's quarter-pel positions are an average of two
;;;; half-pel ones, so they are blurrier than the half-pel positions either side of them, and an
;;;; encoder can see that in the residual.  HEVC computes all three directly from the same eight
;;;; integer samples, so every position is equally sharp.
;;;;
;;;; Chroma is eighth-sample with four taps, because 4:2:0 chroma is half resolution and a
;;;; quarter-sample luma vector lands on an eighth of a chroma sample.
;;;;
;;;; THE INTERMEDIATE PRECISION IS NORMATIVE.  Everything here produces 14-bit samples, not 8-bit
;;;; ones, and the shift back down happens once at the end — after the two predictions of a
;;;; bi-predicted block have been added together.  Rounding to eight bits first and then averaging
;;;; is the obvious implementation and it is a different picture.

(in-package #:reel.hevc)

(defparameter +luma-filter+
  ;; Table 8-11, indexed by the quarter-sample position 0..3
  (make-array '(4 8) :element-type '(signed-byte 32) :initial-contents
              '((0   0   0  64   0   0  0  0)
                (-1  4 -10  58  17  -5  1  0)
                (-1  4 -11  40  40 -11  4 -1)
                (0   1  -5  17  58 -10  4 -1)))
  "The eight-tap luma filters.  Position 0 is the identity, kept so the code need not branch.")

(defparameter +chroma-filter+
  ;; Table 8-12, indexed by the eighth-sample position 0..7
  (make-array '(8 4) :element-type '(signed-byte 32) :initial-contents
              '((0  64   0   0)
                (-2 58  10  -2)
                (-4 54  16  -2)
                (-6 46  28  -4)
                (-4 36  36  -4)
                (-4 28  46  -6)
                (-2 16  54  -4)
                (-2 10  58  -2)))
  "The four-tap chroma filters.")

(declaim (type (simple-array (signed-byte 32) (4 8)) +luma-filter+))
(declaim (type (simple-array (signed-byte 32) (8 4)) +chroma-filter+))

(declaim (inline %clamp))
(defun %clamp (v lo hi) (declare (type fixnum v lo hi)) (max lo (min hi v)))

(defun %mc-luma (dst w h ref stride rw rh x0 y0 mvx mvy)
  "One luma prediction block, at 14-bit precision, into DST (W by H, row-major).

   The vector is in quarter samples: its low two bits select the filter and the rest the integer
   position.  Reference samples outside the picture are the nearest edge sample repeated, which is
   what lets a vector point off the picture at all — the standard says to clamp the COORDINATE, not
   to pad the buffer, so there is no border to maintain and no way to read the wrong memory."
  (declare (type (simple-array (signed-byte 32) (*)) dst)
           (type (simple-array (unsigned-byte 8) (*)) ref)
           (type fixnum w h stride rw rh x0 y0 mvx mvy)
           (optimize (speed 3) (safety 1)))
  (let* ((fx (logand mvx 3)) (fy (logand mvy 3))
         (ix (+ x0 (ash mvx -2))) (iy (+ y0 (ash mvy -2))))
    (declare (type fixnum fx fy ix iy))
    (macrolet ((r (x y) `(aref ref (+ (* (%clamp ,y 0 (1- rh)) stride) (%clamp ,x 0 (1- rw))))))
      (cond
        ;; ---- integer position: nothing to interpolate, only to scale
        ((and (zerop fx) (zerop fy))
         (dotimes (y h)
           (dotimes (x w)
             (setf (aref dst (+ (* y w) x)) (ash (r (+ ix x) (+ iy y)) 6)))))
        ;; ---- horizontal only
        ((zerop fy)
         (dotimes (y h)
           (dotimes (x w)
             (let ((s 0))
               (declare (type fixnum s))
               (dotimes (k 8)
                 (incf s (* (aref +luma-filter+ fx k) (r (+ ix x k -3) (+ iy y)))))
               (setf (aref dst (+ (* y w) x)) s)))))
        ;; ---- vertical only
        ((zerop fx)
         (dotimes (y h)
           (dotimes (x w)
             (let ((s 0))
               (declare (type fixnum s))
               (dotimes (k 8)
                 (incf s (* (aref +luma-filter+ fy k) (r (+ ix x) (+ iy y k -3)))))
               (setf (aref dst (+ (* y w) x)) s)))))
        ;; ---- both: horizontal into a tall intermediate, then vertical down it
        (t
         (let ((tmp (make-array (* w (+ h 7)) :element-type '(signed-byte 32))))
           (declare (dynamic-extent tmp))
           (dotimes (y (+ h 7))
             (dotimes (x w)
               (let ((s 0))
                 (declare (type fixnum s))
                 (dotimes (k 8)
                   (incf s (* (aref +luma-filter+ fx k) (r (+ ix x k -3) (+ iy y -3)))))
                 (setf (aref tmp (+ (* y w) x)) s))))
           (dotimes (y h)
             (dotimes (x w)
               (let ((s 0))
                 (declare (type fixnum s))
                 (dotimes (k 8)
                   (incf s (* (aref +luma-filter+ fy k) (aref tmp (+ (* (+ y k) w) x)))))
                 (setf (aref dst (+ (* y w) x)) (ash s -6)))))))))
    dst))

(defun %mc-chroma (dst w h ref stride rw rh x0 y0 mvx mvy)
  "One chroma prediction block, at 14-bit precision.  The vector is in EIGHTHS of a chroma sample:
   a quarter-sample luma vector over half-resolution chroma is an eighth."
  (declare (type (simple-array (signed-byte 32) (*)) dst)
           (type (simple-array (unsigned-byte 8) (*)) ref)
           (type fixnum w h stride rw rh x0 y0 mvx mvy)
           (optimize (speed 3) (safety 1)))
  (let* ((fx (logand mvx 7)) (fy (logand mvy 7))
         (ix (+ x0 (ash mvx -3))) (iy (+ y0 (ash mvy -3))))
    (declare (type fixnum fx fy ix iy))
    (macrolet ((r (x y) `(aref ref (+ (* (%clamp ,y 0 (1- rh)) stride) (%clamp ,x 0 (1- rw))))))
      (cond
        ((and (zerop fx) (zerop fy))
         (dotimes (y h)
           (dotimes (x w)
             (setf (aref dst (+ (* y w) x)) (ash (r (+ ix x) (+ iy y)) 6)))))
        ((zerop fy)
         (dotimes (y h)
           (dotimes (x w)
             (let ((s 0))
               (declare (type fixnum s))
               (dotimes (k 4)
                 (incf s (* (aref +chroma-filter+ fx k) (r (+ ix x k -1) (+ iy y)))))
               (setf (aref dst (+ (* y w) x)) s)))))
        ((zerop fx)
         (dotimes (y h)
           (dotimes (x w)
             (let ((s 0))
               (declare (type fixnum s))
               (dotimes (k 4)
                 (incf s (* (aref +chroma-filter+ fy k) (r (+ ix x) (+ iy y k -1)))))
               (setf (aref dst (+ (* y w) x)) s)))))
        (t
         (let ((tmp (make-array (* w (+ h 3)) :element-type '(signed-byte 32))))
           (declare (dynamic-extent tmp))
           (dotimes (y (+ h 3))
             (dotimes (x w)
               (let ((s 0))
                 (declare (type fixnum s))
                 (dotimes (k 4)
                   (incf s (* (aref +chroma-filter+ fx k) (r (+ ix x k -1) (+ iy y -1)))))
                 (setf (aref tmp (+ (* y w) x)) s))))
           (dotimes (y h)
             (dotimes (x w)
               (let ((s 0))
                 (declare (type fixnum s))
                 (dotimes (k 4)
                   (incf s (* (aref +chroma-filter+ fy k) (aref tmp (+ (* (+ y k) w) x)))))
                 (setf (aref dst (+ (* y w) x)) (ash s -6)))))))))
    dst))

(defun %write-uni (plane stride x0 y0 w h src)
  "One prediction, rounded from 14 bits back to eight (8.5.3.3.4.2)."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane)
           (type (simple-array (signed-byte 32) (*)) src)
           (type fixnum stride x0 y0 w h)
           (optimize (speed 3) (safety 1)))
  (dotimes (y h)
    (dotimes (x w)
      (setf (aref plane (+ (* (+ y0 y) stride) x0 x))
            (%clamp (ash (+ (aref src (+ (* y w) x)) 32) -6) 0 255)))))

(defun %write-bi (plane stride x0 y0 w h a b)
  "Two predictions averaged, then rounded once.  Rounding each to eight bits first and averaging
   after is the obvious implementation and it is a different picture — the whole point of carrying
   14 bits out of the filters is that this addition happens before the loss."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane)
           (type (simple-array (signed-byte 32) (*)) a b)
           (type fixnum stride x0 y0 w h)
           (optimize (speed 3) (safety 1)))
  (dotimes (y h)
    (dotimes (x w)
      (setf (aref plane (+ (* (+ y0 y) stride) x0 x))
            (%clamp (ash (+ (aref a (+ (* y w) x)) (aref b (+ (* y w) x)) 64) -7) 0 255)))))

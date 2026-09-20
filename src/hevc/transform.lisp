;;;; hevc/transform.lisp — dequantisation and the inverse transforms (8.6.2 to 8.6.4).
;;;;
;;;; Two one-dimensional passes, columns then rows, with a CLIP TO SIXTEEN BITS between them.  That
;;;; intermediate clip is normative, not an implementation detail: the standard fixes the dynamic
;;;; range at every step so a decoder can use sixteen-bit arithmetic and still be bit-exact, which
;;;; means a decoder that keeps more precision because it can produces different pictures.
;;;;
;;;; The quantiser is a step and a half rather than a table: six multipliers cover one octave and
;;;; the exponent is the quantiser divided by six, so QP + 6 is exactly twice the step.  That is
;;;; H.264's arrangement, and it is the one part of this path HEVC left alone.

(in-package #:reel.hevc)

(defparameter +level-scale+
  (make-array 6 :element-type '(signed-byte 32) :initial-contents '(40 45 51 57 64 72))
  "The six multipliers of one octave of the quantiser (8.6.3).")
(declaim (type (simple-array (signed-byte 32) (6)) +level-scale+))

(defparameter +chroma-qp-table+
  ;; Table 8-10 for 4:2:0: how the chroma quantiser lags luma above 30.  Chroma artefacts are more
  ;; visible than luma ones at the same step, so chroma is held back as luma coarsens.
  (make-array 14 :element-type '(signed-byte 32)
              :initial-contents '(29 30 31 32 33 33 34 34 35 35 36 36 37 37))
  "qPi to QpC for 4:2:0, for qPi in 30..43.")
(declaim (type (simple-array (signed-byte 32) (14)) +chroma-qp-table+))

(defun %chroma-qp (qp-y offset)
  "Qp'C from the luma quantiser and the picture's and slice's chroma offsets (8.6.1)."
  (declare (type fixnum qp-y offset) (optimize (speed 3) (safety 1)))
  (let ((qpi (max 0 (min 57 (+ qp-y offset)))))
    (declare (type fixnum qpi))
    (cond ((< qpi 30) qpi)
          ((> qpi 43) (- qpi 6))
          (t (aref +chroma-qp-table+ (- qpi 30))))))

(defun %dequantise (coeffs n log2-size qp bit-depth &optional matrix dc)
  "Scale the decoded levels back up (8.6.3).

   The shift depends on the block SIZE as well as the bit depth, because each of the two transform
   passes below gains headroom and the scale here is what pays for it.

   MATRIX, when a scaling list is in force, weights each frequency separately.  It is always the
   8x8 list above that size, with each entry covering a 2x2 or 4x4 patch of a 16x16 or 32x32 block
   — a full 32x32 list would be a kilobyte per matrix and the eye does not distinguish that finely
   at high frequency.  DC replaces the corner, which IS sent at full precision."
  (declare (type (simple-array (signed-byte 32) (*)) coeffs)
           (type fixnum n log2-size qp bit-depth)
           (optimize (speed 3) (safety 1)))
  (let* ((shift (+ bit-depth log2-size -5))
         (add (ash 1 (1- shift)))
         (base (ash (aref +level-scale+ (mod qp 6)) (floor qp 6)))
         (size (ash 1 log2-size))
         ;; how many coefficients share one entry of the 8x8 list at this size
         (down (case log2-size (2 0) (3 0) (4 1) (t 2)))
         (row (if (= log2-size 2) 4 8)))
    (declare (type fixnum shift add base size down row))
    (dotimes (i n)
      (let ((v (aref coeffs i)))
        (declare (type fixnum v))
        (unless (zerop v)
          (let* ((x (mod i size)) (y (floor i size))
                 (m (cond ((null matrix) 16)
                          ((and dc (zerop i)) (the fixnum dc))
                          (t (aref (the (simple-array (unsigned-byte 8) (*)) matrix)
                                   (+ (* row (ash y (- down))) (ash x (- down))))))))
            (declare (type fixnum x y m))
            (setf (aref coeffs i)
                  (max -32768 (min 32767 (ash (+ (* v base m) add) (- shift)))))))))))

;;; ---- the one-dimensional transforms -------------------------------------------------------------
;;;
;;; y[i] = sum over j of M[j][i] * x[j] — the TRANSPOSE of the forward matrix, which is what makes
;;; this the inverse.  One 32x32 matrix serves every size: the 16-point transform is its even rows,
;;; the 8-point every fourth, the 4-point every eighth.

(defun %dct-1d (src src-off src-step dst dst-off dst-step size shift)
  (declare (type (simple-array (signed-byte 32) (*)) src dst)
           (type fixnum src-off src-step dst-off dst-step size shift)
           (optimize (speed 3) (safety 1)))
  (let ((add (ash 1 (1- shift)))
        (step (floor 32 size)))
    (declare (type fixnum add step))
    (dotimes (i size)
      (let ((sum 0))
        (declare (type fixnum sum))
        (dotimes (j size)
          (incf sum (* (the fixnum (aref +dct+ (* j step) i))
                       (the fixnum (aref src (+ src-off (* j src-step)))))))
        (setf (aref dst (+ dst-off (* i dst-step)))
              (max -32768 (min 32767 (ash (+ sum add) (- shift)))))))))

(defun %dst-1d (src src-off src-step dst dst-off dst-step shift)
  (declare (type (simple-array (signed-byte 32) (*)) src dst)
           (type fixnum src-off src-step dst-off dst-step shift)
           (optimize (speed 3) (safety 1)))
  (let ((add (ash 1 (1- shift))))
    (declare (type fixnum add))
    (dotimes (i 4)
      (let ((sum 0))
        (declare (type fixnum sum))
        (dotimes (j 4)
          (incf sum (* (the fixnum (aref +dst4+ j i))
                       (the fixnum (aref src (+ src-off (* j src-step)))))))
        (setf (aref dst (+ dst-off (* i dst-step)))
              (max -32768 (min 32767 (ash (+ sum add) (- shift)))))))))

(defun %inverse-transform (coeffs scratch log2-size dst-p bit-depth)
  "The two-dimensional inverse transform, in place in COEFFS (8.6.4.2).

   Columns first at a shift of 7, then rows at 20 - bitDepth.  DST-P selects the DST-VII, which
   only 4x4 intra luma blocks use: an intra residual grows away from the predicted edge rather than
   being flat, and a basis that starts small and ends large fits that better than a cosine."
  (declare (type (simple-array (signed-byte 32) (*)) coeffs scratch)
           (type fixnum log2-size bit-depth)
           (optimize (speed 3) (safety 1)))
  (let ((n (ash 1 log2-size))
        (shift2 (- 20 bit-depth)))
    (declare (type fixnum n shift2))
    ;; element (x,y) lives at y*n + x, so a column has stride n and a row stride 1
    (if dst-p
        (progn
          (dotimes (x n) (%dst-1d coeffs x n scratch x n 7))
          (dotimes (y n) (%dst-1d scratch (* y n) 1 coeffs (* y n) 1 shift2)))
        (progn
          (dotimes (x n) (%dct-1d coeffs x n scratch x n n 7))
          (dotimes (y n) (%dct-1d scratch (* y n) 1 coeffs (* y n) 1 n shift2))))
    coeffs))

(defun %transform-skip (coeffs n log2-size bit-depth)
  "transform_skip: the residual was sent in the sample domain, so only the scaling applies (8.6.2).

   The standard states this as two steps — shift LEFT by 5 + log2(nTbS) to reach the transform's
   output scale, then right by 20 - bitDepth like any other block — and the two collapse to one
   shift of 15 - bitDepth - log2(nTbS).  Doing them separately is not wrong, but doing them
   separately and getting the second one from the wrong place is: a residual four times too large
   looks like a decoder that is nearly right, because it only shows on the blocks that skipped the
   transform and only where their coefficients were not zero."
  (declare (type (simple-array (signed-byte 32) (*)) coeffs)
           (type fixnum n log2-size bit-depth)
           (optimize (speed 3) (safety 1)))
  (let ((shift (- 15 bit-depth log2-size)))
    (declare (type fixnum shift))
    (if (plusp shift)
        (let ((add (ash 1 (1- shift))))
          (declare (type fixnum add))
          (dotimes (i n)
            (setf (aref coeffs i) (ash (+ (aref coeffs i) add) (- shift)))))
        (dotimes (i n)
          (setf (aref coeffs i) (ash (aref coeffs i) (- shift)))))))

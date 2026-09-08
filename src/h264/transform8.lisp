;;;; h264/transform8.lisp — the 8x8 transform, which is High profile's other half.
;;;;
;;;; A macroblock may code its luma residual as four 8x8 blocks instead of sixteen 4x4 ones, and
;;;; says so per macroblock with transform_size_8x8_flag.  Nothing about it reuses the 4x4 path: a
;;;; different scan, a different normalisation table with six position classes rather than three,
;;;; a different butterfly, and a different rounding.  Chroma is unaffected and stays 4x4 always.
;;;;
;;;; The transform is an eight-point butterfly in three stages, applied to rows and then columns.
;;;; The shifts inside it are part of the definition rather than an optimisation: this is an INTEGER
;;;; transform specified exactly, so `>> 1' and `>> 2' on intermediate values are what the encoder
;;;; inverted and there is no freedom to compute them another way.

(in-package #:reel.h264)

(defparameter +flat-scale-8x8+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-element 16)
  "The 8x8 weight matrix a stream that signals none is decoded with.")
(declaim (type (simple-array (unsigned-byte 8) (64)) +flat-scale-8x8+))

(defun dequant-8x8 (coeffs out qp &key (end 63) (weights +flat-scale-8x8+))
  "Dequantise an 8x8 block.  COEFFS is in SCAN order, OUT is filled in RASTER order.

   END is the highest scan position carrying a coefficient, the same economy the 4x4 path uses: a
   block with a handful of low-frequency coefficients leaves most of the scan untouched."
  (declare (type (simple-array fixnum (*)) coeffs out) (type fixnum qp end)
           (type (simple-array (unsigned-byte 8) (64)) weights)
           (optimize (speed 3) (safety 1)))
  (fill out 0)
  (when (minusp end) (return-from dequant-8x8 out))
  (let ((m (mod qp 6)) (e (floor qp 6)))
    (declare (type (integer 0 5) m) (type (integer 0 8) e))
    (loop for scan of-type (integer 0 64) from 0 to (the (integer 0 63) end)
          for c of-type (signed-byte 26) = (aref coeffs scan)
          do (unless (zerop c)
               (let* ((raster (aref +zigzag-8x8+ scan))
                      (row (ash raster -3)) (col (logand raster 7))
                      (class (aref +dequant8-class+ (+ (* 4 (logand row 3)) (logand col 3))))
                      (scale (the (integer 0 16384)
                                  (* (aref weights raster) (aref +dequant8-coeff+ m class)))))
                 (declare (type (integer 0 63) raster) (type fixnum row col))
                 ;; the 8x8 shift pivots at 36 rather than 24: the transform carries six more bits
                 (setf (aref out raster)
                       (if (>= e 6)
                           (ash (* c scale) (- e 6))
                           (ash (+ (* c scale) (ash 1 (- 5 e))) (- (- 6 e))))))))
    out))

(declaim (inline %idct8-line))
(defun %idct8-line (b i0 step)
  "One eight-point pass of the inverse transform, in place, over B starting at I0."
  (declare (type (simple-array fixnum (64)) b) (type fixnum i0 step)
           (optimize (speed 3) (safety 0)))
  (macrolet ((d (k) `(aref b (+ i0 (* ,k step)))))
    (let* ((d0 (d 0)) (d1 (d 1)) (d2 (d 2)) (d3 (d 3))
           (d4 (d 4)) (d5 (d 5)) (d6 (d 6)) (d7 (d 7))
           ;; stage one
           (e0 (+ d0 d4))
           (e1 (+ (- d3) d5 (- d7) (- (ash d7 -1))))
           (e2 (- d0 d4))
           (e3 (+ d1 d7 (- d3) (- (ash d3 -1))))
           (e4 (- (ash d2 -1) d6))
           (e5 (+ (- d1) d7 d5 (ash d5 -1)))
           (e6 (+ d2 (ash d6 -1)))
           (e7 (+ d3 d5 d1 (ash d1 -1)))
           ;; stage two
           (f0 (+ e0 e6)) (f1 (+ e1 (ash e7 -2)))
           (f2 (+ e2 e4)) (f3 (+ e3 (ash e5 -2)))
           (f4 (- e2 e4)) (f5 (- (ash e3 -2) e5))
           (f6 (- e0 e6)) (f7 (- e7 (ash e1 -2))))
      (declare (type fixnum d0 d1 d2 d3 d4 d5 d6 d7
                      e0 e1 e2 e3 e4 e5 e6 e7 f0 f1 f2 f3 f4 f5 f6 f7))
      ;; stage three
      (setf (d 0) (+ f0 f7) (d 1) (+ f2 f5) (d 2) (+ f4 f3) (d 3) (+ f6 f1)
            (d 4) (- f6 f1) (d 5) (- f4 f3) (d 6) (- f2 f5) (d 7) (- f0 f7)))))

(defun idct-8x8 (block)
  "In-place inverse 8x8 transform of a dequantised block, raster order (8.5.13.2).

   Leaves the residual scaled by 64, exactly as the 4x4 transform does, so the caller's
   (+ 32) >> 6 when adding to the prediction is the same in both paths."
  (declare (type (simple-array fixnum (64)) block) (optimize (speed 3) (safety 1)))
  (dotimes (i 8) (%idct8-line block (* i 8) 1))
  (dotimes (j 8) (%idct8-line block j 8))
  block)

(defun add-residual-8x8 (plane stride base block)
  "Add an 8x8 residual to the prediction already sitting in PLANE."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane)
           (type (simple-array fixnum (64)) block)
           (type fixnum stride base)
           (optimize (speed 3) (safety 1)))
  (dotimes (row 8)
    (declare (type fixnum row))
    (let ((o (+ base (* row stride))) (k (* row 8)))
      (declare (type fixnum o k))
      (dotimes (col 8)
        (declare (type fixnum col))
        (setf (aref plane (+ o col))
              (clamp255 (+ (aref plane (+ o col))
                           (ash (+ (aref block (+ k col)) 32) -6))))))))

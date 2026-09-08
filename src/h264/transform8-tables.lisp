;;;; h264/transform8-tables.lisp — GENERATED.  The 8x8 transform's constants, from ITU-T H.264.
;;;;
;;;; The scan order the 8x8 residual arrives in, and the normalisation coefficients its
;;;; dequantisation uses.  The 8x8 transform does not reuse the 4x4 ones: it has its own scan, and
;;;; six position classes rather than three.
;;;;
;;;; Extracted mechanically and checked against properties the specification states apart from the
;;;; numbers: the scan is a permutation of 0..63 running from DC to the highest frequency, the class
;;;; indices are 0..5, and each normalisation column grows with the quantiser step.

(in-package #:reel.h264)

(defparameter +zigzag-8x8+
  ;; scan position -> raster position
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(  0   1   8  16   9   2   3  10  17  24  32  25  18  11   4   5
      12  19  26  33  40  48  41  34  27  20  13   6   7  14  21  28
      35  42  49  56  57  50  43  36  29  22  15  23  30  37  44  51
      58  59  52  45  38  31  39  46  53  60  61  54  47  55  62  63))
  "The 8x8 scan (Table 8-13, frame coding).")

(defparameter +dequant8-class+
  ;; which normalisation column a position uses, by (row mod 4) * 4 + (column mod 4)
  (make-array 16 :element-type '(unsigned-byte 8) :initial-contents
   '( 0  3  4  3  3  1  5  1  4  5  2  5  3  1  5  1)))

(defparameter +dequant8-coeff+
  ;; [qp mod 6][class]: the 8x8 normAdjust
  (make-array '(6 6) :element-type '(unsigned-byte 8) :initial-contents
   '(( 20  18  32  19  25  24)
     ( 22  19  35  21  28  26)
     ( 26  23  42  24  33  31)
     ( 28  25  45  26  35  33)
     ( 32  28  51  30  40  38)
     ( 36  32  58  34  46  43)))
  "Table 8-15's 8x8 companion.")

(declaim (type (simple-array (unsigned-byte 8) (64)) +zigzag-8x8+))
(declaim (type (simple-array (unsigned-byte 8) (16)) +dequant8-class+))
(declaim (type (simple-array (unsigned-byte 8) (6 6)) +dequant8-coeff+))

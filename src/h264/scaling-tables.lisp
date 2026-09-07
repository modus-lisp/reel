;;;; h264/scaling-tables.lisp — GENERATED.  The default scaling matrices of ITU-T H.264.
;;;;
;;;; Tables 7-3 and 7-4, in RASTER order rather than the zig-zag the specification prints them in.
;;;; A stream may signal "use the default" instead of transmitting a list, and a list it omits
;;;; falls back to one of these or to an earlier list, so a decoder cannot do without them.
;;;;
;;;; Extracted mechanically and checked against properties the specification states apart from the
;;;; numbers: the intra and inter 4x4 matrices begin 6 and 10, the 8x8 ones begin 6 and 9, every
;;;; entry is a byte, and all four are symmetric about the diagonal.

(in-package #:reel.h264)

(defparameter +default-scale-4x4+
  ;; [0] intra, [1] inter
  (make-array '(2 16) :element-type '(unsigned-byte 8) :initial-contents
   '((  6  13  20  28  13  20  28  32  20  28  32  37  28  32  37  42)
     ( 10  14  20  24  14  20  24  27  20  24  27  30  24  27  30  34))))

(defparameter +default-scale-8x8+
  (make-array '(2 64) :element-type '(unsigned-byte 8) :initial-contents
   '((  6  10  13  16  18  23  25  27
       10  11  16  18  23  25  27  29
       13  16  18  23  25  27  29  31
       16  18  23  25  27  29  31  33
       18  23  25  27  29  31  33  36
       23  25  27  29  31  33  36  38
       25  27  29  31  33  36  38  40
       27  29  31  33  36  38  40  42)
     (  9  13  15  17  19  21  22  24
       13  13  17  19  21  22  24  25
       15  17  19  21  22  24  25  27
       17  19  21  22  24  25  27  28
       19  21  22  24  25  27  28  30
       21  22  24  25  27  28  30  32
       22  24  25  27  28  30  32  33
       24  25  27  28  30  32  33  35))))

(declaim (type (simple-array (unsigned-byte 8) (2 16)) +default-scale-4x4+))
(declaim (type (simple-array (unsigned-byte 8) (2 64)) +default-scale-8x8+))

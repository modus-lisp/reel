;;;; hevc/scaling-tables.lisp — GENERATED.  The default scaling lists (Tables 7-5 and 7-6).
;;;;
;;;; A scaling list is a per-frequency weighting applied during dequantisation: it lets an encoder
;;;; quantise the coefficients the eye cares least about more coarsely than the ones it notices.
;;;; A stream may send its own, say "use the default", or refer to an earlier one — so a decoder
;;;; cannot do without these even for a stream that transmits nothing.
;;;;
;;;; There is no default 4x4 list: at that size every frequency is weighted 16, which is to say not
;;;; at all.  The 16x16 and 32x32 sizes both use the 8x8 list, upsampled by repeating each entry,
;;;; with the DC coefficient sent separately — a full 32x32 list would be a kilobyte per matrix and
;;;; the eye does not distinguish that finely at high frequency anyway.
;;;;
;;;; In RASTER order, not the diagonal scan the specification prints them in.  Extracted
;;;; mechanically and checked for the property that identifies the ordering: a frequency weighting
;;;; is symmetric about the diagonal, and in raster order these are.

(in-package #:reel.hevc)

(defparameter +default-scaling-intra+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
       16   16   16   16   17   18   21   24
       16   16   16   16   17   19   22   25
       16   16   17   18   20   22   25   29
       16   16   18   21   24   27   31   36
       17   17   20   24   30   35   41   47
       18   19   22   27   35   44   54   65
       21   22   25   31   41   54   70   88
       24   25   29   36   47   65   88  115))
  "The default 8x8 list for intra blocks.")

(defparameter +default-scaling-inter+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
       16   16   16   16   17   18   20   24
       16   16   16   17   18   20   24   25
       16   16   17   18   20   24   25   28
       16   17   18   20   24   25   28   33
       17   18   20   24   25   28   33   41
       18   20   24   25   28   33   41   54
       20   24   25   28   33   41   54   71
       24   25   28   33   41   54   71   91))
  "The default 8x8 list for inter blocks.")

(declaim (type (simple-array (unsigned-byte 8) (64))
               +default-scaling-intra+ +default-scaling-inter+))

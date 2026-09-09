;;;; hevc/sig-ctx-tables.lisp — GENERATED.  sig_coeff_flag context increments (Table 9-39).
;;;;
;;;; Which context a "is this coefficient non-zero" bin uses, by its POSITION in the sub-block.
;;;; That is the deepest difference between HEVC's residual coding and H.264's: H.264 gives every
;;;; scan position in a block its own context, which is why its tables are long and why an 8x8
;;;; block needed a second set.  HEVC gives a 4x4 SUB-BLOCK's sixteen positions a shared handful of
;;;; contexts, selected by where the position sits and by whether the neighbouring sub-blocks had
;;;; anything in them — so one table serves every transform size from 4x4 to 32x32.
;;;;
;;;; Indexed [scan][row][position], where the five rows are:
;;;;
;;;;   0  a 4x4 transform block, where the position alone gives the context
;;;;   1  a larger block, neither the sub-block to the right nor the one below has coefficients
;;;;   2  a larger block, the sub-block to the RIGHT has coefficients
;;;;   3  a larger block, the sub-block BELOW has coefficients
;;;;   4  a larger block, both do
;;;;
;;;; Extracted mechanically, and checked against what the specification says apart from the
;;;; numbers: three scans by five rows of sixteen, no increment above 8, row 0 starts at 0 for the
;;;; DC position in every scan, and the "both neighbours" row is constant.

(in-package #:reel.hevc)

(defparameter +sig-ctx-map+
  (make-array '(3 5 16) :element-type '(unsigned-byte 8) :initial-contents
   (list
    ;; ---- diagonal scan
    (list
     (list 0 2 1 6 3 4 7 6 4 5 7 8 5 8 8 8)   ; a 4x4 transform block, where the position alone gives the context
     (list 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)   ; the rest, when neither neighbouring sub-block has coefficients
     (list 2 1 2 0 1 2 0 0 1 2 0 0 1 0 0 0)   ; the rest, when the sub-block to the RIGHT has coefficients
     (list 2 2 1 2 1 0 2 1 0 0 1 0 0 0 0 0)   ; the rest, when the sub-block BELOW has coefficients
     (list 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)   ; the rest, when both do — every position takes the same context
     )
    ;; ---- horizontal scan
    (list
     (list 0 1 4 5 2 3 4 5 6 6 8 8 7 7 8 8)   ; a 4x4 transform block, where the position alone gives the context
     (list 1 1 1 0 1 1 0 0 1 0 0 0 0 0 0 0)   ; the rest, when neither neighbouring sub-block has coefficients
     (list 2 2 2 2 1 1 1 1 0 0 0 0 0 0 0 0)   ; the rest, when the sub-block to the RIGHT has coefficients
     (list 2 1 0 0 2 1 0 0 2 1 0 0 2 1 0 0)   ; the rest, when the sub-block BELOW has coefficients
     (list 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)   ; the rest, when both do — every position takes the same context
     )
    ;; ---- vertical scan
    (list
     (list 0 2 6 7 1 3 6 7 4 4 8 8 5 5 8 8)   ; a 4x4 transform block, where the position alone gives the context
     (list 1 1 1 0 1 1 0 0 1 0 0 0 0 0 0 0)   ; the rest, when neither neighbouring sub-block has coefficients
     (list 2 1 0 0 2 1 0 0 2 1 0 0 2 1 0 0)   ; the rest, when the sub-block to the RIGHT has coefficients
     (list 2 2 2 2 1 1 1 1 0 0 0 0 0 0 0 0)   ; the rest, when the sub-block BELOW has coefficients
     (list 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)   ; the rest, when both do — every position takes the same context
     )
   )))

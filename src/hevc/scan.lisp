;;;; hevc/scan.lisp — the coefficient scan orders (6.5.3 to 6.5.5).
;;;;
;;;; DERIVED RATHER THAN TABULATED, because the derivation is three lines of the specification and
;;;; a transcription is forty numbers that all look alike.  The up-right diagonal scan in
;;;; particular is stated as an algorithm, and running it is both shorter and more obviously right
;;;; than printing what it produces.
;;;;
;;;; HEVC scans a transform block in two levels: 4x4 SUB-BLOCKS in one of these orders, and the
;;;; sixteen coefficients inside each sub-block in the same order again.  That is what makes a
;;;; 32x32 block affordable — the sub-block level carries a "anything here at all?" flag, so an
;;;; empty region costs one bin rather than sixteen.
;;;;
;;;; All three orders exist because an intra block predicted along a direction has its residual
;;;; energy lying ACROSS that direction, so a near-horizontal prediction is best scanned
;;;; vertically.  Which one a block uses is not signalled; it is derived from the prediction mode.

(in-package #:reel.hevc)

(defun %make-diag-scan (n)
  "The up-right diagonal scan of an NxN block (6.5.3), as two vectors of coordinates."
  (let ((xs (make-array (* n n) :element-type '(unsigned-byte 8)))
        (ys (make-array (* n n) :element-type '(unsigned-byte 8)))
        (i 0) (x 0) (y 0))
    (loop
      (loop while (>= y 0)
            do (when (and (< x n) (< y n))
                 (setf (aref xs i) x (aref ys i) y)
                 (incf i))
               (decf y) (incf x))
      (setf y x x 0)
      (when (>= i (* n n)) (return)))
    (values xs ys)))

(defun %make-horiz-scan (n)
  "Row by row, left to right."
  (let ((xs (make-array (* n n) :element-type '(unsigned-byte 8)))
        (ys (make-array (* n n) :element-type '(unsigned-byte 8)))
        (i 0))
    (dotimes (y n) (dotimes (x n)
                     (setf (aref xs i) x (aref ys i) y)
                     (incf i)))
    (values xs ys)))

(defun %invert-scan (xs ys n)
  "The scan position of each coordinate: INV[y][x], which is what \"where does the last significant
   coefficient sit in scan order\" needs."
  (let ((inv (make-array (list n n) :element-type '(unsigned-byte 8))))
    (dotimes (i (* n n) inv)
      (setf (aref inv (aref ys i) (aref xs i)) i))))

;;; The two scans this needs at each of the sizes that occur: 4x4 for the coefficients inside a
;;; sub-block, and 1x1 through 8x8 for the sub-blocks of a 4x4 through 32x32 transform.
(defparameter +diag4x4+ (multiple-value-list (%make-diag-scan 4)))
(defparameter +diag2x2+ (multiple-value-list (%make-diag-scan 2)))
(defparameter +diag8x8+ (multiple-value-list (%make-diag-scan 8)))
(defparameter +horiz4x4+ (multiple-value-list (%make-horiz-scan 4)))
(defparameter +horiz2x2+ (multiple-value-list (%make-horiz-scan 2)))

(defparameter +diag4x4-inv+ (%invert-scan (first +diag4x4+) (second +diag4x4+) 4))
(defparameter +diag2x2-inv+ (%invert-scan (first +diag2x2+) (second +diag2x2+) 2))
(defparameter +diag8x8-inv+ (%invert-scan (first +diag8x8+) (second +diag8x8+) 8))
(defparameter +horiz8x8-inv+
  ;; The horizontal scan of the SUB-BLOCKS of an 8x8 grid composed with the horizontal scan inside
  ;; each: position (x,y) in a 8x8 coefficient grid, expressed as sub-block index * 16 + offset.
  (let ((inv (make-array '(8 8) :element-type '(unsigned-byte 8))))
    (dotimes (y 8 inv)
      (dotimes (x 8)
        (setf (aref inv y x) (+ (* 16 (+ (* 2 (ash y -2)) (ash x -2)))
                                (* 4 (logand y 3)) (logand x 3)))))))

(defconstant +scan-diag+ 0)
(defconstant +scan-horiz+ 1)
(defconstant +scan-vert+ 2)

(defun %scan-for-intra (pred-mode)
  "Which scan an intra block of this prediction mode uses (7.4.9.11).

   A prediction that runs nearly horizontally leaves its residual energy in vertical stripes, so it
   is scanned VERTICALLY — the numbering is the opposite way round from what the name suggests, and
   only for the small blocks where it is worth the bother."
  (cond ((<= 6 pred-mode 14) +scan-vert+)
        ((<= 22 pred-mode 30) +scan-horiz+)
        (t +scan-diag+)))

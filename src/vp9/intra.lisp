;;;; vp9/intra.lisp — the fifteen intra prediction modes, at four sizes.
;;;;
;;;; Ten modes are codeable and five more exist only inside the decoder: when a block's neighbours
;;;; are missing, the mode it asked for is REPLACED by one that does not need them.  A vertical
;;;; prediction with nothing above it becomes a flat 127; a horizontal one with nothing to its left
;;;; becomes a flat 129; and the two DC modes that use only one side exist for the corners.  The
;;;; substitution is a table, not a rule, and the three constants are not 128 — they differ by one
;;;; in each direction so that a decoder cannot silently substitute the wrong one and get away with it.
;;;;
;;;; THE LEFT SAMPLES ARRIVE REVERSED.  Every mode but one reads them bottom-to-top, so the gatherer
;;;; stores them that way and the predictors index them from the end.  HOR_UP is the exception and
;;;; the gatherer knows: it has an `invert' flag for exactly that one mode.
;;;;
;;;; The top samples arrive with the corner sample at index minus one, which is why the array is
;;;; passed with an offset rather than from its start.  Six modes need that corner; four need the
;;;; four samples above-right of the block as well, and only at 4x4 — the larger sizes never look
;;;; past their own width.

(in-package #:reel.vp9)

(defconstant +pred-vert+ 0) (defconstant +pred-hor+ 1) (defconstant +pred-dc+ 2)
(defconstant +pred-diag-dl+ 3) (defconstant +pred-diag-dr+ 4)
(defconstant +pred-vert-right+ 5) (defconstant +pred-hor-down+ 6)
(defconstant +pred-vert-left+ 7) (defconstant +pred-hor-up+ 8) (defconstant +pred-tm+ 9)
(defconstant +pred-left-dc+ 10) (defconstant +pred-top-dc+ 11)
(defconstant +pred-dc128+ 12) (defconstant +pred-dc127+ 13) (defconstant +pred-dc129+ 14)

(defparameter +mode-conv+
  (make-array '(10 2 2) :element-type '(unsigned-byte 8) :initial-contents
   ;; [mode][have-left][have-top] -> the mode actually used
   '(((13  0) (13  0))          ; vertical: without a top row, a flat 127
     ((14 14) ( 1  1))          ; horizontal: without a left column, a flat 129
     ((12 11) (10  2))          ; DC: one side, the other, both, or 128
     ((13  3) (13  3))
     (( 4  4) ( 4  4))
     (( 5  5) ( 5  5))
     (( 6  6) ( 6  6))
     ((13  7) (13  7))
     ((14 14) ( 8  8))
     ((14  0) ( 1  9))))        ; TM needs both, and degrades to whichever it has
  "Which mode a block really uses, once availability is known.  Several rows ignore the
   availability entirely — the diagonal modes trust the gatherer to have substituted samples — and
   that is the specification, not an omission here.")

(defparameter +mode-needs+
  (make-array 15 :element-type '(unsigned-byte 8) :initial-contents
   ;; bit 0 left, bit 1 top, bit 2 top-left, bit 3 top-right, bit 4 left is NOT reversed
   '(#x02 #x01 #x03 #x0a #x07 #x07 #x07 #x0a #x11 #x07 #x01 #x02 #x00 #x00 #x00))
  "Which edges each mode reads, and whether it wants the left column the right way up.")

(declaim (type (simple-array (unsigned-byte 8) (10 2 2)) +mode-conv+)
         (type (simple-array (unsigned-byte 8) (15)) +mode-needs+))

;;; ---- the predictors ------------------------------------------------------------------------------

(defmacro %with-pred ((dst stride base sz) &body body)
  "Bind (PUT x y v) to a store into the block, and SZ to its side."
  `(macrolet ((put (x y v) `(setf (aref ,',dst (+ ,',base (* ,y ,',stride) ,x)) ,v)))
     (progn ,@body)))

(defun %pred-flat (dst stride base sz v)
  (declare (type octets dst) (type fixnum stride base sz v) (optimize (speed 3) (safety 1)))
  (dotimes (y sz)
    (let ((o (+ base (* y stride))))
      (dotimes (x sz) (setf (aref dst (+ o x)) v)))))

(defun %pred-intra (mode dst stride base sz l a)
  "One transform block's prediction.  A is the top row with its corner at index 31; L is the left
   column, reversed unless the mode asked otherwise."
  (declare (type octets dst l a) (type fixnum mode stride base sz)
           (optimize (speed 3) (safety 1)))
  (macrolet ((tp (k) `(aref a (+ 32 ,k)))
             (lf (k) `(aref l ,k))
             (put (x y v) `(setf (aref dst (+ base (* ,y stride) ,x)) ,v))
             (avg2 (x y) `(ash (+ ,x ,y 1) -1))
             (avg3 (x y z) `(ash (+ ,x (* 2 ,y) ,z 2) -2)))
    (case mode
      (#.+pred-vert+ (dotimes (y sz) (dotimes (x sz) (put x y (tp x)))))
      (#.+pred-hor+ (dotimes (y sz) (let ((v (lf (- sz 1 y)))) (dotimes (x sz) (put x y v)))))
      (#.+pred-dc+ (let ((s (ash sz 1)))
                     (declare (type fixnum s))
                     (let ((sum (ash s -1)))
                       (declare (type fixnum sum))
                       (dotimes (i sz) (incf sum (tp i)) (incf sum (lf i)))
                       (%pred-flat dst stride base sz (ash sum (- (integer-length (1- s))))))))
      (#.+pred-left-dc+ (let ((sum (ash sz -1)))
                          (declare (type fixnum sum))
                          (dotimes (i sz) (incf sum (lf i)))
                          (%pred-flat dst stride base sz
                                      (ash sum (- (integer-length (1- sz)))))))
      (#.+pred-top-dc+ (let ((sum (ash sz -1)))
                         (declare (type fixnum sum))
                         (dotimes (i sz) (incf sum (tp i)))
                         (%pred-flat dst stride base sz
                                     (ash sum (- (integer-length (1- sz)))))))
      (#.+pred-dc128+ (%pred-flat dst stride base sz 128))
      (#.+pred-dc127+ (%pred-flat dst stride base sz 127))
      (#.+pred-dc129+ (%pred-flat dst stride base sz 129))
      (#.+pred-tm+
       (let ((tl (tp -1)))
         (declare (type fixnum tl))
         (dotimes (y sz)
           (let ((d (- (lf (- sz 1 y)) tl)))
             (declare (type fixnum d))
             (dotimes (x sz) (put x y (%clip8 (+ (tp x) d))))))))
      (#.+pred-diag-dl+
       (if (= sz 4)
           ;; AT 4x4 THE MODE READS EIGHT SAMPLES ABOVE, not four: the above-right neighbour is
           ;; available at this size and the diagonal runs into it.  The larger sizes do not.
           (progn
             (put 0 0 (avg3 (tp 0) (tp 1) (tp 2)))
             (let ((v (avg3 (tp 1) (tp 2) (tp 3)))) (put 1 0 v) (put 0 1 v))
             (let ((v (avg3 (tp 2) (tp 3) (tp 4)))) (put 2 0 v) (put 1 1 v) (put 0 2 v))
             (let ((v (avg3 (tp 3) (tp 4) (tp 5))))
               (put 3 0 v) (put 2 1 v) (put 1 2 v) (put 0 3 v))
             (let ((v (avg3 (tp 4) (tp 5) (tp 6)))) (put 3 1 v) (put 2 2 v) (put 1 3 v))
             (let ((v (avg3 (tp 5) (tp 6) (tp 7)))) (put 3 2 v) (put 2 3 v))
             (put 3 3 (tp 7)))
           (let ((v (make-array 32 :element-type 'fixnum)))
             (declare (dynamic-extent v))
             (dotimes (i (- sz 2)) (setf (aref v i) (avg3 (tp i) (tp (1+ i)) (tp (+ i 2)))))
             (setf (aref v (- sz 2)) (ash (+ (tp (- sz 2)) (* 3 (tp (1- sz))) 2) -2))
             (dotimes (y sz)
               (dotimes (x sz)
                 (put x y (if (< x (- sz 1 y)) (aref v (+ y x)) (tp (1- sz)))))))))
      (#.+pred-diag-dr+
       (let ((v (make-array 64 :element-type 'fixnum)))
         (declare (dynamic-extent v))
         (dotimes (i (- sz 2))
           (setf (aref v i) (avg3 (lf i) (lf (1+ i)) (lf (+ i 2)))
                 (aref v (+ sz 1 i)) (avg3 (tp i) (tp (1+ i)) (tp (+ i 2)))))
         (setf (aref v (- sz 2)) (avg3 (lf (- sz 2)) (lf (1- sz)) (tp -1))
               (aref v (1- sz)) (avg3 (lf (1- sz)) (tp -1) (tp 0))
               (aref v sz) (avg3 (tp -1) (tp 0) (tp 1)))
         (dotimes (y sz)
           (dotimes (x sz) (put x y (aref v (+ sz -1 (- y) x)))))))
      (#.+pred-vert-right+
       (let ((ve (make-array 48 :element-type 'fixnum))
             (vo (make-array 48 :element-type 'fixnum))
             (half (ash sz -1)))
         (declare (dynamic-extent ve vo) (type fixnum half))
         (dotimes (i (- half 2))
           (setf (aref vo i) (avg3 (lf (+ (* 2 i) 3)) (lf (+ (* 2 i) 2)) (lf (+ (* 2 i) 1)))
                 (aref ve i) (avg3 (lf (+ (* 2 i) 4)) (lf (+ (* 2 i) 3)) (lf (+ (* 2 i) 2)))))
         (setf (aref vo (- half 2)) (avg3 (lf (1- sz)) (lf (- sz 2)) (lf (- sz 3)))
               (aref ve (- half 2)) (avg3 (tp -1) (lf (1- sz)) (lf (- sz 2)))
               (aref ve (1- half)) (avg2 (tp -1) (tp 0))
               (aref vo (1- half)) (avg3 (lf (1- sz)) (tp -1) (tp 0)))
         (dotimes (i (1- sz))
           (setf (aref ve (+ half i)) (avg2 (tp i) (tp (1+ i)))
                 (aref vo (+ half i)) (avg3 (tp (1- i)) (tp i) (tp (1+ i)))))
         (dotimes (j half)
           (dotimes (x sz)
             (put x (* 2 j) (aref ve (+ half -1 (- j) x)))
             (put x (1+ (* 2 j)) (aref vo (+ half -1 (- j) x)))))))
      (#.+pred-hor-down+
       (let ((v (make-array 96 :element-type 'fixnum)))
         (declare (dynamic-extent v))
         (dotimes (i (- sz 2))
           (setf (aref v (* 2 i)) (avg2 (lf (1+ i)) (lf i))
                 (aref v (1+ (* 2 i))) (avg3 (lf (+ i 2)) (lf (1+ i)) (lf i))
                 (aref v (+ (* 2 sz) i)) (avg3 (tp (1- i)) (tp i) (tp (1+ i)))))
         (setf (aref v (- (* 2 sz) 2)) (avg2 (tp -1) (lf (1- sz)))
               (aref v (- (* 2 sz) 4)) (avg2 (lf (1- sz)) (lf (- sz 2)))
               (aref v (- (* 2 sz) 1)) (avg3 (tp 0) (tp -1) (lf (1- sz)))
               (aref v (- (* 2 sz) 3)) (avg3 (tp -1) (lf (1- sz)) (lf (- sz 2))))
         (dotimes (y sz)
           (dotimes (x sz) (put x y (aref v (+ (* 2 sz) -2 (* -2 y) x)))))))
      (#.+pred-vert-left+
       (if (= sz 4)
           (progn
             (put 0 0 (avg2 (tp 0) (tp 1)))
             (put 0 1 (avg3 (tp 0) (tp 1) (tp 2)))
             (let ((v (avg2 (tp 1) (tp 2)))) (put 1 0 v) (put 0 2 v))
             (let ((v (avg3 (tp 1) (tp 2) (tp 3)))) (put 1 1 v) (put 0 3 v))
             (let ((v (avg2 (tp 2) (tp 3)))) (put 2 0 v) (put 1 2 v))
             (let ((v (avg3 (tp 2) (tp 3) (tp 4)))) (put 2 1 v) (put 1 3 v))
             (let ((v (avg2 (tp 3) (tp 4)))) (put 3 0 v) (put 2 2 v))
             (let ((v (avg3 (tp 3) (tp 4) (tp 5)))) (put 3 1 v) (put 2 3 v))
             (put 3 2 (avg2 (tp 4) (tp 5)))
             (put 3 3 (avg3 (tp 4) (tp 5) (tp 6))))
           (let ((ve (make-array 32 :element-type 'fixnum))
                 (vo (make-array 32 :element-type 'fixnum)))
             (declare (dynamic-extent ve vo))
             (dotimes (i (- sz 2))
               (setf (aref ve i) (avg2 (tp i) (tp (1+ i)))
                     (aref vo i) (avg3 (tp i) (tp (1+ i)) (tp (+ i 2)))))
             (setf (aref ve (- sz 2)) (avg2 (tp (- sz 2)) (tp (1- sz)))
                   (aref vo (- sz 2)) (ash (+ (tp (- sz 2)) (* 3 (tp (1- sz))) 2) -2))
             (dotimes (j (ash sz -1))
               (dotimes (x sz)
                 (put x (* 2 j) (if (< x (- sz j 1)) (aref ve (+ j x)) (tp (1- sz))))
                 (put x (1+ (* 2 j)) (if (< x (- sz j 1)) (aref vo (+ j x)) (tp (1- sz)))))))))
      (#.+pred-hor-up+
       (if (= sz 4)
           (progn
             (put 0 0 (avg2 (lf 0) (lf 1)))
             (put 1 0 (avg3 (lf 0) (lf 1) (lf 2)))
             (let ((v (avg2 (lf 1) (lf 2)))) (put 0 1 v) (put 2 0 v))
             (let ((v (avg3 (lf 1) (lf 2) (lf 3)))) (put 1 1 v) (put 3 0 v))
             (let ((v (avg2 (lf 2) (lf 3)))) (put 0 2 v) (put 2 1 v))
             (let ((v (ash (+ (lf 2) (* 3 (lf 3)) 2) -2))) (put 1 2 v) (put 3 1 v))
             (let ((v (lf 3)))
               (put 0 3 v) (put 1 3 v) (put 2 2 v) (put 2 3 v) (put 3 2 v) (put 3 3 v)))
           (let ((v (make-array 64 :element-type 'fixnum)))
             (declare (dynamic-extent v))
             (dotimes (i (- sz 2))
               (setf (aref v (* 2 i)) (avg2 (lf i) (lf (1+ i)))
                     (aref v (1+ (* 2 i))) (avg3 (lf i) (lf (1+ i)) (lf (+ i 2)))))
             (setf (aref v (- (* 2 sz) 4)) (avg2 (lf (- sz 2)) (lf (1- sz)))
                   (aref v (- (* 2 sz) 3)) (ash (+ (lf (- sz 2)) (* 3 (lf (1- sz))) 2) -2))
             (dotimes (y sz)
               (dotimes (x sz)
                 (let ((k (+ (* 2 y) x)))
                   (put x y (if (< k (- (* 2 sz) 2)) (aref v k) (lf (1- sz))))))))))
      (t (%err "intra prediction mode ~d" mode))))
  (values))

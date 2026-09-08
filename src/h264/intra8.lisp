;;;; h264/intra8.lisp — Intra_8x8 prediction (8.3.2).
;;;;
;;;; The nine modes are the Intra_4x4 nine, widened.  What is genuinely new is that the reference
;;;; samples are FILTERED FIRST, with a 1-2-1 kernel along the row above and the column left, before
;;;; any mode looks at them.  That step has no counterpart in the 4x4 path and is not optional: skip
;;;; it and every 8x8 block is slightly wrong in a way that looks like a rounding bug in the modes.
;;;;
;;;; Sixteen samples are taken from the row above rather than eight, because the diagonal and
;;;; vertical-left modes reach past the block's own width into the above-right neighbour.  When that
;;;; neighbour does not exist the last real sample is repeated, which is what makes those modes
;;;; degrade to something sensible at the right edge of a picture rather than reading rubbish.

(in-package #:reel.h264)

(defun gather-8x8-neighbours (plane stride base top left tl-out up-p left-p up-right-p up-left-p)
  "Collect and FILTER the reference samples for one 8x8 block.

   TOP receives sixteen filtered samples from the row above, LEFT eight from the column to the
   left, and TL-OUT the single corner.  Availability is handled before filtering, by substitution:
   a missing above-right repeats the eighth sample, and a missing side is filled from the other so
   that the filter has something to work on."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane top left tl-out)
           (type fixnum stride base)
           (optimize (speed 3) (safety 1)))
  (let ((raw-top (make-array 16 :element-type '(unsigned-byte 8) :initial-element 128))
        (raw-left (make-array 8 :element-type '(unsigned-byte 8) :initial-element 128))
        (raw-tl 128))
    (declare (dynamic-extent raw-top raw-left) (type fixnum raw-tl))
    (when up-p
      (dotimes (i 8) (setf (aref raw-top i) (aref plane (+ base (- stride) i))))
      (if up-right-p
          (dotimes (i 8) (setf (aref raw-top (+ 8 i)) (aref plane (+ base (- stride) 8 i))))
          ;; no above-right: the eighth sample stands in for all of it
          (dotimes (i 8) (setf (aref raw-top (+ 8 i)) (aref raw-top 7)))))
    (when left-p
      (dotimes (i 8) (setf (aref raw-left i) (aref plane (+ base (* i stride) -1)))))
    (setf raw-tl (cond (up-left-p (aref plane (+ base (- stride) -1)))
                       (up-p (aref raw-top 0))
                       (left-p (aref raw-left 0))
                       (t 128)))
    (unless up-p (dotimes (i 16) (setf (aref raw-top i) (if left-p (aref raw-left 0) 128))))
    (unless left-p (dotimes (i 8) (setf (aref raw-left i) (if up-p (aref raw-top 0) 128))))
    ;; ---- the 1-2-1 filter (8.3.2.2.1)
    (macrolet ((f3 (a b c) `(ash (+ ,a (* 2 ,b) ,c 2) -2)))
      (setf (aref tl-out 0)
            (cond ((and up-p left-p) (f3 (aref raw-top 0) raw-tl (aref raw-left 0)))
                  (up-p (f3 (aref raw-top 0) raw-tl raw-tl))
                  (left-p (f3 raw-tl raw-tl (aref raw-left 0)))
                  (t raw-tl)))
      (setf (aref top 0) (f3 raw-tl (aref raw-top 0) (aref raw-top 1)))
      (loop for i from 1 below 15
            do (setf (aref top i) (f3 (aref raw-top (1- i)) (aref raw-top i) (aref raw-top (1+ i)))))
      (setf (aref top 15) (ash (+ (aref raw-top 14) (* 3 (aref raw-top 15)) 2) -2))
      (setf (aref left 0) (f3 raw-tl (aref raw-left 0) (aref raw-left 1)))
      (loop for i from 1 below 7
            do (setf (aref left i) (f3 (aref raw-left (1- i)) (aref raw-left i) (aref raw-left (1+ i)))))
      (setf (aref left 7) (ash (+ (aref raw-left 6) (* 3 (aref raw-left 7)) 2) -2)))
    (values)))

(defun intra8x8-predict (out mode top left tl up-p left-p)
  "Fill OUT, sixty-four samples in raster order, with the prediction MODE calls for.

   The mode numbering is Intra_4x4's: 0 vertical, 1 horizontal, 2 DC, then the six directional
   ones.  The arithmetic is the 4x4 arithmetic with eight in place of four, except that DC has
   three fall-backs rather than one because either side may be missing on its own."
  (declare (type (simple-array (unsigned-byte 8) (*)) out top left)
           (type fixnum mode tl) (optimize (speed 3) (safety 1)))
  (macrolet ((t@ (i) `(let ((k ,i)) (if (minusp k) tl (aref top k))))
             (l@ (i) `(let ((k ,i)) (if (minusp k) tl (aref left k))))
             (avg2 (a b) `(ash (+ ,a ,b 1) -1))
             (avg3 (a b c) `(ash (+ ,a (* 2 ,b) ,c 2) -2))
             (put (x y v) `(setf (aref out (+ (* 8 ,y) ,x)) ,v)))
    (case mode
      (0 (dotimes (y 8) (dotimes (x 8) (put x y (aref top x)))))
      (1 (dotimes (y 8) (dotimes (x 8) (put x y (aref left y)))))
      (2 (let ((v (cond ((and up-p left-p)
                         (let ((s 8)) (dotimes (i 8) (incf s (aref top i)) (incf s (aref left i)))
                              (ash s -4)))
                        (left-p (let ((s 4)) (dotimes (i 8) (incf s (aref left i))) (ash s -3)))
                        (up-p (let ((s 4)) (dotimes (i 8) (incf s (aref top i))) (ash s -3)))
                        (t 128))))
           (dotimes (y 8) (dotimes (x 8) (put x y v)))))
      (3 (dotimes (y 8)
           (dotimes (x 8)
             (put x y (if (and (= x 7) (= y 7))
                          (ash (+ (aref top 14) (* 3 (aref top 15)) 2) -2)
                          (avg3 (aref top (+ x y)) (aref top (+ x y 1)) (aref top (+ x y 2))))))))
      (4 (dotimes (y 8)
           (dotimes (x 8)
             (put x y (cond ((> x y) (avg3 (t@ (- x y 2)) (t@ (- x y 1)) (t@ (- x y))))
                            ((< x y) (avg3 (l@ (- y x 2)) (l@ (- y x 1)) (l@ (- y x))))
                            (t (avg3 (aref top 0) tl (aref left 0))))))))
      (5 (dotimes (y 8)
           (dotimes (x 8)
             (let ((z (- (* 2 x) y)))
               (put x y (cond ((and (>= z 0) (evenp z))
                               (avg2 (t@ (- x (ash y -1) 1)) (t@ (- x (ash y -1)))))
                              ((and (>= z 0) (oddp z))
                               (avg3 (t@ (- x (ash y -1) 2)) (t@ (- x (ash y -1) 1))
                                     (t@ (- x (ash y -1)))))
                              ((= z -1) (avg3 (aref left 0) tl (aref top 0)))
                              (t (avg3 (l@ (- y (* 2 x) 1)) (l@ (- y (* 2 x) 2))
                                       (l@ (- y (* 2 x) 3))))))))))
      (6 (dotimes (y 8)
           (dotimes (x 8)
             (let ((z (- (* 2 y) x)))
               (put x y (cond ((and (>= z 0) (evenp z))
                               (avg2 (l@ (- y (ash x -1) 1)) (l@ (- y (ash x -1)))))
                              ((and (>= z 0) (oddp z))
                               (avg3 (l@ (- y (ash x -1) 2)) (l@ (- y (ash x -1) 1))
                                     (l@ (- y (ash x -1)))))
                              ((= z -1) (avg3 (aref left 0) tl (aref top 0)))
                              (t (avg3 (t@ (- x (* 2 y) 1)) (t@ (- x (* 2 y) 2))
                                       (t@ (- x (* 2 y) 3))))))))))
      (7 (dotimes (y 8)
           (dotimes (x 8)
             (put x y (if (evenp y)
                          (avg2 (aref top (+ x (ash y -1))) (aref top (+ x (ash y -1) 1)))
                          (avg3 (aref top (+ x (ash y -1))) (aref top (+ x (ash y -1) 1))
                                (aref top (+ x (ash y -1) 2))))))))
      (8 (dotimes (y 8)
           (dotimes (x 8)
             (let ((z (+ x (* 2 y))))
               (put x y (cond ((> z 13) (aref left 7))
                              ((= z 13) (ash (+ (aref left 6) (* 3 (aref left 7)) 2) -2))
                              ((evenp z) (avg2 (aref left (+ y (ash x -1)))
                                               (aref left (+ y (ash x -1) 1))))
                              (t (avg3 (aref left (+ y (ash x -1)))
                                       (aref left (+ y (ash x -1) 1))
                                       (aref left (+ y (ash x -1) 2))))))))))
      (t (%err "Intra_8x8 prediction mode ~d" mode))))
  out)

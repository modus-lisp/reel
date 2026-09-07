;;;; h264/intra.lisp — intra prediction (8.3): nine 4x4 modes, four 16x16, four for chroma.
;;;;
;;;; Everything here predicts a block from the row above it and the column to its left, both of
;;;; which are RECONSTRUCTED neighbours inside the same picture — decoded, residual added, but NOT
;;;; yet deblocked.  That last clause is the one that catches people: the deblocking filter runs
;;;; over the whole picture afterwards, and predicting from filtered samples instead of unfiltered
;;;; ones gives a decoder that is nearly right and drifts, which is exactly the failure it looks
;;;; least like.  This decoder keeps prediction on the unfiltered planes and filters at the end.
;;;;
;;;; AVAILABILITY is the other half.  A neighbour is available when it exists in the picture, has
;;;; already been decoded in raster order, and belongs to this slice.  When it does not, a mode
;;;; that needs it is not merely degraded — it is illegal, and an encoder will not have chosen it.
;;;; The exception is DC, which is defined for every combination and falls back to 128.
;;;;
;;;; The "above right" samples of a 4x4 block are a special case worth naming: they are often
;;;; outside the macroblock, sometimes not yet decoded, and when unavailable the specification
;;;; says to REPLICATE p[3,-1] into p[4..7,-1] rather than to forbid the mode.

(in-package #:reel.h264)

;;; ---- gathering the neighbouring samples ---------------------------------------------------------
;;;
;;; The specification names them p[x,y] for x in -1..7 and y in -1..3.  Here they are two small
;;; vectors: PT holds p[-1,-1] at index 0 and p[0..7,-1] at 1..8, and PL holds p[-1,0..3].

(defun gather-4x4-neighbours (plane stride base pt pl up-p left-p up-right-p up-left-p)
  "Fill PT (9) and PL (4) from PLANE around the 4x4 block at BASE."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane pt pl)
           (type fixnum stride base) (optimize (speed 3) (safety 0)))
  (fill pt 0) (fill pl 0)
  (when up-p
    (let ((u (- base stride)))
      (declare (type fixnum u))
      (dotimes (i 4) (setf (aref pt (1+ i)) (aref plane (+ u i))))
      (if up-right-p
          (dotimes (i 4) (setf (aref pt (+ 5 i)) (aref plane (+ u 4 i))))
          ;; not available: p[3,-1] stands in for p[4..7,-1] (8.3.1.2)
          (dotimes (i 4) (setf (aref pt (+ 5 i)) (aref pt 4))))))
  (when left-p
    (dotimes (i 4) (setf (aref pl i) (aref plane (+ base (* i stride) -1)))))
  (when up-left-p (setf (aref pt 0) (aref plane (- base stride 1))))
  (values))

(defmacro %p (i) "p[I,-1], for I from -1 to 7.  p[-1,-1] is the above-left corner." `(aref pt (1+ ,i)))
(defmacro %l (i)
  "p[-1,I], for I from -1 to 3.

   The -1 case is not an accident to guard against: Vertical Right at (0,2) genuinely reaches
   p[-1,-1], which is the above-left CORNER and lives with the above row rather than the left
   column.  Two of the nine modes walk off the top of the left column into it."
  `(let ((%i ,i)) (if (minusp %i) (aref pt 0) (aref pl %i))))

;;; ---- the nine 4x4 modes (8.3.1.2) -----------------------------------------------------------------

(defun intra4x4-predict (out mode pt pl up-p left-p up-left-p)
  "Fill OUT (16, raster) with the 4x4 prediction for MODE."
  (declare (type (simple-array (unsigned-byte 8) (*)) out pt pl) (type fixnum mode)
           (optimize (speed 3) (safety 1)))
  (macrolet ((setp (x y v) `(setf (aref out (+ (* ,y 4) ,x)) (clamp255 ,v)))
             (avg2 (a b) `(ash (+ ,a ,b 1) -1))
             (avg3 (a b c) `(ash (+ ,a (ash ,b 1) ,c 2) -2)))
    (ecase mode
      (0                                        ; Vertical
       (dotimes (y 4) (dotimes (x 4) (setp x y (%p x)))))
      (1                                        ; Horizontal
       (dotimes (y 4) (dotimes (x 4) (setp x y (%l y)))))
      (2                                        ; DC — the only mode defined for every neighbourhood
       (let ((dc (cond ((and up-p left-p)
                        (ash (+ (%p 0) (%p 1) (%p 2) (%p 3)
                                (%l 0) (%l 1) (%l 2) (%l 3) 4) -3))
                       (left-p (ash (+ (%l 0) (%l 1) (%l 2) (%l 3) 2) -2))
                       (up-p (ash (+ (%p 0) (%p 1) (%p 2) (%p 3) 2) -2))
                       (t 128))))
         (dotimes (i 16) (setf (aref out i) (clamp255 dc)))))
      (3                                        ; Diagonal Down Left
       (dotimes (y 4)
         (dotimes (x 4)
           (setp x y (if (and (= x 3) (= y 3))
                         (avg3 (%p 6) (%p 7) (%p 7))
                         (avg3 (%p (+ x y)) (%p (+ x y 1)) (%p (+ x y 2))))))))
      (4                                        ; Diagonal Down Right
       (dotimes (y 4)
         (dotimes (x 4)
           (setp x y (cond ((> x y) (avg3 (%p (- x y 2)) (%p (- x y 1)) (%p (- x y))))
                           ((< x y) (avg3 (%l (- y x 2)) (%l (- y x 1)) (%l (- y x))))
                           (t (avg3 (%p 0) (%p -1) (%l 0))))))))
      (5                                        ; Vertical Right
       (dotimes (y 4)
         (dotimes (x 4)
           (let ((z (- (* 2 x) y)) (h (ash y -1)))
             (setp x y (cond ((member z '(0 2 4 6)) (avg2 (%p (- x h 1)) (%p (- x h))))
                             ((member z '(1 3 5)) (avg3 (%p (- x h 2)) (%p (- x h 1)) (%p (- x h))))
                             ((= z -1) (avg3 (%l 0) (%p -1) (%p 0)))
                             (t (avg3 (%l (- y 1)) (%l (- y 2)) (%l (- y 3))))))))))
      (6                                        ; Horizontal Down
       (dotimes (y 4)
         (dotimes (x 4)
           (let ((z (- (* 2 y) x)) (h (ash x -1)))
             (setp x y (cond ((member z '(0 2 4 6)) (avg2 (%l (- y h 1)) (%l (- y h))))
                             ((member z '(1 3 5)) (avg3 (%l (- y h 2)) (%l (- y h 1)) (%l (- y h))))
                             ((= z -1) (avg3 (%l 0) (%p -1) (%p 0)))
                             (t (avg3 (%p (- x 1)) (%p (- x 2)) (%p (- x 3))))))))))
      (7                                        ; Vertical Left
       (dotimes (y 4)
         (dotimes (x 4)
           (let ((h (ash y -1)))
             (setp x y (if (member y '(0 2))
                           (avg2 (%p (+ x h)) (%p (+ x h 1)))
                           (avg3 (%p (+ x h)) (%p (+ x h 1)) (%p (+ x h 2)))))))))
      (8                                        ; Horizontal Up
       (dotimes (y 4)
         (dotimes (x 4)
           (let ((z (+ x (* 2 y))) (h (ash x -1)))
             (setp x y (cond ((member z '(0 2 4)) (avg2 (%l (+ y h)) (%l (+ y h 1))))
                             ((member z '(1 3)) (avg3 (%l (+ y h)) (%l (+ y h 1)) (%l (+ y h 2))))
                             ((= z 5) (avg3 (%l 2) (%l 3) (%l 3)))
                             (t (%l 3)))))))))
    out))

;;; ---- the four 16x16 modes (8.3.3) -------------------------------------------------------------------

(defun intra16x16-predict (plane stride base mode up-p left-p)
  "Predict a whole 16x16 luma macroblock IN PLACE in PLANE at BASE."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane) (type fixnum stride base mode)
           (optimize (speed 3) (safety 1)))
  (macrolet ((up (i) `(aref plane (+ base (- stride) ,i)))
             (lf (i) `(aref plane (+ base (* ,i stride) -1))))
    (ecase mode
      (0 (dotimes (y 16) (dotimes (x 16) (setf (aref plane (+ base (* y stride) x)) (up x)))))
      (1 (dotimes (y 16) (let ((v (lf y)))
                           (dotimes (x 16) (setf (aref plane (+ base (* y stride) x)) v)))))
      (2 (let ((dc (cond ((and up-p left-p)
                          (let ((s 16))
                            (declare (type fixnum s))
                            (dotimes (i 16) (incf s (up i)) (incf s (lf i)))
                            (ash s -5)))
                         (up-p (let ((s 8)) (dotimes (i 16) (incf s (up i))) (ash s -4)))
                         (left-p (let ((s 8)) (dotimes (i 16) (incf s (lf i))) (ash s -4)))
                         (t 128))))
           (dotimes (y 16) (dotimes (x 16) (setf (aref plane (+ base (* y stride) x)) (clamp255 dc))))))
      (3                                        ; Plane — a linear ramp fitted to both edges
       (let ((h 0) (v 0))
         (declare (type fixnum h v))
         (dotimes (i 8)
           (incf h (* (1+ i) (- (up (+ 8 i)) (if (= i 7) (aref plane (- base stride 1)) (up (- 6 i))))))
           (incf v (* (1+ i) (- (lf (+ 8 i)) (if (= i 7) (aref plane (- base stride 1)) (lf (- 6 i)))))))
         (let* ((a (* 16 (+ (lf 15) (up 15))))
                (b (ash (+ (* 5 h) 32) -6))
                (c (ash (+ (* 5 v) 32) -6)))
           (declare (type fixnum a b c))
           (dotimes (y 16)
             (dotimes (x 16)
               (setf (aref plane (+ base (* y stride) x))
                     (clamp255 (ash (+ a (* b (- x 7)) (* c (- y 7)) 16) -5)))))))))
    plane))

;;; ---- chroma (8.3.4) ----------------------------------------------------------------------------------

(defun chroma-predict (plane stride base mode up-p left-p)
  "Predict an 8x8 chroma block IN PLACE.  MODE: 0 DC, 1 Horizontal, 2 Vertical, 3 Plane."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane) (type fixnum stride base mode)
           (optimize (speed 3) (safety 1)))
  (macrolet ((up (i) `(aref plane (+ base (- stride) ,i)))
             (lf (i) `(aref plane (+ base (* ,i stride) -1)))
             (put (x y v) `(setf (aref plane (+ base (* ,y stride) ,x)) (clamp255 ,v))))
    (ecase mode
      (0
       ;; DC, and the asymmetry that makes it the fiddliest of the four: each 4x4 quadrant has its
       ;; own preference for which neighbour to use when only one is available.  The top-right
       ;; quadrant prefers ABOVE, the bottom-left prefers LEFT, and the two on the diagonal use
       ;; both.  Averaging all eight neighbours for the whole block is the plausible wrong answer.
       (dolist (q '((0 0) (4 0) (0 4) (4 4)))
         (destructuring-bind (x0 y0) q
           (let* ((sa (when up-p (let ((s 0)) (dotimes (i 4) (incf s (up (+ x0 i)))) s)))
                  (sl (when left-p (let ((s 0)) (dotimes (i 4) (incf s (lf (+ y0 i)))) s)))
                  (dc (cond ((and (= x0 4) (= y0 0))
                             (cond (sa (ash (+ sa 2) -2)) (sl (ash (+ sl 2) -2)) (t 128)))
                            ((and (= x0 0) (= y0 4))
                             (cond (sl (ash (+ sl 2) -2)) (sa (ash (+ sa 2) -2)) (t 128)))
                            (t
                             (cond ((and sa sl) (ash (+ sa sl 4) -3))
                                   (sa (ash (+ sa 2) -2))
                                   (sl (ash (+ sl 2) -2))
                                   (t 128))))))
             (dotimes (y 4) (dotimes (x 4) (put (+ x0 x) (+ y0 y) dc)))))))
      (1 (dotimes (y 8) (let ((v (lf y))) (dotimes (x 8) (put x y v)))))
      (2 (dotimes (y 8) (dotimes (x 8) (put x y (up x)))))
      (3
       (let ((h 0) (v 0))
         (declare (type fixnum h v))
         (dotimes (i 4)
           (incf h (* (1+ i) (- (up (+ 4 i)) (if (= i 3) (aref plane (- base stride 1)) (up (- 2 i))))))
           (incf v (* (1+ i) (- (lf (+ 4 i)) (if (= i 3) (aref plane (- base stride 1)) (lf (- 2 i)))))))
         (let* ((a (* 16 (+ (lf 7) (up 7))))
                (b (ash (+ (* 34 h) 32) -6))
                (c (ash (+ (* 34 v) 32) -6)))
           (declare (type fixnum a b c))
           (dotimes (y 8)
             (dotimes (x 8)
               (put x y (ash (+ a (* b (- x 3)) (* c (- y 3)) 16) -5))))))))
    plane))

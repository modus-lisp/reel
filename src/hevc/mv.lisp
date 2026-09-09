;;;; hevc/mv.lisp — merge and AMVP candidate derivation (8.5.3.2).
;;;;
;;;; A prediction block either MERGES — takes a neighbour's whole motion, both lists, and sends
;;;; nothing but an index — or codes a difference against a PREDICTED vector.  H.264 has both ideas
;;;; in weaker forms (skip, and the median predictor); what HEVC changed is that the candidate set
;;;; is now an explicit, ordered, deduplicated LIST, identical on both sides, and the encoder
;;;; chooses from it by index.
;;;;
;;;; That is why so much of this file is redundancy checks.  A list with two identical entries
;;;; wastes an index, so the standard specifies exactly which pairs are compared and in which
;;;; order — not as an optimisation but as part of the bitstream's meaning.  Compare a pair the
;;;; standard does not, or skip one it does, and the list is a different length and every index
;;;; after it names the wrong candidate.
;;;;
;;;; The five spatial positions are the two to the left, the two above, and the corner:
;;;;
;;;;      B2 . . . . B1 B0
;;;;       . +-------+
;;;;       . |       |
;;;;      A1 |  this |
;;;;       . +-------+
;;;;      A0
;;;;
;;;; A1 and B1 are inside the block's own row and column, so they are the ones a split block would
;;;; predict from — which is why the second half of a split partition is forbidden the one that
;;;; would make the split pointless.

(in-package #:reel.hevc)

;;; A candidate is eight fixnums: mv and reference index and reference POC for each list, with an
;;; index of -1 meaning the list is unused.  Kept as a flat vector because the lists are built,
;;; compared and copied far more often than they are read.

(defconstant +cand-size+ 8)

(defparameter +bi-order-l0+
  (make-array 12 :element-type 'fixnum :initial-contents '(0 1 0 2 1 2 0 3 1 3 2 3))
  "l0Cand index of the combined bi-predictive candidates, in order (Table 8-6).")
(defparameter +bi-order-l1+
  (make-array 12 :element-type 'fixnum :initial-contents '(1 0 2 0 2 1 3 0 3 1 3 2))
  "and the l1Cand index that goes with it.")
(declaim (type (simple-array fixnum (12)) +bi-order-l0+ +bi-order-l1+))

(declaim (inline cand-ref cand-mvx cand-mvy cand-poc))
(defun cand-ref (c i lx) (aref c (+ (* +cand-size+ i) (* 4 lx) 2)))
(defun cand-mvx (c i lx) (aref c (+ (* +cand-size+ i) (* 4 lx))))
(defun cand-mvy (c i lx) (aref c (+ (* +cand-size+ i) (* 4 lx) 1)))
(defun cand-poc (c i lx) (aref c (+ (* +cand-size+ i) (* 4 lx) 3)))

(defun %set-cand (c i lx mvx mvy ref poc)
  (setf (aref c (+ (* +cand-size+ i) (* 4 lx))) mvx
        (aref c (+ (* +cand-size+ i) (* 4 lx) 1)) mvy
        (aref c (+ (* +cand-size+ i) (* 4 lx) 2)) ref
        (aref c (+ (* +cand-size+ i) (* 4 lx) 3)) poc))

(defun %clear-cand (c i)
  (dotimes (k +cand-size+) (setf (aref c (+ (* +cand-size+ i) k)) 0))
  (setf (aref c (+ (* +cand-size+ i) 2)) -1
        (aref c (+ (* +cand-size+ i) 6)) -1))

(defun %same-motion-p (c i j)
  "Do two candidates have identical motion in both lists?  This is the redundancy test the
   standard's pruning is written in terms of, and it compares the reference PICTURE rather than the
   index — two indices into different lists can name the same picture."
  (dotimes (lx 2 t)
    (let ((ri (cand-ref c i lx)) (rj (cand-ref c j lx)))
      (unless (and (= (if (minusp ri) -1 1) (if (minusp rj) -1 1))
                   (or (minusp ri)
                       (and (= (cand-mvx c i lx) (cand-mvx c j lx))
                            (= (cand-mvy c i lx) (cand-mvy c j lx))
                            (= (cand-poc c i lx) (cand-poc c j lx)))))
        (return nil)))))

(defun %neighbour-motion (c x y)
  "(values available-p) for the 4x4 block at luma (X,Y), and its motion through the picture.

   Available means inside the picture, decoded before us in z-scan order, in the same slice, and
   NOT intra — an intra neighbour has no motion to lend."
  (declare (type ctx c) (type fixnum x y))
  (and (%available-p c (cx-cu-x c) (cx-cu-y c) x y)
       (zerop (aref (pic-intra (cx-pic c)) (pic-mv-index (cx-pic c) x y)))))

(defun %take-neighbour (c cands i x y)
  "Copy the motion at (X,Y) into candidate slot I.  Returns true when there was any."
  (declare (type ctx c) (type fixnum i x y))
  (%clear-cand cands i)
  (when (%neighbour-motion c x y)
    (let ((pic (cx-pic c)) (any nil))
      (dotimes (lx 2)
        (multiple-value-bind (mvx mvy ref poc) (pic-motion pic x y lx)
          (unless (minusp ref)
            (setf any t)
            (%set-cand cands i lx mvx mvy ref poc))))
      any)))

(defun %scale-mv (mv td tb)
  "Scale a vector from one temporal distance to another (8.5.3.2.8).

   The reciprocal is computed once at fourteen bits and then applied — the standard specifies the
   division exactly, because an implementation that divides twice or rounds differently produces
   vectors that differ by one and pictures that differ everywhere."
  (declare (type fixnum mv td tb))
  (if (or (zerop td) (= td tb))
      mv
      (let* ((td (%clip3 -128 127 td))
             (tb (%clip3 -128 127 tb))
             (tx (truncate (+ 16384 (ash (abs td) -1)) td))
             (scale (%clip3 -4096 4095 (ash (+ (* tb tx) 32) -6)))
             (v (* scale mv)))
        (declare (type fixnum td tb tx scale v))
        (%clip3 -32768 32767 (* (if (minusp v) -1 1) (ash (+ (abs v) 127) -8))))))

(defun %temporal-candidate (c cands i x0 y0 w h)
  "The collocated candidate (8.5.3.2.8): the motion of the block at the same place in another
   picture, scaled for the difference in temporal distance.

   Bottom-right first and the centre as a fallback, because the bottom-right of a moving object is
   more likely to still be the object in the next picture than its centre is to still be its
   centre.  It is read at SIXTEEN-sample granularity — the collocated field is stored coarsely, so
   a decoder need not keep a full-resolution motion field for every reference picture."
  (declare (type ctx c) (type fixnum i x0 y0 w h))
  (%clear-cand cands i)
  (let ((col (cx-collocated c)))
    (unless col (return-from %temporal-candidate nil))
    (let* ((pic (cx-pic c))
           (cw (pic-width pic)) (ch (pic-height pic))
           (bx (+ x0 w)) (by (+ y0 h))
           (found nil))
      (declare (type fixnum bx by))
      (flet ((probe (px py)
               ;; the collocated position is rounded down to a multiple of sixteen
               (let ((qx (logandc2 px 15)) (qy (logandc2 py 15)))
                 (declare (type fixnum qx qy))
                 (and (< -1 qx cw) (< -1 qy ch)
                      (zerop (aref (pic-intra col) (pic-mv-index col qx qy)))
                      (cons qx qy)))))
        (let ((at (or (and (< bx cw) (< by ch)
                           ;; and only when it is still inside this coding tree block row
                           (= (ash by (- (pic-ctb-log2 pic))) (ash y0 (- (pic-ctb-log2 pic))))
                           (probe bx by))
                      (probe (+ x0 (ash w -1)) (+ y0 (ash h -1))))))
          (when at
            ;; the collocated block's own list 0 is preferred; where it used only list 1, that
            ;; vector serves for both of ours
            (let ((src (if (minusp (nth-value 2 (pic-motion col (car at) (cdr at) 0))) 1 0)))
              (multiple-value-bind (mvx mvy ref poc) (pic-motion col (car at) (cdr at) src)
                (unless (minusp ref)
                  (dotimes (lx 2)
                    (let ((list (if (zerop lx) (cx-list0 c) (cx-list1 c))))
                      (when (plusp (length list))
                        (let* ((target (aref list 0))
                               (tb (- (pic-poc pic) (pic-poc target)))
                               (td (- (pic-poc col) poc)))
                          (setf found t)
                          (%set-cand cands i lx (%scale-mv mvx td tb) (%scale-mv mvy td tb)
                                     0 (pic-poc target)))))))))
            found))))))

(defun %merge-candidates (c x0 y0 w h part-idx part-mode out)
  "The merge list (8.5.3.2.2), in order and deduplicated.

   OUT is filled with up to MaxNumMergeCand candidates and the count is returned.  The order is
   A1, B1, B0, A0, B2, then the temporal one, then — in a B slice — pairs of earlier entries
   combined into bi-prediction, then zero vectors.  Nothing here is heuristic: the encoder built
   the same list and sent an index into it."
  (declare (type ctx c) (type fixnum x0 y0 w h part-idx part-mode))
  (let ((n 0)
        (maxn (sh-max-merge-cand (cx-sh c)))
        (tmp (make-array (* +cand-size+ 6) :element-type 'fixnum :initial-element 0))
        ;; the second partition of a split may not merge with the first: that would code the split
        ;; and then undo it
        (skip-a1 (and (= part-idx 1) (member part-mode '(2 4 5))))     ; Nx2N, nLx2N, nRx2N
        (skip-b1 (and (= part-idx 1) (member part-mode '(1 6 7)))))    ; 2NxN, 2NxnU, 2NxnD
    (declare (type fixnum n maxn))
    (flet ((emit (from)
             (when (< n maxn)
               (dotimes (k +cand-size+)
                 (setf (aref out (+ (* +cand-size+ n) k)) (aref tmp (+ (* +cand-size+ from) k))))
               (incf n))))
      ;; ---- A1, left, level with the bottom of the block
      (let ((a1 (and (not skip-a1) (%take-neighbour c tmp 0 (1- x0) (+ y0 h -1)))))
        (when a1 (emit 0))
        ;; ---- B1, above, level with the right of the block
        (let ((b1 (and (not skip-b1) (%take-neighbour c tmp 1 (+ x0 w -1) (1- y0)))))
          (when (and b1 (or (not a1) (not (%same-motion-p tmp 1 0)))) (emit 1))
          ;; ---- B0, above-right
          (let ((b0 (%take-neighbour c tmp 2 (+ x0 w) (1- y0))))
            (when (and b0 (or (not b1) (not (%same-motion-p tmp 2 1)))) (emit 2)))
          ;; ---- A0, below-left
          (let ((a0 (%take-neighbour c tmp 3 (1- x0) (+ y0 h))))
            (when (and a0 (or (not a1) (not (%same-motion-p tmp 3 0)))) (emit 3)))
          ;; ---- B2, the corner, and only when the other four did not fill the list
          (when (< n 4)
            (let ((b2 (%take-neighbour c tmp 4 (1- x0) (1- y0))))
              (when (and b2
                         (or (not a1) (not (%same-motion-p tmp 4 0)))
                         (or (not b1) (not (%same-motion-p tmp 4 1))))
                (emit 4))))))
      ;; ---- the collocated candidate
      (when (and (< n maxn) (sh-temporal-mvp (cx-sh c)))
        (when (%temporal-candidate c tmp 5 x0 y0 w h) (emit 5)))
      ;; ---- combined bi-predictive: list 0 of one earlier candidate with list 1 of another
      (when (and (sh-b-slice-p (cx-sh c)) (> n 1))
        (let ((base n) (k 0))
          (declare (type fixnum base k))
          (loop while (and (< n maxn) (< k (* base (1- base)))) do
            (let ((i (aref +bi-order-l0+ k)) (j (aref +bi-order-l1+ k)))
              (when (and (< i base) (< j base)
                         (not (minusp (cand-ref out i 0)))
                         (not (minusp (cand-ref out j 1)))
                         (or (/= (cand-poc out i 0) (cand-poc out j 1))
                             (/= (cand-mvx out i 0) (cand-mvx out j 1))
                             (/= (cand-mvy out i 0) (cand-mvy out j 1))))
                (%clear-cand out n)
                (%set-cand out n 0 (cand-mvx out i 0) (cand-mvy out i 0)
                           (cand-ref out i 0) (cand-poc out i 0))
                (%set-cand out n 1 (cand-mvx out j 1) (cand-mvy out j 1)
                           (cand-ref out j 1) (cand-poc out j 1))
                (incf n)))
            (incf k))))
      ;; ---- and zero vectors, with the reference index walking up the list
      (let ((zero 0)
            (nref (if (sh-b-slice-p (cx-sh c))
                      (min (length (cx-list0 c)) (length (cx-list1 c)))
                      (length (cx-list0 c)))))
        (declare (type fixnum zero nref))
        (loop while (< n maxn) do
          (%clear-cand out n)
          (let ((r (if (< zero nref) zero 0)))
            (when (plusp (length (cx-list0 c)))
              (%set-cand out n 0 0 0 r (pic-poc (aref (cx-list0 c) r))))
            (when (and (sh-b-slice-p (cx-sh c)) (plusp (length (cx-list1 c))))
              (%set-cand out n 1 0 0 r (pic-poc (aref (cx-list1 c) r)))))
          (incf zero) (incf n))))
    n))

(defun %amvp (c x0 y0 w h lx ref-idx)
  "The two motion vector predictors (8.5.3.2.6), as (values ax ay bx by count).

   Each side is searched twice: first for a neighbour that used the SAME reference picture, whose
   vector can be taken as it is, and only then for one that used a different reference, whose
   vector has to be scaled by the ratio of temporal distances.  Doing it in one pass and scaling
   everything gives a different predictor whenever an unscaled one was available."
  (declare (type ctx c) (type fixnum x0 y0 w h lx ref-idx))
  (let* ((pic (cx-pic c))
         (list (if (zerop lx) (cx-list0 c) (cx-list1 c)))
         (target (and (< ref-idx (length list)) (pic-poc (aref list ref-idx))))
         (cur (pic-poc pic))
         (ax 0) (ay 0) (a-found nil)
         (bx 0) (by 0) (b-found nil))
    (declare (type fixnum ax ay bx by))
    (unless target (return-from %amvp (values 0 0 0 0 0)))
    (labels ((probe (px py want-same)
               ;; (values mvx mvy) from this neighbour, or NIL
               (when (%neighbour-motion c px py)
                 (dotimes (l 2)
                   ;; the neighbour's own list LX first, then the other one: a vector is a vector,
                   ;; and which list it was coded in does not change where it points
                   (let ((k (if (zerop l) lx (- 1 lx))))
                     (multiple-value-bind (mvx mvy ref poc) (pic-motion pic px py k)
                       (declare (ignore ref))
                       (let ((r (nth-value 2 (pic-motion pic px py k))))
                         (unless (minusp r)
                           (if want-same
                               (when (= poc target) (return-from probe (values mvx mvy)))
                               (return-from probe
                                 (values (%scale-mv mvx (- cur poc) (- cur target))
                                         (%scale-mv mvy (- cur poc) (- cur target))))))))))
                 nil)))
      ;; ---- A, from below-left then left
      (dolist (same '(t nil))
        (unless a-found
          (dolist (at (list (cons (1- x0) (+ y0 h)) (cons (1- x0) (+ y0 h -1))))
            (unless a-found
              (multiple-value-bind (mx my) (probe (car at) (cdr at) same)
                (when mx (setf ax mx ay my a-found t)))))))
      ;; ---- B, from above-right, above, then the corner
      (dolist (same '(t nil))
        (unless b-found
          (dolist (at (list (cons (+ x0 w) (1- y0)) (cons (+ x0 w -1) (1- y0))
                            (cons (1- x0) (1- y0))))
            (unless b-found
              (multiple-value-bind (mx my) (probe (car at) (cdr at) same)
                (when mx (setf bx mx by my b-found t))))))))
    (let ((n 0))
      (declare (type fixnum n))
      (when a-found (incf n))
      (when (and b-found (or (not a-found) (/= ax bx) (/= ay by)))
        (if a-found (setf n 2) (setf ax bx ay by n 1)))
      (when (and (< n 2) (sh-temporal-mvp (cx-sh c)))
        (let ((tmp (make-array (* +cand-size+ 6) :element-type 'fixnum :initial-element 0)))
          (when (%temporal-candidate c tmp 5 x0 y0 w h)
            (let ((mx (cand-mvx tmp 5 lx)) (my (cand-mvy tmp 5 lx)))
              (unless (minusp (cand-ref tmp 5 lx))
                (if (zerop n) (setf ax mx ay my n 1) (setf bx mx by my n 2)))))))
      (values ax ay bx by n))))

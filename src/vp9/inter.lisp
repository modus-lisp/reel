;;;; vp9/inter.lisp — inter blocks: which references, which mode, and which motion vector.
;;;;
;;;; VP9 predicts a motion vector before it codes one, and the prediction is a SEARCH rather than a
;;;; formula.  Up to eight neighbouring blocks are visited in an order that depends on the block's
;;;; shape — nearer edge first, because that is the neighbour most likely to agree — and the first
;;;; one that used the same reference frame supplies the vector.  If none did, the search runs again
;;;; accepting a different reference and negating the vector when the two point opposite ways in
;;;; time.  If that fails too, the previous frame's vector at this position is tried, and only then
;;;; does the predictor give up and answer zero.
;;;;
;;;; THE SEARCH RETURNS THE SECOND DISTINCT ANSWER when the block asked for the near vector rather
;;;; than the nearest, which is why the whole thing is written as early returns over a remembered
;;;; first candidate rather than as a list of candidates.  Building the list and taking the second
;;;; element is not the same function: the specification's order of comparison is observable.
;;;;
;;;; Two of ffmpeg's comments here say the behaviour is a bug in libvpx.  They are transcribed as
;;;; they stand, because libvpx is what encoders were written against and a decoder that fixes the
;;;; bug decodes those streams wrongly.

(in-package #:reel.vp9)

(defconstant +nearestmv+ 10) (defconstant +nearmv+ 11)
(defconstant +zeromv+ 12) (defconstant +newmv+ 13)

(defparameter +size-group+
  (make-array 13 :element-type 'fixnum :initial-contents '(3 3 3 3 2 2 2 1 1 1 0 0 0))
  "Which of the four luma-mode probability sets a block size uses, for an intra block inside an
   inter frame.")
(defparameter +sub8x8-mode-off+
  (make-array 13 :element-type 'fixnum :initial-contents '(3 0 0 1 0 0 0 0 0 0 0 0 0))
  "Where inside the block to read the neighbouring mode from, for a sub-8x8 block's single mode.")
(declaim (type (simple-array fixnum (13)) +size-group+ +sub8x8-mode-off+))

;;; ---- the predicted vector ------------------------------------------------------------------------

(declaim (inline %clamp-mvx %clamp-mvy))
(defun %clamp-mvx (st v) (declare (type fixnum v))
  (max (st-min-mvx st) (min (st-max-mvx st) v)))
(defun %clamp-mvy (st v) (declare (type fixnum v))
  (max (st-min-mvy st) (min (st-max-mvy st) v)))

(defmacro %with-mv-search ((st idx sb) &body body)
  "Bind the two early-return forms the search is written in, and the memory they share."
  `(let ((mem-set nil) (mem-x 0) (mem-y 0)
         (sub-set nil) (sub-x 0) (sub-y 0))
     (declare (type fixnum mem-x mem-y sub-x sub-y) (ignorable sub-set sub-x sub-y))
     (macrolet
         ((direct (mx my)
            ;; a vector from one of this block's own earlier sub-blocks, taken as it stands
            `(let ((x ,mx) (y ,my))
               (declare (type fixnum x y))
               (cond ((zerop ,',idx) (return-from search (values x y)))
                     ((not mem-set) (setf mem-set t mem-x x mem-y y))
                     ((or (/= x mem-x) (/= y mem-y)) (return-from search (values x y))))))
          (candidate (mx my)
            ;; and a vector from a neighbour, which is clamped to what this block may address
            `(let ((x ,mx) (y ,my))
               (declare (type fixnum x y))
               (if (plusp ,',sb)
                   (let ((cx (%clamp-mvx ,',st x)) (cy (%clamp-mvy ,',st y)))
                     (declare (type fixnum cx cy))
                     (cond
                       ((not sub-set)
                        (when (or (/= cx mem-x) (/= cy mem-y))
                          (return-from search (values cx cy)))
                        (setf sub-set t sub-x x sub-y y))
                       ((or (/= sub-x x) (/= sub-y y))
                        ;; ffmpeg: "BUG I'm pretty sure this isn't the intention" — and it is
                        ;; transcribed anyway, because libvpx is what encoders were written against
                        (return-from search
                          (if (or (/= cx mem-x) (/= cy mem-y)) (values cx cy) (values 0 0))))))
                   (cond
                     ((zerop ,',idx)
                      (return-from search (values (%clamp-mvx ,',st x) (%clamp-mvy ,',st y))))
                     ((not mem-set) (setf mem-set t mem-x x mem-y y))
                     ((or (/= x mem-x) (/= y mem-y))
                      (return-from search
                        (values (%clamp-mvx ,',st x) (%clamp-mvy ,',st y)))))))))
       ,@body)))

(defun %find-ref-mvs (st ref z idx sb)
  "The predicted vector for list Z of this block (6.4.22), as (values x y)."
  (declare (type state st) (type fixnum ref z idx sb) (optimize (speed 3) (safety 1)))
  (let* ((h (st-h st)) (bs (st-bs st))
         (row (st-row st)) (col (st-col st)) (row7 (st-row7 st))
         (stride (* 8 (st-sb-cols st)))
         (cur (st-mvref st)) (prev (st-mvref-prev st))
         (start 0))
    (declare (type fixnum bs row col row7 stride start)
             (type (simple-array fixnum (*)) cur))
    (macrolet ((ref0 (a i) `(aref ,a (+ (* 6 ,i) 0)))
               (ref1 (a i) `(aref ,a (+ (* 6 ,i) 1)))
               (mvx (a i lx) `(aref ,a (+ (* 6 ,i) 2 (* 2 ,lx))))
               (mvy (a i lx) `(aref ,a (+ (* 6 ,i) 3 (* 2 ,lx)))))
      (block search
        (%with-mv-search (st idx sb)
          ;; ---- the block's own earlier sub-blocks, when this is a sub-8x8 partition
          (when (>= sb 0)
            (case sb
              ((1 2) (direct (aref (st-bmv st) 0 z 0) (aref (st-bmv st) 0 z 1)))
              (3 (direct (aref (st-bmv st) 2 z 0) (aref (st-bmv st) 2 z 1))
                 (direct (aref (st-bmv st) 1 z 0) (aref (st-bmv st) 1 z 1))
                 (direct (aref (st-bmv st) 0 z 0) (aref (st-bmv st) 0 z 1))))
            ;; then the immediate above and left neighbours, from the saved edge vectors
            (when (plusp row)
              (let ((i (+ (* (1- row) stride) col)))
                (declare (type fixnum i))
                (cond ((= (ref0 cur i) ref)
                       (let ((n (+ (* 2 col) (logand sb 1))))
                         (candidate (aref (st-above-mv st) n 0 0) (aref (st-above-mv st) n 0 1))))
                      ((= (ref1 cur i) ref)
                       (let ((n (+ (* 2 col) (logand sb 1))))
                         (candidate (aref (st-above-mv st) n 1 0) (aref (st-above-mv st) n 1 1)))))))
            (when (> col (st-tile-col-start st))
              (let ((i (+ (* row stride) col -1)))
                (declare (type fixnum i))
                (cond ((= (ref0 cur i) ref)
                       (let ((n (+ (* 2 row7) (ash sb -1))))
                         (candidate (aref (st-left-mv st) n 0 0) (aref (st-left-mv st) n 0 1))))
                      ((= (ref1 cur i) ref)
                       (let ((n (+ (* 2 row7) (ash sb -1))))
                         (candidate (aref (st-left-mv st) n 1 0) (aref (st-left-mv st) n 1 1)))))))
            (setf start 2))
          ;; ---- eight neighbours, in the order this block size prescribes, same reference
          (loop for i of-type fixnum from start below 8
                do (let ((c (+ (aref +mv-ref-blk-off+ bs i 0) col))
                         (r (+ (aref +mv-ref-blk-off+ bs i 1) row)))
                     (declare (type fixnum c r))
                     (when (and (>= c (st-tile-col-start st)) (< c (st-cols st))
                                (>= r 0) (< r (st-rows st)))
                       (let ((j (+ (* r stride) c)))
                         (declare (type fixnum j))
                         (cond ((= (ref0 cur j) ref)
                                (candidate (mvx cur j 0) (mvy cur j 0)))
                               ((= (ref1 cur j) ref)
                                (candidate (mvx cur j 1) (mvy cur j 1))))))))
          ;; ---- and this position in the previous frame
          (when (and (st-use-last-mvs st) prev)
            (let ((j (+ (* row stride) col)))
              (declare (type fixnum j))
              (let ((p (the (simple-array fixnum (*)) prev)))
                (cond ((= (ref0 p j) ref) (candidate (mvx p j 0) (mvy p j 0)))
                      ((= (ref1 p j) ref) (candidate (mvx p j 1) (mvy p j 1)))))))
          ;; ---- the same two passes again, accepting a DIFFERENT reference.  A vector that points
          ;; the other way in time is negated, which is what makes it usable at all.
          (macrolet ((scaled (mx my flip)
                       `(if ,flip (candidate (- ,mx) (- ,my)) (candidate ,mx ,my))))
            (dotimes (i 8)
              (let ((c (+ (aref +mv-ref-blk-off+ bs i 0) col))
                    (r (+ (aref +mv-ref-blk-off+ bs i 1) row)))
                (declare (type fixnum c r))
                (when (and (>= c (st-tile-col-start st)) (< c (st-cols st))
                           (>= r 0) (< r (st-rows st)))
                  (let ((j (+ (* r stride) c)))
                    (declare (type fixnum j))
                    (let ((r0 (ref0 cur j)) (r1 (ref1 cur j)))
                      (declare (type fixnum r0 r1))
                      (when (and (/= r0 ref) (>= r0 0))
                        (scaled (mvx cur j 0) (mvy cur j 0)
                                (/= (aref (h-sign-bias h) r0) (aref (h-sign-bias h) ref))))
                      ;; ffmpeg: libvpx applies this condition whether or not the first vector was
                      ;; used, and whether or not it was scaled.  Transcribed as it stands.
                      (when (and (/= r1 ref) (>= r1 0)
                                 (or (/= (mvx cur j 0) (mvx cur j 1))
                                     (/= (mvy cur j 0) (mvy cur j 1))))
                        (scaled (mvx cur j 1) (mvy cur j 1)
                                (/= (aref (h-sign-bias h) r1) (aref (h-sign-bias h) ref)))))))))
            (when (and (st-use-last-mvs st) prev)
              (let ((j (+ (* row stride) col))
                    (p (the (simple-array fixnum (*)) prev)))
                (declare (type fixnum j))
                (let ((r0 (ref0 p j)) (r1 (ref1 p j)))
                  (declare (type fixnum r0 r1))
                  (when (and (/= r0 ref) (>= r0 0))
                    (scaled (mvx p j 0) (mvy p j 0)
                            (/= (aref (h-sign-bias h) r0) (aref (h-sign-bias h) ref))))
                  (when (and (/= r1 ref) (>= r1 0)
                             (or (/= (mvx p j 0) (mvx p j 1)) (/= (mvy p j 0) (mvy p j 1))))
                    (scaled (mvx p j 1) (mvy p j 1)
                            (/= (aref (h-sign-bias h) r1) (aref (h-sign-bias h) ref))))))))
          ;; nothing agreed: zero, clamped, which for a block at the picture's edge is not zero
          (values (%clamp-mvx st 0) (%clamp-mvy st 0)))))))

;;; ---- and the coded difference --------------------------------------------------------------------

(defun %read-mv-component (st idx hp)
  "One component of a coded vector (6.4.23): a sign, a magnitude class, the bits that refine it,
   an eighth, and — only when the frame allows it — a sixteenth.

   THE VALUE IS ONE MORE THAN WHAT IS CODED, because a coded zero would be a vector the mode
   already covers."
  (declare (type state st) (type fixnum idx) (optimize (speed 3) (safety 1)))
  (let* ((c (st-c st)) (p (fp-p (st-fp st))) (cn (st-counts st))
         (sign (bool-bit c (aref (pr-mv-sign p) idx)))
         (cl (bool-tree c +mv-class-tree+ (pr-mv-classes p) (* 10 idx)))
         (n 0))
    (declare (type fixnum sign cl n))
    (incf (aref (cn-mv-sign cn) idx sign))
    (incf (aref (cn-mv-classes cn) idx cl))
    (if (plusp cl)
        (progn
          (dotimes (m cl)
            (let ((bit (bool-bit c (aref (pr-mv-bits p) idx m))))
              (declare (type fixnum bit))
              (incf (aref (cn-mv-bits cn) idx m bit))
              (setf n (logior n (ash bit m)))))
          (setf n (ash n 3))
          (let ((bit (bool-tree c +mv-fp-tree+ (pr-mv-fp p) (* 3 idx))))
            (declare (type fixnum bit))
            (incf (aref (cn-mv-fp cn) idx bit))
            (setf n (logior n (ash bit 1))))
          ;; THE SIXTEENTH IS COUNTED EVEN WHEN IT IS NOT CODED, which is a bug in libvpx and is
          ;; therefore the format: a decoder that counts honestly adapts differently from every
          ;; encoder in existence.
          (if (plusp hp)
              (let ((bit (bool-bit c (aref (pr-mv-hp p) idx))))
                (declare (type fixnum bit))
                (incf (aref (cn-mv-hp cn) idx bit))
                (setf n (logior n bit)))
              (progn (incf (aref (cn-mv-hp cn) idx 1)) (setf n (logior n 1))))
          (incf n (ash 8 cl)))
        (progn
          (setf n (bool-bit c (aref (pr-mv-class0 p) idx)))
          (incf (aref (cn-mv-class0 cn) idx n))
          (let ((bit (bool-tree c +mv-fp-tree+ (pr-mv-class0-fp p) (+ (* 6 idx) (* 3 n)))))
            (declare (type fixnum bit))
            (incf (aref (cn-mv-class0-fp cn) idx n bit))
            (setf n (logior (ash n 3) (ash bit 1))))
          (if (plusp hp)
              (let ((bit (bool-bit c (aref (pr-mv-class0-hp p) idx))))
                (declare (type fixnum bit))
                (incf (aref (cn-mv-class0-hp cn) idx bit))
                (setf n (logior n bit)))
              (progn (incf (aref (cn-mv-class0-hp cn) idx 1)) (setf n (logior n 1))))))
    (if (plusp sign) (- (1+ n)) (1+ n))))

(defun %fill-mv (st which mode sb)
  "The motion vector or vectors of one partition, into sub-block WHICH of the block (6.4.22)."
  (declare (type state st) (type fixnum which mode sb) (optimize (speed 3) (safety 1)))
  (let ((h (st-h st)) (mv (st-bmv st)))
    (if (= mode +zeromv+)
        (dotimes (lx 2) (setf (aref mv which lx 0) 0 (aref mv which lx 1) 0))
        (dotimes (lx (if (st-bcomp st) 2 1))
          (multiple-value-bind (x y)
              (%find-ref-mvs st (aref (st-bref st) lx) lx (if (= mode +nearmv+) 1 0)
                             (if (= mode +newmv+) -1 sb))
            (declare (type fixnum x y))
            ;; A VECTOR IS ROUNDED TO A HALF-SAMPLE unless the frame allows sixteenths AND the
            ;; predictor is small.  Rounding towards zero, which is not the same as truncating.
            (let ((hp (if (and (h-high-precision-mv h) (< (abs x) 64) (< (abs y) 64)) 1 0)))
              (declare (type fixnum hp))
              (when (and (or (= mode +newmv+) (= sb -1)) (zerop hp))
                (when (logbitp 0 y) (if (minusp y) (incf y) (decf y)))
                (when (logbitp 0 x) (if (minusp x) (incf x) (decf x))))
              (when (= mode +newmv+)
                (let ((j (bool-tree (st-c st) +mv-joint-tree+ (pr-mv-joint (fp-p (st-fp st))) 0)))
                  (declare (type fixnum j))
                  (incf (aref (cn-mv-joint (st-counts st)) j))
                  (when (>= j 2) (incf y (%read-mv-component st 0 hp)))
                  (when (logbitp 0 j) (incf x (%read-mv-component st 1 hp)))))
              (setf (aref mv which lx 0) x (aref mv which lx 1) y)))))
    (unless (st-bcomp st)
      (setf (aref mv which 1 0) 0 (aref mv which 1 1) 0))
    (values)))

;;; ---- an inter block's references, mode, filter and vectors ---------------------------------------

(defun %decode-inter-mode (st h p c bs row row7 col have-a have-l)
  "Everything an inter block codes, in order, and the reference this block leaves in the context.

   The order is not negotiable and is not obvious: the references come first, then — for a sub-8x8
   block only — a single mode for the whole block, then the interpolation filter, then for a larger
   block the mode and vectors per partition.  A sub-8x8 block reads its mode BEFORE the filter and a
   larger one after, which is the one place the syntax depends on the block size in a way that no
   context does."
  (declare (type state st) (type fixnum bs row row7 col) (optimize (speed 3) (safety 1)))
  (let ((mv (st-bmv st)) (m (st-mode st)) (fixref (h-fix-comp-ref h)))
    (declare (type fixnum fixref))
    ;; ---- which reference or references
    (if (and (h-seg-enabled h) (plusp (aref (h-seg-feature-on h) (st-seg-id st) 2)))
        (setf (st-bcomp st) nil
              (aref (st-bref st) 0) (1- (aref (h-seg-feature h) (st-seg-id st) 2)))
        (progn
          (setf (st-bcomp st)
                (if (/= (fp-comp-pred-mode (st-fp st)) +pred-switchable+)
                    (= (fp-comp-pred-mode (st-fp st)) +pred-comp+)
                    (let* ((k (%ref-ctx-comp st h have-a have-l col row7 fixref))
                           (bit (bool-bit c (aref (pr-comp p) k))))
                      (declare (type fixnum k bit))
                      (incf (aref (cn-comp (st-counts st)) k bit))
                      (plusp bit))))
          (if (st-bcomp st)
              ;; one reference is fixed by the frame header and the other is coded
              (let* ((fix-idx (aref (h-sign-bias h) fixref))
                     (var-idx (- 1 fix-idx))
                     (k (%ref-ctx-compref st h have-a have-l col row7 fixref)))
                (declare (type fixnum fix-idx var-idx k))
                (setf (aref (st-bref st) fix-idx) fixref)
                (let ((bit (bool-bit c (aref (pr-comp-ref p) k))))
                  (declare (type fixnum bit))
                  (incf (aref (cn-comp-ref (st-counts st)) k bit))
                  (setf (aref (st-bref st) var-idx) (aref (h-var-comp-ref h) bit))))
              (let* ((k (%ref-ctx-single0 st h have-a have-l col row7 fixref))
                     (bit (bool-bit c (aref (pr-single-ref p) k 0))))
                (declare (type fixnum k bit))
                (incf (aref (cn-single-ref (st-counts st)) k 0 bit))
                (if (zerop bit)
                    (setf (aref (st-bref st) 0) 0)
                    (let* ((k2 (%ref-ctx-single1 st h have-a have-l col row7 fixref))
                           (b2 (bool-bit c (aref (pr-single-ref p) k2 1))))
                      (declare (type fixnum k2 b2))
                      (incf (aref (cn-single-ref (st-counts st)) k2 1 b2))
                      (setf (aref (st-bref st) 0) (1+ b2))))))))
    ;; ---- a sub-8x8 block has one mode for all four of its partitions, read here
    (when (<= bs +bs-8x8+)
      (if (and (h-seg-enabled h) (plusp (aref (st-seg-skip st) (st-seg-id st))))
          (dotimes (i 4) (setf (aref m i) +zeromv+))
          (let* ((off (aref +sub8x8-mode-off+ bs))
                 (k (aref +inter-mode-ctx-lut+
                          (aref (st-above-mode st) (+ col off))
                          (aref (st-left-mode st) (+ row7 off))))
                 (v (bool-tree c +inter-mode-tree+ (pr-mv-mode p) (* 3 k))))
            (declare (type fixnum off k v))
            (incf (aref (cn-mv-mode (st-counts st)) k (- v 10)))
            (dotimes (i 4) (setf (aref m i) v)))))
    ;; ---- the interpolation filter
    (if (= 3 (h-filter-mode h))
        (let ((k (cond ((and have-a (>= (aref (st-above-mode st) col) +nearestmv+))
                        (if (and have-l (>= (aref (st-left-mode st) row7) +nearestmv+))
                            (if (= (aref (st-above-filter st) col)
                                   (aref (st-left-filter st) row7))
                                (aref (st-left-filter st) row7)
                                3)
                            (aref (st-above-filter st) col)))
                       ((and have-l (>= (aref (st-left-mode st) row7) +nearestmv+))
                        (aref (st-left-filter st) row7))
                       (t 3))))
          (declare (type fixnum k))
          (setf (st-bfilter-id st) (bool-tree c +filter-tree+ (pr-filter p) (* 2 k))
                (st-bfilter st) (aref +filter-lut+ (st-bfilter-id st)))
          (incf (aref (cn-filter (st-counts st)) k (st-bfilter-id st))))
        (setf (st-bfilter st) (h-filter-mode h) (st-bfilter-id st) 0))
    ;; ---- and the modes and vectors of the partitions
    (if (> bs +bs-8x8+)
        (let ((k (aref +inter-mode-ctx-lut+
                       (aref (st-above-mode st) col) (aref (st-left-mode st) row7))))
          (declare (type fixnum k))
          (macrolet ((imode () '(let ((v (bool-tree c +inter-mode-tree+ (pr-mv-mode p) (* 3 k))))
                                  (incf (aref (cn-mv-mode (st-counts st)) k (- v 10))) v)))
           (setf (aref m 0) (imode))
           (%fill-mv st 0 (aref m 0) 0)
           (if (/= bs +bs-8x4+)
              (progn (setf (aref m 1) (imode))
                     (%fill-mv st 1 (aref m 1) 1))
              (progn (setf (aref m 1) (aref m 0))
                     (dotimes (lx 2) (setf (aref mv 1 lx 0) (aref mv 0 lx 0)
                                           (aref mv 1 lx 1) (aref mv 0 lx 1)))))
           (if (/= bs +bs-4x8+)
              (progn
                (setf (aref m 2) (imode))
                (%fill-mv st 2 (aref m 2) 2)
                (if (/= bs +bs-8x4+)
                    (progn (setf (aref m 3) (imode))
                           (%fill-mv st 3 (aref m 3) 3))
                    (progn (setf (aref m 3) (aref m 2))
                           (dotimes (lx 2) (setf (aref mv 3 lx 0) (aref mv 2 lx 0)
                                                 (aref mv 3 lx 1) (aref mv 2 lx 1))))))
              (progn
                (setf (aref m 2) (aref m 0) (aref m 3) (aref m 1))
                (dotimes (lx 2)
                  (setf (aref mv 2 lx 0) (aref mv 0 lx 0) (aref mv 2 lx 1) (aref mv 0 lx 1)
                        (aref mv 3 lx 0) (aref mv 1 lx 0) (aref mv 3 lx 1) (aref mv 1 lx 1)))))))
        (progn
          (%fill-mv st 0 (aref m 0) -1)
          (loop for i of-type fixnum from 1 to 3
                do (dotimes (lx 2) (setf (aref mv i lx 0) (aref mv 0 lx 0)
                                         (aref mv i lx 1) (aref mv 0 lx 1))))))
    ;; THE REFERENCE LEFT IN THE CONTEXT is the variable one of a compound pair, not the fixed one:
    ;; the fixed one carries no information, since the frame header already said what it is.
    (if (st-bcomp st)
        (aref (st-bref st) (aref (h-sign-bias h) (aref (h-var-comp-ref h) 0)))
        (aref (st-bref st) 0))))

(defparameter +filter-lut+
  (make-array 3 :element-type 'fixnum :initial-contents '(1 0 2))
  "The filter tree's three symbols are not the filter numbers: the tree is ordered by how often each
   is chosen and the filters by how smooth they are.")
(declaim (type (simple-array fixnum (3)) +filter-lut+))

(defun %store-mv-contexts (st bs row7 col bw4 bh4)
  "The vectors a later block will read as its above and left neighbours.

   A block larger than 8x8 stores its FIRST and LAST sub-block vectors separately, because a
   neighbour reading the top half of it and one reading the bottom half must see different answers;
   a smaller block stores one vector everywhere."
  (declare (type state st) (type fixnum bs row7 col bw4 bh4))
  (let ((mv (st-bmv st)) (a (st-above-mv st)) (l (st-left-mv st)))
    (if (> bs +bs-8x8+)
        (dotimes (lx 2)
          (setf (aref l (* 2 row7) lx 0) (aref mv 1 lx 0)
                (aref l (* 2 row7) lx 1) (aref mv 1 lx 1)
                (aref l (1+ (* 2 row7)) lx 0) (aref mv 3 lx 0)
                (aref l (1+ (* 2 row7)) lx 1) (aref mv 3 lx 1)
                (aref a (* 2 col) lx 0) (aref mv 2 lx 0)
                (aref a (* 2 col) lx 1) (aref mv 2 lx 1)
                (aref a (1+ (* 2 col)) lx 0) (aref mv 3 lx 0)
                (aref a (1+ (* 2 col)) lx 1) (aref mv 3 lx 1)))
        (dotimes (lx 2)
          (dotimes (n (* 2 bw4))
            (when (< (+ (* 2 col) n) (array-dimension a 0))
              (setf (aref a (+ (* 2 col) n) lx 0) (aref mv 3 lx 0)
                    (aref a (+ (* 2 col) n) lx 1) (aref mv 3 lx 1))))
          (dotimes (n (* 2 bh4))
            (when (< (+ (* 2 row7) n) 16)
              (setf (aref l (+ (* 2 row7) n) lx 0) (aref mv 3 lx 0)
                    (aref l (+ (* 2 row7) n) lx 1) (aref mv 3 lx 1))))))))

(defun %store-mvref (st row col w4 h4)
  "And the reference and vector of every eight-sample block this one covers, which is what the NEXT
   frame's vector prediction will read."
  (declare (type state st) (type fixnum row col w4 h4))
  (let ((a (st-mvref st)) (stride (* 8 (st-sb-cols st))) (mv (st-bmv st)))
    (declare (type (simple-array fixnum (*)) a) (type fixnum stride))
    (dotimes (y h4)
      (let ((o (* 6 (+ (* (+ row y) stride) col))))
        (declare (type fixnum o))
        (dotimes (x w4)
          (let ((i (+ o (* 6 x))))
            (declare (type fixnum i))
            (cond
              ((st-intra st) (setf (aref a i) -1 (aref a (+ i 1)) -1))
              ((st-bcomp st)
               (setf (aref a i) (aref (st-bref st) 0)
                     (aref a (+ i 1)) (aref (st-bref st) 1)
                     (aref a (+ i 2)) (aref mv 3 0 0) (aref a (+ i 3)) (aref mv 3 0 1)
                     (aref a (+ i 4)) (aref mv 3 1 0) (aref a (+ i 5)) (aref mv 3 1 1)))
              (t (setf (aref a i) (aref (st-bref st) 0)
                       (aref a (+ i 1)) -1
                       (aref a (+ i 2)) (aref mv 3 0 0)
                       (aref a (+ i 3)) (aref mv 3 0 1))))))))))

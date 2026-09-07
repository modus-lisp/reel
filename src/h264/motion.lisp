;;;; h264/motion.lisp — inter prediction: where a block came from, and how it is resampled.
;;;;
;;;; Two separate jobs live here and it is worth keeping them apart in your head.
;;;;
;;;; PREDICTING THE VECTOR (8.4.1.3).  A motion vector is coded as a difference from a prediction
;;;; made out of the neighbours already decoded, so getting the prediction wrong corrupts the
;;;; picture without desynchronising the bitstream — the same failure mode as the intra mode
;;;; prediction, and just as hard to see.  The rule is a component-wise median of the left, above
;;;; and above-right neighbours, with enough special cases that the median is almost the exception.
;;;;
;;;; RESAMPLING (8.4.2.2).  Vectors are in quarter samples, so most blocks are copied from between
;;;; the reference picture's samples.  Half positions come from a six-tap filter, quarter positions
;;;; from averaging a half position with its neighbour, and the centre position from applying the
;;;; six-tap to the *unrounded* output of the six-tap — rounding early there is a classic off-by-one
;;;; that shows as a faint checkerboard nobody notices until the errors accumulate over a GOP.

(in-package #:reel.h264)

;;; ---- reading the reference picture ---------------------------------------------------------------

(declaim (inline %ref-luma %ref-chroma))
(defun %ref-luma (pic x y)
  "One luma sample of PIC, with coordinates CLAMPED to the picture.

   Clamping rather than bounds-checking is the specification's own rule (8.4.2.2.1): a vector may
   point outside the picture entirely, and the edge sample is repeated for as far as it does.  The
   padding around the planes is not what makes this safe — a vector can be much larger than the
   padding — the clamp is."
  (declare (type picture pic) (type fixnum x y) (optimize (speed 3) (safety 0)))
  (let ((w (* 16 (pic-mb-width pic))) (h (* 16 (pic-mb-height pic))))
    (declare (type fixnum w h))
    (let ((cx (if (< x 0) 0 (if (>= x w) (1- w) x)))
          (cy (if (< y 0) 0 (if (>= y h) (1- h) y))))
      (declare (type fixnum cx cy))
      (aref (pic-y pic) (+ (pic-yoff pic) (* cy (pic-ystride pic)) cx)))))

(defun %ref-chroma (plane pic x y)
  (declare (type (simple-array (unsigned-byte 8) (*)) plane) (type picture pic) (type fixnum x y)
           (optimize (speed 3) (safety 0)))
  (let ((w (* 8 (pic-mb-width pic))) (h (* 8 (pic-mb-height pic))))
    (declare (type fixnum w h))
    (let ((cx (if (< x 0) 0 (if (>= x w) (1- w) x)))
          (cy (if (< y 0) 0 (if (>= y h) (1- h) y))))
      (declare (type fixnum cx cy))
      (aref plane (+ (pic-coff pic) (* cy (pic-cstride pic)) cx)))))

;;; ---- the six-tap, and the sixteen sample positions ------------------------------------------------

(declaim (inline %tap6))
(defun %tap6 (a b c d e f)
  (declare (type fixnum a b c d e f) (optimize (speed 3) (safety 0)))
  (+ a (* -5 b) (* 20 c) (* 20 d) (* -5 e) f))

(declaim (inline %h6 %v6))
(defun %h6 (ref x y)
  "The unrounded horizontal half-sample between (X,Y) and (X+1,Y)."
  (declare (type fixnum x y) (optimize (speed 3) (safety 0)))
  (%tap6 (%ref-luma ref (- x 2) y) (%ref-luma ref (- x 1) y) (%ref-luma ref x y)
         (%ref-luma ref (+ x 1) y) (%ref-luma ref (+ x 2) y) (%ref-luma ref (+ x 3) y)))

(defun %v6 (ref x y)
  "The unrounded vertical half-sample between (X,Y) and (X,Y+1)."
  (declare (type fixnum x y) (optimize (speed 3) (safety 0)))
  (%tap6 (%ref-luma ref x (- y 2)) (%ref-luma ref x (- y 1)) (%ref-luma ref x y)
         (%ref-luma ref x (+ y 1)) (%ref-luma ref x (+ y 2)) (%ref-luma ref x (+ y 3))))

(declaim (inline %r5 %avg))
(defun %r5 (v) (declare (type fixnum v) (optimize (speed 3) (safety 0))) (clamp255 (ash (+ v 16) -5)))
(defun %avg (a b) (declare (type fixnum a b) (optimize (speed 3) (safety 0))) (ash (+ a b 1) -1))

(defun luma-sample (ref x y xf yf)
  "The luma sample of REF at (X + XF/4, Y + YF/4), by 8.4.2.2.1.

   The letters are the specification's: b and h are the half positions right and below the integer
   sample G, j is the centre, m is the half position below G's right neighbour and s the one right
   of G's lower neighbour.  Every quarter position is the average of two of those."
  (declare (type fixnum x y xf yf) (optimize (speed 3) (safety 0)))
  (cond
    ;; integer position
    ((and (zerop xf) (zerop yf)) (%ref-luma ref x y))
    ;; along the top row: only the horizontal half is needed
    ((zerop yf)
     (let ((b (%r5 (%h6 ref x y))))
       (case xf (1 (%avg (%ref-luma ref x y) b)) (2 b) (t (%avg (%ref-luma ref (1+ x) y) b)))))
    ;; down the left column: only the vertical half
    ((zerop xf)
     (let ((h (%r5 (%v6 ref x y))))
       (case yf (1 (%avg (%ref-luma ref x y) h)) (2 h) (t (%avg (%ref-luma ref x (1+ y)) h)))))
    (t
     ;; anywhere else the centre position j is needed, and j comes from filtering the UNROUNDED
     ;; half-sample values, not the rounded ones
     (let* ((b (%r5 (%h6 ref x y)))
            (h (%r5 (%v6 ref x y)))
            (j (let ((j1 (%tap6 (%h6 ref x (- y 2)) (%h6 ref x (- y 1)) (%h6 ref x y)
                                (%h6 ref x (+ y 1)) (%h6 ref x (+ y 2)) (%h6 ref x (+ y 3)))))
                 (clamp255 (ash (+ j1 512) -10)))))
       (declare (type fixnum b h j))
       (if (= xf 2)
           (case yf (1 (%avg b j)) (2 j) (t (%avg j (%r5 (%h6 ref x (1+ y))))))   ; f, j, q
           (if (= yf 2)
               (if (= xf 1) (%avg h j) (%avg j (%r5 (%v6 ref (1+ x) y))))          ; i, k
               ;; the four true diagonals: e, g, p, r
               (let ((m (%r5 (%v6 ref (1+ x) y)))
                     (s (%r5 (%h6 ref x (1+ y)))))
                 (declare (type fixnum m s))
                 (if (= yf 1)
                     (if (= xf 1) (%avg b h) (%avg b m))                           ; e, g
                     (if (= xf 1) (%avg h s) (%avg m s))))))))))                   ; p, r

;;; ---- copying a partition out of the reference ----------------------------------------------------

(defun predict-luma (dst stride base ref px py w h mvx mvy)
  "Fill a W x H luma partition at DST/BASE from REF, displaced by the quarter-pel vector."
  (declare (type (simple-array (unsigned-byte 8) (*)) dst)
           (type fixnum stride base px py w h mvx mvy)
           (optimize (speed 3) (safety 1)))
  (let ((xi (+ px (ash mvx -2))) (yi (+ py (ash mvy -2)))
        (xf (logand mvx 3)) (yf (logand mvy 3)))
    (declare (type fixnum xi yi xf yf))
    (dotimes (j h)
      (declare (type fixnum j))
      (let ((row (+ base (* j stride))))
        (declare (type fixnum row))
        (dotimes (i w)
          (declare (type fixnum i))
          (setf (aref dst (+ row i)) (luma-sample ref (+ xi i) (+ yi j) xf yf)))))))

(defun predict-chroma (dst plane stride base ref px py w h mvx mvy)
  "Fill a W x H chroma partition.  4:2:0, so the luma vector addresses chroma in EIGHTHS of a
   sample, and the resampling is a plain bilinear rather than the luma six-tap (8.4.2.2.2)."
  (declare (type (simple-array (unsigned-byte 8) (*)) dst plane)
           (type fixnum stride base px py w h mvx mvy)
           (optimize (speed 3) (safety 1)))
  (let ((xi (+ px (ash mvx -3))) (yi (+ py (ash mvy -3)))
        (xf (logand mvx 7)) (yf (logand mvy 7)))
    (declare (type fixnum xi yi xf yf))
    (let ((a (* (- 8 xf) (- 8 yf))) (b (* xf (- 8 yf)))
          (c (* (- 8 xf) yf)) (d (* xf yf)))
      (declare (type fixnum a b c d))
      (dotimes (j h)
        (declare (type fixnum j))
        (let ((row (+ base (* j stride))) (sy (+ yi j)))
          (declare (type fixnum row sy))
          (dotimes (i w)
            (declare (type fixnum i))
            (let ((sx (+ xi i)))
              (declare (type fixnum sx))
              (setf (aref dst (+ row i))
                    (ash (+ (* a (%ref-chroma plane ref sx sy))
                            (* b (%ref-chroma plane ref (1+ sx) sy))
                            (* c (%ref-chroma plane ref sx (1+ sy)))
                            (* d (%ref-chroma plane ref (1+ sx) (1+ sy)))
                            32)
                         -6)))))))))

;;; ---- predicting the vector ------------------------------------------------------------------------

(defun %neighbour-motion (ss bx by &optional (lx 0))
  "(values mvx mvy ref available-p) for the 4x4 block at picture block coordinates BX,BY.

   An unavailable neighbour answers a zero vector and reference -1, which is what the prediction
   rules expect to see rather than a special case at every use."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss))
         (mbw (pic-mb-width pic)) (mbh (pic-mb-height pic)))
    (if (or (minusp bx) (minusp by) (>= bx (* 4 mbw)) (>= by (* 4 mbh)))
        (values 0 0 -1 nil)
        (let ((mbx (floor bx 4)) (mby (floor by 4)))
          ;; the neighbour must be decoded already; raster order and one slice per picture make
          ;; that the same test macroblock availability uses everywhere else
          (cond
            ((= -1 (aref (pic-mb-types pic) (+ (* mby mbw) mbx))) (values 0 0 -1 nil))
            ;; inside the macroblock being decoded, a block whose partition has not been reached
            ;; yet is NOT AVAILABLE (6.4.11.7) — not merely zero.  See SS-MB-DONE.
            ((and (= mbx (ss-mbx ss)) (= mby (ss-mby ss))
                  (zerop (logand (ss-mb-done ss)
                                 (ash 1 (+ (* 4 (mod by 4)) (mod bx 4))))))
             (values 0 0 -1 nil))
            (t (multiple-value-bind (mvx mvy ref) (blk-mv pic bx by lx)
                 (values mvx mvy ref t))))))))

(declaim (inline %median3))
(defun %median3 (a b c)
  (declare (type fixnum a b c) (optimize (speed 3) (safety 0)))
  (max (min a b) (min (max a b) c)))

(defun predict-mv (ss bx by pw ref-idx &key shape part (lx 0))
  "(values mvpx mvpy): the predicted motion vector for a partition whose top-left 4x4 block is at
   picture block coordinates BX,BY and which is PW blocks wide, referring to REF-IDX.

   SHAPE and PART name the directional special cases of 8.4.1.3, which exist because a 16x8's top
   half really is more like the macroblock above it than like a median of three neighbours, and a
   8x16's left half more like the one to its left.  Everything else takes the median."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-bind (ax ay aref a-ok) (%neighbour-motion ss (1- bx) by lx)
    (multiple-value-bind (bx* by* bref b-ok) (%neighbour-motion ss bx (1- by) lx)
      ;; C is above-right of the whole partition; when it is not available D, above-left, stands in
      (multiple-value-bind (cx cy cref c-ok) (%neighbour-motion ss (+ bx pw) (1- by) lx)
        (unless c-ok
          (multiple-value-setq (cx cy cref c-ok) (%neighbour-motion ss (1- bx) (1- by) lx)))
        ;; the directional cases first: each is a whole answer, not a tweak to the median
        (when (and shape part)
          (cond
            ((and (eq shape :16x8) (= part 0) (= bref ref-idx)) (return-from predict-mv (values bx* by*)))
            ((and (eq shape :16x8) (= part 1) (= aref ref-idx)) (return-from predict-mv (values ax ay)))
            ((and (eq shape :8x16) (= part 0) (= aref ref-idx)) (return-from predict-mv (values ax ay)))
            ((and (eq shape :8x16) (= part 1) (= cref ref-idx)) (return-from predict-mv (values cx cy)))))
        ;; with nothing above at all, the left neighbour stands in for both B and C, and this
        ;; happens BEFORE the count below — otherwise the top row of every picture takes a median
        ;; of one real vector and two zeros
        (when (and (not b-ok) (not c-ok) a-ok)
          (setf bx* ax by* ay bref aref
                cx ax cy ay cref aref))
        (let ((matches (+ (if (= aref ref-idx) 1 0)
                          (if (= bref ref-idx) 1 0)
                          (if (= cref ref-idx) 1 0))))
          (declare (type fixnum matches))
          (cond
            ;; exactly one neighbour used this reference picture: believe it rather than the median
            ((= matches 1)
             (cond ((= aref ref-idx) (values ax ay))
                   ((= bref ref-idx) (values bx* by*))
                   (t (values cx cy))))
            (t (values (%median3 ax bx* cx) (%median3 ay by* cy)))))))))

(defun skip-mv (ss)
  "The motion vector of a P_Skip macroblock (8.4.1.1).

   Zero whenever a neighbour is missing or is itself a zero vector into the same reference — which
   is what makes a run of skipped macroblocks in still content cost nothing at all — and otherwise
   the ordinary 16x16 prediction."
  (declare (optimize (speed 3) (safety 1)))
  (let ((bx (* 4 (ss-mbx ss))) (by (* 4 (ss-mby ss))))
    (multiple-value-bind (ax ay aref a-ok) (%neighbour-motion ss (1- bx) by 0)
      (multiple-value-bind (bvx bvy bref b-ok) (%neighbour-motion ss bx (1- by) 0)
        (if (or (not a-ok) (not b-ok)
                (and (zerop aref) (zerop ax) (zerop ay))
                (and (zerop bref) (zerop bvx) (zerop bvy)))
            (values 0 0)
            (predict-mv ss bx by 4 0))))))

;;; ---- weighted prediction (8.4.2.3) ---------------------------------------------------------------

(defun apply-weight (plane stride base w h weight offset log2-denom)
  "Scale and offset a predicted partition in place.

   The rounding term exists only when the denominator does: at a denominator of zero there is
   nothing to round and adding half of nothing shifts every sample by one."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane)
           (type fixnum stride base w h weight offset log2-denom)
           (optimize (speed 3) (safety 1)))
  (let ((round (if (plusp log2-denom) (ash 1 (1- log2-denom)) 0)))
    (declare (type fixnum round))
    (dotimes (j h)
      (let ((row (+ base (* j stride))))
        (declare (type fixnum row))
        (dotimes (i w)
          (let* ((v (aref plane (+ row i)))
                 (scaled (if (plusp log2-denom)
                             (+ (ash (+ (* v weight) round) (- log2-denom)) offset)
                             (+ (* v weight) offset))))
            (declare (type fixnum v scaled))
            (setf (aref plane (+ row i)) (clamp255 scaled))))))))

;;; ---- bi-prediction -------------------------------------------------------------------------------

(defun average-into (dst dstride dbase src sstride sbase w h)
  "DST = (DST + SRC + 1) >> 1 over a W x H rectangle.

   The rounding is up, and it is not optional: a decoder that truncates here drifts half a level
   darker on every bi-predicted block and the error compounds through the sequence."
  (declare (type (simple-array (unsigned-byte 8) (*)) dst src)
           (type fixnum dstride dbase sstride sbase w h)
           (optimize (speed 3) (safety 1)))
  (dotimes (j h)
    (let ((d (+ dbase (* j dstride))) (s (+ sbase (* j sstride))))
      (declare (type fixnum d s))
      (dotimes (i w)
        (setf (aref dst (+ d i))
              (ash (+ (aref dst (+ d i)) (aref src (+ s i)) 1) -1))))))

(defun weighted-average-into (dst dstride dbase src sstride sbase w h w0 w1 log2-denom o0 o1)
  "DST = the WEIGHTED mean of the two predictions (8.4.2.3.2).

   One rounding, not two: the two predictions are combined and rounded once, which is why this
   cannot be expressed as weighting each side and then averaging."
  (declare (type (simple-array (unsigned-byte 8) (*)) dst src)
           (type fixnum dstride dbase sstride sbase w h w0 w1 log2-denom o0 o1)
           (optimize (speed 3) (safety 1)))
  (let ((round (ash 1 log2-denom))
        (shift (1+ log2-denom))
        (off (ash (+ o0 o1 1) -1)))
    (declare (type fixnum round shift off))
    (dotimes (j h)
      (let ((d (+ dbase (* j dstride))) (s (+ sbase (* j sstride))))
        (declare (type fixnum d s))
        (dotimes (i w)
          (setf (aref dst (+ d i))
                (clamp255 (+ (ash (+ (* (aref dst (+ d i)) w0)
                                     (* (aref src (+ s i)) w1)
                                     round)
                                  (- shift))
                             off))))))))

(defun implicit-bi-weights (curr-poc poc0 poc1)
  "(values w0 w1) for implicit weighted bi-prediction (8.4.2.3.1).

   The weights come from WHERE the two references sit relative to this picture: a reference twice
   as far away counts half as much.  No weights are transmitted at all — both ends derive them from
   the order counts, which is why an encoder can turn this on for free.

   Out-of-range distances fall back to an even split, which is also what a zero distance means."
  (let* ((tb (max -128 (min 127 (- curr-poc poc0))))
         (td (max -128 (min 127 (- poc1 poc0)))))
    (if (zerop td)
        (values 32 32)
        ;; truncating division, for the same reason as the temporal direct scaling
        (let* ((tx (truncate (+ 16384 (abs (truncate td 2))) td))
               (dsf (max -1024 (min 1023 (ash (+ (* tb tx) 32) -6))))
               (w1 (ash dsf -2)))
          (if (or (< w1 -64) (> w1 128))
              (values 32 32)
              (values (- 64 w1) w1))))))

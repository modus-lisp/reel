;;;; hevc/filter.lisp — the two in-loop filters: deblocking (8.7.2) and SAO (8.7.3).
;;;;
;;;; DEBLOCKING differs from H.264's in three ways that matter to a decoder rather than to a
;;;; viewer.  It runs on the EIGHT-sample grid, not the four — a transform block's internal edges
;;;; are not filtered at all, so there are a quarter as many of them.  It runs over the whole
;;;; picture in two passes, every vertical edge before any horizontal one, rather than per
;;;; macroblock: the horizontal pass reads what the vertical pass wrote, and doing them per block
;;;; gets a different answer.  And its strong filter reaches three samples deep on each side
;;;; instead of two, with the decision to use it taken per four-line segment from two of those
;;;; lines rather than per line.
;;;;
;;;; SAMPLE ADAPTIVE OFFSET has no counterpart in H.264 at all.  It is a per-coding-tree-block
;;;; table of offsets added after deblocking, either by intensity BAND — four adjacent bands of the
;;;; sample range get four offsets — or by EDGE, where each sample is compared with two neighbours
;;;; along one of four directions and the offset depends on whether it sits in a valley, on a step,
;;;; or on a peak.  It is a cheap way to undo the systematic brightness error that quantisation
;;;; leaves in a region, which is exactly what a deblocking filter cannot touch.
;;;;
;;;; SAO READS THE DEBLOCKED PICTURE AND WRITES A COPY.  Every comparison it makes must see samples
;;;; as deblocking left them, so a filter that works in place feeds its own output back into the
;;;; next sample's decision and drifts.

(in-package #:reel.hevc)

(defparameter +beta-table+
  (make-array 52 :element-type '(unsigned-byte 8) :initial-contents
              '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 7 8
                9 10 11 12 13 14 15 16 17 18 20 22 24 26 28 30 32 34 36
                38 40 42 44 46 48 50 52 54 56 58 60 62 64))
  "beta' by quantiser (Table 8-12): how much variation across an edge still counts as flat.")

(defparameter +tc-table+
  (make-array 54 :element-type '(unsigned-byte 8) :initial-contents
              '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
                1 1 1 1 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4
                5 5 6 6 7 8 9 10 11 13 14 16 18 20 22 24))
  "tc' by quantiser (Table 8-12): the most any one sample may be moved.")

(declaim (type (simple-array (unsigned-byte 8) (52)) +beta-table+))
(declaim (type (simple-array (unsigned-byte 8) (54)) +tc-table+))

(declaim (inline %clip3 %clip8))
(defun %clip3 (lo hi v) (declare (type fixnum lo hi v)) (max lo (min hi v)))
(defun %clip8 (v) (declare (type fixnum v)) (max 0 (min 255 v)))

;;; ---- deblocking ---------------------------------------------------------------------------------

(defun %filter-luma-edge (plane base step line-step bs qp beta-off tc-off allow-p allow-q)
  "One four-line segment of a luma edge.

   STEP walks ACROSS the edge and LINE-STEP along it, so the same code filters a vertical edge and
   a horizontal one; BASE is the first sample on the q side of the first line.

   The decision is taken once for the whole segment, from lines 0 and 3 only.  That is not an
   approximation for speed — it is what the standard says, and taking it per line gives a
   different picture.

   ALLOW-P and ALLOW-Q are separate because the exemptions are.  A block sent exactly — transquant
   bypass, or PCM with the filter disabled — must come out exactly, but that says nothing about the
   block on the OTHER side of the edge, which is still filtered against it.  Suppressing the whole
   edge instead is wrong on one side either way."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane)
           (type fixnum base step line-step bs qp beta-off tc-off)
           (optimize (speed 3) (safety 1)))
  (let* ((beta (aref +beta-table+ (%clip3 0 51 (+ qp beta-off))))
         (tc (aref +tc-table+ (%clip3 0 53 (+ qp (* 2 (1- bs)) tc-off)))))
    (declare (type fixnum beta tc))
    (when (zerop beta) (return-from %filter-luma-edge nil))
    (macrolet ((s (line off) `(aref plane (+ base (* ,line line-step) (* ,off step)))))
      (let* ((dp0 (abs (- (s 0 -3) (* 2 (s 0 -2)) (- (s 0 -1)))))
             (dq0 (abs (- (s 0 2) (* 2 (s 0 1)) (- (s 0 0)))))
             (dp3 (abs (- (s 3 -3) (* 2 (s 3 -2)) (- (s 3 -1)))))
             (dq3 (abs (- (s 3 2) (* 2 (s 3 1)) (- (s 3 0)))))
             (dpq0 (+ dp0 dq0)) (dpq3 (+ dp3 dq3))
             (d (+ dpq0 dpq3)))
        (declare (type fixnum dp0 dq0 dp3 dq3 dpq0 dpq3 d))
        (when (>= d beta) (return-from %filter-luma-edge nil))
        (flet ((strong-p (line dpq)
                 (declare (type fixnum line dpq))
                 (and (< (* 2 dpq) (ash beta -2))
                      (< (+ (abs (- (s line -4) (s line -1)))
                            (abs (- (s line 0) (s line 3))))
                         (ash beta -3))
                      (< (abs (- (s line -1) (s line 0)))
                         (ash (+ (* 5 tc) 1) -1)))))
          (let ((strong (and (strong-p 0 dpq0) (strong-p 3 dpq3)))
                ;; beta + beta/2, over eight.  Shifting beta the other way is a factor of four
                ;; on the threshold, which lets the weak filter touch p1 and q1 on edges it should
                ;; have left alone — a small error on a great many samples.
                (filter-p (< (+ dp0 dp3) (ash (+ beta (ash beta -1)) -3)))
                (filter-q (< (+ dq0 dq3) (ash (+ beta (ash beta -1)) -3))))
            (dotimes (line 4)
              (let ((p0 (s line -1)) (p1 (s line -2)) (p2 (s line -3)) (p3 (s line -4))
                    (q0 (s line 0)) (q1 (s line 1)) (q2 (s line 2)) (q3 (s line 3)))
                (declare (type fixnum p0 p1 p2 p3 q0 q1 q2 q3))
                (if strong
                    (let ((lo2 (* -2 tc)) (hi2 (* 2 tc)))
                      (when allow-p
                       (setf (s line -3) (%clip8 (+ p2 (%clip3 lo2 hi2
                                                               (- (ash (+ (* 2 p3) (* 3 p2) p1 p0
                                                                          q0 4) -3)
                                                                  p2))))))
                      (when allow-q
                       (setf (s line 2) (%clip8 (+ q2 (%clip3 lo2 hi2
                                                              (- (ash (+ p0 q0 q1 (* 3 q2)
                                                                         (* 2 q3) 4) -3)
                                                                 q2))))))
                      (when allow-p
                        (setf (s line -1) (%clip8 (+ p0 (%clip3 lo2 hi2
                                                                (- (ash (+ p2 (* 2 p1) (* 2 p0)
                                                                           (* 2 q0) q1 4) -3)
                                                                   p0))))
                              (s line -2) (%clip8 (+ p1 (%clip3 lo2 hi2
                                                                (- (ash (+ p2 p1 p0 q0 2) -2)
                                                                   p1))))))
                      (when allow-q
                        (setf (s line 0) (%clip8 (+ q0 (%clip3 lo2 hi2
                                                               (- (ash (+ p1 (* 2 p0) (* 2 q0)
                                                                          (* 2 q1) q2 4) -3)
                                                                  q0))))
                              (s line 1) (%clip8 (+ q1 (%clip3 lo2 hi2
                                                               (- (ash (+ p0 q0 q1 q2 2) -2)
                                                                  q1)))))))
                    ;; the weak filter moves p0 and q0 by one shared delta, and p1 and q1 only
                    ;; where that side was flat enough to be worth touching
                    (let ((delta (ash (+ (* 9 (- q0 p0)) (* -3 (- q1 p1)) 8) -4)))
                      (declare (type fixnum delta))
                      (when (< (abs delta) (* 10 tc))
                        (setf delta (%clip3 (- tc) tc delta))
                        (when allow-p (setf (s line -1) (%clip8 (+ p0 delta))))
                        (when allow-q (setf (s line 0) (%clip8 (- q0 delta))))
                        (when (and filter-p allow-p)
                          (setf (s line -2)
                                (%clip8 (+ p1 (%clip3 (- (ash tc -1)) (ash tc -1)
                                                      (ash (+ (- (ash (+ p2 p0 1) -1) p1) delta)
                                                           -1))))))
                        (when (and filter-q allow-q)
                          (setf (s line 1)
                                (%clip8 (+ q1 (%clip3 (- (ash tc -1)) (ash tc -1)
                                                      (ash (- (- (ash (+ q2 q0 1) -1) q1) delta)
                                                           -1)))))))))))))))))

(defun %filter-chroma-edge (plane base step line-step qp tc-off allow-p allow-q)
  "One two-line segment of a chroma edge.  Only strength 2 — an intra edge — is filtered at all:
   chroma has less detail to lose and more to gain from being left alone."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane)
           (type fixnum base step line-step qp tc-off)
           (optimize (speed 3) (safety 1)))
  (let ((tc (aref +tc-table+ (%clip3 0 53 (+ qp 2 tc-off)))))
    (declare (type fixnum tc))
    (when (zerop tc) (return-from %filter-chroma-edge nil))
    (macrolet ((s (line off) `(aref plane (+ base (* ,line line-step) (* ,off step)))))
      (dotimes (line 2)
        (let* ((p0 (s line -1)) (p1 (s line -2)) (q0 (s line 0)) (q1 (s line 1))
               (delta (%clip3 (- tc) tc (ash (+ (ash (- q0 p0) 2) p1 (- q1) 4) -3))))
          (declare (type fixnum p0 p1 q0 q1 delta))
          (when allow-p (setf (s line -1) (%clip8 (+ p0 delta))))
          (when allow-q (setf (s line 0) (%clip8 (- q0 delta)))))))))

(defun %boundary-strength (pic xp yp xq yq)
  "bS for the four samples either side of an edge (8.7.2.4).

   Three tiers, and only the first is about the pictures themselves.  An INTRA block on either side
   is 2 — the strongest filter — because intra content has no motion to compare and its block
   structure is the artefact the filter exists to remove.  Below that, 1 is EARNED: either a
   transform block edge where one side carried coefficients, or a genuine discontinuity in motion.
   Everything else is 0 and is left alone, because filtering two blocks that came from the same
   place with the same vector only blurs something that was never broken.

   The coefficient test applies at every edge the marking pass recorded, without asking what kind
   it is — because the marking pass only records transform block edges in the first place."
  (declare (type picture pic) (type fixnum xp yp xq yq))
  (let ((ip (pic-mv-index pic xp yp))
        (iq (pic-mv-index pic xq yq)))
    (cond
      ((or (plusp (aref (pic-intra pic) ip)) (plusp (aref (pic-intra pic) iq))) 2)
      ((or (plusp (aref (pic-cbf pic) ip)) (plusp (aref (pic-cbf pic) iq))) 1)
      (t
       ;; the motion test: same pictures, same count of them, and no component differing by a whole
       ;; sample.  A quarter-sample vector is four units, so the threshold of 4 IS one sample.
       (let ((np 0) (nq 0))
         (declare (type fixnum np nq))
         (dotimes (lx 2)
           (when (>= (aref (pic-ref-idx pic) (+ (* 2 ip) lx)) 0) (incf np))
           (when (>= (aref (pic-ref-idx pic) (+ (* 2 iq) lx)) 0) (incf nq)))
         (cond
           ((/= np nq) 1)
           ((zerop np) 0)
           ((= np 1)
            (let ((lp (if (>= (aref (pic-ref-idx pic) (* 2 ip)) 0) 0 1))
                  (lq (if (>= (aref (pic-ref-idx pic) (* 2 iq)) 0) 0 1)))
              (multiple-value-bind (px py pr pp) (pic-motion pic xp yp lp)
                (declare (ignore pr))
                (multiple-value-bind (qx qy qr qp) (pic-motion pic xq yq lq)
                  (declare (ignore qr))
                  (if (or (/= pp qp) (>= (abs (- px qx)) 4) (>= (abs (- py qy)) 4)) 1 0)))))
           (t
            ;; both bi-predicted: the two references must be the same PAIR, and then every way of
            ;; matching them up has to agree to within a sample for the edge to be left alone
            (multiple-value-bind (p0x p0y r0 p0p) (pic-motion pic xp yp 0)
              (declare (ignore r0))
              (multiple-value-bind (p1x p1y r1 p1p) (pic-motion pic xp yp 1)
                (declare (ignore r1))
                (multiple-value-bind (q0x q0y r2 q0p) (pic-motion pic xq yq 0)
                  (declare (ignore r2))
                  (multiple-value-bind (q1x q1y r3 q1p) (pic-motion pic xq yq 1)
                    (declare (ignore r3))
                    (flet ((near (ax ay bx by)
                             (and (< (abs (- ax bx)) 4) (< (abs (- ay by)) 4))))
                      (cond
                        ((and (= p0p q0p) (= p1p q1p) (/= p0p p1p))
                         (if (and (near p0x p0y q0x q0y) (near p1x p1y q1x q1y)) 0 1))
                        ((and (= p0p q1p) (= p1p q0p) (/= p0p p1p))
                         (if (and (near p0x p0y q1x q1y) (near p1x p1y q0x q0y)) 0 1))
                        ((and (= p0p p1p) (= p0p q0p) (= p0p q1p))
                         ;; the same picture twice: either pairing may justify leaving it alone
                         (if (or (and (near p0x p0y q0x q0y) (near p1x p1y q1x q1y))
                                 (and (near p0x p0y q1x q1y) (near p1x p1y q0x q0y)))
                             0 1))
                        (t 1)))))))))))))) 

(defun %ctb-of (pic x y)
  (+ (* (ash y (- (pic-ctb-log2 pic))) (pic-ctbs-wide pic)) (ash x (- (pic-ctb-log2 pic)))))

(defun %edge-filterable-p (pic xp yp xq yq)
  "(values filter-at-all allow-p allow-q) for an edge between these two samples.

   Two different questions.  Whether the edge is filtered AT ALL is about the slice: the Q side's
   header turns deblocking off, and decides whether the filter may cross into a different slice.
   Whether each SIDE is written is about that side's own block — a block sent exactly, by
   transquant bypass or by PCM with the filter disabled, must come out exactly, and that is no
   reason to leave the block opposite it unfiltered."
  (declare (type fixnum xp yp xq yq))
  (let* ((w (pic-blk-w pic))
         (ip (+ (* (ash yp -3) w) (ash xp -3)))
         (iq (+ (* (ash yq -3) w) (ash xq -3)))
         (cp (%ctb-of pic xp yp)) (cq (%ctb-of pic xq yq)))
    (values (and (zerop (logand (aref (pic-ctb-dbf pic) cq) 1))
                 (or (= (aref (pic-ctb-slice pic) cp) (aref (pic-ctb-slice pic) cq))
                     (plusp (aref (pic-ctb-across pic) cq))))
            (zerop (aref (pic-blk-nofilt pic) ip))
            (zerop (aref (pic-blk-nofilt pic) iq)))))

(defun deblock-picture (pic pps sh)
  "Every vertical edge in the picture, then every horizontal one (8.7.2).

   The order is normative and it is the whole reason this cannot be folded into the block loop: the
   horizontal pass reads samples the vertical pass has already moved."
  (declare (type picture pic) (ignore sh) (optimize (speed 3) (safety 1)))
  (let* ((w (pic-width pic)) (h (pic-height pic))
         (ys (pic-ystride pic)) (cs (pic-cstride pic))
         (bqw (pic-blk-w pic))
         (cb-off (pps-cb-qp-offset pps)) (cr-off (pps-cr-qp-offset pps)))
    (declare (type fixnum w h ys cs bqw))
    (flet ((qp-at (x y) (aref (pic-blk-qp pic) (+ (* (ash y -3) bqw) (ash x -3))))
           (offs (x y) (let ((v (aref (pic-ctb-dbf pic) (%ctb-of pic x y))))
                         (values (- (logand (ash v -8) 255) 32)
                                 (- (logand (ash v -16) 255) 32)))))
      ;; ---- vertical edges, left to right
      (loop for x of-type fixnum from 8 below w by 8 do
        (loop for y of-type fixnum from 0 below h by 4 do
          (let* ((kind (aref (pic-bs-v pic) (+ (* (ash y -2) (pic-bs-vw pic)) (ash x -3))))
                 (bs (if (plusp kind) (%boundary-strength pic (1- x) y x y) 0)))
            (multiple-value-bind (ok ap aq) (%edge-filterable-p pic (1- x) y x y)
             (when (and (plusp bs) ok)
              (multiple-value-bind (bo to) (offs x y)
                (%filter-luma-edge (pic-y pic) (+ (* y ys) x) 1 ys bs
                                   (ash (+ (qp-at (1- x) y) (qp-at x y) 1) -1) bo to ap aq)
                ;; Chroma edges sit on the eight-sample CHROMA grid, which is every sixteen luma
                ;; columns — but along the edge every four luma rows are two chroma rows, and all
                ;; of them are filtered.  Testing the row alignment as well leaves half the chroma
                ;; lines untouched.
                (when (and (= bs 2) (zerop (logand x 15)))
                  (let ((qp (ash (+ (qp-at (1- x) y) (qp-at x y) 1) -1))
                        (cx (ash x -1)) (cy (ash y -1)))
                    (%filter-chroma-edge (pic-u pic) (+ (* cy cs) cx) 1 cs
                                         (%chroma-qp qp cb-off) to ap aq)
                    (%filter-chroma-edge (pic-v pic) (+ (* cy cs) cx) 1 cs
                                         (%chroma-qp qp cr-off) to ap aq)))))))))
      ;; ---- horizontal edges, top to bottom
      (loop for y of-type fixnum from 8 below h by 8 do
        (loop for x of-type fixnum from 0 below w by 4 do
          (let* ((kind (aref (pic-bs-h pic) (+ (* (ash y -3) (pic-bs-hw pic)) (ash x -2))))
                 (bs (if (plusp kind) (%boundary-strength pic x (1- y) x y) 0)))
            (multiple-value-bind (ok ap aq) (%edge-filterable-p pic x (1- y) x y)
             (when (and (plusp bs) ok)
              (multiple-value-bind (bo to) (offs x y)
                (%filter-luma-edge (pic-y pic) (+ (* y ys) x) ys 1 bs
                                   (ash (+ (qp-at x (1- y)) (qp-at x y) 1) -1) bo to ap aq)
                (when (and (= bs 2) (zerop (logand y 15)))
                  (let ((qp (ash (+ (qp-at x (1- y)) (qp-at x y) 1) -1))
                        (cx (ash x -1)) (cy (ash y -1)))
                    (%filter-chroma-edge (pic-u pic) (+ (* cy cs) cx) cs 1
                                         (%chroma-qp qp cb-off) to ap aq)
                    (%filter-chroma-edge (pic-v pic) (+ (* cy cs) cx) cs 1
                                         (%chroma-qp qp cr-off) to ap aq))))))))))
    pic))

;;; ---- sample adaptive offset ---------------------------------------------------------------------

(defparameter +eo-dx+ (make-array 4 :element-type '(signed-byte 32)
                                    :initial-contents '(-1 0 -1 1))
  "The first neighbour of each edge class: horizontal, vertical, 135 degrees, 45 degrees.")
(defparameter +eo-dy+ (make-array 4 :element-type '(signed-byte 32)
                                    :initial-contents '(0 -1 -1 -1))
  "Its vertical component; the second neighbour is the reflection of the first.")
(declaim (type (simple-array (signed-byte 32) (4)) +eo-dx+ +eo-dy+))

(defun sao-picture (pic sh)
  "Apply the per-coding-tree-block offsets (8.7.3), reading a copy of the deblocked picture.

   The copy is not an optimisation to be removed.  Edge offset compares each sample against two
   neighbours, and if those neighbours have already had their own offsets applied then the
   comparison is against samples the encoder never saw."
  (declare (type picture pic) (optimize (speed 3) (safety 1)))
  (unless (or (sh-sao-luma sh) (sh-sao-chroma sh)) (return-from sao-picture pic))
  (let* ((log2 (pic-ctb-log2 pic))
         (wide (pic-ctbs-wide pic))
         (high (pic-ctbs-high pic))
         (src (vector (copy-seq (pic-y pic)) (copy-seq (pic-u pic)) (copy-seq (pic-v pic)))))
    (declare (type fixnum log2 wide high))
    (dotimes (ry high)
      (dotimes (rx wide)
        (let ((addr (+ (* ry wide) rx)))
          (dotimes (comp 3)
            (let ((type (aref (pic-sao-type pic) (+ (* 3 addr) comp))))
              (unless (zerop type)
                (let* ((sub (if (zerop comp) 0 1))
                       (plane (pic-plane pic comp))
                       (ref (aref src comp))
                       (stride (pic-stride pic comp))
                       (pw (ash (pic-width pic) (- sub)))
                       (ph (ash (pic-height pic) (- sub)))
                       (x0 (ash (ash rx log2) (- sub)))
                       (y0 (ash (ash ry log2) (- sub)))
                       (x1 (min pw (ash (ash (1+ rx) log2) (- sub))))
                       (y1 (min ph (ash (ash (1+ ry) log2) (- sub))))
                       (base (* 12 addr))
                       (param (aref (pic-sao-param pic) (+ (* 3 addr) comp))))
                  (declare (type fixnum sub stride pw ph x0 y0 x1 y1 base param))
                  (macrolet ((exempt (x y)
                               ;; A block sent exactly — transquant bypass, or PCM with the filter
                               ;; disabled — must come out exactly, and that binds SAO as much as
                               ;; deblocking (8.7.3.1).  The flag is recorded per eight-by-eight
                               ;; LUMA block, so a chroma coordinate has to be doubled to find it.
                               `(plusp (aref (pic-blk-nofilt pic)
                                             (+ (* (ash (ash ,y sub) -3) (pic-blk-w pic))
                                                (ash (ash ,x sub) -3))))))
                  (if (= type 1)
                      ;; BAND: four adjacent bands of the sample range, offset independently
                      (loop for y of-type fixnum from y0 below y1 do
                        (loop for x of-type fixnum from x0 below x1 do
                          (let* ((v (aref ref (+ (* y stride) x)))
                                 (band (- (ash v -3) param)))
                            (declare (type fixnum v band))
                            (when (and (<= 0 band 3) (not (exempt x y)))
                              (setf (aref plane (+ (* y stride) x))
                                    (%clip8 (+ v (aref (pic-sao-off pic)
                                                       (+ base (* 4 comp) band)))))))))
                      ;; EDGE: which of a valley, a step or a peak this sample sits on
                      (let ((dx (aref +eo-dx+ param)) (dy (aref +eo-dy+ param)))
                        (declare (type fixnum dx dy))
                        (loop for y of-type fixnum from y0 below y1 do
                          (loop for x of-type fixnum from x0 below x1 do
                            ;; a sample whose neighbours fall outside the picture is left alone
                            (let ((ax (+ x dx)) (ay (+ y dy))
                                  (bx (- x dx)) (by (- y dy)))
                              (declare (type fixnum ax ay bx by))
                              (when (and (<= 0 ax (1- pw)) (<= 0 ay (1- ph))
                                         (<= 0 bx (1- pw)) (<= 0 by (1- ph))
                                         (not (exempt x y)))
                                (let* ((v (aref ref (+ (* y stride) x)))
                                       (a (aref ref (+ (* ay stride) ax)))
                                       (b (aref ref (+ (* by stride) bx)))
                                       (cat (+ 2 (signum (- v a)) (signum (- v b)))))
                                  (declare (type fixnum v a b cat))
                                  ;; category 2 is "no edge here", and gets no offset
                                  (unless (= cat 2)
                                    (let ((k (if (< cat 2) cat (1- cat))))
                                      (setf (aref plane (+ (* y stride) x))
                                            (%clip8 (+ v (aref (pic-sao-off pic)
                                                               (+ base (* 4 comp) k))))))))))))))))))))))
    pic))

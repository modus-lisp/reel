;;;; h264/deblock.lisp — the in-loop deblocking filter (8.7).
;;;;
;;;; MANDATORY, NOT COSMETIC.  H.264's filter is *in loop*: the filtered picture is what later
;;;; pictures predict from, so a decoder that skips it does not merely look blockier — it diverges
;;;; from the encoder and keeps diverging. (This decoder is intra-only today, so nothing predicts
;;;; from the result yet; it is still exactly what a conforming decoder outputs, which is what the
;;;; tests compare against.)
;;;;
;;;; THE SHAPE.  Macroblocks in raster order; within each, every vertical edge left to right, then
;;;; every horizontal edge top to bottom.  Each edge is filtered with samples that earlier edges
;;;; may already have modified — the order is normative, not an implementation detail.
;;;;
;;;; HOW HARD IT FILTERS is decided twice over. BOUNDARY STRENGTH (bS) comes from what the blocks
;;;; are: for intra content, 4 on a macroblock edge and 3 inside one. Then alpha and beta, both
;;;; functions of the quantiser, decide whether a given step across the edge is a blocking artifact
;;;; worth smoothing or real detail worth keeping — at a coarse quantiser a big step is more likely
;;;; to be the block structure, so the filter is allowed to touch more.
;;;;
;;;; The whole thing is a no-op where the picture already has a genuine edge, which is the point:
;;;; it removes the discontinuities the block transform invented and leaves the ones that were
;;;; photographed.

(in-package #:reel.h264)

(defconstant +qp-table-offset+ 52
  "The alpha/beta/tc0 tables are stored with the specification's index biased by 52, so that a
negative slice offset still lands inside the array instead of before it.")

(declaim (inline clip3))
(defun clip3 (lo hi v)
  (declare (type fixnum lo hi v) (optimize (speed 3) (safety 0)))
  (max lo (min hi v)))

;;; ---- one edge ---------------------------------------------------------------------------------

(declaim (inline %filter-line))
(defun %filter-line (plane q0i step bs alpha beta tc0 chroma-p)
  "Filter one line of samples across an edge.  Q0I indexes q0; STEP is the distance from one
   sample to the next ACROSS the edge (1 for a vertical edge, the stride for a horizontal one)."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane)
           (type fixnum q0i step bs alpha beta tc0)
           (optimize (speed 3) (safety 0)))
  (let* ((p0 (aref plane (- q0i step))) (p1 (aref plane (- q0i (* 2 step))))
         (p2 (aref plane (- q0i (* 3 step)))) (p3 (aref plane (- q0i (* 4 step))))
         (q0 (aref plane q0i)) (q1 (aref plane (+ q0i step)))
         (q2 (aref plane (+ q0i (* 2 step)))) (q3 (aref plane (+ q0i (* 3 step)))))
    (declare (type fixnum p0 p1 p2 p3 q0 q1 q2 q3))
    ;; the gate: a step bigger than alpha across the edge, or beta within either side, is taken
    ;; to be real detail and left alone
    (unless (and (< (abs (- p0 q0)) alpha)
                 (< (abs (- p1 p0)) beta)
                 (< (abs (- q1 q0)) beta))
      (return-from %filter-line nil))
    (let ((ap (abs (- p2 p0))) (aq (abs (- q2 q0))))
      (declare (type fixnum ap aq))
      (cond
        ((= bs 4)
         (cond
           (chroma-p
            (setf (aref plane (- q0i step)) (ash (+ (* 2 p1) p0 q1 2) -2)
                  (aref plane q0i) (ash (+ (* 2 q1) q0 p1 2) -2)))
           (t
            (let ((strong (< (abs (- p0 q0)) (+ (ash alpha -2) 2))))
              (if (and (< ap beta) strong)
                  (setf (aref plane (- q0i step))
                        (ash (+ p2 (* 2 p1) (* 2 p0) (* 2 q0) q1 4) -3)
                        (aref plane (- q0i (* 2 step))) (ash (+ p2 p1 p0 q0 2) -2)
                        (aref plane (- q0i (* 3 step)))
                        (ash (+ (* 2 p3) (* 3 p2) p1 p0 q0 4) -3))
                  (setf (aref plane (- q0i step)) (ash (+ (* 2 p1) p0 q1 2) -2)))
              (if (and (< aq beta) strong)
                  (setf (aref plane q0i)
                        (ash (+ q2 (* 2 q1) (* 2 q0) (* 2 p0) p1 4) -3)
                        (aref plane (+ q0i step)) (ash (+ q2 q1 q0 p0 2) -2)
                        (aref plane (+ q0i (* 2 step)))
                        (ash (+ (* 2 q3) (* 3 q2) q1 q0 p0 4) -3))
                  (setf (aref plane q0i) (ash (+ (* 2 q1) q0 p1 2) -2)))))))
        (t
         (let* ((tc (if chroma-p
                        (1+ tc0)
                        (+ tc0 (if (< ap beta) 1 0) (if (< aq beta) 1 0))))
                (delta (clip3 (- tc) tc (ash (+ (ash (- q0 p0) 2) (- p1 q1) 4) -3))))
           (declare (type fixnum tc delta))
           (setf (aref plane (- q0i step)) (clamp255 (+ p0 delta))
                 (aref plane q0i) (clamp255 (- q0 delta)))
           ;; only luma moves the samples one further out from the edge
           (unless chroma-p
             (when (< ap beta)
               (setf (aref plane (- q0i (* 2 step)))
                     (+ p1 (clip3 (- tc0) tc0 (ash (- (+ p2 (ash (+ p0 q0 1) -1)) (* 2 p1)) -1)))))
             (when (< aq beta)
               (setf (aref plane (+ q0i step))
                     (+ q1 (clip3 (- tc0) tc0 (ash (- (+ q2 (ash (+ p0 q0 1) -1)) (* 2 q1)) -1))))))))))
    t))

(defun %filter-edge (plane q0i step line-step count bs qp alpha-off beta-off chroma-p)
  "Filter COUNT lines of one edge.  QP is the average of the two sides' quantisers."
  (declare (type fixnum q0i step line-step count bs qp alpha-off beta-off)
           (type (simple-array (unsigned-byte 8) (*)) plane)
           (optimize (speed 3) (safety 1)))
  (when (zerop bs) (return-from %filter-edge nil))
  (let* ((ia (+ +qp-table-offset+ (clip3 0 51 (+ qp alpha-off))))
         (ib (+ +qp-table-offset+ (clip3 0 51 (+ qp beta-off))))
         (alpha (aref +alpha-table+ ia))
         (beta (aref +beta-table+ ib))
         (tc0 (if (= bs 4) 0 (aref +tc0-table+ ia bs))))
    (declare (type fixnum ia ib alpha beta tc0))
    (when (or (zerop alpha) (zerop beta)) (return-from %filter-edge nil))
    ;; FOUR LOOPS, not one.  CHROMA-P and "is bS 4" are constant for a whole edge, but tested per
    ;; line inside the filter, and at 1080p this is 34% of decode: about 1.6 million filtered lines
    ;; a picture.  Expanding the filter once per combination lets the compiler fold both tests away
    ;; and specialise the body; the cost is four copies of it in the object file.
    (macrolet ((run (chroma bs4)
                 `(dotimes (i count)
                    (declare (type fixnum i))
                    (%filter-line plane (+ q0i (* i line-step)) step
                                  ,(if bs4 4 'bs) alpha beta tc0 ,chroma))))
      (if chroma-p
          (if (= bs 4) (run t t) (run t nil))
          (if (= bs 4) (run nil t) (run nil nil))))))

;;; ---- the picture ---------------------------------------------------------------------------------


;;; ---- how hard: the boundary strength (8.7.2.1) ----------------------------------------------------

(declaim (inline %far-apart))
(defun %far-apart (ax ay bx by)
  "Did these two vectors part company by a whole sample or more?  Quarter-pel units, so four."
  (declare (type fixnum ax ay bx by))
  (or (>= (abs (- ax bx)) 4) (>= (abs (- ay by)) 4)))

(defun %block-motion (pic bx by)
  "(values n pocA mvAx mvAy pocB mvBx mvBy) for one block: how many predictions it used, and each
   one's picture and vector.  A block predicting only from list 1 answers with that in the A slot,
   because what matters downstream is the SET of predictions, not which list held them."
  (let ((n 0) (pa +no-ref-poc+) (ax 0) (ay 0) (pb +no-ref-poc+) (bx* 0) (by* 0))
    (dotimes (lx 2)
      (let ((poc (blk-ref-poc pic bx by lx)))
        (unless (= poc +no-ref-poc+)
          (multiple-value-bind (mx my) (blk-mv pic bx by lx)
            (if (zerop n)
                (setf pa poc ax mx ay my)
                (setf pb poc bx* mx by* my))
            (incf n)))))
    (values n pa ax ay pb bx* by*)))

(defun %boundary-strength (pic pbx pby qbx qby mb-edge-p)
  "bS for the pair of 4x4 blocks either side of an edge, P before Q in decoding order.

   The intra cases are constants — 4 across a macroblock edge, 3 inside one — because intra content
   has no motion to compare and its block structure is the thing the filter exists to remove.  For
   inter content the strength is EARNED: 2 where either side carries residual coefficients, 1 where
   the two sides genuinely came from different places, and 0 where they came from the same place
   with the same motion, in which case there is no seam to soften and filtering would only blur.

   `The same place' means the same PICTURES, compared as a set and without regard to which list
   held them or at what index (8.7.2.1).  A bi-predicted block that used one picture TWICE is the
   awkward case: the two vectors can be matched to the other block's two either way round, and the
   edge is only left alone if some pairing works."
  (declare (optimize (speed 3) (safety 1)))
  (let ((p-intra (mb-intra-p pic (floor pbx 4) (floor pby 4)))
        (q-intra (mb-intra-p pic (floor qbx 4) (floor qby 4))))
    (cond
      ((or p-intra q-intra) (if mb-edge-p 4 3))
      (t
       (let ((gw (* 4 (pic-mb-width pic))))
         (if (or (plusp (aref (pic-nz-y pic) (+ (* pby gw) pbx)))
                 (plusp (aref (pic-nz-y pic) (+ (* qby gw) qbx))))
             2
             (multiple-value-bind (pn pa pax pay pb pbx* pby*) (%block-motion pic pbx pby)
               (multiple-value-bind (qn qa qax qay qb qbx* qby*) (%block-motion pic qbx qby)
                 (cond
                   ((/= pn qn) 1)
                   ((zerop pn) 0)
                   ((= pn 1) (if (or (/= pa qa) (%far-apart pax pay qax qay)) 1 0))
                   ;; two predictions each: the sets of pictures must match first
                   ((not (or (and (= pa qa) (= pb qb)) (and (= pa qb) (= pb qa)))) 1)
                   ((= pa pb)
                    ;; the same picture twice, so either pairing is legitimate and the edge is
                    ;; only spared when one of them holds
                    (if (and (or (%far-apart pax pay qax qay) (%far-apart pbx* pby* qbx* qby*))
                             (or (%far-apart pax pay qbx* qby*) (%far-apart pbx* pby* qax qay)))
                        1 0))
                   (t
                    ;; two different pictures: each vector is compared with the one for its own
                    (if (= pa qa)
                        (if (or (%far-apart pax pay qax qay) (%far-apart pbx* pby* qbx* qby*)) 1 0)
                        (if (or (%far-apart pax pay qbx* qby*) (%far-apart pbx* pby* qax qay))
                            1 0))))))))))))

(defvar *skip-loop-filter* nil
  "Bind true to reconstruct without filtering, matching `ffmpeg -skip_loop_filter all\'.

   A DIAGNOSTIC, not an option: the filter is in-loop, so skipping it does not merely look
   blockier, it diverges from the encoder and keeps diverging.  Its use is telling in one run
   whether a mismatch is in the filter or underneath it, and that is worth a variable because the
   alternative is guessing.")

(defun deblock-picture (pic sh)
  "Filter every macroblock of PIC, in the order 8.7 requires.

   Each edge is filtered in FOUR GROUPS with their own boundary strength, not as one edge with one
   strength: bS is a property of the pair of 4x4 blocks either side, and in inter content it varies
   along a single macroblock edge.  A chroma group is two lines where a luma group is four, because
   4:2:0 chroma is half resolution and takes its strength from the luma edge it lies on."
  (declare (optimize (speed 3) (safety 1)))
  (when *skip-loop-filter* (return-from deblock-picture pic))
  (let* ((idc (sh-disable-deblocking sh)))
    (when (= idc 1) (return-from deblock-picture pic))
    (let* ((pps (sh-pps sh))
           (alpha-off (sh-alpha-offset sh))
           (beta-off (sh-beta-offset sh))
           (mbw (pic-mb-width pic)) (mbh (pic-mb-height pic))
           (ys (pic-ystride pic)) (cs (pic-cstride pic)))
      (dotimes (mby mbh)
        (dotimes (mbx mbw)
          (let* ((mbi (+ (* mby mbw) mbx))
                 (qp (aref (pic-mb-qps pic) mbi))
                 (ybase (pic-y-base pic mbx mby))
                 (cbase (pic-c-base pic mbx mby))
                 (bx0 (* 4 mbx)) (by0 (* 4 mby))
                 (left-p (plusp mbx))
                 (up-p (plusp mby))
                 (qp-left (and left-p (aref (pic-mb-qps pic) (1- mbi))))
                 (qp-up (and up-p (aref (pic-mb-qps pic) (- mbi mbw)))))
            (flet ((cqp (a b plane)
                     ;; chroma filters at the CHROMA quantiser, derived per side then averaged
                     (ash (+ (chroma-qp a (pps-chroma-qp-offset-for pps plane))
                             (chroma-qp b (pps-chroma-qp-offset-for pps plane)) 1) -1))
                   (bs-v (dx k)
                     ;; vertical edge DX samples in, group K: the block pair is side by side
                     (%boundary-strength pic (+ bx0 (ash dx -2) -1) (+ by0 k)
                                         (+ bx0 (ash dx -2)) (+ by0 k) (zerop dx)))
                   (bs-h (dy k)
                     (%boundary-strength pic (+ bx0 k) (+ by0 (ash dy -2) -1)
                                         (+ bx0 k) (+ by0 (ash dy -2)) (zerop dy))))
              (macrolet ((each-group ((kvar) &body body) `(dotimes (,kvar 4) ,@body)))
                ;; ---- vertical edges, left to right
                (when left-p
                  (each-group (k)
                    (let ((bs (bs-v 0 k)))
                      (%filter-edge (pic-y pic) (+ ybase (* 4 k ys)) 1 ys 4 bs
                                    (ash (+ qp qp-left 1) -1) alpha-off beta-off nil)
                      (dotimes (plane 2)
                        (%filter-edge (if (zerop plane) (pic-u pic) (pic-v pic))
                                      (+ cbase (* 2 k cs)) 1 cs 2 bs
                                      (cqp qp qp-left plane) alpha-off beta-off t)))))
                (loop for dx in '(4 8 12)
                      do (each-group (k)
                           (let ((bs (bs-v dx k)))
                             (%filter-edge (pic-y pic) (+ ybase dx (* 4 k ys)) 1 ys 4 bs
                                           qp alpha-off beta-off nil))))
                ;; chroma has one internal vertical edge, at the luma edge 8 samples in
                (each-group (k)
                  (let ((bs (bs-v 8 k)))
                    (dotimes (plane 2)
                      (%filter-edge (if (zerop plane) (pic-u pic) (pic-v pic))
                                    (+ cbase 4 (* 2 k cs)) 1 cs 2 bs
                                    (cqp qp qp plane) alpha-off beta-off t))))
                ;; ---- horizontal edges, top to bottom
                (when up-p
                  (each-group (k)
                    (let ((bs (bs-h 0 k)))
                      (%filter-edge (pic-y pic) (+ ybase (* 4 k)) ys 1 4 bs
                                    (ash (+ qp qp-up 1) -1) alpha-off beta-off nil)
                      (dotimes (plane 2)
                        (%filter-edge (if (zerop plane) (pic-u pic) (pic-v pic))
                                      (+ cbase (* 2 k)) cs 1 2 bs
                                      (cqp qp qp-up plane) alpha-off beta-off t)))))
                (loop for dy in '(4 8 12)
                      do (each-group (k)
                           (let ((bs (bs-h dy k)))
                             (%filter-edge (pic-y pic) (+ ybase (* dy ys) (* 4 k)) ys 1 4 bs
                                           qp alpha-off beta-off nil))))
                (each-group (k)
                  (let ((bs (bs-h 8 k)))
                    (dotimes (plane 2)
                      (%filter-edge (if (zerop plane) (pic-u pic) (pic-v pic))
                                    (+ cbase (* 4 cs) (* 2 k)) cs 1 2 bs
                                    (cqp qp qp plane) alpha-off beta-off t)))))))))
      pic)))

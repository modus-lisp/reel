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
(defun clip3 (lo hi v) (declare (type fixnum lo hi v)) (max lo (min hi v)))

;;; ---- one edge ---------------------------------------------------------------------------------

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
  (declare (type fixnum q0i step line-step count bs qp alpha-off beta-off))
  (when (zerop bs) (return-from %filter-edge nil))
  (let* ((ia (+ +qp-table-offset+ (clip3 0 51 (+ qp alpha-off))))
         (ib (+ +qp-table-offset+ (clip3 0 51 (+ qp beta-off))))
         (alpha (aref +alpha-table+ ia))
         (beta (aref +beta-table+ ib))
         (tc0 (if (= bs 4) 0 (aref +tc0-table+ ia bs))))
    (declare (type fixnum ia ib alpha beta tc0))
    (when (or (zerop alpha) (zerop beta)) (return-from %filter-edge nil))
    (dotimes (i count)
      (%filter-line plane (+ q0i (* i line-step)) step bs alpha beta tc0 chroma-p))))

;;; ---- the picture ---------------------------------------------------------------------------------

(defun deblock-picture (pic sh)
  "Filter every macroblock of PIC, in the order 8.7 requires."
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
                 (left-p (plusp mbx))
                 (up-p (plusp mby))
                 (qp-left (and left-p (aref (pic-mb-qps pic) (1- mbi))))
                 (qp-up (and up-p (aref (pic-mb-qps pic) (- mbi mbw)))))
            (flet ((cqp (a b plane)
                     ;; chroma filters at the CHROMA quantiser, derived per side then averaged
                     (ash (+ (chroma-qp a (pps-chroma-qp-offset-for pps plane))
                             (chroma-qp b (pps-chroma-qp-offset-for pps plane)) 1) -1)))
              ;; ---- vertical edges, left to right
              (when left-p
                (%filter-edge (pic-y pic) ybase 1 ys 16 4
                              (ash (+ qp qp-left 1) -1) alpha-off beta-off nil)
                (dotimes (plane 2)
                  (%filter-edge (if (zerop plane) (pic-u pic) (pic-v pic))
                                cbase 1 cs 8 4 (cqp qp qp-left plane) alpha-off beta-off t)))
              (loop for dx in '(4 8 12)
                    do (%filter-edge (pic-y pic) (+ ybase dx) 1 ys 16 3 qp alpha-off beta-off nil))
              (dotimes (plane 2)
                (%filter-edge (if (zerop plane) (pic-u pic) (pic-v pic))
                              (+ cbase 4) 1 cs 8 3 (cqp qp qp plane) alpha-off beta-off t))
              ;; ---- horizontal edges, top to bottom
              (when up-p
                (%filter-edge (pic-y pic) ybase ys 1 16 4
                              (ash (+ qp qp-up 1) -1) alpha-off beta-off nil)
                (dotimes (plane 2)
                  (%filter-edge (if (zerop plane) (pic-u pic) (pic-v pic))
                                cbase cs 1 8 4 (cqp qp qp-up plane) alpha-off beta-off t)))
              (loop for dy in '(4 8 12)
                    do (%filter-edge (pic-y pic) (+ ybase (* dy ys)) ys 1 16 3
                                     qp alpha-off beta-off nil))
              (dotimes (plane 2)
                (%filter-edge (if (zerop plane) (pic-u pic) (pic-v pic))
                              (+ cbase (* 4 cs)) cs 1 8 3 (cqp qp qp plane)
                              alpha-off beta-off t))))))
      pic)))

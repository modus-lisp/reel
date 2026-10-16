;;;; hevc/inter.lisp — prediction units: reading the motion, and using it.
;;;;
;;;; A coding unit is cut into one, two or four PREDICTION UNITS, each of which either merges — an
;;;; index into the candidate list, and nothing else — or codes a reference index, a vector
;;;; difference and which of the two predictors to add it to.  Both paths end in the same place:
;;;; a vector and a reference for each of up to two lists, written into the picture's motion field
;;;; so that later blocks and later pictures can predict from it.
;;;;
;;;; THE MOTION IS STORED BEFORE THE SAMPLES ARE.  A later prediction unit in the same coding unit
;;;; derives its candidates from this one, so the field has to be written as each unit is decoded
;;;; rather than at the end.

(in-package #:reel.hevc)

(defparameter +part-geometry+
  ;; For each part_mode: the number of prediction units, then for each one its (x y w h) as
  ;; SIXTEENTHS of the coding unit, so the same table serves every size.
  (let ((v (make-array 8)))
    (setf (aref v 0) '((0 0 16 16))                                  ; 2Nx2N
          (aref v 1) '((0 0 16 8) (0 8 16 8))                        ; 2NxN
          (aref v 2) '((0 0 8 16) (8 0 8 16))                        ; Nx2N
          (aref v 3) '((0 0 8 8) (8 0 8 8) (0 8 8 8) (8 8 8 8))      ; NxN
          (aref v 4) '((0 0 16 4) (0 4 16 12))                       ; 2NxnU
          (aref v 5) '((0 0 16 12) (0 12 16 4))                      ; 2NxnD
          (aref v 6) '((0 0 4 16) (4 0 12 16))                       ; nLx2N
          (aref v 7) '((0 0 12 16) (12 0 4 16)))                     ; nRx2N
    v)
  "Where each prediction unit sits inside its coding unit, in sixteenths.")

(declaim (inline %mv-wrap))
(defun %mv-wrap (v)
  "A motion vector component after adding its difference (8.5.3.2.6).

   THE SUM WRAPS, IT DOES NOT SATURATE.  The standard spells it out — add, take the result modulo
   2^16, and read it back as a signed sixteen-bit number — and ffmpeg gets it for free by storing
   the vector in an int16_t.  Clipping to the same range instead is right for every vector an
   encoder would ever choose and wrong for the ones a conformance stream chooses on purpose."
  (declare (type fixnum v))
  (- (mod (+ v 32768) 65536) 32768))

(defun %inter-pred-idc (c w h)
  "inter_pred_idc: 0 list 0 only, 1 list 1 only, 2 both.

   An 8x4 or 4x8 prediction unit — the only ones whose width and height sum to twelve — may not be
   bi-predicted at all.  Two motion compensations of a block that small move more memory per sample
   than the coding gain is worth, so the standard forbids it rather than leaving it to the encoder."
  (if (= 12 (+ w h))
      (%bin c (+ +ctx-inter-pred-idc+ 4))
      (if (= 1 (%bin c (+ +ctx-inter-pred-idc+ (cx-cu-depth c))))
          2
          (%bin c (+ +ctx-inter-pred-idc+ 4)))))

(defun %store-motion (c x0 y0 w h mv)
  "Write one prediction unit's motion over its 4x4 blocks."
  (declare (type ctx c) (type fixnum x0 y0 w h))
  (let ((pic (cx-pic c)))
    (loop for y of-type fixnum from y0 below (+ y0 h) by 4 do
      (loop for x of-type fixnum from x0 below (+ x0 w) by 4 do
        (setf (aref (pic-intra pic) (pic-mv-index pic x y)) 0)
        (dotimes (lx 2)
          (let* ((idx (aref mv (+ (* 4 lx) 2)))
                 (list (if (zerop lx) (cx-list0 c) (cx-list1 c)))
                 ;; whether the reference was LONG-TERM travels with the motion, because a later
                 ;; picture reading this field has no access to the list that gave the index meaning
                 (lt (and (>= idx 0) (< idx (length list))
                          (pic-long-term (aref list idx)))))
            (set-pic-motion pic x y lx
                            (aref mv (* 4 lx)) (aref mv (+ (* 4 lx) 1))
                            idx (aref mv (+ (* 4 lx) 3)) lt)))))))

(defun %weight (c lx idx comp)
  "(values weight offset denom) for list LX's reference IDX and component COMP, NIL when the slice
   sent no weight table.  The weights are per reference INDEX, not per picture: an encoder wanting
   two weights for one picture puts it in the list twice, which is why a list may name the same
   picture more than once."
  (declare (type ctx c) (type fixnum lx idx comp))
  (let* ((sh (cx-sh c))
         (tbl (if (zerop lx) (sh-wp-l0 sh) (sh-wp-l1 sh))))
    (when (and tbl (>= idx 0) (< idx (length tbl)))
      (let ((e (aref tbl idx)))
        (case comp
          (0 (values (first e) (second e) (sh-wp-luma-denom sh)))
          (1 (values (third e) (fourth e) (sh-wp-chroma-denom sh)))
          (t (values (fifth e) (sixth e) (sh-wp-chroma-denom sh))))))))

(defun %motion-compensate (c x0 y0 w h mv)
  "Predict one prediction unit's samples from one or two reference pictures."
  (declare (type ctx c) (type fixnum x0 y0 w h))
  (let* ((pic (cx-pic c))
         (r0 (aref mv 2)) (r1 (aref mv 6))
         (bi (and (>= r0 0) (>= r1 0))))
    (declare (type fixnum r0 r1))
    (flet ((one (lx buf)
             (let* ((list (if (zerop lx) (cx-list0 c) (cx-list1 c)))
                    (idx (aref mv (+ (* 4 lx) 2))))
               (when (and (>= idx 0) (< idx (length list)))
                 (let ((ref (aref list idx))
                       (mvx (aref mv (* 4 lx))) (mvy (aref mv (+ (* 4 lx) 1))))
                   (%mc-luma buf w h (pic-y ref) (pic-ystride ref)
                             (pic-width ref) (pic-height ref) x0 y0 mvx mvy
                             (pic-bd-luma pic))
                   ref)))))
      (cond
        (bi
         (let ((a (cx-p0 c)) (b (cx-p1 c)))
           (one 0 a) (one 1 b)
           (multiple-value-bind (w0 o0 dn) (%weight c 0 r0 0)
             (multiple-value-bind (w1 o1) (%weight c 1 r1 0)
               (%write-bi (pic-y pic) (pic-ystride pic) x0 y0 w h a b
                          (pic-bd-luma pic) w0 o0 w1 o1 dn)))
           (%mc-chroma-pair c x0 y0 w h mv t)))
        (t
         (let ((lx (if (>= r0 0) 0 1)) (a (cx-p0 c)))
           (when (one lx a)
             (multiple-value-bind (wt off dn) (%weight c lx (aref mv (+ (* 4 lx) 2)) 0)
               (%write-uni (pic-y pic) (pic-ystride pic) x0 y0 w h a
                           (pic-bd-luma pic) wt off dn))
             (%mc-chroma-pair c x0 y0 w h mv nil))))))))

(defun %mc-chroma-pair (c x0 y0 w h mv bi)
  "The two chroma planes of one prediction unit.  Everything halves: the position, the size, and
   the vector — which is why a quarter-sample luma vector needs eighth-sample chroma filters."
  (declare (type ctx c) (type fixnum x0 y0 w h))
  (let* ((pic (cx-pic c))
         (cx0 (ash x0 -1)) (cy0 (ash y0 -1))
         (cw (ash w -1)) (ch (ash h -1)))
    (declare (type fixnum cx0 cy0 cw ch))
    (when (or (zerop cw) (zerop ch)) (return-from %mc-chroma-pair nil))
    (dotimes (plane 2)
      (let ((dst (if (zerop plane) (pic-u pic) (pic-v pic))))
        (flet ((one (lx buf)
                 (let* ((list (if (zerop lx) (cx-list0 c) (cx-list1 c)))
                        (idx (aref mv (+ (* 4 lx) 2))))
                   (when (and (>= idx 0) (< idx (length list)))
                     (let* ((ref (aref list idx))
                            (rp (if (zerop plane) (pic-u ref) (pic-v ref))))
                       (%mc-chroma buf cw ch rp (pic-cstride ref)
                                   (ash (pic-width ref) -1) (ash (pic-height ref) -1)
                                   cx0 cy0
                                   (aref mv (* 4 lx)) (aref mv (+ (* 4 lx) 1))
                                   (pic-bd-chroma pic))
                       t)))))
          (if bi
              (let ((a (cx-p0 c)) (b (cx-p1 c)))
                (one 0 a) (one 1 b)
                (multiple-value-bind (w0 o0 dn) (%weight c 0 (aref mv 2) (1+ plane))
                  (multiple-value-bind (w1 o1) (%weight c 1 (aref mv 6) (1+ plane))
                    (%write-bi dst (pic-cstride pic) cx0 cy0 cw ch a b
                               (pic-bd-chroma pic) w0 o0 w1 o1 dn))))
              (let ((lx (if (>= (aref mv 2) 0) 0 1)) (a (cx-p0 c)))
                (when (one lx a)
                  (multiple-value-bind (wt off dn)
                      (%weight c lx (aref mv (+ (* 4 lx) 2)) (1+ plane))
                    (%write-uni dst (pic-cstride pic) cx0 cy0 cw ch a
                                (pic-bd-chroma pic) wt off dn))))))))))

(defun %prediction-unit (c x0 y0 w h part-idx skip)
  "One prediction unit (7.3.8.6): read its motion, store it, and predict from it."
  (declare (type ctx c) (type fixnum x0 y0 w h part-idx))
  (let ((mv (make-array 8 :element-type 'fixnum :initial-element 0))
        (merge nil))
    (setf (aref mv 2) -1 (aref mv 6) -1)
    (if (or skip (setf merge (= 1 (%bin c +ctx-merge-flag+))))
        ;; ---- merged: an index into the candidate list, and nothing else
        (let* ((maxn (sh-max-merge-cand (cx-sh c)))
               (idx (if (> maxn 1)
                        (let ((i (%bin c +ctx-merge-idx+)))
                          (when (plusp i)
                            (setf i 1)
                            (loop while (and (< i (1- maxn)) (= 1 (%bypass c))) do (incf i)))
                          i)
                        0))
               (cands (make-array (* +cand-size+ 8) :element-type 'fixnum :initial-element 0))
               ;; A SINGLE MERGE CANDIDATE LIST for a whole 8x8 coding unit: above the default
               ;; parallel merge level every prediction unit of an 8x8 unit uses the list derived
               ;; for the unit as a whole, so none of them has to wait for its siblings.  The
               ;; substitution is of the POSITION and SIZE the list is derived at, not of the unit
               ;; being predicted — the vectors still land where this unit is.
               (single (and (> (pps-log2-parallel-merge-level (cx-pps c)) 2)
                            (= 8 (ash 1 (cx-cu-log2 c)))))
               (mx0 (if single (cx-cu-x c) x0))
               (my0 (if single (cx-cu-y c) y0))
               (mw (if single 8 w)) (mh (if single 8 h))
               (mpi (if single 0 part-idx)))
          (setf (cx-merge-2nx2n c) (and (= (cx-part-mode c) 0) t))
          (%merge-candidates c mx0 my0 mw mh mpi (cx-part-mode c) cands single)
          (dotimes (lx 2)
            (setf (aref mv (* 4 lx)) (cand-mvx cands idx lx)
                  (aref mv (+ (* 4 lx) 1)) (cand-mvy cands idx lx)
                  (aref mv (+ (* 4 lx) 2)) (cand-ref cands idx lx)
                  (aref mv (+ (* 4 lx) 3)) (cand-poc cands idx lx)))
          ;; An 8x4 or 4x8 unit may not be bi-predicted, and a merged one enforces that by DROPPING
          ;; LIST 1 rather than by refusing the candidate.  The list is built as if bi-prediction
          ;; were allowed and then cut down, so the candidate at a given index is the same one
          ;; either way — and the motion stored for later blocks to predict from is the cut-down
          ;; one, which is how a single small unit changes the vectors of everything after it.
          (when (and (= 12 (+ w h)) (>= (aref mv 2) 0) (>= (aref mv 6) 0))
            (setf (aref mv 6) -1)))
        ;; ---- coded: a reference, a difference, and which predictor to add it to
        (let* ((b (sh-b-slice-p (cx-sh c)))
               (idc (if b (%inter-pred-idc c w h) 0)))
          (declare (type fixnum idc))
          (setf (cx-merge-2nx2n c) nil)
          (dotimes (lx 2)
            (when (or (= idc 2) (= idc lx))
              (let* ((list (if (zerop lx) (cx-list0 c) (cx-list1 c)))
                     (n (length list))
                     (ref (if (> n 1) (%ref-idx c n) 0)))
                (multiple-value-bind (dx dy)
                    (if (and (= lx 1) (sh-mvd-l1-zero (cx-sh c)) (= idc 2))
                        (values 0 0)
                        (%mvd c))
                  (let ((which (%bin c +ctx-mvp-lx-flag+)))
                    (multiple-value-bind (ax ay bx by n-cand) (%amvp c x0 y0 w h lx ref)
                      (declare (ignore n-cand))
                      (let ((px (if (zerop which) ax bx))
                            (py (if (zerop which) ay by)))
                        (setf (aref mv (* 4 lx)) (%mv-wrap (+ px dx))
                              (aref mv (+ (* 4 lx) 1)) (%mv-wrap (+ py dy))
                              (aref mv (+ (* 4 lx) 2)) ref
                              (aref mv (+ (* 4 lx) 3))
                              (if (< ref n) (pic-poc (aref list ref)) 0)))))))))))
    (%store-motion c x0 y0 w h mv)
    (%mark-edges c x0 y0 w h 1)
    (%motion-compensate c x0 y0 w h mv)))

(defun %prediction-units (c x0 y0 size part-mode skip)
  "Every prediction unit of one inter coding unit, in order."
  (declare (type ctx c) (type fixnum x0 y0 size part-mode))
  (let ((geo (aref +part-geometry+ part-mode))
        (i 0))
    (dolist (g geo)
      (destructuring-bind (gx gy gw gh) g
        (%prediction-unit c (+ x0 (ash (* size gx) -4)) (+ y0 (ash (* size gy) -4))
                          (ash (* size gw) -4) (ash (* size gh) -4)
                          i skip))
      (incf i))))

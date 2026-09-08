;;;; h264/slice.lisp — the macroblock layer and the slice decoding loop (7.3.5, 8.3, 8.5).
;;;;
;;;; This is where the pieces meet: a macroblock's type says how it is predicted, its coded block
;;;; pattern says which of its blocks carry residual, CAVLC or CABAC produces the coefficients, the
;;;; transforms turn them into a residual, and the prediction plus that residual is the picture.
;;;;
;;;; THE PART THAT IS NOT OBVIOUS FROM THE PIECES is bookkeeping, and it is most of the file:
;;;;
;;;;   * PREDICTED PREDICTION MODES.  An Intra4x4 block does not code its mode outright; it codes
;;;;     whether the mode equals the smaller of its two neighbours\' modes, and if not, which of
;;;;     the remaining eight it is.  So every block\'s mode has to be remembered for the blocks
;;;;     below and right of it.
;;;;   * nC.  Every residual block\'s VLC table is chosen by how many coefficients its LEFT and
;;;;     ABOVE neighbours had — across macroblock boundaries — so a per-picture grid of
;;;;     coefficient counts has to be kept, at 4x4 granularity, for luma and both chroma planes.
;;;;   * AVAILABILITY.  A neighbour exists if it is inside the picture and earlier in raster
;;;;     order.  With one slice per picture that is the whole rule; a decoder that grows multiple
;;;;     slices per picture must also check they are the same slice, which is why the check is a
;;;;     function rather than an inline comparison.
;;;;
;;;; The picture and slice-state structures themselves are in picture.lisp, ahead of every file that
;;;; reads them in a loop.  See the note there; it is a performance decision, not a tidiness one.

(in-package #:reel.h264)

;;; ---- nC: the coefficient-count context ------------------------------------------------------------

(defun %nz-at (grid gw bx by)
  
  (declare (optimize (speed 3) (safety 1)))(if (or (minusp bx) (minusp by)) nil (aref grid (+ (* by gw) bx))))

(defun luma-nc (ss blk)
  "nC for luma 4x4 block BLK of the current macroblock (9.2.1)."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss)) (gw (* 4 (pic-mb-width pic)))
         (bx (+ (* 4 (ss-mbx ss)) (aref +blk-x+ blk)))
         (by (+ (* 4 (ss-mby ss)) (aref +blk-y+ blk)))
         (left (when (or (plusp (aref +blk-x+ blk)) (mb-available-p ss -1 0))
                 (%nz-at (pic-nz-y pic) gw (1- bx) by)))
         (up (when (or (plusp (aref +blk-y+ blk)) (mb-available-p ss 0 -1))
               (%nz-at (pic-nz-y pic) gw bx (1- by)))))
    (cond ((and left up) (ash (+ left up 1) -1))
          (left left) (up up) (t 0))))

(defun chroma-nc (ss plane blk)
  "nC for a chroma 4x4 block (0..3) of PLANE (0 = Cb, 1 = Cr)."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss)) (gw (* 2 (pic-mb-width pic)))
         (grid (if (zerop plane) (pic-nz-u pic) (pic-nz-v pic)))
         (lx (logand blk 1)) (ly (ash blk -1))
         (bx (+ (* 2 (ss-mbx ss)) lx))
         (by (+ (* 2 (ss-mby ss)) ly))
         (left (when (or (plusp lx) (mb-available-p ss -1 0)) (%nz-at grid gw (1- bx) by)))
         (up (when (or (plusp ly) (mb-available-p ss 0 -1)) (%nz-at grid gw bx (1- by)))))
    (cond ((and left up) (ash (+ left up 1) -1))
          (left left) (up up) (t 0))))

(defun set-luma-nz (ss blk n)
  
  (declare (optimize (speed 3) (safety 1)))(let* ((pic (ss-pic ss)) (gw (* 4 (pic-mb-width pic))))
    (setf (aref (pic-nz-y pic)
                (+ (* (+ (* 4 (ss-mby ss)) (aref +blk-y+ blk)) gw)
                   (+ (* 4 (ss-mbx ss)) (aref +blk-x+ blk))))
          n)))

(defun set-chroma-nz (ss plane blk n)
  
  (declare (optimize (speed 3) (safety 1)))(let* ((pic (ss-pic ss)) (gw (* 2 (pic-mb-width pic)))
         (grid (if (zerop plane) (pic-nz-u pic) (pic-nz-v pic))))
    (setf (aref grid (+ (* (+ (* 2 (ss-mby ss)) (ash blk -1)) gw)
                        (+ (* 2 (ss-mbx ss)) (logand blk 1))))
          n)))

;;; ---- Intra4x4 prediction modes (8.3.1.1) ------------------------------------------------------------

(defun intra-mb-available-p (ss dx dy)
  "MB-AVAILABLE-P, narrowed by constrained_intra_pred_flag.

   When that flag is set, an INTER-coded neighbour is unavailable to intra prediction even though
   it is decoded and in this slice.  The point is error resilience: an intra macroblock that never
   reads a predicted sample is a clean restart, so a lost reference picture cannot leak through the
   intra macroblocks that were supposed to repair it.

   This is deliberately NOT the predicate the coefficient counts use.  nC only ignores inter
   neighbours when the stream is slice-data-partitioned (9.2.1), which is a coding tool this
   decoder does not accept, so applying the flag there would desynchronise streams that are
   otherwise fine.  Prediction and entropy context ask different questions of the same neighbour."
  (declare (optimize (speed 3) (safety 1)))
  (and (mb-available-p ss dx dy)
       (or (not (ss-constrained-intra ss))
           ;; MB-TYPES stores intra types as non-negative and inter ones as negative
           (let ((pic (ss-pic ss)))
             (>= (the fixnum
                      (aref (pic-mb-types pic)
                            (+ (* (+ (ss-mby ss) dy) (pic-mb-width pic)) (ss-mbx ss) dx)))
                 0)))))

(defun neighbour-mb-available-p (ss blk dx dy)
  "Is the MACROBLOCK containing the neighbour DX,DY away from BLK available?"
  (declare (optimize (speed 3) (safety 1)))
  (let* ((lx (+ (aref +blk-x+ blk) dx)) (ly (+ (aref +blk-y+ blk) dy))
         (mbdx (cond ((minusp lx) -1) ((> lx 3) 1) (t 0)))
         (mbdy (cond ((minusp ly) -1) ((> ly 3) 1) (t 0))))
    (or (and (zerop mbdx) (zerop mbdy))          ; inside this macroblock
        (intra-mb-available-p ss mbdx mbdy))))

(defun neighbour-mode (ss blk dx dy)
  "The Intra4x4 mode of the block DX,DY away from BLK, or 2 (DC) when the macroblock it lives in
   was not Intra4x4 coded."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss)) (gw (* 4 (pic-mb-width pic)))
         (lx (+ (aref +blk-x+ blk) dx)) (ly (+ (aref +blk-y+ blk) dy))
         (mbdx (cond ((minusp lx) -1) ((> lx 3) 1) (t 0)))
         (mbdy (cond ((minusp ly) -1) ((> ly 3) 1) (t 0))))
    (if (and (zerop mbdx) (zerop mbdy))
        ;; inside this macroblock: it is Intra4x4 by construction, and already decoded
        (aref (pic-modes pic)
              (+ (* (+ (* 4 (ss-mby ss)) ly) gw) (+ (* 4 (ss-mbx ss)) lx)))
        (let* ((nmb (+ (* (+ (ss-mby ss) mbdy) (pic-mb-width pic)) (+ (ss-mbx ss) mbdx)))
               (ntype (aref (pic-mb-types pic) nmb)))
          ;; only an Intra4x4 neighbour has per-block modes to predict from
          (if (zerop ntype)
              (aref (pic-modes pic)
                    (+ (* (+ (* 4 (+ (ss-mby ss) mbdy)) (mod ly 4)) gw)
                       (+ (* 4 (+ (ss-mbx ss) mbdx)) (mod lx 4))))
              2)))))

(defun predicted-mode (ss blk)
  "predIntra4x4PredMode for BLK (8.3.1.1).

   THE SUBTLETY THAT COSTS A DAY: dcPredModePredictedFlag is set when EITHER neighbouring
   macroblock is unavailable, and when it is set it forces BOTH intraMxMPredModeA and
   intraMxMPredModeB to DC — not just the missing one.  So a block on the top row of a picture
   predicts DC even though its LEFT neighbour is present and has a perfectly good mode.

   Applying unavailability per-neighbour instead — taking min(realLeftMode, 2) — is the obvious
   reading and it is wrong.  It desynchronises nothing, because the flag and remainder are the
   same number of bits either way; it silently decodes the wrong prediction mode, and the picture
   comes out structured but wrong in a way that looks like a transform bug."
  (declare (optimize (speed 3) (safety 1)))
  (let ((a-ok (neighbour-mb-available-p ss blk -1 0))
        (b-ok (neighbour-mb-available-p ss blk 0 -1)))
    (if (not (and a-ok b-ok))
        2
        (min (neighbour-mode ss blk -1 0) (neighbour-mode ss blk 0 -1)))))

(defun set-mode (ss blk mode)
  
  (declare (optimize (speed 3) (safety 1)))(let* ((pic (ss-pic ss)) (gw (* 4 (pic-mb-width pic))))
    (setf (aref (pic-modes pic)
                (+ (* (+ (* 4 (ss-mby ss)) (aref +blk-y+ blk)) gw)
                   (+ (* 4 (ss-mbx ss)) (aref +blk-x+ blk))))
          mode)))


(declaim (inline %scale-list))
(defun %scale-list (ss idx)
  "The weight matrix for one of the six block kinds: intra Y, Cb, Cr then inter Y, Cb, Cr.
   Flat when the stream sent no scaling list, which is the overwhelming majority of streams."
  (let ((m (sh-scale-4x4 (ss-sh ss))))
    (if m (aref m idx) +flat-scale-4x4+)))

(declaim (inline %scale-list-8))
(defun %scale-list-8 (ss intra-p)
  "The 8x8 weight matrix, intra luma or inter luma.  Chroma never asks: there is no 8x8 chroma
   transform in 4:2:0."
  (let ((m (sh-scale-4x4 (ss-sh ss))))
    (if m (aref m (if intra-p 6 7)) +flat-scale-8x8+)))

;;; ---- the two entropy coders, behind one interface ------------------------------------------------
;;;
;;; CAVLC and CABAC disagree about how every syntax element is spelled, and agree completely about
;;; what to do with the values once read.  Keeping the dispatch to these few functions is what lets
;;; one reconstruction path serve both: the prediction, the transforms and the loop filter never
;;; learn which entropy coder the slice used.

(declaim (ftype function cabac-mb-type-i cabac-intra-chroma-mode cabac-intra4x4-mode
                cabac-cbp cabac-mb-qp-delta cabac-residual-block init-cabac decode-terminate
                cb-last-qp-delta))

(defun %read-intra4x4-mode (ss blk)
  (if (ss-cabac ss)
      (cabac-intra4x4-mode ss blk)
      (let* ((br (ss-br ss)) (pred (predicted-mode ss blk)) (flag (u1 br)))
        (if (= flag 1)
            pred
            (let ((rem (ub br 3))) (if (< rem pred) rem (1+ rem)))))))

(defun %read-chroma-mode (ss)
  (if (ss-cabac ss) (cabac-intra-chroma-mode ss) (%chroma-pred-mode (ss-br ss))))

(defun %read-cbp (ss intra-p)
  (if (ss-cabac ss)
      (cabac-cbp ss)
      (let ((code (ue (ss-br ss))))
        (if (< code 48)
            (aref (if intra-p +intra4x4-cbp+ +inter-cbp+) code)
            (%err "coded_block_pattern code ~d" code)))))

(defun %read-qp-delta (ss)
  (if (ss-cabac ss) (cabac-mb-qp-delta ss) (se (ss-br ss))))

(defun %no-qp-delta (ss)
  "A macroblock that codes no mb_qp_delta still counts as `the previous one had none\', which is
   what the first bin\'s context asks about."
  (when (ss-cabac ss) (setf (cb-last-qp-delta (ss-cabac ss)) 0)))

(defun %residual (ss coeffs &key cat nc max-coeff (start 0) (bx 0) (by 0) (plane 0))
  "One residual block through whichever entropy coder this slice uses.  Both return
   (values count highest-scan-position)."
  (if (ss-cabac ss)
      (cabac-residual-block ss cat coeffs bx by plane :start start)
      (residual-block (ss-br ss) coeffs nc max-coeff :start start)))

(declaim (inline %luma-blk-xy %chroma-blk-xy))
(defun %luma-blk-xy (ss blk)
  (values (+ (* 4 (ss-mbx ss)) (aref +blk-x+ blk)) (+ (* 4 (ss-mby ss)) (aref +blk-y+ blk))))
(defun %chroma-blk-xy (ss blk)
  (values (+ (* 2 (ss-mbx ss)) (mod blk 2)) (+ (* 2 (ss-mby ss)) (floor blk 2))))

;;; ---- one macroblock ------------------------------------------------------------------------------

(defun up-right-available-p (ss blk)
  "Are the four samples above and to the right of luma block BLK available?

   This is a question about DECODING ORDER, not geometry, and it was worth deriving rather than
   tabulating: a 4x4 block's above-right neighbour may be in the macroblock above, in the
   macroblock above-right, in this macroblock but not yet decoded, or in the macroblock to the
   right and therefore never yet decoded.  Only the first two and the already-decoded case count.

   When it is unavailable the mode is not forbidden — p[3,-1] is replicated into p[4..7,-1]
   (8.3.1.2) — so getting this wrong does not crash, it quietly predicts two of the nine modes
   from the wrong samples."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((bx (aref +blk-x+ blk)) (by (aref +blk-y+ blk))
         (nx (1+ bx)) (ny (1- by)))
    (cond
      ;; the row above this macroblock
      ((minusp ny) (if (> nx 3) (intra-mb-available-p ss 1 -1) (intra-mb-available-p ss 0 -1)))
      ;; the macroblock to the right, which is always later in raster order
      ((> nx 3) nil)
      ;; inside this macroblock: only if that block comes earlier in decoding order
      (t (< (aref +blk-index+ ny nx) blk)))))

(defvar *debug-blk* nil "When (mb . blk), print that block's decode in detail.")
(defvar *debug-mb* nil "When (mbx . mby), print that macroblock's inter decisions.")

(defmacro when-debug-blk ((mb blk) &body body)
  "BODY runs only when *DEBUG-BLK* names this block.

   The NULL test comes first and the pair is built only once it passes.  The obvious spelling —
   (equal *debug-blk* (cons mb blk)) — conses a fresh pair for every block just to compare it,
   which is some twenty thousand conses per picture with debugging switched off.  That is not
   free: SBCL stops every thread to collect, so on a desktop that is also mixing audio it is
   heard, not merely measured."
  `(when (and *debug-blk* (equal *debug-blk* (cons ,mb ,blk)))
     ,@body))

(declaim (inline %mb-index))
(defun %mb-index (ss) 
  (declare (optimize (speed 3) (safety 1)))(+ (* (ss-mby ss) (pic-mb-width (ss-pic ss))) (ss-mbx ss)))

(defun decode-intra-macroblock (ss mb-type)
  "Reconstruct one INTRA macroblock whose mb_type has already been read.

   Split out from DECODE-I-MACROBLOCK because a P slice reaches it too: an mb_type of 5 or more
   there is an intra macroblock with 5 subtracted, and it must be decoded exactly as it would be
   in an I slice — including clearing its motion, so that a neighbour predicting a vector from it
   sees `intra\' rather than whatever the array happened to hold."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss))
         (mbi (+ (* (ss-mby ss) (pic-mb-width pic)) (ss-mbx ss))))
    (when (= mb-type 25) (%err "I_PCM macroblocks are not supported"))
    (when (> mb-type 25) (%err "mb_type ~d in an I slice" mb-type))
    (setf (aref (pic-mb-types pic) mbi) mb-type)
    (let ((bx (* 4 (ss-mbx ss))) (by (* 4 (ss-mby ss))))
      (dotimes (j 4) (dotimes (i 4) (clear-blk-motion pic (+ bx i) (+ by j)))))
    ;; I_NxN is two macroblock kinds wearing one mb_type.  Which one it is arrives here, between
    ;; the type and the prediction modes, and only when the picture parameter set allowed it.
    (setf (ss-tf8 ss) (and (zerop mb-type)
                           (pps-transform-8x8 (sh-pps (ss-sh ss)))
                           (%read-transform-size-8x8 ss)))
    (setf (aref (pic-mb-tf8 pic) mbi) (if (ss-tf8 ss) 1 0))
    (cond ((not (zerop mb-type)) (decode-i16x16-macroblock ss (1- mb-type)))
          ((ss-tf8 ss) (decode-i8x8-macroblock ss))
          (t (decode-i4x4-macroblock ss)))
    (setf (aref (pic-mb-qps pic) mbi) (ss-qp ss))
    mb-type))

(defun decode-i-macroblock (ss)
  "Parse and reconstruct one I-slice macroblock."
  (declare (optimize (speed 3) (safety 1)))
  (decode-intra-macroblock ss (ue (ss-br ss))))

(defun %chroma-pred-mode (br) 
  (declare (optimize (speed 3) (safety 1)))(ue br))

(declaim (ftype function cabac-transform-size-8x8))
(defun %read-transform-size-8x8 (ss)
  (= 1 (if (ss-cabac ss) (cabac-transform-size-8x8 ss) (u1 (ss-br ss)))))

(defun %read-intra8x8-mode (ss i8)
  "prev_intra8x8_pred_mode_flag and its remainder, spelled exactly as the 4x4 pair.

   The prediction is the 4x4 prediction asked about block 4*I8, which is the top-left 4x4 of this
   8x8.  That works because SET-MODE-8X8 writes one mode into all four of an 8x8's 4x4 slots, so
   the neighbour to the left of block 4*I8 already holds its own 8x8's mode."
  (let ((blk (* 4 i8)))
    (if (ss-cabac ss)
        (cabac-intra4x4-mode ss blk)
        (let* ((br (ss-br ss)) (pred (predicted-mode ss blk)) (flag (u1 br)))
          (if (= flag 1)
              pred
              (let ((rem (ub br 3))) (if (< rem pred) rem (1+ rem))))))))

(defun set-mode-8x8 (ss i8 mode)
  "Record an 8x8 block's mode in all four of its 4x4 slots, so that everything asking a
   neighbour for a mode — including an Intra_4x4 macroblock next door — gets an answer without
   learning that 8x8 blocks exist."
  (declare (optimize (speed 3) (safety 1)))
  (dotimes (i 4) (set-mode ss (+ (* 4 i8) i) mode)))

(defun %i8x8-neighbours (ss i8)
  "Availability of the four reference directions for 8x8 block I8, in decoding order 0..3."
  (let ((bx (logand i8 1)) (by (ash i8 -1)))
    (values (or (plusp by) (intra-mb-available-p ss 0 -1))                 ; above
            (or (plusp bx) (intra-mb-available-p ss -1 0))                 ; left
            ;; above-right: outside for 0 and 1, block 1 itself for 2, and never for 3, whose
            ;; above-right lies in the macroblock to the right and so is always still undecoded
            (case i8
              (0 (intra-mb-available-p ss 0 -1))
              (1 (intra-mb-available-p ss 1 -1))
              (2 t)
              (t nil))
            (cond ((and (plusp bx) (plusp by)) t)                    ; above-left
                  ((plusp bx) (intra-mb-available-p ss 0 -1))
                  ((plusp by) (intra-mb-available-p ss -1 0))
                  (t (intra-mb-available-p ss -1 -1))))))

(defun %luma-residual-8x8 (ss base i8 intra-p)
  "Read, dequantise, transform and add one 8x8 luma residual.

   In CAVLC an 8x8 block is not coded as an 8x8 block at all: it arrives as four ordinary 4x4
   blocks, each with its own coeff_token and its own nC from its own neighbours, and the four are
   INTERLEAVED into the 8x8 scan afterwards — the k'th coefficient of the i'th sub-block lands at
   scan position 4k+i.  So the whole of the 4x4 residual reader is reused unchanged, and only what
   happens to the numbers after it differs."
  (declare (optimize (speed 3) (safety 1)))
  (let ((pic (ss-pic ss)) (c8 (ss-coeffs8 ss)) (end -1) (any nil))
    (declare (type fixnum end))
    (fill c8 0)
    (if (ss-cabac ss)
        (multiple-value-bind (n hi) (%residual ss c8 :cat +cat-luma-8x8+ :max-coeff 64
                                                     :bx (logand i8 1) :by (ash i8 -1))
          (declare (type fixnum n hi))
          ;; CABAC codes the 8x8 block whole, and marks all four of its 4x4 slots non-zero so that
          ;; the loop filter and the neighbouring contexts see coefficients where there are some
          (dotimes (i 4) (set-luma-nz ss (+ (* 4 i8) i) (if (plusp n) 16 0)))
          (when (plusp n) (setf any t end hi)))
        (dotimes (i 4)
          (let ((blk (+ (* 4 i8) i)))
            (multiple-value-bind (n hi)
                (multiple-value-bind (bx by) (%luma-blk-xy ss blk)
                  (%residual ss (ss-coeffs ss) :cat +cat-luma+ :nc (luma-nc ss blk)
                                               :max-coeff 16 :bx bx :by by))
              (declare (type fixnum n hi))
              (set-luma-nz ss blk n)
              (when (plusp n)
                (setf any t)
                (setf end (max end (+ (* 4 hi) i)))
                (dotimes (k 16)
                  (setf (aref c8 (+ (* 4 k) i)) (aref (ss-coeffs ss) k))))))))
    (when any
      (dequant-8x8 c8 (ss-block8 ss) (ss-qp ss) :end end :weights (%scale-list-8 ss intra-p))
      (idct-8x8 (ss-block8 ss))
      (add-residual-8x8 (pic-y pic) (pic-ystride pic) base (ss-block8 ss)))))

(defun decode-i8x8-macroblock (ss)
  "Reconstruct one I_NxN macroblock that chose the 8x8 transform.

   Structurally the Intra_4x4 path with four blocks instead of sixteen, but nothing inside it is
   shared: different prediction, different neighbours, a filtered reference row, and a different
   transform.  Only the chroma is the same, because chroma has no 8x8 transform in 4:2:0."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss))
         (modes (make-array 4 :element-type '(signed-byte 32))))
    (declare (dynamic-extent modes))
    (dotimes (i8 4)
      (let ((mode (%read-intra8x8-mode ss i8)))
        (setf (aref modes i8) mode)
        (set-mode-8x8 ss i8 mode)))
    (let* ((mbi (%mb-index ss))
           (chroma-mode (%read-chroma-mode ss))
           (cbp (%read-cbp ss t)))
      (setf (aref (pic-mb-chroma-mode pic) mbi) chroma-mode
            (aref (pic-mb-cbp pic) mbi) cbp)
      (if (plusp cbp)
          (progn (incf (ss-qp ss) (%read-qp-delta ss))
                 (setf (ss-qp ss) (mod (+ (ss-qp ss) 52) 52)))
          (%no-qp-delta ss))
      (dotimes (i8 4)
        (let ((base (+ (pic-y-base pic (ss-mbx ss) (ss-mby ss))
                       (* (ash i8 -1) 8 (pic-ystride pic))
                       (* (logand i8 1) 8))))
          (multiple-value-bind (up-p left-p up-right-p up-left-p) (%i8x8-neighbours ss i8)
            (gather-8x8-neighbours (pic-y pic) (pic-ystride pic) base
                                   (ss-pt8 ss) (ss-pl8 ss) (ss-ptl8 ss)
                                   up-p left-p up-right-p up-left-p)
            (intra8x8-predict (ss-pred8 ss) (aref modes i8) (ss-pt8 ss) (ss-pl8 ss)
                              (aref (ss-ptl8 ss) 0) up-p left-p))
          (dotimes (i 8)
            (dotimes (j 8)
              (setf (aref (pic-y pic) (+ base (* i (pic-ystride pic)) j))
                    (aref (ss-pred8 ss) (+ (* i 8) j)))))
          (if (logbitp i8 cbp)
              (%luma-residual-8x8 ss base i8 t)
              (dotimes (i 4) (set-luma-nz ss (+ (* 4 i8) i) 0)))))
      (decode-chroma ss chroma-mode cbp)
      cbp)))

(defun decode-i4x4-macroblock (ss)
  
  (declare (optimize (speed 3) (safety 1)))(let* ((br (ss-br ss)) (pic (ss-pic ss))
         (modes (make-array 16 :element-type '(signed-byte 32))))
    (declare (dynamic-extent modes))
    ;; the sixteen prediction modes, each coded against its neighbours' minimum
    (dotimes (blk 16)
      (let ((mode (%read-intra4x4-mode ss blk)))
        (when-debug-blk ((%mb-index ss) :modes)
          (format t "~&  blk ~2d: mode ~d~%" blk mode))
        (setf (aref modes blk) mode)
        (set-mode ss blk mode)))
    (let* ((mbi (%mb-index ss))
           (chroma-mode (%read-chroma-mode ss))
           (cbp (%read-cbp ss t)))
      (setf (aref (pic-mb-chroma-mode pic) mbi) chroma-mode
            (aref (pic-mb-cbp pic) mbi) cbp)
      (if (plusp cbp)
          (progn (incf (ss-qp ss) (%read-qp-delta ss))
                 (setf (ss-qp ss) (mod (+ (ss-qp ss) 52) 52)))
          (%no-qp-delta ss))
      ;; luma: predict and reconstruct each 4x4 in turn, because each one predicts from the
      ;; reconstruction of the ones before it
      (dotimes (blk 16)
        (let* ((base (+ (pic-y-base pic (ss-mbx ss) (ss-mby ss))
                        (* (aref +blk-y+ blk) 4 (pic-ystride pic))
                        (* (aref +blk-x+ blk) 4)))
               (bx (aref +blk-x+ blk)) (by (aref +blk-y+ blk))
               (up-p (or (plusp by) (intra-mb-available-p ss 0 -1)))
               (left-p (or (plusp bx) (intra-mb-available-p ss -1 0)))
               (up-left-p (cond ((and (plusp bx) (plusp by)) t)
                                ((plusp bx) (intra-mb-available-p ss 0 -1))
                                ((plusp by) (intra-mb-available-p ss -1 0))
                                (t (intra-mb-available-p ss -1 -1))))
               (up-right-p (up-right-available-p ss blk)))
          (gather-4x4-neighbours (pic-y pic) (pic-ystride pic) base (ss-pt ss) (ss-pl ss)
                                 up-p left-p up-right-p up-left-p)
          (intra4x4-predict (ss-pred ss) (aref modes blk) (ss-pt ss) (ss-pl ss)
                            up-p left-p up-left-p)
          (when-debug-blk ((%mb-index ss) blk)
            (format t "~&  blk ~d mode=~d up=~a left=~a upleft=~a upright=~a~%   pt=~a~%   pl=~a~%   pred=~a~%"
                    blk (aref modes blk) up-p left-p up-left-p up-right-p
                    (coerce (ss-pt ss) 'list) (coerce (ss-pl ss) 'list)
                    (coerce (ss-pred ss) 'list)))
          ;; write the prediction into the plane, then add the residual on top of it
          (dotimes (i 4)
            (dotimes (j 4)
              (setf (aref (pic-y pic) (+ base (* i (pic-ystride pic)) j))
                    (aref (ss-pred ss) (+ (* i 4) j)))))
          (if (logbitp (ash blk -2) cbp)
              (multiple-value-bind (n hi)
                  (multiple-value-bind (bx by) (%luma-blk-xy ss blk)
                    (%residual ss (ss-coeffs ss) :cat +cat-luma+ :nc (luma-nc ss blk)
                                                 :max-coeff 16 :bx bx :by by))
                (declare (type fixnum n hi))
                (when-debug-blk ((%mb-index ss) blk)
                  (format t "   nc=~d n=~d coeffs(scan)=~a~%" (luma-nc ss blk) n
                          (coerce (ss-coeffs ss) 'list)))
                (set-luma-nz ss blk n)
                (when (plusp n)
                  (dequant-4x4 (ss-coeffs ss) (ss-block ss) (ss-qp ss) :end hi
                                                    :weights (%scale-list ss 0))
                  (if (zerop hi)
                      (idct-4x4-dc (ss-block ss) (aref (ss-block ss) 0))
                      (idct-4x4 (ss-block ss)))
                  (add-residual-4x4 (pic-y pic) (pic-ystride pic) base (ss-block ss))))
              (set-luma-nz ss blk 0))))
      (decode-chroma ss chroma-mode cbp)
      cbp)))

(defun decode-i16x16-macroblock (ss code)
  "An Intra16x16 macroblock.  CODE is mb_type - 1, which packs three things: the prediction mode,
   whether chroma has DC only or DC and AC, and whether luma has AC at all."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (pic (ss-pic ss))
         (pred-mode (mod code 4))
         (cbp-chroma (mod (floor code 4) 3))
         (cbp-luma (if (>= code 12) 15 0))
         (base (pic-y-base pic (ss-mbx ss) (ss-mby ss))))
    (let* ((mbi (%mb-index ss))
           (chroma-mode (%read-chroma-mode ss))
           (cbp (logior cbp-luma (ash cbp-chroma 4))))
      (setf (aref (pic-mb-chroma-mode pic) mbi) chroma-mode
            (aref (pic-mb-cbp pic) mbi) cbp)
      ;; an Intra16x16 macroblock always carries a QP delta, because it always has a DC block
      (incf (ss-qp ss) (%read-qp-delta ss))
      (setf (ss-qp ss) (mod (+ (ss-qp ss) 52) 52))
      (intra16x16-predict (pic-y pic) (pic-ystride pic) base pred-mode
                          (intra-mb-available-p ss 0 -1) (intra-mb-available-p ss -1 0))
      ;; the DC block: sixteen coefficients, one per 4x4, transformed again
      (let ((dc (ss-luma-dc ss)))
        (let ((n (%residual ss (ss-coeffs ss) :cat +cat-luma-dc+ :nc (luma-nc ss 0) :max-coeff 16)))
          ;; CABAC asks its neighbours whether THEY had a luma DC block, so record that we did
          (when (plusp n)
            (setf (aref (pic-mb-dc-cbf pic) mbi) (logior (aref (pic-mb-dc-cbf pic) mbi) 1)))
          ;; the DC block's own coefficients are in scan order; un-scan them into raster
          (fill dc 0)
          (dotimes (i 16) (setf (aref dc (aref +zigzag-4x4+ i)) (aref (ss-coeffs ss) i))))
        (luma-dc-transform dc (ss-qp ss) (aref (%scale-list ss 0) 0))
        ;; then every 4x4's AC, with that block's DC substituted in
        (dotimes (blk 16)
          (let ((bbase (+ base (* (aref +blk-y+ blk) 4 (pic-ystride pic))
                          (* (aref +blk-x+ blk) 4)))
                (n 0))
            (let ((hi -1))
              (declare (type fixnum hi))
              (if (logbitp (ash blk -2) cbp-luma)
                  (progn (multiple-value-bind (bx by) (%luma-blk-xy ss blk)
                           (multiple-value-setq (n hi)
                             (%residual ss (ss-coeffs ss) :cat +cat-luma-ac+ :nc (luma-nc ss blk)
                                                          :max-coeff 15 :start 1 :bx bx :by by)))
                         (dequant-4x4 (ss-coeffs ss) (ss-block ss) (ss-qp ss) :start 1 :end hi
                                                          :weights (%scale-list ss 0)))
                  (fill (ss-block ss) 0))
              (set-luma-nz ss blk n)
              ;; the DC comes from the second-order transform, not from this block's own scan
              (setf (aref (ss-block ss) 0)
                    (aref dc (+ (* (aref +blk-y+ blk) 4) (aref +blk-x+ blk))))
              ;; no AC coefficients means the block is now DC-only, whatever its DC came from
              (if (minusp hi)
                  (idct-4x4-dc (ss-block ss) (aref (ss-block ss) 0))
                  (idct-4x4 (ss-block ss))))
            (add-residual-4x4 (pic-y pic) (pic-ystride pic) bbase (ss-block ss)))))
      (decode-chroma ss chroma-mode cbp)
      cbp)))

(defun decode-chroma (ss mode cbp)
  "Both chroma planes: predict, then the DC blocks, then the AC blocks.

   THE ORDER IS BOTH DCs FIRST, then both planes' AC (7.3.5.3), and it is not the order the
   planes are otherwise processed in.  Doing Cb entirely and then Cr — which is what writing the
   loop the obvious way gives you — reads the Cr DC block out of the middle of Cb's AC data and
   desynchronises the whole slice from that macroblock on."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (sh (ss-sh ss)) (pps (sh-pps sh)) (pic (ss-pic ss))
         (cbp-chroma (ash cbp -4))
         (base (pic-c-base pic (ss-mbx ss) (ss-mby ss)))
         (up-p (intra-mb-available-p ss 0 -1))
         (left-p (intra-mb-available-p ss -1 0))
         (planes (vector (pic-u pic) (pic-v pic)))
         (qps (vector (chroma-qp (ss-qp ss) (pps-chroma-qp-offset-for pps 0))
                      (chroma-qp (ss-qp ss) (pps-chroma-qp-offset-for pps 1))))
         ;; scratch off the slice state rather than fresh per macroblock: see the note in
         ;; RESIDUAL-BLOCK about what per-macroblock garbage does to the audio on the same desktop
         (dcs (vector (ss-chroma-dc ss) (ss-chroma-dc-v ss))))
    (declare (dynamic-extent planes qps dcs))
    (fill (ss-chroma-dc ss) 0)
    (fill (ss-chroma-dc-v ss) 0)
    ;; prediction first, for both planes — but only for an INTRA macroblock.  An inter one was
    ;; already predicted from its reference picture, and predicting over that would erase it.
    (when mode
      (dotimes (plane 2)
        (chroma-predict (aref planes plane) (pic-cstride pic) base mode up-p left-p)))
    ;; then both DC blocks, in plane order
    (when (plusp cbp-chroma)
      (dotimes (plane 2)
        (let ((dc (aref dcs plane)))
          (let ((n (%residual ss (ss-coeffs ss) :cat +cat-chroma-dc+ :nc -1 :max-coeff 4
                                                :plane plane)))
            (when (plusp n)
              (setf (aref (pic-mb-dc-cbf pic) (%mb-index ss))
                    (logior (aref (pic-mb-dc-cbf pic) (%mb-index ss)) (ash 1 (1+ plane))))))
          (dotimes (i 4) (setf (aref dc i) (aref (ss-coeffs ss) i)))
          (chroma-dc-transform dc (aref qps plane)
                               (aref (%scale-list ss (+ (if mode 1 4) plane)) 0)))))
    ;; then the AC blocks, all of Cb's before any of Cr's
    (dotimes (plane 2)
      (let ((data (aref planes plane)) (qpc (aref qps plane)) (dc (aref dcs plane)))
        (dotimes (blk 4)
          (let ((bbase (+ base (* (ash blk -1) 4 (pic-cstride pic)) (* (logand blk 1) 4)))
                (n 0))
            (let ((hi -1))
              (declare (type fixnum hi))
              (if (= cbp-chroma 2)
                  (progn (multiple-value-bind (bx by) (%chroma-blk-xy ss blk)
                           (multiple-value-setq (n hi)
                             (%residual ss (ss-coeffs ss) :cat +cat-chroma-ac+
                                                          :nc (chroma-nc ss plane blk)
                                                          :max-coeff 15 :start 1
                                                          :bx bx :by by :plane plane)))
                         (dequant-4x4 (ss-coeffs ss) (ss-block ss) qpc :start 1 :end hi
                                                     :weights (%scale-list ss (+ (if mode 1 4) plane))))
                  (fill (ss-block ss) 0))
              (set-chroma-nz ss plane blk n)
              (setf (aref (ss-block ss) 0) (aref dc blk))
              (if (minusp hi)
                  (idct-4x4-dc (ss-block ss) (aref (ss-block ss) 0))
                  (idct-4x4 (ss-block ss))))
            (add-residual-4x4 data (pic-cstride pic) bbase (ss-block ss))))))))

;;; ---- the slice ------------------------------------------------------------------------------------


;;; ---- inter macroblocks ---------------------------------------------------------------------------

(defun %te (br n)
  "te(v): truncated Exp-Golomb.  With a range of exactly two values it degenerates to one inverted
   bit, which is the only place in the syntax where a `v\' code is not Exp-Golomb at all."
  (declare (optimize (speed 3) (safety 1)))
  (if (= n 2) (- 1 (u1 br)) (ue br)))

(defun %set-partition-mvd (ss bx by wb hb dx dy &optional (lx 0))
  (let ((pic (ss-pic ss)))
    (dotimes (j hb) (dotimes (i wb) (set-blk-mvd pic (+ bx i) (+ by j) dx dy lx)))))

(defun %ref-picture-poc (ss ref-idx lx)
  (let ((v (if (zerop lx) (ss-reflist ss) (or (ss-reflist1 ss) #()))))
    (if (and (>= ref-idx 0) (< ref-idx (length v))) (pic-poc (aref v ref-idx)) +no-ref-poc+)))

(defun %ref-picture-id (ss ref &optional (lx 0))
  "The identity of the picture at index REF of list LX, as its PICTURE ORDER COUNT.

   It must be the order count and not frame_num, for two reasons.  Frame_num does not identify a
   picture — every non-reference picture between two references shares one — and, more sharply,
   temporal direct prediction READS THIS BACK from a previously decoded picture and looks it up in
   the current list 0.  Storing one kind of number here and comparing it against another produces a
   lookup that silently misses and falls back to index 0.  That is invisible with a single
   reference picture, because index 0 is the only answer there is, and wrong with several."
  (%ref-picture-poc ss ref lx))

(defun %set-partition-motion (ss bx by wb hb mvx mvy ref)
  "Record one partition's vector on every 4x4 block it covers.

   Per block rather than per partition because that is the granularity everything downstream reads
   at: the next partition's vector prediction, the next picture's, and the loop filter's boundary
   strength all ask about 4x4 blocks and do not care how they were grouped."
  (declare (optimize (speed 3) (safety 1)))
  (let ((pic (ss-pic ss))
        (rid (%ref-picture-id ss ref))
        (bx0 (* 4 (ss-mbx ss))) (by0 (* 4 (ss-mby ss))))
    (dotimes (j hb)
      (dotimes (i wb)
        (setf (aref (pic-blk-direct pic) (%mv-index pic (+ bx i) (+ by j))) 0)
        (set-blk-mv pic (+ bx i) (+ by j) mvx mvy ref 0 rid)
        (set-blk-mv pic (+ bx i) (+ by j) 0 0 -1 1 +no-ref-poc+)
        (let ((lx (- (+ bx i) bx0)) (ly (- (+ by j) by0)))
          (when (and (<= 0 lx 3) (<= 0 ly 3))
            (setf (ss-mb-done ss) (logior (ss-mb-done ss) (ash 1 (+ (* 4 ly) lx))))))))))

(defun %ref-picture (ss ref-idx &optional (lx 0))
  "Entry REF-IDX of list LX.  Compensating from the wrong reference is invisible to the parser — it
   corrupts the picture and keeps decoding — so this refuses an index it cannot honour."
  (let ((v (if (zerop lx) (ss-reflist ss) (or (ss-reflist1 ss) #()))))
    (if (and (>= ref-idx 0) (< ref-idx (length v)))
        (aref v ref-idx)
        (%err "ref_idx ~d, but list ~d holds ~d picture~:p" ref-idx lx (length v)))))


(defun %mc-partition (ss ref-pic px py w h mvx mvy &optional (ref 0) (lx 0) skip-weight)
  "Motion compensate one partition, luma and both chroma planes, into the current picture, and
   apply this reference's weight if the slice carries a prediction weight table."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss)) (sh (ss-sh ss))
         (ybase (+ (pic-yoff pic) (* py (pic-ystride pic)) px))
         (cx (ash px -1)) (cy (ash py -1)) (cw (ash w -1)) (ch (ash h -1))
         (cbase (+ (pic-coff pic) (* cy (pic-cstride pic)) cx)))
    (predict-luma (pic-y pic) (pic-ystride pic) ybase ref-pic px py w h mvx mvy)
    (predict-chroma (pic-u pic) (pic-u ref-pic) (pic-cstride pic) cbase
                    ref-pic cx cy cw ch mvx mvy)
    (predict-chroma (pic-v pic) (pic-v ref-pic) (pic-cstride pic) cbase
                    ref-pic cx cy cw ch mvx mvy)
    (when (and (sh-weighted-p sh) (not skip-weight))
      (let ((lw (if (zerop lx) (sh-luma-weights sh) (or (sh-luma-weights-l1 sh) (sh-luma-weights sh))))
            (cwt (if (zerop lx) (sh-chroma-weights sh)
                     (or (sh-chroma-weights-l1 sh) (sh-chroma-weights sh)))))
        (when (and lw (< ref (array-dimension lw 0)))
          (apply-weight (pic-y pic) (pic-ystride pic) ybase w h
                        (aref lw ref 0) (aref lw ref 1) (sh-luma-log2-denom sh))
          (apply-weight (pic-u pic) (pic-cstride pic) cbase cw ch
                        (aref cwt ref 0 0) (aref cwt ref 0 1) (sh-chroma-log2-denom sh))
          (apply-weight (pic-v pic) (pic-cstride pic) cbase cw ch
                        (aref cwt ref 1 0) (aref cwt ref 1 1) (sh-chroma-log2-denom sh)))))))

(defun %inter-tf8-p (ss mb-type subs eightp b-slice-p)
  "Does this inter macroblock carry a transform_size_8x8_flag, and is it set?

   The condition is not simply `the picture parameter set allowed it'.  A macroblock partitioned
   any finer than 8x8 cannot use an 8x8 transform, because a transform block would then straddle
   two partitions with different motion, so the flag is not sent at all — and a decoder that reads
   it anyway is a bit ahead of the encoder for the rest of the slice.  B_Direct_16x16 is the same
   case wearing a disguise: its motion is per-4x4 unless direct_8x8_inference_flag says otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (and (pps-transform-8x8 (sh-pps (ss-sh ss)))
       (let ((inference (sps-direct-8x8 (sh-sps (ss-sh ss)))))
         (cond
           ((and b-slice-p (zerop mb-type)) inference)   ; B_Direct_16x16
           ((not eightp) t)
           (b-slice-p (dotimes (i 4 t)
                        (let ((st (aref subs i)))
                          (unless (if (zerop st) inference (= 1 (aref +b-sub+ st 0)))
                            (return nil)))))
           (t (dotimes (i 4 t) (unless (zerop (aref subs i)) (return nil))))))
       (%read-transform-size-8x8 ss)))

(defun %inter-luma-residual (ss cbp)
  "The luma residual of an inter macroblock: no prediction step, because motion compensation has
   already written the prediction into the picture."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (pic (ss-pic ss))
         (ybase (pic-y-base pic (ss-mbx ss) (ss-mby ss))))
    (declare (ignorable br))
    (when (ss-tf8 ss)
      (return-from %inter-luma-residual
        (dotimes (i8 4)
          (let ((base (+ ybase (* (ash i8 -1) 8 (pic-ystride pic)) (* (logand i8 1) 8))))
            (if (logbitp i8 cbp)
                (%luma-residual-8x8 ss base i8 nil)
                (dotimes (i 4) (set-luma-nz ss (+ (* 4 i8) i) 0)))))))
    (dotimes (blk 16)
      (let ((base (+ ybase (* (aref +blk-y+ blk) 4 (pic-ystride pic)) (* (aref +blk-x+ blk) 4))))
        (if (logbitp (ash blk -2) cbp)
            (multiple-value-bind (n hi)
                (multiple-value-bind (bx by) (%luma-blk-xy ss blk)
                  (%residual ss (ss-coeffs ss) :cat +cat-luma+ :nc (luma-nc ss blk)
                                               :max-coeff 16 :bx bx :by by))
              (declare (type fixnum n hi))
              (set-luma-nz ss blk n)
              (when (plusp n)
                (dequant-4x4 (ss-coeffs ss) (ss-block ss) (ss-qp ss) :end hi
                                                  :weights (%scale-list ss 3))
                (if (zerop hi)
                    (idct-4x4-dc (ss-block ss) (aref (ss-block ss) 0))
                    (idct-4x4 (ss-block ss)))
                (add-residual-4x4 (pic-y pic) (pic-ystride pic) base (ss-block ss))))
            (set-luma-nz ss blk 0))))))

(defun decode-skip-macroblock (ss)
  "A P_Skip macroblock: no bits of its own at all beyond being counted in the skip run.

   Its vector is predicted the ordinary way with one extra rule, and it has no residual, so a run
   of these is how a still background costs almost nothing."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss)) (mbx (ss-mbx ss)) (mby (ss-mby ss))
         (mbi (+ (* mby (pic-mb-width pic)) mbx)))
    (unless (ss-ref0 ss) (%err "a skipped macroblock with no reference picture"))
    (setf (aref (pic-mb-types pic) mbi) -2)
    (setf (aref (pic-mb-tf8 pic) mbi) 0)
    (setf (ss-mb-done ss) 0)
    (multiple-value-bind (mvx mvy) (skip-mv ss)
      (%set-partition-mvd ss (* 4 mbx) (* 4 mby) 4 4 0 0)
      (%set-partition-motion ss (* 4 mbx) (* 4 mby) 4 4 mvx mvy 0)
      (%mc-partition ss (ss-ref0 ss) (* 16 mbx) (* 16 mby) 16 16 mvx mvy))
    (dotimes (blk 16) (set-luma-nz ss blk 0))
    (dotimes (plane 2) (dotimes (blk 4) (set-chroma-nz ss plane blk 0)))
    (setf (aref (pic-mb-cbp pic) mbi) 0
          (aref (pic-mb-dc-cbf pic) mbi) 0)
    (%no-qp-delta ss)
    (setf (aref (pic-mb-qps pic) mbi) (ss-qp ss))))

(defun decode-p-macroblock (ss mb-type)
  "Parse and reconstruct one INTER macroblock of a P slice (mb_type 0..4).

   THE ORDER OF THE SYNTAX IS NOT THE ORDER OF THE WORK.  Every sub_mb_type comes first, then every
   reference index, then every vector difference — not one partition at a time.  Reading it a
   partition at a time parses without complaint and produces vectors attached to the wrong
   partitions, which looks like a motion compensation bug and is not one."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (pic (ss-pic ss)) (sh (ss-sh ss))
         (mbx (ss-mbx ss)) (mby (ss-mby ss))
         (mbi (+ (* mby (pic-mb-width pic)) mbx))
         (px (* 16 mbx)) (py (* 16 mby))
         (bx0 (* 4 mbx)) (by0 (* 4 mby))
         (nref (max 1 (or (sh-num-ref-idx-l0 sh) 1)))
         (ref-pic (ss-ref0 ss))
         (p8x8 (>= mb-type 3))
         (nparts (aref +p-part-count+ mb-type))
         (pw (aref +p-part-width+ mb-type))
         (ph (aref +p-part-height+ mb-type))
         (shape (case mb-type (1 :16x8) (2 :8x16) (t nil)))
         (subs (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
         (refs (make-array 4 :element-type '(signed-byte 32) :initial-element 0)))
    (declare (dynamic-extent subs refs))
    (unless ref-pic (%err "a P macroblock with no reference picture"))
    (setf (aref (pic-mb-types pic) mbi) (- -3 mb-type))
    (setf (ss-mb-done ss) 0)
    ;; 1. every sub_mb_type
    (when p8x8
      (dotimes (i 4)
        (let ((st (if (ss-cabac ss) (cabac-sub-mb-type-p ss) (ue br))))
          (when (> st 3) (%err "sub_mb_type ~d in a P slice" st))
          (setf (aref subs i) st))))
    ;; 2. every reference index.  P_8x8ref0 (mb_type 4) codes none: they are all zero.
    (when (and (> nref 1) (/= mb-type 4))
      (dotimes (i nparts)
        (multiple-value-bind (sx sy sw sh*)
            (if p8x8
                (values (* 8 (mod i 2)) (* 8 (floor i 2)) 8 8)
                (let ((across (floor 16 pw)))
                  (values (* pw (mod i across)) (* ph (floor i across)) pw ph)))
          (let ((bx (+ bx0 (ash sx -2))) (by (+ by0 (ash sy -2))))
            (setf (aref refs i)
                  (if (ss-cabac ss) (cabac-ref-idx ss bx by) (%te br nref)))
            ;; RECORD IT NOW, not with the vector later.  Every reference index of the macroblock
            ;; is read before any vector difference is, and the context for one partition's index
            ;; asks what its neighbours chose — including the partition beside it in this same
            ;; macroblock, whose index was read a moment ago and whose vector has not been read at
            ;; all yet.  Leaving the array until the vectors are known feeds the arithmetic decoder
            ;; a stale context, and with CABAC that loses the rest of the slice.
            (dotimes (jj (ash sh* -2))
              (dotimes (ii (ash sw -2))
                (setf (aref (pic-refs pic) (* 2 (%mv-index pic (+ bx ii) (+ by jj))))
                      (aref refs i))))))))
    ;; 3. every vector difference, and with it the prediction, the storage and the resampling
    (if p8x8
        (dotimes (i 4)
          (let* ((st (aref subs i))
                 (nsub (aref +p-sub-count+ st))
                 (sw (aref +p-sub-width+ st)) (shh (aref +p-sub-height+ st))
                 (across (floor 8 sw))
                 (ox (* 8 (mod i 2))) (oy (* 8 (floor i 2)))
                 (ref (aref refs i)))
            (dotimes (k nsub)
              (let* ((sx (+ ox (* sw (mod k across))))
                     (sy (+ oy (* shh (floor k across))))
                     (bx (+ bx0 (ash sx -2))) (by (+ by0 (ash sy -2))))
                (multiple-value-bind (mpx mpy) (predict-mv ss bx by (ash sw -2) ref)
                  (let* ((dx (if (ss-cabac ss) (cabac-mvd ss bx by 0) (se br)))
                         (dy (if (ss-cabac ss) (cabac-mvd ss bx by 1) (se br)))
                         (mvx (+ mpx dx)) (mvy (+ mpy dy)))
                    (%set-partition-mvd ss bx by (ash sw -2) (ash shh -2) dx dy)
                    (%set-partition-motion ss bx by (ash sw -2) (ash shh -2) mvx mvy ref)
                    (%mc-partition ss (%ref-picture ss ref) (+ px sx) (+ py sy) sw shh mvx mvy ref)))))))
        (let ((across (floor 16 pw)))
          (dotimes (i nparts)
            (let* ((sx (* pw (mod i across))) (sy (* ph (floor i across)))
                   (bx (+ bx0 (ash sx -2))) (by (+ by0 (ash sy -2)))
                   (ref (aref refs i)))
              (multiple-value-bind (mpx mpy)
                  (predict-mv ss bx by (ash pw -2) ref :shape shape :part i)
                (let* ((dx (if (ss-cabac ss) (cabac-mvd ss bx by 0) (se br)))
                       (dy (if (ss-cabac ss) (cabac-mvd ss bx by 1) (se br)))
                       (mvx (+ mpx dx)) (mvy (+ mpy dy)))
                  (%set-partition-mvd ss bx by (ash pw -2) (ash ph -2) dx dy)
                  (%set-partition-motion ss bx by (ash pw -2) (ash ph -2) mvx mvy ref)
                  (%mc-partition ss (%ref-picture ss ref) (+ px sx) (+ py sy) pw ph mvx mvy ref)))))))
    ;; 4. the residual, on top of what motion compensation predicted
    (let ((cbp (%read-cbp ss nil)))
      (setf (aref (pic-mb-cbp pic) mbi) cbp)
      (setf (ss-tf8 ss) (and (plusp (logand cbp 15)) (%inter-tf8-p ss mb-type subs p8x8 nil)))
      (setf (aref (pic-mb-tf8 pic) mbi) (if (ss-tf8 ss) 1 0))
      (if (plusp cbp)
          (progn (incf (ss-qp ss) (%read-qp-delta ss))
                 (setf (ss-qp ss) (mod (+ (ss-qp ss) 52) 52)))
          (%no-qp-delta ss))
      (%inter-luma-residual ss cbp)
      (decode-chroma ss nil cbp)
      (setf (aref (pic-mb-qps pic) mbi) (ss-qp ss))
      cbp)))

(defun decode-slice (pic sh br &optional refs refs1)
  "Decode every macroblock of a slice into PIC.  REFS and REFS1 are the two reference lists."
  (let* ((cabac-p (pps-cabac (sh-pps sh)))
         (b-slice (sh-b-slice-p sh))
         (p-slice (sh-p-slice-p sh))
         (ss (make-slice-state :pic pic :sh sh :br br :qp (sh-qp sh)
                               :slice-id (sh-first-mb sh)
                               :constrained-intra (pps-constrained-intra (sh-pps sh))
                               :reflist (or refs #())
                               :reflist1 (or refs1 #())
                               :ref0 (and refs (plusp (length refs)) (aref refs 0))
                               :direct-spatial (sh-direct-spatial sh)
                               :cabac (when cabac-p
                                        (init-cabac br (sh-qp sh) (sh-i-slice-p sh)
                                                    (sh-cabac-init-idc sh)))))
         (mbw (pic-mb-width pic))
         (total (* mbw (pic-mb-height pic)))
         (mb (sh-first-mb sh))
         (inter (or p-slice b-slice)))
    (unless (or (sh-i-slice-p sh) inter)
      (%err "slice_type ~a is not supported" (slice-type-name (sh-slice-type sh))))
    (flet ((at (n)
             ;; every macroblock passes through here, skipped ones included, so this is the one
             ;; place that has to record which slice coded it
             (setf (aref (pic-mb-slice pic) n) (ss-slice-id ss)
                   (aref (pic-dbf-idc pic) n) (sh-disable-deblocking sh)
                   (aref (pic-dbf-alpha pic) n) (sh-alpha-offset sh)
                   (aref (pic-dbf-beta pic) n) (sh-beta-offset sh))
             (setf (ss-mbx ss) (mod n mbw) (ss-mby ss) (floor n mbw)))
           (skip-one ()
             (if b-slice (decode-b-skip-macroblock ss) (decode-skip-macroblock ss)))
           (coded-one ()
             (if b-slice
                 (let ((mt (if cabac-p (cabac-mb-type-b ss) (ue br))))
                   (if (< mt 23)
                       (decode-b-macroblock ss mt)
                       (decode-intra-macroblock ss (- mt 23))))
                 (let ((mt (if cabac-p (cabac-mb-type-p ss) (ue br))))
                   (if (< mt 5)
                       (decode-p-macroblock ss mt)
                       (decode-intra-macroblock ss (- mt 5))))))
           (more-p ()
             ;; CABAC ends a slice with an explicit decision, not by running out of bits
             (if cabac-p
                 (zerop (decode-terminate (ss-cabac ss)))
                 (more-rbsp-data-p br))))
      (loop
        (when (>= mb total) (return))
        (cond
          ;; CABAC: every macroblock carries its own skip flag, so there is no run to unroll
          ((and inter cabac-p)
           (at mb)
           (if (cabac-mb-skip-flag ss b-slice) (skip-one) (coded-one))
           (incf mb))
          ;; CAVLC: a RUN of skipped macroblocks precedes each coded one, and the run may be the
          ;; last thing in the slice — so the end-of-data test belongs after it
          (inter
           (let ((skip (ue br)) (ran-out nil))
             (dotimes (i skip)
               (when (>= mb total) (return))
               (at mb)
               (skip-one)
               (incf mb))
             (when (>= mb total) (return))
             (when (and (plusp skip) (not (more-rbsp-data-p br))) (setf ran-out t))
             (unless ran-out
               (at mb)
               (coded-one)
               (incf mb))
             (when ran-out (return))))
          (t
           (at mb)
           (decode-intra-macroblock ss (if cabac-p (cabac-mb-type-i ss) (ue br)))
           (incf mb)))
        (unless (more-p) (return))))
    ss))

;;; ---- B macroblocks -------------------------------------------------------------------------------

(defun %mc-b (ss px py w h mode r0 mv0x mv0y r1 mv1x mv1y)
  "Motion compensate one B partition, from one list or from both.

   Bi-prediction is the mean of two whole predictions, not a blend done sample by sample as they
   are made: list 0 goes into the picture, list 1 into scratch, and the two are averaged.  Doing it
   any other way rounds twice."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((pic (ss-pic ss))
         (ybase (+ (pic-yoff pic) (* py (pic-ystride pic)) px))
         (cx (ash px -1)) (cy (ash py -1)) (cw (ash w -1)) (ch (ash h -1))
         (cbase (+ (pic-coff pic) (* cy (pic-cstride pic)) cx)))
    (flet ((one (lx ref mvx mvy)
             (%mc-partition ss (%ref-picture ss ref lx) px py w h mvx mvy ref lx)))
      (cond
        ((= mode +pred-l0+) (one 0 r0 mv0x mv0y))
        ((= mode +pred-l1+) (one 1 r1 mv1x mv1y))
        (t
         ;; list 0 straight into the picture, list 1 into scratch, then the mean
         (%mc-partition ss (%ref-picture ss r0 0) px py w h mv0x mv0y r0 0 t)
         (let ((rp (%ref-picture ss r1 1)))
           (predict-luma (ss-bi-y ss) w 0 rp px py w h mv1x mv1y)
           (predict-chroma (ss-bi-u ss) (pic-u rp) cw 0 rp cx cy cw ch mv1x mv1y)
           (predict-chroma (ss-bi-v ss) (pic-v rp) cw 0 rp cx cy cw ch mv1x mv1y))
         (let ((idc (pps-weighted-bipred (sh-pps (ss-sh ss)))))
           (case idc
             (2 ;; implicit: the weights come from the order counts, nothing is transmitted
              (multiple-value-bind (w0 w1)
                  (implicit-bi-weights (pic-poc pic) (%ref-picture-poc ss r0 0)
                                       (%ref-picture-poc ss r1 1))
                (weighted-average-into (pic-y pic) (pic-ystride pic) ybase (ss-bi-y ss) w 0 w h
                                       w0 w1 5 0 0)
                (weighted-average-into (pic-u pic) (pic-cstride pic) cbase (ss-bi-u ss) cw 0 cw ch
                                       w0 w1 5 0 0)
                (weighted-average-into (pic-v pic) (pic-cstride pic) cbase (ss-bi-v ss) cw 0 cw ch
                                       w0 w1 5 0 0)))
             (1 ;; explicit: a weight per reference, from the slice header
              (let* ((sh (ss-sh ss))
                     (l0 (sh-luma-weights sh)) (l1 (sh-luma-weights-l1 sh))
                     (c0 (sh-chroma-weights sh)) (c1 (sh-chroma-weights-l1 sh))
                     (ld (sh-luma-log2-denom sh)) (cd (sh-chroma-log2-denom sh)))
                (weighted-average-into (pic-y pic) (pic-ystride pic) ybase (ss-bi-y ss) w 0 w h
                                       (aref l0 r0 0) (aref l1 r1 0) ld
                                       (aref l0 r0 1) (aref l1 r1 1))
                (weighted-average-into (pic-u pic) (pic-cstride pic) cbase (ss-bi-u ss) cw 0 cw ch
                                       (aref c0 r0 0 0) (aref c1 r1 0 0) cd
                                       (aref c0 r0 0 1) (aref c1 r1 0 1))
                (weighted-average-into (pic-v pic) (pic-cstride pic) cbase (ss-bi-v ss) cw 0 cw ch
                                       (aref c0 r0 1 0) (aref c1 r1 1 0) cd
                                       (aref c0 r0 1 1) (aref c1 r1 1 1))))
             (t
              (average-into (pic-y pic) (pic-ystride pic) ybase (ss-bi-y ss) w 0 w h)
              (average-into (pic-u pic) (pic-cstride pic) cbase (ss-bi-u ss) cw 0 cw ch)
              (average-into (pic-v pic) (pic-cstride pic) cbase (ss-bi-v ss) cw 0 cw ch)))))))))

(defun %set-b-motion (ss bx by wb hb mode r0 mv0x mv0y r1 mv1x mv1y)
  "Record a B partition's motion for both lists on every 4x4 block it covers."
  (declare (type slice-state ss) (type fixnum bx by wb hb r0 mv0x mv0y r1 mv1x mv1y)
           (optimize (speed 3) (safety 1)))
  (let ((pic (the picture (ss-pic ss))))
    (dotimes (j hb)
      (dotimes (i wb)
        (setf (aref (pic-blk-direct pic) (%mv-index pic (+ bx i) (+ by j))) 0)
        (if (pred-uses-l0-p mode)
            (set-blk-mv pic (+ bx i) (+ by j) mv0x mv0y r0 0 (%ref-picture-poc ss r0 0))
            (set-blk-mv pic (+ bx i) (+ by j) 0 0 -1 0 +no-ref-poc+))
        (if (pred-uses-l1-p mode)
            (set-blk-mv pic (+ bx i) (+ by j) mv1x mv1y r1 1 (%ref-picture-poc ss r1 1))
            (set-blk-mv pic (+ bx i) (+ by j) 0 0 -1 1 +no-ref-poc+))))
    (let ((bx0 (* 4 (ss-mbx ss))) (by0 (* 4 (ss-mby ss))))
      (dotimes (j hb)
        (dotimes (i wb)
          (let ((lx (- (+ bx i) bx0)) (ly (- (+ by j) by0)))
            (when (and (<= 0 lx 3) (<= 0 ly 3))
              (setf (ss-mb-done ss) (logior (ss-mb-done ss) (ash 1 (+ (* 4 ly) lx)))))))))))

(defun %min-positive (a b)
  "MinPositive of 8.4.1.2.2: the smaller of two reference indices when both exist, and whichever
   one exists when only one does.  -1 means `no reference', and -1 must lose to a real index."
  (if (and (>= a 0) (>= b 0)) (min a b) (max a b)))

(defun %colocated (ss bx by)
  "(values mvx mvy ref-idx) of the block in the co-located picture — list 1's first entry — that
   sits where this one does.  A co-located block that predicted forwards is read from its list 0,
   and one that did not is read from its list 1."
  (let ((col (and (ss-reflist1 ss) (plusp (length (ss-reflist1 ss))) (aref (ss-reflist1 ss) 0))))
    (if (null col)
        (values 0 0 -1)
        (multiple-value-bind (mx my r) (blk-mv col bx by 0)
          (if (>= r 0)
              (values mx my r)
              (blk-mv col bx by 1))))))

(defun %direct-spatial (ss)
  "B_Direct with spatial prediction (8.4.1.2.2), for the whole macroblock.

   The references come from the neighbours — the smallest index either side used — and the vectors
   from the ordinary median prediction.  Then each 4x4 block is checked against the CO-LOCATED
   block in list 1's first picture: where that block barely moved against its own first reference,
   this one is held at zero rather than given the macroblock's vector.  That is what keeps a still
   background still through a run of B pictures instead of letting it drift with its neighbours."
  (let* ((bx0 (* 4 (ss-mbx ss))) (by0 (* 4 (ss-mby ss))))
    (flet ((nbr-ref (lx)
             (multiple-value-bind (ax ay ar) (%neighbour-motion ss (1- bx0) by0 lx)
               (declare (ignore ax ay))
               (multiple-value-bind (bxv byv brf) (%neighbour-motion ss bx0 (1- by0) lx)
                 (declare (ignore bxv byv))
                 (multiple-value-bind (cx cy cr ok) (%neighbour-motion ss (+ bx0 4) (1- by0) lx)
                   (declare (ignore cx cy))
                   (unless ok
                     (multiple-value-bind (dx dy dr) (%neighbour-motion ss (1- bx0) (1- by0) lx)
                       (declare (ignore dx dy))
                       (setf cr dr)))
                   (%min-positive ar (%min-positive brf cr)))))))
      (let ((r0 (nbr-ref 0)) (r1 (nbr-ref 1)) (zero-p nil))
        (when (and (minusp r0) (minusp r1))
          (setf r0 0 r1 0 zero-p t))
        (multiple-value-bind (m0x m0y) (if (minusp r0) (values 0 0) (predict-mv ss bx0 by0 4 r0 :lx 0))
          (multiple-value-bind (m1x m1y) (if (minusp r1) (values 0 0) (predict-mv ss bx0 by0 4 r1 :lx 1))
            (values r0 r1 m0x m0y m1x m1y zero-p)))))))

(defun %col-zero-p (ss bx by)
  "colZeroFlag: did the co-located block sit still, against a picture that is itself a short-term
   reference?  Only then may a direct block be pinned to zero."
  (multiple-value-bind (mx my r) (%colocated ss bx by)
    (and (= r 0) (<= -1 mx 1) (<= -1 my 1))))

(defun %direct-temporal (ss bx by)
  "B_Direct with temporal prediction (8.4.1.2.1): take the co-located block's vector and SPLIT it
   between the two references in proportion to how far each one is away in display order.

   Where spatial direct asks the neighbours what is going on, temporal direct asks the future
   picture what happened and interpolates backwards through it."
  (let* ((pic (ss-pic ss))
         (col (and (ss-reflist1 ss) (plusp (length (ss-reflist1 ss))) (aref (ss-reflist1 ss) 0))))
    (multiple-value-bind (mx my r) (%colocated ss bx by)
      (if (or (null col) (minusp r))
          (values 0 0 0 0 0 0)
          ;; the co-located block's reference, found in OUR list 0 by matching order counts
          (let* ((col-ref-poc (blk-ref-poc col bx by (if (>= (nth-value 2 (blk-mv col bx by 0)) 0) 0 1)))
                 (l0 (ss-reflist ss))
                 (idx (or (position col-ref-poc l0 :key #'pic-poc :test #'=) 0))
                 (poc0 (pic-poc (aref l0 idx)))
                 (poc1 (pic-poc col))
                 (curr (pic-poc pic))
                 (td (max -128 (min 127 (- poc1 poc0))))
                 (tb (max -128 (min 127 (- curr poc0)))))
            (if (zerop td)
                (values idx 0 mx my 0 0)
                ;; TRUNCATE, not FLOOR.  The specification's `/' truncates toward zero and Lisp's
                ;; FLOOR rounds toward negative infinity; they agree for positive values and part
                ;; company for negative ones.  TD is negative whenever the co-located picture's own
                ;; reference sits after it in display order, which cannot happen with a single
                ;; reference picture and happens constantly with several — so this is invisible
                ;; until a stream uses both temporal direct and more than one reference.
                (let* ((tx (truncate (+ 16384 (abs (truncate td 2))) td))
                       (scale (max -1024 (min 1023 (ash (+ (* tb tx) 32) -6))))
                       (m0x (ash (+ (* scale mx) 128) -8))
                       (m0y (ash (+ (* scale my) 128) -8)))
                  (values idx 0 m0x m0y (- m0x mx) (- m0y my)))))))))

(defun %direct-col-xy (ss bx by)
  "Which block of the co-located picture a direct 4x4 block reads.

   With direct_8x8_inference_flag set — which every ordinary stream sets — all four 4x4 blocks of
   an 8x8 read the SAME co-located block, the one at the 8x8's outer corner.  It exists so that a
   decoder need only keep motion at 8x8 granularity for reference pictures."
  (let ((pic (ss-pic ss)) (sps (sh-sps (ss-sh ss))))
    (declare (ignorable pic))
    (if (sps-direct-8x8 sps)
        (let* ((bx0 (* 4 (ss-mbx ss))) (by0 (* 4 (ss-mby ss)))
               (lx (- bx bx0)) (ly (- by by0)))
          (values (+ bx0 (if (< lx 2) 0 3)) (+ by0 (if (< ly 2) 0 3))))
        (values bx by))))

(defun %apply-direct (ss bx by)
  "Predict and record one 4x4 direct block, by whichever direct method the slice chose."
  (setf (aref (pic-blk-direct (ss-pic ss)) (%mv-index (ss-pic ss) bx by)) 1)
  (let ((px (* 4 bx)) (py (* 4 by)))
    (multiple-value-bind (cbx cby) (%direct-col-xy ss bx by)
      (if (ss-direct-spatial ss)
          (multiple-value-bind (r0 r1 m0x m0y m1x m1y zero-p) (%direct-spatial ss)
            (let ((z (and (not zero-p) (%col-zero-p ss cbx cby))))
              (when (or zero-p (and z (= r0 0))) (setf m0x 0 m0y 0))
              (when (or zero-p (and z (= r1 0))) (setf m1x 0 m1y 0))
              (let ((mode (cond ((and (>= r0 0) (>= r1 0)) +pred-bi+)
                                ((>= r0 0) +pred-l0+)
                                (t +pred-l1+))))
                (%set-b-motion ss bx by 1 1 mode r0 m0x m0y r1 m1x m1y)
                (%mc-b ss px py 4 4 mode r0 m0x m0y r1 m1x m1y))))
          (multiple-value-bind (r0 r1 m0x m0y m1x m1y) (%direct-temporal ss cbx cby)
            (%set-b-motion ss bx by 1 1 +pred-bi+ r0 m0x m0y r1 m1x m1y)
            (%mc-b ss px py 4 4 +pred-bi+ r0 m0x m0y r1 m1x m1y))))
    (setf (aref (pic-blk-direct (ss-pic ss)) (%mv-index (ss-pic ss) bx by)) 1)))

(defun %direct-16x16 (ss)
  "A whole macroblock of direct blocks."
  (let ((bx0 (* 4 (ss-mbx ss))) (by0 (* 4 (ss-mby ss))))
    (dotimes (j 4) (dotimes (i 4) (%apply-direct ss (+ bx0 i) (+ by0 j))))))

(defun decode-b-skip-macroblock (ss)
  "B_Skip: direct prediction and no residual at all."
  (let* ((pic (ss-pic ss)) (mbi (%mb-index ss)))
    (setf (aref (pic-mb-types pic) mbi) -2
          (aref (pic-mb-tf8 pic) mbi) 0
          (ss-mb-done ss) 0)
    (%direct-16x16 ss)
    (dotimes (blk 16) (set-luma-nz ss blk 0))
    (dotimes (plane 2) (dotimes (blk 4) (set-chroma-nz ss plane blk 0)))
    (setf (aref (pic-mb-cbp pic) mbi) 0
          (aref (pic-mb-dc-cbf pic) mbi) 0)
    (%no-qp-delta ss)
    (setf (aref (pic-mb-qps pic) mbi) (ss-qp ss))))

(defun decode-b-macroblock (ss mb-type)
  "One inter macroblock of a B slice (mb_type 0..22).

   THE SYNTAX INTERLEAVES BY LIST, NOT BY PARTITION: every list-0 reference index, then every
   list-1 one, then every list-0 vector difference, then every list-1 one.  Reading it a partition
   at a time parses without complaint and attaches the values to the wrong partitions."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (pic (ss-pic ss)) (sh (ss-sh ss))
         (mbi (%mb-index ss))
         (px (* 16 (ss-mbx ss))) (py (* 16 (ss-mby ss)))
         (bx0 (* 4 (ss-mbx ss))) (by0 (* 4 (ss-mby ss)))
         (n0 (max 1 (or (sh-num-ref-idx-l0 sh) 1)))
         (n1 (max 1 (or (sh-num-ref-idx-l1 sh) 1)))
         (b8x8 (= mb-type 22))
         (nparts (aref +b-part+ mb-type 0))
         (pw (aref +b-part+ mb-type 1))
         (ph (aref +b-part+ mb-type 2))
         (subs (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
         (modes (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
         (r0s (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
         (r1s (make-array 4 :element-type '(signed-byte 32) :initial-element 0)))
    (declare (dynamic-extent subs modes r0s r1s))
    (setf (aref (pic-mb-types pic) mbi) (- -3 mb-type)
          (ss-mb-done ss) 0)
    (cond
      ((zerop mb-type) (%direct-16x16 ss))              ; B_Direct_16x16
      (t
       (if b8x8
           (dotimes (i 4)
             (let ((st (if (ss-cabac ss) (cabac-sub-mb-type-b ss) (ue br))))
               (when (> st 12) (%err "sub_mb_type ~d in a B slice" st))
               (setf (aref subs i) st (aref modes i) (aref +b-sub+ st 3))))
           (dotimes (i nparts)
             (setf (aref modes i) (aref +b-part+ mb-type (+ 3 i)))))
       (when (equal *debug-mb* (cons (ss-mbx ss) (ss-mby ss)))
         (format t "~&  B mb(~d,~d) type=~d subs=~a modes=~a~%"
                 (ss-mbx ss) (ss-mby ss) mb-type (coerce subs 'list) (coerce modes 'list)))
       ;; the reference indices, list 0 for every partition and then list 1 for every partition
       (dotimes (lx 2)
         (dotimes (i nparts)
           (let ((m (aref modes i))
                 (n (if (zerop lx) n0 n1)))
             (when (and (/= m +pred-direct+)
                        (if (zerop lx) (pred-uses-l0-p m) (pred-uses-l1-p m))
                        (> n 1))
               (multiple-value-bind (sx sy sw sh*)
                   (if b8x8
                       (values (* 8 (mod i 2)) (* 8 (floor i 2)) 8 8)
                       (let ((across (floor 16 pw)))
                         (values (* pw (mod i across)) (* ph (floor i across)) pw ph)))
                 (let ((bx (+ bx0 (ash sx -2))) (by (+ by0 (ash sy -2))))
                   (setf (aref (if (zerop lx) r0s r1s) i)
                         (if (ss-cabac ss) (cabac-ref-idx ss bx by lx) (%te br n)))
                   ;; recorded at once, because the next partition's context asks what this one chose
                   (dotimes (jj (ash sh* -2))
                     (dotimes (ii (ash sw -2))
                       (setf (aref (pic-refs pic)
                                   (+ (* 2 (%mv-index pic (+ bx ii) (+ by jj))) lx))
                             (aref (if (zerop lx) r0s r1s) i))))))))))
       ;; READING AND DERIVING ARE TWO PASSES, and they run in different orders.
       ;;
       ;; The syntax is list-major: every list-0 vector difference for the whole macroblock, then
       ;; every list-1 one.  The DERIVATION is partition-major: partition 1 predicts its vectors
       ;; from partition 0's, and must not see partition 2's, in either list.  Those two orders are
       ;; incompatible, so the differences are read first as plain numbers — nothing about reading
       ;; them depends on prediction — and the vectors are worked out afterwards, one partition at a
       ;; time.  Doing it in one pass makes a later partition visible to an earlier one during the
       ;; list-1 pass, which is a wrong prediction and not a desynchronisation.
       (let ((mvd (make-array '(4 4 2 2) :element-type '(signed-byte 32) :initial-element 0)))
         (declare (dynamic-extent mvd))
         (dotimes (lx 2)
           (dotimes (i nparts)
             (let* ((m (aref modes i))
                    (uses (if (zerop lx) (pred-uses-l0-p m) (pred-uses-l1-p m))))
               (when (and (/= m +pred-direct+) uses)
                 (multiple-value-bind (sx sy sw sh*)
                     (if b8x8
                         (values (* 8 (mod i 2)) (* 8 (floor i 2))
                                 (aref +b-sub+ (aref subs i) 1) (aref +b-sub+ (aref subs i) 2))
                         (let ((across (floor 16 pw)))
                           (values (* pw (mod i across)) (* ph (floor i across)) pw ph)))
                   (let ((nsub (if b8x8 (aref +b-sub+ (aref subs i) 0) 1))
                         (across (if b8x8 (floor 8 sw) 1)))
                     (dotimes (k nsub)
                       (let* ((ox (if b8x8 (* sw (mod k across)) 0))
                              (oy (if b8x8 (* sh* (floor k across)) 0))
                              (bx (+ bx0 (ash (+ sx ox) -2))) (by (+ by0 (ash (+ sy oy) -2)))
                              (dx (if (ss-cabac ss) (cabac-mvd ss bx by 0 lx) (se br)))
                              (dy (if (ss-cabac ss) (cabac-mvd ss bx by 1 lx) (se br))))
                         (setf (aref mvd i k lx 0) dx (aref mvd i k lx 1) dy)
                         ;; recorded now because the NEXT difference's context asks how big the
                         ;; neighbouring differences were, and that is a property of the bitstream
                         ;; rather than of the prediction
                         (%set-partition-mvd ss bx by (ash sw -2) (ash sh* -2) dx dy lx)))))))))
         ;; now the vectors, one partition at a time
         (dotimes (i nparts)
           (let ((m (aref modes i)))
             (multiple-value-bind (sx sy sw sh*)
                 (if b8x8
                     (values (* 8 (mod i 2)) (* 8 (floor i 2))
                             (aref +b-sub+ (aref subs i) 1) (aref +b-sub+ (aref subs i) 2))
                     (let ((across (floor 16 pw)))
                       (values (* pw (mod i across)) (* ph (floor i across)) pw ph)))
               (if (= m +pred-direct+)
                   ;; a direct 8x8 is four direct 4x4s, derived at this partition's turn
                   (dotimes (j 2)
                     (dotimes (ii 2)
                       (%apply-direct ss (+ bx0 (ash sx -2) ii) (+ by0 (ash sy -2) j))))
                   (let ((nsub (if b8x8 (aref +b-sub+ (aref subs i) 0) 1))
                         (across (if b8x8 (floor 8 sw) 1)))
                     (dotimes (k nsub)
                       (let* ((ox (if b8x8 (* sw (mod k across)) 0))
                              (oy (if b8x8 (* sh* (floor k across)) 0))
                              (bx (+ bx0 (ash (+ sx ox) -2))) (by (+ by0 (ash (+ sy oy) -2)))
                              (wb (ash sw -2)) (hb (ash sh* -2))
                              (mv0x 0) (mv0y 0) (mv1x 0) (mv1y 0))
                         (when (pred-uses-l0-p m)
                           (multiple-value-bind (px* py*)
                               (predict-mv ss bx by wb (aref r0s i) :lx 0
                                           :shape (and (not b8x8)
                                                       (cond ((and (= pw 16) (= ph 8)) :16x8)
                                                             ((and (= pw 8) (= ph 16)) :8x16)))
                                           :part i)
                             (setf mv0x (+ px* (aref mvd i k 0 0))
                                   mv0y (+ py* (aref mvd i k 0 1)))))
                         (when (pred-uses-l1-p m)
                           (multiple-value-bind (px* py*)
                               (predict-mv ss bx by wb (aref r1s i) :lx 1
                                           :shape (and (not b8x8)
                                                       (cond ((and (= pw 16) (= ph 8)) :16x8)
                                                             ((and (= pw 8) (= ph 16)) :8x16)))
                                           :part i)
                             (setf mv1x (+ px* (aref mvd i k 1 0))
                                   mv1y (+ py* (aref mvd i k 1 1)))))
                         (%set-b-motion ss bx by wb hb m
                                        (aref r0s i) mv0x mv0y (aref r1s i) mv1x mv1y)
                         (%mc-b ss (+ px sx ox) (+ py sy oy) sw sh* m
                                (aref r0s i) mv0x mv0y (aref r1s i) mv1x mv1y)))))))))))
    ;; the residual, on top of whatever was predicted
    (let ((cbp (%read-cbp ss nil)))
      (setf (aref (pic-mb-cbp pic) mbi) cbp)
      (setf (ss-tf8 ss) (and (plusp (logand cbp 15)) (%inter-tf8-p ss mb-type subs b8x8 t)))
      (setf (aref (pic-mb-tf8 pic) mbi) (if (ss-tf8 ss) 1 0))
      (if (plusp cbp)
          (progn (incf (ss-qp ss) (%read-qp-delta ss))
                 (setf (ss-qp ss) (mod (+ (ss-qp ss) 52) 52)))
          (%no-qp-delta ss))
      (%inter-luma-residual ss cbp)
      (decode-chroma ss nil cbp)
      (setf (aref (pic-mb-qps pic) mbi) (ss-qp ss))
      cbp)))

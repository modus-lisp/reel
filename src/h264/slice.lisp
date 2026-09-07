;;;; h264/slice.lisp — the macroblock layer and the slice decoding loop (7.3.5, 8.3, 8.5).
;;;;
;;;; This is where the pieces meet: a macroblock's type says how it is predicted, its coded block
;;;; pattern says which of its blocks carry residual, CAVLC produces the coefficients, the
;;;; transforms turn them into a residual, and the prediction plus that residual is the picture.
;;;;
;;;; THE PART THAT IS NOT OBVIOUS FROM THE PIECES is bookkeeping, and it is most of the file:
;;;;
;;;;   * PREDICTED PREDICTION MODES.  An Intra4x4 block does not code its mode outright; it codes
;;;;     whether the mode equals the smaller of its two neighbours' modes, and if not, which of
;;;;     the remaining eight it is.  So every block's mode has to be remembered for the blocks
;;;;     below and right of it.
;;;;   * nC.  Every residual block's VLC table is chosen by how many coefficients its LEFT and
;;;;     ABOVE neighbours had — across macroblock boundaries — so a per-picture grid of
;;;;     coefficient counts has to be kept, at 4x4 granularity, for luma and both chroma planes.
;;;;   * AVAILABILITY.  A neighbour exists if it is inside the picture and earlier in raster
;;;;     order.  With one slice per picture that is the whole rule; a decoder that grows multiple
;;;;     slices per picture must also check they are the same slice, which is why the check is a
;;;;     function rather than an inline comparison.
;;;;
;;;; Only I slices are decoded here.  P slices parse (see params.lisp) but their macroblocks need
;;;; motion compensation, which is the next increment.

(in-package #:reel.h264)

;;; ---- where a macroblock's 4x4 blocks live ---------------------------------------------------

(defparameter +blk-x+
  ;; luma block index (0..15) to its x position in 4-sample units: 8x8 sub-blocks in raster
  ;; order, and 4x4 blocks in raster order inside each of those
  (make-array 16 :element-type 'fixnum
                 :initial-contents '(0 1 0 1  2 3 2 3  0 1 0 1  2 3 2 3)))
(defparameter +blk-y+
  (make-array 16 :element-type 'fixnum
                 :initial-contents '(0 0 1 1  0 0 1 1  2 2 3 3  2 2 3 3)))

(defparameter +blk-index+
  ;; the inverse of +BLK-X+/+BLK-Y+: which block index sits at (x, y) in 4x4 units.  Needed to ask
  ;; "has that neighbour been decoded yet", which is a question about DECODING ORDER, and decoding
  ;; order here is 8x8 sub-blocks in raster with 4x4s in raster inside them — not raster overall.
  (make-array '(4 4) :element-type 'fixnum
                     :initial-contents '((0 1 4 5)
                                         (2 3 6 7)
                                         (8 9 12 13)
                                         (10 11 14 15))))

(defparameter +intra4x4-cbp+
  ;; Table 9-4: the mapping from the coded_block_pattern code number to the pattern itself, for
  ;; Intra_4x4 macroblocks.  Six bits: four luma 8x8 flags, then 0/1/2 for chroma.
  (make-array 48 :element-type '(unsigned-byte 8)
                 :initial-contents '(47 31 15 0  23 27 29 30  7 11 13 14  39 43 45 46
                                     16 3 5 10  12 19 21 26  28 35 37 42  44 1 2 4
                                     8 17 18 20  24 6 9 22  25 32 33 34  36 40 38 41)))

;;; ---- the picture being decoded into ----------------------------------------------------------

(defconstant +pad+ 16 "Border samples around each plane, so a neighbour read at -1 is in bounds.")

(deftype fixnums () '(simple-array fixnum (*)))

(declaim (inline %empty8 %emptyfx))
(defun %empty8 () (make-array 0 :element-type '(unsigned-byte 8)))
(defun %emptyfx () (make-array 0 :element-type 'fixnum))

(defstruct (picture (:conc-name pic-))
  (width 0 :type fixnum) (height 0 :type fixnum)          ; displayed, after cropping
  (mb-width 0 :type fixnum) (mb-height 0 :type fixnum)
  ;; TYPED, and it matters more than it looks.  Every sample the decoder reads or writes goes
  ;; through one of these slots, and an untyped slot makes each of those a generic array dispatch
  ;; at run time — it measured at 4% of decode time all by itself.
  (y (%empty8) :type octets) (u (%empty8) :type octets) (v (%empty8) :type octets)
  (ystride 0 :type fixnum) (cstride 0 :type fixnum)
  (yoff 0 :type fixnum) (coff 0 :type fixnum)
  ;; per-4x4-block coefficient counts, for nC
  (nz-y (%emptyfx) :type fixnums) (nz-u (%emptyfx) :type fixnums) (nz-v (%emptyfx) :type fixnums)
  ;; per-4x4-block Intra4x4 prediction modes, and per-macroblock facts the loop filter needs
  (modes (%emptyfx) :type fixnums)
  ;; MB-TYPES doubles as "has this macroblock been decoded": -1 means not yet, a value >= 0 is an
  ;; intra type, and an inter one is stored negative.  That keeps availability and Intra4x4 mode
  ;; prediction correct without either of them having to learn about inter macroblocks.
  (mb-types (%emptyfx) :type fixnums)
  (mb-qps (%emptyfx) :type fixnums)
  ;; motion, per 4x4 block: two components each, in quarter-pel units, and the reference index
  ;; (-1 where the block is intra).  The loop filter reads both, and so does the next picture.
  (mvs (%emptyfx) :type fixnums)
  (refs (%emptyfx) :type fixnums)
  ;; the coded DIFFERENCES, which only CABAC needs: the context for a vector difference is chosen
  ;; from how large the neighbouring differences were, not from the vectors themselves
  (mvds (%emptyfx) :type fixnums)
  ;; Per macroblock, and only CABAC reads them: it chooses a context for nearly every syntax
  ;; element from what the neighbouring macroblocks decoded, so facts CAVLC could forget as soon
  ;; as it used them have to survive here.
  (mb-cbp (%emptyfx) :type fixnums)       ; coded_block_pattern, for the CBP contexts
  (mb-chroma-mode (%emptyfx) :type fixnums) ; intra_chroma_pred_mode
  (mb-dc-cbf (%emptyfx) :type fixnums))   ; bit 0 luma DC, 1 Cb DC, 2 Cr DC

(defun make-picture-for (sps)
  (let* ((mbw (sps-mb-width sps)) (mbh (sps-mb-height sps))
         (aw (* 16 mbw)) (ah (* 16 mbh))
         (ys (+ aw (* 2 +pad+))) (cs (+ (ash aw -1) (* 2 +pad+))))
    (make-picture
     :width (sps-width sps) :height (sps-height sps)
     :mb-width mbw :mb-height mbh
     :y (make-array (* ys (+ ah (* 2 +pad+))) :element-type '(unsigned-byte 8) :initial-element 128)
     :u (make-array (* cs (+ (ash ah -1) (* 2 +pad+))) :element-type '(unsigned-byte 8) :initial-element 128)
     :v (make-array (* cs (+ (ash ah -1) (* 2 +pad+))) :element-type '(unsigned-byte 8) :initial-element 128)
     :ystride ys :cstride cs
     :yoff (+ (* +pad+ ys) +pad+) :coff (+ (* +pad+ cs) +pad+)
     :nz-y (make-array (* mbw 4 mbh 4) :element-type 'fixnum :initial-element 0)
     :nz-u (make-array (* mbw 2 mbh 2) :element-type 'fixnum :initial-element 0)
     :nz-v (make-array (* mbw 2 mbh 2) :element-type 'fixnum :initial-element 0)
     :modes (make-array (* mbw 4 mbh 4) :element-type 'fixnum :initial-element 2)
     :mb-types (make-array (* mbw mbh) :element-type 'fixnum :initial-element -1)
     :mb-qps (make-array (* mbw mbh) :element-type 'fixnum :initial-element 0)
     :mvs (make-array (* mbw 4 mbh 4 2) :element-type 'fixnum :initial-element 0)
     :refs (make-array (* mbw 4 mbh 4) :element-type 'fixnum :initial-element -1)
     :mvds (make-array (* mbw 4 mbh 4 2) :element-type 'fixnum :initial-element 0)
     :mb-cbp (make-array (* mbw mbh) :element-type 'fixnum :initial-element 0)
     :mb-chroma-mode (make-array (* mbw mbh) :element-type 'fixnum :initial-element 0)
     :mb-dc-cbf (make-array (* mbw mbh) :element-type 'fixnum :initial-element 0))))

(declaim (inline pic-y-base pic-c-base))
(defun pic-y-base (p mbx mby)
  
  (declare (optimize (speed 3) (safety 1)))(+ (pic-yoff p) (* (* 16 mby) (pic-ystride p)) (* 16 mbx)))
(defun pic-c-base (p mbx mby)
  
  (declare (optimize (speed 3) (safety 1)))(+ (pic-coff p) (* (* 8 mby) (pic-cstride p)) (* 8 mbx)))

;;; ---- the decoding state for one slice -----------------------------------------------------------

(defstruct (slice-state (:conc-name ss-))
  pic sh br
  ref0                                          ; list-0 entry 0, the common case
  (reflist #() :type simple-vector)              ; the whole of list 0, indexed by ref_idx
  ;; Which of this macroblock's sixteen 4x4 blocks have had their motion assigned yet, as a bit
  ;; per block in raster order.  6.4.11.7 marks a neighbouring partition NOT AVAILABLE while it is
  ;; still undecoded, and that is not the same as available-with-a-zero-vector: an unavailable C
  ;; makes D stand in for it, and a zero one does not.  The difference only shows on partitions
  ;; smaller than 8x8, where a partition's above-right neighbour can be a later partition of the
  ;; same macroblock.
  (mb-done 0 :type fixnum)
  cabac                                         ; the arithmetic decoder, or NIL for CAVLC
  (mbx 0 :type fixnum) (mby 0 :type fixnum)
  (qp 26 :type fixnum)
  ;; typed for the same reason the picture's planes are: these are read and written per block
  (coeffs (make-array 16 :element-type 'fixnum) :type fixnums)   ; scan-order scratch
  (block (make-array 16 :element-type 'fixnum) :type fixnums)    ; raster-order scratch
  (luma-dc (make-array 16 :element-type 'fixnum) :type fixnums)
  (chroma-dc (make-array 4 :element-type 'fixnum) :type fixnums)
  ;; the second plane's, so neither is per-macroblock
  (chroma-dc-v (make-array 4 :element-type 'fixnum) :type fixnums)
  (pred (make-array 16 :element-type '(unsigned-byte 8)) :type octets)
  (pt (make-array 9 :element-type '(unsigned-byte 8)) :type octets)
  (pl (make-array 4 :element-type '(unsigned-byte 8)) :type octets))

(defun mb-available-p (ss dx dy)
  "Is the macroblock at (mbx+DX, mby+DY) decoded and in this slice?

   Raster order and one slice per picture, so this is `is it inside, and is it before us'.  A
   decoder with several slices per picture must also compare slice numbers here, which is why
   this is one function and not an inline test in six places."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((x (+ (ss-mbx ss) dx)) (y (+ (ss-mby ss) dy))
         (pic (ss-pic ss)))
    (and (>= x 0) (>= y 0) (< x (pic-mb-width pic)) (< y (pic-mb-height pic))
         (/= -1 (aref (pic-mb-types pic) (+ (* y (pic-mb-width pic)) x))))))

(declaim (inline mb-intra-p %mv-index))
(defun mb-intra-p (pic mbx mby)
  "Was the macroblock at MBX,MBY coded intra?  Undecoded counts as not-intra and callers check
   availability separately."
  (>= (aref (pic-mb-types pic) (+ (* mby (pic-mb-width pic)) mbx)) 0))

(defun %mv-index (pic bx by)
  "Index of the 4x4 block at picture block coordinates BX,BY in the motion arrays."
  (+ (* by (* 4 (pic-mb-width pic))) bx))

(defun blk-mv (pic bx by)
  "(values mvx mvy ref) for the 4x4 block at BX,BY, all in quarter-pel units."
  (let ((i (%mv-index pic bx by)))
    (values (aref (pic-mvs pic) (* 2 i)) (aref (pic-mvs pic) (1+ (* 2 i))) (aref (pic-refs pic) i))))

(defun set-blk-mv (pic bx by mvx mvy ref)
  (let ((i (%mv-index pic bx by)))
    (setf (aref (pic-mvs pic) (* 2 i)) mvx
          (aref (pic-mvs pic) (1+ (* 2 i))) mvy
          (aref (pic-refs pic) i) ref)))

(declaim (inline mb-skipped-p blk-mvd))
(defun mb-skipped-p (pic mbx mby)
  "Was that macroblock a P_Skip?  -2 is reserved for it; other inter types are -3 and below, so
   the two are distinguishable, which the skip-flag context needs and nothing else does."
  (= -2 (aref (pic-mb-types pic) (+ (* mby (pic-mb-width pic)) mbx))))

(defun blk-mvd (pic bx by)
  (let ((i (%mv-index pic bx by)))
    (values (aref (pic-mvds pic) (* 2 i)) (aref (pic-mvds pic) (1+ (* 2 i))))))

(defun set-blk-mvd (pic bx by dx dy)
  (let ((i (%mv-index pic bx by)))
    (setf (aref (pic-mvds pic) (* 2 i)) dx
          (aref (pic-mvds pic) (1+ (* 2 i))) dy)))

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

(defun neighbour-mb-available-p (ss blk dx dy)
  "Is the MACROBLOCK containing the neighbour DX,DY away from BLK available?"
  (declare (optimize (speed 3) (safety 1)))
  (let* ((lx (+ (aref +blk-x+ blk) dx)) (ly (+ (aref +blk-y+ blk) dy))
         (mbdx (cond ((minusp lx) -1) ((> lx 3) 1) (t 0)))
         (mbdy (cond ((minusp ly) -1) ((> ly 3) 1) (t 0))))
    (or (and (zerop mbdx) (zerop mbdy))          ; inside this macroblock
        (mb-available-p ss mbdx mbdy))))

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
      ((minusp ny) (if (> nx 3) (mb-available-p ss 1 -1) (mb-available-p ss 0 -1)))
      ;; the macroblock to the right, which is always later in raster order
      ((> nx 3) nil)
      ;; inside this macroblock: only if that block comes earlier in decoding order
      (t (< (aref +blk-index+ ny nx) blk)))))

(defvar *debug-blk* nil "When (mb . blk), print that block's decode in detail.")

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
      (dotimes (j 4) (dotimes (i 4)
                       (set-blk-mv pic (+ bx i) (+ by j) 0 0 -1)
                       (set-blk-mvd pic (+ bx i) (+ by j) 0 0))))
    (if (zerop mb-type)
        (decode-i4x4-macroblock ss)
        (decode-i16x16-macroblock ss (1- mb-type)))
    (setf (aref (pic-mb-qps pic) mbi) (ss-qp ss))
    mb-type))

(defun decode-i-macroblock (ss)
  "Parse and reconstruct one I-slice macroblock."
  (declare (optimize (speed 3) (safety 1)))
  (decode-intra-macroblock ss (ue (ss-br ss))))

(defun %chroma-pred-mode (br) 
  (declare (optimize (speed 3) (safety 1)))(ue br))

(defun decode-i4x4-macroblock (ss)
  
  (declare (optimize (speed 3) (safety 1)))(let* ((br (ss-br ss)) (pic (ss-pic ss))
         (modes (make-array 16 :element-type 'fixnum)))
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
               (up-p (or (plusp by) (mb-available-p ss 0 -1)))
               (left-p (or (plusp bx) (mb-available-p ss -1 0)))
               (up-left-p (cond ((and (plusp bx) (plusp by)) t)
                                ((plusp bx) (mb-available-p ss 0 -1))
                                ((plusp by) (mb-available-p ss -1 0))
                                (t (mb-available-p ss -1 -1))))
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
                  (dequant-4x4 (ss-coeffs ss) (ss-block ss) (ss-qp ss) :end hi)
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
                          (mb-available-p ss 0 -1) (mb-available-p ss -1 0))
      ;; the DC block: sixteen coefficients, one per 4x4, transformed again
      (let ((dc (ss-luma-dc ss)))
        (let ((n (%residual ss (ss-coeffs ss) :cat +cat-luma-dc+ :nc (luma-nc ss 0) :max-coeff 16)))
          ;; CABAC asks its neighbours whether THEY had a luma DC block, so record that we did
          (when (plusp n)
            (setf (aref (pic-mb-dc-cbf pic) mbi) (logior (aref (pic-mb-dc-cbf pic) mbi) 1)))
          ;; the DC block's own coefficients are in scan order; un-scan them into raster
          (fill dc 0)
          (dotimes (i 16) (setf (aref dc (aref +zigzag-4x4+ i)) (aref (ss-coeffs ss) i))))
        (luma-dc-transform dc (ss-qp ss))
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
                         (dequant-4x4 (ss-coeffs ss) (ss-block ss) (ss-qp ss) :start 1 :end hi))
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
         (up-p (mb-available-p ss 0 -1))
         (left-p (mb-available-p ss -1 0))
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
          (chroma-dc-transform dc (aref qps plane)))))
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
                         (dequant-4x4 (ss-coeffs ss) (ss-block ss) qpc :start 1 :end hi))
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

(defun %set-partition-mvd (ss bx by wb hb dx dy)
  (let ((pic (ss-pic ss)))
    (dotimes (j hb) (dotimes (i wb) (set-blk-mvd pic (+ bx i) (+ by j) dx dy)))))

(defun %set-partition-motion (ss bx by wb hb mvx mvy ref)
  "Record one partition's vector on every 4x4 block it covers.

   Per block rather than per partition because that is the granularity everything downstream reads
   at: the next partition's vector prediction, the next picture's, and the loop filter's boundary
   strength all ask about 4x4 blocks and do not care how they were grouped."
  (declare (optimize (speed 3) (safety 1)))
  (let ((pic (ss-pic ss))
        (bx0 (* 4 (ss-mbx ss))) (by0 (* 4 (ss-mby ss))))
    (dotimes (j hb)
      (dotimes (i wb)
        (set-blk-mv pic (+ bx i) (+ by j) mvx mvy ref)
        (let ((lx (- (+ bx i) bx0)) (ly (- (+ by j) by0)))
          (when (and (<= 0 lx 3) (<= 0 ly 3))
            (setf (ss-mb-done ss) (logior (ss-mb-done ss) (ash 1 (+ (* 4 ly) lx))))))))))

(defun %ref-picture (ss ref-idx)
  "List-0 entry REF-IDX.  Compensating from the wrong reference is invisible to the parser — it
   corrupts the picture and keeps decoding — so this refuses an index it cannot honour."
  (let ((v (ss-reflist ss)))
    (if (and (>= ref-idx 0) (< ref-idx (length v)))
        (aref v ref-idx)
        (%err "ref_idx ~d, but list 0 holds ~d picture~:p" ref-idx (length v)))))

(defun %mc-partition (ss ref-pic px py w h mvx mvy)
  "Motion compensate one partition, luma and both chroma planes, into the current picture."
  (declare (optimize (speed 3) (safety 1)))
  (let ((pic (ss-pic ss)))
    (predict-luma (pic-y pic) (pic-ystride pic)
                  (+ (pic-yoff pic) (* py (pic-ystride pic)) px)
                  ref-pic px py w h mvx mvy)
    (let ((cx (ash px -1)) (cy (ash py -1)) (cw (ash w -1)) (ch (ash h -1)))
      (let ((cbase (+ (pic-coff pic) (* cy (pic-cstride pic)) cx)))
        (predict-chroma (pic-u pic) (pic-u ref-pic) (pic-cstride pic) cbase
                        ref-pic cx cy cw ch mvx mvy)
        (predict-chroma (pic-v pic) (pic-v ref-pic) (pic-cstride pic) cbase
                        ref-pic cx cy cw ch mvx mvy)))))

(defun %inter-luma-residual (ss cbp)
  "The luma residual of an inter macroblock: no prediction step, because motion compensation has
   already written the prediction into the picture."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (pic (ss-pic ss))
         (ybase (pic-y-base pic (ss-mbx ss) (ss-mby ss))))
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
                (dequant-4x4 (ss-coeffs ss) (ss-block ss) (ss-qp ss) :end hi)
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
         (subs (make-array 4 :element-type 'fixnum :initial-element 0))
         (refs (make-array 4 :element-type 'fixnum :initial-element 0)))
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
                (setf (aref (pic-refs pic) (%mv-index pic (+ bx ii) (+ by jj))) (aref refs i))))))))
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
                    (%mc-partition ss (%ref-picture ss ref) (+ px sx) (+ py sy) sw shh mvx mvy)))))))
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
                  (%mc-partition ss (%ref-picture ss ref) (+ px sx) (+ py sy) pw ph mvx mvy)))))))
    ;; 4. the residual, on top of what motion compensation predicted
    (let ((cbp (%read-cbp ss nil)))
      (setf (aref (pic-mb-cbp pic) mbi) cbp)
      (if (plusp cbp)
          (progn (incf (ss-qp ss) (%read-qp-delta ss))
                 (setf (ss-qp ss) (mod (+ (ss-qp ss) 52) 52)))
          (%no-qp-delta ss))
      (%inter-luma-residual ss cbp)
      (decode-chroma ss nil cbp)
      (setf (aref (pic-mb-qps pic) mbi) (ss-qp ss))
      cbp)))

(defun decode-slice (pic sh br &optional refs)
  "Decode every macroblock of a slice into PIC.  REFS is the list-0 reference picture vector."
  (let* ((cabac-p (pps-cabac (sh-pps sh)))
         (ss (make-slice-state :pic pic :sh sh :br br :qp (sh-qp sh)
                               :reflist (or refs #())
                               :ref0 (and refs (plusp (length refs)) (aref refs 0))
                               :cabac (when cabac-p
                                        (init-cabac br (sh-qp sh) (sh-i-slice-p sh)
                                                    (sh-cabac-init-idc sh)))))
         (mbw (pic-mb-width pic))
         (total (* mbw (pic-mb-height pic)))
         (mb (sh-first-mb sh))
         (p-slice (sh-p-slice-p sh)))
    (unless (or (sh-i-slice-p sh) p-slice)
      (%err "only I and P slices are decoded so far; this is a ~a slice"
            (slice-type-name (sh-slice-type sh))))
    (flet ((at (n) (setf (ss-mbx ss) (mod n mbw) (ss-mby ss) (floor n mbw)))
           (more-p ()
             ;; CABAC ends a slice with an explicit decision, not by running out of bits: the
             ;; arithmetic decoder has no "am I at the end" to consult, so the encoder says so
             (if cabac-p
                 (zerop (decode-terminate (ss-cabac ss)))
                 (more-rbsp-data-p br))))
      (loop
        (when (>= mb total) (return))
        (cond
          ;; CABAC: every macroblock carries its own skip flag, so there is no run to unroll and
          ;; the end-of-slice decision comes after skipped macroblocks too
          ((and p-slice cabac-p)
           (at mb)
           (if (cabac-mb-skip-flag ss)
               (decode-skip-macroblock ss)
               (let ((mt (cabac-mb-type-p ss)))
                 (if (< mt 5)
                     (decode-p-macroblock ss mt)
                     (decode-intra-macroblock ss (- mt 5)))))
           (incf mb))
          ;; CAVLC: a RUN of skipped macroblocks precedes each coded one, and the run may be the
          ;; last thing in the slice — so the end-of-data test belongs after it
          (p-slice
           (let ((skip (ue br)) (ran-out nil))
             (dotimes (i skip)
               (when (>= mb total) (return))
               (at mb)
               (decode-skip-macroblock ss)
               (incf mb))
             (when (>= mb total) (return))
             (when (and (plusp skip) (not (more-rbsp-data-p br))) (setf ran-out t))
             (unless ran-out
               (at mb)
               (let ((mt (ue br)))
                 ;; in a P slice, mb_type 5 and up is an intra macroblock with 5 subtracted
                 (if (< mt 5)
                     (decode-p-macroblock ss mt)
                     (decode-intra-macroblock ss (- mt 5))))
               (incf mb))
             (when ran-out (return))))
          (t
           (at mb)
           (decode-intra-macroblock ss (if cabac-p (cabac-mb-type-i ss) (ue br)))
           (incf mb)))
        (unless (more-p) (return))))
    ss))

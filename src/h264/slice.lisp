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

(defstruct (picture (:conc-name pic-))
  (width 0 :type fixnum) (height 0 :type fixnum)          ; displayed, after cropping
  (mb-width 0 :type fixnum) (mb-height 0 :type fixnum)
  y u v
  (ystride 0 :type fixnum) (cstride 0 :type fixnum)
  (yoff 0 :type fixnum) (coff 0 :type fixnum)
  ;; per-4x4-block coefficient counts, for nC
  nz-y nz-u nz-v
  ;; per-4x4-block Intra4x4 prediction modes, and per-macroblock facts the loop filter needs
  modes mb-types mb-qps)

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
     :mb-qps (make-array (* mbw mbh) :element-type 'fixnum :initial-element 0))))

(declaim (inline pic-y-base pic-c-base))
(defun pic-y-base (p mbx mby)
  (+ (pic-yoff p) (* (* 16 mby) (pic-ystride p)) (* 16 mbx)))
(defun pic-c-base (p mbx mby)
  (+ (pic-coff p) (* (* 8 mby) (pic-cstride p)) (* 8 mbx)))

;;; ---- the decoding state for one slice -----------------------------------------------------------

(defstruct (slice-state (:conc-name ss-))
  pic sh br
  (mbx 0 :type fixnum) (mby 0 :type fixnum)
  (qp 26 :type fixnum)
  (coeffs (make-array 16 :element-type 'fixnum))       ; scan-order scratch
  (block (make-array 16 :element-type 'fixnum))        ; raster-order scratch
  (luma-dc (make-array 16 :element-type 'fixnum))
  (chroma-dc (make-array 4 :element-type 'fixnum))
  (pred (make-array 16 :element-type '(unsigned-byte 8)))
  (pt (make-array 9 :element-type '(unsigned-byte 8)))
  (pl (make-array 4 :element-type '(unsigned-byte 8))))

(defun mb-available-p (ss dx dy)
  "Is the macroblock at (mbx+DX, mby+DY) decoded and in this slice?

   Raster order and one slice per picture, so this is `is it inside, and is it before us'.  A
   decoder with several slices per picture must also compare slice numbers here, which is why
   this is one function and not an inline test in six places."
  (let* ((x (+ (ss-mbx ss) dx)) (y (+ (ss-mby ss) dy))
         (pic (ss-pic ss)))
    (and (>= x 0) (>= y 0) (< x (pic-mb-width pic)) (< y (pic-mb-height pic))
         (/= -1 (aref (pic-mb-types pic) (+ (* y (pic-mb-width pic)) x))))))

;;; ---- nC: the coefficient-count context ------------------------------------------------------------

(defun %nz-at (grid gw bx by)
  (if (or (minusp bx) (minusp by)) nil (aref grid (+ (* by gw) bx))))

(defun luma-nc (ss blk)
  "nC for luma 4x4 block BLK of the current macroblock (9.2.1)."
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
  (let* ((pic (ss-pic ss)) (gw (* 4 (pic-mb-width pic))))
    (setf (aref (pic-nz-y pic)
                (+ (* (+ (* 4 (ss-mby ss)) (aref +blk-y+ blk)) gw)
                   (+ (* 4 (ss-mbx ss)) (aref +blk-x+ blk))))
          n)))

(defun set-chroma-nz (ss plane blk n)
  (let* ((pic (ss-pic ss)) (gw (* 2 (pic-mb-width pic)))
         (grid (if (zerop plane) (pic-nz-u pic) (pic-nz-v pic))))
    (setf (aref grid (+ (* (+ (* 2 (ss-mby ss)) (ash blk -1)) gw)
                        (+ (* 2 (ss-mbx ss)) (logand blk 1))))
          n)))

;;; ---- Intra4x4 prediction modes (8.3.1.1) ------------------------------------------------------------

(defun neighbour-mb-available-p (ss blk dx dy)
  "Is the MACROBLOCK containing the neighbour DX,DY away from BLK available?"
  (let* ((lx (+ (aref +blk-x+ blk) dx)) (ly (+ (aref +blk-y+ blk) dy))
         (mbdx (cond ((minusp lx) -1) ((> lx 3) 1) (t 0)))
         (mbdy (cond ((minusp ly) -1) ((> ly 3) 1) (t 0))))
    (or (and (zerop mbdx) (zerop mbdy))          ; inside this macroblock
        (mb-available-p ss mbdx mbdy))))

(defun neighbour-mode (ss blk dx dy)
  "The Intra4x4 mode of the block DX,DY away from BLK, or 2 (DC) when the macroblock it lives in
   was not Intra4x4 coded."
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
  (let ((a-ok (neighbour-mb-available-p ss blk -1 0))
        (b-ok (neighbour-mb-available-p ss blk 0 -1)))
    (if (not (and a-ok b-ok))
        2
        (min (neighbour-mode ss blk -1 0) (neighbour-mode ss blk 0 -1)))))

(defun set-mode (ss blk mode)
  (let* ((pic (ss-pic ss)) (gw (* 4 (pic-mb-width pic))))
    (setf (aref (pic-modes pic)
                (+ (* (+ (* 4 (ss-mby ss)) (aref +blk-y+ blk)) gw)
                   (+ (* 4 (ss-mbx ss)) (aref +blk-x+ blk))))
          mode)))

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

(defun decode-i-macroblock (ss)
  "Parse and reconstruct one I-slice macroblock."
  (let* ((br (ss-br ss)) (pic (ss-pic ss))
         (mb-type (ue br))
         (mbi (+ (* (ss-mby ss) (pic-mb-width pic)) (ss-mbx ss))))
    (when (= mb-type 25) (%err "I_PCM macroblocks are not supported"))
    (when (> mb-type 25) (%err "mb_type ~d in an I slice" mb-type))
    (setf (aref (pic-mb-types pic) mbi) mb-type)
    (if (zerop mb-type)
        (decode-i4x4-macroblock ss)
        (decode-i16x16-macroblock ss (1- mb-type)))
    (setf (aref (pic-mb-qps pic) mbi) (ss-qp ss))
    mb-type))

(defun %chroma-pred-mode (br) (ue br))

(defun decode-i4x4-macroblock (ss)
  (let* ((br (ss-br ss)) (pic (ss-pic ss))
         (modes (make-array 16 :element-type 'fixnum)))
    (declare (dynamic-extent modes))
    ;; the sixteen prediction modes, each coded against its neighbours' minimum
    (dotimes (blk 16)
      (let* ((pred (predicted-mode ss blk))
             (flag (u1 br))
             (rem (if (= flag 1) nil (ub br 3)))
             (mode (if (= flag 1) pred (if (< rem pred) rem (1+ rem)))))
        (when (equal *debug-blk* (cons (+ (* (ss-mby ss) (pic-mb-width pic)) (ss-mbx ss)) :modes))
          (format t "~&  blk ~2d: pred=~d flag=~d rem=~a -> mode ~d~%" blk pred flag rem mode))
        (setf (aref modes blk) mode)
        (set-mode ss blk mode)))
    (let* ((chroma-mode (%chroma-pred-mode br))
           (cbp-code (ue br))
           (cbp (if (< cbp-code 48)
                    (aref +intra4x4-cbp+ cbp-code)
                    (%err "coded_block_pattern code ~d" cbp-code))))
      (when (plusp cbp)
        (incf (ss-qp ss) (se br))
        (setf (ss-qp ss) (mod (+ (ss-qp ss) 52) 52)))
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
          (when (equal *debug-blk*
                       (cons (+ (* (ss-mby ss) (pic-mb-width pic)) (ss-mbx ss)) blk))
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
              (let ((n (residual-block br (ss-coeffs ss) (luma-nc ss blk) 16)))
                (when (equal *debug-blk*
                             (cons (+ (* (ss-mby ss) (pic-mb-width pic)) (ss-mbx ss)) blk))
                  (format t "   nc=~d n=~d coeffs(scan)=~a~%" (luma-nc ss blk) n
                          (coerce (ss-coeffs ss) 'list)))
                (set-luma-nz ss blk n)
                (when (plusp n)
                  (dequant-4x4 (ss-coeffs ss) (ss-block ss) (ss-qp ss))
                  (idct-4x4 (ss-block ss))
                  (add-residual-4x4 (pic-y pic) (pic-ystride pic) base (ss-block ss))))
              (set-luma-nz ss blk 0))))
      (decode-chroma ss chroma-mode cbp)
      cbp)))

(defun decode-i16x16-macroblock (ss code)
  "An Intra16x16 macroblock.  CODE is mb_type - 1, which packs three things: the prediction mode,
   whether chroma has DC only or DC and AC, and whether luma has AC at all."
  (let* ((br (ss-br ss)) (pic (ss-pic ss))
         (pred-mode (mod code 4))
         (cbp-chroma (mod (floor code 4) 3))
         (cbp-luma (if (>= code 12) 15 0))
         (base (pic-y-base pic (ss-mbx ss) (ss-mby ss))))
    (let* ((chroma-mode (%chroma-pred-mode br))
           (cbp (logior cbp-luma (ash cbp-chroma 4))))
      ;; an Intra16x16 macroblock always carries a QP delta, because it always has a DC block
      (incf (ss-qp ss) (se br))
      (setf (ss-qp ss) (mod (+ (ss-qp ss) 52) 52))
      (intra16x16-predict (pic-y pic) (pic-ystride pic) base pred-mode
                          (mb-available-p ss 0 -1) (mb-available-p ss -1 0))
      ;; the DC block: sixteen coefficients, one per 4x4, transformed again
      (let ((dc (ss-luma-dc ss)))
        (let ((n (residual-block br (ss-coeffs ss) (luma-nc ss 0) 16)))
          (declare (ignorable n))
          ;; the DC block's own coefficients are in scan order; un-scan them into raster
          (fill dc 0)
          (dotimes (i 16) (setf (aref dc (aref +zigzag-4x4+ i)) (aref (ss-coeffs ss) i))))
        (luma-dc-transform dc (ss-qp ss))
        ;; then every 4x4's AC, with that block's DC substituted in
        (dotimes (blk 16)
          (let ((bbase (+ base (* (aref +blk-y+ blk) 4 (pic-ystride pic))
                          (* (aref +blk-x+ blk) 4)))
                (n 0))
            (if (logbitp (ash blk -2) cbp-luma)
                (progn (setf n (residual-block br (ss-coeffs ss) (luma-nc ss blk) 15 :start 1))
                       (dequant-4x4 (ss-coeffs ss) (ss-block ss) (ss-qp ss) :start 1))
                (fill (ss-block ss) 0))
            (set-luma-nz ss blk n)
            ;; the DC comes from the second-order transform, not from this block's own scan
            (setf (aref (ss-block ss) 0)
                  (aref dc (+ (* (aref +blk-y+ blk) 4) (aref +blk-x+ blk))))
            (idct-4x4 (ss-block ss))
            (add-residual-4x4 (pic-y pic) (pic-ystride pic) bbase (ss-block ss)))))
      (decode-chroma ss chroma-mode cbp)
      cbp)))

(defun decode-chroma (ss mode cbp)
  "Both chroma planes: predict, then the DC blocks, then the AC blocks.

   THE ORDER IS BOTH DCs FIRST, then both planes' AC (7.3.5.3), and it is not the order the
   planes are otherwise processed in.  Doing Cb entirely and then Cr — which is what writing the
   loop the obvious way gives you — reads the Cr DC block out of the middle of Cb's AC data and
   desynchronises the whole slice from that macroblock on."
  (let* ((br (ss-br ss)) (sh (ss-sh ss)) (pps (sh-pps sh)) (pic (ss-pic ss))
         (cbp-chroma (ash cbp -4))
         (base (pic-c-base pic (ss-mbx ss) (ss-mby ss)))
         (up-p (mb-available-p ss 0 -1))
         (left-p (mb-available-p ss -1 0))
         (planes (vector (pic-u pic) (pic-v pic)))
         (qps (vector (chroma-qp (ss-qp ss) (pps-chroma-qp-offset-for pps 0))
                      (chroma-qp (ss-qp ss) (pps-chroma-qp-offset-for pps 1))))
         (dcs (vector (make-array 4 :element-type 'fixnum :initial-element 0)
                      (make-array 4 :element-type 'fixnum :initial-element 0))))
    ;; prediction first, for both planes
    (dotimes (plane 2)
      (chroma-predict (aref planes plane) (pic-cstride pic) base mode up-p left-p))
    ;; then both DC blocks, in plane order
    (when (plusp cbp-chroma)
      (dotimes (plane 2)
        (let ((dc (aref dcs plane)))
          (residual-block br (ss-coeffs ss) -1 4)
          (dotimes (i 4) (setf (aref dc i) (aref (ss-coeffs ss) i)))
          (chroma-dc-transform dc (aref qps plane)))))
    ;; then the AC blocks, all of Cb's before any of Cr's
    (dotimes (plane 2)
      (let ((data (aref planes plane)) (qpc (aref qps plane)) (dc (aref dcs plane)))
        (dotimes (blk 4)
          (let ((bbase (+ base (* (ash blk -1) 4 (pic-cstride pic)) (* (logand blk 1) 4)))
                (n 0))
            (if (= cbp-chroma 2)
                (progn (setf n (residual-block br (ss-coeffs ss) (chroma-nc ss plane blk) 15 :start 1))
                       (dequant-4x4 (ss-coeffs ss) (ss-block ss) qpc :start 1))
                (fill (ss-block ss) 0))
            (set-chroma-nz ss plane blk n)
            (setf (aref (ss-block ss) 0) (aref dc blk))
            (idct-4x4 (ss-block ss))
            (add-residual-4x4 data (pic-cstride pic) bbase (ss-block ss))))))))

;;; ---- the slice ------------------------------------------------------------------------------------

(defun decode-slice (pic sh br)
  "Decode every macroblock of a slice into PIC."
  (let ((ss (make-slice-state :pic pic :sh sh :br br :qp (sh-qp sh))))
    (unless (sh-i-slice-p sh)
      (%err "only I slices are decoded so far; this is a ~a slice" (slice-type-name (sh-slice-type sh))))
    (let ((mb (sh-first-mb sh)) (total (* (pic-mb-width pic) (pic-mb-height pic))))
      (loop while (< mb total) do
        (setf (ss-mbx ss) (mod mb (pic-mb-width pic))
              (ss-mby ss) (floor mb (pic-mb-width pic)))
        (decode-i-macroblock ss)
        (incf mb)
        ;; the slice ends at its stop bit, not at a macroblock count it declares
        (unless (more-rbsp-data-p br) (return))))
    ss))

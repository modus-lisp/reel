;;;; h264/cabac.lisp — the arithmetic decoder, and the syntax read through it.
;;;;
;;;; CAVLC codes a symbol by looking it up in a table.  CABAC codes one BINARY DECISION at a time
;;;; against a probability that it keeps adjusting as it goes, so a syntax element is first turned
;;;; into a string of bins (its binarization) and each bin is decoded against a context chosen from
;;;; what the neighbouring macroblocks did.  That is where the compression comes from and also
;;;; where the difficulty is: the arithmetic decoder itself is forty lines, and everything else in
;;;; this file is the bookkeeping of WHICH context each bin belongs to.
;;;;
;;;; THE FAILURE MODE IS TOTAL.  A wrong context number does not corrupt one block the way a wrong
;;;; intra mode does; it feeds the wrong probability to the arithmetic decoder, which then returns
;;;; the wrong bin, and every symbol after it in the slice is garbage.  There is no graceful
;;;; degradation and no partial credit, which is worth knowing before debugging one: the first
;;;; wrong sample in the picture is nowhere near the first wrong context.
;;;;
;;;; The constants are in cabac-tables.lisp, generated and invariant-checked.

(in-package #:reel.h264)

;;; ---- context indices (9.3.3.1) ------------------------------------------------------------------

(defconstant +ctx-mb-type-i+ 3)
(defconstant +ctx-mb-skip-p+ 11)
(defconstant +ctx-mb-type-p-prefix+ 14)
(defconstant +ctx-mb-type-p-suffix+ 17)
(defconstant +ctx-sub-mb-type-p+ 21)
(defconstant +ctx-mb-type-b+ 27)
(defconstant +ctx-sub-mb-type-b+ 36)
(defconstant +ctx-mb-skip-b+ 24)
(defconstant +ctx-mvd-x+ 40)
(defconstant +ctx-mvd-y+ 47)
(defconstant +ctx-ref-idx+ 54)
(defconstant +ctx-mb-qp-delta+ 60)
(defconstant +ctx-chroma-pred+ 64)
(defconstant +ctx-prev-intra4x4+ 68)
(defconstant +ctx-rem-intra4x4+ 69)
(defconstant +ctx-cbp-luma+ 73)
(defconstant +ctx-cbp-chroma+ 77)
(defconstant +ctx-coded-block-flag+ 85)
(defconstant +ctx-significant+ 105)
(defconstant +ctx-last-significant+ 166)
(defconstant +ctx-abs-level+ 227)
(defconstant +ctx-terminate+ 276
  "The one context that never adapts: it decides end-of-slice, and I_PCM.")

;;; ---- the arithmetic decoder (9.3.3.2) -------------------------------------------------------------

(defstruct (cabac (:conc-name cb-) (:constructor %make-cabac))
  (br nil)
  (range 510 :type fixnum)
  (offset 0 :type fixnum)
  ;; one byte per context: pStateIdx in bits 1..6, valMPS in bit 0 — the packing costs nothing and
  ;; keeps the two halves of a context together, which is how they are always used
  (state (make-array 1024 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (last-qp-delta 0 :type fixnum))

(defun init-cabac (br qp slice-type-i-p cabac-init-idc)
  "Start the arithmetic decoder at the current (byte-aligned) bit position, 9.3.1.

   The initial state of every context is a function of the slice quantiser: the encoder and decoder
   must agree on the starting probabilities, and QP is the one thing that predicts them."
  (declare (type fixnum qp cabac-init-idc) (optimize (speed 3) (safety 1)))
  (byte-align br)
  (let ((c (%make-cabac :br br)))
    (let ((st (cb-state c))
          (q (max 0 (min 51 qp))))
      (declare (type fixnum q))
      (dotimes (i 1024)
        (let* ((m (if slice-type-i-p
                      (aref +cabac-init-i+ i 0)
                      (aref +cabac-init-pb+ cabac-init-idc i 0)))
               (n (if slice-type-i-p
                      (aref +cabac-init-i+ i 1)
                      (aref +cabac-init-pb+ cabac-init-idc i 1)))
               (pre (max 1 (min 126 (+ (ash (* m q) -4) n)))))
          (declare (type fixnum m n pre))
          (setf (aref st i)
                (if (<= pre 63)
                    (ash (- 63 pre) 1)              ; valMPS 0
                    (logior (ash (- pre 64) 1) 1))))))   ; valMPS 1
    (setf (cb-range c) 510
          (cb-offset c) (ub br 9))
    c))

(declaim (inline %renorm %read-bit))
(defun %read-bit (br)
  (declare (optimize (speed 3) (safety 0)))
  ;; past the end reads as zero: a slice's last bins can legitimately need bits the RBSP does not
  ;; physically contain, because the encoder stopped once the decoder could not be in doubt
  (if (>= (br-bit br) (br-end br)) 0 (u1 br)))

(defun %renorm (c)
  (declare (type cabac c) (optimize (speed 3) (safety 0)))
  (let ((r (cb-range c)) (o (cb-offset c)) (br (cb-br c)))
    (declare (type fixnum r o))
    (loop while (< r 256)
          do (setf r (ash r 1)
                   o (logior (ash o 1) (%read-bit br))))
    (setf (cb-range c) r (cb-offset c) o)))

(defun decode-decision (c ctx)
  "One context-coded bin (9.3.3.2.1)."
  (declare (type cabac c) (type fixnum ctx) (optimize (speed 3) (safety 1)))
  (let* ((st (cb-state c))
         (s (aref st ctx))
         (pstate (ash s -1))
         (mps (logand s 1))
         (r (cb-range c))
         (lps (aref +cabac-range-lps+ pstate (logand (ash r -6) 3)))
         (bit 0))
    (declare (type fixnum s pstate mps r lps bit))
    (decf r lps)
    (cond
      ((>= (cb-offset c) r)
       ;; the less probable symbol: take the small sub-range, and if we were already as unsure as
       ;; the model can be, swap which symbol is the probable one
       (setf bit (- 1 mps))
       (decf (cb-offset c) r)
       (setf r lps)
       (when (zerop pstate) (setf mps (- 1 mps)))
       (setf (aref st ctx) (logior (ash (aref +cabac-trans-lps+ pstate) 1) mps)))
      (t
       (setf bit mps)
       (setf (aref st ctx) (logior (ash (aref +cabac-trans-mps+ pstate) 1) mps))))
    (setf (cb-range c) r)
    (%renorm c)
    bit))

(defun decode-bypass (c)
  "A bin with no context at all (9.3.3.2.3): an even chance, so the range never has to adapt."
  (declare (type cabac c) (optimize (speed 3) (safety 1)))
  (let ((o (logior (ash (cb-offset c) 1) (%read-bit (cb-br c)))))
    (declare (type fixnum o))
    (cond ((>= o (cb-range c)) (setf (cb-offset c) (- o (cb-range c))) 1)
          (t (setf (cb-offset c) o) 0))))

(defun decode-terminate (c)
  "The end-of-slice decision (9.3.3.2.4), which is also how I_PCM is signalled."
  (declare (type cabac c) (optimize (speed 3) (safety 1)))
  (let ((r (- (cb-range c) 2)))
    (declare (type fixnum r))
    (cond ((>= (cb-offset c) r) 1)
          (t (setf (cb-range c) r) (%renorm c) 0))))

;;; ---- binarizations that several syntax elements share --------------------------------------------

(defun %unary (c ctx-fn &optional (limit 32))
  "A run of ones ended by a zero; CTX-FN gives the context for each bin index."
  (declare (type function ctx-fn) (optimize (speed 3) (safety 1)))
  (let ((n 0))
    (declare (type fixnum n))
    (loop
      (when (>= n limit) (return n))
      (if (zerop (decode-decision c (funcall ctx-fn n)))
          (return n)
          (incf n)))))

(defun %bypass-bits (c n)
  (declare (type fixnum n) (optimize (speed 3) (safety 1)))
  (let ((v 0))
    (declare (type fixnum v))
    (dotimes (i n v) (setf v (logior (ash v 1) (decode-bypass c))))))

(defun %exp-golomb-bypass (c k)
  "Exp-Golomb order K, read entirely in bypass — the tail of a UEGk binarization."
  (declare (type fixnum k) (optimize (speed 3) (safety 1)))
  (let ((v 0) (shift k))
    (declare (type fixnum v shift))
    (loop while (= 1 (decode-bypass c))
          do (incf v (ash 1 shift))
             (incf shift)
             (when (> shift 30) (%err "runaway Exp-Golomb suffix in CABAC")))
    (loop while (plusp shift)
          do (decf shift)
             (incf v (ash (decode-bypass c) shift)))
    v))

;;; ---- the neighbours a context is chosen from -----------------------------------------------------
;;;
;;; Nearly every context index is "what did the macroblock to my left and the one above me do".
;;; These helpers answer that; the rules for what an ABSENT neighbour contributes differ per syntax
;;; element and are spelled out at each use, because they are not the same rule and assuming they
;;; are is how a decoder ends up working on the middle of a picture and not its edges.

(declaim (inline %nbr-mbi))
(defun %nbr-mbi (ss dx dy)
  "Index of the neighbouring macroblock, or NIL when it is outside or not yet decoded."
  (let* ((pic (ss-pic ss)) (x (+ (ss-mbx ss) dx)) (y (+ (ss-mby ss) dy)))
    (and (>= x 0) (>= y 0) (< x (pic-mb-width pic)) (< y (pic-mb-height pic))
         (let ((i (+ (* y (pic-mb-width pic)) x)))
           (and (/= -1 (aref (pic-mb-types pic) i)) i)))))

(defun %i16x16-p (pic mbi)
  (let ((tp (aref (pic-mb-types pic) mbi))) (and (>= tp 1) (<= tp 24))))

(defun %mb-type-i-ctx-inc (ss)
  "ctxIdxInc for bin 0 of mb_type in an I slice: how many neighbours were NOT I_NxN (9.3.3.1.1.3)."
  (let ((pic (ss-pic ss)) (n 0))
    (dolist (d '((-1 0) (0 -1)) n)
      (let ((i (%nbr-mbi ss (first d) (second d))))
        (when (and i (/= 0 (aref (pic-mb-types pic) i))) (incf n))))))

(defun %chroma-pred-ctx-inc (ss)
  "ctxIdxInc for bin 0 of intra_chroma_pred_mode (9.3.3.1.1.8): a neighbour counts only if it is
   intra AND chose a mode other than DC."
  (let ((pic (ss-pic ss)) (n 0))
    (dolist (d '((-1 0) (0 -1)) n)
      (let ((i (%nbr-mbi ss (first d) (second d))))
        (when (and i (>= (aref (pic-mb-types pic) i) 0)
                   (/= 0 (aref (pic-mb-chroma-mode pic) i)))
          (incf n))))))

;;; ---- macroblock-level syntax ---------------------------------------------------------------------

(defun cabac-mb-type-i (ss)
  "mb_type in an I slice (Table 9-36).  0 is I_NxN, 1..24 the Intra16x16 variants, 25 I_PCM."
  (let ((c (ss-cabac ss)))
    (if (zerop (decode-decision c (+ +ctx-mb-type-i+ (%mb-type-i-ctx-inc ss))))
        0
        ;; the second bin is the TERMINATE context, not an adapting one: I_PCM is signalled the
        ;; same way the end of a slice is
        (if (= 1 (decode-terminate c))
            25
            (let* ((cbp-luma (decode-decision c (+ +ctx-mb-type-i+ 3)))
                   (chroma (if (zerop (decode-decision c (+ +ctx-mb-type-i+ 4)))
                               0
                               (if (zerop (decode-decision c (+ +ctx-mb-type-i+ 5))) 1 2)))
                   ;; Table 9-39 shifts the bin INDEX when the chroma part takes a second bin, but
                   ;; the two prediction-mode bins land on increments 6 and 7 either way
                   (p1 (decode-decision c (+ +ctx-mb-type-i+ 6)))
                   (p0 (decode-decision c (+ +ctx-mb-type-i+ 7))))
              (+ 1 (* 12 cbp-luma) (* 4 chroma) (* 2 p1) p0))))))

(defun cabac-intra-chroma-mode (ss)
  "intra_chroma_pred_mode: truncated unary, cMax 3."
  (let ((c (ss-cabac ss)))
    (if (zerop (decode-decision c (+ +ctx-chroma-pred+ (%chroma-pred-ctx-inc ss))))
        0
        (if (zerop (decode-decision c (+ +ctx-chroma-pred+ 3)))
            1
            (if (zerop (decode-decision c (+ +ctx-chroma-pred+ 3))) 2 3)))))

(defun cabac-intra4x4-mode (ss blk)
  "prev_intra4x4_pred_mode_flag and rem_intra4x4_pred_mode, one context each."
  (let ((c (ss-cabac ss)) (pred (predicted-mode ss blk)))
    (if (= 1 (decode-decision c +ctx-prev-intra4x4+))
        pred
        (let ((rem (logior (decode-decision c +ctx-rem-intra4x4+)
                           (ash (decode-decision c +ctx-rem-intra4x4+) 1)
                           (ash (decode-decision c +ctx-rem-intra4x4+) 2))))
          (if (< rem pred) rem (1+ rem))))))

(defun %cbp-luma-ctx-inc (ss blk8 sofar)
  "ctxIdxInc for luma 8x8 block BLK8 of coded_block_pattern (9.3.3.1.1.4).

   A neighbour contributes 1 when its 8x8 block has NO coefficients, which reads backwards until
   you notice the element being coded is `there is something here' and the context is `were my
   neighbours empty'.  SOFAR is the pattern decoded so far in THIS macroblock, because two of the
   four 8x8 blocks have their neighbours inside it."
  (let* ((pic (ss-pic ss))
         (x (mod blk8 2)) (y (floor blk8 2)))
    (flet ((bit-of (dx dy bidx)
             ;; 1 when that 8x8 block coded nothing, 0 when it coded something or is absent
             (if (and (zerop dx) (zerop dy))
                 (if (logbitp bidx sofar) 0 1)
                 (let ((i (%nbr-mbi ss dx dy)))
                   (if (null i)
                       0
                       (if (logbitp bidx (aref (pic-mb-cbp pic) i)) 0 1))))))
      (let ((a (if (plusp x) (bit-of 0 0 (+ blk8 -1)) (bit-of -1 0 (+ blk8 1))))
            (b (if (plusp y) (bit-of 0 0 (- blk8 2)) (bit-of 0 -1 (+ blk8 2)))))
        (+ a (* 2 b))))))

(defun %cbp-chroma-ctx-inc (ss which)
  "ctxIdxInc for the two chroma bins of coded_block_pattern.  WHICH is 0 for `is there any chroma
   residual' and 1 for `is it the AC one too'."
  (let ((pic (ss-pic ss)) (a 0) (b 0))
    (flet ((val (dx dy)
             (let ((i (%nbr-mbi ss dx dy)))
               (if (null i)
                   0
                   (let ((ch (ash (aref (pic-mb-cbp pic) i) -4)))
                     (if (zerop which) (if (plusp ch) 1 0) (if (= ch 2) 1 0)))))))
      (setf a (val -1 0) b (val 0 -1)))
    (+ (if (zerop which) 0 4) a (* 2 b))))

(defun cabac-cbp (ss)
  "coded_block_pattern: four luma bins then two chroma ones."
  (let ((c (ss-cabac ss)) (luma 0))
    (dotimes (i 4)
      (when (= 1 (decode-decision c (+ +ctx-cbp-luma+ (%cbp-luma-ctx-inc ss i luma))))
        (setf luma (logior luma (ash 1 i)))))
    (let ((chroma 0))
      (when (= 1 (decode-decision c (+ +ctx-cbp-chroma+ (%cbp-chroma-ctx-inc ss 0))))
        (setf chroma (if (= 1 (decode-decision c (+ +ctx-cbp-chroma+ (%cbp-chroma-ctx-inc ss 1))))
                         2 1)))
      (logior luma (ash chroma 4)))))

(defconstant +ctx-transform-8x8+ 399)

(defun cabac-transform-size-8x8 (ss)
  "transform_size_8x8_flag: one bin, its context counting how many neighbours also chose 8x8."
  (let ((c (ss-cabac ss)))
    (flet ((term (dx dy)
             (let ((i (%nbr-mbi ss dx dy)))
               (if (and i (plusp (aref (pic-mb-tf8 (ss-pic ss)) i))) 1 0))))
      (decode-decision c (+ +ctx-transform-8x8+ (term -1 0) (term 0 -1))))))

(defun cabac-mb-qp-delta (ss)
  "mb_qp_delta: unary, then the signed mapping of 9.3.2.7."
  (let* ((c (ss-cabac ss))
         (prev (cb-last-qp-delta c))
         (k (let ((n 0))
              (if (zerop (decode-decision c (+ +ctx-mb-qp-delta+ (if (zerop prev) 0 1))))
                  0
                  (progn
                    (setf n 1)
                    (if (zerop (decode-decision c (+ +ctx-mb-qp-delta+ 2)))
                        n
                        (progn
                          (incf n)
                          (loop while (= 1 (decode-decision c (+ +ctx-mb-qp-delta+ 3)))
                                do (incf n)
                                   (when (> n 88) (%err "runaway mb_qp_delta")))
                          n)))))))
    (let ((v (if (oddp k) (ceiling k 2) (- (ceiling k 2)))))
      (setf (cb-last-qp-delta c) v)
      v)))

;;; ---- residual blocks -----------------------------------------------------------------------------

(defun %cbf-ctx-inc-dc (ss cat plane)
  "ctxIdxInc for coded_block_flag of a DC block, which lives once per macroblock (9.3.3.1.1.9).

   An ABSENT neighbour contributes 1 when the current macroblock is intra and 0 when it is inter.
   That asymmetry is deliberate and is not shared by the other syntax elements: intra content at a
   picture edge is assumed busy, inter content is assumed empty."
  (let* ((pic (ss-pic ss))
         (intra-p (>= (aref (pic-mb-types pic) (+ (* (ss-mby ss) (pic-mb-width pic)) (ss-mbx ss))) 0)))
    (flet ((term (dx dy)
             (let ((i (%nbr-mbi ss dx dy)))
               (cond
                 ((null i) (if intra-p 1 0))
                 ((= cat +cat-luma-dc+)
                  ;; only an Intra16x16 macroblock HAS a luma DC block; from anything else there is
                  ;; no transform block to ask about, which is a 0 rather than a missing neighbour
                  (if (and (%i16x16-p pic i) (logbitp 0 (aref (pic-mb-dc-cbf pic) i))) 1 0))
                 (t (if (logbitp (1+ plane) (aref (pic-mb-dc-cbf pic) i)) 1 0))))))
      (+ (term -1 0) (* 2 (term 0 -1))))))

(defun %cbf-ctx-inc-4x4 (ss cat bx by plane)
  "ctxIdxInc for coded_block_flag of an ordinary 4x4 block at picture block coordinates BX,BY."
  (let* ((pic (ss-pic ss))
         (intra-p (>= (aref (pic-mb-types pic) (+ (* (ss-mby ss) (pic-mb-width pic)) (ss-mbx ss))) 0))
         (lumap (or (= cat +cat-luma+) (= cat +cat-luma-ac+)))
         (per-mb (if lumap 4 2))
         (gw (* per-mb (pic-mb-width pic)))
         (grid (cond (lumap (pic-nz-y pic)) ((zerop plane) (pic-nz-u pic)) (t (pic-nz-v pic)))))
    (flet ((term (nbx nby)
             (if (or (minusp nbx) (minusp nby))
                 (if intra-p 1 0)
                 (let* ((dx (- (floor nbx per-mb) (ss-mbx ss)))
                        (dy (- (floor nby per-mb) (ss-mby ss))))
                   (if (and (zerop dx) (zerop dy))
                       (if (plusp (aref grid (+ (* nby gw) nbx))) 1 0)
                       (let ((i (%nbr-mbi ss dx dy)))
                         (if (null i)
                             (if intra-p 1 0)
                             (if (plusp (aref grid (+ (* nby gw) nbx))) 1 0))))))))
      (+ (term (1- bx) by) (* 2 (term bx (1- by)))))))

(declaim (inline %sig-ctx-inc %last-ctx-inc))
(defun %sig-ctx-inc (cat i)
  "ctxIdxInc for significant_coeff_flag: the scan position itself, except for chroma DC where
   there are only four positions and they share three contexts, and for the 8x8 block where
   sixty-three positions share fifteen contexts by a map."
  (declare (type fixnum cat i))
  (cond ((= cat +cat-chroma-dc+) (min i 2))
        ((= cat +cat-luma-8x8+) (aref +sig-map-8x8+ i))
        (t i)))

(defun %last-ctx-inc (cat i)
  "ctxIdxInc for last_significant_coeff_flag.  The same as significance for every category but
   the 8x8 one, which has its own coarser map — five contexts rather than fifteen."
  (declare (type fixnum cat i))
  (cond ((= cat +cat-chroma-dc+) (min i 2))
        ((= cat +cat-luma-8x8+) (aref +last-map-8x8+ i))
        (t i)))

(defun cabac-residual-block (ss cat coeffs bx by plane &key (start 0))
  "One residual block through the arithmetic decoder (9.3.2.3).  Returns (values count highest),
   the same two values RESIDUAL-BLOCK returns for CAVLC, so the reconstruction path is shared.

   The shape is quite unlike CAVLC's.  First a flag saying whether the block has anything at all,
   then a SIGNIFICANCE MAP — one bin per position saying `is there a coefficient here', each with
   its own context, and a second bin after each one saying `and is it the last'.  Only then are the
   magnitudes read, BACKWARDS from the highest frequency, with the context for each chosen from how
   many ones and how many larger values have already come out of this block."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((c (ss-cabac ss))
         (maxc (aref +cat-max-coeff+ cat))
         (big-p (= cat +cat-luma-8x8+))
         (dc-p (or (= cat +cat-luma-dc+) (= cat +cat-chroma-dc+))))
    (declare (type fixnum maxc))
    (fill coeffs 0)
    ;; The 8x8 luma block carries NO coded_block_flag.  Its coded_block_pattern bit already said
    ;; the block has coefficients, and 7.3.5.3.3 omits the flag rather than sending it twice; a
    ;; decoder that reads one anyway is a bin ahead of the encoder from the first coded block.
    (unless big-p
      (let ((cbf-ctx (+ +ctx-coded-block-flag+ (aref +cat-cbf-offset+ cat)
                        (if dc-p
                            (%cbf-ctx-inc-dc ss cat plane)
                            (%cbf-ctx-inc-4x4 ss cat bx by plane)))))
        (declare (type fixnum cbf-ctx))
        (when (zerop (decode-decision c cbf-ctx))
          (return-from cabac-residual-block (values 0 -1)))))
    (let ((sig (make-array 64 :element-type 'bit :initial-element 0))
          (numcoeff maxc)
          (sig-base (if big-p +ctx-significant-8x8+
                        (+ +ctx-significant+ (aref +cat-sig-offset+ cat))))
          (last-base (if big-p +ctx-last-significant-8x8+
                         (+ +ctx-last-significant+ (aref +cat-sig-offset+ cat)))))
      (declare (dynamic-extent sig) (type fixnum numcoeff sig-base last-base))
      (let ((i 0))
        (declare (type fixnum i))
        (loop while (< i (1- numcoeff))
              do (when (= 1 (decode-decision c (+ sig-base (%sig-ctx-inc cat i))))
                   (setf (aref sig i) 1)
                   (when (= 1 (decode-decision c (+ last-base (%last-ctx-inc cat i))))
                     (setf numcoeff (1+ i))))
                 (incf i)))
      ;; whatever position we stopped at is significant: either LAST said so, or it is the only
      ;; place left for the coefficient the block flag promised
      (setf (aref sig (1- numcoeff)) 1)
      (let ((eq1 0) (gt1 0) (n 0) (hi -1)
            (abs-base (+ +ctx-abs-level+ (aref +cat-abs-offset+ cat))))
        (declare (type fixnum eq1 gt1 n hi abs-base))
        (loop for i of-type fixnum from (1- numcoeff) downto 0
              do (when (= 1 (aref sig i))
                   (let* ((ctx0 (+ abs-base (if (plusp gt1) 0 (min 4 (1+ eq1)))))
                          (ctxn (+ abs-base 5
                                   (min (- 4 (if (= cat +cat-chroma-dc+) 1 0)) gt1)))
                          (k 0))
                     (declare (type fixnum ctx0 ctxn k))
                     ;; truncated unary to 14, then an Exp-Golomb tail in bypass if it saturates
                     (loop while (< k 14)
                           do (if (zerop (decode-decision c (if (zerop k) ctx0 ctxn)))
                                  (return)
                                  (incf k)))
                     (let ((mag (1+ k)))
                       (declare (type fixnum mag))
                       (when (= k 14) (incf mag (%exp-golomb-bypass c 0)))
                       (if (= mag 1) (incf eq1) (incf gt1))
                       (when (= 1 (decode-bypass c)) (setf mag (- mag)))
                       (let ((pos (+ start i)))
                         (setf (aref coeffs pos) mag)
                         (when (> pos hi) (setf hi pos))
                         (incf n))))))
        (values n hi)))))

;;; ---- P slices ------------------------------------------------------------------------------------

(defun cabac-mb-skip-flag (ss &optional b-slice)
  "mb_skip_flag (9.3.3.1.1.1).  CABAC has no skip RUN: every macroblock carries its own flag, and
   the context is how many of its neighbours were themselves NOT skipped.  P and B slices count
   from different context bases."
  (let ((pic (ss-pic ss)) (inc 0) (base (if b-slice +ctx-mb-skip-b+ +ctx-mb-skip-p+)))
    (dolist (d '((-1 0) (0 -1)))
      (let ((i (%nbr-mbi ss (first d) (second d))))
        (when (and i (/= -2 (aref (pic-mb-types pic) i))) (incf inc))))
    (= 1 (decode-decision (ss-cabac ss) (+ base inc)))))

(defun cabac-intra-suffix (ss base)
  "The intra mb_type tree as it appears INSIDE a P or B slice, on whichever contexts that slice
   uses for it.  Same shape as the I-slice tree; only the base differs."
  (let ((c (ss-cabac ss)))
    (if (zerop (decode-decision c (+ base 0)))
        0
        (if (= 1 (decode-terminate c))
            25
            (let* ((cbp-luma (decode-decision c (+ base 1)))
                   (chroma (if (zerop (decode-decision c (+ base 2)))
                               0
                               (if (zerop (decode-decision c (+ base 2))) 1 2)))
                   (p1 (decode-decision c (+ base 3)))
                   (p0 (decode-decision c (+ base 3))))
              (+ 1 (* 12 cbp-luma) (* 4 chroma) (* 2 p1) p0))))))

(defun cabac-mb-type-p (ss)
  "mb_type in a P slice.  Returns an inter type 0..3, or (+ 5 intra-type) so the caller can use the
   same `5 and up is intra' convention CAVLC uses.

   P_8x8ref0 has no CABAC binarization at all — it exists only in the CAVLC table — so the values
   here stop at 3."
  (let ((c (ss-cabac ss)))
    (if (= 1 (decode-decision c (+ +ctx-mb-type-p-prefix+ 0)))
        ;; the intra suffix, which is the I-slice tree on its own contexts (Table 9-39)
        (+ 5 (cabac-intra-suffix ss +ctx-mb-type-p-suffix+))
        (let* ((b1 (decode-decision c (+ +ctx-mb-type-p-prefix+ 1)))
               (b2 (decode-decision c (+ +ctx-mb-type-p-prefix+ (if (= b1 1) 3 2)))))
          ;; 000 -> 16x16, 001 -> 8x8, 010 -> 8x16, 011 -> 16x8
          (if (zerop b1)
              (if (zerop b2) 0 3)
              (if (zerop b2) 2 1))))))

(defun cabac-sub-mb-type-p (ss)
  "sub_mb_type in a P slice: 1 -> 8x8, 00 -> 8x4, 011 -> 4x8, 010 -> 4x4."
  (let ((c (ss-cabac ss)))
    (if (= 1 (decode-decision c (+ +ctx-sub-mb-type-p+ 0)))
        0
        (if (zerop (decode-decision c (+ +ctx-sub-mb-type-p+ 1)))
            1
            (if (= 1 (decode-decision c (+ +ctx-sub-mb-type-p+ 2))) 2 3)))))

(defun %ref-idx-ctx-inc (ss bx by &optional (lx 0) b-slice)
  "ctxIdxInc for bin 0 of ref_idx (9.3.3.1.1.6): a neighbour counts when it used a reference other
   than the first one."
  (let ((pic (ss-pic ss)))
    (flet ((term (nbx nby)
             (let* ((mbw (pic-mb-width pic)))
               (if (or (minusp nbx) (minusp nby) (>= nbx (* 4 mbw)))
                   0
                   (let* ((dx (- (floor nbx 4) (ss-mbx ss))) (dy (- (floor nby 4) (ss-mby ss)))
                          (i (if (and (zerop dx) (zerop dy))
                                 (+ (* (ss-mby ss) mbw) (ss-mbx ss))
                                 (%nbr-mbi ss dx dy))))
                     (cond
                       ((null i) 0)
                       ;; 9.3.3.1.1.6 excludes an intra neighbour, a skipped one, and — in a B
                       ;; slice — any partition predicted in DIRECT mode.  All three exclusions are
                       ;; load-bearing rather than tidiness: a direct partition has a reference
                       ;; index it was never told, only inferred, and counting it desynchronises.
                       ;;
                       ;; It takes three things at once to notice: a CODED direct macroblock rather
                       ;; than a skipped one, sitting next to a macroblock that codes a reference
                       ;; index, in a stream with more than one reference so that the inferred index
                       ;; can exceed zero.  With a single reference it can never bite.
                       ((>= (aref (pic-mb-types pic) i) 0) 0)
                       ((= -2 (aref (pic-mb-types pic) i)) 0)
                       ((and b-slice
                             (plusp (aref (pic-blk-direct pic) (%mv-index pic nbx nby))))
                        0)
                       (t (multiple-value-bind (mx my r) (blk-mv pic nbx nby lx)
                            (declare (ignore mx my))
                            (if (> r 0) 1 0)))))))))
      (+ (term (1- bx) by) (* 2 (term bx (1- by)))))))

(defun cabac-ref-idx (ss bx by &optional (lx 0))
  "ref_idx for list LX: unary, with the first three bins on their own contexts."
  (let ((c (ss-cabac ss)))
    (if (zerop (decode-decision c (+ +ctx-ref-idx+
                                    (%ref-idx-ctx-inc ss bx by lx (sh-b-slice-p (ss-sh ss))))))
        0
        (if (zerop (decode-decision c (+ +ctx-ref-idx+ 4)))
            1
            (let ((n 2))
              (loop while (= 1 (decode-decision c (+ +ctx-ref-idx+ 5)))
                    do (incf n)
                       (when (> n 32) (%err "runaway ref_idx")))
              n)))))

(defun %mvd-ctx-inc (ss bx by comp &optional (lx 0))
  "ctxIdxInc for bin 0 of a vector difference (9.3.3.1.1.7).

   The context is the SIZE of the neighbouring differences, not their direction: where the
   neighbours barely moved relative to their own predictions, this one probably will not either."
  (let* ((pic (ss-pic ss)) (sum 0))
    (flet ((term (nbx nby)
             (let ((mbw (pic-mb-width pic)))
               (unless (or (minusp nbx) (minusp nby) (>= nbx (* 4 mbw)))
                 (let* ((dx (- (floor nbx 4) (ss-mbx ss))) (dy (- (floor nby 4) (ss-mby ss)))
                        (i (if (and (zerop dx) (zerop dy))
                               (+ (* (ss-mby ss) mbw) (ss-mbx ss))
                               (%nbr-mbi ss dx dy))))
                   (when (and i (< (aref (pic-mb-types pic) i) -2))
                     (multiple-value-bind (dxv dyv) (blk-mvd pic nbx nby lx)
                       (incf sum (abs (if (zerop comp) dxv dyv))))))))))
      (term (1- bx) by)
      (term bx (1- by)))
    (cond ((< sum 3) 0) ((> sum 32) 2) (t 1))))

(defun cabac-mvd (ss bx by comp &optional (lx 0))
  "One component of a motion vector difference: UEG3, signed, with the unary part context coded
   and everything past nine in bypass."
  (let* ((c (ss-cabac ss))
         (base (if (zerop comp) +ctx-mvd-x+ +ctx-mvd-y+))
         (n 0))
    (declare (type fixnum n))
    (loop while (< n 9)
          do (let ((ctx (+ base (case n
                                  (0 (%mvd-ctx-inc ss bx by comp lx))
                                  (1 3) (2 4) (3 5) (t 6)))))
               (if (zerop (decode-decision c ctx)) (return) (incf n))))
    (let ((v n))
      (declare (type fixnum v))
      (when (= n 9) (incf v (%exp-golomb-bypass c 3)))
      (if (zerop v) 0 (if (= 1 (decode-bypass c)) (- v) v)))))

(defun cabac-sub-mb-type-b (ss)
  "sub_mb_type in a B slice, Table 9-38's B column.

   The tree is lopsided on purpose: B_Direct_8x8 is one bin because it is by far the commonest, and
   the four-way splits cost six."
  (let ((c (ss-cabac ss)))
    (if (zerop (decode-decision c (+ +ctx-sub-mb-type-b+ 0)))
        0                                                       ; B_Direct_8x8
        (if (zerop (decode-decision c (+ +ctx-sub-mb-type-b+ 1)))
            (+ 1 (decode-decision c (+ +ctx-sub-mb-type-b+ 3))) ; B_L0_8x8 / B_L1_8x8
            (if (zerop (decode-decision c (+ +ctx-sub-mb-type-b+ 2)))
                (+ 3 (logior (ash (decode-decision c (+ +ctx-sub-mb-type-b+ 3)) 1)
                             (decode-decision c (+ +ctx-sub-mb-type-b+ 3))))
                (if (zerop (decode-decision c (+ +ctx-sub-mb-type-b+ 3)))
                    (+ 7 (logior (ash (decode-decision c (+ +ctx-sub-mb-type-b+ 3)) 1)
                                 (decode-decision c (+ +ctx-sub-mb-type-b+ 3))))
                    (+ 11 (decode-decision c (+ +ctx-sub-mb-type-b+ 3)))))))))

(defconstant +ctx-mb-type-b-suffix+ 32)

(defun %mb-type-b-ctx-inc (ss)
  "ctxIdxInc for bin 0 of a B slice's mb_type: how many neighbours did something other than take
   the direct prediction whole.  A skipped macroblock and a B_Direct_16x16 both count as nothing
   happening, which is what makes a still region cheap."
  (let ((pic (ss-pic ss)) (n 0))
    (dolist (d '((-1 0) (0 -1)) n)
      (let ((i (%nbr-mbi ss (first d) (second d))))
        (when i
          (let ((tp (aref (pic-mb-types pic) i)))
            ;; -2 is B_Skip and -3 is B_Direct_16x16; anything else is a real decision
            (unless (or (= tp -2) (= tp -3)) (incf n))))))))

(defun cabac-mb-type-b (ss)
  "mb_type in a B slice (Table 9-34's B column).  Returns 0..22, or (+ 23 intra-type).

   The tree is deliberately lopsided: B_Direct_16x16 costs one bin because it is much the
   commonest, the two single-list 16x16 types cost three, and the two-partition combinations —
   which is most of the table — cost six or seven."
  (let ((c (ss-cabac ss)))
    (if (zerop (decode-decision c (+ +ctx-mb-type-b+ (%mb-type-b-ctx-inc ss))))
        0
        (if (zerop (decode-decision c (+ +ctx-mb-type-b+ 3)))
            (+ 1 (decode-decision c (+ +ctx-mb-type-b+ 5)))
            (let ((bits (logior (ash (decode-decision c (+ +ctx-mb-type-b+ 4)) 3)
                                (ash (decode-decision c (+ +ctx-mb-type-b+ 5)) 2)
                                (ash (decode-decision c (+ +ctx-mb-type-b+ 5)) 1)
                                (decode-decision c (+ +ctx-mb-type-b+ 5)))))
              (cond
                ((< bits 8) (+ bits 3))
                ((= bits 13) (+ 23 (cabac-intra-suffix ss +ctx-mb-type-b-suffix+)))
                ((= bits 14) 11)
                ((= bits 15) 22)
                (t (- (logior (ash bits 1) (decode-decision c (+ +ctx-mb-type-b+ 5))) 4))))))))

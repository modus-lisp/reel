;;;; mpeg2/slice.lisp — the macroblock layer: types, motion, coefficients, reconstruction.
;;;;
;;;; A slice is a run of macroblocks along one row, and its length is not transmitted: it ends when
;;;; twenty-three zero bits appear, because that is the prefix of a start code and an encoder
;;;; guarantees it cannot occur anywhere else.  That is also the whole error-resilience story of
;;;; MPEG-2 — a damaged slice costs one row, and the next start code resynchronises.
;;;;
;;;; THE ADDRESS IS A RUN LENGTH.  Macroblocks are not numbered; each one says how far it is from the
;;;; one before, and the gap is filled with SKIPPED macroblocks whose meaning depends on the picture
;;;; type.  In a P picture a skipped macroblock is a zero vector from the forward reference.  In a B
;;;; picture it is the previous macroblock's vectors and prediction mode repeated.  Those are not the
;;;; same rule and getting them the wrong way round produces a picture that is right wherever
;;;; anything moves and wrong wherever nothing does.

(in-package #:reel.mpeg2)

;;; macroblock_type flags, the specification's own (Tables B.2-B.4)
(defconstant +mb-intra+ 1)
(defconstant +mb-pattern+ 2)
(defconstant +mb-backward+ 4)
(defconstant +mb-forward+ 8)
(defconstant +mb-quant+ 16)

;;; frame_motion_type / field_motion_type (Tables 6-17, 6-18)
(defconstant +mt-field+ 1)
(defconstant +mt-frame+ 2)
(defconstant +mt-dmv+ 3)

(define-vlc dc-luma +dc-size-luma+ "dct_dc_size_luminance")
(define-vlc dc-chroma +dc-size-chroma+ "dct_dc_size_chrominance")
(define-vlc mb-incr +mb-addr-incr+ "macroblock_address_increment")
(define-vlc cbp-vlc +cbp-table+ "coded_block_pattern")
(define-vlc motion +motion-code+ "motion_code")
(define-vlc coeff-b14 +coeff-vlc-b14+ "DCT coefficients, Table B.14")
(define-vlc coeff-b15 +coeff-vlc-b15+ "DCT coefficients, Table B.15")

;;; macroblock_type carries its flags rather than its position, so these two are built by hand
(defparameter +p-type-table+ nil)
(defparameter +p-type-bits+ 0)
(defparameter +b-type-table+ nil)
(defparameter +b-type-bits+ 0)
(multiple-value-bind (a n) (build-vlc (mapcar (lambda (e) (list (first e) (second e) (third e)))
                                              +p-mb-type+))
  (setf +p-type-table+ a +p-type-bits+ n))
(multiple-value-bind (a n) (build-vlc (mapcar (lambda (e) (list (first e) (second e) (third e)))
                                              +b-mb-type+))
  (setf +b-type-table+ a +b-type-bits+ n))
(declaim (type (or null fixnums) +p-type-table+ +b-type-table+)
         (type fixnum +p-type-bits+ +b-type-bits+))

;;; ---- a decoded picture -----------------------------------------------------------------------

(defstruct (frame (:conc-name fr-))
  (width 0 :type fixnum) (height 0 :type fixnum)         ; displayed
  (cwidth 0 :type fixnum) (cheight 0 :type fixnum)       ; coded, a whole number of macroblocks
  (y (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (u (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (v (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (ystride 0 :type fixnum) (cstride 0 :type fixnum)
  (coding-type 1 :type fixnum)
  (temporal-reference 0 :type fixnum)
  (timestamp nil))

(defun make-frame-for (seq)
  (let* ((cw (* 16 (seq-mb-width seq))) (ch (* 16 (seq-mb-height seq))))
    (make-frame :width (seq-width seq) :height (seq-height seq)
                :cwidth cw :cheight ch
                :y (make-array (* cw ch) :element-type '(unsigned-byte 8) :initial-element 0)
                :u (make-array (* (ash cw -1) (ash ch -1)) :element-type '(unsigned-byte 8)
                               :initial-element 128)
                :v (make-array (* (ash cw -1) (ash ch -1)) :element-type '(unsigned-byte 8)
                               :initial-element 128)
                :ystride cw :cstride (ash cw -1))))

;;; ---- the state one slice carries ---------------------------------------------------------------

(defstruct (slice-state (:conc-name ss-))
  br seq ph
  cur forward backward                          ; the current frame and the two references
  (mbx 0 :type fixnum) (mby 0 :type fixnum)
  (qscale 2 :type fixnum)
  ;; DC predictors, one per component.  Reset at the start of a slice and by every non-intra
  ;; macroblock, which is why an intra macroblock beside an inter one predicts from grey.
  (dc (make-array 3 :element-type 'fixnum) :type fixnums)
  ;; PMV[list][slot][component]: the running motion vector predictor.  Two slots because field
  ;; prediction in a frame picture carries a vector per field.
  (pmv (make-array '(2 2 2) :element-type 'fixnum :initial-element 0)
       :type (simple-array fixnum (2 2 2)))
  ;; the vectors this macroblock actually uses, and which reference field each came from
  (mv (make-array '(2 2 2) :element-type 'fixnum :initial-element 0)
      :type (simple-array fixnum (2 2 2)))
  (field-select (make-array '(2 2) :element-type 'fixnum :initial-element 0)
                :type (simple-array fixnum (2 2)))
  (mv-type +mt-frame+ :type fixnum)
  (mb-flags 0 :type fixnum)                     ; the previous macroblock's, for B skips
  (interlaced-dct nil)
  (block (make-array 64 :element-type 'fixnum) :type (simple-array fixnum (64))))

(declaim (inline %escape-level %oddify))
(defun %escape-level (br mpeg1-p)
  "The level of an escape-coded coefficient.

   MPEG-2 spends twelve bits and is done.  MPEG-1 spends EIGHT, and reserves the two values it
   cannot otherwise express — 0 and -128 — as a marker that eight more bits follow.  So the same
   escape is one, two or three bytes long depending on the magnitude, and a decoder that reads
   MPEG-2's twelve bits from an MPEG-1 stream is four bits adrift immediately."
  (declare (optimize (speed 3) (safety 1)))
  (if (not mpeg1-p)
      (read-signed br 12)
      (let ((v (read-signed br 8)))
        (declare (type fixnum v))
        (cond ((= v -128) (- (read-bits br 8) 256))
              ((zerop v) (read-bits br 8))
              (t v)))))

(defun %oddify (m)
  "MPEG-1's reconstruction rule: make the magnitude odd by subtracting one when it is even (2.4.4.2).

   MPEG-2 replaced this with mismatch control on the last coefficient of the block.  They exist for
   the same reason — bounding the drift between two decoders whose transforms differ slightly — and
   a stream gets exactly one of them, never both."
  (declare (type fixnum m) (optimize (speed 3) (safety 1)))
  (logior (1- m) 1))

(declaim (inline %sat))
(defun %sat (v)
  "Saturate a reconstructed coefficient to [-2048, 2047] (7.4.3).

   Normative, and on any real picture a no-op: an encoder that produced a coefficient outside this
   range would be describing a sample outside the range samples have."
  (declare (type fixnum v) (optimize (speed 3) (safety 1)))
  (cond ((< v -2048) -2048) ((> v 2047) 2047) (t v)))

(declaim (inline %reset-dc %reset-pmv))
(defun %reset-dc (ss)
  (let ((v (ash 128 (ph-intra-dc-precision (ss-ph ss)))))
    (dotimes (i 3) (setf (aref (ss-dc ss) i) v))))
(defun %reset-pmv (ss)
  (dotimes (l 2) (dotimes (s 2) (dotimes (c 2) (setf (aref (ss-pmv ss) l s c) 0)))))

;;; ---- coefficients --------------------------------------------------------------------------

(defun %decode-dc-size (ss luma-p)
  (or (if luma-p
          (%vlc (ss-br ss) +dc-luma-table+ +dc-luma-bits+)
          (%vlc (ss-br ss) +dc-chroma-table+ +dc-chroma-bits+))
      (%err "dct_dc_size at macroblock (~d,~d)" (ss-mbx ss) (ss-mby ss))))

(defun decode-intra-block (ss n)
  "One intra 8x8 block: a DC differential against the running predictor, then run-level pairs.

   The DC is coded as a DIFFERENCE from the last block of the same component, which is why the
   predictors have to be reset wherever the chain breaks — at a slice boundary and at every
   non-intra macroblock.  A decoder that forgets one of those resets produces a picture with
   correct detail and drifting brightness."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (ph (ss-ph ss)) (blk (ss-block ss))
         (luma-p (< n 4))
         (component (if luma-p 0 (1+ (logand n 1))))
         (matrix (if luma-p (seq-intra-matrix (ss-seq ss)) (seq-chroma-intra-matrix (ss-seq ss))))
         (scan (if (ph-alternate-scan ph) +alt-scan+ +zigzag+))
         (qscale (ss-qscale ss))
         (size (%decode-dc-size ss luma-p))
         (diff (read-dc-differential br size))
         (dc (+ (aref (ss-dc ss) component) diff))
         (mismatch 0)
         (mpeg1-p (not (seq-mpeg2-p (ss-seq ss))))
         (table (if (ph-intra-vlc-format ph) +coeff-b15-table+ +coeff-b14-table+))
         (bits (if (ph-intra-vlc-format ph) +coeff-b15-bits+ +coeff-b14-bits+)))
    (declare (type fixnum size diff dc qscale mismatch bits)
             (type (simple-array (unsigned-byte 8) (64)) matrix scan))
    (fill blk 0)
    (setf (aref (ss-dc ss) component) dc)
    ;; MPEG-1 scales the DC by the matrix's own first weight, which the specification requires to
    ;; be eight; MPEG-2 scales it by a shift the picture chooses.  Written the way each says rather
    ;; than the way they coincide.
    (setf (aref blk 0) (if mpeg1-p (* dc (aref matrix 0))
                           (ash dc (- 3 (ph-intra-dc-precision ph)))))
    (setf mismatch (logxor (aref blk 0) 1))
    (let ((i 0))
      (declare (type fixnum i))
      (loop
        (let ((sym (or (%vlc br table bits)
                       (%err "a DCT coefficient at macroblock (~d,~d)" (ss-mbx ss) (ss-mby ss)))))
          (declare (type fixnum sym))
          (when (= sym +coeff-eob+) (return))
          (multiple-value-bind (run level)
              (if (= sym +coeff-escape+)
                  (values (read-bits br 6) (%escape-level br mpeg1-p))
                  (let ((r (aref +coeff-run+ sym)) (l (aref +coeff-level+ sym)))
                    (values r (if (= 1 (read-bit br)) (- l) l))))
            (declare (type fixnum run level))
            ;; RUN IS THE NUMBER OF ZEROS, so the coefficient itself is one further on.  Both this
            ;; and the non-intra loop step by RUN+1; they differ only in where they start.
            (incf i (1+ run))
            (when (> i 63) (%err "a run past the end of a block at (~d,~d)" (ss-mbx ss) (ss-mby ss)))
            (let* ((j (aref scan i))
                   (mag (abs level))
                   (v (let ((x (ash (* mag qscale (aref matrix j)) -4)))
                        (if mpeg1-p (%oddify x) x)))
                   (val (%sat (if (minusp level) (- v) v))))
              (declare (type fixnum j mag v val))
              (setf mismatch (logxor mismatch val))
              (setf (aref blk j) val))))))
    ;; mismatch control (7.4.4): the last coefficient is forced odd, which bounds how far two
    ;; conforming decoders with slightly different transforms can drift apart.  MPEG-1 does not
    ;; have it — it makes every coefficient odd instead, which costs more and bounds the same thing.
    (unless mpeg1-p
      (setf (aref blk 63) (logxor (aref blk 63) (logand mismatch 1))))
    blk))

(defun decode-inter-block (ss n)
  "One non-intra 8x8 block.  No DC prediction: every coefficient including the first is a
   difference from the prediction already made, so there is nothing to carry between blocks."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (ph (ss-ph ss)) (blk (ss-block ss))
         (matrix (if (< n 4) (seq-non-intra-matrix (ss-seq ss))
                     (seq-chroma-non-intra-matrix (ss-seq ss))))
         (scan (if (ph-alternate-scan ph) +alt-scan+ +zigzag+))
         (qscale (ss-qscale ss))
         (mismatch 1)
         (mpeg1-p (not (seq-mpeg2-p (ss-seq ss))))
         (table +coeff-b14-table+) (bits +coeff-b14-bits+)
         (i -1) (first-p t))
    (declare (type fixnum qscale mismatch bits i)
             (type (simple-array (unsigned-byte 8) (64)) matrix scan))
    (fill blk 0)
    (loop
      ;; THE FIRST COEFFICIENT IS SPELLED DIFFERENTLY.  Table B.14's shortest code, a single 1, is
      ;; end-of-block everywhere except at the very start of a non-intra block, where the same bits
      ;; mean a run of zero and a level of one.  One table, two meanings, decided by position.
      (let ((sym (if (and first-p (= 1 (peek-bits br 1)))
                     (progn (skip-bits br 1) 0)
                     (or (%vlc br table bits)
                         (%err "a DCT coefficient at macroblock (~d,~d)" (ss-mbx ss) (ss-mby ss))))))
        (declare (type fixnum sym))
        (when (= sym +coeff-eob+) (return))
        (multiple-value-bind (run level)
            (if (= sym +coeff-escape+)
                (values (read-bits br 6) (%escape-level br mpeg1-p))
                (let ((r (aref +coeff-run+ sym)) (l (aref +coeff-level+ sym)))
                  (values r (if (= 1 (read-bit br)) (- l) l))))
          (declare (type fixnum run level))
          (setf first-p nil)
          (incf i (1+ run))
          (when (> i 63) (%err "a run past the end of a block at (~d,~d)" (ss-mbx ss) (ss-mby ss)))
          (let* ((j (aref scan i))
                 (mag (abs level))
                 (v (let ((x (ash (* (1+ (* 2 mag)) qscale (aref matrix j)) -5)))
                      (if mpeg1-p (%oddify x) x)))
                 (val (%sat (if (minusp level) (- v) v))))
            (declare (type fixnum j mag v val))
            (setf mismatch (logxor mismatch val))
            (setf (aref blk j) val)))))
    (unless mpeg1-p
      (setf (aref blk 63) (logxor (aref blk 63) (logand mismatch 1))))
    blk))

;;; ---- motion vectors -------------------------------------------------------------------------

(defun decode-mv-component (ss list comp pred)
  "One motion vector component (7.6.3.1): a Huffman-coded magnitude, an optional residual whose
   width comes from f_code, and a modulo wrap against PRED.

   The wrap is the part that looks wrong and is not.  A vector is coded as a DIFFERENCE from the
   predictor, and the difference is reduced modulo the range f_code allows — so a vector that would
   overflow comes back round the other side, and sign-extending to exactly 5 + (f_code - 1) bits is
   what recovers it.  Without that, long pans decode with the picture torn in the direction of
   travel, because the vectors that wrapped come back as enormous ones."
  (declare (type fixnum list comp pred) (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss))
         (fcode (aref (ph-f-code (ss-ph ss)) list comp))
         (code (or (%vlc br +motion-table+ +motion-bits+)
                   (%err "motion_code at macroblock (~d,~d)" (ss-mbx ss) (ss-mby ss)))))
    (declare (type fixnum fcode code))
    (when (zerop code) (return-from decode-mv-component pred))
    (let* ((sign (read-bit br))
           (shift (1- fcode))
           (val code))
      (declare (type fixnum sign shift val))
      (when (plusp shift)
        (setf val (logior (ash (1- val) shift) (read-bits br shift)))
        (incf val))
      (when (= 1 sign) (setf val (- val)))
      (incf val pred)
      (let* ((bits (+ 5 shift))
             (m (logand val (1- (ash 1 bits)))))
        (declare (type fixnum bits m))
        (if (logbitp (1- bits) m) (- m (ash 1 bits)) m)))))

(defun %decode-frame-motion (ss list)
  "The vectors for one direction of a macroblock in a FRAME picture."
  (declare (type fixnum list) (optimize (speed 3) (safety 1)))
  (let ((br (ss-br ss)) (ph (ss-ph ss)) (pmv (ss-pmv ss)) (mv (ss-mv ss)))
    (cond
      ((= (ss-mv-type ss) +mt-frame+)
       (let ((x (decode-mv-component ss list 0 (aref pmv list 0 0))))
         (setf (aref pmv list 0 0) x (aref pmv list 1 0) x)
         (let ((y (decode-mv-component ss list 1 (aref pmv list 0 1))))
           (setf (aref pmv list 0 1) y (aref pmv list 1 1) y)
           ;; MPEG-1 may code vectors in whole samples rather than halves.  The PREDICTOR stays in
           ;; the coded units; only the vector that reaches motion compensation is doubled.
           (when (if (zerop list) (ph-full-pel-forward ph) (ph-full-pel-backward ph))
             (setf x (* 2 x) y (* 2 y)))
           (setf (aref mv list 0 0) x (aref mv list 0 1) y))))
      ((= (ss-mv-type ss) +mt-field+)
       ;; two 16x8 predictions, each from a chosen field of the reference.  The VERTICAL component
       ;; is measured in FIELD lines, so the predictor is halved on the way in and doubled on the
       ;; way back out — the predictor chain stays in frame units and the vector does not.
       (dotimes (s 2)
         (setf (aref (ss-field-select ss) list s) (read-bit br))
         (let ((x (decode-mv-component ss list 0 (aref pmv list s 0))))
           (setf (aref pmv list s 0) x (aref mv list s 0) x))
         (let ((y (decode-mv-component ss list 1 (ash (aref pmv list s 1) -1))))
           (setf (aref pmv list s 1) (* 2 y) (aref mv list s 1) y))))
      (t (%err "dual-prime motion vectors are not supported")))))

;;; ---- prediction ------------------------------------------------------------------------------

(defun %predict (ss list ref avg-p)
  "Motion compensate this macroblock from REF in one direction."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((cur (ss-cur ss)) (mbx (ss-mbx ss)) (mby (ss-mby ss))
         (ys (fr-ystride cur)) (cs (fr-cstride cur))
         (cw (fr-cwidth cur)) (ch (fr-cheight cur))
         (ccw (ash cw -1)) (cch (ash ch -1))
         (ybase (+ (* mby 16 ys) (* mbx 16)))
         (cbase (+ (* mby 8 cs) (* mbx 8))))
    (declare (type fixnum ys cs cw ch ccw cch ybase cbase))
    (ecase (ss-mv-type ss)
      (#.+mt-frame+
       (let ((mx (aref (ss-mv ss) list 0 0)) (my (aref (ss-mv ss) list 0 1)))
         (predict-block (fr-y cur) ys ybase (fr-y ref) ys cw ch
                        (* mbx 16) (* mby 16) 16 16 mx my avg-p)
         (let ((cx (chroma-mv mx)) (cy (chroma-mv my)))
           (predict-block (fr-u cur) cs cbase (fr-u ref) cs ccw cch
                          (* mbx 8) (* mby 8) 8 8 cx cy avg-p)
           (predict-block (fr-v cur) cs cbase (fr-v ref) cs ccw cch
                          (* mbx 8) (* mby 8) 8 8 cx cy avg-p))))
      (#.+mt-field+
       ;; each half is one FIELD of this macroblock: eight lines taken every other row.  Both the
       ;; source and the destination are read at twice the stride, and the chosen reference field
       ;; is simply a one-row offset into the same plane.
       (dotimes (s 2)
         (let* ((mx (aref (ss-mv ss) list s 0)) (my (aref (ss-mv ss) list s 1))
                (fs (aref (ss-field-select ss) list s))
                (cx (chroma-mv mx)) (cy (chroma-mv my)))
           (declare (type fixnum mx my fs cx cy))
           (predict-block (fr-y cur) (* 2 ys) (+ ybase (* s ys))
                          (fr-y ref) (* 2 ys) cw (ash ch -1)
                          (* mbx 16) (* mby 8) 16 8 mx my avg-p
                          :src-offset (* fs ys))
           (predict-block (fr-u cur) (* 2 cs) (+ cbase (* s cs))
                          (fr-u ref) (* 2 cs) ccw (ash cch -1)
                          (* mbx 8) (* mby 4) 8 4 cx cy avg-p :src-offset (* fs cs))
           (predict-block (fr-v cur) (* 2 cs) (+ cbase (* s cs))
                          (fr-v ref) (* 2 cs) ccw (ash cch -1)
                          (* mbx 8) (* mby 4) 8 4 cx cy avg-p :src-offset (* fs cs))))))))

;;; ---- one macroblock ---------------------------------------------------------------------------

(defun %block-base (ss n)
  "Where block N of this macroblock lands, as (values plane stride base).

   FIELD DCT is the wrinkle.  When a macroblock says its transform was applied to fields rather
   than to the frame, its four luma blocks are not the four quadrants: blocks 0 and 1 are the EVEN
   lines of the macroblock and blocks 2 and 3 the odd ones, so they are written at twice the stride
   one row apart.  Chroma is untouched by this in 4:2:0."
  (let* ((cur (ss-cur ss)) (ys (fr-ystride cur)) (cs (fr-cstride cur))
         (ybase (+ (* (ss-mby ss) 16 ys) (* (ss-mbx ss) 16)))
         (cbase (+ (* (ss-mby ss) 8 cs) (* (ss-mbx ss) 8))))
    (declare (type fixnum ys cs ybase cbase))
    (if (< n 4)
        (if (ss-interlaced-dct ss)
            (values (fr-y cur) (* 2 ys)
                    (+ ybase (* (ash n -1) ys) (* 8 (logand n 1))))
            (values (fr-y cur) ys
                    (+ ybase (* (ash n -1) 8 ys) (* 8 (logand n 1)))))
        (values (if (= n 4) (fr-u cur) (fr-v cur)) cs cbase))))

(defun %read-qscale (ss)
  (let ((code (read-bits (ss-br ss) 5)))
    (when (zerop code) (%err "quantiser_scale_code 0 at macroblock (~d,~d)" (ss-mbx ss) (ss-mby ss)))
    (setf (ss-qscale ss) (ph-quantiser-scale (ss-ph ss) code))))

(defun %read-mb-type (ss)
  "macroblock_type, as the specification's five flags (Tables B.2-B.4)."
  (let ((br (ss-br ss)) (type (ph-coding-type (ss-ph ss))))
    (cond
      ((= type +pic-i+)
       ;; two codes only, and worth spelling out rather than tabulating
       (if (= 1 (read-bit br))
           +mb-intra+
           (if (= 1 (read-bit br))
               (logior +mb-quant+ +mb-intra+)
               (%err "macroblock_type in an I picture at (~d,~d)" (ss-mbx ss) (ss-mby ss)))))
      ((= type +pic-p+)
       (or (%vlc br +p-type-table+ +p-type-bits+)
           (%err "macroblock_type in a P picture at (~d,~d)" (ss-mbx ss) (ss-mby ss))))
      (t
       (or (%vlc br +b-type-table+ +b-type-bits+)
           (%err "macroblock_type in a B picture at (~d,~d)" (ss-mbx ss) (ss-mby ss)))))))

(defun decode-macroblock (ss)
  "Parse and reconstruct one coded macroblock."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (ss-br ss)) (ph (ss-ph ss))
         (flags (%read-mb-type ss))
         (intra-p (logtest flags +mb-intra+))
         (frame-p (= 3 (ph-structure ph))))
    (declare (type fixnum flags))
    (setf (ss-mb-flags ss) flags)
    (setf (ss-interlaced-dct ss) nil)
    (cond
      (intra-p
       (when (and frame-p (not (ph-frame-pred-frame-dct ph)))
         (setf (ss-interlaced-dct ss) (= 1 (read-bit br))))
       (when (logtest flags +mb-quant+) (%read-qscale ss))
       (if (ph-concealment-motion-vectors ph)
           ;; read and discard: they exist so a decoder can conceal a lost slice, and this one
           ;; refuses damaged streams instead of concealing them.  The BITS still have to go.
           (progn (setf (ss-mv-type ss) +mt-frame+)
                  (%decode-frame-motion ss 0)
                  (read-bit br))               ; marker_bit
           (%reset-pmv ss)))
      (t
       (setf (ss-mv-type ss)
             (cond ((not (logtest flags (logior +mb-forward+ +mb-backward+))) +mt-frame+)
                   ((and frame-p (ph-frame-pred-frame-dct ph)) +mt-frame+)
                   (t (read-bits br 2))))
       (when (and frame-p (not (ph-frame-pred-frame-dct ph))
                  (logtest flags (logior +mb-forward+ +mb-backward+))
                  (logtest flags +mb-pattern+))
         (setf (ss-interlaced-dct ss) (= 1 (read-bit br))))
       (when (and (not (logtest flags (logior +mb-forward+ +mb-backward+)))
                  frame-p (not (ph-frame-pred-frame-dct ph)))
         (setf (ss-interlaced-dct ss) (= 1 (read-bit br))))
       (when (logtest flags +mb-quant+) (%read-qscale ss))
       ;; a macroblock with neither motion flag is "no motion compensation": a zero vector from
       ;; the forward reference, and the predictors reset to nothing
       (if (logtest flags (logior +mb-forward+ +mb-backward+))
           (progn
             (when (logtest flags +mb-forward+) (%decode-frame-motion ss 0))
             (when (logtest flags +mb-backward+) (%decode-frame-motion ss 1)))
           (progn (%reset-pmv ss)
                  (dotimes (c 2) (setf (aref (ss-mv ss) 0 0 c) 0))))))
    ;; ---- prediction, before any residual is added to it
    (unless intra-p
      (let ((fwd (or (logtest flags +mb-forward+)
                     (not (logtest flags (logior +mb-forward+ +mb-backward+)))))
            (bwd (logtest flags +mb-backward+)))
        (when fwd
          (unless (ss-forward ss) (%err "a forward-predicted macroblock with no reference picture"))
          (%predict ss 0 (ss-forward ss) nil))
        (when bwd
          (unless (ss-backward ss) (%err "a backward-predicted macroblock with no reference picture"))
          (%predict ss 1 (ss-backward ss) fwd))))
    ;; ---- the residual
    (let ((cbp (cond (intra-p 63)
                     ((logtest flags +mb-pattern+)
                      (or (%vlc br +cbp-vlc-table+ +cbp-vlc-bits+)
                          (%err "coded_block_pattern at (~d,~d)" (ss-mbx ss) (ss-mby ss))))
                     (t 0))))
      (declare (type fixnum cbp))
      (when (and (logtest flags +mb-pattern+) (zerop cbp) (not intra-p))
        (%err "coded_block_pattern 0 at (~d,~d)" (ss-mbx ss) (ss-mby ss)))
      (dotimes (n 6)
        (when (logbitp (- 5 n) cbp)
          (multiple-value-bind (plane stride base) (%block-base ss n)
            (if intra-p
                (progn (decode-intra-block ss n) (idct-put plane stride base (ss-block ss)))
                (progn (decode-inter-block ss n) (idct-add plane stride base (ss-block ss))))))))
    (unless intra-p (%reset-dc ss))
    flags))

(defun skip-macroblock (ss)
  "A macroblock the stream did not code at all (7.6.6).

   Its meaning depends entirely on the picture type, and the two rules are not variations of each
   other.  In a P picture it is a zero vector from the forward reference and the predictors are
   cleared.  In a B picture it repeats the PREVIOUS macroblock's vectors and prediction directions
   and the predictors are left alone."
  (declare (optimize (speed 3) (safety 1)))
  (let ((type (ph-coding-type (ss-ph ss))))
    (%reset-dc ss)
    (setf (ss-interlaced-dct ss) nil
          (ss-mv-type ss) +mt-frame+)
    (cond
      ((= type +pic-i+) (%err "a skipped macroblock in an I picture at (~d,~d)"
                              (ss-mbx ss) (ss-mby ss)))
      ((= type +pic-p+)
       (%reset-pmv ss)
       (dotimes (c 2) (setf (aref (ss-mv ss) 0 0 c) 0))
       (unless (ss-forward ss) (%err "a skipped macroblock with no reference picture"))
       (%predict ss 0 (ss-forward ss) nil))
      (t
       (dotimes (l 2) (dotimes (c 2) (setf (aref (ss-mv ss) l 0 c) (aref (ss-pmv ss) l 0 c))))
       (let* ((flags (ss-mb-flags ss))
              (fwd (logtest flags +mb-forward+))
              (bwd (logtest flags +mb-backward+)))
         (when (and (not fwd) (not bwd))
           (%err "a skipped macroblock in a B picture with nothing to repeat at (~d,~d)"
                 (ss-mbx ss) (ss-mby ss)))
         (when fwd (%predict ss 0 (ss-forward ss) nil))
         (when bwd (%predict ss 1 (ss-backward ss) fwd)))))))

;;; ---- one slice --------------------------------------------------------------------------------

(defun %read-increment (ss)
  "macroblock_address_increment, escapes and stuffing consumed.  NIL at the end of the slice."
  (let ((br (ss-br ss)) (acc 0))
    (declare (type fixnum acc))
    (loop
      (let ((sym (or (%vlc br +mb-incr-table+ +mb-incr-bits+)
                     (%err "macroblock_address_increment at row ~d" (ss-mby ss)))))
        (declare (type fixnum sym))
        (cond ((= sym 35) (return nil))          ; the end-of-slice code
              ((= sym 34))                       ; stuffing: read and thrown away
              ((= sym 33) (incf acc 33))         ; the escape: another code follows
              (t (return (+ acc sym 1))))))))

(defun decode-slice (ss row bytes start end)
  "Every macroblock of one slice, into the current frame."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (make-br bytes :start start :end end))
         (seq (ss-seq ss)))
    (setf (ss-br ss) br (ss-mby ss) row)
    (when (and (seq-mpeg2-p seq) (> (seq-mb-height seq) (floor 2800 16)))
      (read-bits br 3))                          ; slice_vertical_position_extension
    (%read-qscale ss)
    ;; intra_slice_flag and the extra information that may follow it
    (when (= 1 (peek-bits br 1))
      (read-bit br) (read-bit br) (read-bits br 7))
    (loop while (= 1 (read-bit br)) do (read-bits br 8))
    (%reset-dc ss)
    (%reset-pmv ss)
    (setf (ss-mb-flags ss) 0)
    (let ((first-p t) (mbx -1))
      (declare (type fixnum mbx))
      (loop
        (let ((inc (%read-increment ss)))
          (when (null inc) (return))
          ;; the FIRST increment of a slice positions rather than skips: macroblocks before it were
          ;; never in this slice at all, and a P picture must not treat them as zero-vector copies
          (if first-p
              (setf mbx (1- inc) first-p nil)
              (progn
                (dotimes (k (1- inc))
                  (setf (ss-mbx ss) (+ mbx k 1))
                  (when (>= (ss-mbx ss) (seq-mb-width seq))
                    (%err "a macroblock past the end of row ~d" row))
                  (skip-macroblock ss))
                (incf mbx inc)))
          (setf (ss-mbx ss) mbx)
          (when (>= mbx (seq-mb-width seq))
            (%err "a macroblock past the end of row ~d" row))
          (decode-macroblock ss)
          (when (next-start-code-p br) (return))
          (when (br-eof-p br) (return)))))))

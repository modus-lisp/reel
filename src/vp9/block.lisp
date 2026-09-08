;;;; vp9/block.lisp — tiles, the partition tree, block modes, and coefficients.
;;;;
;;;; A VP9 frame is a grid of 64x64 SUPERBLOCKS, and each is a quadtree: at every level a block may
;;;; be left whole, split in two across, split in two down, or split into four and the question asked
;;;; again.  Four levels take it from 64x64 to 8x8, and an 8x8 may split once more into 4x4s.  So a
;;;; block is not a fixed size and not even square — 64x32 and 4x8 are ordinary — and thirteen sizes
;;;; exist, which is why so much here is a table indexed by size.
;;;;
;;;; TILES ARE INDEPENDENT AND THAT IS THE POINT.  A frame may be cut into tile columns and rows,
;;;; each its own arithmetic-coded partition with its own left-edge context, so that they can be
;;;; decoded in parallel and, more importantly, so that a lost one damages only its own rectangle.
;;;; Every tile but the last carries its length in four bytes in front of it.
;;;;
;;;; The contexts are the awkward part and they are what most of this file is.  Nearly every symbol
;;;; is coded against what the blocks ABOVE and to the LEFT did, at a granularity that differs per
;;;; symbol — per 4 samples for prediction modes and coefficient counts, per 8 for skip and transform
;;;; size, per superblock for the partition.  The above contexts span the frame and the left ones
;;;; span one superblock row of one tile, which is exactly what makes a tile independent.

(in-package #:reel.vp9)

;;; ---- block geometry ------------------------------------------------------------------------------

(defconstant +bs-8x8+ 9 "Block sizes are ordered largest first, so `> BS_8x8' means smaller than 8x8.")
(defconstant +bs-8x4+ 10)
(defconstant +bs-4x8+ 11)

(defparameter +max-tx-for-bs+
  (make-array 13 :element-type '(signed-byte 32) :initial-contents
              '(3 3 3 3 2 2 2 1 1 1 0 0 0))
  "The largest transform each block size may use: a 4x8 block cannot hold an 8x8 transform.")

(defparameter +left-ctx-for-bs+
  (make-array 13 :element-type '(unsigned-byte 8) :initial-contents
              '(#x0 #x8 #x0 #x8 #xc #x8 #xc #xe #xc #xe #xf #xe #xf))
  "What a block of each size writes into the partition context to its right, as a bit per level.")

(defparameter +above-ctx-for-bs+
  (make-array 13 :element-type '(unsigned-byte 8) :initial-contents
              '(#x0 #x0 #x8 #x8 #x8 #xc #xc #xc #xe #xe #xe #xf #xf))
  "And into the one below it.")

(defparameter +band-counts+
  (make-array '(4 8) :element-type '(signed-byte 32) :initial-contents
              '((1 2 3 4  3    3 0 0)
                (1 2 3 4 11   43 0 0)
                (1 2 3 4 11  235 0 0)
                (1 2 3 4 11 1003 0 0)))
  "How many coefficient positions each of the six probability BANDS covers, per transform size.
   The first few positions get a band each because the low frequencies differ most from one another;
   everything past the twenty-first shares one.

   EIGHT COLUMNS FOR SIX BANDS, and the two zeros are load-bearing.  The band advances after the
   last coefficient of a block as well as between them, so the counter runs one past the end before
   the loop notices it is finished — and lands on a zero, which advances it no further.")

(declaim (type (simple-array (signed-byte 32) (13)) +max-tx-for-bs+)
         (type (simple-array (unsigned-byte 8) (13)) +left-ctx-for-bs+ +above-ctx-for-bs+)
         (type (simple-array (signed-byte 32) (4 8)) +band-counts+))

;;; ---- the decoding state --------------------------------------------------------------------------

(defstruct (state (:conc-name st-) (:constructor make-state-))
  h fp                                          ; the two headers
  (cols 0 :type fixnum) (rows 0 :type fixnum)   ; in eight-sample units
  (sb-cols 0 :type fixnum) (sb-rows 0 :type fixnum)
  ;; above contexts: one per column, at the granularity each symbol wants
  (above-partition (%o 0) :type octets)         ; per 8 samples
  (above-mode (%o 0) :type octets)              ; per 4
  (above-y-nnz (%o 0) :type octets)             ; per 4
  (above-uv-nnz (vector (%o 0) (%o 0)) :type simple-vector)   ; per 8, being 4:2:0
  (above-skip (%o 0) :type octets)
  (above-txfm (%o 0) :type octets)
  (above-segpred (%o 0) :type octets)
  (above-intra (%o 0) :type octets)
  (above-comp (%o 0) :type octets)
  (above-ref (%o 0) :type octets)
  (above-filter (%o 0) :type octets)
  ;; the vectors an inter block's neighbours used, two per eight-sample column, two lists each
  (above-mv (make-array '(1 2 2) :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (* 2 2)))
  ;; left contexts: one superblock row of one tile, and reset at the start of each
  (left-partition (%o 8) :type octets)
  (left-mode (%o 16) :type octets)
  (left-y-nnz (%o 16) :type octets)
  (left-uv-nnz (vector (%o 8) (%o 8)) :type simple-vector)
  (left-skip (%o 8) :type octets)
  (left-txfm (%o 8) :type octets)
  (left-segpred (%o 8) :type octets)
  (left-intra (%o 8) :type octets)
  (left-comp (%o 8) :type octets)
  (left-ref (%o 8) :type octets)
  (left-filter (%o 8) :type octets)
  (left-mv (make-array '(16 2 2) :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (16 2 2)))
  ;; the block being decoded
  (row 0 :type fixnum) (col 0 :type fixnum) (row7 0 :type fixnum)
  (bs 0 :type fixnum) (bl 0 :type fixnum) (bp 0 :type fixnum)
  (seg-id 0 :type fixnum) (skip nil) (intra t)
  (tx 0 :type fixnum) (uvtx 0 :type fixnum)
  (mode (make-array 4 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (4)))
  (uvmode 0 :type fixnum)
  ;; an inter block: which references, whether both, which filter, and a vector per sub-block
  (bref (make-array 2 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (2)))
  (bcomp nil)
  (bfilter 0 :type fixnum) (bfilter-id 0 :type fixnum)
  (bmv (make-array '(4 2 2) :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (4 2 2)))
  (min-mvx 0 :type fixnum) (min-mvy 0 :type fixnum)
  (max-mvx 0 :type fixnum) (max-mvy 0 :type fixnum)
  ;; the reference and vector of every eight-sample block of this frame, and of the last one
  (mvref (make-array 6 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (*)))
  mvref-prev
  (use-last-mvs nil)
  ;; the segment each block belongs to, this frame and last
  (segmap (%o 0) :type octets)
  segmap-prev
  ;; the three reference frames this frame may predict from
  (ref-frames (make-array 3 :initial-element nil) :type simple-vector)
  ;; and how often each symbol was decoded, which a non-parallel stream turns into the next
  ;; frame's probability model
  (counts (make-counts) :type counts)
  ;; How many blocks predicted from TWO references.  Kept because compound prediction is reachable
  ;; only from a stream whose alt-ref points forward in time, which most encoder settings do not
  ;; produce — so a test that merely decodes such a stream correctly cannot tell whether it
  ;; exercised the path at all.  This lets it say so.
  (comp-blocks 0 :type fixnum)
  ;; Scratch for motion compensation, allocated once for the largest block a frame can hold rather
  ;; than per call.  A 64x64 block needs seventy-one rows of intermediate, and allocating that on
  ;; every one of the thousands of predictions in a frame costs more than the filtering does.
  (mc-tmp (make-array (* 64 71) :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (4544)))
  (mc-win (make-array (* 71 71) :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (5041)))
  (tile-col-start 0 :type fixnum) (tile-col-end 0 :type fixnum)
  ;; coefficients of one block, and how many each of its transform blocks held
  (coeffs (make-array 4096 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (4096)))
  (uvcoeffs (vector (make-array 1024 :element-type '(signed-byte 32))
                    (make-array 1024 :element-type '(signed-byte 32)))
            :type simple-vector)
  (eob (make-array 256 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (256)))
  (uveob (vector (make-array 64 :element-type '(signed-byte 32)) (make-array 64 :element-type '(signed-byte 32)))
         :type simple-vector)
  ;; the dequantiser, per segment: [seg][plane][dc? ac?]
  (qmul (make-array '(8 2 2) :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (8 2 2)))
  ;; per-segment skip and reference features, read out of the header once
  (seg-skip (make-array 8 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (8)))
  frame                                         ; the picture being decoded
  ;; The last line of each superblock row, saved BEFORE the loop filter touched it.  Intra
  ;; prediction is defined on unfiltered samples and the filter has already run over that line by
  ;; the time the row below is decoded, so exactly one line has to be kept.
  (intra-row (vector (%o 0) (%o 0) (%o 0)) :type simple-vector)
  (edge-a (%o 96) :type octets)                 ; the row above, corner at index thirty-one
  (edge-l (%o 64) :type octets)                 ; and the column to the left
  ;; the loop filter's working data: a level per 8x8 block and an edge mask, per superblock column
  (lf-level (make-array '(1 8 8) :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (* 8 8)))
  (lf-mask (make-array '(1 2 2 8 4) :element-type '(signed-byte 32))
           :type (simple-array (signed-byte 32) (* 2 2 8 4)))
  (lf-lim (make-array 64 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (64)))
  (lf-mblim (make-array 64 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (64)))
  ;; [segment][reference + 1, or 0 for intra][the block's vector is non-zero]
  (lf-lvl (make-array '(8 4 2) :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (8 4 2)))
  c                                             ; the arithmetic coder of the current tile
  (blocks 0 :type fixnum)                       ; how many blocks this frame has decoded
  ;; THE CHECK THAT COSTS NOTHING: how far any tile's coder finished from the end of its own
  ;; partition.  A correct parse lands on it — the coder reads one byte ahead and no more — so a
  ;; slack of more than one says a symbol somewhere was read at the wrong width or under the wrong
  ;; condition, and says it without a decoded picture to compare against.
  (tile-slack 0 :type fixnum))

(defun %init-quant (st h)
  "The dequantiser for every segment, from the header's base index and deltas (6.2.11)."
  (let ((q (st-qmul st)))
    (flet ((c8 (v) (max 0 (min 255 v))))
      (dotimes (i (if (h-seg-enabled h) 8 1))
        (let ((yac (h-base-q h)))
          (when (and (h-seg-enabled h) (plusp (aref (h-seg-feature-on h) i 0)))
            (setf yac (if (h-seg-abs h)
                          (c8 (aref (h-seg-feature h) i 0))
                          (c8 (+ (h-base-q h) (aref (h-seg-feature h) i 0))))))
          (let ((ydc (c8 (+ yac (h-ydc-delta h))))
                (uvdc (c8 (+ yac (h-uvdc-delta h))))
                (uvac (c8 (+ yac (h-uvac-delta h)))))
            (setf yac (c8 yac))
            (setf (aref q i 0 0) (aref +dc-qlookup+ ydc)
                  (aref q i 0 1) (aref +ac-qlookup+ yac)
                  (aref q i 1 0) (aref +dc-qlookup+ uvdc)
                  (aref q i 1 1) (aref +ac-qlookup+ uvac)))))
      (dotimes (i 8)
        (setf (aref (st-seg-skip st) i)
              (if (and (h-seg-enabled h) (plusp (aref (h-seg-feature-on h) i 3))) 1 0))))))

(defun make-state (h fp)
  "Everything that depends on the frame's size and headers."
  (let* ((cols (ash (+ (h-width h) 7) -3))
         (rows (ash (+ (h-height h) 7) -3))
         (sb-cols (ash (+ (h-width h) 63) -6))
         (sb-rows (ash (+ (h-height h) 63) -6))
         (st (make-state- :h h :fp fp :cols cols :rows rows
                          :sb-cols sb-cols :sb-rows sb-rows
                          :above-partition (%o cols)
                          :above-mode (%o (* 2 cols))
                          :above-y-nnz (%o (* 16 sb-cols))
                          :above-uv-nnz (vector (%o (* 8 sb-cols)) (%o (* 8 sb-cols)))
                          :above-skip (%o cols)
                          :above-txfm (%o cols)
                          :above-segpred (%o cols)
                          :above-intra (%o cols)
                          :above-comp (%o cols)
                          :above-ref (%o cols)
                          :above-filter (%o cols))))
    (setf (st-above-mv st) (make-array (list (* 2 cols) 2 2) :element-type '(signed-byte 32)
                                       :initial-element 0))
    (setf (st-mvref st) (make-array (* 6 (* 8 sb-rows) (* 8 sb-cols)) :element-type '(signed-byte 32)
                                    :initial-element -1)
          (st-segmap st) (%o (* (* 8 sb-rows) (* 8 sb-cols))))
    (setf (st-frame st) (make-frame-for h))
    (setf (st-lf-level st) (make-array (list (st-sb-cols st) 8 8) :element-type '(signed-byte 32)
                                       :initial-element 0)
          (st-lf-mask st) (make-array (list (st-sb-cols st) 2 2 8 4) :element-type '(signed-byte 32)
                                      :initial-element 0))
    (multiple-value-bind (lim mblim) (%filter-luts (h-sharpness h))
      (setf (st-lf-lim st) lim (st-lf-mblim st) mblim))
    (%init-filter-levels st h)
    (dotimes (p 3)
      (setf (aref (st-intra-row st) p)
            (%o (aref (fr-stride (st-frame st)) p))))
    (%init-quant st h)
    st))

(declaim (inline st-block-base))
(defun st-block-base (st plane)
  "Where the block being decoded begins in PLANE."
  (declare (type state st) (type fixnum plane))
  (let ((stride (aref (fr-stride (st-frame st)) plane)))
    (declare (type fixnum stride))
    (if (zerop plane)
        (+ (* 8 (st-row st) stride) (* 8 (st-col st)))
        (+ (* 4 (st-row st) stride) (* 4 (st-col st))))))

(defun %reset-above (st)
  "The above contexts, once per frame: a block in the top row has no neighbour above and the
   contexts must say so rather than hold the last frame's."
  (fill (st-above-partition st) 0)
  (fill (st-above-skip st) 0)
  ;; AT TWO DIFFERENT GRANULARITIES, and the array serves both: a key frame's mode context is per
  ;; four samples and an inter frame's is per eight, so the same array is filled to twice the length
  ;; for one and indexed half as far for the other.
  (if (or (h-keyframe (st-h st)) (h-intra-only (st-h st)))
      (fill (st-above-mode st) 2 :end (* 2 (st-cols st)))
      (fill (st-above-mode st) +nearestmv+ :end (st-cols st)))
  (fill (st-above-y-nnz st) 0)
  (fill (the octets (aref (st-above-uv-nnz st) 0)) 0)
  (fill (the octets (aref (st-above-uv-nnz st) 1)) 0)
  (fill (st-above-segpred st) 0)
  (fill (st-above-intra st) 0)
  (fill (st-above-txfm st) 0))

(defun %reset-left (st)
  "And the left contexts, at the start of every superblock row of every tile — which is what makes
   a tile independent of the one beside it."
  (fill (st-left-partition st) 0)
  (fill (st-left-skip st) 0)
  (if (or (h-keyframe (st-h st)) (h-intra-only (st-h st)))
      (fill (st-left-mode st) 2)
      (fill (st-left-mode st) +nearestmv+ :end 8))
  (fill (st-left-y-nnz st) 0)
  (fill (the octets (aref (st-left-uv-nnz st) 0)) 0)
  (fill (the octets (aref (st-left-uv-nnz st) 1)) 0)
  (fill (st-left-segpred st) 0)
  (fill (st-left-intra st) 0)
  (fill (st-left-txfm st) 0))

;;; ---- one block's modes ---------------------------------------------------------------------------

(declaim (inline %bw4 %bh4))
(defun %bw4 (bs) (aref +block-size-wh+ 1 bs 0))     ; in eight-sample units
(defun %bh4 (bs) (aref +block-size-wh+ 1 bs 1))

(defun %decode-mode (st)
  "The segment, the skip flag, the transform size, and then either the intra modes or the whole of
   the inter decision — which references, which mode, which filter, and the vectors (6.4.15).

   KEY FRAMES CODE THEIR INTRA MODES AGAINST FIXED TABLES chosen by the neighbouring blocks' modes,
   not against an adapting model — a key frame has no history and must decode on its own.  An intra
   block inside an INTER frame codes the same modes against adapting models instead, which is why
   the two paths look alike and share nothing."
  (let* ((h (st-h st)) (p (fp-p (st-fp st))) (c (st-c st))
         (bs (st-bs st)) (row (st-row st)) (col (st-col st)) (row7 (st-row7 st))
         (max-tx (aref +max-tx-for-bs+ bs))
         (bw4 (%bw4 bs)) (bh4 (%bh4 bs))
         (w4 (min (- (st-cols st) col) bw4))
         (h4 (min (- (st-rows st) row) bh4))
         (keyish (or (h-keyframe h) (h-intra-only h)))
         (have-a (plusp row)) (have-l (> col (st-tile-col-start st)))
         (vref 0))
    (declare (type fixnum bs row col row7 max-tx bw4 bh4 w4 h4 vref))
    ;; ---- what this block may address, which bounds every predicted vector
    (setf (st-min-mvx st) (- (+ 128 (* col 64)))
          (st-min-mvy st) (- (+ 128 (* row 64)))
          (st-max-mvx st) (+ 128 (* (- (st-cols st) col bw4) 64))
          (st-max-mvy st) (+ 128 (* (- (st-rows st) row bh4) 64)))
    ;; ---- segment
    (setf (st-seg-id st) (%decode-segment st h c keyish w4 h4 bw4 bh4))
    ;; ---- skip: a segment may force it, and otherwise it is coded against the two neighbours
    (setf (st-skip st) (plusp (aref (st-seg-skip st) (st-seg-id st))))
    (unless (st-skip st)
      (let* ((k (+ (aref (st-left-skip st) row7) (aref (st-above-skip st) col)))
             (bit (bool-bit c (aref (pr-skip p) k))))
        (declare (type fixnum k bit))
        (incf (aref (cn-skip (st-counts st)) k bit))
        (setf (st-skip st) (plusp bit))))
    ;; ---- intra or inter
    (setf (st-intra st)
          (cond (keyish t)
                ((and (h-seg-enabled h) (plusp (aref (h-seg-feature-on h) (st-seg-id st) 2)))
                 (zerop (aref (h-seg-feature h) (st-seg-id st) 2)))
                (t (let ((k (if (and have-a have-l)
                                (let ((v (+ (aref (st-above-intra st) col)
                                            (aref (st-left-intra st) row7))))
                                  (declare (type fixnum v))
                                  (+ v (if (= v 2) 1 0)))
                                (cond (have-a (* 2 (aref (st-above-intra st) col)))
                                      (have-l (* 2 (aref (st-left-intra st) row7)))
                                      (t 0)))))
                     (declare (type fixnum k))
                     (let ((bit (bool-bit c (aref (pr-intra p) k))))
                       (declare (type fixnum bit))
                       (incf (aref (cn-intra (st-counts st)) k bit))
                       (zerop bit))))))
    ;; ---- transform size, whose context is `did my neighbours use a big one'
    (if (and (or (st-intra st) (not (st-skip st))) (= (fp-tx-mode (st-fp st)) +tx-switchable+))
        (let ((k (cond ((and have-a have-l)
                        (if (> (+ (if (plusp (aref (st-above-skip st) col))
                                      max-tx (aref (st-above-txfm st) col))
                                  (if (plusp (aref (st-left-skip st) row7))
                                      max-tx (aref (st-left-txfm st) row7)))
                               max-tx)
                            1 0))
                       (have-a (if (plusp (aref (st-above-skip st) col))
                                   1 (if (> (* 2 (aref (st-above-txfm st) col)) max-tx) 1 0)))
                       (have-l (if (plusp (aref (st-left-skip st) row7))
                                   1 (if (> (* 2 (aref (st-left-txfm st) row7)) max-tx) 1 0)))
                       (t 1))))
          (declare (type fixnum k))
          (setf (st-tx st)
                (case max-tx
                  (3 (let ((v (bool-bit c (aref (pr-tx32p p) k 0))))
                       (when (plusp v)
                         (incf v (bool-bit c (aref (pr-tx32p p) k 1)))
                         (when (= v 2) (incf v (bool-bit c (aref (pr-tx32p p) k 2)))))
                       (incf (aref (cn-tx32p (st-counts st)) k v))
                       v))
                  (2 (let ((v (bool-bit c (aref (pr-tx16p p) k 0))))
                       (when (plusp v) (incf v (bool-bit c (aref (pr-tx16p p) k 1))))
                       (incf (aref (cn-tx16p (st-counts st)) k v))
                       v))
                  (1 (let ((v (bool-bit c (aref (pr-tx8p p) k))))
                       (incf (aref (cn-tx8p (st-counts st)) k v))
                       v))
                  (t 0))))
        (setf (st-tx st) (min max-tx (fp-tx-mode (st-fp st)))))
    ;; ---- the modes themselves
    (cond
      (keyish (%decode-kf-modes st c bs row7 col))
      ((st-intra st)
       (setf (st-bcomp st) nil)
       (let ((m (st-mode st)))
         (if (> bs +bs-8x8+)
             (progn
               (macrolet ((ym () '(let ((v (bool-tree c +intramode-tree+ (pr-y-mode p) 0)))
                                    (incf (aref (cn-y-mode (st-counts st)) 0 v)) v)))
                 (setf (aref m 0) (ym))
                 (setf (aref m 1) (if (/= bs +bs-8x4+) (ym) (aref m 0)))
                 (if (/= bs +bs-4x8+)
                     (progn
                       (setf (aref m 2) (ym))
                       (setf (aref m 3) (if (/= bs +bs-8x4+) (ym) (aref m 2))))
                     (setf (aref m 2) (aref m 0) (aref m 3) (aref m 1)))))
             (let* ((sz (aref +size-group+ bs))
                    (v (bool-tree c +intramode-tree+ (pr-y-mode p) (* 9 sz))))
               (declare (type fixnum sz v))
               (incf (aref (cn-y-mode (st-counts st)) sz v))
               (dotimes (i 4) (setf (aref m i) v))))
         (setf (st-uvmode st)
               (bool-tree c +intramode-tree+ (pr-uv-mode p) (* 9 (aref m 3))))
         (incf (aref (cn-uv-mode (st-counts st)) (aref m 3) (st-uvmode st)))))
      (t (setf vref (%decode-inter-mode st h p c bs row row7 col have-a have-l))))
    ;; ---- and what this block leaves for its neighbours.  THE FULL BLOCK WIDTH, not the part
    ;; inside the picture: a block that hangs over the edge still writes the context a block below
    ;; it would read, and clipping here would make the two disagree.
    (%splat (st-above-skip st) col bw4 (if (st-skip st) 1 0))
    (%splat (st-left-skip st) row7 bh4 (if (st-skip st) 1 0))
    (%splat (st-above-txfm st) col bw4 (st-tx st))
    (%splat (st-left-txfm st) row7 bh4 (st-tx st))
    (%splat (st-above-partition st) col bw4 (aref +above-ctx-for-bs+ bs))
    (%splat (st-left-partition st) row7 bh4 (aref +left-ctx-for-bs+ bs))
    (cond
      (keyish
       (%splat (st-above-intra st) col bw4 1)
       (%splat (st-left-intra st) row7 bh4 1))
      (t
       (%splat (st-above-intra st) col bw4 (if (st-intra st) 1 0))
       (%splat (st-left-intra st) row7 bh4 (if (st-intra st) 1 0))
       (%splat (st-above-comp st) col bw4 (if (st-bcomp st) 1 0))
       (%splat (st-left-comp st) row7 bh4 (if (st-bcomp st) 1 0))
       (%splat (st-above-mode st) col bw4 (aref (st-mode st) 3))
       (%splat (st-left-mode st) row7 bh4 (aref (st-mode st) 3))
       (unless (st-intra st)
         (%splat (st-above-ref st) col bw4 vref)
         (%splat (st-left-ref st) row7 bh4 vref)
         (when (h-filter-switchable h)
           (%splat (st-above-filter st) col bw4 (st-bfilter-id st))
           (%splat (st-left-filter st) row7 bh4 (st-bfilter-id st))))
       (%store-mv-contexts st bs row7 col bw4 bh4)))
    ;; ---- and the block's reference and vector, for the neighbours of the blocks after it
    (%store-mvref st row col w4 h4)))

(defun %decode-segment (st h c keyish w4 h4 bw4 bh4)
  "Which of the eight segments this block belongs to (6.4.16).

   An inter frame may PREDICT the segment from the co-located blocks of the previous frame rather
   than code it — and when it does, the predicted value is the SMALLEST of them, not the most
   common, because a segment number is an index into a table of adjustments and the conservative
   choice is the lowest."
  (declare (type state st) (type fixnum w4 h4 bw4 bh4) (optimize (speed 3) (safety 1)))
  (let ((row (st-row st)) (col (st-col st)) (row7 (st-row7 st))
        (stride (* 8 (st-sb-cols st)))
        (id 0))
    (declare (type fixnum row col row7 stride id))
    (flet ((predicted ()
             ;; the block's segment number as the last frame had it, which is the MINIMUM over the
             ;; area it covers — a block may straddle several of the previous frame's
             (if (and (not (h-error-resilient h)) (st-segmap-prev st))
                 (let ((pred 8) (prev (the octets (st-segmap-prev st))))
                   (declare (type fixnum pred))
                   (dotimes (y h4 pred)
                     (let ((base (+ (* (+ y row) stride) col)))
                       (declare (type fixnum base))
                       (dotimes (x w4)
                         (setf pred (min pred (aref prev (+ base x))))))))
                 0))
           (write-map (v)
             (let ((map (st-segmap st)))
               (dotimes (y bh4)
                 (let ((base (+ (* (+ y row) stride) col)))
                   (declare (type fixnum base))
                   (dotimes (x bw4)
                     (when (< (+ base x) (length map))
                       (setf (aref map (+ base x)) v))))))))
      (cond
        ((not (h-seg-enabled h)) (setf id 0))
        (keyish
         (setf id (if (h-seg-update-map h)
                      (bool-tree c +segmentation-tree+ (h-seg-tree-probs h) 0)
                      0))
         (write-map id))
        ((not (h-seg-update-map h))
         ;; A FRAME THAT DOES NOT UPDATE THE MAP STILL HAS ONE, and it is the last frame's, copied
         ;; forward BLOCK BY BLOCK rather than filled with this block's number.  Writing nothing —
         ;; which is what happens if you think "no update means no work" — leaves the current map
         ;; all zeros, and the frame after this one predicts from zeros.
         (setf id (predicted))
         (let ((map (st-segmap st)) (prev (and (st-segmap-prev st)
                                               (the octets (st-segmap-prev st)))))
           (dotimes (y bh4)
             (let ((base (+ (* (+ y row) stride) col)))
               (declare (type fixnum base))
               (dotimes (x bw4)
                 (when (< (+ base x) (length map))
                   (setf (aref map (+ base x)) (if prev (aref prev (+ base x)) 0))))))))
        ((h-seg-temporal h)
         ;; only the temporal branch touches the prediction contexts, because only it codes a bit
         ;; they are the context for
         (let ((predp (plusp (bool-bit c (aref (h-seg-pred-probs h)
                                               (+ (aref (st-above-segpred st) col)
                                                  (aref (st-left-segpred st) row7)))))))
           (setf id (if predp
                        (predicted)
                        (bool-tree c +segmentation-tree+ (h-seg-tree-probs h) 0)))
           (%splat (st-above-segpred st) col w4 (if predp 1 0))
           (%splat (st-left-segpred st) row7 h4 (if predp 1 0)))
         (write-map id))
        (t (setf id (bool-tree c +segmentation-tree+ (h-seg-tree-probs h) 0))
           (write-map id))))
    id))

(defun %decode-kf-modes (st c bs row7 col)
  "A key frame's intra modes, against the fixed tables its neighbours' modes select."
  (declare (type state st) (type fixnum bs row7 col))
  (let ((a (st-above-mode st)) (ao (* 2 col))
        (l (st-left-mode st)) (lo (* 2 row7))
        (m (st-mode st)))
    (if (> bs +bs-8x8+)
        (progn
          (setf (aref m 0) (%kf-ymode c (aref a ao) (aref l lo))
                (aref a ao) (aref m 0))
          (if (/= bs +bs-8x4+)
              (setf (aref m 1) (%kf-ymode c (aref a (1+ ao)) (aref m 0))
                    (aref l lo) (aref m 1)
                    (aref a (1+ ao)) (aref m 1))
              (setf (aref m 1) (aref m 0)
                    (aref l lo) (aref m 0)
                    (aref a (1+ ao)) (aref m 0)))
          (if (/= bs +bs-4x8+)
              (progn
                (setf (aref m 2) (%kf-ymode c (aref a ao) (aref l (1+ lo)))
                      (aref a ao) (aref m 2))
                (if (/= bs +bs-8x4+)
                    (setf (aref m 3) (%kf-ymode c (aref a (1+ ao)) (aref m 2))
                          (aref l (1+ lo)) (aref m 3)
                          (aref a (1+ ao)) (aref m 3))
                    (setf (aref m 3) (aref m 2)
                          (aref l (1+ lo)) (aref m 2)
                          (aref a (1+ ao)) (aref m 2))))
              (setf (aref m 2) (aref m 0)
                    (aref m 3) (aref m 1)
                    (aref l (1+ lo)) (aref m 1)
                    (aref a (1+ ao)) (aref m 1))))
        (let ((v (%kf-ymode c (aref a ao) (aref l lo))))
          (dotimes (i 4) (setf (aref m i) v))
          (fill a v :start ao :end (min (length a) (+ ao (aref +block-size-wh+ 0 bs 0))))
          (fill l v :start lo :end (min (length l) (+ lo (aref +block-size-wh+ 0 bs 1))))))
    (setf (st-uvmode st) (bool-tree c +intramode-tree+ +default-kf-uvmode+ (* 9 (aref m 3))))))

(declaim (inline %kf-ymode %splat))
(defun %kf-ymode (c above left)
  "A key frame's luma mode, against the table for this pair of neighbouring modes."
  (declare (type fixnum above left))
  (bool-tree c +intramode-tree+ +default-kf-ymode+ (* 9 (+ (* 10 above) left))))

(defun %splat (arr at n v)
  (declare (type octets arr) (type fixnum at n v))
  (fill arr v :start at :end (min (length arr) (+ at n))))

;;; ---- one transform block's coefficients ----------------------------------------------------------

(defun %decode-coeffs-block (st probs pbase nnz scan nb bands qdc qac out base n-coeffs is32
                             tx plane inter)
  "One transform block, from its first coefficient to its end-of-block (6.4.24).

   Every coefficient is coded against a probability chosen by TWO things: which BAND of the scan it
   sits in — the first few positions get a band each, because low frequencies differ most from one
   another — and how many non-zero coefficients its two already-scanned neighbours had.  The second
   is why a scan order and a neighbour table have to travel together.

   Returns the number of coefficients decoded, which is what the reconstruction needs to know how
   much of the block to transform, and what the NEXT block's context is."
  (declare (type state st)
           (type (simple-array (unsigned-byte 8) (4 2 2 6 6 11)) probs)
           (type fixnum pbase nnz base n-coeffs qdc qac tx plane inter)
           (type (simple-array (unsigned-byte 16) (*)) scan)
           (type (simple-array (unsigned-byte 16) (* 2)) nb)
           (type (simple-array (signed-byte 32) (*)) bands)
           (type coefs out)
           (optimize (speed 3) (safety 1)))
  (let* ((c (st-c st)) (cnt (st-counts st))
         (cache (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0))
         (i 0) (band 0) (band-left (aref bands 0)))
    (declare (type fixnum i band band-left) (dynamic-extent cache))
    (macrolet ((tp (k) `(row-major-aref probs (+ pbase (* 66 band) (* 11 nnz) ,k))))
      (loop
        ;; ---- is this the end of the block?
        (let ((v (bool-bit c (tp 0))))
          (declare (type fixnum v))
          (incf (aref (cn-eob cnt) tx plane inter band nnz v))
          (when (zerop v) (return)))
        ;; ---- runs of zeros, each still costing a decision but not a value
        (loop
          (when (plusp (bool-bit c (tp 1))) (return))
          (incf (aref (cn-coef cnt) tx plane inter band nnz 0))
          (setf (aref cache (aref scan i)) 0)
          (when (zerop (decf band-left)) (setf band-left (aref bands (incf band))))
          (setf nnz (ash (+ 1 (aref cache (aref nb i 0)) (aref cache (aref nb i 1))) -1))
          (incf i)
          ;; a well formed block ends with an end-of-block token, never by running out
          (when (>= i n-coeffs) (return-from %decode-coeffs-block i)))
        (let ((rc (aref scan i)) (val 0))
          (declare (type fixnum rc val))
          (if (zerop (bool-bit c (tp 2)))
              (progn (incf (aref (cn-coef cnt) tx plane inter band nnz 1))
                     (setf val 1 (aref cache rc) 1))
              (progn
                (incf (aref (cn-coef cnt) tx plane inter band nnz 2))
                (if (zerop (bool-bit c (tp 3)))
                    (if (zerop (bool-bit c (tp 4)))
                        (setf val 2 (aref cache rc) 2)
                        (setf val (+ 3 (bool-bit c (tp 5))) (aref cache rc) 3))
                    (if (zerop (bool-bit c (tp 6)))
                        (progn
                          (setf (aref cache rc) 4)
                          (if (zerop (bool-bit c (tp 7)))
                              (setf val (+ 5 (bool-bit c 159)))
                              (setf val (+ 7 (ash (bool-bit c 165) 1) (bool-bit c 145)))))
                        (progn
                          (setf (aref cache rc) 5)
                          (cond
                            ((zerop (bool-bit c (tp 8)))
                             (if (zerop (bool-bit c (tp 9)))
                                 (setf val (+ 11 (ash (bool-bit c 173) 2)
                                              (ash (bool-bit c 148) 1) (bool-bit c 140)))
                                 (setf val (+ 19 (ash (bool-bit c 176) 3)
                                              (ash (bool-bit c 155) 2)
                                              (ash (bool-bit c 140) 1) (bool-bit c 135)))))
                            ((zerop (bool-bit c (tp 10)))
                             (setf val (+ 35 (ash (bool-bit c 180) 4) (ash (bool-bit c 157) 3)
                                          (ash (bool-bit c 141) 2) (ash (bool-bit c 134) 1)
                                          (bool-bit c 130))))
                            (t
                             ;; the last escape: fourteen bits, each with its own probability,
                             ;; which is a Golomb code fitted to the distribution rather than
                             ;; derived from it
                             (setf val (+ 67 (ash (bool-bit c 254) 13) (ash (bool-bit c 254) 12)
                                          (ash (bool-bit c 254) 11) (ash (bool-bit c 252) 10)
                                          (ash (bool-bit c 249) 9) (ash (bool-bit c 243) 8)
                                          (ash (bool-bit c 230) 7) (ash (bool-bit c 196) 6)
                                          (ash (bool-bit c 177) 5) (ash (bool-bit c 153) 4)
                                          (ash (bool-bit c 140) 3) (ash (bool-bit c 133) 2)
                                          (ash (bool-bit c 130) 1) (bool-bit c 129))))))))))
          (when (zerop (decf band-left)) (setf band-left (aref bands (incf band))))
          (let* ((q (if (zerop i) qdc qac))
                 (signed (if (plusp (bool-flag c)) (- val) val)))
            (declare (type fixnum q signed))
            ;; A 32x32 TRANSFORM'S COEFFICIENTS ARE HALVED, because its own scaling is one bit
            ;; larger than the others' and the inverse transform expects them all in one range.
            (setf (aref out (+ base rc))
                  (if is32 (truncate (* signed q) 2) (* signed q))))
          (setf nnz (ash (+ 1 (aref cache (aref nb i 0)) (aref cache (aref nb i 1))) -1))
          (incf i)
          (when (>= i n-coeffs) (return)))))
    i))

(declaim (inline %merge-ctx %splat-ctx))
(defun %merge-ctx (a at end step)
  "Collapse STEP neighbouring 4x4 contexts into one, which is what a larger transform reads."
  (declare (type octets a) (type fixnum at end step))
  (loop for n of-type fixnum from 0 below end by step
        do (let ((any 0))
             (declare (type fixnum any))
             (loop for k of-type fixnum from n below (min end (+ n step))
                   do (setf any (logior any (aref a (+ at k)))))
             (setf (aref a (+ at n)) (if (plusp any) 1 0)))))

(defun %splat-ctx (a at end step)
  "And spread it back out again, so that a smaller transform beside it reads the same answer."
  (declare (type octets a) (type fixnum at end step))
  (loop for n of-type fixnum from 0 below end by step
        do (let ((v (aref a (+ at n))))
             (loop for k of-type fixnum from (1+ n) below (min end (+ n step))
                   do (setf (aref a (+ at k)) v)))))

(defun %decode-coeffs (st)
  "Every transform block of one block: luma first, then the two chroma planes.

   The transform size is a property of the BLOCK, so a 32x32 block with an 8x8 transform holds
   sixteen transform blocks and they are visited in raster order within it."
  (let* ((h (st-h st)) (fp (st-fp st)) (p (pr-coef (fp-p fp)))
         (bs (st-bs st)) (row (st-row st)) (col (st-col st))
         (tx (st-tx st)) (uvtx (st-uvtx st))
         (inter (if (st-intra st) 0 1))
         (w4 (ash (%bw4 bs) 1)) (h4 (ash (%bh4 bs) 1))
         (end-x (min (* 2 (- (st-cols st) col)) w4))
         (end-y (min (* 2 (- (st-rows st) row)) h4))
         (seg (st-seg-id st))
         (any nil)
         ;; a lossless frame scans and transforms as if 4x4 whatever the block says
         (tx-index (if (h-lossless h) 4 tx)))
    (declare (type fixnum bs row col tx uvtx inter w4 h4 end-x end-y seg tx-index))
    (macrolet ((band-row (which) `(let ((b (make-array 8 :element-type '(signed-byte 32))))
                                    (dotimes (k 8 b) (setf (aref b k) (aref +band-counts+ ,which k))))))
      (let ((ybands (band-row tx)) (uvbands (band-row uvtx)))
        ;; ---- luma
        (let* ((a (st-above-y-nnz st)) (ao (* 2 col))
               (l (st-left-y-nnz st)) (lo (* 2 (st-row7 st)))
               (step (ash 1 tx))
               (qdc (aref (st-qmul st) seg 0 0)) (qac (aref (st-qmul st) seg 0 1))
               (n 0))
          (declare (type fixnum ao lo step qdc qac n))
          (when (> step 1)
            (%merge-ctx l lo end-y step) (%merge-ctx a ao end-x step))
          (loop for y of-type fixnum from 0 below end-y by step
                do (loop for x of-type fixnum from 0 below end-x by step
                         do (let* ((mi (if (> bs +bs-8x8+) n 0))
                                   (txtp (aref +intra-txfm-type+ (aref (st-mode st) (min mi 3))))
                                   (scan (aref (the simple-vector (aref +scans+ tx-index)) txtp))
                                   (nb (aref (the simple-vector (aref +scans-nb+ tx-index)) txtp))
                                   (got (%decode-coeffs-block
                                         st p (%coef-base tx 0 inter)
                                         (+ (aref a (+ ao x)) (aref l (+ lo y)))
                                         scan nb ybands qdc qac
                                         (st-coeffs st) (* 16 n) (* 16 step step) (= tx 3)
                                         tx 0 inter)))
                              (declare (type fixnum mi txtp got))
                              (setf (aref a (+ ao x)) (if (plusp got) 1 0)
                                    (aref l (+ lo y)) (if (plusp got) 1 0))
                              (when (plusp got) (setf any t))
                              (setf (aref (st-eob st) n) got)
                              (incf n (* step step)))))
          (when (> step 1)
            (%splat-ctx l lo end-y step) (%splat-ctx a ao end-x step)))
        ;; ---- and the two chroma planes, at half the resolution in each direction
        (let ((cw4 (ash w4 -1)) (cex (ash end-x -1)) (ceh (ash end-y -1))
              (step (ash 1 uvtx))
              (qdc (aref (st-qmul st) seg 1 0)) (qac (aref (st-qmul st) seg 1 1))
              (scan (aref (the simple-vector (aref +scans+ uvtx)) 0))
              (nb (aref (the simple-vector (aref +scans-nb+ uvtx)) 0)))
          (declare (type fixnum cw4 cex ceh step qdc qac) (ignorable cw4))
          (dotimes (pl 2)
            (let ((a (the octets (aref (st-above-uv-nnz st) pl))) (ao col)
                  (l (the octets (aref (st-left-uv-nnz st) pl))) (lo (st-row7 st))
                  (out (the coefs (aref (st-uvcoeffs st) pl)))
                  (eobs (the (simple-array (signed-byte 32) (*)) (aref (st-uveob st) pl)))
                  (n 0))
              (declare (type fixnum ao lo n))
              (when (> step 1)
                (%merge-ctx l lo ceh step) (%merge-ctx a ao cex step))
              (loop for y of-type fixnum from 0 below ceh by step
                    do (loop for x of-type fixnum from 0 below cex by step
                             do (let ((got (%decode-coeffs-block
                                            st p (%coef-base uvtx 1 inter)
                                            (+ (aref a (+ ao x)) (aref l (+ lo y)))
                                            scan nb uvbands qdc qac
                                            out (* 16 n) (* 16 step step) (= uvtx 3)
                                            uvtx 1 inter)))
                                  (declare (type fixnum got))
                                  (setf (aref a (+ ao x)) (if (plusp got) 1 0)
                                        (aref l (+ lo y)) (if (plusp got) 1 0))
                                  (when (plusp got) (setf any t))
                                  (setf (aref eobs n) got)
                                  (incf n (* step step)))))
              (when (> step 1)
                (%splat-ctx l lo ceh step) (%splat-ctx a ao cex step)))))))
    any))

(declaim (inline %coef-base))
(defun %coef-base (tx plane inter)
  "Row-major index of coef[tx][plane][inter] in the (4 2 2 6 6 11) probability array."
  (declare (type fixnum tx plane inter))
  (* 396 (+ (* 4 tx) (* 2 plane) inter)))

;;; ---- the partition quadtree --------------------------------------------------------------------

(defun %decode-block (st row col bl bp)
  "One block of the partition: its modes, and its coefficients unless it is marked skipped."
  (let* ((bs (+ (* 3 bl) bp)))
    (declare (type fixnum bs))
    (setf (st-row st) row (st-col st) col (st-row7 st) (logand row 7)
          (st-bs st) bs (st-bl st) bl (st-bp st) bp)
    (%decode-mode st)
    ;; THE CHROMA TRANSFORM IS ONE SIZE SMALLER when the block is exactly as wide or as tall as its
    ;; own transform, because halving the resolution halves the room
    (let ((w4 (%bw4 bs)) (h4 (%bh4 bs)))
      (declare (type fixnum w4 h4))
      (setf (st-uvtx st)
            (- (st-tx st)
               (if (or (= (* 2 w4) (ash 1 (st-tx st))) (= (* 2 h4) (ash 1 (st-tx st)))) 1 0))))
    (if (st-skip st)
        (let ((w4 (%bw4 bs)) (h4 (%bh4 bs)))
          ;; a skipped block has no coefficients, and says so to its neighbours
          (%splat (st-above-y-nnz st) (* 2 col) (* 2 w4) 0)
          (%splat (st-left-y-nnz st) (* 2 (st-row7 st)) (* 2 h4) 0)
          (dotimes (pl 2)
            (%splat (aref (st-above-uv-nnz st) pl) col w4 0)
            (%splat (aref (st-left-uv-nnz st) pl) (st-row7 st) h4 0)))
        (%decode-coeffs st))
    (if (st-intra st) (%intra-recon st) (%inter-recon st))
    ;; and record which of this block's edges the filter will visit, and how wide
    (let ((lvl (aref (st-lf-lvl st) (st-seg-id st)
                     (if (st-intra st) 0 (1+ (aref (st-bref st) 0)))
                     (if (/= (aref (st-mode st) 3) +zeromv+) 1 0))))
      (declare (type fixnum lvl))
      (when (and (plusp (h-filter-level (st-h st))) (plusp lvl))
        (let* ((bs (st-bs st))
               (sb (ash (st-col st) -3))
               (w4 (%bw4 bs)) (h4 (%bh4 bs))
               (x-end (min (- (st-cols st) (st-col st)) w4))
               (y-end (min (- (st-rows st) (st-row st)) h4))
               (row7 (st-row7 st)) (col7 (logand (st-col st) 7))
               ;; A SKIPPED INTER BLOCK HAS NO TRANSFORM EDGES INSIDE IT, only its own boundary:
               ;; there is no residual, so nothing was transformed and nothing needs smoothing.
               (skip-inter (and (not (st-intra st)) (st-skip st))))
          (declare (type fixnum bs sb w4 h4 x-end y-end row7 col7))
          (dotimes (dy h4)
            (dotimes (dx w4)
              (when (and (< (+ row7 dy) 8) (< (+ col7 dx) 8))
                (setf (aref (st-lf-level st) sb (+ row7 dy) (+ col7 dx)) lvl))))
          (%mask-edges (st-lf-mask st) sb 0 0 0 row7 col7 x-end y-end 0 0 (st-tx st) skip-inter)
          (%mask-edges (st-lf-mask st) sb 1 1 1 row7 col7 x-end y-end
                       (if (and (logbitp 0 (st-cols st))
                                (>= (+ (st-col st) w4) (st-cols st)))
                           (logand (st-cols st) 7) 0)
                       (if (and (logbitp 0 (st-rows st))
                                (>= (+ (st-row st) h4) (st-rows st)))
                           (logand (st-rows st) 7) 0)
                       (st-uvtx st) skip-inter))))
    (incf (st-blocks st))))

(defun %init-filter-levels (st h)
  "The filter level for every combination of segment, reference frame and zero-or-not vector (6.2.9).

   The deltas are multiplied by TWO once the frame level reaches thirty-two, which is the format
   saying that a strong filter should be adjusted in bigger steps than a weak one.  And intra blocks
   take the reference-zero delta with no mode delta at all, which is why their two entries are equal
   and the other three references' are not."
  (declare (type state st))
  (let ((sh (if (>= (h-filter-level h) 32) 1 0))
        (out (st-lf-lvl st)))
    (declare (type fixnum sh))
    (flet ((c6 (v) (max 0 (min 63 v))))
      (dotimes (i 8)
        (let ((lvl (h-filter-level h)))
          (declare (type fixnum lvl))
          (when (and (h-seg-enabled h) (plusp (aref (h-seg-feature-on h) i 1)))
            (setf lvl (c6 (if (h-seg-abs h)
                              (aref (h-seg-feature h) i 1)
                              (+ (h-filter-level h) (aref (h-seg-feature h) i 1))))))
          (if (h-lf-delta-enabled h)
              (progn
                (let ((v (c6 (+ lvl (* (aref (h-lf-ref-delta h) 0) (ash 1 sh))))))
                  (setf (aref out i 0 0) v (aref out i 0 1) v))
                (loop for j of-type fixnum from 1 to 3
                      do (setf (aref out i j 0)
                               (c6 (+ lvl (* (+ (aref (h-lf-ref-delta h) j)
                                                (aref (h-lf-mode-delta h) 0))
                                             (ash 1 sh))))
                               (aref out i j 1)
                               (c6 (+ lvl (* (+ (aref (h-lf-ref-delta h) j)
                                                (aref (h-lf-mode-delta h) 1))
                                             (ash 1 sh)))))))
              (dotimes (j 4) (setf (aref out i j 0) lvl (aref out i j 1) lvl))))))))

(defun %decode-sb (st row col bl)
  "One node of the quadtree (6.4.4).

   The three cases that are not a plain four-way choice are all about the picture's edge.  A block
   whose right half falls outside the picture cannot be left whole or split vertically, so only one
   bit is read and it says `split' or `horizontal'; the same the other way for the bottom; and a
   block outside on both sides is split with no bit read at all."
  (declare (type fixnum row col bl))
  (let* ((c (st-c st))
         (k (logior (logand (ash (aref (st-above-partition st) col) (- (- 3 bl))) 1)
                    (ash (logand (ash (aref (st-left-partition st) (logand row 7)) (- (- 3 bl))) 1)
                         1)))
         (probs (if (or (h-keyframe (st-h st)) (h-intra-only (st-h st)))
                    +default-kf-partition+
                    (pr-partition (fp-p (st-fp st)))))
         (base (* 3 (+ (* 4 bl) k)))
         (hbs (ash 4 (- bl))))
    (declare (type fixnum k base hbs))
    (macrolet ((count-bp (bp) `(incf (aref (cn-partition (st-counts st)) bl k ,bp))))
     (cond
      ((= bl 3)
       (let ((bp (bool-tree c +partition-tree+ probs base)))
         (count-bp bp)
         (%decode-block st row col bl bp)))
      ((< (+ col hbs) (st-cols st))
       (if (< (+ row hbs) (st-rows st))
           (let ((bp (bool-tree c +partition-tree+ probs base)))
             (declare (type fixnum bp))
             (count-bp bp)
             (case bp
               (0 (%decode-block st row col bl bp))
               (1 (%decode-block st row col bl bp)
                  (%decode-block st (+ row hbs) col bl bp))
               (2 (%decode-block st row col bl bp)
                  (%decode-block st row (+ col hbs) bl bp))
               (t (%decode-sb st row col (1+ bl))
                  (%decode-sb st row (+ col hbs) (1+ bl))
                  (%decode-sb st (+ row hbs) col (1+ bl))
                  (%decode-sb st (+ row hbs) (+ col hbs) (1+ bl)))))
           ;; the bottom half is outside: split, or one wide block
           (if (plusp (bool-bit c (row-major-aref probs (+ base 1))))
               (progn (count-bp 3)
                      (%decode-sb st row col (1+ bl))
                      (%decode-sb st row (+ col hbs) (1+ bl)))
               (progn (count-bp 1) (%decode-block st row col bl 1)))))
      ((< (+ row hbs) (st-rows st))
       ;; the right half is outside: split, or one tall block
       (if (plusp (bool-bit c (row-major-aref probs (+ base 2))))
           (progn (count-bp 3)
                  (%decode-sb st row col (1+ bl))
                  (%decode-sb st (+ row hbs) col (1+ bl)))
           (progn (count-bp 2) (%decode-block st row col bl 2))))
      (t (count-bp 3) (%decode-sb st row col (1+ bl)))))))

;;; ---- tiles ---------------------------------------------------------------------------------------

(declaim (inline %tile-bounds))
(defun %tile-bounds (idx log2-n n)
  "Where tile IDX of 2^LOG2-N begins and ends, in eight-sample units."
  (declare (type fixnum idx log2-n n))
  (values (ash (min n (ash (* idx n) (- log2-n))) 3)
          (ash (min n (ash (* (1+ idx) n) (- log2-n))) 3)))

(defun decode-tiles (bytes start end st)
  "Every tile of one frame's tile data.

   Every tile but the last carries its length in FOUR BYTES in front of it, big endian, and the last
   simply runs to the end of the frame — which is what lets a decoder find the tiles without
   decoding any of them, and is the same reasoning that put the frame header in plain bits."
  (declare (type octets bytes) (type fixnum start end))
  (let* ((h (st-h st))
         (rows (ash 1 (h-log2-tile-rows h)))
         (cols (ash 1 (h-log2-tile-cols h)))
         (at start)
         (coders (make-array cols)))
    (declare (type fixnum rows cols at))
    (%reset-above st)
    (dotimes (tr rows)
      (multiple-value-bind (row-start row-end)
          (%tile-bounds tr (h-log2-tile-rows h) (st-sb-rows st))
        ;; EVERY TILE COLUMN'S CODER IS OPENED FIRST, because the rows are then decoded across all
        ;; of them: the frame is walked a superblock row at a time, and each row visits every tile
        ;; column in turn.  Decoding a whole tile column before starting the next would read the
        ;; same bits — each tile has its own coder — but would put the loop filter and the saved
        ;; intra row a whole tile out of step, since both are frame-wide and per superblock row.
        (dotimes (tc cols)
          (let ((size (if (and (= tr (1- rows)) (= tc (1- cols)))
                          (- end at)
                          (progn
                            (when (> (+ at 4) end) (%err "a tile length past the end of a frame"))
                            (prog1 (logior (ash (aref bytes at) 24) (ash (aref bytes (1+ at)) 16)
                                           (ash (aref bytes (+ at 2)) 8) (aref bytes (+ at 3)))
                              (incf at 4))))))
            (declare (type fixnum size))
            (when (or (minusp size) (> (+ at size) end))
              (%err "a tile of ~d bytes with ~d left in the frame" size (- end at)))
            (setf (aref coders tc) (make-bool bytes at (+ at size)))
            (when (plusp (bool-flag (aref coders tc)))
              (%err "a tile whose marker bit is set"))
            (incf at size)))
        (loop for row of-type fixnum from row-start below row-end by 8
              do (dotimes (sb (st-sb-cols st))
                   (dotimes (i 2) (dotimes (j 2) (dotimes (y 8) (dotimes (k 4)
                     (setf (aref (st-lf-mask st) sb i j y k) 0))))))
                 (dotimes (tc cols)
                   (multiple-value-bind (col-start col-end)
                       (%tile-bounds tc (h-log2-tile-cols h) (st-sb-cols st))
                     ;; IN EIGHT-SAMPLE UNITS, the same as COL, because that is what the
                     ;; availability test compares it against.  Storing it in superblocks makes a
                     ;; block at the left edge of a tile believe it has a neighbour.
                     (setf (st-c st) (aref coders tc)
                           (st-tile-col-start st) col-start
                           (st-tile-col-end st) col-end)
                     (%reset-left st)
                     (loop for col of-type fixnum from col-start below col-end by 8
                           do (%decode-sb st row col 0))))
                 (%save-intra-row st row)
                 (when (plusp (h-filter-level h))
                   (loop for col of-type fixnum from 0 below (st-cols st) by 8
                         do (%filter-superblock st (ash col -3) col row))))
        (dotimes (tc cols)
          (setf (st-tile-slack st)
                (max (st-tile-slack st)
                     (abs (- (bd-end (aref coders tc)) (bd-pos (aref coders tc)))))))))
    at))

(defun %save-intra-row (st row)
  "Keep the last line of this superblock row for the intra prediction of the next one."
  (declare (type state st) (type fixnum row))
  (let ((f (st-frame st)))
    (when (< (+ row 8) (st-rows st))
      (dotimes (p 3)
        (let* ((stride (aref (fr-stride f) p))
               (plane (the octets (aref (fr-planes f) p)))
               (line (if (zerop p) (+ (* 8 row) 63) (+ (* 4 row) 31)))
               (o (* line stride)))
          (declare (type fixnum stride line o))
          (replace (the octets (aref (st-intra-row st) p)) plane
                   :start1 0 :end1 stride :start2 o :end2 (+ o stride)))))))

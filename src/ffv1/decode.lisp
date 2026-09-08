;;;; ffv1/decode.lisp — FFV1, the lossless codec archives keep masters in.
;;;;
;;;; NOTHING IN THIS FILE IS A TRANSFORM.  FFV1 has no DCT, no motion, no reference pictures at all
;;;; in its usual mode: every sample is predicted from its three already-decoded neighbours, and the
;;;; DIFFERENCE is entropy coded.  That is the whole codec, and it is why it is lossless and why
;;;; it is small.
;;;;
;;;; THE CONTEXT IS THE INTERESTING PART.  A residual is not coded against one probability model but
;;;; against one of several thousand, chosen by quantising the GRADIENTS around the sample — how
;;;; much it changes to the left, above, and diagonally.  A flat area and an edge get different
;;;; models, so the coder does not have to average them, and the quantisation tables that decide
;;;; which is which are transmitted rather than fixed.
;;;;
;;;; Also worth knowing before reading: a context and its NEGATION share a model.  If the gradients
;;;; come out negative the sign is remembered, the context is negated, and the residual is negated
;;;; on the way back — so half the models are saved for nothing but a sign flip.

(in-package #:reel.ffv1)

(defconstant +max-context-inputs+ 5)

(defstruct (config (:conc-name cfg-))
  (version 3 :type fixnum)
  (micro 0 :type fixnum)
  (ac 1 :type fixnum)                           ; 0 Golomb-Rice, 1 range coder, 2 custom table
  (colorspace 0 :type fixnum)                   ; 0 YCbCr, 1 RGB through a reversible transform
  (bits 8 :type fixnum)
  (chroma-planes nil)
  (chroma-h-shift 0 :type fixnum) (chroma-v-shift 0 :type fixnum)
  (transparency nil)
  (plane-count 1 :type fixnum)
  (h-slices 1 :type fixnum) (v-slices 1 :type fixnum)
  (quant-tables nil)                            ; vector of (5 x 256) fixnum arrays
  (context-counts nil)                          ; per quant table
  (initial-states nil)                          ; per quant table, or NIL
  (state-transition nil)                        ; a custom one-state table, or NIL for the default
  (ec 0 :type fixnum)
  (intra 0 :type fixnum)
  (width 0 :type fixnum) (height 0 :type fixnum))

(defun %read-quant-table (c scale table row)
  "One of the five gradient quantisers, run-length coded and then mirrored.

   Only the first 128 entries are sent; the rest are the negation of the first, which is the
   symmetry that lets a context and its negation share a model."
  (declare (type (simple-array fixnum (5 256)) table) (type fixnum scale row))
  ;; A FRESH STATE PER TABLE, not one shared across the five.  They are five independent run-length
  ;; codes that happen to be adjacent, and carrying the adaptation from one into the next reads the
  ;; second one against probabilities learned from the first.
  (let ((state (fresh-state)) (i 0) (v 0))
    (declare (type fixnum i v))
    (loop while (< i 128)
          do (let ((len (1+ (get-symbol c state nil))))
               (declare (type fixnum len))
               (when (or (> len (- 128 i)) (zerop len)) (%err "a quantiser table run of ~d" len))
               (dotimes (k len) (setf (aref table row i) (* scale v)) (incf i))
               (incf v)))
    (loop for k of-type fixnum from 1 below 128
          do (setf (aref table row (- 256 k)) (- (aref table row k))))
    (setf (aref table row 128) (- (aref table row 127)))
    (1- (* 2 v))))

(defun %read-quant-tables (c)
  "The five of them, and how many contexts they produce between them."
  (let ((table (make-array '(5 256) :element-type 'fixnum :initial-element 0))
        (count 1))
    (declare (type fixnum count))
    (dotimes (i 5)
      (setf count (* count (%read-quant-table c count table i)))
      (when (> count 32768) (%err "~d contexts is more than a quantiser table may describe" count)))
    (values table (floor (1+ count) 2))))

(defun parse-configuration (bytes)
  "The global header FFV1 version 3 keeps in its container (CodecPrivate in Matroska).

   IT IS RANGE CODED, not a plain bit field, which is unusual for a header and is why the range
   coder has to exist before anything can be parsed at all."
  (declare (type octets bytes))
  (let* ((c (make-decoder-over bytes 0 (length bytes)))
         (state (fresh-state))
         (cfg (make-config)))
    (setf (cfg-version cfg) (get-symbol c state nil))
    (when (< (cfg-version cfg) 2)
      ;; versions 0 and 1 keep the header in the packet rather than the container, so there is
      ;; nothing here to parse and the caller has been handed the wrong bytes
      (%err "FFV1 version ~d keeps its header in the frame, not the container" (cfg-version cfg)))
    (when (> (cfg-version cfg) 3) (%err "FFV1 version ~d is not supported" (cfg-version cfg)))
    (when (> (cfg-version cfg) 2)
      ;; the last four bytes are a checksum over the record and are not part of it
      (decf (rc-end c) 4)
      (setf (cfg-micro cfg) (get-symbol c state nil)))
    (setf (cfg-ac cfg) (get-symbol c state nil))
    (when (zerop (cfg-ac cfg))
      ;; Golomb-Rice instead of the range coder.  A different entropy coder entirely, with its own
      ;; adaptive state and a run mode; refused rather than approximated.
      (%err "the Golomb-Rice entropy coder is not supported yet"))
    (when (= (cfg-ac cfg) 2)
      ;; A CUSTOM STATE TABLE, which `-coder 1' means and which is therefore the common case rather
      ;; than an exotic one.  Each entry is a DIFFERENCE from the default table's, so the default
      ;; has to be built first — and the differences are read with a coder that is itself using the
      ;; default, which is only consistent because the table takes effect for the SLICES.
      (let ((tr (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
        (loop for i of-type fixnum from 1 below 256
              ;; masked rather than checked: the difference is against the DEFAULT table, and the
              ;; default has zeros at the indices no probability ever reaches, so a perfectly good
              ;; stream produces negative sums there.  They are never used.
              do (setf (aref tr i) (logand (+ (get-symbol c state t) (aref (rc-one-state c) i)) 255)))
        (setf (cfg-state-transition cfg) tr)))
    (setf (cfg-colorspace cfg) (get-symbol c state nil)
          (cfg-bits cfg) (get-symbol c state nil)
          (cfg-chroma-planes cfg) (= 1 (get-rac c state 0))
          (cfg-chroma-h-shift cfg) (get-symbol c state nil)
          (cfg-chroma-v-shift cfg) (get-symbol c state nil)
          (cfg-transparency cfg) (= 1 (get-rac c state 0)))
    (when (= (cfg-colorspace cfg) 2) (%err "Bayer FFV1 is not supported"))
    (when (> (cfg-bits cfg) 8) (%err "~d bits per sample is not supported (8 only)" (cfg-bits cfg)))
    ;; PLANE COUNT IS THE NUMBER OF CONTEXT SETS, NOT OF IMAGE PLANES.  The two chroma planes share
    ;; one set — and so do two of the three RGB planes — because they carry the same kind of picture
    ;; and what one learns is worth having for the other.
    (setf (cfg-plane-count cfg) (+ 2 (if (cfg-transparency cfg) 1 0)))
    (setf (cfg-h-slices cfg) (1+ (get-symbol c state nil))
          (cfg-v-slices cfg) (1+ (get-symbol c state nil)))
    (let ((n (get-symbol c state nil)))
      (when (or (zerop n) (> n 8)) (%err "~d quantiser tables" n))
      (let ((tables (make-array n)) (counts (make-array n :element-type 'fixnum)))
        (dotimes (i n)
          (multiple-value-bind (tbl cnt) (%read-quant-tables c)
            (setf (aref tables i) tbl (aref counts i) cnt)))
        (setf (cfg-quant-tables cfg) tables (cfg-context-counts cfg) counts)
        ;; optional initial context states, coded as differences from the previous context's
        (let ((state2 (make-array (list 32 +context-size+) :element-type '(unsigned-byte 8)
                                  :initial-element 128))
              (inits (make-array n :initial-element nil)))
          (dotimes (i n)
            (when (= 1 (get-rac c state 0))
              (let ((tbl (make-array (list (aref counts i) +context-size+)
                                     :element-type '(unsigned-byte 8) :initial-element 128)))
                (dotimes (j (aref counts i))
                  (dotimes (k +context-size+)
                    (let ((row (make-array +context-size+ :element-type '(unsigned-byte 8))))
                      (declare (ignorable row))
                      (let* ((pred (if (plusp j) (aref tbl (1- j) k) 128))
                             (tmp (make-array +context-size+ :element-type '(unsigned-byte 8))))
                        (declare (ignorable tmp))
                        (setf (aref tbl j k)
                              (logand (+ pred (%get-symbol-row c state2 k)) #xff))))))
                (setf (aref inits i) tbl))))
          (setf (cfg-initial-states cfg) inits))))
    (when (> (cfg-version cfg) 2)
      (setf (cfg-ec cfg) (get-symbol c state nil))
      (when (>= (+ (ash (cfg-version cfg) 16) (cfg-micro cfg)) #x30003)
        (setf (cfg-intra cfg) (get-symbol c state nil))))
    cfg))

(defun %get-symbol-row (c state2 k)
  "A signed symbol against row K of the two-dimensional state used for initial states."
  (let ((row (make-array +context-size+ :element-type '(unsigned-byte 8))))
    (dotimes (i +context-size+) (setf (aref row i) (aref state2 k i)))
    (prog1 (get-symbol c row t)
      (dotimes (i +context-size+) (setf (aref state2 k i) (aref row i))))))

;;; ---- a decoded picture ---------------------------------------------------------------------

(defstruct (frame (:conc-name fr-))
  (width 0 :type fixnum) (height 0 :type fixnum)
  (cwidth 0 :type fixnum) (cheight 0 :type fixnum)
  (y (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (u (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (v (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (ystride 0 :type fixnum) (cstride 0 :type fixnum)
  (timestamp nil))

(defun make-frame-for (cfg)
  (let* ((w (cfg-width cfg)) (h (cfg-height cfg))
         (cw (max 1 (ash w (- (cfg-chroma-h-shift cfg)))))
         (ch (max 1 (ash h (- (cfg-chroma-v-shift cfg))))))
    (make-frame :width w :height h :cwidth cw :cheight ch
                :y (make-array (* w h) :element-type '(unsigned-byte 8) :initial-element 0)
                :u (make-array (* cw ch) :element-type '(unsigned-byte 8) :initial-element 128)
                :v (make-array (* cw ch) :element-type '(unsigned-byte 8) :initial-element 128)
                :ystride w :cstride cw)))

;;; ---- prediction and context -------------------------------------------------------------------

(declaim (inline %median %predict))
(defun %median (a b c)
  (declare (type fixnum a b c) (optimize (speed 3) (safety 0)))
  (max (min a b) (min (max a b) c)))

(defun %predict (buf i last)
  "The median predictor: the left sample, the one above, and their sum less the diagonal.

   This is the LOCO-I / JPEG-LS predictor and it is chosen for what it does at EDGES: on a vertical
   edge it picks the sample above, on a horizontal one the sample to the left, and on a gradient the
   planar extrapolation.  A plain average would smear all three."
  (declare (type (simple-array fixnum (*)) buf) (type fixnum i last)
           (optimize (speed 3) (safety 0)))
  (let ((lt (aref buf (- last 1))) (tt (aref buf last)) (l (aref buf (- i 1))))
    (declare (type fixnum lt tt l))
    (%median l (- (+ l tt) lt) tt)))

(declaim (inline %context))
(defun %context (qt buf i last last2 five-p)
  "Which probability model this sample uses, from the quantised gradients around it."
  (declare (type (simple-array fixnum (5 256)) qt)
           (type (simple-array fixnum (*)) buf) (type fixnum i last last2)
           (optimize (speed 3) (safety 0)))
  (let ((lt (aref buf (- last 1))) (tt (aref buf last)) (rt (aref buf (+ last 1)))
        (l (aref buf (- i 1))))
    (declare (type fixnum lt tt rt l))
    (if five-p
        (let ((ttt (aref buf last2)) (ll (aref buf (- i 2))))
          (declare (type fixnum ttt ll))
          (+ (aref qt 0 (logand (- l lt) 255))
             (aref qt 1 (logand (- lt tt) 255))
             (aref qt 2 (logand (- tt rt) 255))
             (aref qt 3 (logand (- ll l) 255))
             (aref qt 4 (logand (- ttt tt) 255))))
        (+ (aref qt 0 (logand (- l lt) 255))
           (aref qt 1 (logand (- lt tt) 255))
           (aref qt 2 (logand (- tt rt) 255))))))

;;; ---- one plane of one slice -------------------------------------------------------------------

(defstruct (slice (:conc-name sl-))
  rc
  (x 0 :type fixnum) (y 0 :type fixnum) (w 0 :type fixnum) (h 0 :type fixnum)
  (quant-index (make-array 4 :element-type 'fixnum :initial-element 0) :type (simple-array fixnum (4)))
  (coding-mode 0 :type fixnum)
  (rct-by 1 :type fixnum) (rct-ry 1 :type fixnum)
  states)                                       ; per plane: (context-count x 32) octets

(defun %decode-line (cfg sl states qt w buf cur last last2 bits)
  "One line of one plane: a residual per sample, added to the prediction.

   The residual's model is chosen per SAMPLE from the neighbourhood, and a model and its mirror are
   the same model read backwards — a negative context means `decode as usual and then negate', which
   halves the number of models a picture needs to learn."
  (declare (type (simple-array fixnum (5 256)) qt)
           (type (simple-array fixnum (*)) buf)
           (type (simple-array (unsigned-byte 8) (* 32)) states)
           (type fixnum w cur last last2 bits)
           (optimize (speed 3) (safety 1)))
  (let* ((rc (sl-rc sl))
         (mask (1- (ash 1 bits)))
         (five-p (or (/= 0 (aref qt 3 127)) (/= 0 (aref qt 4 127))))
         (row (make-array +context-size+ :element-type '(unsigned-byte 8))))
    (declare (type fixnum mask) (dynamic-extent row))
    (when (= 1 (sl-coding-mode sl))
      ;; a slice the encoder gave up on: every sample sent raw, a bit at a time
      (dotimes (x w)
        (let ((v 0) (st (make-array 1 :element-type '(unsigned-byte 8) :initial-element 128)))
          (declare (dynamic-extent st) (type fixnum v))
          (dotimes (i bits) (setf (aref st 0) 128) (setf v (+ v v (get-rac rc st 0))))
          (setf (aref buf (+ cur x)) v)))
      (return-from %decode-line))
    (dotimes (x w)
      (declare (type fixnum x))
      (let* ((ctx (%context qt buf (+ cur x) (+ last x) (+ last2 x) five-p))
             (sign (minusp ctx)))
        (declare (type fixnum ctx))
        (when sign (setf ctx (- ctx)))
        (dotimes (i +context-size+) (setf (aref row i) (aref states ctx i)))
        (let ((diff (get-symbol rc row t)))
          (declare (type fixnum diff))
          (dotimes (i +context-size+) (setf (aref states ctx i) (aref row i)))
          (when sign (setf diff (- diff)))
          (setf (aref buf (+ cur x))
                (logand (+ (%predict buf (+ cur x) (+ last x)) diff) mask)))))))

(defun %decode-plane (cfg sl plane-index states qt dst stride ox oy w h bits)
  "One plane of one slice, line by line, into DST.

   TWO LINES OF HISTORY, kept in one buffer with three samples of margin at each end so that the
   neighbours off the left and right edges can be read without a test.  The margin is not padding
   for safety: the specification says what those samples are — the line above repeats its first and
   last — and writing them is what makes the prediction correct at the edges rather than merely
   in range."
  (declare (type octets dst) (type fixnum stride ox oy w h bits)
           (optimize (speed 3) (safety 1)))
  (let* ((n (+ w 6))
         (buf (make-array (* 2 n) :element-type 'fixnum :initial-element 0))
         (cur (+ n 3)) (last 3))
    (declare (type fixnum n cur last))
    (dotimes (y h)
      (declare (type fixnum y))
      (rotatef cur last)
      ;; the sample left of the line's start is the one above it, and the one past the previous
      ;; line's end repeats that line's last
      (setf (aref buf (- cur 1)) (aref buf last)
            (aref buf (+ last w)) (aref buf (+ last w -1)))
      ;; THE THIRD ARGUMENT IS THE CURRENT ROW, NOT THE ONE ABOVE IT, and that is not a slip.  The
      ;; two-line buffer alternates, so before a sample is written its slot still holds the value
      ;; from TWO rows up — which is exactly the neighbour the five-input context wants.  Keeping a
      ;; third line would be the obvious thing and would also be a third more memory traffic.
      (%decode-line cfg sl states qt w buf cur last cur bits)
      (let ((o (+ (* (+ oy y) stride) ox)))
        (declare (type fixnum o))
        (dotimes (x w) (setf (aref dst (+ o x)) (logand (aref buf (+ cur x)) 255)))))))

;;; ---- slices ------------------------------------------------------------------------------------

(defun %slice-coord (cfg total i n shift)
  "Where slice I of N begins along an axis of TOTAL samples, rounded so that chroma divides evenly."
  (declare (type fixnum total i n shift))
  (if (zerop shift)
      (floor (* total i) n)
      (ash (floor (* (ash total (- shift)) i) n) shift)))

(defun %read-slice-header (cfg sl frame)
  "The per-slice header: where it is, which quantiser table each plane uses, and how it was coded."
  (declare (ignorable frame))
  (let* ((c (sl-rc sl)) (state (fresh-state))
         (sx (get-symbol c state nil)) (sy (get-symbol c state nil))
         (sw (1+ (get-symbol c state nil))) (sh (1+ (get-symbol c state nil))))
    (declare (type fixnum sx sy sw sh))
    (when (or (minusp sx) (minusp sy) (< sw 1) (< sh 1)
              (> sx (- (cfg-h-slices cfg) sw)) (> sy (- (cfg-v-slices cfg) sh)))
      (%err "a slice at ~d,~d of ~dx~d" sx sy sw sh))
    (setf (sl-x sl) (%slice-coord cfg (cfg-width cfg) sx (cfg-h-slices cfg) (cfg-chroma-h-shift cfg))
          (sl-y sl) (%slice-coord cfg (cfg-height cfg) sy (cfg-v-slices cfg) (cfg-chroma-v-shift cfg)))
    (setf (sl-w sl) (- (%slice-coord cfg (cfg-width cfg) (+ sx sw) (cfg-h-slices cfg)
                                     (cfg-chroma-h-shift cfg))
                       (sl-x sl))
          (sl-h sl) (- (%slice-coord cfg (cfg-height cfg) (+ sy sh) (cfg-v-slices cfg)
                                     (cfg-chroma-v-shift cfg))
                       (sl-y sl)))
    (dotimes (i (cfg-plane-count cfg))
      (let ((idx (get-symbol c state nil)))
        (when (>= idx (length (cfg-quant-tables cfg))) (%err "quantiser table index ~d" idx))
        (setf (aref (sl-quant-index sl) (min i 3)) idx)))
    (get-symbol c state nil)                     ; picture structure
    (get-symbol c state nil)                     ; sample aspect ratio numerator
    (get-symbol c state nil)                     ; and denominator
    (when (> (cfg-version cfg) 3)
      (get-rac c state 0)
      (setf (sl-coding-mode sl) (get-symbol c state nil)))
    sl))

(defun %find-slices (cfg bytes start end)
  "Where each slice of a packet begins and ends.

   THE LENGTHS ARE AT THE END, NOT THE FRONT.  Every slice but the first carries its own length in
   the three bytes before its trailer, so the packet is walked BACKWARDS from the end — which is
   what lets an encoder write slices in any order and a decoder decode them in parallel."
  (declare (type octets bytes) (type fixnum start end))
  (let* ((n (* (cfg-h-slices cfg) (cfg-v-slices cfg)))
         (trailer (+ 3 (if (plusp (cfg-ec cfg)) 5 0)))
         (out (make-array n))
         (e end))
    (declare (type fixnum n trailer e))
    (loop for i of-type fixnum from (1- n) downto 0
          do (let ((len (if (or (plusp i) (> (cfg-version cfg) 2))
                            (if (> trailer (- e start))
                                (%err "a slice length field past the end of the packet")
                                (+ trailer (logior (ash (aref bytes (- e trailer)) 16)
                                                   (ash (aref bytes (- e trailer -1)) 8)
                                                   (aref bytes (- e trailer -2)))))
                            (- e start))))
               (declare (type fixnum len))
               (when (> len (- e start)) (%err "the slice chain is broken at slice ~d" i))
               (setf (aref out i) (cons (if (plusp i) (- e len) start) e))
               (decf e len)))
    out))

(defstruct (decoder (:conc-name d-))
  cfg
  ;; per slice, per plane context: the adaptive probability models.  THEY OUTLIVE THE FRAME, which
  ;; is the whole of FFV1's inter coding — there is no motion and no reference picture, but a
  ;; non-key frame starts from the models the last one finished with, and on video that is a large
  ;; part of the saving.
  (states nil))

(defun make-ffv1-decoder (cfg) (make-decoder :cfg cfg))

(defun %slice-states (d cfg sl i key-p)
  "The state arrays for slice I, kept from last frame unless this one is a key frame."
  (let ((n (* (cfg-h-slices cfg) (cfg-v-slices cfg))))
    (unless (and (d-states d) (= (length (d-states d)) n))
      (setf (d-states d) (make-array n :initial-element nil)))
    (let ((cur (aref (d-states d) i)))
      (when (or key-p (null cur))
        (setf cur (make-array (cfg-plane-count cfg)))
        (dotimes (k (cfg-plane-count cfg))
          (let* ((qi (aref (sl-quant-index sl) (min k 3)))
                 (tbl (make-array (list (aref (cfg-context-counts cfg) qi) +context-size+)
                                  :element-type '(unsigned-byte 8) :initial-element 128))
                 (init (aref (cfg-initial-states cfg) qi)))
            (when init
              (dotimes (j (aref (cfg-context-counts cfg) qi))
                (dotimes (m +context-size+) (setf (aref tbl j m) (aref init j m)))))
            (setf (aref cur k) tbl)))
        (setf (aref (d-states d) i) cur))
      cur)))

(defun decode-frame (d bytes &key (start 0) (end (length bytes)))
  "One FFV1 packet into a frame."
  (declare (type octets bytes) (type fixnum start end))
  (let* ((cfg (d-cfg d))
         (frame (make-frame-for cfg))
         (bounds (%find-slices cfg bytes start end))
         (rgb (= 1 (cfg-colorspace cfg)))
         ;; THE PACKET BEGINS WITH ONE BIT, and slice zero does not get a fresh coder afterwards —
         ;; it CONTINUES this one.  That bit says whether the frame carries a header, which for
         ;; version 3 it never does because the header is in the container; but the bit is there,
         ;; and a decoder that starts slice zero at the first byte reads it as picture data.  Every
         ;; other slice decodes perfectly, which is what makes it look like a slice-zero bug rather
         ;; than a missing bit.
         (frame-rc (make-decoder-over bytes start end))
         (keystate (make-array 1 :element-type '(unsigned-byte 8) :initial-element 128))
         (key-p nil))
    (setf key-p (= 1 (get-rac frame-rc keystate 0)))
    (dotimes (i (length bounds))
      (destructuring-bind (a . b) (aref bounds i)
        (let* ((sl (make-slice :rc (if (zerop i)
                                       (progn (setf (rc-end frame-rc) b) frame-rc)
                                       (make-decoder-over bytes a b)))))
          (%apply-state-transition cfg (sl-rc sl))
          (%read-slice-header cfg sl frame)
          ;; THE TWO CHROMA PLANES SHARE ONE SET OF CONTEXTS, and share them RUNNING: the states
          ;; the U plane finished with are the states the V plane starts from.  They are the same
          ;; kind of picture, so what one learns is worth having for the other — and a decoder that
          ;; gives them a set each decodes the U plane perfectly and the V plane not at all.
          (let ((states (%slice-states d cfg sl i key-p)))
            (flet ((plane (ctx dst stride ox oy w h)
                     (when (and (plusp w) (plusp h))
                       (%decode-plane cfg sl ctx (aref states ctx)
                                      (aref (cfg-quant-tables cfg)
                                            (aref (sl-quant-index sl) (min ctx 3)))
                                      dst stride ox oy w h (cfg-bits cfg)))))
              (if rgb
                  (%decode-rgb-slice cfg sl states frame)
                  (let ((hs (cfg-chroma-h-shift cfg)) (vs (cfg-chroma-v-shift cfg)))
                    (plane 0 (fr-y frame) (fr-ystride frame)
                           (sl-x sl) (sl-y sl) (sl-w sl) (sl-h sl))
                    (when (cfg-chroma-planes cfg)
                      (plane 1 (fr-u frame) (fr-cstride frame)
                             (ash (sl-x sl) (- hs)) (ash (sl-y sl) (- vs))
                             (ceiling (sl-w sl) (ash 1 hs)) (ceiling (sl-h sl) (ash 1 vs)))
                      (plane 1 (fr-v frame) (fr-cstride frame)
                             (ash (sl-x sl) (- hs)) (ash (sl-y sl) (- vs))
                             (ceiling (sl-w sl) (ash 1 hs)) (ceiling (sl-h sl) (ash 1 vs)))))))))))
    frame))

(defun %decode-rgb-slice (cfg sl states frame)
  "The three RGB planes of one slice, decoded LINE BY LINE ACROSS THE PLANES.

   Not plane by plane: all three lines of row zero, then all three of row one.  They share one range
   coder, so the interleaving is not an implementation choice — it is the order the bits are in.

   The two difference planes are NINE bits wide rather than eight, because a difference of two
   eight-bit values needs the extra one; and the reversible colour transform that turns them back
   into red, green and blue is applied per line, not at the end."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((w (sl-w sl)) (h (sl-h sl))
         (n (+ w 6))
         (bufs (make-array 3))
         (cur (make-array 3 :element-type 'fixnum :initial-element (+ n 3)))
         (last (make-array 3 :element-type 'fixnum :initial-element 3))
         (offset (ash 1 (cfg-bits cfg)))
         (g (fr-y frame)) (b (fr-u frame)) (r (fr-v frame))
         (stride (fr-ystride frame)))
    (declare (type fixnum w h n offset stride))
    (dotimes (p 3) (setf (aref bufs p) (make-array (* 2 n) :element-type 'fixnum
                                                   :initial-element 0)))
    (dotimes (y h)
      (declare (type fixnum y))
      (dotimes (p 3)
        (let ((buf (aref bufs p)))
          (declare (type (simple-array fixnum (*)) buf))
          (rotatef (aref cur p) (aref last p))
          (setf (aref buf (- (aref cur p) 1)) (aref buf (aref last p))
                (aref buf (+ (aref last p) w)) (aref buf (+ (aref last p) w -1)))
          (let ((ctx (if (zerop p) 0 1)))
            (%decode-line cfg sl (aref states ctx)
                          (aref (cfg-quant-tables cfg) (aref (sl-quant-index sl) (min ctx 3)))
                          w buf (aref cur p) (aref last p) (aref cur p)
                          (if (zerop p) (cfg-bits cfg) (1+ (cfg-bits cfg)))))))
      (let ((o (+ (* (+ (sl-y sl) y) stride) (sl-x sl))))
        (declare (type fixnum o))
        (dotimes (x w)
          (declare (type fixnum x))
          (let* ((gv (aref (the (simple-array fixnum (*)) (aref bufs 0)) (+ (aref cur 0) x)))
                 (bv (- (aref (the (simple-array fixnum (*)) (aref bufs 1)) (+ (aref cur 1) x))
                        offset))
                 (rv (- (aref (the (simple-array fixnum (*)) (aref bufs 2)) (+ (aref cur 2) x))
                        offset)))
            (declare (type fixnum gv bv rv))
            (decf gv (ash (+ (* bv (sl-rct-by sl)) (* rv (sl-rct-ry sl))) -2))
            (incf bv gv) (incf rv gv)
            (setf (aref g (+ o x)) (logand gv 255)
                  (aref b (+ o x)) (logand bv 255)
                  (aref r (+ o x)) (logand rv 255))))))))

(defun %apply-state-transition (cfg c)
  "Install the stream's own state table, if it sent one."
  (let ((tr (cfg-state-transition cfg)))
    (when tr
      (let ((os (rc-one-state c)) (zs (rc-zero-state c)))
        (replace os tr)
        (loop for i of-type fixnum from 1 below 255
              do (setf (aref zs i) (logand (- 256 (aref os (- 256 i))) 255)))))))


;;; ---- output ---------------------------------------------------------------------------------

(defun picture->yuv420 (f)
  "One frame as planar samples in whatever chroma layout it has."
  (let* ((w (fr-width f)) (h (fr-height f))
         (cw (fr-cwidth f)) (ch (fr-cheight f))
         (out (make-array (+ (* w h) (* 2 cw ch)) :element-type '(unsigned-byte 8)))
         (o 0))
    (dotimes (y h)
      (replace out (fr-y f) :start1 o :start2 (* y (fr-ystride f)) :end2 (+ (* y (fr-ystride f)) w))
      (incf o w))
    (dolist (p (list (fr-u f) (fr-v f)))
      (dotimes (y ch)
        (replace out p :start1 o :start2 (* y (fr-cstride f)) :end2 (+ (* y (fr-cstride f)) cw))
        (incf o cw)))
    out))

(defun as-picture (f)
  "This decoder's frame as a REEL.DECODE:PICTURE, sharing planes rather than copying them."
  (reel.decode::%make-shared-picture
   :width (fr-width f) :height (fr-height f)
   :y (fr-y f) :u (fr-u f) :v (fr-v f)
   :y-stride (fr-ystride f) :uv-stride (fr-cstride f)
   :y-offset 0 :uv-offset 0))

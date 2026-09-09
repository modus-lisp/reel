;;;; hevc/params.lisp — video, sequence and picture parameter sets, and the slice segment header.
;;;;
;;;; H.264 puts two numbers in the sequence parameter set and derives the rest: a picture is a grid
;;;; of sixteen-by-sixteen macroblocks, full stop.  HEVC declares its own geometry instead.  The
;;;; coding tree block may be 16, 32 or 64 samples square; the transform block may be 4 to 32; the
;;;; quadtree between them has a configurable depth.  So almost nothing about how a picture is cut
;;;; up is known until the sequence parameter set has been read, and the derived quantities below —
;;;; CtbLog2SizeY and the rest — are what everything above this file is written in terms of.
;;;;
;;;; WHAT IS REFUSED, AND WHY IT IS REFUSED HERE.  This decodes Main profile: eight bits per sample
;;;; and 4:2:0.  Every refusal is on the FLAG that turns a tool on rather than on the profile that
;;;; permits it, so a stream that declares a profile this does not know but uses nothing from it
;;;; still decodes.  The refusals live in the parser because that is the last place the bitstream
;;;; is still comprehensible: past an unparsed field, every later value is nonsense, and a decoder
;;;; that carries on produces a confident wrong picture rather than an error.

(in-package #:reel.hevc)

;;; ---- profile, tier and level ------------------------------------------------------------------

(defstruct (ptl (:conc-name ptl-))
  (profile-space 0) (tier 0) (profile-idc 0)
  (compatibility 0 :type (unsigned-byte 32))    ; general_profile_compatibility_flag[0..31]
  (progressive-source nil) (interlaced-source nil)
  (level 0))

(defun %parse-ptl (br max-sub-layers-minus1 &key (profile-present t))
  "profile_tier_level (7.3.3).

   The size of this thing is the point: 88 bits of profile before a byte of level, then a
   two-bit-per-sub-layer presence table that is PADDED TO EIGHT ENTRIES whenever there is more than
   one sub-layer, and only then the sub-layer profiles themselves.  Get the padding wrong and the
   sequence parameter set that follows reads as garbage — which looks like a corrupt stream rather
   than like a parser bug, because everything downstream of it is."
  (let ((p (make-ptl)))
    (when profile-present
      (setf (ptl-profile-space p) (ub br 2)
            (ptl-tier p) (u1 br)
            (ptl-profile-idc p) (ub br 5)
            (ptl-compatibility p) (ub br 32)
            (ptl-progressive-source p) (= 1 (u1 br))
            (ptl-interlaced-source p) (= 1 (u1 br)))
      (u1 br)                                   ; general_non_packed_constraint_flag
      (u1 br)                                   ; general_frame_only_constraint_flag
      ;; 43 bits of profile-specific constraint flags, then one bit that is general_inbld_flag for
      ;; the profiles that define it and reserved for the rest.  Nothing here reads them.
      (ub br 43)
      (u1 br))
    (setf (ptl-level p) (ub br 8))
    (let ((prof (make-array 8 :initial-element nil))
          (lev (make-array 8 :initial-element nil)))
      (dotimes (i max-sub-layers-minus1)
        (setf (aref prof i) (= 1 (u1 br))
              (aref lev i) (= 1 (u1 br))))
      (when (plusp max-sub-layers-minus1)
        (loop for i from max-sub-layers-minus1 below 8 do (ub br 2)))
      (dotimes (i max-sub-layers-minus1)
        (when (aref prof i) (ub br 44) (ub br 44))   ; the same 88 bits, unread
        (when (aref lev i) (ub br 8))))
    p))

;;; ---- scaling lists ----------------------------------------------------------------------------

(defstruct (scaling (:conc-name sl-))
  "The dequantisation weights, as RASTER matrices ready to index.

   SL is four sizes by six matrices — intra Y, Cb, Cr then inter Y, Cb, Cr — each 16 entries at
   4x4 and 64 at every larger size, because 16x16 and 32x32 both weight through the 8x8 list with
   each entry repeated.  DC holds the separately transmitted DC weight for those two sizes."
  (sl (make-array '(4 6)) :type (simple-array t (4 6)))
  (dc (make-array '(2 6) :element-type 'fixnum :initial-element 16)
   :type (simple-array fixnum (2 6))))

(defun %default-scaling ()
  "Every matrix at its default: flat at 4x4, and the two 8x8 defaults above it."
  (let ((s (make-scaling)))
    (dotimes (m 6)
      (setf (aref (sl-sl s) 0 m)
            (make-array 16 :element-type '(unsigned-byte 8) :initial-element 16)))
    (loop for size from 1 to 3
          do (dotimes (m 6)
               (setf (aref (sl-sl s) size m)
                     (copy-seq (if (< m 3) +default-scaling-intra+ +default-scaling-inter+)))))
    s))

(defun %parse-scaling-list-data (br)
  "scaling_list_data (7.3.4): four sizes by six matrices, each defaulted, copied, or sent.

   The coefficients are a DELTA CHAIN modulo 256 rather than absolute values, and the chain starts
   at 8 — or, above 8x8, at the DC weight that is sent first and then also seeds the chain.  They
   arrive in the DIAGONAL SCAN and are scattered into raster here, because that is the order
   dequantisation indexes them in and doing it later would mean doing it per coefficient."
  (let ((s (%default-scaling)))
    (dotimes (size-id 4)
      (do ((matrix-id 0 (+ matrix-id (if (= size-id 3) 3 1))))
          ((>= matrix-id 6))
        (if (zerop (u1 br))                     ; scaling_list_pred_mode_flag
            ;; 0 means the default, which is already in place; anything else names an earlier matrix
            (let ((delta (* (ue br) (if (= size-id 3) 3 1))))
              (when (plusp delta)
                (when (< matrix-id delta)
                  (%err "a scaling list refers to matrix ~d, which is before the first"
                        (- matrix-id delta)))
                (setf (aref (sl-sl s) size-id matrix-id)
                      (copy-seq (aref (sl-sl s) size-id (- matrix-id delta))))
                (when (> size-id 1)
                  (setf (aref (sl-dc s) (- size-id 2) matrix-id)
                        (aref (sl-dc s) (- size-id 2) (- matrix-id delta))))))
            (let* ((n (min 64 (ash 1 (+ 4 (ash size-id 1)))))
                   (m (aref (sl-sl s) size-id matrix-id))
                   (xs (if (zerop size-id) (first +diag4x4+) (first +diag8x8+)))
                   (ys (if (zerop size-id) (second +diag4x4+) (second +diag8x8+)))
                   (w (if (zerop size-id) 4 8))
                   (next 8))
              (when (> size-id 1)
                (setf next (+ 8 (se br))
                      (aref (sl-dc s) (- size-id 2) matrix-id) next))
              (dotimes (i n)
                (setf next (mod (+ next (se br) 256) 256)
                      (aref m (+ (* w (aref ys i)) (aref xs i))) next))))))
    s))

;;; ---- short-term reference picture sets --------------------------------------------------------

(defstruct (strps (:conc-name strps-))
  "One short-term reference picture set, as the two lists 8.3.2 actually wants.

   NEGATIVE holds the pictures before this one in output order, nearest first, as positive
   distances; POSITIVE holds those after it the same way.  The USED flags say which of them this
   picture may itself predict from, as opposed to merely having to keep alive for a later one."
  ;; STORED AS POSITIVE DISTANCES, where the specification's DeltaPocS0 is negative.  That is
  ;; convenient everywhere except in the inter-set derivation of 7-59 and 7-60, which adds a signed
  ;; delta to a signed POC: every read of NEGATIVE there has to be negated first, and two of the
  ;; four loops that do it are easy to get backwards.
  (negative #() :type simple-vector)
  (positive #() :type simple-vector)
  (used-neg #() :type simple-vector)
  (used-pos #() :type simple-vector))

(defun strps-count (s) (+ (length (strps-negative s)) (length (strps-positive s))))

(defun %parse-strps (br idx sets num-sets)
  "st_ref_pic_set (7.3.7), including the inter-set prediction that most streams use.

   A set may be sent as a DELTA FROM AN EARLIER ONE, which is the whole reason this is not four
   lines: a stream with a regular prediction structure describes the first set in full and then
   says \"the same, shifted by one picture\" for the rest.  The derivation of 7-59 to 7-61 walks the
   reference set's two lists in the order that keeps the result sorted by distance, which means
   walking the positives backwards into the negatives and vice versa — and a decoder that just
   concatenates and sorts gets the same set with the used-flags attached to the wrong entries."
  (let ((inter-pred (and (plusp idx) (= 1 (u1 br)))))
    (if (not inter-pred)
        (let* ((nneg (ue br)) (npos (ue br))
               (neg (make-array nneg)) (pos (make-array npos))
               (un (make-array nneg)) (up (make-array npos))
               (prev 0))
          (dotimes (i nneg)
            (incf prev (1+ (ue br)))
            (setf (aref neg i) prev (aref un i) (= 1 (u1 br))))
          (setf prev 0)
          (dotimes (i npos)
            (incf prev (1+ (ue br)))
            (setf (aref pos i) prev (aref up i) (= 1 (u1 br))))
          (make-strps :negative neg :positive pos :used-neg un :used-pos up))
        (let* ((delta-idx (if (= idx num-sets) (1+ (ue br)) 1))
               (ref-idx (- idx delta-idx))
               (ref (or (and (>= ref-idx 0) (aref sets ref-idx))
                        (%err "a reference picture set predicts from set ~d, which is not there"
                              ref-idx)))
               (sign (u1 br))
               (delta-rps (* (if (zerop sign) 1 -1) (1+ (ue br))))
               (n (strps-count ref))
               (used (make-array (1+ n)))
               (use-delta (make-array (1+ n) :initial-element t)))
          (dotimes (j (1+ n))
            (setf (aref used j) (= 1 (u1 br)))
            (unless (aref used j) (setf (aref use-delta j) (= 1 (u1 br)))))
          (let ((rneg (strps-negative ref)) (rpos (strps-positive ref))
                (neg '()) (un '()) (pos '()) (up '()))
            ;; 7-59: the new negatives, nearest first.  The reference's POSITIVES are visited from
            ;; the far end inwards first, because shifting by a negative delta can move a picture
            ;; that was after this one to before it, and it lands nearer than the old negatives do.
            (loop for j from (1- (length rpos)) downto 0
                  do (let ((d (+ (aref rpos j) delta-rps)))
                       (when (and (minusp d) (aref use-delta (+ (length rneg) j)))
                         (push (cons (- d) (aref used (+ (length rneg) j))) neg))))
            (when (and (minusp delta-rps) (aref use-delta n))
              (push (cons (- delta-rps) (aref used n)) neg))
            (loop for j from 0 below (length rneg)
                  do (let ((d (+ (- (aref rneg j)) delta-rps)))
                       (when (and (minusp d) (aref use-delta j))
                         (push (cons (- d) (aref used j)) neg))))
            (setf neg (nreverse neg))
            ;; 7-60: the new positives, the same walk mirrored
            (loop for j from (1- (length rneg)) downto 0
                  do (let ((d (+ (- (aref rneg j)) delta-rps)))
                       (when (and (plusp d) (aref use-delta j))
                         (push (cons d (aref used j)) pos))))
            (when (and (plusp delta-rps) (aref use-delta n))
              (push (cons delta-rps (aref used n)) pos))
            (loop for j from 0 below (length rpos)
                  do (let ((d (+ (aref rpos j) delta-rps)))
                       (when (and (plusp d) (aref use-delta (+ (length rneg) j)))
                         (push (cons d (aref used (+ (length rneg) j))) pos))))
            (setf pos (nreverse pos))
            (setf un (mapcar #'cdr neg) up (mapcar #'cdr pos))
            (make-strps :negative (coerce (mapcar #'car neg) 'simple-vector)
                        :positive (coerce (mapcar #'car pos) 'simple-vector)
                        :used-neg (coerce un 'simple-vector)
                        :used-pos (coerce up 'simple-vector)))))))

;;; ---- the sequence parameter set ---------------------------------------------------------------

(defstruct (sps (:conc-name sps-))
  (id 0) (vps-id 0)
  (ptl nil)
  (max-sub-layers 1)
  (chroma-format 1)                     ; 1 = 4:2:0, which is all this decodes
  (separate-colour-planes nil)
  (width 0) (height 0)                  ; pic_width/height_in_luma_samples, before the window
  (conf-left 0) (conf-right 0) (conf-top 0) (conf-bottom 0)
  (bit-depth-luma 8) (bit-depth-chroma 8)
  (log2-max-poc-lsb 4)
  (max-dec-pic-buffering 1) (max-num-reorder 0)
  ;; the block geometry, all as log2 because every use of them is a shift
  (min-cb-log2 3) (ctb-log2 6)
  (min-tb-log2 2) (max-tb-log2 5)
  (max-transform-depth-inter 0) (max-transform-depth-intra 0)
  (scaling-list-enabled nil) (scaling nil)
  (amp-enabled nil) (sao-enabled nil)
  (pcm-enabled nil) (pcm-bit-depth-luma 8) (pcm-bit-depth-chroma 8)
  (pcm-min-cb-log2 3) (pcm-max-cb-log2 3) (pcm-loop-filter-disabled nil)
  (strps #() :type simple-vector)
  (long-term-present nil) (long-term-poc #() :type simple-vector)
  (long-term-used #() :type simple-vector)
  (temporal-mvp-enabled nil) (strong-intra-smoothing nil))

(defun sps-sub-width (s) (if (member (sps-chroma-format s) '(1 2)) 2 1))
(defun sps-sub-height (s) (if (= (sps-chroma-format s) 1) 2 1))

(defun sps-ctb-size (s) (ash 1 (sps-ctb-log2 s)))
(defun sps-ctbs-wide (s) (ceiling (sps-width s) (sps-ctb-size s)))
(defun sps-ctbs-high (s) (ceiling (sps-height s) (sps-ctb-size s)))
(defun sps-ctbs (s) (* (sps-ctbs-wide s) (sps-ctbs-high s)))

(defun sps-display-width (s)
  "The width after the conformance window, which is HEVC's spelling of H.264's frame cropping.
   The offsets are in chroma samples, so each costs SubWidthC luma columns."
  (- (sps-width s) (* (sps-sub-width s) (+ (sps-conf-left s) (sps-conf-right s)))))

(defun sps-display-height (s)
  (- (sps-height s) (* (sps-sub-height s) (+ (sps-conf-top s) (sps-conf-bottom s)))))

(defun parse-sps (rbsp)
  "One sequence parameter set (7.3.2.2)."
  (let ((br (make-bitreader rbsp))
        (s (make-sps)))
    (setf (sps-vps-id s) (ub br 4))
    (let ((max-sub (ub br 3)))
      (setf (sps-max-sub-layers s) (1+ max-sub))
      (u1 br)                                   ; sps_temporal_id_nesting_flag
      (setf (sps-ptl s) (%parse-ptl br max-sub)))
    (setf (sps-id s) (ue br)
          (sps-chroma-format s) (ue br))
    (when (= 3 (sps-chroma-format s))
      (setf (sps-separate-colour-planes s) (= 1 (u1 br))))
    (setf (sps-width s) (ue br)
          (sps-height s) (ue br))
    (when (= 1 (u1 br))                         ; conformance_window_flag
      (setf (sps-conf-left s) (ue br) (sps-conf-right s) (ue br)
            (sps-conf-top s) (ue br) (sps-conf-bottom s) (ue br)))
    (setf (sps-bit-depth-luma s) (+ 8 (ue br))
          (sps-bit-depth-chroma s) (+ 8 (ue br))
          (sps-log2-max-poc-lsb s) (+ 4 (ue br)))
    ;; the ordering info may be sent once for the top sub-layer or once per sub-layer; either way
    ;; the last one read is the one that governs a decoder that keeps every layer
    (let ((all (= 1 (u1 br))))
      (loop for i from (if all 0 (1- (sps-max-sub-layers s))) below (sps-max-sub-layers s)
            do (setf (sps-max-dec-pic-buffering s) (1+ (ue br))
                     (sps-max-num-reorder s) (ue br))
               (ue br)))                        ; sps_max_latency_increase_plus1
    (setf (sps-min-cb-log2 s) (+ 3 (ue br))
          (sps-ctb-log2 s) (+ (sps-min-cb-log2 s) (ue br))
          (sps-min-tb-log2 s) (+ 2 (ue br))
          (sps-max-tb-log2 s) (+ (sps-min-tb-log2 s) (ue br))
          (sps-max-transform-depth-inter s) (ue br)
          (sps-max-transform-depth-intra s) (ue br))
    (when (= 1 (u1 br))                         ; scaling_list_enabled_flag
      (setf (sps-scaling-list-enabled s) t
            ;; enabled but not transmitted means the defaults, which is a real setting and not the
            ;; same thing as disabled: the default lists are not flat
            (sps-scaling s) (if (= 1 (u1 br))   ; sps_scaling_list_data_present_flag
                                (%parse-scaling-list-data br)
                                (%default-scaling))))
    (setf (sps-amp-enabled s) (= 1 (u1 br))
          (sps-sao-enabled s) (= 1 (u1 br))
          (sps-pcm-enabled s) (= 1 (u1 br)))
    (when (sps-pcm-enabled s)
      (setf (sps-pcm-bit-depth-luma s) (1+ (ub br 4))
            (sps-pcm-bit-depth-chroma s) (1+ (ub br 4))
            (sps-pcm-min-cb-log2 s) (+ 3 (ue br))
            (sps-pcm-max-cb-log2 s) (+ (sps-pcm-min-cb-log2 s) (ue br))
            (sps-pcm-loop-filter-disabled s) (= 1 (u1 br))))
    (let* ((n (ue br))
           (sets (make-array n)))
      (dotimes (i n) (setf (aref sets i) (%parse-strps br i sets n)))
      (setf (sps-strps s) sets))
    (when (= 1 (u1 br))                         ; long_term_ref_pics_present_flag
      (setf (sps-long-term-present s) t)
      (let* ((n (ue br))
             (poc (make-array n)) (used (make-array n)))
        (dotimes (i n)
          (setf (aref poc i) (ub br (sps-log2-max-poc-lsb s))
                (aref used i) (= 1 (u1 br))))
        (setf (sps-long-term-poc s) poc (sps-long-term-used s) used)))
    (setf (sps-temporal-mvp-enabled s) (= 1 (u1 br))
          (sps-strong-intra-smoothing s) (= 1 (u1 br)))
    ;; Nothing past here is read.  The VUI and the extension flags follow, and skipping the VUI
    ;; correctly means parsing hrd_parameters, which is a long mechanical function with nothing
    ;; above it that wants an answer.  It goes in when something needs what comes after it.
    (%check-sps s)
    s))

(defun %check-sps (s)
  "Refuse what this decoder does not implement, naming the tool rather than the profile."
  (unless (= 1 (sps-chroma-format s))
    (%err "chroma_format_idc ~d (~a); this decodes 4:2:0" (sps-chroma-format s)
          (case (sps-chroma-format s) (0 "monochrome") (2 "4:2:2") (3 "4:4:4") (t "unknown"))))
  (unless (and (= 8 (sps-bit-depth-luma s)) (= 8 (sps-bit-depth-chroma s)))
    (%err "~d-bit luma and ~d-bit chroma; this decodes eight"
          (sps-bit-depth-luma s) (sps-bit-depth-chroma s)))
  (when (plusp (sps-width s))
    (unless (and (zerop (mod (sps-width s) (ash 1 (sps-min-cb-log2 s))))
                 (zerop (mod (sps-height s) (ash 1 (sps-min-cb-log2 s)))))
      (%err "picture ~dx~d is not a whole number of ~d-sample coding blocks"
            (sps-width s) (sps-height s) (ash 1 (sps-min-cb-log2 s)))))
  s)

;;; ---- the picture parameter set ----------------------------------------------------------------

(defstruct (pps (:conc-name pps-))
  (id 0) (sps-id 0)
  (dependent-slices-enabled nil) (output-flag-present nil)
  (extra-slice-header-bits 0)
  (sign-data-hiding nil) (cabac-init-present nil)
  (num-ref-idx-l0 1) (num-ref-idx-l1 1)
  (init-qp 26)
  (constrained-intra nil) (transform-skip nil)
  (cu-qp-delta-enabled nil) (diff-cu-qp-delta-depth 0)
  (cb-qp-offset 0) (cr-qp-offset 0) (slice-chroma-qp-offsets nil)
  (weighted-pred nil) (weighted-bipred nil)
  (transquant-bypass nil)
  (tiles-enabled nil) (entropy-coding-sync nil)
  (num-tile-columns 1) (num-tile-rows 1) (uniform-spacing t)
  (column-widths nil) (row-heights nil) (loop-filter-across-tiles t)
  (loop-filter-across-slices nil)
  (deblocking-control-present nil) (deblocking-override-enabled nil)
  (deblocking-disabled nil) (beta-offset 0) (tc-offset 0)
  (scaling nil)
  (lists-modification-present nil)
  (log2-parallel-merge-level 2)
  (slice-header-extension nil))

(defun parse-pps (rbsp)
  "One picture parameter set (7.3.2.3)."
  (let ((br (make-bitreader rbsp))
        (p (make-pps)))
    (setf (pps-id p) (ue br)
          (pps-sps-id p) (ue br)
          (pps-dependent-slices-enabled p) (= 1 (u1 br))
          (pps-output-flag-present p) (= 1 (u1 br))
          (pps-extra-slice-header-bits p) (ub br 3)
          (pps-sign-data-hiding p) (= 1 (u1 br))
          (pps-cabac-init-present p) (= 1 (u1 br))
          (pps-num-ref-idx-l0 p) (1+ (ue br))
          (pps-num-ref-idx-l1 p) (1+ (ue br))
          (pps-init-qp p) (+ 26 (se br))
          (pps-constrained-intra p) (= 1 (u1 br))
          (pps-transform-skip p) (= 1 (u1 br))
          (pps-cu-qp-delta-enabled p) (= 1 (u1 br)))
    (when (pps-cu-qp-delta-enabled p)
      (setf (pps-diff-cu-qp-delta-depth p) (ue br)))
    (setf (pps-cb-qp-offset p) (se br)
          (pps-cr-qp-offset p) (se br)
          (pps-slice-chroma-qp-offsets p) (= 1 (u1 br))
          (pps-weighted-pred p) (= 1 (u1 br))
          (pps-weighted-bipred p) (= 1 (u1 br))
          (pps-transquant-bypass p) (= 1 (u1 br))
          (pps-tiles-enabled p) (= 1 (u1 br))
          (pps-entropy-coding-sync p) (= 1 (u1 br)))
    (when (pps-tiles-enabled p)
      (setf (pps-num-tile-columns p) (1+ (ue br))
            (pps-num-tile-rows p) (1+ (ue br))
            (pps-uniform-spacing p) (= 1 (u1 br)))
      (unless (pps-uniform-spacing p)
        (setf (pps-column-widths p)
              (coerce (loop repeat (1- (pps-num-tile-columns p)) collect (1+ (ue br))) 'vector)
              (pps-row-heights p)
              (coerce (loop repeat (1- (pps-num-tile-rows p)) collect (1+ (ue br))) 'vector)))
      (setf (pps-loop-filter-across-tiles p) (= 1 (u1 br))))
    (setf (pps-loop-filter-across-slices p) (= 1 (u1 br))
          (pps-deblocking-control-present p) (= 1 (u1 br)))
    (when (pps-deblocking-control-present p)
      (setf (pps-deblocking-override-enabled p) (= 1 (u1 br))
            (pps-deblocking-disabled p) (= 1 (u1 br)))
      (unless (pps-deblocking-disabled p)
        (setf (pps-beta-offset p) (* 2 (se br))
              (pps-tc-offset p) (* 2 (se br)))))
    (when (= 1 (u1 br))                         ; pps_scaling_list_data_present_flag
      (setf (pps-scaling p) (%parse-scaling-list-data br)))
    (setf (pps-lists-modification-present p) (= 1 (u1 br))
          (pps-log2-parallel-merge-level p) (+ 2 (ue br))
          (pps-slice-header-extension p) (= 1 (u1 br)))
    p))

;;; ---- the slice segment header -----------------------------------------------------------------

(defstruct (slice (:conc-name sh-))
  (nal nil) (sps nil) (pps nil)
  (first-in-pic nil) (dependent nil)
  (segment-address 0)
  (type 2)                              ; 0 B, 1 P, 2 I — note the ORDER, which is not H.264's
  (pic-output t)
  (poc-lsb 0) (strps nil)
  (temporal-mvp nil)
  (sao-luma nil) (sao-chroma nil)
  (num-ref-idx-l0 1) (num-ref-idx-l1 1)
  (mvd-l1-zero nil) (cabac-init nil)
  (collocated-from-l0 t) (collocated-ref-idx 0)
  (max-merge-cand 5)
  (qp 26) (cb-qp-offset 0) (cr-qp-offset 0)
  (deblocking-disabled nil) (beta-offset 0) (tc-offset 0)
  (loop-filter-across-slices nil)
  (entry-points #() :type simple-vector))

(defun sh-i-slice-p (sh) (= 2 (sh-type sh)))
(defun sh-p-slice-p (sh) (= 1 (sh-type sh)))
(defun sh-b-slice-p (sh) (= 0 (sh-type sh)))

(defun parse-slice-header (br nal sps-table pps-table)
  "Parse a slice segment header (7.3.6.1), leaving BR at the first byte of the slice data.

   A DEPENDENT slice segment carries almost none of this: it names its picture parameter set and
   its address and then stops, and everything else is inherited from the last independent segment.
   That is a different thing from H.264's slices, which are all independent by construction — a
   dependent segment is a way of cutting a picture up for transport without giving up the
   prediction across the cut, so it is NOT a slice boundary for availability purposes."
  (let* ((sh (make-slice :nal nal))
         (first-p (= 1 (u1 br))))
    (setf (sh-first-in-pic sh) first-p)
    (when (nal-irap-p nal) (u1 br))             ; no_output_of_prior_pics_flag
    (let* ((pps (or (gethash (ue br) pps-table)
                    (%err "slice refers to a picture parameter set that has not been seen")))
           (sps (or (gethash (pps-sps-id pps) sps-table)
                    (%err "picture parameter set ~d refers to an unseen sequence parameter set"
                          (pps-id pps)))))
      (setf (sh-pps sh) pps (sh-sps sh) sps
            (sh-loop-filter-across-slices sh) (pps-loop-filter-across-slices pps)
            (sh-deblocking-disabled sh) (pps-deblocking-disabled pps)
            (sh-beta-offset sh) (pps-beta-offset pps)
            (sh-tc-offset sh) (pps-tc-offset pps))
      (unless first-p
        (when (pps-dependent-slices-enabled pps)
          (setf (sh-dependent sh) (= 1 (u1 br))))
        ;; the address is a CTB index, in exactly as many bits as the picture needs
        (setf (sh-segment-address sh)
              (ub br (max 1 (integer-length (1- (sps-ctbs sps)))))))
      (when (sh-dependent sh)
        (return-from parse-slice-header sh))
      (dotimes (i (pps-extra-slice-header-bits pps)) (u1 br))
      (setf (sh-type sh) (ue br))
      (unless (<= 0 (sh-type sh) 2)
        (%err "slice_type ~d, which is not one of B, P or I" (sh-type sh)))
      (when (pps-output-flag-present pps)
        (setf (sh-pic-output sh) (= 1 (u1 br))))
      (when (sps-separate-colour-planes sps) (ub br 2))
      (unless (nal-idr-p nal)
        (setf (sh-poc-lsb sh) (ub br (sps-log2-max-poc-lsb sps)))
        (if (zerop (u1 br))                     ; short_term_ref_pic_set_sps_flag
            (setf (sh-strps sh)
                  (%parse-strps br (length (sps-strps sps)) (sps-strps sps)
                                (length (sps-strps sps))))
            (let ((n (length (sps-strps sps))))
              (setf (sh-strps sh)
                    (aref (sps-strps sps)
                          (if (> n 1) (ub br (integer-length (1- n))) 0)))))
        (when (sps-long-term-present sps)
          (%err "long-term reference pictures are not supported"))
        (when (sps-temporal-mvp-enabled sps)
          (setf (sh-temporal-mvp sh) (= 1 (u1 br)))))
      (when (sps-sao-enabled sps)
        (setf (sh-sao-luma sh) (= 1 (u1 br))
              (sh-sao-chroma sh) (= 1 (u1 br))))
      (unless (sh-i-slice-p sh)
        (setf (sh-num-ref-idx-l0 sh) (pps-num-ref-idx-l0 pps)
              (sh-num-ref-idx-l1 sh) (pps-num-ref-idx-l1 pps))
        (when (= 1 (u1 br))                     ; num_ref_idx_active_override_flag
          (setf (sh-num-ref-idx-l0 sh) (1+ (ue br)))
          (when (sh-b-slice-p sh) (setf (sh-num-ref-idx-l1 sh) (1+ (ue br)))))
        (when (pps-lists-modification-present pps)
          (%err "reference picture list modification is not supported"))
        (when (sh-b-slice-p sh) (setf (sh-mvd-l1-zero sh) (= 1 (u1 br))))
        (when (pps-cabac-init-present pps) (setf (sh-cabac-init sh) (= 1 (u1 br))))
        (when (sh-temporal-mvp sh)
          (when (sh-b-slice-p sh) (setf (sh-collocated-from-l0 sh) (= 1 (u1 br))))
          (when (or (and (sh-collocated-from-l0 sh) (> (sh-num-ref-idx-l0 sh) 1))
                    (and (not (sh-collocated-from-l0 sh)) (> (sh-num-ref-idx-l1 sh) 1)))
            (setf (sh-collocated-ref-idx sh) (ue br))))
        (when (or (and (pps-weighted-pred pps) (sh-p-slice-p sh))
                  (and (pps-weighted-bipred pps) (sh-b-slice-p sh)))
          (%err "weighted prediction is not supported"))
        (setf (sh-max-merge-cand sh) (- 5 (ue br))))
      (setf (sh-qp sh) (+ (pps-init-qp pps) (se br)))
      (when (pps-slice-chroma-qp-offsets pps)
        (setf (sh-cb-qp-offset sh) (se br)
              (sh-cr-qp-offset sh) (se br)))
      (let ((override (and (pps-deblocking-override-enabled pps) (= 1 (u1 br)))))
        (when override
          (setf (sh-deblocking-disabled sh) (= 1 (u1 br)))
          (unless (sh-deblocking-disabled sh)
            (setf (sh-beta-offset sh) (* 2 (se br))
                  (sh-tc-offset sh) (* 2 (se br))))))
      (when (and (pps-loop-filter-across-slices pps)
                 (or (sh-sao-luma sh) (sh-sao-chroma sh) (not (sh-deblocking-disabled sh))))
        (setf (sh-loop-filter-across-slices sh) (= 1 (u1 br))))
      (when (or (pps-tiles-enabled pps) (pps-entropy-coding-sync pps))
        (let ((n (ue br)))
          (when (plusp n)
            (let ((len (1+ (ue br)))
                  (v (make-array n)))
              (dotimes (i n) (setf (aref v i) (1+ (ub br len))))
              (setf (sh-entry-points sh) v)))))
      (when (pps-slice-header-extension pps)
        (let ((n (ue br))) (dotimes (i n) (ub br 8))))
      ;; byte_alignment(): a one bit and then zeros.  The CABAC engine starts at the next byte.
      (u1 br)
      (byte-align br)
      sh)))

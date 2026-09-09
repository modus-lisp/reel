;;;; h264/params.lisp — the sequence and picture parameter sets, and the slice header.
;;;;
;;;; These are the three headers that say what everything else means: how big the picture is, how
;;;; wide the frame number is, whether the residuals are CAVLC or CABAC, what the quantizer starts
;;;; at, and whether the deblocking filter runs.  None of them can be skipped past, because every
;;;; field is an Exp-Golomb code whose length is its own value (see bits.lisp).
;;;;
;;;; WHAT IS DELIBERATELY NOT HERE.  Interlace (field pictures, MBAFF), multiple slice groups
;;;; (FMO/ASO), and the High-profile scaling matrices are parsed only far enough to REFUSE them
;;;; clearly.  Each is a large feature that Constrained Baseline does not use, and a decoder that
;;;; parsed them silently and then ignored them would produce a wrong picture instead of an error.

(in-package #:reel.h264)

;;; ---- sequence parameter set (7.3.2.1) --------------------------------------------------------

(defstruct (sps (:conc-name sps-))
  (id 0) (profile 0) (level 0)
  (chroma-format 1)                     ; 1 = 4:2:0, which is all this decodes
  (bit-depth-luma 8) (bit-depth-chroma 8)
  (log2-max-frame-num 4)
  (poc-type 0) (log2-max-poc-lsb 4)
  (delta-poc-always-zero nil) (offset-for-non-ref-pic 0) (offset-for-top-to-bottom 0)
  (poc-cycle '())
  (max-ref-frames 1)
  (gaps-allowed nil)
  (num-reorder-frames nil)              ; from the VUI, when it says; NIL when it does not
  (scale-4x4 nil)                       ; six 4x4 weight matrices in raster order, or NIL for flat
  (mb-width 0) (mb-height 0)            ; in macroblocks
  (frame-mbs-only t) (mb-adaptive nil)
  (direct-8x8 nil)
  (crop-left 0) (crop-right 0) (crop-top 0) (crop-bottom 0)
  (separate-colour-plane nil))

(defun sps-width (s)
  "The displayed width: the coded width less the cropping the SPS declares.

   Cropping is in CHROMA SAMPLE units for 4:2:0, so a crop of 2 removes 4 luma columns.  Getting
   this wrong is how a decoder ends up 4 pixels wider than ffmpeg on any file whose dimensions are
   not a multiple of 16."
  (- (* 16 (sps-mb-width s))
     (* 2 (+ (sps-crop-left s) (sps-crop-right s)))))

(defun sps-height (s)
  (- (* 16 (sps-mb-height s))
     (* 2 (+ (sps-crop-top s) (sps-crop-bottom s)))))

(defparameter +flat-scale-4x4+
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 16)
  "The weight matrix a stream that signals none is decoded with: every position 16.")
(declaim (type (simple-array (unsigned-byte 8) (16)) +flat-scale-4x4+))

(defun parse-scaling-list (br size)
  "One scaling list (7.3.2.1.1.1), in the SCAN order the bitstream sends it.

   Returns (values list use-default-p).  A zero delta at the very first position does not mean a
   weight of zero — it means `use the default matrix for this list\', and nothing more is sent.
   After that, a next scale of zero means the run simply continues at the last value."
  (let ((list (make-array size :element-type '(unsigned-byte 8)))
        (last 8) (next 8) (use-default nil))
    (dotimes (j size)
      (when (/= next 0)
        (let ((delta (se br)))
          (setf next (mod (+ last delta 256) 256))
          (when (and (zerop j) (zerop next)) (setf use-default t))))
      (setf (aref list j) (if (zerop next) last next)
            last (aref list j)))
    (values list use-default)))

(defun %unscan-4x4 (list)
  "A 4x4 scaling list from scan order into raster, which is how dequantisation indexes it."
  (let ((out (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (j 16 out) (setf (aref out (aref +zigzag-4x4+ j)) (aref list j)))))

(defun %default-4x4 (which)
  (let ((out (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (j 16 out) (setf (aref out j) (aref +default-scale-4x4+ which j)))))

(defun %unscan-8x8 (list)
  "An 8x8 scaling list from scan order into raster.  A different scan from the 4x4 one, which is
   why it is a different function and not the same one with a size argument."
  (let ((out (make-array 64 :element-type '(unsigned-byte 8))))
    (dotimes (j 64 out) (setf (aref out (aref +zigzag-8x8+ j)) (aref list j)))))

(defun %default-8x8 (which)
  (let ((out (make-array 64 :element-type '(unsigned-byte 8))))
    (dotimes (j 64 out) (setf (aref out j) (aref +default-scale-8x8+ which j)))))

(defun parse-scaling-matrices (br n8x8)
  "The lists a parameter set carries: six 4x4, then N8X8 of the 8x8 ones (7.3.2.1.1).

   THE COUNT IS NOT FIXED, and getting it wrong is not a scaling bug.  A sequence parameter set
   always carries the two 8x8 lists for 4:2:0; a picture parameter set carries them ONLY when it
   enables the 8x8 transform.  Reading two lists that are not there consumes the flags belonging to
   second_chroma_qp_index_offset, so the Cr plane alone decodes at the wrong quantiser while luma
   and Cb stay perfect.

   Returns a six-vector holding a raster-order matrix for each list the stream sent and NIL for
   each it omitted.  The fall-back is NOT applied here, because rule set B sends a picture
   parameter set to the SEQUENCE parameter set for its answer, and the two are parsed apart.
   RESOLVE-SCALING-MATRICES does it once both are in hand.

   The result is EIGHT long, not six: the two 8x8 lists live at 6 and 7, intra luma then inter
   luma.  For 4:2:0 those are the only two, because chroma has no 8x8 transform."
  (let ((out (make-array 8 :initial-element nil)))
    (dotimes (i 6)
      (when (= 1 (u1 br))
        (multiple-value-bind (list use-default) (parse-scaling-list br 16)
          (setf (aref out i)
                (if use-default (%default-4x4 (if (< i 3) 0 1)) (%unscan-4x4 list))))))
    (dotimes (i n8x8)
      (when (= 1 (u1 br))
        (multiple-value-bind (list use-default) (parse-scaling-list br 64)
          (setf (aref out (+ 6 i))
                (if use-default (%default-8x8 (logand i 1)) (%unscan-8x8 list))))))
    out))

(defun resolve-scaling-matrices (sps pps)
  "The six weight matrices in force for a slice, or NIL when everything is flat.

   The rules are positional and chained (Table 7-2): lists 1 and 2 fall back to the list before
   them rather than to a default, so a stream that omits the Cb list means `the same as luma', which
   is what encoders intend and never transmit.  Lists 0 and 3 fall back to the sequence's own list
   when a picture parameter set is overriding one, and to the specification's default otherwise."
  (let ((pic (and pps (pps-scale-4x4 pps)))
        (seq (and sps (sps-scale-4x4 sps))))
    (when (and (null pic) (null seq)) (return-from resolve-scaling-matrices nil))
    (let ((out (make-array 8)))
      (dotimes (i 8 out)
        (setf (aref out i)
              (or (and pic (aref pic i))
                  ;; a picture parameter set that overrides at all defers to the sequence's list
                  (and (null pic) seq (aref seq i))
                  (and pic seq (member i '(0 3 6 7)) (aref seq i))
                  (case i
                    (0 (%default-4x4 0))
                    (3 (%default-4x4 1))
                    ;; the 8x8 lists start their own chains rather than continuing the 4x4 one
                    (6 (%default-8x8 0))
                    (7 (%default-8x8 1))
                    (t (aref out (1- i))))))))))

(defun parse-sps (rbsp)
  "Parse a sequence parameter set from a NAL's RBSP."
  (let ((br (make-bitreader rbsp))
        (s (make-sps)))
    (setf (sps-profile s) (ub br 8))
    (ub br 8)                                          ; constraint flags + reserved
    (setf (sps-level s) (ub br 8)
          (sps-id s) (ue br))
    ;; the High-profile family carries chroma and bit-depth here; Baseline does not
    (when (member (sps-profile s) '(100 110 122 244 44 83 86 118 128 138 139 134 135))
      (setf (sps-chroma-format s) (ue br))
      (when (= (sps-chroma-format s) 3)
        (setf (sps-separate-colour-plane s) (= 1 (u1 br))))
      (setf (sps-bit-depth-luma s) (+ 8 (ue br))
            (sps-bit-depth-chroma s) (+ 8 (ue br)))
      (u1 br)                                          ; qpprime_y_zero_transform_bypass_flag
      (when (= 1 (u1 br))                              ; seq_scaling_matrix_present_flag
        (setf (sps-scale-4x4 s) (parse-scaling-matrices br 2))))
    (setf (sps-log2-max-frame-num s) (+ 4 (ue br))
          (sps-poc-type s) (ue br))
    (case (sps-poc-type s)
      (0 (setf (sps-log2-max-poc-lsb s) (+ 4 (ue br))))
      (1 (setf (sps-delta-poc-always-zero s) (= 1 (u1 br))
               (sps-offset-for-non-ref-pic s) (se br)
               (sps-offset-for-top-to-bottom s) (se br))
         (let ((n (ue br)))
           (setf (sps-poc-cycle s) (loop repeat n collect (se br)))))
      (2 nil)
      (t (%err "pic_order_cnt_type ~d" (sps-poc-type s))))
    (setf (sps-max-ref-frames s) (ue br)
          (sps-gaps-allowed s) (= 1 (u1 br))
          (sps-mb-width s) (1+ (ue br)))
    (let ((map-units (1+ (ue br))))
      (setf (sps-frame-mbs-only s) (= 1 (u1 br)))
      (unless (sps-frame-mbs-only s)
        (setf (sps-mb-adaptive s) (= 1 (u1 br)))
        (%err "field and MBAFF pictures are not supported"))
      ;; for a progressive stream the map units ARE the macroblock rows
      (setf (sps-mb-height s) map-units))
    (setf (sps-direct-8x8 s) (= 1 (u1 br)))
    (when (= 1 (u1 br))                                ; frame_cropping_flag
      (setf (sps-crop-left s) (ue br) (sps-crop-right s) (ue br)
            (sps-crop-top s) (ue br) (sps-crop-bottom s) (ue br)))
    ;; The VUI used to be skipped, on the grounds that nothing below it changes how a sample
    ;; decodes.  That stopped being true with B slices: max_num_reorder_frames says how far
    ;; display order can run behind decoding order, and guessing it wrong shows pictures in the
    ;; wrong sequence.  Nothing else in there is read, and a VUI we cannot parse is not an error —
    ;; the caller falls back to a conservative delay.
    (when (= 1 (u1 br))
      (ignore-errors (parse-vui br s)))
    (unless (= 1 (sps-chroma-format s))
      (%err "chroma_format_idc ~d is not supported (4:2:0 only)" (sps-chroma-format s)))
    (unless (and (= 8 (sps-bit-depth-luma s)) (= 8 (sps-bit-depth-chroma s)))
      (%err "~d-bit video is not supported (8-bit only)" (sps-bit-depth-luma s)))
    s))

;;; ---- picture parameter set (7.3.2.2) ------------------------------------------------------------

(defstruct (pps (:conc-name pps-))
  (id 0) (sps-id 0)
  (cabac nil)
  (bottom-field-order nil)
  (num-slice-groups 1)
  (num-ref-idx-l0 1) (num-ref-idx-l1 1)
  (weighted-pred nil) (weighted-bipred 0)
  (init-qp 26) (init-qs 26)
  (chroma-qp-offset 0) (second-chroma-qp-offset nil)
  (deblocking-control nil)
  (constrained-intra nil)
  (redundant-pic-cnt nil)
  (transform-8x8 nil)
  (scale-4x4 nil))

(defun parse-pps (rbsp)
  (let ((br (make-bitreader rbsp))
        (p (make-pps)))
    (setf (pps-id p) (ue br)
          (pps-sps-id p) (ue br)
          (pps-cabac p) (= 1 (u1 br))
          (pps-bottom-field-order p) (= 1 (u1 br))
          (pps-num-slice-groups p) (1+ (ue br)))
    (when (> (pps-num-slice-groups p) 1)
      (%err "multiple slice groups (FMO) are not supported"))
    (setf (pps-num-ref-idx-l0 p) (1+ (ue br))
          (pps-num-ref-idx-l1 p) (1+ (ue br))
          (pps-weighted-pred p) (= 1 (u1 br))
          (pps-weighted-bipred p) (ub br 2)
          (pps-init-qp p) (+ 26 (se br))
          (pps-init-qs p) (+ 26 (se br))
          (pps-chroma-qp-offset p) (se br)
          (pps-deblocking-control p) (= 1 (u1 br))
          (pps-constrained-intra p) (= 1 (u1 br))
          (pps-redundant-pic-cnt p) (= 1 (u1 br)))
    ;; the optional tail, present only in High-profile streams
    (when (more-rbsp-data-p br)
      (setf (pps-transform-8x8 p) (= 1 (u1 br)))
      (when (= 1 (u1 br))                              ; pic_scaling_matrix_present_flag
        (setf (pps-scale-4x4 p) (parse-scaling-matrices br (if (pps-transform-8x8 p) 2 0))))
      (setf (pps-second-chroma-qp-offset p) (se br)))
    p))

(defun pps-chroma-qp-offset-for (p plane)
  "The chroma QP offset for PLANE (0 = Cb, 1 = Cr).  A High-profile PPS may carry a second offset
   for Cr; without one both planes use the first."
  (if (and (= plane 1) (pps-second-chroma-qp-offset p))
      (pps-second-chroma-qp-offset p)
      (pps-chroma-qp-offset p)))

;;; ---- slice header (7.3.3) --------------------------------------------------------------------

(defconstant +slice-p+ 0) (defconstant +slice-b+ 1) (defconstant +slice-i+ 2)
(defconstant +slice-sp+ 3) (defconstant +slice-si+ 4)

(defstruct (slice-header (:conc-name sh-))
  (first-mb 0) (slice-type 0) (pps-id 0)
  (frame-num 0)
  (idr-pic-id 0)
  (poc-lsb 0) (delta-poc-bottom 0)
  (delta-poc-0 0) (delta-poc-1 0)       ; pic_order_cnt_type 1's per-picture corrections
  (redundant-pic-cnt 0)
  ;; NIL, not 1: this is the flag for "the slice did not override it", and a default of 1 makes
  ;; the fallback to the picture parameter set below unreachable — which reads no reference indices
  ;; at all on a stream with more than one reference, and desynchronises the slice.
  (num-ref-idx-l0 nil)
  (num-ref-idx-l1 nil)
  (direct-spatial nil)                  ; direct_spatial_mv_pred_flag
  (ref-list-reordering-l1 '())
  (cabac-init-idc 0)
  ;; explicit weighted prediction (7.4.3.2): a scale and an offset per reference picture, applied
  ;; to the motion-compensated prediction before the residual is added
  (weighted-p nil)
  (luma-log2-denom 0) (chroma-log2-denom 0)
  (scale-4x4 nil)                       ; six resolved weight matrices, or NIL for flat
  (luma-weights nil) (chroma-weights nil)
  (luma-weights-l1 nil) (chroma-weights-l1 nil)
  (ref-list-reordering '())
  (no-output-of-prior-pics nil) (long-term-reference nil)
  (adaptive-ref-marking nil)
  ;; the memory_management_control_operations, in the order they were sent.  Acted on, not merely
  ;; parsed: with a B pyramid an encoder retires its reference B pictures with these, and a decoder
  ;; that only slides a window keeps a picture the encoder dropped and then evicts the wrong one.
  (mmco '())
  (qp 26)
  (disable-deblocking 0) (alpha-offset 0) (beta-offset 0)
  (nal nil) (sps nil) (pps nil))

(defun slice-type-name (n)
  (case (mod n 5)
    (0 :p) (1 :b) (2 :i) (3 :sp) (4 :si)))

(defun sh-i-slice-p (sh) (eq (slice-type-name (sh-slice-type sh)) :i))
(defun sh-p-slice-p (sh) (eq (slice-type-name (sh-slice-type sh)) :p))
(defun sh-b-slice-p (sh) (eq (slice-type-name (sh-slice-type sh)) :b))

(defun %skip-hrd (br)
  "hrd_parameters (E.1.2), read only far enough to get past it."
  (let ((cpb-cnt (1+ (ue br))))
    (ub br 4) (ub br 4)                                ; bit_rate_scale, cpb_size_scale
    (dotimes (i cpb-cnt) (ue br) (ue br) (u1 br))
    (ub br 5) (ub br 5) (ub br 5) (ub br 5)))

(defun parse-vui (br s)
  "The video usability information (E.1.1), for the one field that matters here.

   Everything before max_num_reorder_frames has to be stepped over exactly, which is why this
   reads fields it then throws away: they are variable width, so skipping is walking."
  (when (= 1 (u1 br))                                  ; aspect_ratio_info_present_flag
    (let ((idc (ub br 8)))
      (when (= idc 255) (ub br 16) (ub br 16))))       ; Extended_SAR
  (when (= 1 (u1 br)) (u1 br))                         ; overscan
  (when (= 1 (u1 br))                                  ; video_signal_type
    (ub br 3) (u1 br)
    (when (= 1 (u1 br)) (ub br 8) (ub br 8) (ub br 8)))
  (when (= 1 (u1 br)) (ue br) (ue br))                 ; chroma_loc_info
  (when (= 1 (u1 br)) (ub br 32) (ub br 32) (u1 br))   ; timing_info
  (let ((nal-hrd (= 1 (u1 br))))
    (when nal-hrd (%skip-hrd br))
    (let ((vcl-hrd (= 1 (u1 br))))
      (when vcl-hrd (%skip-hrd br))
      (when (or nal-hrd vcl-hrd) (u1 br))))            ; low_delay_hrd_flag
  (u1 br)                                              ; pic_struct_present_flag
  (when (= 1 (u1 br))                                  ; bitstream_restriction_flag
    (u1 br) (ue br) (ue br) (ue br) (ue br)
    (setf (sps-num-reorder-frames s) (ue br))
    (ue br))                                           ; max_dec_frame_buffering
  s)

(defun parse-pred-weight-table (br sh)
  "pred_weight_table (7.3.3.2), list 0 only — which is all a P slice has.

   A reference with no flag of its own is not unweighted-by-omission: it takes the DEFAULT weight,
   which is 1 at the current denominator, and that is not the same as skipping the arithmetic."
  (let* ((n (max 1 (or (sh-num-ref-idx-l0 sh) 1)))
         (lw (make-array (list n 2) :element-type '(signed-byte 32)))
         (cw (make-array (list n 2 2) :element-type '(signed-byte 32))))
    (setf (sh-weighted-p sh) t
          (sh-luma-log2-denom sh) (ue br)
          (sh-chroma-log2-denom sh) (ue br))
    (dotimes (i n)
      (setf (aref lw i 0) (ash 1 (sh-luma-log2-denom sh)) (aref lw i 1) 0)
      (when (= 1 (u1 br))
        (setf (aref lw i 0) (se br) (aref lw i 1) (se br)))
      (dotimes (j 2)
        (setf (aref cw i j 0) (ash 1 (sh-chroma-log2-denom sh)) (aref cw i j 1) 0))
      (when (= 1 (u1 br))
        (dotimes (j 2)
          (setf (aref cw i j 0) (se br) (aref cw i j 1) (se br)))))
    (setf (sh-luma-weights sh) lw (sh-chroma-weights sh) cw)
    (when (sh-b-slice-p sh)
      (let* ((n1 (max 1 (or (sh-num-ref-idx-l1 sh) 1)))
             (lw1 (make-array (list n1 2) :element-type '(signed-byte 32)))
             (cw1 (make-array (list n1 2 2) :element-type '(signed-byte 32))))
        (dotimes (i n1)
          (setf (aref lw1 i 0) (ash 1 (sh-luma-log2-denom sh)) (aref lw1 i 1) 0)
          (when (= 1 (u1 br))
            (setf (aref lw1 i 0) (se br) (aref lw1 i 1) (se br)))
          (dotimes (j 2)
            (setf (aref cw1 i j 0) (ash 1 (sh-chroma-log2-denom sh)) (aref cw1 i j 1) 0))
          (when (= 1 (u1 br))
            (dotimes (j 2)
              (setf (aref cw1 i j 0) (se br) (aref cw1 i j 1) (se br)))))
        (setf (sh-luma-weights-l1 sh) lw1 (sh-chroma-weights-l1 sh) cw1)))
    sh))

(defun parse-slice-header (br nal sps-table pps-table)
  "Parse a slice header from BR, which is positioned at the start of a slice NAL's RBSP.
   Leaves BR positioned at the first macroblock's data."
  (let ((sh (make-slice-header :nal nal)))
    (setf (sh-first-mb sh) (ue br)
          (sh-slice-type sh) (ue br)
          (sh-pps-id sh) (ue br))
    (let* ((pps (or (gethash (sh-pps-id sh) pps-table)
                    (%err "slice refers to PPS ~d, which has not been seen" (sh-pps-id sh))))
           (sps (or (gethash (pps-sps-id pps) sps-table)
                    (%err "PPS ~d refers to SPS ~d, which has not been seen"
                          (sh-pps-id sh) (pps-sps-id pps)))))
      (setf (sh-pps sh) pps (sh-sps sh) sps
            (sh-scale-4x4 sh) (resolve-scaling-matrices sps pps))
      (setf (sh-frame-num sh) (ub br (sps-log2-max-frame-num sps)))
      ;; frame_mbs_only_flag is asserted in PARSE-SPS, so there is no field_pic_flag here
      (when (nal-idr-p nal) (setf (sh-idr-pic-id sh) (ue br)))
      (when (= 0 (sps-poc-type sps))
        (setf (sh-poc-lsb sh) (ub br (sps-log2-max-poc-lsb sps)))
        (when (pps-bottom-field-order pps) (setf (sh-delta-poc-bottom sh) (se br))))
      (when (and (= 1 (sps-poc-type sps)) (not (sps-delta-poc-always-zero sps)))
        (setf (sh-delta-poc-0 sh) (se br))
        (when (pps-bottom-field-order pps) (setf (sh-delta-poc-1 sh) (se br))))
      (when (pps-redundant-pic-cnt pps) (setf (sh-redundant-pic-cnt sh) (ue br)))
      ;; which of the two direct prediction methods a B slice uses, chosen per slice
      (when (sh-b-slice-p sh)
        (setf (sh-direct-spatial sh) (= 1 (u1 br))))
      (when (or (sh-p-slice-p sh) (sh-b-slice-p sh))
        (when (= 1 (u1 br))                            ; num_ref_idx_active_override_flag
          (setf (sh-num-ref-idx-l0 sh) (1+ (ue br)))
          (when (sh-b-slice-p sh) (setf (sh-num-ref-idx-l1 sh) (1+ (ue br)))))
        (unless (sh-num-ref-idx-l0 sh)
          (setf (sh-num-ref-idx-l0 sh) (pps-num-ref-idx-l0 pps)))
        (when (and (sh-b-slice-p sh) (null (sh-num-ref-idx-l1 sh)))
          (setf (sh-num-ref-idx-l1 sh) (pps-num-ref-idx-l1 pps))))
      (unless (or (sh-i-slice-p sh) (sh-p-slice-p sh) (sh-b-slice-p sh))
        (%err "slice_type ~d (~a) is not supported" (sh-slice-type sh)
              (slice-type-name (sh-slice-type sh))))
      ;; reference picture list modification, one list for a P slice and two for a B
      (unless (sh-i-slice-p sh)
        (when (= 1 (u1 br))                            ; ref_pic_list_modification_flag_l0
          (loop for op = (ue br)
                until (= op 3)
                do (push (cons op (ue br)) (sh-ref-list-reordering sh)))
          (setf (sh-ref-list-reordering sh) (nreverse (sh-ref-list-reordering sh))))
        (when (sh-b-slice-p sh)
          (when (= 1 (u1 br))                          ; ref_pic_list_modification_flag_l1
            (loop for op = (ue br)
                  until (= op 3)
                  do (push (cons op (ue br)) (sh-ref-list-reordering-l1 sh)))
            (setf (sh-ref-list-reordering-l1 sh) (nreverse (sh-ref-list-reordering-l1 sh))))))
      (when (or (and (pps-weighted-pred pps) (sh-p-slice-p sh))
                (and (= 1 (pps-weighted-bipred pps)) (sh-b-slice-p sh)))
        (parse-pred-weight-table br sh))
      ;; decoded reference picture marking
      (when (plusp (nal-ref-idc nal))
        (cond ((nal-idr-p nal)
               (setf (sh-no-output-of-prior-pics sh) (= 1 (u1 br))
                     (sh-long-term-reference sh) (= 1 (u1 br)))
               (when (sh-long-term-reference sh)
                 (%err "long-term reference pictures are not supported")))
              (t
               (setf (sh-adaptive-ref-marking sh) (= 1 (u1 br)))
               (when (sh-adaptive-ref-marking sh)
                 (loop for op = (ue br)
                       until (zerop op)
                       do (case op
                            ;; 1: retire one short-term reference, named by how far back it is
                            (1 (push (cons 1 (ue br)) (sh-mmco sh)))
                            ;; 4: raise or lower the long-term ceiling.  With no long-term
                            ;; references there is nothing for it to evict, so it is a no-op here —
                            ;; but the value still has to be read or the next operation is garbage
                            (4 (ue br))
                            ((2 3 6) (ue br) (when (= op 3) (ue br))
                             (%err "long-term reference pictures are not supported"))
                            (5 (%err "memory management control operation 5 (reset all references)"))
                            (t (%err "memory management control operation ~d" op))))
                 (setf (sh-mmco sh) (nreverse (sh-mmco sh)))))))
      ;; 7.3.3 puts cabac_init_idc HERE: after the reference picture marking and immediately
      ;; before slice_qp_delta.  A bit read in the wrong place costs the quantiser and everything
      ;; after it, so the order matters more than it looks.
      (when (and (pps-cabac pps) (not (sh-i-slice-p sh)))
        (setf (sh-cabac-init-idc sh) (ue br)))
      (setf (sh-qp sh) (+ (pps-init-qp pps) (se br)))
      (when (pps-deblocking-control pps)
        (setf (sh-disable-deblocking sh) (ue br))
        (unless (= 1 (sh-disable-deblocking sh))
          (setf (sh-alpha-offset sh) (* 2 (se br))
                (sh-beta-offset sh) (* 2 (se br)))))
      sh)))

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
        (%err "scaling matrices are not supported (High profile)")))
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
    ;; the VUI is not read: nothing below it changes how a sample decodes
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
  (transform-8x8 nil))

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
      (when (= 1 (u1 br)) (%err "picture scaling matrices are not supported"))
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
  (redundant-pic-cnt 0)
  ;; NIL, not 1: this is the flag for "the slice did not override it", and a default of 1 makes
  ;; the fallback to the picture parameter set below unreachable — which reads no reference indices
  ;; at all on a stream with more than one reference, and desynchronises the slice.
  (num-ref-idx-l0 nil)
  (ref-list-reordering '())
  (no-output-of-prior-pics nil) (long-term-reference nil)
  (adaptive-ref-marking nil)
  (qp 26)
  (disable-deblocking 0) (alpha-offset 0) (beta-offset 0)
  (nal nil) (sps nil) (pps nil))

(defun slice-type-name (n)
  (case (mod n 5)
    (0 :p) (1 :b) (2 :i) (3 :sp) (4 :si)))

(defun sh-i-slice-p (sh) (eq (slice-type-name (sh-slice-type sh)) :i))
(defun sh-p-slice-p (sh) (eq (slice-type-name (sh-slice-type sh)) :p))

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
      (setf (sh-pps sh) pps (sh-sps sh) sps)
      (when (pps-cabac pps) (%err "CABAC is not supported (Baseline is CAVLC)"))
      (setf (sh-frame-num sh) (ub br (sps-log2-max-frame-num sps)))
      ;; frame_mbs_only_flag is asserted in PARSE-SPS, so there is no field_pic_flag here
      (when (nal-idr-p nal) (setf (sh-idr-pic-id sh) (ue br)))
      (when (= 0 (sps-poc-type sps))
        (setf (sh-poc-lsb sh) (ub br (sps-log2-max-poc-lsb sps)))
        (when (pps-bottom-field-order pps) (setf (sh-delta-poc-bottom sh) (se br))))
      (when (and (= 1 (sps-poc-type sps)) (not (sps-delta-poc-always-zero sps)))
        (se br)                                        ; delta_pic_order_cnt[0]
        (when (pps-bottom-field-order pps) (se br)))
      (when (pps-redundant-pic-cnt pps) (setf (sh-redundant-pic-cnt sh) (ue br)))
      ;; B slices would read direct_spatial_mv_pred_flag here; we refuse them below
      (when (sh-p-slice-p sh)
        (when (= 1 (u1 br))                            ; num_ref_idx_active_override_flag
          (setf (sh-num-ref-idx-l0 sh) (1+ (ue br))))
        (unless (sh-num-ref-idx-l0 sh)
          (setf (sh-num-ref-idx-l0 sh) (pps-num-ref-idx-l0 pps))))
      (unless (or (sh-i-slice-p sh) (sh-p-slice-p sh))
        (%err "slice_type ~d (~a) is not supported" (sh-slice-type sh)
              (slice-type-name (sh-slice-type sh))))
      ;; reference picture list modification
      (unless (sh-i-slice-p sh)
        (when (= 1 (u1 br))                            ; ref_pic_list_modification_flag_l0
          (loop for op = (ue br)
                until (= op 3)
                do (push (cons op (ue br)) (sh-ref-list-reordering sh)))
          (setf (sh-ref-list-reordering sh) (nreverse (sh-ref-list-reordering sh)))))
      (when (and (pps-weighted-pred pps) (sh-p-slice-p sh))
        (%err "weighted prediction is not supported"))
      ;; decoded reference picture marking
      (when (plusp (nal-ref-idc nal))
        (cond ((nal-idr-p nal)
               (setf (sh-no-output-of-prior-pics sh) (= 1 (u1 br))
                     (sh-long-term-reference sh) (= 1 (u1 br))))
              (t
               (setf (sh-adaptive-ref-marking sh) (= 1 (u1 br)))
               (when (sh-adaptive-ref-marking sh)
                 ;; memory_management_control_operation loop; the operations are read so the
                 ;; bit position stays right, and only sliding-window marking is implemented
                 (loop for op = (ue br)
                       until (zerop op)
                       do (case op
                            ((1 3) (ue br) (when (= op 3) (ue br)))
                            (2 (ue br))
                            (4 (ue br))
                            (6 (ue br))
                            (5 nil)
                            (t (%err "memory management control operation ~d" op))))))))
      (setf (sh-qp sh) (+ (pps-init-qp pps) (se br)))
      (when (pps-deblocking-control pps)
        (setf (sh-disable-deblocking sh) (ue br))
        (unless (= 1 (sh-disable-deblocking sh))
          (setf (sh-alpha-offset sh) (* 2 (se br))
                (sh-beta-offset sh) (* 2 (se br)))))
      sh)))

;;;; mpeg4/headers.lisp — the video object layer and the video object plane.
;;;;
;;;; MPEG-4's headers are longer than MPEG-2's out of ambition rather than complexity: the layer
;;;; header describes sprites, scalability, arbitrary shapes and complexity estimation, almost none
;;;; of which any real file uses.  What matters for decoding a DivX or an XviD is about a dozen of
;;;; its bits — the quantiser type, the two matrices, quarter-sample, and how many bits a time
;;;; increment takes — and the rest has to be READ ANYWAY, because everything here is a bit field
;;;; and skipping a field means losing the ones after it.

(in-package #:reel.mpeg4)

(defconstant +vop-i+ 0)
(defconstant +vop-p+ 1)
(defconstant +vop-b+ 2)
(defconstant +vop-s+ 3)

(defun vop-type-name (n) (case n (0 "I") (1 "P") (2 "B") (3 "S") (t "?")))

(defstruct (vol (:conc-name vol-))
  (width 0 :type fixnum) (height 0 :type fixnum)
  (mb-width 0 :type fixnum) (mb-height 0 :type fixnum)
  (vo-type 1 :type fixnum)
  (version 1 :type fixnum)
  (time-increment-bits 1 :type fixnum)
  (time-resolution 1 :type fixnum)
  (low-delay nil)
  (progressive-p t)
  (quant-precision 5 :type fixnum)
  (mpeg-quant nil)                              ; the MPEG-style quantiser instead of H.263's
  (quarter-sample nil)
  (resync-marker nil)
  (data-partitioning nil)
  (sprite-usage 0 :type fixnum)
  (complexity-i 0 :type fixnum) (complexity-p 0 :type fixnum) (complexity-b 0 :type fixnum)
  (intra-matrix nil) (inter-matrix nil))

(defstruct (vop (:conc-name vop-))
  (coding-type 0 :type fixnum)
  (time-increment 0 :type fixnum)
  (time-base 0 :type fixnum)
  (coded t)
  (rounding 0 :type fixnum)
  (intra-dc-threshold 0 :type fixnum)
  (alternate-scan nil)
  (top-field-first t)
  (qscale 1 :type fixnum)
  (f-code 1 :type fixnum) (b-code 1 :type fixnum))

(defun %read-matrix (br)
  "A quantiser matrix, in zig-zag order, terminated early by a zero whose value then repeats."
  (let ((out (make-array 64 :element-type '(unsigned-byte 8)))
        (last 0))
    (dotimes (i 64)
      (let ((v (read-bits br 8)))
        (when (zerop v)
          ;; a zero ends the list and the LAST value fills the rest, which is how a matrix that is
          ;; constant above some frequency is sent in a dozen bytes instead of sixty-four
          (loop for j from i below 64 do (setf (aref out (aref +zigzag+ j)) last))
          (return-from %read-matrix out))
        (setf last v (aref out (aref +zigzag+ i)) v)))
    out))

(defun %log2 (n) (if (<= n 1) 0 (integer-length (1- n))))

(defun parse-vol-header (br)
  "The video object layer header (6.2.3)."
  (let ((v (make-vol)))
    (read-bit br)                                ; random_accessible_vol
    (setf (vol-vo-type v) (read-bits br 8))
    (when (member (vol-vo-type v) '(#x0e #x0f))
      (%err "the Simple Studio profile is not supported"))
    (setf (vol-version v) (if (= 1 (read-bit br))
                              (prog1 (read-bits br 4) (read-bits br 3))
                              1))
    (let ((aspect (read-bits br 4)))
      (when (= aspect 15) (read-bits br 8) (read-bits br 8)))
    (if (= 1 (read-bit br))                      ; vol_control_parameters
        (progn
          (let ((chroma (read-bits br 2)))
            (unless (= chroma 1) (%err "chroma_format ~d is not supported (4:2:0 only)" chroma)))
          (setf (vol-low-delay v) (= 1 (read-bit br)))
          (when (= 1 (read-bit br))              ; vbv_parameters
            (read-bits br 15) (marker-bit br)
            (read-bits br 15) (marker-bit br)
            (read-bits br 15) (marker-bit br)
            (read-bits br 3) (read-bits br 11) (marker-bit br)
            (read-bits br 15) (marker-bit br)))
        (setf (vol-low-delay v) (member (vol-vo-type v) '(1 17))))
    (let ((shape (read-bits br 2)))
      (unless (zerop shape) (%err "only rectangular video objects are supported")))
    (marker-bit br)
    (setf (vol-time-resolution v) (read-bits br 16))
    (when (zerop (vol-time-resolution v)) (%err "vop_time_increment_resolution is zero"))
    (setf (vol-time-increment-bits v) (max 1 (%log2 (vol-time-resolution v))))
    (marker-bit br)
    (when (= 1 (read-bit br))                    ; fixed_vop_rate
      (read-bits br (vol-time-increment-bits v)))
    (marker-bit br)
    (setf (vol-width v) (read-bits br 13))
    (marker-bit br)
    (setf (vol-height v) (read-bits br 13))
    (marker-bit br)
    (setf (vol-mb-width v) (ceiling (vol-width v) 16)
          (vol-mb-height v) (ceiling (vol-height v) 16))
    (setf (vol-progressive-p v) (= 0 (read-bit br)))   ; interlaced
    (unless (vol-progressive-p v) (%err "interlaced video objects are not supported"))
    (read-bit br)                                ; obmc_disable
    (setf (vol-sprite-usage v) (if (= 1 (vol-version v)) (read-bit br) (read-bits br 2)))
    (unless (zerop (vol-sprite-usage v))
      (%err "sprites and global motion compensation are not supported"))
    (setf (vol-quant-precision v)
          (if (= 1 (read-bit br))                ; not_8_bit
              (let ((q (read-bits br 4)))
                (unless (= 8 (read-bits br 4)) (%err "only 8 bits per sample are supported"))
                (if (<= 3 q 9) q 5))
              5))
    (when (setf (vol-mpeg-quant v) (= 1 (read-bit br)))
      (setf (vol-intra-matrix v) (copy-seq +default-intra-matrix+)
            (vol-inter-matrix v) (copy-seq +default-inter-matrix+))
      (when (= 1 (read-bit br)) (setf (vol-intra-matrix v) (%read-matrix br)))
      (when (= 1 (read-bit br)) (setf (vol-inter-matrix v) (%read-matrix br))))
    (setf (vol-quarter-sample v) (and (/= 1 (vol-version v)) (= 1 (read-bit br))))
    ;; complexity estimation: a pile of flags whose only effect on a decoder is that each one set
    ;; means a byte of nothing to skip in every picture header
    (when (= 0 (read-bit br))                    ; complexity_estimation_disable
      (let ((method (read-bits br 2)))
        (when (< method 2)
          (flet ((flags (n) (let ((s 0)) (dotimes (i n s) (incf s (* 8 (read-bit br)))))))
            (when (= 0 (read-bit br)) (incf (vol-complexity-i v) (flags 6)))
            (when (= 0 (read-bit br))
              (incf (vol-complexity-i v) (* 8 (read-bit br)))
              (incf (vol-complexity-p v) (* 8 (read-bit br)))
              (incf (vol-complexity-p v) (* 8 (read-bit br)))
              (incf (vol-complexity-i v) (* 8 (read-bit br))))
            (marker-bit br)
            (when (= 0 (read-bit br))
              (incf (vol-complexity-i v) (* 8 (read-bit br)))
              (incf (vol-complexity-i v) (* 8 (read-bit br)))
              (incf (vol-complexity-i v) (* 8 (read-bit br)))
              (incf (vol-complexity-i v) (* 4 (read-bit br))))
            (when (= 0 (read-bit br))
              (incf (vol-complexity-p v) (* 8 (read-bit br)))
              (incf (vol-complexity-p v) (* 8 (read-bit br)))
              (incf (vol-complexity-b v) (* 8 (read-bit br)))
              (incf (vol-complexity-p v) (* 8 (read-bit br)))
              (incf (vol-complexity-p v) (* 8 (read-bit br)))
              (incf (vol-complexity-p v) (* 8 (read-bit br))))
            (marker-bit br)
            (when (= method 1)
              (incf (vol-complexity-i v) (* 8 (read-bit br)))
              (incf (vol-complexity-p v) (* 8 (read-bit br))))))))
    (setf (vol-resync-marker v) (= 0 (read-bit br)))
    (when (setf (vol-data-partitioning v) (= 1 (read-bit br)))
      (read-bit br)                              ; reversible_vlc
      (%err "data partitioning is not supported"))
    (when (/= 1 (vol-version v))
      (when (= 1 (read-bit br)) (read-bits br 2) (read-bit br))   ; new_pred
      (when (= 1 (read-bit br)) (%err "reduced resolution VOPs are not supported")))
    (when (= 1 (read-bit br)) (%err "scalable video objects are not supported"))
    v))

(defun parse-vop-header (br v)
  "The video object plane header (6.2.5).  Returns a VOP, or one with CODED nil for a picture the
   stream declined to send at all — which is legal and means `show the last one again'."
  (let ((p (make-vop)))
    (setf (vop-coding-type p) (read-bits br 2))
    ;; modulo_time_base: a run of ones counting whole seconds, terminated by a zero
    (let ((incr 0))
      (loop while (= 1 (read-bit br)) do (incf incr))
      (setf (vop-time-base p) incr))
    (marker-bit br)
    (setf (vop-time-increment p) (read-bits br (vol-time-increment-bits v)))
    (marker-bit br)
    (unless (= 1 (read-bit br))                  ; vop_coded
      (setf (vop-coded p) nil)
      (return-from parse-vop-header p))
    (when (or (= (vop-coding-type p) +vop-p+)
              (= (vop-coding-type p) +vop-s+))
      (setf (vop-rounding p) (read-bit br)))
    (skip-bits br (vol-complexity-i v))
    (unless (= (vop-coding-type p) +vop-i+) (skip-bits br (vol-complexity-p v)))
    (when (= (vop-coding-type p) +vop-b+) (skip-bits br (vol-complexity-b v)))
    (setf (vop-intra-dc-threshold p) (aref +dc-threshold+ (read-bits br 3)))
    (if (vol-progressive-p v)
        (setf (vop-alternate-scan p) nil)
        (setf (vop-top-field-first p) (= 1 (read-bit br))
              (vop-alternate-scan p) (= 1 (read-bit br))))
    (setf (vop-qscale p) (read-bits br (vol-quant-precision v)))
    (when (zerop (vop-qscale p)) (%err "vop_quant is zero"))
    (unless (= (vop-coding-type p) +vop-i+)
      (setf (vop-f-code p) (max 1 (read-bits br 3))))
    (when (= (vop-coding-type p) +vop-b+)
      (setf (vop-b-code p) (max 1 (read-bits br 3))))
    p))

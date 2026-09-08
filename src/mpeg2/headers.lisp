;;;; mpeg2/headers.lisp — sequence, group, picture, and the extensions that turn MPEG-1 into MPEG-2.
;;;;
;;;; THE TWO STANDARDS ARE ONE BITSTREAM.  An MPEG-2 sequence header is byte for byte an MPEG-1
;;;; sequence header; everything MPEG-2 added arrives afterwards in EXTENSIONS, each a start code
;;;; followed by a four-bit identifier.  A decoder that reads the base headers and then whatever
;;;; extensions follow decodes both formats without ever branching on which it is holding — which is
;;;; why this file has no MPEG-1 special case in it and the entropy decoder has exactly two.

(in-package #:reel.mpeg2)

(defconstant +pic-i+ 1)
(defconstant +pic-p+ 2)
(defconstant +pic-b+ 3)
(defconstant +pic-d+ 4)

(defun picture-type-name (n)
  (case n (1 "I") (2 "P") (3 "B") (4 "D") (t (format nil "~d" n))))

(defstruct (sequence-header (:conc-name seq-))
  (width 0 :type fixnum) (height 0 :type fixnum)
  (mb-width 0 :type fixnum) (mb-height 0 :type fixnum)
  (aspect 1 :type fixnum)
  (frame-rate-code 3 :type fixnum)
  (frame-rate 25d0 :type double-float)
  ;; NIL until a sequence extension arrives, and that absence is the only thing that distinguishes
  ;; an MPEG-1 sequence from an MPEG-2 one
  (mpeg2-p nil)
  (progressive-p t)
  (chroma-format 1 :type fixnum)                ; 1 = 4:2:0, 2 = 4:2:2, 3 = 4:4:4
  (low-delay nil)
  ;; the four weight matrices, in RASTER order.  Chroma has its own pair only in 4:2:2 and above,
  ;; but the syntax to load them exists at every chroma format and has to be read either way.
  (intra-matrix nil) (non-intra-matrix nil)
  (chroma-intra-matrix nil) (chroma-non-intra-matrix nil))

(defstruct (picture-header (:conc-name ph-))
  (temporal-reference 0 :type fixnum)
  (coding-type 1 :type fixnum)
  (full-pel-forward nil) (full-pel-backward nil)
  ;; f_code[list][component]: how many bits a motion vector residual carries, and so how far a
  ;; vector may reach.  MPEG-1 carries one per direction; MPEG-2 carries four.
  (f-code (make-array '(2 2) :element-type '(signed-byte 32) :initial-element 15) :type (simple-array (signed-byte 32) (2 2)))
  (intra-dc-precision 0 :type fixnum)
  (structure 3 :type fixnum)                    ; 1 top field, 2 bottom field, 3 frame
  (top-field-first t)
  (frame-pred-frame-dct t)
  (concealment-motion-vectors nil)
  (q-scale-type nil)
  (intra-vlc-format nil)
  (alternate-scan nil)
  (repeat-first-field nil)
  (progressive-frame t)
  (mpeg2-p nil))

(defun %unscan (list)
  "A quantiser matrix from the zig-zag order it is transmitted in into raster order."
  (let ((out (make-array 64 :element-type '(unsigned-byte 8))))
    (dotimes (j 64 out) (setf (aref out (aref +zigzag+ j)) (aref list j)))))

(defun %read-matrix (br)
  (let ((v (make-array 64 :element-type '(unsigned-byte 8))))
    (dotimes (i 64) (setf (aref v i) (read-bits br 8)))
    (%unscan v)))

(defparameter +frame-rates+
  (vector 0d0 (/ 24000d0 1001) 24d0 25d0 (/ 30000d0 1001) 30d0 50d0 (/ 60000d0 1001) 60d0
          0d0 0d0 0d0 0d0 0d0 0d0 0d0)
  "Table 6-4.  Codes 9 upwards are reserved and appear only in broken streams.")

(defun parse-sequence-header (br)
  "The base sequence header (6.2.2.1), which MPEG-1 and MPEG-2 share exactly."
  (let* ((w (read-bits br 12)) (h (read-bits br 12))
         (aspect (read-bits br 4))
         (fr (read-bits br 4)))
    (when (or (zerop w) (zerop h))
      (%err "a sequence header with a ~dx~d picture size" w h))
    (read-bits br 18)                            ; bit_rate_value
    (read-bit br)                                ; marker_bit
    (read-bits br 10)                            ; vbv_buffer_size_value
    (read-bit br)                                ; constrained_parameters_flag
    (let ((s (make-sequence-header
              :width w :height h
              :mb-width (ceiling w 16) :mb-height (ceiling h 16)
              :aspect aspect :frame-rate-code fr
              :frame-rate (if (< fr 16) (aref +frame-rates+ fr) 25d0))))
      (setf (seq-intra-matrix s)
            (if (= 1 (read-bit br)) (%read-matrix br) (copy-seq +default-intra-matrix+)))
      (setf (seq-non-intra-matrix s)
            (if (= 1 (read-bit br)) (%read-matrix br) (copy-seq +default-non-intra-matrix+)))
      ;; chroma starts as a copy of luma and is only replaced by a quant matrix extension
      (setf (seq-chroma-intra-matrix s) (seq-intra-matrix s)
            (seq-chroma-non-intra-matrix s) (seq-non-intra-matrix s))
      s)))

(defun parse-sequence-extension (br s)
  "Extension 1: what makes the sequence MPEG-2 (6.2.2.3)."
  (read-bits br 8)                               ; profile_and_level_indication
  (setf (seq-mpeg2-p s) t
        (seq-progressive-p s) (= 1 (read-bit br))
        (seq-chroma-format s) (read-bits br 2))
  (let ((hx (read-bits br 2)) (vx (read-bits br 2)))
    ;; the extension bits are the HIGH bits of the size, so a 1920-wide picture is not expressible
    ;; without them and a decoder that skips the extension gets a plausible smaller picture
    (setf (seq-width s) (logior (seq-width s) (ash hx 12))
          (seq-height s) (logior (seq-height s) (ash vx 12))
          (seq-mb-width s) (ceiling (seq-width s) 16)))
  ;; AN INTERLACED SEQUENCE IS CODED IN AN EVEN NUMBER OF MACROBLOCK ROWS, always, because a field
  ;; picture has half of them and half of an odd number is not a number of macroblock rows.  So a
  ;; 144-line interlaced picture is coded as 160 lines and displayed as 144 — and a decoder that
  ;; sizes the frame from the displayed height finds a tenth slice it has nowhere to put.
  (setf (seq-mb-height s)
        (if (seq-progressive-p s)
            (ceiling (seq-height s) 16)
            (* 2 (ceiling (seq-height s) 32))))
  (read-bits br 12)                              ; bit_rate_extension
  (read-bit br)                                  ; marker_bit
  (read-bits br 8)                               ; vbv_buffer_size_extension
  (setf (seq-low-delay s) (= 1 (read-bit br)))
  (let ((n (read-bits br 2)) (d (read-bits br 5)))
    (when (plusp (seq-frame-rate s))
      (setf (seq-frame-rate s) (/ (* (seq-frame-rate s) (1+ n)) (1+ d)))))
  (unless (= 1 (seq-chroma-format s))
    (%err "chroma_format ~d is not supported (4:2:0 only)" (seq-chroma-format s)))
  s)

(defun parse-quant-matrix-extension (br s)
  "Extension 3: replacement weight matrices, mid-sequence (6.2.3.2)."
  (when (= 1 (read-bit br)) (setf (seq-intra-matrix s) (%read-matrix br)
                                  (seq-chroma-intra-matrix s) (seq-intra-matrix s)))
  (when (= 1 (read-bit br)) (setf (seq-non-intra-matrix s) (%read-matrix br)
                                  (seq-chroma-non-intra-matrix s) (seq-non-intra-matrix s)))
  (when (= 1 (read-bit br)) (setf (seq-chroma-intra-matrix s) (%read-matrix br)))
  (when (= 1 (read-bit br)) (setf (seq-chroma-non-intra-matrix s) (%read-matrix br)))
  s)

(defun parse-picture-header (br)
  "The base picture header (6.2.3), MPEG-1 and MPEG-2 alike."
  (let* ((tr (read-bits br 10))
         (type (read-bits br 3))
         (ph (make-picture-header :temporal-reference tr :coding-type type)))
    (read-bits br 16)                            ; vbv_delay
    (when (or (= type +pic-p+) (= type +pic-b+))
      (setf (ph-full-pel-forward ph) (= 1 (read-bit br)))
      (let ((f (read-bits br 3)))
        (setf (aref (ph-f-code ph) 0 0) f (aref (ph-f-code ph) 0 1) f)))
    (when (= type +pic-b+)
      (setf (ph-full-pel-backward ph) (= 1 (read-bit br)))
      (let ((f (read-bits br 3)))
        (setf (aref (ph-f-code ph) 1 0) f (aref (ph-f-code ph) 1 1) f)))
    ;; extra_bit_picture: a 1 means another byte of extra_information_picture follows
    (loop while (= 1 (read-bit br)) do (read-bits br 8))
    (when (= type +pic-d+) (%err "D pictures are not supported"))
    (when (or (zerop type) (> type +pic-d+)) (%err "picture_coding_type ~d" type))
    ph))

(defun parse-picture-coding-extension (br ph)
  "Extension 8: everything MPEG-2 added to a picture (6.2.3.1)."
  (dotimes (i 2) (dotimes (j 2) (setf (aref (ph-f-code ph) i j) (read-bits br 4))))
  (setf (ph-mpeg2-p ph) t
        (ph-intra-dc-precision ph) (read-bits br 2)
        (ph-structure ph) (read-bits br 2)
        (ph-top-field-first ph) (= 1 (read-bit br))
        (ph-frame-pred-frame-dct ph) (= 1 (read-bit br))
        (ph-concealment-motion-vectors ph) (= 1 (read-bit br))
        (ph-q-scale-type ph) (= 1 (read-bit br))
        (ph-intra-vlc-format ph) (= 1 (read-bit br))
        (ph-alternate-scan ph) (= 1 (read-bit br))
        (ph-repeat-first-field ph) (= 1 (read-bit br)))
  (read-bit br)                                  ; chroma_420_type
  (setf (ph-progressive-frame ph) (= 1 (read-bit br)))
  (when (= 1 (read-bit br))                      ; composite_display_flag
    (read-bits br 20))
  ph)

(defun ph-quantiser-scale (ph code)
  "quantiser_scale from its code (7.4.2.2).  Two ladders, and a picture picks one."
  (declare (type fixnum code))
  (if (ph-q-scale-type ph)
      (aref +non-linear-qscale+ code)
      (* 2 code)))

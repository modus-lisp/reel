;;;; h264/tables.lisp — the CAVLC variable-length code tables, and the scan and quant tables.
;;;;
;;;; PROVENANCE.  Every number here is a value from ITU-T Rec. H.264: the CAVLC tables are Tables
;;;; 9-5 (coeff_token), 9-7 and 9-8 (total_zeros), 9-9 (chroma DC total_zeros) and 9-10
;;;; (run_before); the scan is Figure 8-8 and the dequantisation values are Table 8-15.  They were
;;;; transcribed MECHANICALLY rather than by hand and cross-checked against ffmpeg's
;;;; h264_cavlc.c, which is the same specification tables in another form.  A single wrong entry
;;;; in a VLC table gives a decoder that works on most macroblocks and desynchronises on one,
;;;; which is the worst kind of bug to find by reading; so none of it was typed.
;;;;
;;;; A (LENGTH, BITS) PAIR PER SYMBOL, not a decoding tree.  Decoding accumulates bits and looks
;;;; for the entry whose code and length both match.  That is a scan where a tree would be a walk,
;;;; and it is the right trade here: the tables are small, the code is visibly the table, and a
;;;; tree built from a mistyped table hides the mistake instead of showing it.

(in-package #:reel.h264)

;;; The tables are special variables, so without these DECLAIMs every AREF on one compiles to
;;; SB-KERNEL:HAIRY-DATA-VECTOR-REF — a full generic dispatch on the array's type, at run time,
;;; per lookup.  It measured at 7% of decode time.  Naming the type once here makes each of them a
;;; direct indexed read instead.
(declaim (type (simple-array (unsigned-byte 8) (* *))
               +coeff-token-len+ +coeff-token-bits+
               +chroma-dc-coeff-token-len+ +chroma-dc-coeff-token-bits+
               +total-zeros-len+ +total-zeros-bits+
               +chroma-dc-total-zeros-len+ +chroma-dc-total-zeros-bits+
               +run-len+ +run-bits+ +dequant-coeff+))
(declaim (type (simple-array (unsigned-byte 8) (*))
               +zigzag-4x4+ +dequant-class+ +qpc-from-qpy+))

(defparameter +coeff-token-len+
  ;; Table 9-5, indexed [nC range][4*total_coeff + trailing_ones].
  (make-array '(4 68) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 0 0 0 6 2 0 0 8 6 3 0 9 8 7 5 10 9 8 6 11 10 9 7 13 11 10 8 13 13 11 9 13 13 13 10 14 14 13 11 14 14 14 13 15 15 14 14 15 15 15 14 16 15 15 15 16 16 16 15 16 16 16 16 16 16 16 16)
                (2 0 0 0 6 2 0 0 6 5 3 0 7 6 6 4 8 6 6 4 8 7 7 5 9 8 8 6 11 9 9 6 11 11 11 7 12 11 11 9 12 12 12 11 12 12 12 11 13 13 13 12 13 13 13 13 13 14 13 13 14 14 14 13 14 14 14 14)
                (4 0 0 0 6 4 0 0 6 5 4 0 6 5 5 4 7 5 5 4 7 5 5 4 7 6 6 4 7 6 6 4 8 7 7 5 8 8 7 6 9 8 8 7 9 9 8 8 9 9 9 8 10 9 9 9 10 10 10 10 10 10 10 10 10 10 10 10)
                (6 0 0 0 6 6 0 0 6 6 6 0 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6)
                )))

(defparameter +coeff-token-bits+
  ;; Table 9-5, the codes for those lengths.
  (make-array '(4 68) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 0 0 0 5 1 0 0 7 4 1 0 7 6 5 3 7 6 5 3 7 6 5 4 15 6 5 4 11 14 5 4 8 10 13 4 15 14 9 4 11 10 13 12 15 14 9 12 11 10 13 8 15 1 9 12 11 14 13 8 7 10 9 12 4 6 5 8)
                (3 0 0 0 11 2 0 0 7 7 3 0 7 10 9 5 7 6 5 4 4 6 5 6 7 6 5 8 15 6 5 4 11 14 13 4 15 10 9 4 11 14 13 12 8 10 9 8 15 14 13 12 11 10 9 12 7 11 6 8 9 8 10 1 7 6 5 4)
                (15 0 0 0 15 14 0 0 11 15 13 0 8 12 14 12 15 10 11 11 11 8 9 10 9 14 13 9 8 10 9 8 15 14 13 13 11 14 10 12 15 10 13 12 11 14 9 12 8 10 13 8 13 7 9 12 9 12 11 10 5 8 7 6 1 4 3 2)
                (3 0 0 0 0 1 0 0 4 5 6 0 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63)
                )))

(defparameter +chroma-dc-coeff-token-len+
  ;; Table 9-5, the nC = -1 column, for the 2x2 chroma DC block.
  (make-array '(1 20) :element-type '(unsigned-byte 8)
              :initial-contents
              '((2 0 0 0 6 1 0 0 6 6 3 0 6 7 7 6 6 8 8 7)
                )))

(defparameter +chroma-dc-coeff-token-bits+
  ;; Table 9-5, chroma DC codes.
  (make-array '(1 20) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 0 0 0 7 1 0 0 4 6 1 0 3 3 2 5 2 3 2 0)
                )))

(defparameter +total-zeros-len+
  ;; Tables 9-7 and 9-8, indexed [total_coeff - 1][total_zeros].
  (make-array '(15 16) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 3 3 4 4 5 5 6 6 7 7 8 8 9 9 9)
                (3 3 3 3 3 4 4 4 4 5 5 6 6 6 6 0)
                (4 3 3 3 4 4 3 3 4 5 5 6 5 6 0 0)
                (5 3 4 4 3 3 3 4 3 4 5 5 5 0 0 0)
                (4 4 4 3 3 3 3 3 4 5 4 5 0 0 0 0)
                (6 5 3 3 3 3 3 3 4 3 6 0 0 0 0 0)
                (6 5 3 3 3 2 3 4 3 6 0 0 0 0 0 0)
                (6 4 5 3 2 2 3 3 6 0 0 0 0 0 0 0)
                (6 6 4 2 2 3 2 5 0 0 0 0 0 0 0 0)
                (5 5 3 2 2 2 4 0 0 0 0 0 0 0 0 0)
                (4 4 3 3 1 3 0 0 0 0 0 0 0 0 0 0)
                (4 4 2 1 3 0 0 0 0 0 0 0 0 0 0 0)
                (3 3 1 2 0 0 0 0 0 0 0 0 0 0 0 0)
                (2 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                )))

(defparameter +total-zeros-bits+
  ;; Tables 9-7 and 9-8, codes.
  (make-array '(15 16) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 3 2 3 2 3 2 3 2 3 2 3 2 3 2 1)
                (7 6 5 4 3 5 4 3 2 3 2 3 2 1 0 0)
                (5 7 6 5 4 3 4 3 2 3 2 1 1 0 0 0)
                (3 7 5 4 6 5 4 3 3 2 2 1 0 0 0 0)
                (5 4 3 7 6 5 4 3 2 1 1 0 0 0 0 0)
                (1 1 7 6 5 4 3 2 1 1 0 0 0 0 0 0)
                (1 1 5 4 3 3 2 1 1 0 0 0 0 0 0 0)
                (1 1 1 3 3 2 2 1 0 0 0 0 0 0 0 0)
                (1 0 1 3 2 1 1 1 0 0 0 0 0 0 0 0)
                (1 0 1 3 2 1 1 0 0 0 0 0 0 0 0 0)
                (0 1 1 2 1 3 0 0 0 0 0 0 0 0 0 0)
                (0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0)
                (0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                )))

(defparameter +chroma-dc-total-zeros-len+
  ;; Table 9-9(a), for the 2x2 chroma DC block.
  (make-array '(3 4) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 2 3 3)
                (1 2 2 0)
                (1 1 0 0)
                )))

(defparameter +chroma-dc-total-zeros-bits+
  ;; Table 9-9(a), codes.
  (make-array '(3 4) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 1 1 0)
                (1 1 0 0)
                (1 0 0 0)
                )))

(defparameter +run-len+
  ;; Table 9-10, indexed [min(zeros_left, 7) - 1][run_before].
  (make-array '(7 16) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (1 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0)
                (2 2 2 3 3 0 0 0 0 0 0 0 0 0 0 0)
                (2 2 3 3 3 3 0 0 0 0 0 0 0 0 0 0)
                (2 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0)
                (3 3 3 3 3 3 3 4 5 6 7 8 9 10 11 0)
                )))

(defparameter +run-bits+
  ;; Table 9-10, codes.
  (make-array '(7 16) :element-type '(unsigned-byte 8)
              :initial-contents
              '((1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (3 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (3 2 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
                (3 2 3 2 1 0 0 0 0 0 0 0 0 0 0 0)
                (3 0 1 3 2 5 4 0 0 0 0 0 0 0 0 0)
                (7 6 5 4 3 2 1 1 1 1 1 1 1 1 1 0)
                )))


;;; ---- scan order and quantisation ---------------------------------------------------------------

(defparameter +zigzag-4x4+
  ;; Figure 8-8: the inverse scan for a frame-coded 4x4 block — scan position to raster position.
  (make-array 16 :element-type '(unsigned-byte 8)
                 :initial-contents '(0 1 4 8 5 2 3 6 9 12 13 10 7 11 14 15)))

(defparameter +dequant-coeff+
  ;; Table 8-15, the LevelScale values indexed [qp mod 6][position class].  Dequantisation in
  ;; H.264 is POSITION DEPENDENT — the transform is not orthonormal, so the four corners, the
  ;; centre and the rest of a 4x4 each get their own scale — which is the part a decoder written
  ;; from a JPEG intuition gets wrong.
  (make-array '(6 3) :element-type '(unsigned-byte 8)
                     :initial-contents '((10 16 13) (11 18 14) (13 20 16)
                                         (14 23 18) (16 25 20) (18 29 23))))

(defparameter +dequant-class+
  ;; which LevelScale column each raster position of a 4x4 block uses
  (make-array 16 :element-type '(unsigned-byte 8)
                 :initial-contents '(0 2 0 2
                                     2 1 2 1
                                     0 2 0 2
                                     2 1 2 1)))

(defparameter +qpc-from-qpy+
  ;; Table 8-15: above 29 the chroma quantiser rises more slowly than luma, so colour stays finer
  ;; than brightness where it matters.  Indexed by qPi clamped to 0..51.
  (make-array 52 :element-type '(unsigned-byte 8)
                 :initial-contents '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
                                     20 21 22 23 24 25 26 27 28 29
                                     29 30 31 32 32 33 34 34 35 35 36 36 37 37 37 38 38 38 39 39 39 39)))


;;; ---- inter macroblocks -----------------------------------------------------------------------

(defparameter +inter-cbp+
  ;; Table 9-4 again, the OTHER column.  Inter macroblocks map the same code numbers to different
  ;; patterns, and using the intra column for them is a silent wrong answer rather than an error.
  (make-array 48 :element-type '(unsigned-byte 8)
                 :initial-contents '(0 16 1 2  4 8 32 3  5 10 12 15  47 7 11 13
                                     14 6 9 31  35 37 42 44  33 34 36 40  39 43 45 46
                                     17 18 20 24  19 21 26 28  23 27 29 30  22 25 38 41)))

(defparameter +p-part-width+
  ;; Table 7-13: the partition shape of each P macroblock type.  Types 0..3 are inter; 4 is
  ;; P_8x8ref0, which is 8x8 with ref_idx inferred to be 0 rather than coded.
  (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(16 16 8 8 8)))
(defparameter +p-part-height+
  (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(16 8 16 8 8)))
(defparameter +p-part-count+
  (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(1 2 2 4 4)))

(defparameter +p-sub-width+
  ;; Table 7-17: the shape of each sub-macroblock partition inside a P_8x8.
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(8 8 4 4)))
(defparameter +p-sub-height+
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(8 4 8 4)))
(defparameter +p-sub-count+
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 2 4)))

(declaim (type (simple-array (unsigned-byte 8) (*))
               +inter-cbp+ +p-part-width+ +p-part-height+ +p-part-count+
               +p-sub-width+ +p-sub-height+ +p-sub-count+))

;;; ---- B macroblocks ----------------------------------------------------------------------------
;;;
;;; A prediction mode here is 0 = from list 0, 1 = from list 1, 2 = from both, 3 = direct (inferred
;;; from the neighbours and the co-located picture, with nothing coded at all).

(defconstant +pred-l0+ 0)
(defconstant +pred-l1+ 1)
(defconstant +pred-bi+ 2)
(defconstant +pred-direct+ 3)

(defparameter +b-part+
  ;; Table 7-14, mb_type 0..22: (partition-count width height mode0 mode1).  Type 22 is B_8x8,
  ;; whose four partitions carry their own sub types.  Anything above 22 is an intra macroblock
  ;; with 23 subtracted.
  (make-array '(23 5) :element-type '(signed-byte 32) :initial-contents
   '((1 16 16 3 3)      ; B_Direct_16x16
     (1 16 16 0 0)      ; B_L0_16x16
     (1 16 16 1 1)      ; B_L1_16x16
     (1 16 16 2 2)      ; B_Bi_16x16
     (2 16  8 0 0)      ; B_L0_L0_16x8
     (2  8 16 0 0)      ; B_L0_L0_8x16
     (2 16  8 1 1)      ; B_L1_L1_16x8
     (2  8 16 1 1)      ; B_L1_L1_8x16
     (2 16  8 0 1)      ; B_L0_L1_16x8
     (2  8 16 0 1)      ; B_L0_L1_8x16
     (2 16  8 1 0)      ; B_L1_L0_16x8
     (2  8 16 1 0)      ; B_L1_L0_8x16
     (2 16  8 0 2)      ; B_L0_Bi_16x8
     (2  8 16 0 2)      ; B_L0_Bi_8x16
     (2 16  8 1 2)      ; B_L1_Bi_16x8
     (2  8 16 1 2)      ; B_L1_Bi_8x16
     (2 16  8 2 0)      ; B_Bi_L0_16x8
     (2  8 16 2 0)      ; B_Bi_L0_8x16
     (2 16  8 2 1)      ; B_Bi_L1_16x8
     (2  8 16 2 1)      ; B_Bi_L1_8x16
     (2 16  8 2 2)      ; B_Bi_Bi_16x8
     (2  8 16 2 2)      ; B_Bi_Bi_8x16
     (4  8  8 9 9))))   ; B_8x8: the modes come from sub_mb_type

(defparameter +b-sub+
  ;; Table 7-18, sub_mb_type 0..12: (sub-partition-count width height mode).
  (make-array '(13 4) :element-type '(signed-byte 32) :initial-contents
   '((4 4 4 3)          ; B_Direct_8x8 — four 4x4 blocks, each inferred
     (1 8 8 0)          ; B_L0_8x8
     (1 8 8 1)          ; B_L1_8x8
     (1 8 8 2)          ; B_Bi_8x8
     (2 8 4 0)          ; B_L0_8x4
     (2 4 8 0)          ; B_L0_4x8
     (2 8 4 1)          ; B_L1_8x4
     (2 4 8 1)          ; B_L1_4x8
     (2 8 4 2)          ; B_Bi_8x4
     (2 4 8 2)          ; B_Bi_4x8
     (4 4 4 0)          ; B_L0_4x4
     (4 4 4 1)          ; B_L1_4x4
     (4 4 4 2))))       ; B_Bi_4x4

(declaim (type (simple-array (signed-byte 32) (23 5)) +b-part+))
(declaim (type (simple-array (signed-byte 32) (13 4)) +b-sub+))

(declaim (inline pred-uses-l0-p pred-uses-l1-p))
(defun pred-uses-l0-p (m) (or (= m +pred-l0+) (= m +pred-bi+)))
(defun pred-uses-l1-p (m) (or (= m +pred-l1+) (= m +pred-bi+)))

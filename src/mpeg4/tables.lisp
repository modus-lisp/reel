;;;; mpeg4/tables.lisp — GENERATED.  The constant tables of ISO/IEC 14496-2 (MPEG-4 Part 2).
;;;;
;;;; This is the codec behind DivX and XviD, and its tables come from two places: the coefficient,
;;;; macroblock-type and motion tables it inherited wholesale from H.263, and the intra coefficient
;;;; table and DC tables it added.  Both are here because a decoder needs both and neither is
;;;; transmitted.
;;;;
;;;; Extracted mechanically and checked, as the H.264 and MPEG-2 tables were, against properties the
;;;; specification states apart from the numbers: every Huffman table is PREFIX-FREE, both alternate
;;;; scans are permutations of 0..63, and the run/level arrays are the length their VLC tables say.

(in-package #:reel.mpeg4)

;;; ---- scans ---------------------------------------------------------------------------------
;;;
;;; THREE of them, and an INTRA block chooses per block rather than per picture.  When a block
;;; predicts its AC coefficients from the block above, the first row is already accounted for and
;;; the rest is scanned horizontally; when it predicts from the left, vertically; and when it
;;; predicts nothing, the familiar zig-zag.

(defparameter +zigzag+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   1   8  16   9   2   3  10  17  24  32  25  18  11   4   5
    12  19  26  33  40  48  41  34  27  20  13   6   7  14  21  28
    35  42  49  56  57  50  43  36  29  22  15  23  30  37  44  51
    58  59  52  45  38  31  39  46  53  60  61  54  47  55  62  63))
  "The zig-zag, used when a block predicts no AC coefficients from its neighbours.")

(defparameter +alt-horizontal-scan+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   1   2   3   8   9  16  17
    10  11   4   5   6   7  15  14
    13  12  19  18  24  25  32  33
    26  27  20  21  22  23  28  29
    30  31  34  35  40  41  48  49
    42  43  36  37  38  39  44  45
    46  47  50  51  56  57  58  59
    52  53  54  55  60  61  62  63))
  "For a block that predicted its AC from the block ABOVE.")

(defparameter +alt-vertical-scan+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   8  16  24   1   9   2  10
    17  25  32  40  48  56  57  49
    41  33  26  18   3  11   4  12
    19  27  34  42  50  58  35  43
    51  59  20  28   5  13   6  14
    21  29  36  44  52  60  37  45
    53  61  22  30   7  15  23  31
    38  46  54  62  39  47  55  63))
  "For a block that predicted its AC from the block to the LEFT.")

;;; ---- quantisation ---------------------------------------------------------------------------

(defparameter +default-intra-matrix+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     8  17  18  19  21  23  25  27
    17  18  19  21  23  25  27  28
    20  21  22  23  24  26  28  30
    21  22  23  24  26  28  30  32
    22  23  24  26  28  30  32  35
    23  24  26  28  30  32  35  38
    25  26  28  30  32  35  38  41
    27  28  30  32  35  38  41  45))
  "In RASTER order, for the MPEG-style quantiser a stream may choose instead of H.263's.")

(defparameter +default-inter-matrix+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
    16  17  18  19  20  21  22  23
    17  18  19  20  21  22  23  24
    18  19  20  21  22  23  24  25
    19  20  21  22  23  24  26  27
    20  21  22  23  25  26  27  28
    21  22  23  24  26  27  28  30
    22  23  24  26  27  28  30  31
    23  24  25  27  28  30  31  33))
  "Likewise.  Not flat, unlike MPEG-2's.")

(defparameter +y-dc-scale+
  (make-array 32 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   8   8   8   8  10  12  14
    16  17  18  19  20  21  22  23
    24  25  26  27  28  29  30  31
    32  34  36  38  40  42  44  46))
  "How much the luma DC is scaled at each quantiser (Table 7-4).  Not 8 >> something: a table.")

(defparameter +c-dc-scale+
  (make-array 32 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   8   8   8   8   9   9  10
    10  11  11  12  12  13  13  14
    14  15  15  16  16  17  17  18
    18  19  20  21  22  23  24  25))
  "And for chroma, which is scaled less aggressively.")

(defparameter +dc-threshold+
  (make-array 8 :element-type '(unsigned-byte 8) :initial-contents
   '(
    99  13  15  17  19  21  23   0))
  "intra_dc_vlc_thr: above this quantiser the DC is coded with the AC table rather than its own.
   A per-picture switch that changes the shape of every intra block, and easy to miss.")

;;; ---- variable length codes ------------------------------------------------------------------

(defparameter +dc-size-luma+
  '(
   (#x3 3) (#x3 2) (#x2 2) (#x2 3) (#x1 3)
   (#x1 4) (#x1 5) (#x1 6) (#x1 7) (#x1 8)
   (#x1 9) (#x1 10) (#x1 11))
  "dct_dc_size_luminance.")

(defparameter +dc-size-chroma+
  '(
   (#x3 2) (#x2 2) (#x1 2) (#x1 3) (#x1 4)
   (#x1 5) (#x1 6) (#x1 7) (#x1 8) (#x1 9)
   (#x1 10) (#x1 11) (#x1 12))
  "dct_dc_size_chrominance.")

(defparameter +mcbpc-intra+
  '(
   (#x1 1) (#x1 3) (#x2 3) (#x3 3) (#x1 4)
   (#x1 6) (#x2 6) (#x3 6) (#x1 9))
  "MCBPC in an I-VOP: the macroblock type and the two chroma coded-block bits, in one code.")

(defparameter +mcbpc-inter+
  '(
   (#x1 1) (#x3 4) (#x2 4) (#x5 6) (#x3 5)
   (#x4 8) (#x3 8) (#x3 7) (#x3 3) (#x7 7)
   (#x6 7) (#x5 9) (#x4 6) (#x4 9) (#x3 9)
   (#x2 9) (#x2 3) (#x5 7) (#x4 7) (#x5 8)
   (#x1 9) (#x0 0) (#x0 0) (#x0 0) (#x2 11)
   (#xc 13) (#xe 13) (#xf 13))
  "MCBPC in a P-VOP, where the type also says whether there are one or four motion vectors.")

(defparameter +cbpy-table+
  '(
   (#x3 4) (#x5 5) (#x4 5) (#x9 4)
   (#x3 5) (#x7 4) (#x2 6) (#xb 4)
   (#x2 5) (#x3 6) (#x5 4) (#xa 4)
   (#x4 4) (#x8 4) (#x6 4) (#x3 2))
  "CBPY: which of the four luma blocks carry coefficients.  Position i is the pattern.")

(defparameter +mv-table+
  '(
   (#x1 1) (#x1 2) (#x1 3) (#x1 4)
   (#x3 6) (#x5 7) (#x4 7) (#x3 7)
   (#xb 9) (#xa 9) (#x9 9) (#x11 10)
   (#x10 10) (#xf 10) (#xe 10) (#xd 10)
   (#xc 10) (#xb 10) (#xa 10) (#x9 10)
   (#x8 10) (#x7 10) (#x6 10) (#x5 10)
   (#x4 10) (#x7 11) (#x6 11) (#x5 11)
   (#x4 11) (#x3 11) (#x2 11) (#x3 12)
   (#x2 12))
  "The motion vector difference magnitude, 0..32, before f_code scaling.")

(defparameter +b-mb-type+
  '(
   (#x1 1) (#x1 2) (#x1 3) (#x1 4))
  "The four prediction modes of a B-VOP macroblock: direct, interpolated, backward, forward.")

;;; ---- DCT coefficients -------------------------------------------------------------------------
;;;
;;; ONE CODE CARRIES THREE THINGS: whether this is the last coefficient of the block, how many zeros
;;; precede it, and its magnitude.  The tables below are indexed by the code's position, and LAST is
;;; not a field — it is decided by WHERE in the table the entry falls, which is what the split
;;; constants are for.
;;;
;;; The last entry of each VLC table is the ESCAPE, and MPEG-4's escape is three escapes: one that
;;; adds to the level, one that adds to the run, and one that spells both out.  See slice.lisp.

(defparameter +intra-coeff-vlc+
  '(
   (#x2 2) (#x6 3) (#xf 4) (#xd 5)
   (#xc 5) (#x15 6) (#x13 6) (#x12 6)
   (#x17 7) (#x1f 8) (#x1e 8) (#x1d 8)
   (#x25 9) (#x24 9) (#x23 9) (#x21 9)
   (#x21 10) (#x20 10) (#xf 10) (#xe 10)
   (#x7 11) (#x6 11) (#x20 11) (#x21 11)
   (#x50 12) (#x51 12) (#x52 12) (#xe 4)
   (#x14 6) (#x16 7) (#x1c 8) (#x20 9)
   (#x1f 9) (#xd 10) (#x22 11) (#x53 12)
   (#x55 12) (#xb 5) (#x15 7) (#x1e 9)
   (#xc 10) (#x56 12) (#x11 6) (#x1b 8)
   (#x1d 9) (#xb 10) (#x10 6) (#x22 9)
   (#xa 10) (#xd 6) (#x1c 9) (#x8 10)
   (#x12 7) (#x1b 9) (#x54 12) (#x14 7)
   (#x1a 9) (#x57 12) (#x19 8) (#x9 10)
   (#x18 8) (#x23 11) (#x17 8) (#x19 9)
   (#x18 9) (#x7 10) (#x58 12) (#x7 4)
   (#xc 6) (#x16 8) (#x17 9) (#x6 10)
   (#x5 11) (#x4 11) (#x59 12) (#xf 6)
   (#x16 9) (#x5 10) (#xe 6) (#x4 10)
   (#x11 7) (#x24 11) (#x10 7) (#x25 11)
   (#x13 7) (#x5a 12) (#x15 8) (#x5b 12)
   (#x14 8) (#x13 8) (#x1a 8) (#x15 9)
   (#x14 9) (#x13 9) (#x12 9) (#x11 9)
   (#x26 11) (#x27 11) (#x5c 12) (#x5d 12)
   (#x5e 12) (#x5f 12) (#x3 7)))

(defparameter +intra-coeff-run+
  (make-array 102 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     0   0   0   0   0   0   0   0   0   0   0   1   1   1   1   1
     1   1   1   1   1   2   2   2   2   2   3   3   3   3   4   4
     4   5   5   5   6   6   6   7   7   7   8   8   9   9  10  11
    12  13  14   0   0   0   0   0   0   0   0   1   1   1   2   2
     3   3   4   4   5   5   6   6   7   8   9  10  11  12  13  14
    15  16  17  18  19  20))
  "Zeros before the coefficient this code carries.")

(defparameter +intra-coeff-level+
  (make-array 102 :element-type '(unsigned-byte 8) :initial-contents
   '(
     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
    17  18  19  20  21  22  23  24  25  26  27   1   2   3   4   5
     6   7   8   9  10   1   2   3   4   5   1   2   3   4   1   2
     3   1   2   3   1   2   3   1   2   3   1   2   1   2   1   1
     1   1   1   1   2   3   4   5   6   7   8   1   2   3   1   2
     1   2   1   2   1   2   1   2   1   1   1   1   1   1   1   1
     1   1   1   1   1   1))
  "Its magnitude; the sign is one more bit.")

(defconstant +intra-last-split+ 67
  "Entries below this have LAST clear, entries from here up have it set.")

(defparameter +inter-coeff-vlc+
  '(
   (#x2 2) (#xf 4) (#x15 6) (#x17 7)
   (#x1f 8) (#x25 9) (#x24 9) (#x21 10)
   (#x20 10) (#x7 11) (#x6 11) (#x20 11)
   (#x6 3) (#x14 6) (#x1e 8) (#xf 10)
   (#x21 11) (#x50 12) (#xe 4) (#x1d 8)
   (#xe 10) (#x51 12) (#xd 5) (#x23 9)
   (#xd 10) (#xc 5) (#x22 9) (#x52 12)
   (#xb 5) (#xc 10) (#x53 12) (#x13 6)
   (#xb 10) (#x54 12) (#x12 6) (#xa 10)
   (#x11 6) (#x9 10) (#x10 6) (#x8 10)
   (#x16 7) (#x55 12) (#x15 7) (#x14 7)
   (#x1c 8) (#x1b 8) (#x21 9) (#x20 9)
   (#x1f 9) (#x1e 9) (#x1d 9) (#x1c 9)
   (#x1b 9) (#x1a 9) (#x22 11) (#x23 11)
   (#x56 12) (#x57 12) (#x7 4) (#x19 9)
   (#x5 11) (#xf 6) (#x4 11) (#xe 6)
   (#xd 6) (#xc 6) (#x13 7) (#x12 7)
   (#x11 7) (#x10 7) (#x1a 8) (#x19 8)
   (#x18 8) (#x17 8) (#x16 8) (#x15 8)
   (#x14 8) (#x13 8) (#x18 9) (#x17 9)
   (#x16 9) (#x15 9) (#x14 9) (#x13 9)
   (#x12 9) (#x11 9) (#x7 10) (#x6 10)
   (#x5 10) (#x4 10) (#x24 11) (#x25 11)
   (#x26 11) (#x27 11) (#x58 12) (#x59 12)
   (#x5a 12) (#x5b 12) (#x5c 12) (#x5d 12)
   (#x5e 12) (#x5f 12) (#x3 7)))

(defparameter +inter-coeff-run+
  (make-array 102 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   0   0   0   0   0   0   0   0   0   0   0   1   1   1   1
     1   1   2   2   2   2   3   3   3   4   4   4   5   5   5   6
     6   6   7   7   8   8   9   9  10  10  11  12  13  14  15  16
    17  18  19  20  21  22  23  24  25  26   0   0   0   1   1   2
     3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
    19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
    35  36  37  38  39  40)))

(defparameter +inter-coeff-level+
  (make-array 102 :element-type '(unsigned-byte 8) :initial-contents
   '(
     1   2   3   4   5   6   7   8   9  10  11  12   1   2   3   4
     5   6   1   2   3   4   1   2   3   1   2   3   1   2   3   1
     2   3   1   2   1   2   1   2   1   2   1   1   1   1   1   1
     1   1   1   1   1   1   1   1   1   1   1   2   3   1   2   1
     1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
     1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
     1   1   1   1   1   1)))

(defconstant +inter-last-split+ 58)
(defconstant +coeff-escape+ 102 "The last entry of both coefficient tables.")

(declaim (type (simple-array (unsigned-byte 8) (64))
               +zigzag+ +alt-horizontal-scan+ +alt-vertical-scan+
               +default-intra-matrix+ +default-inter-matrix+))
(declaim (type (simple-array (unsigned-byte 8) (32)) +y-dc-scale+ +c-dc-scale+))
(declaim (type (simple-array (unsigned-byte 8) (102))
               +intra-coeff-run+ +intra-coeff-level+ +inter-coeff-run+ +inter-coeff-level+))

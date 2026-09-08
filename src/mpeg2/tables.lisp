;;;; mpeg2/tables.lisp — GENERATED.  The constant tables of ISO/IEC 13818-2 and 11172-2.
;;;;
;;;; MPEG-2 is a VARIABLE LENGTH CODE format from top to bottom: macroblock addresses, macroblock
;;;; types, coded block patterns, motion codes and every DCT coefficient are Huffman codes from
;;;; tables the specification prints and never transmits.  There are eight of them here.
;;;;
;;;; Extracted mechanically and checked against properties the specification states apart from the
;;;; numbers.  The strongest of those is that EVERY ONE OF THESE TABLES IS PREFIX-FREE: no code is a
;;;; prefix of any other in its table.  That is not decoration — it is the property that makes a
;;;; Huffman code decodable at all, and a transcription error large enough to matter almost always
;;;; breaks it.  Also checked: both scan orders are permutations of 0..63, the default intra matrix
;;;; runs 8 to 83, the default non-intra matrix is all sixteens, and the non-linear quantiser ladder
;;;; matches Table 7-6 value for value.

(in-package #:reel.mpeg2)

;;; ---- scan orders --------------------------------------------------------------------------
;;;
;;; TWO of them, and a picture says per picture which it used.  The zig-zag is the familiar
;;; diagonal sweep; the alternate scan runs more vertically and exists because a field of an
;;; interlaced picture has its energy spread differently down the block than across it.

(defparameter +zigzag+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   1   8  16   9   2   3  10  17  24  32  25  18  11   4   5
    12  19  26  33  40  48  41  34  27  20  13   6   7  14  21  28
    35  42  49  56  57  50  43  36  29  22  15  23  30  37  44  51
    58  59  52  45  38  31  39  46  53  60  61  54  47  55  62  63))
  "Scan position -> raster position, the zig-zag of Figure 7-2.")

(defparameter +alt-scan+
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
  "The alternate scan of Figure 7-3, chosen per picture by alternate_scan.")

;;; ---- quantiser weights --------------------------------------------------------------------

(defparameter +default-intra-matrix+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     8  16  19  22  26  27  29  34
    16  16  22  24  27  29  34  37
    19  22  26  27  29  34  34  38
    22  22  26  27  29  34  37  40
    22  26  27  29  32  35  40  48
    26  27  29  32  35  40  48  58
    26  27  29  34  38  46  56  69
    27  29  35  38  46  56  69  83))
  "Table 7-3, in RASTER order.  A sequence that loads none of its own uses this.")

(defparameter +default-non-intra-matrix+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-element 16)
  "Table 7-4: flat sixteens.  Written as a fill rather than a list because that IS the table.")

(defparameter +non-linear-qscale+
  (make-array 32 :element-type '(unsigned-byte 8) :initial-contents
   '(
      0    1    2    3    4    5    6    7
      8   10   12   14   16   18   20   22
     24   28   32   36   40   44   48   52
     56   64   72   80   88   96  104  112))
  "Table 7-6.  A picture with q_scale_type set reads its quantiser through this ladder instead of
   doubling the code, which gives finer steps where they matter and coarser ones where they do not.")

;;; ---- variable length codes ------------------------------------------------------------------
;;;
;;; Each entry is (code length), and the value is the entry's POSITION.  That is how the
;;; specification prints them and it is what makes the invariant check above possible.

(defparameter +dc-size-luma+
  '(
   (#x4 3) (#x0 2) (#x1 2) (#x5 3) (#x6 3) (#xe 4)
   (#x1e 5) (#x3e 6) (#x7e 7) (#xfe 8) (#x1fe 9) (#x1ff 9))
  "Table B.12: dct_dc_size_luminance.  The value is how many extra bits carry the difference.")

(defparameter +dc-size-chroma+
  '(
   (#x0 2) (#x1 2) (#x2 2) (#x6 3) (#xe 4) (#x1e 5)
   (#x3e 6) (#x7e 7) (#xfe 8) (#x1fe 9) (#x3fe 10) (#x3ff 10))
  "Table B.13: dct_dc_size_chrominance.")

(defparameter +mb-addr-incr+
  '(
   (#x1 1) (#x3 3) (#x2 3) (#x3 4)
   (#x2 4) (#x3 5) (#x2 5) (#x7 7)
   (#x6 7) (#xb 8) (#xa 8) (#x9 8)
   (#x8 8) (#x7 8) (#x6 8) (#x17 10)
   (#x16 10) (#x15 10) (#x14 10) (#x13 10)
   (#x12 10) (#x23 11) (#x22 11) (#x21 11)
   (#x20 11) (#x1f 11) (#x1e 11) (#x1d 11)
   (#x1c 11) (#x1b 11) (#x1a 11) (#x19 11)
   (#x18 11) (#x8 11) (#xf 11) (#x0 8))
  "Table B.1: macroblock_address_increment.  Position i is an increment of i+1, up to 33; then
   position 33 is the escape that adds 33 and reads another code, 34 is stuffing, and 35 is the
   end of a slice.")

(defparameter +cbp-table+
  '(
   (#x1 9) (#xb 5) (#x9 5) (#xd 6)
   (#xd 4) (#x17 7) (#x13 7) (#x1f 8)
   (#xc 4) (#x16 7) (#x12 7) (#x1e 8)
   (#x13 5) (#x1b 8) (#x17 8) (#x13 8)
   (#xb 4) (#x15 7) (#x11 7) (#x1d 8)
   (#x11 5) (#x19 8) (#x15 8) (#x11 8)
   (#xf 6) (#xf 8) (#xd 8) (#x3 9)
   (#xf 5) (#xb 8) (#x7 8) (#x7 9)
   (#xa 4) (#x14 7) (#x10 7) (#x1c 8)
   (#xe 6) (#xe 8) (#xc 8) (#x2 9)
   (#x10 5) (#x18 8) (#x14 8) (#x10 8)
   (#xe 5) (#xa 8) (#x6 8) (#x6 9)
   (#x12 5) (#x1a 8) (#x16 8) (#x12 8)
   (#xd 5) (#x9 8) (#x5 8) (#x5 9)
   (#xc 5) (#x8 8) (#x4 8) (#x4 9)
   (#x7 3) (#xa 5) (#x8 5) (#xc 6))
  "Table B.9: coded_block_pattern.  Position i IS the pattern, and pattern 0 is not legal — a
   macroblock with no coded blocks says so with its type instead.")

(defparameter +motion-code+
  '(
   (#x1 1) (#x1 2) (#x1 3) (#x1 4)
   (#x3 6) (#x5 7) (#x4 7) (#x3 7)
   (#xb 9) (#xa 9) (#x9 9) (#x11 10)
   (#x10 10) (#xf 10) (#xe 10) (#xd 10)
   (#xc 10))
  "Table B.10: motion_code.  Position i is a magnitude of i, and 0 means `the same vector as the
   prediction\', not `a vector of zero\'.")

(defparameter +p-mb-type+
  '(
   (#x3 5 #x01) (#x1 2 #x02) (#x1 3 #x08)
   (#x1 1 #x0a) (#x1 6 #x11) (#x1 5 #x12)
   (#x2 5 #x1a))
  "Table B.3, as (code length flags).  The flags are the specification\'s own five: 1 intra,
   2 pattern, 4 backward, 8 forward, 16 quant.")

(defparameter +b-mb-type+
  '(
   (#x3 5 #x01) (#x2 3 #x04) (#x3 3 #x06)
   (#x2 4 #x08) (#x3 4 #x0a) (#x2 2 #x0c)
   (#x3 2 #x0e) (#x1 6 #x11) (#x2 6 #x16)
   (#x3 6 #x1a) (#x2 5 #x1e))
  "Table B.4, same shape.")

;;; ---- DCT coefficients -----------------------------------------------------------------------
;;;
;;; TWO tables for one job.  Table B.14 codes both intra and non-intra blocks in MPEG-1 and
;;; non-intra blocks in MPEG-2; Table B.15 is an alternative for MPEG-2 INTRA blocks only, chosen
;;; per picture by intra_vlc_format, and it spends its short codes differently because an intra
;;; block\'s coefficients are distributed differently from a residual\'s.
;;;
;;; Position i names the run and level at position i of the two arrays below.  The last two
;;; positions are not coefficients: 111 is the escape and 112 is end-of-block.

(defparameter +coeff-vlc-b14+
  '(
   (#x3 2) (#x4 4) (#x5 5) (#x6 7)
   (#x26 8) (#x21 8) (#xa 10) (#x1d 12)
   (#x18 12) (#x13 12) (#x10 12) (#x1a 13)
   (#x19 13) (#x18 13) (#x17 13) (#x1f 14)
   (#x1e 14) (#x1d 14) (#x1c 14) (#x1b 14)
   (#x1a 14) (#x19 14) (#x18 14) (#x17 14)
   (#x16 14) (#x15 14) (#x14 14) (#x13 14)
   (#x12 14) (#x11 14) (#x10 14) (#x18 15)
   (#x17 15) (#x16 15) (#x15 15) (#x14 15)
   (#x13 15) (#x12 15) (#x11 15) (#x10 15)
   (#x3 3) (#x6 6) (#x25 8) (#xc 10)
   (#x1b 12) (#x16 13) (#x15 13) (#x1f 15)
   (#x1e 15) (#x1d 15) (#x1c 15) (#x1b 15)
   (#x1a 15) (#x19 15) (#x13 16) (#x12 16)
   (#x11 16) (#x10 16) (#x5 4) (#x4 7)
   (#xb 10) (#x14 12) (#x14 13) (#x7 5)
   (#x24 8) (#x1c 12) (#x13 13) (#x6 5)
   (#xf 10) (#x12 12) (#x7 6) (#x9 10)
   (#x12 13) (#x5 6) (#x1e 12) (#x14 16)
   (#x4 6) (#x15 12) (#x7 7) (#x11 12)
   (#x5 7) (#x11 13) (#x27 8) (#x10 13)
   (#x23 8) (#x1a 16) (#x22 8) (#x19 16)
   (#x20 8) (#x18 16) (#xe 10) (#x17 16)
   (#xd 10) (#x16 16) (#x8 10) (#x15 16)
   (#x1f 12) (#x1a 12) (#x19 12) (#x17 12)
   (#x16 12) (#x1f 13) (#x1e 13) (#x1d 13)
   (#x1c 13) (#x1b 13) (#x1f 16) (#x1e 16)
   (#x1d 16) (#x1c 16) (#x1b 16) (#x1 6)
   (#x2 2)))

(defparameter +coeff-vlc-b15+
  '(
   (#x2 2) (#x6 3) (#x7 4) (#x1c 5)
   (#x1d 5) (#x5 6) (#x4 6) (#x7b 7)
   (#x7c 7) (#x23 8) (#x22 8) (#xfa 8)
   (#xfb 8) (#xfe 8) (#xff 8) (#x1f 14)
   (#x1e 14) (#x1d 14) (#x1c 14) (#x1b 14)
   (#x1a 14) (#x19 14) (#x18 14) (#x17 14)
   (#x16 14) (#x15 14) (#x14 14) (#x13 14)
   (#x12 14) (#x11 14) (#x10 14) (#x18 15)
   (#x17 15) (#x16 15) (#x15 15) (#x14 15)
   (#x13 15) (#x12 15) (#x11 15) (#x10 15)
   (#x2 3) (#x6 5) (#x79 7) (#x27 8)
   (#x20 8) (#x16 13) (#x15 13) (#x1f 15)
   (#x1e 15) (#x1d 15) (#x1c 15) (#x1b 15)
   (#x1a 15) (#x19 15) (#x13 16) (#x12 16)
   (#x11 16) (#x10 16) (#x5 5) (#x7 7)
   (#xfc 8) (#xc 10) (#x14 13) (#x7 5)
   (#x26 8) (#x1c 12) (#x13 13) (#x6 6)
   (#xfd 8) (#x12 12) (#x7 6) (#x4 9)
   (#x12 13) (#x6 7) (#x1e 12) (#x14 16)
   (#x4 7) (#x15 12) (#x5 7) (#x11 12)
   (#x78 7) (#x11 13) (#x7a 7) (#x10 13)
   (#x21 8) (#x1a 16) (#x25 8) (#x19 16)
   (#x24 8) (#x18 16) (#x5 9) (#x17 16)
   (#x7 9) (#x16 16) (#xd 10) (#x15 16)
   (#x1f 12) (#x1a 12) (#x19 12) (#x17 12)
   (#x16 12) (#x1f 13) (#x1e 13) (#x1d 13)
   (#x1c 13) (#x1b 13) (#x1f 16) (#x1e 16)
   (#x1d 16) (#x1c 16) (#x1b 16) (#x1 6)
   (#x6 4)))

(defparameter +coeff-run+
  (make-array 111 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     0   0   0   0   0   0   0   0   1   1   1   1   1   1   1   1
     1   1   1   1   1   1   1   1   1   1   2   2   2   2   2   3
     3   3   3   4   4   4   5   5   5   6   6   6   7   7   8   8
     9   9  10  10  11  11  12  12  13  13  14  14  15  15  16  16
    17  18  19  20  21  22  23  24  25  26  27  28  29  30  31))
  "How many zero coefficients precede the one this code carries.")

(defparameter +coeff-level+
  (make-array 111 :element-type '(unsigned-byte 8) :initial-contents
   '(
     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
    17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32
    33  34  35  36  37  38  39  40   1   2   3   4   5   6   7   8
     9  10  11  12  13  14  15  16  17  18   1   2   3   4   5   1
     2   3   4   1   2   3   1   2   3   1   2   3   1   2   1   2
     1   2   1   2   1   2   1   2   1   2   1   2   1   2   1   2
     1   1   1   1   1   1   1   1   1   1   1   1   1   1   1))
  "Its magnitude.  The sign is one more bit, read after the code.")

(defconstant +coeff-escape+ 111)
(defconstant +coeff-eob+ 112)

(declaim (type (simple-array (unsigned-byte 8) (64))
               +zigzag+ +alt-scan+ +default-intra-matrix+ +default-non-intra-matrix+))
(declaim (type (simple-array (unsigned-byte 8) (32)) +non-linear-qscale+))
(declaim (type (simple-array (unsigned-byte 8) (111)) +coeff-run+ +coeff-level+))

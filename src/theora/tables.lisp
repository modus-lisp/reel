;;;; theora/tables.lisp — GENERATED.  The constant tables of Theora (and of VP3 beneath it).
;;;;
;;;; Theora transmits most of what other codecs hard-wire — its quantiser matrices, its loop filter
;;;; limits and all EIGHTY of its coefficient Huffman tables arrive in the setup header — so what is
;;;; left here is smaller than it looks: the codes for the run lengths and modes and motion vectors,
;;;; which are fixed, and the VP3 defaults a Theora stream may fall back on.
;;;;
;;;; Extracted mechanically and checked.  The strongest check is on the superblock scan: it must be
;;;; a permutation of a four-by-four, and every step must be to an ADJACENT cell.  That second
;;;; property is what makes it a space-filling curve rather than an arbitrary order, and it is why
;;;; the run-length coding of coded-block flags works as well as it does — neighbouring blocks are
;;;; neighbours in the scan too.

(in-package #:reel.theora)

(defparameter +superblock-scan+
  (make-array '(16 2) :element-type '(unsigned-byte 8) :initial-contents
   '(
   (0 0) (1 0) (1 1) (0 1)
   (0 2) (0 3) (1 3) (1 2)
   (2 2) (2 3) (3 3) (3 2)
   (3 1) (2 1) (2 0) (3 0)))
  "The order the sixteen blocks of a superblock are visited: a Hilbert curve, so that consecutive
   blocks in the scan are adjacent on the picture.")

(defparameter +zigzag+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   1   8  16   9   2   3  10  17  24  32  25  18  11   4   5
    12  19  26  33  40  48  41  34  27  20  13   6   7  14  21  28
    35  42  49  56  57  50  43  36  29  22  15  23  30  37  44  51
    58  59  52  45  38  31  39  46  53  60  61  54  47  55  62  63))
  "Scan position to raster position.")

;;; ---- the fixed variable length codes -----------------------------------------------------------
;;;
;;; These are given as CODE LENGTHS ONLY, in order.  The codes themselves are the canonical Huffman
;;; assignment for those lengths, which is how the specification prints them and is why there is a
;;; builder rather than a table of bit patterns.

(defparameter +superblock-run-lengths+
  (make-array 34 :element-type '(unsigned-byte 8) :initial-contents
   '(
     1   3   3   4   4   6   6   6   6   8   8   8   8   8   8   8   8
    10  10  10  10  10  10  10  10  10  10  10  10  10  10  10  10   6))
  "Runs of one to thirty-three superblocks, and a thirty-fourth entry that means `read twelve more
   bits and add thirty-four\' — which is how a run the length of a whole picture is sent.")

(defparameter +fragment-run-lengths+
  (make-array 30 :element-type '(unsigned-byte 8) :initial-contents
   '(
     2   2   3   3   4   4   6   6   6   6   7   7   7   7   9
     9   9   9   9   9   9   9   9   9   9   9   9   9   9   9))
  "And the same idea one level down, for runs of blocks inside a superblock.")

(defparameter +mode-code-lengths+
  (make-array 8 :element-type '(unsigned-byte 8) :initial-contents
   '(
     1   2   3   4   5   6   7   7))
  "The eight macroblock coding modes, when the picture uses a coded alphabet rather than three bits
   each.")

(defparameter +motion-vector-vlc+
  '(
   (31 3) (32 3) (30 3) (33 4) (29 4)
   (34 4) (28 4) (35 6) (27 6) (36 6)
   (26 6) (37 6) (25 6) (38 6) (24 6)
   (39 7) (23 7) (40 7) (22 7) (41 7)
   (21 7) (42 7) (20 7) (43 7) (19 7)
   (44 7) (18 7) (45 7) (17 7) (46 7)
   (16 7) (47 8) (15 8) (48 8) (14 8)
   (49 8) (13 8) (50 8) (12 8) (51 8)
   (11 8) (52 8) (10 8) (53 8) (9 8)
   (54 8) (8 8) (55 8) (7 8) (56 8)
   (6 8) (57 8) (5 8) (58 8) (4 8)
   (59 8) (3 8) (60 8) (2 8) (61 8)
   (1 8) (62 8) (0 8))
  "(value length) for the motion vector components.  The VALUE IS THE VECTOR PLUS THIRTY-ONE, not
   an index into anything: subtract thirty-one and you have the half-pixel displacement, which is
   why zero has the shortest code and the codes lengthen outwards in both directions.")

(defparameter +fixed-motion-vectors+
  (make-array 64 :element-type '(signed-byte 32) :initial-contents
   '(
      0    0    1   -1    2   -2    3   -3
      4   -4    5   -5    6   -6    7   -7
      8   -8    9   -9   10  -10   11  -11
     12  -12   13  -13   14  -14   15  -15
     16  -16   17  -17   18  -18   19  -19
     20  -20   21  -21   22  -22   23  -23
     24  -24   25  -25   26  -26   27  -27
     28  -28   29  -29   30  -30   31  -31))
  "Zero, then plus and minus one, plus and minus two, and so on — the order the codes are assigned
   in, which is by MAGNITUDE because that is what the distribution is.")

;;; ---- coefficient tokens -------------------------------------------------------------------------
;;;
;;; ONE TOKEN CARRIES A RUN AND A VALUE, and thirty-two of them cover every case from `the block
;;; ends here and so do the next fifteen blocks\' to `a run of ten zeros and then a large
;;; coefficient\'.  The three tables below say, for each token, how many zeros precede its value and
;;; how many extra bits follow to refine it.

(defparameter +eob-run+
  (make-array '(7 2) :element-type '(unsigned-byte 8) :initial-contents
   '(
   (1 0) (2 0) (3 0) (4 2)
   (8 3) (16 4) (0 12)))
  "Tokens zero to six end a run of blocks: a base count, and how many bits extend it.")

(defparameter +zero-run-base+
  (make-array 32 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     0   0   0   0   0   0   0   1   2   3   4   5   6  10   1   2))
  "How many zeros a token carries before its value.")

(defparameter +zero-run-bits+
  (make-array 32 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   0   0   0   0   0   0   3   6   0   0   0   0   0   0   0
     0   0   0   0   0   0   0   0   0   0   0   0   2   3   0   1))
  "And how many bits extend that count.")

(defparameter +coeff-bits+
  (make-array 32 :element-type '(unsigned-byte 8) :initial-contents
   '(
     0   0   0   0   0   0   0   0   0   0   0   0   0   1   1   1
     1   2   3   4   5   6  10   1   1   1   1   1   1   1   2   2))
  "How many bits the value itself takes, which for the small tokens is one — the sign.")

(defparameter +coeff-base+
  (make-array 32 :element-type '(signed-byte 32) :initial-contents
   '(
     0   0   0   0   0   0   0   0   0   1  -1   2  -2   3   4   5
     6   7   9  13  21  37  69   1   1   1   1   1   1   1   2   2))
  "The smallest magnitude a token can carry.  Tokens seven to twelve carry it outright, sign and
   all, and take no extra bits; from thirteen up the extra bits pick a magnitude counting up from
   this base, with the topmost of them acting as a sign.

   Extracted from ffmpeg's twelve separate value tables and CHECKED to reproduce every one of them
   exactly, which is the only reason it is safe to keep the rule rather than the tables — the
   largest of those tables has a thousand and twenty-four entries and says nothing this does not.")

;;; ---- VP3's defaults ------------------------------------------------------------------------------
;;;
;;; A Theora stream sends all of these in its setup header.  They are here because VP3, which Theora
;;; is a descendant of, does not — and because they are the values a malformed setup header should be
;;; compared against rather than trusted over.

(defparameter +vp31-intra-y-dequant+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     16   11   10   16   24   40   51   61
     12   12   14   19   26   58   60   55
     14   13   16   24   40   57   69   56
     14   17   22   29   51   87   80   62
     18   22   37   58   68  109  103   77
     24   35   55   64   81  104  113   92
     49   64   78   87  103  121  120  101
     72   92   95   98  112  100  103   99)))

(defparameter +vp31-inter-dequant+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
     16   16   16   20   24   28   32   40
     16   16   20   24   28   32   40   48
     16   20   24   28   32   40   48   64
     20   24   28   32   40   48   64   64
     24   28   32   40   48   64   64   64
     28   32   40   48   64   64   64   96
     32   40   48   64   64   64   96  128
     40   48   64   64   64   96  128  128)))

(defparameter +vp31-dc-scale+
  (make-array 64 :element-type '(unsigned-byte 16) :initial-contents
   '(
    220  200  190  180  170  170  160  160  150  150  140  140  130  130  120  120
    110  110  100  100   90   90   90   80   80   80   70   70   70   60   60   60
     60   50   50   50   50   40   40   40   40   40   30   30   30   30   30   30
     30   20   20   20   20   20   20   20   20   10   10   10   10   10   10   10)))

(defparameter +vp31-ac-scale+
  (make-array 64 :element-type '(unsigned-byte 16) :initial-contents
   '(
     500   450   400   370   340   310   285   265   245   225   210   195   185   180   170   160
     150   145   135   130   125   115   110   107   100    96    93    89    85    82    75    74
      70    68    64    60    57    56    52    50    49    45    44    43    40    38    37    35
      33    32    30    29    28    25    24    22    21    19    18    17    15    13    12    10)))

(defparameter +vp31-filter-limits+
  (make-array 64 :element-type '(unsigned-byte 8) :initial-contents
   '(
    30  25  20  20  15  15  14  14  13  13  12  12  11  11  10  10
     9   9   8   8   7   7   7   7   6   6   6   6   5   5   5   5
     4   4   4   4   3   3   3   3   2   2   2   2   2   2   2   2
     0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0)))

;;; ---- macroblock modes ----------------------------------------------------------------------------

(defconstant +mode-inter-no-mv+ 0)
(defconstant +mode-intra+ 1)
(defconstant +mode-inter-plus-mv+ 2)
(defconstant +mode-inter-last-mv+ 3)
(defconstant +mode-inter-prior-last+ 4)
(defconstant +mode-using-golden+ 5)
(defconstant +mode-golden-mv+ 6)
(defconstant +mode-inter-fourmv+ 7)

(defparameter +mode-alphabets+
  (make-array '(7 8) :element-type '(signed-byte 32) :initial-contents
   '((3 4 2 0 1 5 6 7)
     (3 4 0 2 1 5 6 7)
     (3 2 4 0 1 5 6 7)
     (3 2 0 4 1 5 6 7)
     (0 3 4 2 1 5 6 7)
     (0 5 3 4 2 1 6 7)
     (0 1 2 3 4 5 6 7)))
  "Six preset orderings of the eight modes, and a seventh row for the free-form scheme where the
   picture sends its own.  A picture picks whichever ordering matches its content, so that the mode
   it uses most often gets the shortest code — which is a Huffman table chosen from a menu rather
   than transmitted.")

(declaim (type (simple-array (unsigned-byte 8) (64)) +zigzag+ +vp31-intra-y-dequant+
               +vp31-inter-dequant+ +vp31-filter-limits+))
(declaim (type (simple-array (unsigned-byte 16) (64)) +vp31-dc-scale+ +vp31-ac-scale+))
(declaim (type (simple-array (unsigned-byte 8) (32))
               +zero-run-base+ +zero-run-bits+ +coeff-bits+))
(declaim (type (simple-array (signed-byte 32) (32)) +coeff-base+))
(declaim (type (simple-array (signed-byte 32) (64)) +fixed-motion-vectors+))
(declaim (type (simple-array (signed-byte 32) (7 8)) +mode-alphabets+))

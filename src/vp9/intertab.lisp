;;;; vp9/intertab.lisp — GENERATED.  Two tables the inter path needs.

(in-package #:reel.vp9)

(defparameter +inter-mode-ctx-lut+
  (make-array '(14 14) :element-type 'fixnum :initial-contents
   '(
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (6 6 6 6 6 6 6 6 6 6 5 5 5 5)
     (5 5 5 5 5 5 5 5 5 5 2 2 1 3)
     (5 5 5 5 5 5 5 5 5 5 2 2 1 3)
     (5 5 5 5 5 5 5 5 5 5 1 1 0 3)
     (5 5 5 5 5 5 5 5 5 5 3 3 3 4)
     ))
  "Which of the seven inter-mode contexts a pair of neighbouring MODES selects.  Indexed by the
   above and left modes, which run over the ten intra modes and then the four inter ones — so most
   of this table is the two `a neighbour was intra' answers, five and six.")

(defparameter +mv-ref-blk-off+
  (make-array '(13 8 2) :element-type 'fixnum :initial-contents
   '(
     (( 3 -1) (-1  3) ( 4 -1) (-1  4) (-1 -1) ( 0 -1) (-1  0) ( 6 -1))
     (( 0 -1) (-1  0) ( 4 -1) (-1  2) (-1 -1) ( 0 -3) (-3  0) ( 2 -1))
     ((-1  0) ( 0 -1) (-1  4) ( 2 -1) (-1 -1) (-3  0) ( 0 -3) (-1  2))
     (( 1 -1) (-1  1) ( 2 -1) (-1  2) (-1 -1) ( 0 -3) (-3  0) (-3 -3))
     (( 0 -1) (-1  0) ( 2 -1) (-1 -1) (-1  1) ( 0 -3) (-3  0) (-3 -3))
     ((-1  0) ( 0 -1) (-1  2) (-1 -1) ( 1 -1) (-3  0) ( 0 -3) (-3 -3))
     (( 0 -1) (-1  0) ( 1 -1) (-1  1) (-1 -1) ( 0 -3) (-3  0) (-3 -3))
     (( 0 -1) (-1  0) ( 1 -1) (-1 -1) ( 0 -2) (-2  0) (-2 -1) (-1 -2))
     ((-1  0) ( 0 -1) (-1  1) (-1 -1) (-2  0) ( 0 -2) (-1 -2) (-2 -1))
     (( 0 -1) (-1  0) (-1 -1) ( 0 -2) (-2  0) (-1 -2) (-2 -1) (-2 -2))
     (( 0 -1) (-1  0) (-1 -1) ( 0 -2) (-2  0) (-1 -2) (-2 -1) (-2 -2))
     (( 0 -1) (-1  0) (-1 -1) ( 0 -2) (-2  0) (-1 -2) (-2 -1) (-2 -2))
     (( 0 -1) (-1  0) (-1 -1) ( 0 -2) (-2  0) (-1 -2) (-2 -1) (-2 -2))
     ))
  "Eight neighbouring blocks to search for a motion vector, per block size, as (column row) offsets
   in eight-sample units.  THE ORDER IS THE SEARCH ORDER and it differs between a wide block and a
   tall one: the first two entries are always the immediate above and left neighbours, nearer edge
   first, because that is the one most likely to agree.")

(declaim (type (simple-array fixnum (14 14)) +inter-mode-ctx-lut+)
         (type (simple-array fixnum (13 8 2)) +mv-ref-blk-off+))

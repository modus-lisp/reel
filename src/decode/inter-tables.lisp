;;;; decode/inter-tables.lisp — VP8 inter-frame constant tables (RFC 6386 s16-17 and
;;;; the reference decoder's modemv_data.h / vp8_prob_data.h).  Key-frame
;;;; tables and coefficient tables come from webp-pure.
(in-package #:reel.decode)

(defmacro fixnum-vector (&rest values)
  `(make-array ,(length values) :element-type '(signed-byte 32) :initial-contents ',values))

;;; prediction modes, numbered as in the reference decoder
(defconstant +dc-pred+ 0) (defconstant +v-pred+ 1) (defconstant +h-pred+ 2)
(defconstant +tm-pred+ 3) (defconstant +b-pred+ 4)
(defconstant +nearestmv+ 5) (defconstant +nearmv+ 6) (defconstant +zeromv+ 7)
(defconstant +newmv+ 8) (defconstant +splitmv+ 9)

;;; reference frames
(defconstant +intra-frame+ 0) (defconstant +last-frame+ 1)
(defconstant +golden-frame+ 2) (defconstant +altref-frame+ 3)

;;; inter-frame intra mode trees (differ from the key-frame ymode tree)
(defparameter +y-mode-tree+ (fixnum-vector 0 2 4 6 -1 -2 -3 -4))       ; -DC_PRED = 0 is a leaf
(defparameter +default-y-mode-probs+ (fixnum-vector 112 86 140 37))
(defparameter +default-uv-mode-probs+ (fixnum-vector 162 101 204))
(defparameter +default-b-mode-probs+ (fixnum-vector 120 90 79 133 87 85 80 111 151))

;;; mv_ref_tree: ZEROMV | NEARESTMV | NEARMV | NEWMV | SPLITMV
(defparameter +mv-ref-tree+ (fixnum-vector -7 2 -5 4 -6 6 -8 -9))
(defparameter +mode-contexts+                   ; mv_counts_to_probs[6][4]
  (make-array '(6 4) :element-type '(signed-byte 32)
                     :initial-contents '((7 1 1 143) (14 18 14 107) (135 64 57 68)
                                         (60 56 128 65) (159 134 128 34) (234 188 128 28))))

;;; split mv
(defparameter +split-mv-tree+ (fixnum-vector -3 2 -2 4 0 -1))          ; 16x8=0 8x16=1 8x8=2 4x4=3
(defparameter +split-mv-probs+ (fixnum-vector 110 111 150))
(defparameter +mv-partitions+
  (make-array '(4 16) :element-type '(signed-byte 32)
                      :initial-contents '((0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
                                          (0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1)
                                          (0 0 1 1 0 0 1 1 2 2 3 3 2 2 3 3)
                                          (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))
(defparameter +mv-partition-count+ (fixnum-vector 2 2 4 16))
;;; sub_mv_ref tree: LEFT4X4=0 | ABOVE4X4=1 | ZERO4X4=2 | NEW4X4=3
(defparameter +submv-ref-tree+ (fixnum-vector 0 2 -1 4 -2 -3))
(defparameter +submv-ref-probs+                 ; [context][3]
  (make-array '(5 3) :element-type '(signed-byte 32)
                     :initial-contents '((147 136 18) (106 145 1) (179 121 1) (223 1 34) (208 1 1))))

;;; motion vector component coding (RFC 6386 s17)
;;; layout per component (19 entries): 0 is_short, 1 sign, 2..8 short tree, 9..18 long bits
(defparameter +small-mv-tree+ (fixnum-vector 2 8 4 6 0 -1 -2 -3 10 12 -4 -5 -6 -7))
(defparameter +mv-update-probs+
  (make-array 38 :element-type '(signed-byte 32)
                 :initial-contents '(237 246 253 253 254 254 254 254 254 254 254 254 254 254 250 250 252 254 254
                                     231 243 245 253 254 254 254 254 254 254 254 254 254 254 251 251 254 254 254)))
(defparameter +default-mv-probs+                ; row component then column component
  (make-array 38 :element-type '(signed-byte 32)
                 :initial-contents '(162 128 225 146 172 147 214 39 156 128 129 132 75 145 178 206 239 254 254
                                     164 128 204 170 119 235 140 230 228 128 130 130 74 148 180 203 236 254 254)))

;;; sub-pixel interpolation filters (RFC 6386 s18.3), indexed by eighth-pel fraction
(defparameter +sixtap-filters+
  (make-array '(8 6) :element-type '(signed-byte 32)
                     :initial-contents '((0 0 128 0 0 0) (0 -6 123 12 -1 0) (2 -11 108 36 -8 1)
                                         (0 -9 93 50 -6 0) (3 -16 77 77 -16 3) (0 -6 50 93 -9 0)
                                         (1 -8 36 108 -11 2) (0 -1 12 123 -6 0))))
(defparameter +bilinear-filters+
  (make-array '(8 6) :element-type '(signed-byte 32)
                     :initial-contents '((0 0 128 0 0 0) (0 0 112 16 0 0) (0 0 96 32 0 0)
                                         (0 0 80 48 0 0) (0 0 64 64 0 0) (0 0 48 80 0 0)
                                         (0 0 32 96 0 0) (0 0 16 112 0 0))))

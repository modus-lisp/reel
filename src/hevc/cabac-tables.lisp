;;;; hevc/cabac-tables.lisp — GENERATED.  The CABAC context initialisation values (Tables 9-5..9-37).
;;;;
;;;; HEVC has 179 adaptive contexts, and every one of them starts each slice at a probability the
;;;; standard states as an eight-bit initValue.  There are three sets of those values — one for I
;;;; slices and two the P and B slices choose between with cabac_init_flag — because a context that
;;;; means "this block has coefficients" starts in a very different place in an intra picture than
;;;; in a picture that could predict from its neighbours.
;;;;
;;;; The initValue is not a probability.  It is a SLOPE and an OFFSET packed into one byte, and the
;;;; probability is a line evaluated at the slice quantiser: a coarsely quantised slice really does
;;;; have different statistics from a fine one, so the tables would otherwise have to be repeated
;;;; per QP.  %INIT-CABAC does that arithmetic.
;;;;
;;;; The contexts are laid out as one flat array, and +CTX-*+ below names where each syntax element
;;;; starts in it.  Elements with no entry are bypass coded: they have no adaptive state at all.
;;;;
;;;; Extracted mechanically, and checked against properties the specification states apart from the
;;;; numbers: three sets of exactly 179 values, every value a byte, and the offsets a running sum of
;;;; the bin counts that ends at 179.

(in-package #:reel.hevc)

(defconstant +cabac-contexts+ 179)

(defconstant +ctx-sao-merge-flag+ 0)
(defconstant +ctx-sao-type-idx+ 1)
(defconstant +ctx-split-coding-unit-flag+ 2)   ; 3 contexts
(defconstant +ctx-cu-transquant-bypass-flag+ 5)
(defconstant +ctx-skip-flag+ 6)   ; 3 contexts
(defconstant +ctx-cu-qp-delta+ 9)   ; 3 contexts
(defconstant +ctx-pred-mode-flag+ 12)
(defconstant +ctx-part-mode+ 13)   ; 4 contexts
(defconstant +ctx-prev-intra-luma-pred-flag+ 17)
(defconstant +ctx-intra-chroma-pred-mode+ 18)   ; 2 contexts
(defconstant +ctx-merge-flag+ 20)
(defconstant +ctx-merge-idx+ 21)
(defconstant +ctx-inter-pred-idc+ 22)   ; 5 contexts
(defconstant +ctx-ref-idx-l0+ 27)   ; 2 contexts
(defconstant +ctx-ref-idx-l1+ 29)   ; 2 contexts
(defconstant +ctx-abs-mvd-greater0-flag+ 31)   ; 2 contexts
(defconstant +ctx-abs-mvd-greater1-flag+ 33)   ; 2 contexts
(defconstant +ctx-mvp-lx-flag+ 35)
(defconstant +ctx-no-residual-data-flag+ 36)
(defconstant +ctx-split-transform-flag+ 37)   ; 3 contexts
(defconstant +ctx-cbf-luma+ 40)   ; 2 contexts
(defconstant +ctx-cbf-cb-cr+ 42)   ; 5 contexts
(defconstant +ctx-transform-skip-flag+ 47)   ; 2 contexts
(defconstant +ctx-explicit-rdpcm-flag+ 49)   ; 2 contexts
(defconstant +ctx-explicit-rdpcm-dir-flag+ 51)   ; 2 contexts
(defconstant +ctx-last-significant-coeff-x-prefix+ 53)   ; 18 contexts
(defconstant +ctx-last-significant-coeff-y-prefix+ 71)   ; 18 contexts
(defconstant +ctx-significant-coeff-group-flag+ 89)   ; 4 contexts
(defconstant +ctx-significant-coeff-flag+ 93)   ; 44 contexts
(defconstant +ctx-coeff-abs-level-greater1-flag+ 137)   ; 24 contexts
(defconstant +ctx-coeff-abs-level-greater2-flag+ 161)   ; 6 contexts
(defconstant +ctx-log2-res-scale-abs+ 167)   ; 8 contexts
(defconstant +ctx-res-scale-sign-flag+ 175)   ; 2 contexts
(defconstant +ctx-cu-chroma-qp-offset-flag+ 177)
(defconstant +ctx-cu-chroma-qp-offset-idx+ 178)

;;; initType 0 is an I slice; 1 and 2 are the two a P or B slice picks between.
(defparameter +cabac-init+
  (make-array '(3 179) :element-type '(unsigned-byte 8)
              :initial-contents
              (list
               (list
   153 200 139 141 157 154 154 154 154 154 154 154 154 184 154 154
   154 184  63 139 154 154 154 154 154 154 154 154 154 154 154 154
   154 154 154 154 154 153 138 138 111 141  94 138 182 154 154 139
   139 139 139 139 139 110 110 124 125 140 153 125 127 140 109 111
   143 127 111  79 108 123  63 110 110 124 125 140 153 125 127 140
   109 111 143 127 111  79 108 123  63  91 171 134 141 111 111 125
   110 110  94 124 108 124 107 125 141 179 153 125 107 125 141 179
   153 125 107 125 141 179 153 125 140 139 182 182 152 136 152 136
   153 136 139 111 136 139 111 141 111 140  92 137 138 140 152 138
   139 153  74 149  92 139 107 122 152 140 179 166 182 140 227 122
   197 138 153 136 167 152 152 154 154 154 154 154 154 154 154 154
   154 154 154)
               (list
   153 185 107 139 126 154 197 185 201 154 154 154 149 154 139 154
   154 154 152 139 110 122  95  79  63  31  31 153 153 153 153 140
   198 140 198 168  79 124 138  94 153 111 149 107 167 154 154 139
   139 139 139 139 139 125 110  94 110  95  79 125 111 110  78 110
   111 111  95  94 108 123 108 125 110  94 110  95  79 125 111 110
    78 110 111 111  95  94 108 123 108 121 140  61 154 155 154 139
   153 139 123 123  63 153 166 183 140 136 153 154 166 183 140 136
   153 154 166 183 140 136 153 154 170 153 123 123 107 121 107 121
   167 151 183 140 151 183 140 140 140 154 196 196 167 154 152 167
   182 182 134 149 136 153 121 136 137 169 194 166 167 154 167 137
   182 107 167  91 122 107 167 154 154 154 154 154 154 154 154 154
   154 154 154)
               (list
   153 160 107 139 126 154 197 185 201 154 154 154 134 154 139 154
   154 183 152 139 154 137  95  79  63  31  31 153 153 153 153 169
   198 169 198 168  79 224 167 122 153 111 149  92 167 154 154 139
   139 139 139 139 139 125 110 124 110  95  94 125 111 111  79 125
   126 111 111  79 108 123  93 125 110 124 110  95  94 125 111 111
    79 125 126 111 111  79 108 123  93 121 140  61 154 170 154 139
   153 139 123 123  63 124 166 183 140 136 153 154 166 183 140 136
   153 154 166 183 140 136 153 154 170 153 138 138 122 121 122 121
   167 151 183 140 151 183 140 140 140 154 196 167 167 154 152 167
   182 182 134 149 136 153 121 136 122 169 208 166 167 154 152 167
   182 107 167  91 107 107 167 154 154 154 154 154 154 154 154 154
   154 154 154)
               )))

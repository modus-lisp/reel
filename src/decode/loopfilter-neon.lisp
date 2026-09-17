;;;; decode/loopfilter-neon.lisp — the VP8 loop-filter edge kernels on integer
;;;; NEON lanes (Modus SIMD layer 2b).  Loaded after loopfilter.lisp; #+modus,
;;;; last-definition-wins.  Bit-exact with the scalar kernels.
;;;;
;;;; Only the HORIZONTAL orientation vectorises directly: filter-plane-edges
;;;; passes ALONG=1 for a horizontal edge, so the COUNT samples along the edge
;;;; are contiguous in memory and load as one u8x16 (COUNT is 16 luma / 8
;;;; chroma).  The samples ACROSS the edge (p3..q3) are S apart (S=stride here),
;;;; so each of p3,p2,p1,p0,q0,q1,q2,q3 is its own contiguous COUNT-wide load.
;;;; The VERTICAL orientation (ALONG=stride) has strided along-samples that NEON
;;;; cannot gather in one op, so it falls through to the scalar kernel — correct,
;;;; just not yet accelerated (a transpose is the follow-up).
;;;;
;;;; All arithmetic is the specification's, in unsigned sample space (the file's
;;;; §15.2 note): differences of samples, widened u8->s16 (uxtl low 8, uxtl2 high
;;;; 8 -> two s16x8 vectors for 16 lanes), c8 = clamp to [-128,127] via
;;;; smax16(-128, smin16(127, .)), the byte results via sqxtun (clamp 0..255).
;;;; The per-lane gate becomes a u8x16 mask (cmhs / uabd), applied with bsl.
(in-package #:reel.decode)

#+modus
(progn

;; All macros expand into the vector tree (a helper defun cannot return a
;; vi-pack), operating on s16x8 lanes.
;; c8 = clamp to [-128,127].
(defmacro %vc8 (v) `(%vi-smax16 (%vi-smin16 ,v (%vi-dup16 127)) (%vi-dup16 -128)))
;; clamp a value already known to fit widely -> [0,255], kept in s16.
(defmacro %vcl255 (v) `(%vi-smax16 (%vi-smin16 ,v (%vi-dup16 255)) (%vi-dup16 0)))
;; per-lane mask 0xFFFF where (lim >= x), else 0, from the sign of lim-x.
(defmacro %vle (limv xv)
  `(%vi-eor (%vi-sshr16 (%vi-sub16 ,limv ,xv) 15) (%vi-dup16 -1)))
;; mask where (x > thr): the complement of (thr >= x).
(defmacro %vgt (thrv xv) `(%vi-eor (%vle ,thrv ,xv) (%vi-dup16 -1)))
;; |a-b| on s16 lanes.
(defmacro %vabsd (a b) `(%vi-abs16 (%vi-sub16 ,a ,b)))
;; load u8x8 at OFF widened to s16x8.
(defmacro %vld (data off) `(%vi-uxtl (%vi-ld8 ,data ,off)))

;; ---------------------------------------------------------------------------
;; Vertical edges (S = 1): a line's eight taps p3..q3 are eight consecutive
;; bytes, so an 8x8 BYTE TRANSPOSE turns eight lines into eight tap vectors.
;; It is done by NEON through the scratch in three pair stages (TRN1/TRN2 on
;; .8B, then .4H, then .2S), each store a 4-instruction ld/ld/trn/st tree, so
;; nothing exceeds the vector register pool.  Scratch layout: stage-1 output A
;; at 0..63, stage-2 B at 64..127, the tap-major result C at 128..191
;; (tap k = 8 bytes at 128+8k).  The same network run backwards puts the
;; filtered taps back on the eight lines.  The h8 kernels run unchanged on C.
(defvar *lf-vscratch* (make-array 192 :element-type '(unsigned-byte 8) :initial-element 0))

(defun %lf-vtrans-in (data q0 along sc)
  (declare (type u8vec data sc) (type fixnum q0 along) (optimize (speed 3) (safety 0)))
  (let* ((r0 (- q0 4)) (r1 (+ r0 along)) (r2 (+ r1 along)) (r3 (+ r2 along))
         (r4 (+ r3 along)) (r5 (+ r4 along)) (r6 (+ r5 along)) (r7 (+ r6 along)))
    (declare (type fixnum r0 r1 r2 r3 r4 r5 r6 r7))
    ;; stage 1 (bytes): A0/A1 from lines 0,1; A2/A3 from 2,3; A4/A5 from 4,5; A6/A7 from 6,7
    (%vi-st8 sc 0 (%vi-trn1-8 (%vi-ld8 data r0) (%vi-ld8 data r1)))
    (%vi-st8 sc 8 (%vi-trn2-8 (%vi-ld8 data r0) (%vi-ld8 data r1)))
    (%vi-st8 sc 16 (%vi-trn1-8 (%vi-ld8 data r2) (%vi-ld8 data r3)))
    (%vi-st8 sc 24 (%vi-trn2-8 (%vi-ld8 data r2) (%vi-ld8 data r3)))
    (%vi-st8 sc 32 (%vi-trn1-8 (%vi-ld8 data r4) (%vi-ld8 data r5)))
    (%vi-st8 sc 40 (%vi-trn2-8 (%vi-ld8 data r4) (%vi-ld8 data r5)))
    (%vi-st8 sc 48 (%vi-trn1-8 (%vi-ld8 data r6) (%vi-ld8 data r7)))
    (%vi-st8 sc 56 (%vi-trn2-8 (%vi-ld8 data r6) (%vi-ld8 data r7)))
    ;; stage 2 (halfwords): B0/B2 from (A0,A2), B1/B3 from (A1,A3), B4/B6 from (A4,A6), B5/B7 from (A5,A7)
    (%vi-st8 sc 64 (%vi-trn1 (%vi-ld8 sc 0) (%vi-ld8 sc 16)))
    (%vi-st8 sc 80 (%vi-trn2 (%vi-ld8 sc 0) (%vi-ld8 sc 16)))
    (%vi-st8 sc 72 (%vi-trn1 (%vi-ld8 sc 8) (%vi-ld8 sc 24)))
    (%vi-st8 sc 88 (%vi-trn2 (%vi-ld8 sc 8) (%vi-ld8 sc 24)))
    (%vi-st8 sc 96 (%vi-trn1 (%vi-ld8 sc 32) (%vi-ld8 sc 48)))
    (%vi-st8 sc 112 (%vi-trn2 (%vi-ld8 sc 32) (%vi-ld8 sc 48)))
    (%vi-st8 sc 104 (%vi-trn1 (%vi-ld8 sc 40) (%vi-ld8 sc 56)))
    (%vi-st8 sc 120 (%vi-trn2 (%vi-ld8 sc 40) (%vi-ld8 sc 56)))
    ;; stage 3 (words): C0/C4 from (B0,B4), C1/C5 from (B1,B5), C2/C6 from (B2,B6), C3/C7 from (B3,B7)
    (%vi-st8 sc 128 (%vi-trn1-32 (%vi-ld8 sc 64) (%vi-ld8 sc 96)))
    (%vi-st8 sc 160 (%vi-trn2-32 (%vi-ld8 sc 64) (%vi-ld8 sc 96)))
    (%vi-st8 sc 136 (%vi-trn1-32 (%vi-ld8 sc 72) (%vi-ld8 sc 104)))
    (%vi-st8 sc 168 (%vi-trn2-32 (%vi-ld8 sc 72) (%vi-ld8 sc 104)))
    (%vi-st8 sc 144 (%vi-trn1-32 (%vi-ld8 sc 80) (%vi-ld8 sc 112)))
    (%vi-st8 sc 176 (%vi-trn2-32 (%vi-ld8 sc 80) (%vi-ld8 sc 112)))
    (%vi-st8 sc 152 (%vi-trn1-32 (%vi-ld8 sc 88) (%vi-ld8 sc 120)))
    (%vi-st8 sc 184 (%vi-trn2-32 (%vi-ld8 sc 88) (%vi-ld8 sc 120)))
    nil))

(defun %lf-vtrans-out (data q0 along sc)
  "The transpose network run on C (taps) yields the eight lines; the last stage stores them."
  (declare (type u8vec data sc) (type fixnum q0 along) (optimize (speed 3) (safety 0)))
  (let* ((r0 (- q0 4)) (r1 (+ r0 along)) (r2 (+ r1 along)) (r3 (+ r2 along))
         (r4 (+ r3 along)) (r5 (+ r4 along)) (r6 (+ r5 along)) (r7 (+ r6 along)))
    (declare (type fixnum r0 r1 r2 r3 r4 r5 r6 r7))
    (%vi-st8 sc 0 (%vi-trn1-8 (%vi-ld8 sc 128) (%vi-ld8 sc 136)))
    (%vi-st8 sc 8 (%vi-trn2-8 (%vi-ld8 sc 128) (%vi-ld8 sc 136)))
    (%vi-st8 sc 16 (%vi-trn1-8 (%vi-ld8 sc 144) (%vi-ld8 sc 152)))
    (%vi-st8 sc 24 (%vi-trn2-8 (%vi-ld8 sc 144) (%vi-ld8 sc 152)))
    (%vi-st8 sc 32 (%vi-trn1-8 (%vi-ld8 sc 160) (%vi-ld8 sc 168)))
    (%vi-st8 sc 40 (%vi-trn2-8 (%vi-ld8 sc 160) (%vi-ld8 sc 168)))
    (%vi-st8 sc 48 (%vi-trn1-8 (%vi-ld8 sc 176) (%vi-ld8 sc 184)))
    (%vi-st8 sc 56 (%vi-trn2-8 (%vi-ld8 sc 176) (%vi-ld8 sc 184)))
    (%vi-st8 sc 64 (%vi-trn1 (%vi-ld8 sc 0) (%vi-ld8 sc 16)))
    (%vi-st8 sc 80 (%vi-trn2 (%vi-ld8 sc 0) (%vi-ld8 sc 16)))
    (%vi-st8 sc 72 (%vi-trn1 (%vi-ld8 sc 8) (%vi-ld8 sc 24)))
    (%vi-st8 sc 88 (%vi-trn2 (%vi-ld8 sc 8) (%vi-ld8 sc 24)))
    (%vi-st8 sc 96 (%vi-trn1 (%vi-ld8 sc 32) (%vi-ld8 sc 48)))
    (%vi-st8 sc 112 (%vi-trn2 (%vi-ld8 sc 32) (%vi-ld8 sc 48)))
    (%vi-st8 sc 104 (%vi-trn1 (%vi-ld8 sc 40) (%vi-ld8 sc 56)))
    (%vi-st8 sc 120 (%vi-trn2 (%vi-ld8 sc 40) (%vi-ld8 sc 56)))
    (%vi-st8 data r0 (%vi-trn1-32 (%vi-ld8 sc 64) (%vi-ld8 sc 96)))
    (%vi-st8 data r4 (%vi-trn2-32 (%vi-ld8 sc 64) (%vi-ld8 sc 96)))
    (%vi-st8 data r1 (%vi-trn1-32 (%vi-ld8 sc 72) (%vi-ld8 sc 104)))
    (%vi-st8 data r5 (%vi-trn2-32 (%vi-ld8 sc 72) (%vi-ld8 sc 104)))
    (%vi-st8 data r2 (%vi-trn1-32 (%vi-ld8 sc 80) (%vi-ld8 sc 112)))
    (%vi-st8 data r6 (%vi-trn2-32 (%vi-ld8 sc 80) (%vi-ld8 sc 112)))
    (%vi-st8 data r3 (%vi-trn1-32 (%vi-ld8 sc 88) (%vi-ld8 sc 120)))
    (%vi-st8 data r7 (%vi-trn2-32 (%vi-ld8 sc 88) (%vi-ld8 sc 120)))
    nil))

(defun %edge-simple (data start along s count elim)
  (declare (type u8vec data) (type dim start along s) (type (integer 0 64) count)
           (type (signed-byte 16) elim) (optimize (speed 3) (safety 0)))
  (if (/= along 1)
      ;; vertical edge: NEON-transpose 8 lines into tap vectors, run the h8 kernel, transpose back
      (let ((sc *lf-vscratch*) (g 0))
        (declare (type u8vec sc) (type (integer 0 64) g))
        (loop
          (when (>= g count) (return nil))
          (let ((q0 (+ start (* g along))))
            (declare (type fixnum q0))
            (%lf-vtrans-in data q0 along sc)
            (%edge-simple-h8 sc 144 152 160 168 elim)
            (%lf-vtrans-out data q0 along sc))
          (setq g (+ g 8))))
      ;; horizontal edge: COUNT contiguous along-samples in [start, start+count)
      (let ((s2 (* 2 s)) (g 0))
        (declare (type dim s2) (type (integer 0 64) g))
        (loop
          (when (>= g count) (return nil))
          (let ((base (+ start g)))                    ; 8 contiguous lanes here
            (declare (type dim base))
            (%edge-simple-h8 data (- base s2) (- base s) base (+ base s) elim))
          (setq g (+ g 8))))))

;; one 8-lane group: p1o/p0o/q0o/q1o are the byte offsets of the eight
;; p1,p0,q0,q1.  Everything is s16x8; the final store narrows with sqxtun,
;; which is the identity on the already-[0,255] result values.  Only A and MASK
;; are held in registers (the FP pool is six wide and there is no vector spill
;; yet); every other value is an inline tree reloading its samples from memory.
(defun %edge-simple-h8 (data p1o p0o q0o q1o elim)
  (declare (type u8vec data) (type dim p1o p0o q0o q1o) (type (signed-byte 16) elim)
           (optimize (speed 3) (safety 0)))
  ;; The FP pool is 12 wide now, so every sample and derived value is held in a
  ;; register: p1/p0/q0/q1 loaded once, the adjustment computed from them.
  (let* ((p1 (%vi-uxtl (%vi-ld8 data p1o)))
         (p0 (%vi-uxtl (%vi-ld8 data p0o)))
         (q0 (%vi-uxtl (%vi-ld8 data q0o)))
         (q1 (%vi-uxtl (%vi-ld8 data q1o)))
         (a  (%vc8 (%vi-add16 (%vc8 (%vi-sub16 p1 q1))
                              (%vi-mul16 (%vi-sub16 q0 p0) (%vi-dup16 3)))))
         (mask (%vle (%vi-dup16 elim)
                     (%vi-add16 (%vi-shl16 (%vi-abs16 (%vi-sub16 p0 q0)) 1)
                                (%vi-sshr16 (%vi-abs16 (%vi-sub16 p1 q1)) 1))))
         (b  (%vi-sshr16 (%vc8 (%vi-add16 a (%vi-dup16 3))) 3))
         (a2 (%vi-sshr16 (%vc8 (%vi-add16 a (%vi-dup16 4))) 3)))
    (declare (type vi-pack p1 p0 q0 q1 a mask b a2))
    (%vi-st8 data p0o (%vi-sqxtun (%vi-bsl mask (%vcl255 (%vi-add16 p0 b)) p0)))
    (%vi-st8 data q0o (%vi-sqxtun (%vi-bsl mask (%vcl255 (%vi-sub16 q0 a2)) q0)))))

;;; ---- normal sub-block edge filter (%edge-sub): p1..q1 may move -------------
(defun %edge-sub (data start along s count hthr ilim elim)
  (declare (type u8vec data) (type dim start along s) (type (integer 0 64) count)
           (type (signed-byte 16) hthr ilim elim) (optimize (speed 3) (safety 0)))
  (if (/= along 1)
      ;; vertical edge: NEON-transpose 8 lines into tap vectors, run the h8 kernel, transpose back
      (let ((sc *lf-vscratch*) (g 0))
        (declare (type u8vec sc) (type (integer 0 64) g))
        (loop
          (when (>= g count) (return nil))
          (let ((q0 (+ start (* g along))))
            (declare (type fixnum q0))
            (%lf-vtrans-in data q0 along sc)
            (%edge-sub-h8 sc 128 136 144 152 160 168 176 184 hthr ilim elim)
            (%lf-vtrans-out data q0 along sc))
          (setq g (+ g 8))))
      (let ((s2 (* 2 s)) (s3 (* 3 s)) (s4 (* 4 s)) (g 0))
        (declare (type dim s2 s3 s4) (type (integer 0 64) g))
        (loop
          (when (>= g count) (return nil))
          (let ((q0o (+ start g)))
            (declare (type dim q0o))
            (%edge-sub-h8 data (- q0o s4) (- q0o s3) (- q0o s2) (- q0o s)
                          q0o (+ q0o s) (+ q0o s2) (+ q0o s3) hthr ilim elim))
          (setq g (+ g 8))))))

;; one 8-lane group of the normal sub-block filter.  Held in registers: the four
;; near samples p1/p0/q0/q1, the edge-ok mask OK, the adjustment A, and OKHEV
;; (edge-ok AND NOT hev, the p1/q1-update mask).  p2/p3/q2/q3 are inline in the
;; gate; b/a2/adj are recomputed inline in the stores.
(defun %edge-sub-h8 (data p3o p2o p1o p0o q0o q1o q2o q3o hthr ilim elim)
  (declare (type u8vec data) (type dim p3o p2o p1o p0o q0o q1o q2o q3o)
           (type (signed-byte 16) hthr ilim elim) (optimize (speed 3) (safety 0)))
  (let* ((p1 (%vld data p1o)) (p0 (%vld data p0o)) (q0 (%vld data q0o)) (q1 (%vld data q1o))
         (ok (%vi-and
              (%vi-and (%vi-and (%vle (%vi-dup16 elim)
                                      (%vi-add16 (%vi-shl16 (%vabsd p0 q0) 1) (%vi-sshr16 (%vabsd p1 q1) 1)))
                                (%vi-and (%vle (%vi-dup16 ilim) (%vabsd (%vld data p3o) (%vld data p2o)))
                                         (%vle (%vi-dup16 ilim) (%vabsd (%vld data p2o) p1))))
                       (%vi-and (%vle (%vi-dup16 ilim) (%vabsd p1 p0))
                                (%vle (%vi-dup16 ilim) (%vabsd q1 q0))))
              (%vi-and (%vle (%vi-dup16 ilim) (%vabsd (%vld data q3o) (%vld data q2o)))
                       (%vle (%vi-dup16 ilim) (%vabsd (%vld data q2o) q1)))))
         (hev (%vi-orr (%vgt (%vi-dup16 hthr) (%vabsd p1 p0)) (%vgt (%vi-dup16 hthr) (%vabsd q1 q0))))
         (a (%vc8 (%vi-add16 (%vi-bsl hev (%vc8 (%vi-sub16 p1 q1)) (%vi-dup16 0))
                             (%vi-mul16 (%vi-sub16 q0 p0) (%vi-dup16 3)))))
         (okhev (%vi-and ok (%vi-eor hev (%vi-dup16 -1)))))
    (declare (type vi-pack p1 p0 q0 q1 ok hev a okhev))
    ;; p0,q0: updated where edge-ok.  b = (c8(a+3))>>3, a2 = (c8(a+4))>>3.
    (%vi-st8 data p0o (%vi-sqxtun (%vi-bsl ok
                        (%vcl255 (%vi-add16 p0 (%vi-sshr16 (%vc8 (%vi-add16 a (%vi-dup16 3))) 3))) p0)))
    (%vi-st8 data q0o (%vi-sqxtun (%vi-bsl ok
                        (%vcl255 (%vi-sub16 q0 (%vi-sshr16 (%vc8 (%vi-add16 a (%vi-dup16 4))) 3))) q0)))
    ;; p1,q1: updated where edge-ok AND NOT hev, by adj = (a2+1)>>1.
    (%vi-st8 data p1o (%vi-sqxtun (%vi-bsl okhev
                        (%vcl255 (%vi-add16 p1 (%vi-sshr16 (%vi-add16 (%vi-sshr16 (%vc8 (%vi-add16 a (%vi-dup16 4))) 3) (%vi-dup16 1)) 1))) p1)))
    (%vi-st8 data q1o (%vi-sqxtun (%vi-bsl okhev
                        (%vcl255 (%vi-sub16 q1 (%vi-sshr16 (%vi-add16 (%vi-sshr16 (%vc8 (%vi-add16 a (%vi-dup16 4))) 3) (%vi-dup16 1)) 1))) q1)))))

) ; progn

;;; ---- macroblock-edge filter (RFC 6386 s15.3), 8 along-samples at a time ------
;;; Same gate and hev as the sub-block filter.  w always includes the outer
;;; term c8(p1-q1); with hev only p0/q0 move by b=(c8(w+3))>>3 / a=(c8(w+4))>>3;
;;; without hev the three pairs move by (27w+63)>>7, (18w+63)>>7, (9w+63)>>7,
;;; each clamped to s8 first, exactly as the scalar %edge-mb.
#+modus
(defun %edge-mb-h8 (data p3o p2o p1o p0o q0o q1o q2o q3o hthr ilim elim)
  (declare (type u8vec data) (type dim p3o p2o p1o p0o q0o q1o q2o q3o)
           (type (signed-byte 16) hthr ilim elim) (optimize (speed 3) (safety 0)))
  ;; Five register-resident values; p1/q1/p2/q2 are re-loaded at each use so the
  ;; store trees fit the vector pool.
  (let* ((p0 (%vld data p0o)) (q0 (%vld data q0o))
         (ok (%vi-and
              (%vi-and (%vi-and (%vle (%vi-dup16 elim)
                                      (%vi-add16 (%vi-shl16 (%vabsd p0 q0) 1) (%vi-sshr16 (%vabsd (%vld data p1o) (%vld data q1o)) 1)))
                                (%vi-and (%vle (%vi-dup16 ilim) (%vabsd (%vld data p3o) (%vld data p2o)))
                                         (%vle (%vi-dup16 ilim) (%vabsd (%vld data p2o) (%vld data p1o)))))
                       (%vi-and (%vle (%vi-dup16 ilim) (%vabsd (%vld data p1o) p0))
                                (%vle (%vi-dup16 ilim) (%vabsd (%vld data q1o) q0))))
              (%vi-and (%vle (%vi-dup16 ilim) (%vabsd (%vld data q3o) (%vld data q2o)))
                       (%vle (%vi-dup16 ilim) (%vabsd (%vld data q2o) (%vld data q1o))))))
         (hev (%vi-orr (%vgt (%vi-dup16 hthr) (%vabsd (%vld data p1o) p0)) (%vgt (%vi-dup16 hthr) (%vabsd (%vld data q1o) q0))))
         (w (%vc8 (%vi-add16 (%vc8 (%vi-sub16 (%vld data p1o) (%vld data q1o))) (%vi-mul16 (%vi-sub16 q0 p0) (%vi-dup16 3))))))
    (declare (type vi-pack p0 q0 ok hev w))
    ;; p0/q0: hev -> b=(c8(w+3))>>3 / a=(c8(w+4))>>3; not hev -> c8((27w+63)>>7): clamp AFTER the shift, as the scalar does.
    (%vi-st8 data p0o (%vi-sqxtun (%vi-bsl ok
                        (%vcl255 (%vi-add16 p0 (%vi-bsl hev (%vi-sshr16 (%vc8 (%vi-add16 w (%vi-dup16 3))) 3)
                                                          (%vc8 (%vi-sshr16 (%vi-add16 (%vi-mul16 w (%vi-dup16 27)) (%vi-dup16 63)) 7)))))
                        p0)))
    (%vi-st8 data q0o (%vi-sqxtun (%vi-bsl ok
                        (%vcl255 (%vi-sub16 q0 (%vi-bsl hev (%vi-sshr16 (%vc8 (%vi-add16 w (%vi-dup16 4))) 3)
                                                          (%vc8 (%vi-sshr16 (%vi-add16 (%vi-mul16 w (%vi-dup16 27)) (%vi-dup16 63)) 7)))))
                        q0)))
    ;; p1/q1, p2/q2: only where ok and not hev, by (c8(18w+63))>>7 and (c8(9w+63))>>7.
    (%vi-st8 data p1o (%vi-sqxtun (%vi-bsl (%vi-and ok (%vi-eor hev (%vi-dup16 -1)))
                        (%vcl255 (%vi-add16 (%vld data p1o) (%vc8 (%vi-sshr16 (%vi-add16 (%vi-mul16 w (%vi-dup16 18)) (%vi-dup16 63)) 7)))) (%vld data p1o))))
    (%vi-st8 data q1o (%vi-sqxtun (%vi-bsl (%vi-and ok (%vi-eor hev (%vi-dup16 -1)))
                        (%vcl255 (%vi-sub16 (%vld data q1o) (%vc8 (%vi-sshr16 (%vi-add16 (%vi-mul16 w (%vi-dup16 18)) (%vi-dup16 63)) 7)))) (%vld data q1o))))
    (%vi-st8 data p2o (%vi-sqxtun (%vi-bsl (%vi-and ok (%vi-eor hev (%vi-dup16 -1)))
                        (%vcl255 (%vi-add16 (%vld data p2o) (%vc8 (%vi-sshr16 (%vi-add16 (%vi-mul16 w (%vi-dup16 9)) (%vi-dup16 63)) 7)))) (%vld data p2o))))
    (%vi-st8 data q2o (%vi-sqxtun (%vi-bsl (%vi-and ok (%vi-eor hev (%vi-dup16 -1)))
                        (%vcl255 (%vi-sub16 (%vld data q2o) (%vc8 (%vi-sshr16 (%vi-add16 (%vi-mul16 w (%vi-dup16 9)) (%vi-dup16 63)) 7)))) (%vld data q2o))))))

(defun %edge-mb (data start along s count hthr ilim elim)
  (declare (type u8vec data) (type dim start along s) (type (integer 0 64) count)
           (type (signed-byte 16) hthr ilim elim) (optimize (speed 3) (safety 0)))
  (if (/= along 1)
      ;; vertical edge: NEON-transpose 8 lines into tap vectors, run the h8 kernel, transpose back
      (let ((sc *lf-vscratch*) (g 0))
        (declare (type u8vec sc) (type (integer 0 64) g))
        (loop
          (when (>= g count) (return nil))
          (let ((q0 (+ start (* g along))))
            (declare (type fixnum q0))
            (%lf-vtrans-in data q0 along sc)
            (%edge-mb-h8 sc 128 136 144 152 160 168 176 184 hthr ilim elim)
            (%lf-vtrans-out data q0 along sc))
          (setq g (+ g 8))))
      (let ((s2 (* 2 s)) (s3 (* 3 s)) (s4 (* 4 s)) (g 0))
        (declare (type dim s2 s3 s4) (type (integer 0 64) g))
        (loop
          (when (>= g count) (return nil))
          (let ((q0o (+ start g)))
            (declare (type dim q0o))
            (%edge-mb-h8 data (- q0o s4) (- q0o s3) (- q0o s2) (- q0o s)
                         q0o (+ q0o s) (+ q0o s2) (+ q0o s3) hthr ilim elim))
          (setq g (+ g 8))))))

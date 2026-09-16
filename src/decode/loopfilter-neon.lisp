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
;; per-lane mask 0xFFFF where (elim >= cond), else 0, from the sign of elim-cond.
(defmacro %vle (elimv condv)
  `(%vi-eor (%vi-sshr16 (%vi-sub16 ,elimv ,condv) 15) (%vi-dup16 -1)))

;; the simple filter's adjustment for one 8-lane half at byte offset OFF into
;; the vectors, stored back through masked bsl.  Written as a LET* of vi-pack
;; temps so the compiler keeps them in Q registers.
(defun %edge-simple (data start along s count elim)
  (declare (type u8vec data) (type dim start along s) (type (integer 0 64) count)
           (type (signed-byte 16) elim) (optimize (speed 3) (safety 0)))
  (if (/= along 1)
      ;; vertical edge: strided along-samples, scalar path (bit-exact reference)
      (let ((s2 (* 2 s)))
        (declare (type dim s2))
        (do ((k 0 (1+ k)) (q0i start (+ q0i along)))
            ((>= k count))
          (declare (type (integer 0 64) k) (type dim q0i))
          (let* ((p1i (- q0i s2)) (p0i (- q0i s)) (q1i (+ q0i s))
                 (p1 (aref data p1i)) (p0 (aref data p0i))
                 (q0 (aref data q0i)) (q1 (aref data q1i)))
            (declare (type dim p1i p0i q1i) (type (unsigned-byte 8) p1 p0 q0 q1))
            (when (<= (+ (* 2 (abs (- p0 q0))) (ash (abs (- p1 q1)) -1)) elim)
              (multiple-value-bind (np0 nq0) (%adjust t p1 p0 q0 q1)
                (setf (aref data p0i) np0 (aref data q0i) nq0))))))
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
  (let ((a (%vc8 (%vi-add16 (%vc8 (%vi-sub16 (%vi-uxtl (%vi-ld8 data p1o))
                                             (%vi-uxtl (%vi-ld8 data q1o))))
                            (%vi-mul16 (%vi-sub16 (%vi-uxtl (%vi-ld8 data q0o))
                                                  (%vi-uxtl (%vi-ld8 data p0o)))
                                       (%vi-dup16 3))))))
    (declare (type vi-pack a))
    (let ((mask (%vle (%vi-dup16 elim)
                      (%vi-add16 (%vi-shl16 (%vi-abs16 (%vi-sub16 (%vi-uxtl (%vi-ld8 data p0o))
                                                                  (%vi-uxtl (%vi-ld8 data q0o)))) 1)
                                 (%vi-sshr16 (%vi-abs16 (%vi-sub16 (%vi-uxtl (%vi-ld8 data p1o))
                                                                   (%vi-uxtl (%vi-ld8 data q1o)))) 1)))))
      (declare (type vi-pack mask))
      (%vi-st8 data p0o
        (%vi-sqxtun (%vi-bsl mask
                            (%vcl255 (%vi-add16 (%vi-uxtl (%vi-ld8 data p0o))
                                                (%vi-sshr16 (%vc8 (%vi-add16 a (%vi-dup16 3))) 3)))
                            (%vi-uxtl (%vi-ld8 data p0o)))))
      (%vi-st8 data q0o
        (%vi-sqxtun (%vi-bsl mask
                            (%vcl255 (%vi-sub16 (%vi-uxtl (%vi-ld8 data q0o))
                                                (%vi-sshr16 (%vc8 (%vi-add16 a (%vi-dup16 4))) 3)))
                            (%vi-uxtl (%vi-ld8 data q0o))))))))

) ; progn

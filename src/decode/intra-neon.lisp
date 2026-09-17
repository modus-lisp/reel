;;;; decode/intra-neon.lisp — the 4x4 residual add on integer NEON lanes (Modus
;;;; SIMD layer 2b).  Loaded after intra.lisp; #+modus, last-definition-wins
;;;; (the board's linkage cells make the precompiled callers use it).
;;;; The residual arrives as 16 packed little-endian s16 (vp8-idct-s16), so a
;;;; row is one 8-octet lane load; the prediction row widens u8->u16, ADD16,
;;;; and SQXTUN's unsigned saturation is exactly clamp255.  Bit-exact with the
;;;; scalar add-residual / add-residual-pred.
(in-package #:reel.decode)

#+modus
(defun add-residual (pl mx my res)
  (declare (type plane pl) (type (signed-byte 26) mx my) (type u8vec res)
           (optimize (speed 3) (safety 0)))
  (let* ((d (pl-data pl)) (s (pl-stride pl)) (o (+ (* (+ my 1) s) (+ mx 1))))
    (declare (type u8vec d) (type fixnum s o))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 d o)) (%vi-ld8 res 0))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 d o)) (%vi-ld8 res 8))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 d o)) (%vi-ld8 res 16))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 d o)) (%vi-ld8 res 24))))
    nil))

#+modus
(defun add-residual-pred (pl mx my pred res)
  (declare (type plane pl) (type (signed-byte 26) mx my) (type u8vec pred res)
           (optimize (speed 3) (safety 0)))
  (let* ((d (pl-data pl)) (s (pl-stride pl)) (o (+ (* (+ my 1) s) (+ mx 1))))
    (declare (type u8vec d) (type fixnum s o))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 pred 0)) (%vi-ld8 res 0))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 pred 4)) (%vi-ld8 res 8))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 pred 8)) (%vi-ld8 res 16))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 pred 12)) (%vi-ld8 res 24))))
    nil))

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

;;; DC-only residual and zero-residual subblock writes (see intra.lisp): the
;;; same row shape as add-residual with a DUP16 of the DC value / a plain copy.
#+modus
(defun add-residual-dc (pl mx my v)
  (declare (type plane pl) (type (signed-byte 26) mx my) (type fixnum v) (optimize (speed 3) (safety 0)))
  (let* ((d (pl-data pl)) (s (pl-stride pl)) (o (+ (* (+ my 1) s) (+ mx 1))))
    (declare (type u8vec d) (type fixnum s o))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 d o)) (%vi-dup16 v))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 d o)) (%vi-dup16 v))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 d o)) (%vi-dup16 v))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 d o)) (%vi-dup16 v))))
    nil))

#+modus
(defun add-residual-pred-dc (pl mx my pred v)
  (declare (type plane pl) (type (signed-byte 26) mx my) (type u8vec pred) (type fixnum v) (optimize (speed 3) (safety 0)))
  (let* ((d (pl-data pl)) (s (pl-stride pl)) (o (+ (* (+ my 1) s) (+ mx 1))))
    (declare (type u8vec d) (type fixnum s o))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 pred 0)) (%vi-dup16 v))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 pred 4)) (%vi-dup16 v))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 pred 8)) (%vi-dup16 v))))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld4 pred 12)) (%vi-dup16 v))))
    nil))

#+modus
(defun copy-pred4 (pl mx my pred)
  (declare (type plane pl) (type (signed-byte 26) mx my) (type u8vec pred) (optimize (speed 3) (safety 0)))
  (let* ((d (pl-data pl)) (s (pl-stride pl)) (o (+ (* (+ my 1) s) (+ mx 1))))
    (declare (type u8vec d) (type fixnum s o))
    (%vi-st4 d o (%vi-ld4 pred 0))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-ld4 pred 4))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-ld4 pred 8))
    (setq o (+ o s))
    (%vi-st4 d o (%vi-ld4 pred 12))
    nil))

;;; 16x16 / 8x8 intra prediction on NEON lanes (RFC 6386 s12.2).  The scalar
;;; PREDICT-BLOCK in intra.lisp indexes the plane per pixel through PIDX; here
;;; a row is one or two lane loads/stores: DC and H are DUP8 fills, V is the
;;; above row replicated, TM widens the above row to u16, adds (left - corner)
;;; and narrows with SQXTUN, whose unsigned saturation is exactly clamp255.
;;; Vector values live only inside a row (the checker wants VI-PACK lets at the
;;; leaves, not around loops); the reloads are L1 hits.  Bit-exact with the
;;; scalar version (MD5 48/48).
#+modus
(defun predict-block (pl mx my size mode has-above has-left out)
  (declare (type plane pl) (type (signed-byte 26) mx my) (type fixnum size mode) (type u8vec out)
           (optimize (speed 3) (safety 0)))
  (let* ((data (pl-data pl)) (stride (pl-stride pl))
         (base (+ (* (+ my 1) stride) (+ mx 1)))   ; pidx
         (above (- base stride))                    ; first pixel of the row above
         (above8 (+ above 8)))
    (declare (type u8vec data) (type fixnum stride base above above8))
    (cond
      ((= mode 0)                                   ; DC
       (let ((dc 128) (s 0))
         (declare (type fixnum dc s))
         (cond
           ((and has-above has-left)
            (dotimes (i size) (setf s (+ s (aref data (+ above i)))))
            (let ((o (- base 1)))
              (declare (type fixnum o))
              (dotimes (i size) (setf s (+ s (aref data o))) (setf o (+ o stride))))
            (setf dc (ash (+ s size) (if (= size 16) -5 -4))))
           (has-above
            (dotimes (i size) (setf s (+ s (aref data (+ above i)))))
            (setf dc (ash (+ s (ash size -1)) (if (= size 16) -4 -3))))
           (has-left
            (let ((o (- base 1)))
              (declare (type fixnum o))
              (dotimes (i size) (setf s (+ s (aref data o))) (setf o (+ o stride))))
            (setf dc (ash (+ s (ash size -1)) (if (= size 16) -4 -3)))))
         (if (= size 16)
             (dotimes (r 16) (%vi-st16 out (* r 16) (%vi-dup8 dc)))
             (dotimes (r 8) (%vi-st8 out (* r 8) (%vi-dup8 dc))))))
      ((= mode 1)                                   ; V: the above row, replicated
       (if (= size 16)
           (dotimes (r 16) (%vi-st16 out (* r 16) (%vi-ld16 data above)))
           (dotimes (r 8) (%vi-st8 out (* r 8) (%vi-ld8 data above)))))
      ((= mode 2)                                   ; H: each row is its left pixel
       (let ((o (- base 1)))
         (declare (type fixnum o))
         (if (= size 16)
             (dotimes (r 16)
               (let ((lv (aref data o))) (declare (type fixnum lv)) (%vi-st16 out (* r 16) (%vi-dup8 lv)))
               (setf o (+ o stride)))
             (dotimes (r 8)
               (let ((lv (aref data o))) (declare (type fixnum lv)) (%vi-st8 out (* r 8) (%vi-dup8 lv)))
               (setf o (+ o stride))))))
      (t                                            ; TM: clamp(left + above - corner)
       (let ((p (aref data (- above 1))) (o (- base 1)))
         (declare (type fixnum p o))
         (if (= size 16)
             (dotimes (r 16)
               (let ((dl (- (aref data o) p)) (ro (* r 16)))
                 (declare (type fixnum dl ro))
                 (let ((d (%vi-dup16 dl)))
                   (declare (type vi-pack d))
                   (%vi-st8 out ro (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld8 data above)) d)))
                   (%vi-st8 out (+ ro 8) (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld8 data above8)) d)))))
               (setf o (+ o stride)))
             (dotimes (r 8)
               (let ((dl (- (aref data o) p)) (ro (* r 8)))
                 (declare (type fixnum dl ro))
                 (%vi-st8 out ro (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld8 data above)) (%vi-dup16 dl)))))
               (setf o (+ o stride)))))))
    nil))

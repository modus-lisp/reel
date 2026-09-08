;;;; mpeg2/idct.lisp — the inverse discrete cosine transform, and an honest note about it.
;;;;
;;;; THIS IS THE ONE PLACE IN THE STACK WHERE "BIT-EXACT" IS NOT A MEANINGFUL GOAL, and it is worth
;;;; saying why rather than quietly failing to reach it.
;;;;
;;;; H.264 and VP8 both specify their inverse transforms as exact integer arithmetic: there is one
;;;; right answer and a decoder either produces it or is wrong.  MPEG-2 does not.  ISO/IEC 13818-2
;;;; requires only that a decoder's IDCT meet the accuracy bounds of IEEE 1180 — a peak error of at
;;;; most 1, and mean and mean-square errors below stated limits, measured over pseudo-random blocks.
;;;; Two conforming decoders may legitimately disagree by one in a sample, and the standard contains
;;;; MISMATCH CONTROL (the forced oddness of the last coefficient, done in the entropy decoder)
;;;; precisely to stop that disagreement accumulating without bound over a group of pictures.
;;;;
;;;; So this is the classic separable integer IDCT: an eight-point transform applied to rows and
;;;; then to columns, with the cosines held as sixteen-bit fixed point.  The constants are
;;;;
;;;;     W(k) = round( cos(k*pi/16) * sqrt(2) * 2^14 )
;;;;
;;;; with one deliberate exception: W4 is 16383 rather than the 16384 that formula gives, because
;;;; 16384 makes the intermediate products overflow a signed 32-bit accumulator on the extreme
;;;; inputs of the IEEE 1180 test, and every implementation of this transform in wide use shaves
;;;; that one.  It is the same choice ffmpeg's `simple' IDCT makes, which is what lets this be
;;;; compared against ffmpeg exactly rather than approximately.
;;;;
;;;; The two shifts are not free parameters either.  Eleven after the rows and twenty after the
;;;; columns put the rounding in the same places the fixed-point scaling requires, and moving either
;;;; changes the answer in the last bit.

(in-package #:reel.mpeg2)

(defconstant +w1+ 22725)   ; cos(1*pi/16) * sqrt(2) << 14
(defconstant +w2+ 21407)
(defconstant +w3+ 19266)
(defconstant +w4+ 16383)   ; 16384 - 1, see above
(defconstant +w5+ 12873)
(defconstant +w6+ 8867)
(defconstant +w7+ 4520)

(defconstant +row-shift+ 11)
(defconstant +col-shift+ 20)

(declaim (inline %s16))
(defun %s16 (v)
  "V as a signed sixteen-bit value.

   The row pass writes its results back into the coefficient block, and that block is sixteen bits
   wide.  On any real picture nothing comes close to overflowing it; this exists so that the
   arithmetic is DEFINED rather than merely usually-in-range, and so that it agrees with a reference
   decoder on the pathological blocks a fuzzer finds."
  (declare (type fixnum v) (optimize (speed 3) (safety 0)))
  (let ((m (logand v #xffff)))
    (declare (type (unsigned-byte 16) m))
    (if (logbitp 15 m) (- m 65536) m)))

(defmacro %idct-pass ((block i0 step) &body writeback)
  "One eight-point inverse transform over BLOCK at I0 with stride STEP.

   Even and odd halves computed apart, then butterflied.  The even half depends only on the even
   inputs and the odd half only on the odd ones, which is the trick every fast DCT is built on:
   half the multiplies serve four outputs each.

   ROUND is bound by the caller because the two passes do not round the same way — see IDCT-8X8."
  (let ((d (loop for k below 8 collect (gensym "D"))))
    `(let* ,(append
             (loop for k below 8 for g in d
                   collect `(,g (aref ,block (+ ,i0 (* ,k ,step)))))
             `((a0 (+ (* +w4+ ,(nth 0 d)) round (* +w2+ ,(nth 2 d)) (* +w4+ ,(nth 4 d)) (* +w6+ ,(nth 6 d))))
               (a1 (+ (* +w4+ ,(nth 0 d)) round (* +w6+ ,(nth 2 d)) (- (* +w4+ ,(nth 4 d))) (- (* +w2+ ,(nth 6 d)))))
               (a2 (+ (* +w4+ ,(nth 0 d)) round (- (* +w6+ ,(nth 2 d))) (- (* +w4+ ,(nth 4 d))) (* +w2+ ,(nth 6 d))))
               (a3 (+ (* +w4+ ,(nth 0 d)) round (- (* +w2+ ,(nth 2 d))) (* +w4+ ,(nth 4 d)) (- (* +w6+ ,(nth 6 d)))))
               (b0 (+ (* +w1+ ,(nth 1 d)) (* +w3+ ,(nth 3 d)) (* +w5+ ,(nth 5 d)) (* +w7+ ,(nth 7 d))))
               (b1 (+ (* +w3+ ,(nth 1 d)) (- (* +w7+ ,(nth 3 d))) (- (* +w1+ ,(nth 5 d))) (- (* +w5+ ,(nth 7 d)))))
               (b2 (+ (* +w5+ ,(nth 1 d)) (- (* +w1+ ,(nth 3 d))) (* +w7+ ,(nth 5 d)) (* +w3+ ,(nth 7 d))))
               (b3 (+ (* +w7+ ,(nth 1 d)) (- (* +w5+ ,(nth 3 d))) (* +w3+ ,(nth 5 d)) (- (* +w1+ ,(nth 7 d)))))
               (o0 (ash (+ a0 b0) (- shift))) (o7 (ash (- a0 b0) (- shift)))
               (o1 (ash (+ a1 b1) (- shift))) (o6 (ash (- a1 b1) (- shift)))
               (o2 (ash (+ a2 b2) (- shift))) (o5 (ash (- a2 b2) (- shift)))
               (o3 (ash (+ a3 b3) (- shift))) (o4 (ash (- a3 b3) (- shift)))))
       (declare (type fixnum ,@d a0 a1 a2 a3 b0 b1 b2 b3 o0 o1 o2 o3 o4 o5 o6 o7)
                (ignorable o0 o1 o2 o3 o4 o5 o6 o7))
       ,@writeback)))

(defun idct-8x8 (block)
  "In-place inverse transform of a dequantised 8x8 block in raster order.

   TWO ROUNDINGS, AND NEITHER IS THE OBVIOUS ONE.

   The rows round by half of their shift, as anyone would write.  The COLUMNS do not: the rounding
   term is folded into the first multiply as W4 * ((1 << 19) / W4), and because W4 is 16383 rather
   than 16384 that integer division loses a little — the constant added is 524256 and not 524288.
   That is a thirty-two out of half a million and it changes the result in the last bit often
   enough to see.  It is not a mistake to be corrected: it is what the transform this is compared
   against does, and matching it is the difference between agreeing exactly and agreeing nearly.

   The DC-ONLY row is a genuine special case rather than an optimisation.  A row whose only nonzero
   coefficient is the first is computed as a plain multiply by eight, and (x * 16383 + 1024) >> 11
   is not always x * 8 — at x = 2048 they are 16383 and 16384.  Skipping the shortcut is not merely
   slower, it disagrees."
  (declare (type (simple-array (signed-byte 32) (64)) block) (optimize (speed 3) (safety 0)))
  (dotimes (r 8)
    (let ((i0 (* r 8)))
      (declare (type fixnum i0))
      (if (and (zerop (aref block (+ i0 1))) (zerop (aref block (+ i0 2)))
               (zerop (aref block (+ i0 3))) (zerop (aref block (+ i0 4)))
               (zerop (aref block (+ i0 5))) (zerop (aref block (+ i0 6)))
               (zerop (aref block (+ i0 7))))
          (let ((v (%s16 (* 8 (aref block i0)))))
            (dotimes (k 8) (setf (aref block (+ i0 k)) v)))
          (let ((round (ash 1 (1- +row-shift+))) (shift +row-shift+))
            (declare (type fixnum round shift))
            (%idct-pass (block i0 1)
              (setf (aref block (+ i0 0)) (%s16 o0) (aref block (+ i0 1)) (%s16 o1)
                    (aref block (+ i0 2)) (%s16 o2) (aref block (+ i0 3)) (%s16 o3)
                    (aref block (+ i0 4)) (%s16 o4) (aref block (+ i0 5)) (%s16 o5)
                    (aref block (+ i0 6)) (%s16 o6) (aref block (+ i0 7)) (%s16 o7)))))))
  (let ((round (* +w4+ (floor (ash 1 (1- +col-shift+)) +w4+))) (shift +col-shift+))
    (declare (type fixnum round shift))
    (dotimes (c 8)
      (%idct-pass (block c 8)
        (setf (aref block (+ c 0)) o0 (aref block (+ c 8)) o1
              (aref block (+ c 16)) o2 (aref block (+ c 24)) o3
              (aref block (+ c 32)) o4 (aref block (+ c 40)) o5
              (aref block (+ c 48)) o6 (aref block (+ c 56)) o7))))
  block)

(declaim (inline clamp255))
(defun clamp255 (v)
  (declare (type fixnum v) (optimize (speed 3) (safety 0)))
  (cond ((< v 0) 0) ((> v 255) 255) (t v)))

(defun idct-put (plane stride base block)
  "Transform BLOCK and write it as samples: an INTRA block carries the picture itself, not a
   correction to one, so nothing is added to it."
  (declare (type octets plane) (type (simple-array (signed-byte 32) (64)) block)
           (type fixnum stride base) (optimize (speed 3) (safety 0)))
  (idct-8x8 block)
  (dotimes (r 8)
    (let ((o (+ base (* r stride))) (k (* r 8)))
      (declare (type fixnum o k))
      (dotimes (c 8) (setf (aref plane (+ o c)) (clamp255 (aref block (+ k c))))))))

(defun idct-add (plane stride base block)
  "Transform BLOCK and add it to the prediction already sitting in PLANE."
  (declare (type octets plane) (type (simple-array (signed-byte 32) (64)) block)
           (type fixnum stride base) (optimize (speed 3) (safety 0)))
  (idct-8x8 block)
  (dotimes (r 8)
    (let ((o (+ base (* r stride))) (k (* r 8)))
      (declare (type fixnum o k))
      (dotimes (c 8)
        (setf (aref plane (+ o c))
              (clamp255 (+ (aref plane (+ o c)) (aref block (+ k c)))))))))

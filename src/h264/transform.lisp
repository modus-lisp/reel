;;;; h264/transform.lisp — dequantisation and the inverse transforms (8.5).
;;;;
;;;; H.264's 4x4 transform is an INTEGER approximation of the DCT chosen so that the inverse is
;;;; exact in 16-bit arithmetic: no multiplies, only adds and shifts by one.  That is the whole
;;;; reason it exists, and it is why an H.264 decoder cannot drift from the encoder the way an
;;;; early MPEG decoder could — there is no rounding to disagree about.
;;;;
;;;; The price is that the transform is NOT ORTHONORMAL: its basis functions have different norms,
;;;; so the quantiser has to undo the difference, and dequantisation is therefore POSITION
;;;; DEPENDENT.  The four corners of a 4x4, its centre four, and the remaining eight each scale
;;;; differently (+DEQUANT-CLASS+ says which).  A decoder written from a JPEG intuition — one
;;;; quantiser value per coefficient position, orthonormal transform — produces a picture that is
;;;; recognisably right and wrong everywhere, which is a miserable bug to chase.
;;;;
;;;; QP IS LOGARITHMIC: every 6 steps doubles the step size, so the scale table has six rows and
;;;; the exponent is (qp / 6).  That split — a table lookup on the remainder, a shift on the
;;;; quotient — is what every formula in this file is doing.

(in-package #:reel.h264)

(declaim (inline clamp255))
(defun clamp255 (v) 
  (declare (optimize (speed 3) (safety 1)))(declare (type fixnum v)) (max 0 (min 255 v)))

;;; ---- dequantisation ---------------------------------------------------------------------------

(defconstant +flat-weight-scale+ 16
  "LevelScale = weightScale * normAdjust (8.5.12.1), and with no scaling matrix present every
weightScale entry is 16.  Leaving it out is the single easiest way to build an H.264 decoder that
parses every bit correctly and produces a picture 16 times too flat — the residual is there, it is
just far too small to matter, so the output looks like prediction alone.")

(declaim (inline idct-4x4-dc))
(defun idct-4x4-dc (block d)
  "The inverse transform of a block whose only coefficient is the DC one.

   Both butterfly passes turn a lone d into d everywhere, so the whole transform is a fill.  This
   is not a rare case: at any ordinary quantiser most coded blocks carry only low-frequency energy,
   and a large share carry exactly one coefficient."
  (declare (type (simple-array (signed-byte 32) (16)) block) (type fixnum d)
           (optimize (speed 3) (safety 0)))
  (fill block d)
  block)

(defun dequant-4x4 (coeffs out qp &key (start 0) (end 15) (weights +flat-scale-4x4+))
  "Dequantise a 4x4 block.  COEFFS is in SCAN order, OUT is filled in RASTER order.

   START is 1 for the AC-only block of an Intra16x16 macroblock, whose DC came from the separate
   Hadamard-coded block; OUT position 0 is then left for the caller to fill in.

   END is the highest scan position that actually carries a coefficient, which RESIDUAL-BLOCK
   returns as its second value.  Walking to it rather than to 15 is most of what this costs: a
   block with two coefficients has fourteen positions nobody need look at.  OUT is zeroed first,
   so positions past END — and zeros inside the range — need no work at all."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (simple-array (signed-byte 32) (*)) coeffs out) (type fixnum qp start end)
           (type (simple-array (unsigned-byte 8) (16)) weights))
  (fill out 0)
  ;; END is -1 when the block carried nothing, and below START when everything it carried was the
  ;; DC the caller supplies separately.  Either way the zeroed OUT is already the answer.
  (when (< end start) (return-from dequant-4x4 out))
  (let ((m (mod qp 6)) (e (floor qp 6)))
    (declare (type (integer 0 5) m) (type (integer 0 8) e))
    (loop for scan of-type (integer 0 16) from start to (the (integer 0 15) end)
          ;; NARROW, not merely FIXNUM.  A fixnum times a fixnum may be a bignum as far as the
          ;; compiler knows, so the obvious declaration still calls generic multiply and generic
          ;; shift.  A coefficient is bounded by the level code and the scale by table 8-15, and
          ;; saying so is what turns this loop into machine arithmetic.
          for c of-type (signed-byte 26) = (aref coeffs scan)
          do (let* ((raster (aref +zigzag-4x4+ scan))
                    ;; the transmitted weight for this POSITION, which is 16 everywhere
                    ;; unless the stream sent a scaling list
                    (scale (the (integer 0 8192)
                                (* (aref weights raster)
                                   (aref +dequant-coeff+ m (aref +dequant-class+ raster))))))
               (declare (type (integer 0 15) raster))
               (unless (zerop c)
                 (setf (aref out raster)
                       (if (>= e 4)
                           (ash (* c scale) (- e 4))
                           (ash (+ (* c scale) (ash 1 (- 3 e))) (- (- 4 e))))))))
    out))

;;; ---- the inverse 4x4 transform (8.5.12.2) ------------------------------------------------------

(defun idct-4x4 (block)
  "In-place inverse transform of a dequantised 4x4 BLOCK, raster order.  Leaves the residual
   scaled by 64; the caller rounds with (+ 32) >> 6 when it adds to the prediction."
  (declare (type (simple-array (signed-byte 32) (16)) block) (optimize (speed 3) (safety 0)))
  ;; rows
  (dotimes (i 4)
    (let* ((o (* i 4))
           (d0 (aref block o)) (d1 (aref block (+ o 1)))
           (d2 (aref block (+ o 2))) (d3 (aref block (+ o 3)))
           (e0 (+ d0 d2)) (e1 (- d0 d2))
           (e2 (- (ash d1 -1) d3)) (e3 (+ d1 (ash d3 -1))))
      (declare (type fixnum d0 d1 d2 d3 e0 e1 e2 e3))
      (setf (aref block o) (+ e0 e3)
            (aref block (+ o 1)) (+ e1 e2)
            (aref block (+ o 2)) (- e1 e2)
            (aref block (+ o 3)) (- e0 e3))))
  ;; columns
  (dotimes (j 4)
    (let* ((d0 (aref block j)) (d1 (aref block (+ j 4)))
           (d2 (aref block (+ j 8))) (d3 (aref block (+ j 12)))
           (e0 (+ d0 d2)) (e1 (- d0 d2))
           (e2 (- (ash d1 -1) d3)) (e3 (+ d1 (ash d3 -1))))
      (declare (type fixnum d0 d1 d2 d3 e0 e1 e2 e3))
      (setf (aref block j) (+ e0 e3)
            (aref block (+ j 4)) (+ e1 e2)
            (aref block (+ j 8)) (- e1 e2)
            (aref block (+ j 12)) (- e0 e3))))
  block)

;;; ---- the DC transforms -------------------------------------------------------------------------

(defun luma-dc-transform (dc qp &optional (w0 16))
  "The 4x4 Hadamard over an Intra16x16 macroblock's sixteen DC coefficients (8.5.10).

   An Intra16x16 macroblock codes the DC of its sixteen 4x4 blocks as a block of its own,
   transformed again — a second-order transform, exactly as VP8's Y2 block is.  Both formats
   reached for it for the same reason: on flat content the DCs are the only thing left and they
   correlate strongly with each other."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (simple-array (signed-byte 32) (16)) dc) (type fixnum qp))
  ;; Rows, then columns, of the un-normalised Hadamard (8.5.10).  The matrix is
  ;;     1  1  1  1 / 1  1 -1 -1 / 1 -1 -1  1 / 1 -1  1 -1
  ;; so with s0=a+b, s1=c+d, s2=a-b, s3=c-d the outputs are s0+s1, s0-s1, s2-s3, s2+s3 — in THAT
  ;; order.  Emitting them as s0+s1, s2+s3, s2-s3, s0-s1 (which is what falling into the VP8
  ;; Walsh-Hadamard's ordering gives you) swaps outputs 1 and 3, and is invisible whenever only
  ;; the DC coefficient is non-zero — every output is then equal — so it survives the flat
  ;; macroblocks and corrupts the detailed ones.
  (dotimes (i 4)
    (let* ((o (* i 4))
           (a (aref dc o)) (b (aref dc (+ o 1))) (c (aref dc (+ o 2))) (d (aref dc (+ o 3)))
           (s0 (+ a b)) (s1 (+ c d)) (s2 (- a b)) (s3 (- c d)))
      (declare (type fixnum a b c d s0 s1 s2 s3))
      (setf (aref dc o) (+ s0 s1) (aref dc (+ o 1)) (- s0 s1)
            (aref dc (+ o 2)) (- s2 s3) (aref dc (+ o 3)) (+ s2 s3))))
  (dotimes (j 4)
    (let* ((a (aref dc j)) (b (aref dc (+ j 4))) (c (aref dc (+ j 8))) (d (aref dc (+ j 12)))
           (s0 (+ a b)) (s1 (+ c d)) (s2 (- a b)) (s3 (- c d)))
      (declare (type fixnum a b c d s0 s1 s2 s3))
      (setf (aref dc j) (+ s0 s1) (aref dc (+ j 4)) (- s0 s1)
            (aref dc (+ j 8)) (- s2 s3) (aref dc (+ j 12)) (+ s2 s3))))
  ;; then scale, with the same qp split as everywhere else
  (let* ((m (mod qp 6)) (e (floor qp 6))
         (scale (* w0 (aref +dequant-coeff+ m 0))))
    (declare (type fixnum m e scale))
    (dotimes (i 16 dc)
      (setf (aref dc i)
            (if (>= e 6)
                (ash (* (aref dc i) scale) (- e 6))
                (ash (+ (* (aref dc i) scale) (ash 1 (- 5 e))) (- (- 6 e))))))))

(defun chroma-dc-transform (dc qp &optional (w0 16))
  "The 2x2 Hadamard over a chroma component's four DC coefficients (8.5.11).  DC is four elements."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (simple-array (signed-byte 32) (*)) dc) (type fixnum qp))
  (let* ((a (aref dc 0)) (b (aref dc 1)) (c (aref dc 2)) (d (aref dc 3))
         (e0 (+ a b)) (e1 (- a b)) (e2 (+ c d)) (e3 (- c d))
         (m (mod qp 6)) (e (floor qp 6))
         (scale (* w0 (aref +dequant-coeff+ m 0))))
    (declare (type fixnum a b c d e0 e1 e2 e3 m e scale))
    (setf (aref dc 0) (+ e0 e2) (aref dc 1) (+ e1 e3)
          (aref dc 2) (- e0 e2) (aref dc 3) (- e1 e3))
    (dotimes (i 4 dc)
      (setf (aref dc i) (ash (ash (* (aref dc i) scale) e) -5)))))

;;; ---- adding a residual to a prediction ----------------------------------------------------------

(defun add-residual-4x4 (plane stride base block)
  "Add a transformed 4x4 residual to the prediction already in PLANE at BASE, rounding by the
   transform's own scale factor of 64 and clamping."
  (declare (type (simple-array (unsigned-byte 8) (*)) plane)
           (type (simple-array (signed-byte 32) (16)) block)
           (type fixnum stride base) (optimize (speed 3) (safety 0)))
  (dotimes (i 4)
    (let ((row (+ base (* i stride))))
      (declare (type fixnum row))
      (dotimes (j 4)
        (setf (aref plane (+ row j))
              (clamp255 (+ (aref plane (+ row j))
                           (ash (+ (aref block (+ (* i 4) j)) 32) -6))))))))

;;; ---- the chroma quantiser -----------------------------------------------------------------------

(defun chroma-qp (qpy offset)
  "The chroma quantiser for a luma QP and the PPS/slice offset (Table 8-15)."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type fixnum qpy offset))
  (let ((qpi (max 0 (min 51 (+ qpy offset)))))
    (aref +qpc-from-qpy+ qpi)))

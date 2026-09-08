;;;; vp9/recon.lisp — gathering an intra block's edge samples, and reconstructing it.
;;;;
;;;; THE EDGE SAMPLES ARE THE WHOLE OF THE DIFFICULTY.  A transform block's prediction reads the row
;;;; above it, the column to its left, the corner between them, and — at 4x4 only — four samples
;;;; above and to the right.  Any of those may be outside the picture, outside the tile, or simply
;;;; not decoded yet, and the rules for each case are different: a missing row is a flat 127, a
;;;; missing column a flat 129, a missing corner 127 or 129 depending on which side survives, and a
;;;; row that runs off the right of the picture repeats its last real sample.
;;;;
;;;; And there is one rule that is not about availability at all.  A block in the FIRST row of a
;;;; superblock row reads the row above it from a saved copy taken BEFORE the loop filter ran, not
;;;; from the picture.  Intra prediction is defined on unfiltered samples; the filter has already
;;;; been over that row by the time this block is decoded, so the decoder keeps the old values for
;;;; exactly one row.  A decoder that reads the picture instead is correct until the first frame
;;;; whose filter level is not zero, and then it is wrong everywhere, faintly.

(in-package #:reel.vp9)

;;; ---- the edge samples ----------------------------------------------------------------------------

(defun %gather-edges (st plane mode tx col x w row y ss-h ss-v a l)
  "Fill A (the row above, with the corner at index 31) and L (the column to the left, reversed) for
   one transform block, and return the mode that is actually usable given what is there.

   Returns the substituted mode: a vertical prediction with no row above is not an error, it is a
   different mode."
  (declare (type state st) (type octets a l)
           (type fixnum plane mode tx col x w row y)
           (optimize (speed 3) (safety 1)))
  (let* ((f (st-frame st))
         (dst (the octets (aref (fr-planes f) plane)))
         (stride (aref (fr-stride f) plane))
         (base (st-block-base st plane))         ; the block's first sample
         (have-top (or (plusp row) (plusp y)))
         (have-left (or (> col (st-tile-col-start st)) (plusp x)))
         (have-right (< x (1- w)))
         (need (ash 4 tx))
         (m (aref +mode-conv+ mode (if have-left 1 0) (if have-top 1 0)))
         (needs (aref +mode-needs+ m)))
    (declare (type fixnum stride base need m needs))
    ;; the sub-block's own origin inside the block
    (let ((o (+ base (* 4 y stride) (* 4 x))))
      (declare (type fixnum o))
      (when (logbitp 1 needs)                    ; needs the row above
        (let ((have (* 4 (- (ash (- (st-cols st) col) (if (plusp ss-h) 0 1)) x)))
              (need-tr (if (and (zerop tx) (logbitp 3 needs) have-right) 4 0))
              (top 0) (topsrc dst))
          (declare (type fixnum have need-tr top) (type octets topsrc))
          (if have-top
              (if (and (zerop (logand row 7)) (zerop y))
                  ;; the saved pre-filter row, not the picture
                  (setf topsrc (the octets (aref (st-intra-row st) plane))
                        top (+ (* col (ash 8 (- (if (plusp ss-h) 1 0)))) (* 4 x)))
                  (setf topsrc dst top (- o stride)))
              (setf top 0))
          (if have-top
              (progn
                (dotimes (i (min need have)) (setf (aref a (+ 32 i)) (aref topsrc (+ top i))))
                (when (> need have)
                  (let ((v (aref a (+ 32 (1- have)))))
                    (loop for i of-type fixnum from have below need
                          do (setf (aref a (+ 32 i)) v)))))
              (dotimes (i need) (setf (aref a (+ 32 i)) 127)))
          (when (logbitp 2 needs)                ; needs the corner
            (setf (aref a 31)
                  (if (and have-left have-top)
                      ;; the corner comes from the same row the top does, one sample earlier
                      (if (and (zerop (logand row 7)) (zerop y))
                          (aref topsrc (max 0 (1- top)))
                          (aref dst (1- (- o stride))))
                      (if have-top 129 127))))
          (when (and (zerop tx) (logbitp 3 needs))
            (if (and have-top have-right (<= (+ need need-tr) have))
                (dotimes (i 4) (setf (aref a (+ 32 4 i)) (aref topsrc (+ top 4 i))))
                (let ((v (aref a (+ 32 3))))
                  (dotimes (i 4) (setf (aref a (+ 32 4 i)) v)))))))
      (when (logbitp 0 needs)                    ; needs the column to the left
        (if have-left
            (let ((have (* 4 (- (ash (- (st-rows st) row) (if (plusp ss-v) 0 1)) y))))
              (declare (type fixnum have))
              (if (logbitp 4 needs)
                  ;; HOR_UP alone wants them top to bottom
                  (progn
                    (dotimes (i (min need have))
                      (setf (aref l i) (aref dst (+ o (* i stride) -1))))
                    (when (> need have)
                      (let ((v (aref l (1- have))))
                        (loop for i of-type fixnum from have below need
                              do (setf (aref l i) v)))))
                  (progn
                    (dotimes (i (min need have))
                      (setf (aref l (- need 1 i)) (aref dst (+ o (* i stride) -1))))
                    (when (> need have)
                      (let ((v (aref l (- need have))))
                        (dotimes (i (- need have)) (setf (aref l i) v)))))))
            (dotimes (i need) (setf (aref l i) 129)))))
    m))

;;; ---- reconstructing one block --------------------------------------------------------------------

(defun %intra-recon (st)
  "Predict and transform every transform block of one intra block, luma then the two chroma planes."
  (let* ((f (st-frame st)) (bs (st-bs st))
         (row (st-row st)) (col (st-col st))
         (tx (st-tx st)) (uvtx (st-uvtx st))
         (lossless (h-lossless (st-h st)))
         (w4 (ash (%bw4 bs) 1)) (h4 (ash (%bh4 bs) 1))
         (end-x (min (* 2 (- (st-cols st) col)) w4))
         (end-y (min (* 2 (- (st-rows st) row)) h4))
         (a (st-edge-a st)) (l (st-edge-l st)))
    (declare (type fixnum bs row col tx uvtx w4 h4 end-x end-y))
    ;; ---- luma
    (let ((step (ash 1 tx)) (sz (ash 4 tx))
          (dst (the octets (aref (fr-planes f) 0))) (stride (aref (fr-stride f) 0))
          (base (st-block-base st 0)) (n 0))
      (declare (type fixnum step sz stride base n))
      (loop for y of-type fixnum from 0 below end-y by step
            do (loop for x of-type fixnum from 0 below end-x by step
                     do (let* ((mi (if (and (> bs +bs-8x8+) (zerop tx)) (+ (* 2 y) x) 0))
                               (mode (aref (st-mode st) (min mi 3)))
                               (txtp (if lossless 0 (aref +intra-txfm-type+ mode)))
                               (eob (if (st-skip st) 0 (aref (st-eob st) n)))
                               (m (%gather-edges st 0 mode tx col x w4 row y 0 0 a l))
                               (o (+ base (* 4 y stride) (* 4 x))))
                          (declare (type fixnum mi mode txtp eob m o))
                          (%pred-intra m dst stride o sz l a)
                          (when (plusp eob)
                            (%itxfm-add dst stride o (st-coeffs st) sz tx txtp eob lossless
                                        (* 16 n))))
                        (incf n (* step step)))))
    ;; ---- and the chroma planes, at half the resolution
    (let* ((cw4 (ash w4 -1)) (cex (ash end-x -1)) (ceh (ash end-y -1))
           (step (ash 1 uvtx)) (sz (ash 4 uvtx)))
      (declare (type fixnum cw4 cex ceh step sz))
      (dotimes (pl 2)
        (let ((dst (the octets (aref (fr-planes f) (1+ pl))))
              (stride (aref (fr-stride f) (1+ pl)))
              (base (st-block-base st (1+ pl)))
              (coeffs (the (simple-array fixnum (*)) (aref (st-uvcoeffs st) pl)))
              (eobs (the (simple-array fixnum (*)) (aref (st-uveob st) pl)))
              (n 0))
          (declare (type fixnum stride base n))
          (loop for y of-type fixnum from 0 below ceh by step
                do (loop for x of-type fixnum from 0 below cex by step
                         do (let* ((eob (if (st-skip st) 0 (aref eobs n)))
                                   (m (%gather-edges st (1+ pl) (st-uvmode st) uvtx
                                                     col x cw4 row y 1 1 a l))
                                   (o (+ base (* 4 y stride) (* 4 x))))
                              (declare (type fixnum eob m o))
                              (%pred-intra m dst stride o sz l a)
                              (when (plusp eob)
                                (%itxfm-add dst stride o coeffs sz uvtx 0 eob lossless (* 16 n))))
                            (incf n (* step step)))))))
    (values)))
(defun %filter-superblock (st sb col row)
  "One superblock: all the vertical edges of each plane, then all the horizontal ones.

   Columns before rows, and never interleaved: a sample on a corner is filtered twice, and the
   horizontal pass must see what the vertical pass left."
  (declare (type state st) (type fixnum sb col row))
  (let* ((f (st-frame st)) (mask (st-lf-mask st)) (level (st-lf-level st))
         (lim (st-lf-lim st)) (mblim (st-lf-mblim st)))
    (let ((stride (aref (fr-stride f) 0))
          (dst (the octets (aref (fr-planes f) 0))))
      (let ((base (+ (* 8 row stride) (* 8 col))))
        (%filter-cols mask sb 0 level col 0 0 dst stride base lim mblim)
        (%filter-rows mask sb 0 level row 0 0 dst stride base lim mblim)))
    (dotimes (p 2)
      (let ((stride (aref (fr-stride f) (1+ p)))
            (dst (the octets (aref (fr-planes f) (1+ p)))))
        (let ((base (+ (* 4 row stride) (* 4 col))))
          (%filter-cols mask sb 1 level col 1 1 dst stride base lim mblim)
          (%filter-rows mask sb 1 level row 1 1 dst stride base lim mblim)))))
  (values))

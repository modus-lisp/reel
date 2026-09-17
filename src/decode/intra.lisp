;;;; decode/intra.lisp — the VP8 key-frame pixel pipeline (RFC 6386 s9-14): coefficient tokens,
;;;; dequantization, intra prediction, reconstruction.
;;;;
;;;; This is webp-pure's decoder, moved.  A lossy WebP is one VP8 key frame, so this code was
;;;; always the codec rather than the image format; what stayed behind in webp-pure is the RIFF
;;;; container, the VP8L lossless path, and the YUV->RGB conversion an image wants at the end.
;;;; Inter frames — everything a still picture never needed — are in decode/inter.lisp.

(in-package #:reel.decode)


;;; ---- planes -----------------------------------------------------------
;;; A plane is a fixnum raster with a one-pixel top border (row -1, value 127)
;;; and left border (column -1, value 129, corner 127), plus a small right pad
;;; used by the B_PRED "above-right" predictors.  Logical pixel (x,y) with
;;; 0 <= x < w, 0 <= y < h maps to array index (y+1)*stride + (x+1).

(defstruct (plane (:conc-name pl-))
  (data nil :type u8vec)
  (stride 0 :type dim)
  (w 0 :type dim)
  (h 0 :type dim))

(declaim (inline pidx))
(defun pidx (pl x y)
  (declare (type plane pl) (type (signed-byte 26) x y))
  (the fixnum (+ (the fixnum (* (the fixnum (+ y 1)) (pl-stride pl))) (+ x 1))))

(defmacro pget (pl x y) `(aref (pl-data ,pl) (pidx ,pl ,x ,y)))
(defmacro pset (pl x y v) `(setf (aref (pl-data ,pl) (pidx ,pl ,x ,y)) ,v))

(defun make-plane* (w h rpad)
  "Allocate a plane WxH with a 1-pixel top/left border and RPAD right columns."
  (let* ((stride (+ w 1 rpad))
         (data (make-array (* stride (+ h 1)) :element-type '(unsigned-byte 8)
                                              :initial-element 0))
         (pl (make-plane :data data :stride stride :w w :h h)))
    ;; top border row (y = -1): 127 across the whole stride
    (dotimes (i stride) (setf (aref data i) 127))
    ;; left border column (x = -1) for y >= 0: 129
    (loop for y from 0 below h do (setf (aref data (* (+ y 1) stride)) 129))
    pl))

(defun pad-right (pl y0 n)
  "Replicate the rightmost visible pixel into the right pad for N rows from Y0."
  (declare (type plane pl) (type fixnum y0 n))
  (let* ((w (pl-w pl)) (stride (pl-stride pl)) (data (pl-data pl)))
    (loop for y from y0 below (+ y0 n) do
      (let ((base (+ (* (+ y 1) stride) 1))
            (v (pget pl (- w 1) y)))
        (loop for x from w below (- stride 1) do
          (setf (aref data (+ base x)) v))))))

;;; ---- tree read --------------------------------------------------------

(declaim (inline treed-read))
(defun treed-read (bd tree probs prob-off)
  (declare (type (simple-array (signed-byte 32) (*)) tree probs) (type fixnum prob-off))
  (let ((i 0))
    (declare (type fixnum i))
    (loop
      (setf i (aref tree (+ i (bool-bit bd (aref probs (+ prob-off (ash i -1)))))))
      (when (<= i 0) (return (- i))))))

;;; ---- coefficient token decode (RFC 6386 §13) --------------------------

(declaim (type (simple-array t (4)) +cat3456+))
(defparameter +cat3456+
  (vector (make-array 3 :element-type '(signed-byte 32) :initial-contents '(173 148 140))
          (make-array 4 :element-type '(signed-byte 32) :initial-contents '(176 155 140 135))
          (make-array 5 :element-type '(signed-byte 32) :initial-contents '(180 157 141 134 130))
          (make-array 11 :element-type '(signed-byte 32)
            :initial-contents '(254 254 243 230 196 177 153 140 133 130 129))))

(declaim (inline cprob-base))
(defun cprob-base (plane band ctx)
  (declare (type fixnum plane band ctx))
  (the fixnum (* (+ (* (+ (* plane 8) band) 3) ctx) 11)))

(defun get-large-value (bd cp base)
  "Decode the magnitude of a dct_cat token given prob base BASE (RFC §13.2)."
  (declare (type (simple-array (signed-byte 32) (*)) cp) (type fixnum base))
  (if (zerop (bool-bit bd (aref cp (+ base 3))))
      (if (zerop (bool-bit bd (aref cp (+ base 4))))
          2
          (+ 3 (bool-bit bd (aref cp (+ base 5)))))
      (if (zerop (bool-bit bd (aref cp (+ base 6))))
          (if (zerop (bool-bit bd (aref cp (+ base 7))))
              (+ 5 (bool-bit bd 159))
              (+ 7 (* 2 (bool-bit bd 165)) (bool-bit bd 145)))
          (let* ((bit1 (bool-bit bd (aref cp (+ base 8))))
                 (bit0 (bool-bit bd (aref cp (+ base (+ 9 bit1)))))
                 (cat (+ (* 2 bit1) bit0))
                 (tab (aref +cat3456+ cat))
                 (v 0))
            (declare (type (simple-array (signed-byte 32) (*)) tab) (type fixnum v))
            (dotimes (k (length tab))
              (setf v (+ v v (bool-bit bd (aref tab k)))))
            (+ v 3 (ash 8 cat))))))

(defun get-coeffs (bd cp plane out ctx dqdc dqac first)
  "Decode one 4x4 block's tokens into OUT (raster, pre-zeroed) applying the
   dequant factors DQDC/DQAC.  Returns T if any non-zero coefficient appeared."
  (declare (type (simple-array (signed-byte 32) (*)) cp out)
           (type fixnum plane ctx dqdc dqac first))
  (let ((n first) (saw nil) (saw-ac nil))
    (declare (type fixnum n))
    (loop
      (when (= n 16) (return))
      (let ((base (cprob-base plane (aref +coeff-bands+ n) ctx)))
        (when (zerop (bool-bit bd (aref cp base)))   ; end-of-block
          (return))
        ;; run of DCT_0 tokens (no EOB check follows a zero)
        (loop
          (if (zerop (bool-bit bd (aref cp (+ base 1))))
              (progn                                  ; DCT_0
                (incf n)
                (when (= n 16) (return-from get-coeffs (values saw saw-ac)))
                (setf base (cprob-base plane (aref +coeff-bands+ n) 0)))
              (return)))
        ;; a non-zero coefficient
        (let ((v (if (zerop (bool-bit bd (aref cp (+ base 2))))
                     1
                     (get-large-value bd cp base))))
          (declare (type fixnum v))
          (setf ctx (if (= v 1) 1 2))
          (when (= 1 (bool-bit bd 128)) (setf v (- v)))
          (setf (aref out (aref +coeff-scan+ n)) (* v (if (= n 0) dqdc dqac)))
          (setf saw t)
          (when (> n 0) (setf saw-ac t))
          (incf n))))
    (values saw saw-ac)))

;;; ---- intra prediction (RFC 6386 §12) ----------------------------------

(declaim (inline clamp255))
(defun clamp255 (v) (declare (type fixnum v)) (max 0 (min 255 v)))

(defun predict-block (pl mx my size mode has-above has-left out)
  "Fill OUT (size*size raster) with the DC/V/H/TM prediction for the block at
   plane pixel (MX,MY).  MODE: 0=DC 1=V 2=H 3=TM."
  (declare (type plane pl) (type (signed-byte 26) mx my) (type fixnum size mode) (type u8vec out))
  (ecase mode
    (0 (let ((dc 0) (s 0))
         (declare (type fixnum dc s))
         (cond
           ((and has-above has-left)
            (dotimes (i size) (incf s (pget pl (+ mx i) (- my 1))))
            (dotimes (i size) (incf s (pget pl (- mx 1) (+ my i))))
            (setf dc (ash (+ s size) (if (= size 16) -5 -4))))
           (has-above
            (dotimes (i size) (incf s (pget pl (+ mx i) (- my 1))))
            (setf dc (ash (+ s (ash size -1)) (if (= size 16) -4 -3))))
           (has-left
            (dotimes (i size) (incf s (pget pl (- mx 1) (+ my i))))
            (setf dc (ash (+ s (ash size -1)) (if (= size 16) -4 -3))))
           (t (setf dc 128)))
         (dotimes (i (* size size)) (setf (aref out i) dc))))
    (1 (dotimes (r size)
         (dotimes (c size)
           (setf (aref out (+ (* r size) c)) (pget pl (+ mx c) (- my 1))))))
    (2 (dotimes (r size)
         (dotimes (c size)
           (setf (aref out (+ (* r size) c)) (pget pl (- mx 1) (+ my r))))))
    (3 (let ((p (pget pl (- mx 1) (- my 1))))
         (dotimes (r size)
           (let ((lv (pget pl (- mx 1) (+ my r))))
             (dotimes (c size)
               (setf (aref out (+ (* r size) c))
                     (clamp255 (+ lv (pget pl (+ mx c) (- my 1)) (- p)))))))))))

;;; --- 4x4 subblock prediction (B_PRED), RFC 6386 §12.3 ---

(declaim (inline avg2 avg3))
(defun avg2 (x y) (declare (type fixnum x y)) (ash (+ x y 1) -1))
(defun avg3 (x y z) (declare (type fixnum x y z)) (ash (+ x y y z 2) -2))

(defmacro bset (r c v) `(setf (aref out (+ (* ,r 4) ,c)) ,v))

(defun predict-subblock (mode a p l out)
  "B_PRED subblock prediction.  A[0..7] above row, P above-left, L[0..3] left.
   OUT is a 16-fixnum raster 4x4 buffer.  MODE per intra_bmode enumeration."
  (declare (type u8vec a l out) (type (unsigned-byte 8) p) (type fixnum mode))
  (macrolet ((a (i) `(aref a ,i)) (l (i) `(aref l ,i)))
    (let ((e0 (l 3)) (e1 (l 2)) (e2 (l 1)) (e3 (l 0)) (e4 p)
          (e5 (a 0)) (e6 (a 1)) (e7 (a 2)) (e8 (a 3)))
      (declare (type fixnum e0 e1 e2 e3 e4 e5 e6 e7 e8))
      (ecase mode
        (0                              ; B_DC_PRED
         (let ((v 4))
           (dotimes (i 4) (incf v (+ (a i) (l i))))
           (setf v (ash v -3))
           (dotimes (i 16) (setf (aref out i) v))))
        (1                              ; B_TM_PRED
         (dotimes (r 4)
           (dotimes (c 4)
             (bset r c (clamp255 (+ (l r) (a c) (- p)))))))
        (2                              ; B_VE_PRED
         (dotimes (c 4)
           (let ((v (avg3 (if (= c 0) p (a (1- c))) (a c) (a (1+ c)))))
             (bset 0 c v) (bset 1 c v) (bset 2 c v) (bset 3 c v))))
        (3                              ; B_HE_PRED
         (let ((v0 (avg3 p (l 0) (l 1)))
               (v1 (avg3 (l 0) (l 1) (l 2)))
               (v2 (avg3 (l 1) (l 2) (l 3)))
               (v3 (avg3 (l 2) (l 3) (l 3))))
           (dotimes (c 4) (bset 0 c v0) (bset 1 c v1) (bset 2 c v2) (bset 3 c v3))))
        (4                              ; B_LD_PRED
         (bset 0 0 (avg3 (a 0) (a 1) (a 2)))
         (let ((v (avg3 (a 1) (a 2) (a 3)))) (bset 0 1 v) (bset 1 0 v))
         (let ((v (avg3 (a 2) (a 3) (a 4)))) (bset 0 2 v) (bset 1 1 v) (bset 2 0 v))
         (let ((v (avg3 (a 3) (a 4) (a 5)))) (bset 0 3 v) (bset 1 2 v) (bset 2 1 v) (bset 3 0 v))
         (let ((v (avg3 (a 4) (a 5) (a 6)))) (bset 1 3 v) (bset 2 2 v) (bset 3 1 v))
         (let ((v (avg3 (a 5) (a 6) (a 7)))) (bset 2 3 v) (bset 3 2 v))
         (bset 3 3 (avg3 (a 6) (a 7) (a 7))))
        (5                              ; B_RD_PRED
         (bset 3 0 (avg3 e0 e1 e2))
         (let ((v (avg3 e1 e2 e3))) (bset 3 1 v) (bset 2 0 v))
         (let ((v (avg3 e2 e3 e4))) (bset 3 2 v) (bset 2 1 v) (bset 1 0 v))
         (let ((v (avg3 e3 e4 e5))) (bset 3 3 v) (bset 2 2 v) (bset 1 1 v) (bset 0 0 v))
         (let ((v (avg3 e4 e5 e6))) (bset 2 3 v) (bset 1 2 v) (bset 0 1 v))
         (let ((v (avg3 e5 e6 e7))) (bset 1 3 v) (bset 0 2 v))
         (bset 0 3 (avg3 e6 e7 e8)))
        (6                              ; B_VR_PRED
         (bset 3 0 (avg3 e1 e2 e3))
         (bset 2 0 (avg3 e2 e3 e4))
         (let ((v (avg3 e3 e4 e5))) (bset 3 1 v) (bset 1 0 v))
         (let ((v (avg2 e4 e5)))     (bset 2 1 v) (bset 0 0 v))
         (let ((v (avg3 e4 e5 e6))) (bset 3 2 v) (bset 1 1 v))
         (let ((v (avg2 e5 e6)))     (bset 2 2 v) (bset 0 1 v))
         (let ((v (avg3 e5 e6 e7))) (bset 3 3 v) (bset 1 2 v))
         (let ((v (avg2 e6 e7)))     (bset 2 3 v) (bset 0 2 v))
         (bset 1 3 (avg3 e6 e7 e8))
         (bset 0 3 (avg2 e7 e8)))
        (7                              ; B_VL_PRED
         (bset 0 0 (avg2 (a 0) (a 1)))
         (bset 1 0 (avg3 (a 0) (a 1) (a 2)))
         (let ((v (avg2 (a 1) (a 2)))) (bset 2 0 v) (bset 0 1 v))
         (let ((v (avg3 (a 1) (a 2) (a 3)))) (bset 1 1 v) (bset 3 0 v))
         (let ((v (avg2 (a 2) (a 3)))) (bset 2 1 v) (bset 0 2 v))
         (let ((v (avg3 (a 2) (a 3) (a 4)))) (bset 3 1 v) (bset 1 2 v))
         (let ((v (avg2 (a 3) (a 4)))) (bset 2 2 v) (bset 0 3 v))
         (let ((v (avg3 (a 3) (a 4) (a 5)))) (bset 3 2 v) (bset 1 3 v))
         (bset 2 3 (avg3 (a 4) (a 5) (a 6)))
         (bset 3 3 (avg3 (a 5) (a 6) (a 7))))
        (8                              ; B_HD_PRED
         (bset 3 0 (avg2 e0 e1))
         (bset 3 1 (avg3 e0 e1 e2))
         (let ((v (avg2 e1 e2)))     (bset 2 0 v) (bset 3 2 v))
         (let ((v (avg3 e1 e2 e3))) (bset 2 1 v) (bset 3 3 v))
         (let ((v (avg2 e2 e3)))     (bset 2 2 v) (bset 1 0 v))
         (let ((v (avg3 e2 e3 e4))) (bset 2 3 v) (bset 1 1 v))
         (let ((v (avg2 e3 e4)))     (bset 1 2 v) (bset 0 0 v))
         (let ((v (avg3 e3 e4 e5))) (bset 1 3 v) (bset 0 1 v))
         (bset 0 2 (avg3 e4 e5 e6))
         (bset 0 3 (avg3 e5 e6 e7)))
        (9                              ; B_HU_PRED
         (bset 0 0 (avg2 (l 0) (l 1)))
         (bset 0 1 (avg3 (l 0) (l 1) (l 2)))
         (let ((v (avg2 (l 1) (l 2)))) (bset 0 2 v) (bset 1 0 v))
         (let ((v (avg3 (l 1) (l 2) (l 3)))) (bset 0 3 v) (bset 1 1 v))
         (let ((v (avg2 (l 2) (l 3)))) (bset 1 2 v) (bset 2 0 v))
         (let ((v (avg3 (l 2) (l 3) (l 3)))) (bset 1 3 v) (bset 2 1 v))
         (let ((v (l 3)))
           (bset 2 2 v) (bset 2 3 v) (bset 3 0 v) (bset 3 1 v) (bset 3 2 v) (bset 3 3 v)))))))

;;; ---- dequantisation factors (RFC 6386 §14.1) --------------------------

(declaim (inline qclip))
(defun qclip (i lo hi) (declare (type fixnum i lo hi)) (max lo (min hi i)))

(defun segment-dequant (q ydc y2dc y2ac uvdc uvac)
  "Return the six dequant factors (y1dc y1ac y2dc y2ac uvdc uvac) for base
   quant index Q with the header deltas."
  (declare (type fixnum q ydc y2dc y2ac uvdc uvac))
  (let ((y2acv (truncate (* (aref +ac-qlookup+ (qclip (+ q y2ac) 0 127)) 155) 100)))
    (list (aref +dc-qlookup+ (qclip (+ q ydc) 0 127))
          (aref +ac-qlookup+ (qclip q 0 127))
          (* 2 (aref +dc-qlookup+ (qclip (+ q y2dc) 0 127)))
          (max 8 y2acv)
          (aref +dc-qlookup+ (qclip (+ q uvdc) 0 117))
          (aref +ac-qlookup+ (qclip (+ q uvac) 0 127)))))

;;; ---- decoder state ----------------------------------------------------

(defun %emptyfx () (make-array 0 :element-type '(signed-byte 32)))

(defstruct (dec (:conc-name d-))
  bytes
  (key-frame t)
  (mb-cols 0 :type fixnum) (mb-rows 0 :type fixnum)
  (width 0 :type fixnum) (height 0 :type fixnum)
  (yplane nil :type (or null plane))
  (uplane nil :type (or null plane))
  (vplane nil :type (or null plane))
  (coeff-probs nil :type (or null (simple-array (signed-byte 32) (*))))
  ;; non-zero context (above spans the frame, left is reset per MB row)
  (above-y (%emptyfx) :type fxvec) (above-u (%emptyfx) :type fxvec)
  (above-v (%emptyfx) :type fxvec) (above-y2 (%emptyfx) :type fxvec)
  (left-y (make-array 4 :element-type '(signed-byte 32)) :type fxvec)
  (left-u (make-array 2 :element-type '(signed-byte 32)) :type fxvec)
  (left-v (make-array 2 :element-type '(signed-byte 32)) :type fxvec)
  (left-y2 0 :type fixnum)
  ;; segmentation
  (seg-enabled nil) (seg-update-map nil) (seg-abs nil)
  (seg-tree-probs (make-array 3 :element-type '(signed-byte 32) :initial-element 255))
  (seg-quant (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
  (seg-filter (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
  seg-dq                                ; vector of 4 dequant-factor lists
  ;; skip
  (mb-no-skip nil) (prob-skip 0 :type fixnum)
  ;; loop filter header
  (filter-simple nil) (filter-level 0 :type fixnum) (sharpness 0 :type fixnum)
  (lf-delta-enabled nil)
  (ref-lf-delta (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
  (mode-lf-delta (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
  ;; per-MB records for the loop filter
  ;; IDCT scratch: the column pass, and the residual as 16 packed s16 (32 octets)
  (idct-tmp (make-array 16 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (16)))
  (res16 (make-array 32 :element-type '(unsigned-byte 8)) :type u8vec)
  ;; per-block coefficient masks for the current MB (bit = block: Y 0-15, U 16-19, V 20-23, Y2 24):
  ;; BLK-NZ = any non-zero coefficient (DC included), BLK-AC = a non-zero AC coefficient.
  ;; A block with neither is skipped at reconstruction; NZ without AC takes the DC-only path.
  ;; Flag arrays, not bit masks: a variable-count (ash 1 idx) is a runtime
  ;; bignum-ash call on Modus (~2 us) — it doubled the whole MB loop.
  (blk-nz (make-array 25 :element-type '(unsigned-byte 8) :initial-element 0) :type u8vec)
  (blk-ac (make-array 25 :element-type '(unsigned-byte 8) :initial-element 0) :type u8vec)
  (mb-i4x4 #() :type simple-vector) (mb-nonzero #() :type simple-vector)
  (mb-seg (%emptyfx) :type fxvec)
  ;; per-MB work buffers
  (ycoeffs (let ((v (make-array 16)))
             (dotimes (i 16) (setf (aref v i) (make-array 16 :element-type '(signed-byte 32))))
             v)
           :type simple-vector)
  (ublocks (let ((v (make-array 4)))
             (dotimes (i 4) (setf (aref v i) (make-array 16 :element-type '(signed-byte 32))))
             v)
           :type simple-vector)
  (vblocks (let ((v (make-array 4)))
             (dotimes (i 4) (setf (aref v i) (make-array 16 :element-type '(signed-byte 32))))
             v)
           :type simple-vector)
  (y2coeffs (make-array 16 :element-type '(signed-byte 32)) :type fxvec)
  ;; subblock modes: 16 per current MB, plus above row and left column caches
  (bmodes (make-array 16 :element-type '(signed-byte 32)) :type fxvec)
  (above-bmode (%emptyfx) :type fxvec)   ; 4 * mb-cols
  (left-bmode (make-array 4 :element-type '(signed-byte 32)) :type fxvec)
  (pred16 (make-array 256 :element-type '(unsigned-byte 8)) :type u8vec)
  (pred8 (make-array 64 :element-type '(unsigned-byte 8)) :type u8vec)
  (subpred (make-array 16 :element-type '(unsigned-byte 8)) :type u8vec)
  (suba (make-array 8 :element-type '(unsigned-byte 8)) :type u8vec)
  (subl (make-array 4 :element-type '(unsigned-byte 8)) :type u8vec))

(declaim (inline zero16))
(defun zero16 (a) (declare (type fxvec a)) (fill a 0))

;;; ---- header continuation (RFC 6386 §9.2-9.11) -------------------------

(defun parse-header-rest (d bd)
  "Read the compressed frame header from partition-0 decoder BD, returning the
   number of token partitions."
  (bool-bit bd 128)                     ; colour_space
  (bool-bit bd 128)                     ; clamp_type
  ;; segmentation (§9.3)
  (when (setf (d-seg-enabled d) (= 1 (bool-bit bd 128)))
    (let ((update-map (= 1 (bool-bit bd 128)))
          (update-data (= 1 (bool-bit bd 128))))
      (setf (d-seg-update-map d) update-map)
      (when update-data
        (setf (d-seg-abs d) (= 1 (bool-bit bd 128)))
        (dotimes (i 4)
          (setf (aref (d-seg-quant d) i)
                (if (= 1 (bool-bit bd 128)) (bool-signed bd 7) 0)))
        (dotimes (i 4)
          (setf (aref (d-seg-filter d) i)
                (if (= 1 (bool-bit bd 128)) (bool-signed bd 6) 0))))
      (when update-map
        (dotimes (i 3)
          (setf (aref (d-seg-tree-probs d) i)
                (if (= 1 (bool-bit bd 128)) (bool-literal bd 8) 255))))))
  ;; loop filter (§9.4)
  (setf (d-filter-simple d) (= 1 (bool-bit bd 128)))
  (setf (d-filter-level d) (bool-literal bd 6))
  (setf (d-sharpness d) (bool-literal bd 3))
  (when (setf (d-lf-delta-enabled d) (= 1 (bool-bit bd 128)))
    (when (= 1 (bool-bit bd 128))       ; mode_ref_lf_delta_update
      (dotimes (i 4)
        (when (= 1 (bool-bit bd 128))
          (setf (aref (d-ref-lf-delta d) i) (bool-signed bd 6))))
      (dotimes (i 4)
        (when (= 1 (bool-bit bd 128))
          (setf (aref (d-mode-lf-delta d) i) (bool-signed bd 6))))))
  ;; token partition count (§9.5)
  (let ((nparts (ash 1 (bool-literal bd 2))))
    ;; dequant indices (§9.6)
    (let* ((yac (bool-literal bd 7))
           (ydc  (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0))
           (y2dc (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0))
           (y2ac (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0))
           (uvdc (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0))
           (uvac (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0)))
      (setf (d-seg-dq d) (make-array 4))
      (dotimes (s 4)
        (let ((q (cond ((not (d-seg-enabled d)) yac)
                       ((d-seg-abs d) (aref (d-seg-quant d) s))
                       (t (+ yac (aref (d-seg-quant d) s))))))
          (setf (aref (d-seg-dq d) s)
                (segment-dequant q ydc y2dc y2ac uvdc uvac)))))
    ;; refresh_entropy_probs (key frame: single flag, rest is forced)
    (bool-bit bd 128)
    ;; coefficient probability updates (§9.9 / §13.4)
    (let ((cp (d-coeff-probs d)))
      (dotimes (i 1056)
        (when (= 1 (bool-bit bd (aref +coeff-update-probs+ i)))
          (setf (aref cp i) (bool-literal bd 8)))))
    ;; mb_no_skip_coeff + prob_skip_false (§9.11)
    (when (setf (d-mb-no-skip d) (= 1 (bool-bit bd 128)))
      (setf (d-prob-skip d) (bool-literal bd 8)))
    nparts))

;;; ---- macroblock prediction record (RFC 6386 §11) ----------------------

(defun read-mb-modes (d bd mbx)
  "Read the intra prediction modes for the macroblock at column MBX.  Returns
   the luma mode (0..4); 4 = B_PRED.  Fills D's bmodes and updates the subblock
   mode caches; also returns the chroma mode as the second value."
  (let ((ymode (treed-read bd +kf-ymode-tree+ +kf-ymode-prob+ 0))
        (ab (d-above-bmode d)) (lb (d-left-bmode d)) (bm (d-bmodes d)))
    (if (= ymode 4)                     ; B_PRED: 16 subblock modes
        (dotimes (sr 4)
          (dotimes (sc 4)
            (let* ((above (if (= sr 0) (aref ab (+ (* mbx 4) sc)) (aref bm (+ (* (1- sr) 4) sc))))
                   (left  (if (= sc 0) (aref lb sr) (aref bm (+ (* sr 4) (1- sc)))))
                   (mode (treed-read bd +bmode-tree+ +kf-bmode-prob+
                                     (* (+ (* above 10) left) 9))))
              (setf (aref bm (+ (* sr 4) sc)) mode))))
        ;; 16x16 mode: derive a constant subblock mode for the context caches
        (let ((bm-equiv (aref #(0 2 3 1) ymode)))  ; DC->DC V->VE H->HE TM->TM
          (dotimes (i 16) (setf (aref bm i) bm-equiv))))
    ;; propagate caches: bottom row -> above, right column -> left
    (dotimes (sc 4) (setf (aref ab (+ (* mbx 4) sc)) (aref bm (+ 12 sc))))
    (dotimes (sr 4) (setf (aref lb sr) (aref bm (+ (* sr 4) 3))))
    (values ymode (treed-read bd +uv-mode-tree+ +kf-uv-mode-prob+ 0))))

;;; ---- residue decode for one macroblock (RFC 6386 §13) -----------------

(defun decode-residue (d bd mbx skip has-y2 dq)
  "Decode all subblock coefficients for the current MB.  Returns T if the MB
   has any non-zero coefficient.  When SKIP, only the contexts are cleared."
  (declare (type dec d) (type dim mbx) (optimize (speed 3) (safety 1)))
  (let* ((ay (d-above-y d)) (au (d-above-u d)) (av (d-above-v d))
         (ay2 (d-above-y2 d)) (ly (d-left-y d)) (lu (d-left-u d)) (lv (d-left-v d))
         (y1dc (first dq)) (y1ac (second dq))
         (y2dc (third dq)) (y2ac (fourth dq))
         (uvdc (fifth dq)) (uvac (sixth dq))
         (cp (d-coeff-probs d))
         (nonzero nil) (nzm (d-blk-nz d)) (acm (d-blk-ac d)))
    (declare (type u8vec nzm acm))
    (dotimes (i 25) (setf (aref nzm i) 0 (aref acm i) 0))
    (cond
      (skip
       (dotimes (i 4) (setf (aref ay (+ (* mbx 4) i)) 0 (aref ly i) 0))
       (dotimes (i 2) (setf (aref au (+ (* mbx 2) i)) 0 (aref lu i) 0
                            (aref av (+ (* mbx 2) i)) 0 (aref lv i) 0))
       (when has-y2 (setf (aref ay2 mbx) 0 (d-left-y2 d) 0))
       ;; a skipped MB has no residue: clear the reused block buffers
       (zero16 (d-y2coeffs d))
       (dotimes (i 16) (zero16 (aref (d-ycoeffs d) i)))
       (dotimes (i 4) (zero16 (aref (d-ublocks d) i)) (zero16 (aref (d-vblocks d) i))))
      (t
       (when has-y2
         (let* ((ctx (+ (aref ay2 mbx) (d-left-y2 d)))
                (blk (d-y2coeffs d)))
           (zero16 blk)
           (multiple-value-bind (nz ac) (get-coeffs bd cp 1 blk ctx y2dc y2ac 0)
             (setf (aref ay2 mbx) (if nz 1 0) (d-left-y2 d) (if nz 1 0))
             (when nz (setf nonzero t (aref nzm 24) 1))
             (when ac (setf (aref acm 24) 1)))))
       (let ((plane (if has-y2 0 3)) (first (if has-y2 1 0)))
         (dotimes (sr 4)
           (dotimes (sc 4)
             (let* ((idx (+ (* sr 4) sc))
                    (ctx (+ (aref ay (+ (* mbx 4) sc)) (aref ly sr)))
                    (blk (aref (d-ycoeffs d) idx)))
               (zero16 blk)
               (multiple-value-bind (nz ac) (get-coeffs bd cp plane blk ctx y1dc y1ac first)
                 (setf (aref ay (+ (* mbx 4) sc)) (if nz 1 0) (aref ly sr) (if nz 1 0))
                 (when nz (setf nonzero t (aref nzm idx) 1))
                 (when ac (setf (aref acm idx) 1)))))))
       (dotimes (sr 2)
         (dotimes (sc 2)
           (let* ((idx (+ (* sr 2) sc))
                  (ctx (+ (aref au (+ (* mbx 2) sc)) (aref lu sr)))
                  (blk (aref (d-ublocks d) idx)))
             (zero16 blk)
             (multiple-value-bind (nz ac) (get-coeffs bd cp 2 blk ctx uvdc uvac 0)
               (setf (aref au (+ (* mbx 2) sc)) (if nz 1 0) (aref lu sr) (if nz 1 0))
               (when nz (setf nonzero t (aref nzm (+ 16 idx)) 1))
               (when ac (setf (aref acm (+ 16 idx)) 1))))))
       (dotimes (sr 2)
         (dotimes (sc 2)
           (let* ((idx (+ (* sr 2) sc))
                  (ctx (+ (aref av (+ (* mbx 2) sc)) (aref lv sr)))
                  (blk (aref (d-vblocks d) idx)))
             (zero16 blk)
             (multiple-value-bind (nz ac) (get-coeffs bd cp 2 blk ctx uvdc uvac 0)
               (setf (aref av (+ (* mbx 2) sc)) (if nz 1 0) (aref lv sr) (if nz 1 0))
               (when nz (setf nonzero t (aref nzm (+ 20 idx)) 1))
               (when ac (setf (aref acm (+ 20 idx)) 1))))))))
    nonzero))

;;; ---- reconstruction (RFC 6386 §14) ------------------------------------

(declaim (inline res16-ref))
(defun res16-ref (res k)
  "Element K of a residual stored as 16 packed little-endian s16 (32 octets)."
  (declare (type u8vec res) (type fixnum k))
  (let ((x (logior (aref res (* 2 k)) (ash (aref res (+ 1 (* 2 k))) 8))))
    (declare (type fixnum x))
    (if (>= x 32768) (- x 65536) x)))

(defun add-residual (pl mx my res)
  "Add a 4x4 IDCT residual RES (16 packed s16, see vp8-idct-s16) to the
   prediction already present in plane PL at pixel (MX,MY), clamping to 0..255."
  (declare (type plane pl) (type (signed-byte 26) mx my) (type u8vec res))
  (let* ((data (pl-data pl)) (stride (pl-stride pl)) (base (pidx pl mx my)))
    (declare (type u8vec data) (type fixnum stride base))
    (dotimes (i 4)
      (let ((o (+ base (* i stride))))
        (declare (type fixnum o))
        (dotimes (j 4)
          (setf (aref data (+ o j))
                (clamp255 (+ (aref data (+ o j)) (res16-ref res (+ (* i 4) j))))))))))

(defun add-residual-pred (pl mx my pred res)
  "Write PRED (a 4x4 raster) plus the 4x4 IDCT residual RES (packed s16) into
   plane PL at pixel (MX,MY), clamping to 0..255.  The B_PRED subblock path: the
   prediction is not in the plane yet, so this is add-residual with a separate
   source."
  (declare (type plane pl) (type (signed-byte 26) mx my) (type u8vec pred res))
  (let* ((data (pl-data pl)) (stride (pl-stride pl)) (base (pidx pl mx my)))
    (declare (type u8vec data) (type fixnum stride base))
    (dotimes (i 4)
      (let ((o (+ base (* i stride))))
        (declare (type fixnum o))
        (dotimes (j 4)
          (setf (aref data (+ o j))
                (clamp255 (+ (aref pred (+ (* i 4) j)) (res16-ref res (+ (* i 4) j))))))))))

;;; DC-only residual: the IDCT of a block whose only non-zero coefficient is
;;; DC is (dc+4)>>3 in all 16 positions (both passes reduce to a1=b1=dc).
(defun add-residual-dc (pl mx my v)
  (declare (type plane pl) (type (signed-byte 26) mx my) (type fixnum v))
  (let ((data (pl-data pl)) (stride (pl-stride pl)))
    (declare (type u8vec data) (type fixnum stride))
    (dotimes (i 4)
      (let ((o (pidx pl mx (+ my i))))
        (declare (type fixnum o))
        (dotimes (j 4) (setf (aref data (+ o j)) (clamp255 (+ (aref data (+ o j)) v))))))))

(defun add-residual-pred-dc (pl mx my pred v)
  (declare (type plane pl) (type (signed-byte 26) mx my) (type u8vec pred) (type fixnum v))
  (let ((data (pl-data pl)) (stride (pl-stride pl)))
    (declare (type u8vec data) (type fixnum stride))
    (dotimes (i 4)
      (let ((o (pidx pl mx (+ my i))))
        (declare (type fixnum o))
        (dotimes (j 4) (setf (aref data (+ o j)) (clamp255 (+ (aref pred (+ (* i 4) j)) v))))))))

(defun copy-pred4 (pl mx my pred)
  "Write a 4x4 prediction into the plane (a zero-residual subblock)."
  (declare (type plane pl) (type (signed-byte 26) mx my) (type u8vec pred))
  (let ((data (pl-data pl)))
    (declare (type u8vec data))
    (dotimes (i 4)
      (let ((o (pidx pl mx (+ my i))))
        (declare (type fixnum o))
        (dotimes (j 4) (setf (aref data (+ o j)) (aref pred (+ (* i 4) j))))))))

(declaim (inline dc-of))
(defun dc-of (blk) (declare (type (simple-array (signed-byte 32) (16)) blk)) (ash (+ (aref blk 0) 4) -3))

(defun reconstruct-luma16 (d mx my ymode has-above has-left)
  (declare (type dec d) (type (signed-byte 26) mx my))
  (let* ((pred (d-pred16 d)) (yp (d-yplane d))
         (data (pl-data yp)) (stride (pl-stride yp))
         (base (pidx yp mx my)))
    (declare (type u8vec pred data) (type fixnum stride base))
    (predict-block yp mx my 16 ymode has-above has-left pred)
    ;; write prediction into the plane (one block move per row), then add residuals
    (dotimes (r 16)
      (let ((o (+ base (* r stride))) (p (* r 16)))
        (declare (type fixnum o p))
        (replace data pred :start1 o :end1 (+ o 16) :start2 p :end2 (+ p 16))))
    (let ((nzm (d-blk-nz d)) (acm (d-blk-ac d)))
      (declare (type u8vec nzm acm))
      (dotimes (br 4)
        (dotimes (bc 4)
          (let ((idx (+ (* br 4) bc)))
            (declare (type fixnum idx))
            (cond ((= 1 (aref acm idx))
                   (add-residual yp (+ mx (* bc 4)) (+ my (* br 4))
                                 (vp8-idct-s16 (aref (d-ycoeffs d) idx) (d-idct-tmp d) (d-res16 d))))
                  ((= 1 (aref nzm idx))
                   (add-residual-dc yp (+ mx (* bc 4)) (+ my (* br 4)) (dc-of (aref (d-ycoeffs d) idx)))))))))))

(defun reconstruct-bpred (d mx my)
  (declare (type dec d) (type (signed-byte 26) mx my))
  (let ((yp (d-yplane d)) (bm (d-bmodes d)) (a (d-suba d)) (l (d-subl d))
        (pred (d-subpred d)))
    (dotimes (sr 4)
      (dotimes (sc 4)
        (let* ((idx (+ (* sr 4) sc)) (bx (+ mx (* sc 4))) (by (+ my (* sr 4)))
               (p (pget yp (- bx 1) (- by 1))))
          (dotimes (k 4) (setf (aref a k) (pget yp (+ bx k) (- by 1))))
          (if (< sc 3)
              (dotimes (k 4) (setf (aref a (+ 4 k)) (pget yp (+ bx 4 k) (- by 1))))
              ;; right-edge subblocks (3,7,11,15) reuse the pixels above-right of
              ;; subblock 3; the right pad supplies the replicated rightmost pixel
              (dotimes (k 4) (setf (aref a (+ 4 k)) (pget yp (+ mx 16 k) (- my 1)))))
          (dotimes (k 4) (setf (aref l k) (pget yp (- bx 1) (+ by k))))
          (predict-subblock (aref bm idx) a p l pred)
          (cond ((= 1 (aref (d-blk-ac d) idx))
                 (add-residual-pred yp bx by pred (vp8-idct-s16 (aref (d-ycoeffs d) idx) (d-idct-tmp d) (d-res16 d))))
                ((= 1 (aref (d-blk-nz d) idx))
                 (add-residual-pred-dc yp bx by pred (dc-of (aref (d-ycoeffs d) idx))))
                (t (copy-pred4 yp bx by pred))))))))

(defun reconstruct-chroma (d mbx mby uvmode has-above has-left)
  (declare (type dec d) (type fixnum mbx mby))
  (let ((cx (* mbx 8)) (cy (* mby 8)) (pred (d-pred8 d)))
    (declare (type fixnum cx cy) (type u8vec pred))
    (dolist (spec (list (list (d-uplane d) (d-ublocks d) 16)
                        (list (d-vplane d) (d-vblocks d) 20)))
      (let* ((pl (first spec)) (blocks (second spec)) (bit0 (third spec))
             (data (pl-data pl)) (stride (pl-stride pl)) (base (pidx pl cx cy)))
        (declare (type plane pl) (type u8vec data) (type fixnum stride base))
        (predict-block pl cx cy 8 uvmode has-above has-left pred)
        (dotimes (r 8)
          (let ((o (+ base (* r stride))) (p (* r 8)))
            (declare (type fixnum o p))
            (replace data pred :start1 o :end1 (+ o 8) :start2 p :end2 (+ p 8))))
        (let ((nzm (d-blk-nz d)) (acm (d-blk-ac d)))
          (declare (type u8vec nzm acm) (type fixnum bit0))
          (dotimes (br 2)
            (dotimes (bc 2)
              (let* ((idx (+ (* br 2) bc)) (fi (+ bit0 idx)))
                (declare (type fixnum idx fi))
                (cond ((= 1 (aref acm fi))
                       (add-residual pl (+ cx (* bc 4)) (+ cy (* br 4))
                                     (vp8-idct-s16 (aref blocks idx) (d-idct-tmp d) (d-res16 d))))
                      ((= 1 (aref nzm fi))
                       (add-residual-dc pl (+ cx (* bc 4)) (+ cy (* br 4)) (dc-of (aref blocks idx)))))))))))))

(defun scatter-y2 (d)
  "Invert the Y2 WHT and place its outputs as the DC of the 16 Y blocks."
  (let ((out (vp8-iwht (d-y2coeffs d))) (nzm (d-blk-nz d)))
    (declare (type u8vec nzm))
    (dotimes (idx 16)
      (let ((v (aref out idx)))
        (setf (aref (aref (d-ycoeffs d) idx) 0) v)
        (unless (zerop v) (setf (aref nzm idx) 1))))))

;;; ---- macroblock loop --------------------------------------------------

(defun run-macroblocks (d part0 tokens nparts)
  (let* ((cols (d-mb-cols d)) (rows (d-mb-rows d))
         (ap (d-above-bmode d)))
    ;; frame-level context reset
    (fill (d-above-y d) 0) (fill (d-above-u d) 0) (fill (d-above-v d) 0)
    (fill (d-above-y2 d) 0) (fill ap 0)
    (dotimes (mby rows)
      (let ((bd (aref tokens (mod mby nparts))))
        ;; row-level left contexts
        (fill (d-left-y d) 0) (fill (d-left-u d) 0) (fill (d-left-v d) 0)
        (setf (d-left-y2 d) 0)
        (fill (d-left-bmode d) 0)
        (dotimes (mbx cols)
          (let* ((seg (if (and (d-seg-enabled d) (d-seg-update-map d))
                          (treed-read part0 +mb-segment-tree+ (d-seg-tree-probs d) 0)
                          0))
                 (skip (and (d-mb-no-skip d) (= 1 (bool-bit part0 (d-prob-skip d))))))
            (multiple-value-bind (ymode uvmode) (read-mb-modes d part0 mbx)
              (let* ((has-y2 (/= ymode 4))
                     (dq (aref (d-seg-dq d) seg))
                     (nz (decode-residue d bd mbx skip has-y2 dq))
                     (mx (* mbx 16)) (my (* mby 16))
                     (has-above (> mby 0)) (has-left (> mbx 0)))
                (when (and has-y2 (not skip)) (scatter-y2 d))
                (when (and has-y2 skip)
                  (dotimes (idx 16) (setf (aref (aref (d-ycoeffs d) idx) 0) 0)))
                (if (= ymode 4)
                    (reconstruct-bpred d mx my)
                    (reconstruct-luma16 d mx my ymode has-above has-left))
                (reconstruct-chroma d mbx mby uvmode has-above has-left)
                ;; records for the loop filter
                (let ((mi (+ (* mby cols) mbx)))
                  (setf (aref (d-mb-i4x4 d) mi) (= ymode 4)
                        (aref (d-mb-nonzero d) mi) nz
                        (aref (d-mb-seg d) mi) seg))))))
        (pad-right (d-yplane d) (* mby 16) 16)))))

;;; ---- top-level pipeline ------------------------------------------------

(defun decode-key-frame (bytes off size fr)
  (let* ((w (fr-width fr)) (h (fr-height fr))
         (cols (ceiling w 16)) (rows (ceiling h 16))
         (d (make-dec :bytes bytes :mb-cols cols :mb-rows rows :width w :height h
                      :yplane (make-plane* (* cols 16) (* rows 16) 4)
                      :uplane (make-plane* (* cols 8) (* rows 8) 0)
                      :vplane (make-plane* (* cols 8) (* rows 8) 0)
                      :coeff-probs (copy-seq +default-coeff-probs+)
                      :above-y (make-array (* cols 4) :element-type '(signed-byte 32) :initial-element 0)
                      :above-u (make-array (* cols 2) :element-type '(signed-byte 32) :initial-element 0)
                      :above-v (make-array (* cols 2) :element-type '(signed-byte 32) :initial-element 0)
                      :above-y2 (make-array cols :element-type '(signed-byte 32) :initial-element 0)
                      :above-bmode (make-array (* cols 4) :element-type '(signed-byte 32) :initial-element 0)
                      :mb-i4x4 (make-array (* cols rows))
                      :mb-nonzero (make-array (* cols rows))
                      :mb-seg (make-array (* cols rows) :element-type '(signed-byte 32) :initial-element 0)))
         (off0 (fr-off0 fr))
         (part0-size (fr-part0-size fr))
         (part0 (bool-init bytes off0 (+ off0 part0-size)))
         (vp8-end (+ off size)))
    ;; header continuation, then set up token partitions
    (let* ((nparts (parse-header-rest d part0))
           (tbl (+ off0 part0-size))
           (data-start (+ tbl (* 3 (1- nparts))))
           (tokens (make-array nparts)))
      (let ((cur data-start))
        (dotimes (i nparts)
          (let ((sz (if (< i (1- nparts)) (u24le bytes (+ tbl (* 3 i))) (- vp8-end cur))))
            (setf (aref tokens i) (bool-init bytes cur (+ cur sz)))
            (incf cur sz))))
      (run-macroblocks d part0 tokens nparts)
      (loop-filter d)
      (values d))))


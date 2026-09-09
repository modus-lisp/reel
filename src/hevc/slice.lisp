;;;; hevc/slice.lisp — slice data: the coding tree, coding units, and residual coding.
;;;;
;;;; A picture is a grid of CODING TREE UNITS, each of which is a quadtree.  Where H.264 had one
;;;; fixed macroblock size and a small menu of partitions inside it, HEVC lets the encoder cut a
;;;; 64x64 region down to 8x8 wherever the picture needs the detail and leave it whole where it does
;;;; not — and then cuts a SECOND quadtree, of transform blocks, inside each leaf.  That is most of
;;;; where the compression came from, and all of why this file is recursive.
;;;;
;;;; RESIDUAL CODING IS THE OTHER HALF.  H.264 codes a block's coefficients as a flat list with a
;;;; context per scan position.  HEVC codes them as 4x4 SUB-BLOCKS, each with a flag saying whether
;;;; it holds anything at all, and derives the contexts from position and from what the neighbouring
;;;; sub-blocks did.  So one set of contexts serves every transform size from 4x4 to 32x32, an empty
;;;; corner of a 32x32 block costs one bin rather than a thousand, and the tables stay small.
;;;;
;;;; What this file does NOT do yet is reconstruct.  It decodes every syntax element of an intra
;;;; slice and hands the coefficients to %ON-RESIDUAL, which currently only counts them.  Prediction
;;;; and the transforms come next; the point of stopping here is that a syntax error shows up as a
;;;; slice that fails to end where it should, which is checkable on its own.

(in-package #:reel.hevc)

;;; ---- decoder state ----------------------------------------------------------------------------

(defstruct (ctx (:conc-name cx-))
  "Everything the slice data walk carries: the parameter sets, the arithmetic decoder, and the
   per-picture arrays a syntax element's context may look at."
  sps pps sh cabac
  (min-cb-width 0 :type fixnum)         ; the picture in units of the smallest coding block
  (min-cb-height 0 :type fixnum)
  (min-pu-width 0 :type fixnum)         ; and in units of 4x4, for the intra modes
  (min-pu-height 0 :type fixnum)
  ;; the depth of the coding quadtree at each min-CB, which is what split_cu_flag's context reads
  (ct-depth (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  ;; the luma intra prediction mode at each 4x4, which the NEXT block's mode is predicted from
  (intra-mode (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  ;; MODE_INTRA at each min-CB.  Only intra is decoded for now, but the flag is what a neighbour
  ;; asks about, so it is recorded rather than assumed.
  (is-intra (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (skip (make-array 0 :element-type 'bit) :type simple-bit-vector)
  ;; which slice segment address covers each CTB, so availability can stop at a slice boundary
  (ctb-slice (make-array 0 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (slice-addr 0 :type fixnum)
  ;; the current coding unit, as the syntax elements that outlive one function
  (cu-x 0 :type fixnum) (cu-y 0 :type fixnum)
  (cu-log2 0 :type fixnum)
  (cu-intra t) (cu-transquant-bypass nil)
  (part-mode 0 :type fixnum)            ; 0 = 2Nx2N, 3 = NxN; only those two occur in intra
  (intra-split nil)
  (max-trafo-depth 0 :type fixnum)
  (cu-depth 0 :type fixnum)             ; the coding quadtree depth, which inter_pred_idc's context uses
  (merge-2nx2n nil)                     ; did the single prediction unit of this unit merge?
  (qp 26 :type fixnum)
  (qp-delta-coded nil)
  (%chroma-idx 4 :type fixnum)          ; this coding unit's intra_chroma_pred_mode
  (sao-type-cr 0 :type fixnum)          ; the two chroma components share one SAO type
  pic                                   ; where the samples go
  ;; the z-scan address of every smallest transform block, which is how availability is decided in
  ;; a quadtree: decoding order is not raster, so "is my neighbour decoded" is an address compare
  (zscan (make-array 0 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (z-width 0 :type fixnum)
  ;; the luma quantiser at each min-CB, and the running predictor cu_qp_delta is relative to
  (qp-y (make-array 0 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (qp-y-prev 26 :type fixnum)
  (qg-x 0 :type fixnum) (qg-y 0 :type fixnum)
  (cu-qp-delta 0 :type fixnum)
  ;; scratch: one transform block's prediction, and the transform's intermediate array
  (pred (make-array (* 32 32) :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  ;; the reference lists in force, and the picture the temporal candidate comes from
  (list0 #() :type simple-vector) (list1 #() :type simple-vector)
  collocated
  ;; two prediction buffers at 14-bit precision, for the two halves of a bi-predicted block
  (p0 (make-array (* 64 64) :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (p1 (make-array (* 64 64) :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (scratch (make-array (* 32 32) :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  ;; scratch for one transform block's coefficients, biggest first
  (coeffs (make-array (* 32 32) :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  ;; what the syntax walk has produced, for the checks that stand in for reconstruction
  (n-cu 0 :type fixnum) (n-tu 0 :type fixnum) (n-coeff 0 :type fixnum))

(defun %make-ctx (sps pps sh cabac)
  (let* ((mcw (ash (sps-width sps) (- (sps-min-cb-log2 sps))))
         (mch (ash (sps-height sps) (- (sps-min-cb-log2 sps))))
         (mpw (ash (sps-width sps) -2))
         (mph (ash (sps-height sps) -2)))
    (make-ctx :sps sps :pps pps :sh sh :cabac cabac
              :min-cb-width mcw :min-cb-height mch
              :min-pu-width mpw :min-pu-height mph
              :ct-depth (make-array (* mcw mch) :element-type '(unsigned-byte 8)
                                                :initial-element 0)
              :intra-mode (make-array (* mpw mph) :element-type '(unsigned-byte 8)
                                                  :initial-element 1)   ; DC
              :is-intra (make-array (* mcw mch) :element-type 'bit :initial-element 0)
              :skip (make-array (* mcw mch) :element-type 'bit :initial-element 0)
              :ctb-slice (make-array (sps-ctbs sps) :element-type '(signed-byte 32)
                                                    :initial-element -1)
              :zscan (%build-z-scan sps)
              :z-width (ash (sps-width sps) (- (sps-min-tb-log2 sps)))
              :qp-y (make-array (* mcw mch) :element-type '(signed-byte 32)
                                            :initial-element (sh-qp sh))
              :qp-y-prev (sh-qp sh)
              :qp (sh-qp sh))))

(declaim (inline %gc %gd))
(defun %gc (c x y)
  "Is the luma sample position (X,Y) inside the picture and already decoded in THIS slice?"
  (declare (type ctx c) (type fixnum x y))
  (let ((sps (cx-sps c)))
    (and (>= x 0) (>= y 0) (< x (sps-width sps)) (< y (sps-height sps))
         (let* ((log2 (sps-ctb-log2 sps))
                (ctb (+ (* (ash y (- log2)) (sps-ctbs-wide sps)) (ash x (- log2)))))
           (= (aref (cx-ctb-slice c) ctb) (cx-slice-addr c))))))

(defun %gd (c x y)
  "The coding quadtree depth recorded at luma position (X,Y)."
  (declare (type ctx c) (type fixnum x y))
  (let ((s (sps-min-cb-log2 (cx-sps c))))
    (aref (cx-ct-depth c) (+ (* (ash y (- s)) (cx-min-cb-width c)) (ash x (- s))))))

;;; The reconstruction lives in intra.lisp and transform.lisp, which are loaded after this file
;;; because they read the state defined above.
(declaim (ftype function %build-z-scan %intra-predict %dequantise %inverse-transform
                %transform-skip %chroma-qp %available-p
                %merge-candidates %amvp %mc-luma %mc-chroma %write-uni %write-bi
                %prediction-units cand-ref cand-mvx cand-mvy cand-poc))

;;; ---- the syntax elements that are one bin each -------------------------------------------------

(defmacro %bin (c ctx) `(decode-decision (cx-cabac ,c) ,ctx))
(defmacro %bypass (c) `(decode-bypass (cx-cabac ,c)))

(defun %split-cu-flag (c x0 y0 depth)
  "split_cu_flag, whose context counts how many neighbours are cut FINER than we are (9.3.4.2.2).
   Two neighbours already split is strong evidence this region is busy too."
  (let ((inc 0))
    (when (and (%gc c (1- x0) y0) (> (%gd c (1- x0) y0) depth)) (incf inc))
    (when (and (%gc c x0 (1- y0)) (> (%gd c x0 (1- y0)) depth)) (incf inc))
    (%bin c (+ +ctx-split-coding-unit-flag+ inc))))

(defun %cu-skip-flag (c x0 y0)
  "cu_skip_flag, whose context counts how many neighbours were themselves skipped.

   A skipped unit is one merged prediction and no residual at all — the cheapest thing HEVC can
   say, and worth its own flag rather than a merge with an empty transform tree because a region
   that is not changing tends to be a run of them."
  (let ((inc 0) (s (sps-min-cb-log2 (cx-sps c))) (w (cx-min-cb-width c)))
    (when (and (%gc c (1- x0) y0)
               (plusp (aref (cx-skip c) (+ (* (ash y0 (- s)) w) (ash (1- x0) (- s))))))
      (incf inc))
    (when (and (%gc c x0 (1- y0))
               (plusp (aref (cx-skip c) (+ (* (ash (1- y0) (- s)) w) (ash x0 (- s))))))
      (incf inc))
    (%bin c (+ +ctx-skip-flag+ inc))))

(defun %part-mode (c log2-size intra-p)
  "part_mode (Table 9-43): how a coding unit is cut into prediction units.

   For intra it is one bin and only at the smallest size.  For inter it is a tree over up to four
   bins plus a bypass one, and the ASYMMETRIC shapes at the end — a quarter and three quarters
   rather than two halves — are what amp_enabled_flag turns on.  They exist because an object edge
   rarely falls down the middle of a block."
  (let ((min-p (= log2-size (sps-min-cb-log2 (cx-sps c)))))
    (cond
      ((= 1 (%bin c +ctx-part-mode+)) 0)                        ; 1        -> 2Nx2N
      (min-p
       (cond (intra-p 3)                                        ; 0        -> NxN
             ((= 1 (%bin c (+ +ctx-part-mode+ 1))) 1)           ; 01       -> 2NxN
             ((= log2-size 3) 2)                                ; 00       -> Nx2N
             ((= 1 (%bin c (+ +ctx-part-mode+ 2))) 2)           ; 001      -> Nx2N
             (t 3)))                                            ; 000      -> NxN
      ((not (sps-amp-enabled (cx-sps c)))
       (if (= 1 (%bin c (+ +ctx-part-mode+ 1))) 1 2))
      ((= 1 (%bin c (+ +ctx-part-mode+ 1)))
       (cond ((= 1 (%bin c (+ +ctx-part-mode+ 3))) 1)           ; 011      -> 2NxN
             ((= 1 (%bypass c)) 5)                              ; 0101     -> 2NxnD
             (t 4)))                                            ; 0100     -> 2NxnU
      (t
       (cond ((= 1 (%bin c (+ +ctx-part-mode+ 3))) 2)           ; 001      -> Nx2N
             ((= 1 (%bypass c)) 7)                              ; 0001     -> nRx2N
             (t 6))))))                                         ; 0000     -> nLx2N

(defun %ref-idx (c n)
  "ref_idx_lX: two context-coded bins then bypass, truncated at the list length."
  (let ((i 0) (maxi (1- n)))
    (declare (type fixnum i maxi))
    (loop while (and (< i (min maxi 2)) (= 1 (%bin c (+ +ctx-ref-idx-l0+ i)))) do (incf i))
    (when (= i 2)
      (loop while (and (< i maxi) (= 1 (%bypass c))) do (incf i)))
    i))

(defun %mvd-component (c)
  "One component of a motion vector difference, after its greater-than-one flag said it is large:
   an order-one Exp-Golomb remainder and a sign, both in bypass."
  (let ((v 2) (k 1))
    (declare (type fixnum v k))
    (loop while (and (< k 31) (= 1 (%bypass c)))
          do (incf v (ash 1 k)) (incf k))
    (when (>= k 31) (%err "runaway motion vector difference"))
    (loop while (plusp k) do (decf k) (incf v (ash (%bypass c) k)))
    (if (= 1 (%bypass c)) (- v) v)))

(defun %mvd (c)
  "mvd_coding (7.3.8.9): (values dx dy).

   BOTH components' greater-than-zero flags come before EITHER greater-than-one flag, and both of
   those before either remainder.  Reading it component by component is the natural shape and the
   wrong one — it interleaves bins that share a context with bins that do not."
  (let* ((g0x (= 1 (%bin c +ctx-abs-mvd-greater0-flag+)))
         (g0y (= 1 (%bin c +ctx-abs-mvd-greater0-flag+)))
         (g1x (and g0x (= 1 (%bin c (+ +ctx-abs-mvd-greater1-flag+ 1)))))
         (g1y (and g0y (= 1 (%bin c (+ +ctx-abs-mvd-greater1-flag+ 1)))))
         (dx 0) (dy 0))
    (declare (type fixnum dx dy))
    (when g0x
      (setf dx (if g1x (%mvd-component c) (if (= 1 (%bypass c)) -1 1))))
    (when g0y
      (setf dy (if g1y (%mvd-component c) (if (= 1 (%bypass c)) -1 1))))
    (values dx dy)))

(defun %intra-chroma-pred-mode (c)
  "intra_chroma_pred_mode: one context-coded bin, then two bypass ones if it was set.
   4 means \"the same mode the luma block chose\", which is what most blocks say."
  (if (zerop (%bin c +ctx-intra-chroma-pred-mode+))
      4
      (logior (ash (%bypass c) 1) (%bypass c))))

;;; ---- intra prediction modes (8.4.2) ------------------------------------------------------------

(defun %intra-mode-at (c x y)
  (aref (cx-intra-mode c) (+ (* (ash y -2) (cx-min-pu-width c)) (ash x -2))))

(defun %set-intra-mode (c x y size mode)
  (let ((w (cx-min-pu-width c)) (n (ash size -2)))
    (dotimes (j n)
      (dotimes (i n)
        (setf (aref (cx-intra-mode c) (+ (* (+ (ash y -2) j) w) (ash x -2) i)) mode)))))

(defun %cu-intra-p (c x y)
  (let ((s (sps-min-cb-log2 (cx-sps c))))
    (plusp (aref (cx-is-intra c) (+ (* (ash y (- s)) (cx-min-cb-width c)) (ash x (- s)))))))

(defun %luma-intra-mode (c x0 y0 prev-flag mpm-idx rem)
  "IntraPredModeY (8.4.2): three most probable modes, then either an index into them or a
   five-bit number that skips over them.

   The ABOVE neighbour is deliberately treated as unavailable when it lies in the coding tree row
   above (8.4.2, the yCb-1 test).  That is not an oversight about availability, it is a memory
   decision written into the standard: a decoder must otherwise keep a whole picture-width row of
   prediction modes, and the encoder is told to expect DC there so both sides agree."
  (let* ((ctb-mask (1- (ash 1 (sps-ctb-log2 (cx-sps c)))))
         (cand-a (if (and (%gc c (1- x0) y0) (%cu-intra-p c (1- x0) y0))
                     (%intra-mode-at c (1- x0) y0)
                     1))
         (cand-b (if (and (%gc c x0 (1- y0)) (%cu-intra-p c x0 (1- y0))
                          ;; and only when it is in the same row of coding tree blocks
                          (plusp (logand y0 ctb-mask)))
                     (%intra-mode-at c x0 (1- y0))
                     1))
         (cands (make-array 3 :element-type 'fixnum)))
    (if (= cand-a cand-b)
        (if (< cand-a 2)
            (setf (aref cands 0) 0 (aref cands 1) 1 (aref cands 2) 26)
            (setf (aref cands 0) cand-a
                  (aref cands 1) (+ 2 (mod (+ cand-a 29) 32))
                  (aref cands 2) (+ 2 (mod (- cand-a 1) 32))))
        (progn
          (setf (aref cands 0) cand-a (aref cands 1) cand-b)
          (setf (aref cands 2)
                (cond ((and (/= cand-a 0) (/= cand-b 0)) 0)
                      ((and (/= cand-a 1) (/= cand-b 1)) 1)
                      (t 26)))))
    (if prev-flag
        (aref cands mpm-idx)
        (let ((m rem)
              (sorted (sort (copy-seq cands) #'<)))
          ;; the three candidates are removed from the code space, so a transmitted value steps
          ;; over each one it has reached
          (dotimes (i 3 m)
            (when (>= m (aref sorted i)) (incf m)))))))

(defun %chroma-intra-mode (luma-mode chroma-idx)
  "IntraPredModeC (Table 8-2/8-3 for 4:2:0).

   Four of the five choices name a fixed direction, and the fifth says \"whatever luma chose\".  The
   substitution to 34 exists so the four fixed choices never merely repeat the luma mode: if the
   named direction is already what luma picked, the code would be wasted, so it means 34 instead."
  (case chroma-idx
    (0 (if (= luma-mode 0) 34 0))
    (1 (if (= luma-mode 26) 34 26))
    (2 (if (= luma-mode 10) 34 10))
    (3 (if (= luma-mode 1) 34 1))
    (t luma-mode)))

;;; ---- residual coding (7.3.8.11) ----------------------------------------------------------------

(defun %last-significant-prefix (c ctx-base c-idx log2-size)
  "The prefix of last_sig_coeff_x/y: truncated unary, its context narrowing as the block grows."
  (let* ((maxi (1- (ash log2-size 1)))
         (offset (if (zerop c-idx)
                     (+ (* 3 (- log2-size 2)) (ash (1- log2-size) -2))
                     15))
         (shift (if (zerop c-idx) (ash (1+ log2-size) -2) (- log2-size 2)))
         (i 0))
    (declare (type fixnum maxi offset shift i))
    (loop while (and (< i maxi)
                     (= 1 (%bin c (+ ctx-base (ash i (- shift)) offset))))
          do (incf i))
    i))

(defun %last-significant-suffix (c prefix)
  "The suffix: (prefix >> 1) - 1 bypass bins, which locate the coefficient inside the range the
   prefix named."
  (let ((len (1- (ash prefix -1))) (v 0))
    (declare (type fixnum len v))
    (dotimes (i len v) (setf v (logior (ash v 1) (%bypass c))))))

(defun %coeff-abs-level-remaining (c rice)
  "coeff_abs_level_remaining (9.3.3.11): Rice coded up to a point, Exp-Golomb after it.

   The escape is the part to get right.  A prefix under three is a plain Rice code, and the value
   is prefix<<k plus k suffix bits.  From three upward the suffix WIDENS with the prefix, so the
   representable range doubles each step and a large coefficient does not cost its own magnitude."
  (declare (type fixnum rice))
  (let ((prefix 0))
    (declare (type fixnum prefix))
    (loop while (and (< prefix 31) (= 1 (%bypass c))) do (incf prefix))
    (when (>= prefix 31) (%err "runaway coeff_abs_level_remaining prefix"))
    (if (< prefix 3)
        (let ((v 0))
          (declare (type fixnum v))
          (dotimes (i rice) (setf v (logior (ash v 1) (%bypass c))))
          (+ (ash prefix rice) v))
        (let* ((k (+ (- prefix 3) rice)) (v 0))
          (declare (type fixnum k v))
          (when (> k 22) (%err "coeff_abs_level_remaining suffix of ~d bits" k))
          (dotimes (i k) (setf v (logior (ash v 1) (%bypass c))))
          (+ (ash (+ (ash 1 (- prefix 3)) 2) rice) v)))))

(defun %residual-coding (c x0 y0 log2-size c-idx pred-mode)
  "One transform block's coefficients.

   The shape of it: find the LAST significant coefficient, then walk backwards through the 4x4
   sub-blocks from there to the DC.  Each sub-block says whether it holds anything, then which of
   its sixteen positions are non-zero, then how large those are — as a greater-than-one flag for
   the first eight, a single greater-than-two flag for the first of those, and a Rice-coded
   remainder for whatever is left over.  Coding the magnitudes in that order is what keeps almost
   every bin in the block on a context that is nearly certain."
  (declare (type ctx c) (type fixnum x0 y0 log2-size c-idx pred-mode)
           (ignorable x0 y0))
  (let* ((pps (cx-pps c))
         (transform-skip nil)
         (scan (if (and (cx-cu-intra c)
                        (or (= log2-size 2) (and (= log2-size 3) (zerop c-idx))))
                   (%scan-for-intra pred-mode)
                   +scan-diag+))
         (size (ash 1 log2-size))
         (coeffs (cx-coeffs c)))
    ;; TRANSFORM-SKIP changes what the inverse transform does, not what the parser reads, so it is
    ;; decoded and set aside until there is a transform to tell
    (declare (type fixnum size) (ignorable transform-skip))
    (when (and (pps-transform-skip pps) (not (cx-cu-transquant-bypass c)) (= log2-size 2))
      (setf transform-skip (= 1 (%bin c (+ +ctx-transform-skip-flag+ (if (zerop c-idx) 0 1))))))
    ;; ---- where the last significant coefficient is
    (let* ((px (%last-significant-prefix c +ctx-last-significant-coeff-x-prefix+ c-idx log2-size))
           (py (%last-significant-prefix c +ctx-last-significant-coeff-y-prefix+ c-idx log2-size))
           (lx (if (> px 3)
                   (+ (* (ash 1 (1- (ash px -1))) (+ 2 (logand px 1)))
                      (%last-significant-suffix c px))
                   px))
           (ly (if (> py 3)
                   (+ (* (ash 1 (1- (ash py -1))) (+ 2 (logand py 1)))
                      (%last-significant-suffix c py))
                   py)))
      (declare (type fixnum lx ly))
      (when (= scan +scan-vert+) (rotatef lx ly))
      (multiple-value-bind (cg-x cg-y off-x off-y num-coeff)
          (ecase scan
            (#.+scan-diag+
             (values (case log2-size (2 (first +diag4x4+)) (3 (first +diag2x2+))
                       (4 (first +diag4x4+)) (t (first +diag8x8+)))
                     (case log2-size (2 (second +diag4x4+)) (3 (second +diag2x2+))
                       (4 (second +diag4x4+)) (t (second +diag8x8+)))
                     (first +diag4x4+) (second +diag4x4+)
                     (+ (aref +diag4x4-inv+ (logand ly 3) (logand lx 3))
                        (ash (case log2-size
                               (2 0)
                               (3 (aref +diag2x2-inv+ (ash ly -2) (ash lx -2)))
                               (4 (aref +diag4x4-inv+ (ash ly -2) (ash lx -2)))
                               (t (aref +diag8x8-inv+ (ash ly -2) (ash lx -2))))
                             4))))
            (#.+scan-horiz+
             (values (first +horiz2x2+) (second +horiz2x2+)
                     (first +horiz4x4+) (second +horiz4x4+)
                     (aref +horiz8x8-inv+ ly lx)))
            (#.+scan-vert+
             (values (second +horiz2x2+) (first +horiz2x2+)
                     (second +horiz4x4+) (first +horiz4x4+)
                     (aref +horiz8x8-inv+ lx ly))))
        (incf num-coeff)
        (let* ((last-subset (ash (1- num-coeff) -4))
               (cg-last-x (ash lx -2)) (cg-last-y (ash ly -2))
               (cgs (make-array '(9 9) :element-type 'bit :initial-element 0))
               (sig-idx (make-array 16 :element-type '(unsigned-byte 8)))
               (greater1-ctx 1))
          (declare (type fixnum last-subset greater1-ctx) (dynamic-extent cgs sig-idx))
          (fill coeffs 0 :end (* size size))
          (loop for i of-type fixnum from last-subset downto 0 do
            (let* ((xcg (aref cg-x i)) (ycg (aref cg-y i))
                   (offset (ash i 4))
                   (implicit nil)
                   (nsig 0))
              (declare (type fixnum xcg ycg offset nsig))
              ;; does this sub-block hold anything?
              (if (and (< i last-subset) (> i 0))
                  (let ((inc 0))
                    (when (< xcg (1- (ash 1 (- log2-size 2))))
                      (incf inc (aref cgs (1+ xcg) ycg)))
                    (when (< ycg (1- (ash 1 (- log2-size 2))))
                      (incf inc (aref cgs xcg (1+ ycg))))
                    (setf (aref cgs xcg ycg)
                          (%bin c (+ +ctx-significant-coeff-group-flag+
                                     (min inc 1) (if (plusp c-idx) 2 0)))
                          implicit t))
                  (setf (aref cgs xcg ycg)
                        (if (or (and (= xcg cg-last-x) (= ycg cg-last-y))
                                (and (zerop xcg) (zerop ycg)))
                            1 0)))
              (let ((n-end (if (= i last-subset)
                               (progn (setf (aref sig-idx 0) (- num-coeff offset 1) nsig 1)
                                      (- num-coeff offset 2))
                               15)))
                (declare (type fixnum n-end))
                (when (and (plusp (aref cgs xcg ycg)) (>= n-end 0))
                  ;; which positions in it are non-zero
                  (let* ((prev-sig 0)
                         (limit (ash (1- (ash 1 log2-size)) -2)))
                    (when (< xcg limit) (setf prev-sig (aref cgs (1+ xcg) ycg)))
                    (when (< ycg limit) (incf prev-sig (ash (aref cgs xcg (1+ ycg)) 1)))
                    (let ((row (if (= log2-size 2) 0 (1+ prev-sig)))
                          (scf 0)
                          (nsig0 nsig))
                      (declare (type fixnum row scf nsig0))
                      (unless (= log2-size 2)
                        (when (plusp c-idx) (setf scf 27))
                        (if (zerop c-idx)
                            (progn
                              (when (or (plusp xcg) (plusp ycg)) (incf scf 3))
                              (incf scf (if (= log2-size 3)
                                            (if (= scan +scan-diag+) 9 15)
                                            21)))
                            (incf scf (if (= log2-size 3) 9 12))))
                      (when (and (= log2-size 2) (plusp c-idx)) (setf scf 27))
                      (loop for n of-type fixnum from n-end above 0
                            do (let ((sig (%bin c (+ +ctx-significant-coeff-flag+ scf
                                                     (aref +sig-ctx-map+ scan row n)))))
                                 (setf (aref sig-idx nsig) n)
                                 (incf nsig sig)))
                      ;; The DC of a sub-block is INFERRED non-zero when the sub-block said it
                      ;; had something and nothing else in it turned out to.  That is the whole
                      ;; point of the sub-block flag: it would otherwise be possible to spend a bin
                      ;; announcing a sub-block and then sixteen more saying it was empty after all,
                      ;; so the encoder is not allowed to and the decoder does not ask.
                      (if (and implicit (= nsig nsig0))
                          (progn (setf (aref sig-idx nsig) 0) (incf nsig))
                          (let ((scf0 (if (zerop i)
                                          (if (zerop c-idx) 0 27)
                                          (+ 2 scf))))
                            (setf (aref sig-idx nsig) 0)
                            (incf nsig (%bin c (+ +ctx-significant-coeff-flag+ scf0))))))))
                ;; ---- and how large they are
                (when (plusp nsig)
                  (let* ((ctx-set (if (and (plusp i) (zerop c-idx)) 2 0))
                         (rice 0)
                         (first-gt1 -1)
                         ;; not a bit vector: the greater-2 flag is ADDED to the first set entry,
                         ;; so an element can reach 2 and the level below reads it as a magnitude
                         (gt1 (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
                         (last-nz (aref sig-idx 0))
                         (first-nz (aref sig-idx (1- nsig)))
                         (sum 0))
                    (declare (type fixnum ctx-set rice first-gt1 last-nz first-nz sum)
                             (dynamic-extent gt1))
                    (when (and (/= i last-subset) (zerop greater1-ctx)) (incf ctx-set))
                    (setf greater1-ctx 1)
                    (dotimes (m (min nsig 8))
                      (let ((flag (%bin c (+ +ctx-coeff-abs-level-greater1-flag+
                                             (ash ctx-set 2) greater1-ctx
                                             (if (plusp c-idx) 16 0)))))
                        (setf (aref gt1 m) flag)
                        (when (and (plusp flag) (minusp first-gt1)) (setf first-gt1 m))
                        (setf greater1-ctx
                              (cond ((plusp flag) 0)
                                    ((and (plusp greater1-ctx) (< greater1-ctx 3))
                                     (1+ greater1-ctx))
                                    (t greater1-ctx)))))
                    (let ((sign-hidden (and (not (cx-cu-transquant-bypass c))
                                            (>= (- last-nz first-nz) 4))))
                      (when (>= first-gt1 0)
                        (incf (aref gt1 first-gt1)
                              (%bin c (+ +ctx-coeff-abs-level-greater2-flag+ ctx-set
                                         (if (plusp c-idx) 4 0)))))
                      (let* ((n-signs (if (and (pps-sign-data-hiding pps) sign-hidden)
                                          (1- nsig) nsig))
                             (signs 0))
                        (declare (type fixnum signs))
                        (dotimes (k n-signs) (setf signs (logior (ash signs 1) (%bypass c))))
                        (setf signs (ash signs (- 16 n-signs)))
                        (dotimes (m nsig)
                          (let* ((n (aref sig-idx m))
                                 (level (if (< m 8) (1+ (aref gt1 m)) 1)))
                            (declare (type fixnum n level))
                            (when (or (>= m 8)
                                      (= level (if (= m first-gt1) 3 2)))
                              (let ((rem (%coeff-abs-level-remaining c rice)))
                                (incf level rem)
                                (when (> level (ash 3 rice)) (setf rice (min (1+ rice) 4)))))
                            (when (and (pps-sign-data-hiding pps) sign-hidden)
                              (incf sum level)
                              (when (and (= n first-nz) (oddp sum)) (setf level (- level))))
                            (when (plusp (logand signs #x8000)) (setf level (- level)))
                            (setf signs (logand (ash signs 1) #xffff))
                            (let ((xc (+ (ash xcg 2) (aref off-x n)))
                                  (yc (+ (ash ycg 2) (aref off-y n))))
                              (setf (aref coeffs (+ (* yc size) xc)) level))
                            (incf (cx-n-coeff c)))))))))))
          (incf (cx-n-tu c))
          transform-skip)))))

;;; ---- reconstruction ----------------------------------------------------------------------------

(defun %qp-for (c c-idx)
  "The quantiser this component's residual was scaled by (8.6.1)."
  (let ((sh (cx-sh c)) (pps (cx-pps c)) (qp (cx-qp c)))
    (case c-idx
      (0 qp)
      (1 (%chroma-qp qp (+ (pps-cb-qp-offset pps) (sh-cb-qp-offset sh))))
      (t (%chroma-qp qp (+ (pps-cr-qp-offset pps) (sh-cr-qp-offset sh)))))))

(defun %reconstruct (c x0 y0 log2-size c-idx mode residual-p transform-skip)
  "Predict this transform block, add its residual if it has one, and write it to the picture.

   The prediction reads the picture, the residual reads the bitstream, and neither reads the other
   — so the only ordering that matters is that a block is FINISHED before the next one starts,
   because the next one predicts from these samples."
  (declare (type ctx c) (type fixnum x0 y0 log2-size c-idx mode)
           (optimize (speed 3) (safety 1)))
  (let* ((pic (cx-pic c))
         (n (ash 1 log2-size))
         (pred (cx-pred c))
         (coeffs (cx-coeffs c))
         (plane (pic-plane pic c-idx))
         (stride (pic-stride pic c-idx)))
    (declare (type fixnum n stride))
    (if (cx-cu-intra c)
        (%intra-predict c pic x0 y0 log2-size c-idx mode pred)
        ;; an INTER block's prediction was written straight into the picture by motion
        ;; compensation, before the transform tree was even parsed; the residual is added to it
        (dotimes (y n)
          (dotimes (x n)
            (setf (aref pred (+ (* y n) x))
                  (aref plane (+ (* (+ y0 y) stride) x0 x))))))
    (when residual-p
      (let* ((qp (%qp-for c c-idx))
             ;; the picture parameter set's lists override the sequence's when it sends any
             (sl (and (sps-scaling-list-enabled (cx-sps c))
                      (or (pps-scaling (cx-pps c)) (sps-scaling (cx-sps c)))))
             ;; six matrices: intra Y, Cb, Cr then inter Y, Cb, Cr
             (mid (+ (if (cx-cu-intra c) 0 3) c-idx))
             (matrix (and sl (aref (sl-sl sl) (- log2-size 2) mid)))
             (dc (and sl (>= log2-size 4) (aref (sl-dc sl) (- log2-size 4) mid))))
        (if (cx-cu-transquant-bypass c)
            nil                                 ; the levels ARE the residual, unscaled
            (%dequantise coeffs (* n n) log2-size qp 8 matrix dc))
        (cond ((cx-cu-transquant-bypass c) nil)
              (transform-skip (%transform-skip coeffs (* n n) log2-size 8))
              (t (%inverse-transform coeffs (cx-scratch c) log2-size
                                     ;; the DST is for 4x4 intra LUMA only
                                     (and (= log2-size 2) (zerop c-idx) (cx-cu-intra c))
                                     8)))))
    (dotimes (y n)
      (let ((row (+ (* (+ y0 y) stride) x0)))
        (declare (type fixnum row))
        (dotimes (x n)
          (let ((v (aref pred (+ (* y n) x))))
            (declare (type fixnum v))
            (when residual-p (incf v (aref coeffs (+ (* y n) x))))
            (setf (aref plane (+ row x)) (max 0 (min 255 v)))))))
    (when (zerop c-idx)
      (when residual-p
        (let ((p (cx-pic c)))
          (loop for y of-type fixnum from y0 below (+ y0 n) by 4
                do (loop for x of-type fixnum from x0 below (+ x0 n) by 4
                         do (setf (aref (pic-cbf p) (pic-mv-index p x y)) 1)))))
      (%mark-for-filters c x0 y0 n))))

(defun %mark-inter-blocks (c x0 y0 size intra)
  "Record over a coding unit's 8x8 blocks what the loop filter reads: the quantiser, and whether
   the block is exempt.  An inter unit does this itself because its prediction units write samples
   before any transform block exists to do it for them."
  (declare (type ctx c) (type fixnum x0 y0 size) (ignore intra))
  (let ((pic (cx-pic c))
        (w (pic-blk-w (cx-pic c)))
        (nofilt (if (cx-cu-transquant-bypass c) 1 0)))
    (loop for y of-type fixnum from y0 below (+ y0 size) by 8
          do (loop for x of-type fixnum from x0 below (+ x0 size) by 8
                   do (let ((i (+ (* (ash y -3) w) (ash x -3))))
                        (setf (aref (pic-blk-qp pic) i) (max 0 (min 255 (cx-qp c)))
                              (aref (pic-blk-nofilt pic) i) nofilt))))
    ;; the unit's own outer edges are prediction unit edges
    (when (and (plusp x0) (zerop (logand x0 7)))
      (loop for y of-type fixnum from y0 below (+ y0 size) by 4
            do (let ((i (+ (* (ash y -2) (pic-bs-vw pic)) (ash x0 -3))))
                 (setf (aref (pic-bs-v pic) i) (logior 1 (aref (pic-bs-v pic) i))))))
    (when (and (plusp y0) (zerop (logand y0 7)))
      (loop for x of-type fixnum from x0 below (+ x0 size) by 4
            do (let ((i (+ (* (ash y0 -3) (pic-bs-hw pic)) (ash x -2))))
                 (setf (aref (pic-bs-h pic) i) (logior 1 (aref (pic-bs-h pic) i))))))))

(defun %mark-for-filters (c x0 y0 n)
  "Record what the loop filters will want to know about this luma transform block.

   A boundary strength of 2 means `an intra block is on at least one side', which in an all-intra
   picture is every transform block edge that falls on the eight-sample grid.  The inter cases —
   strength 1 where coefficients or motion differ, 0 where nothing does — arrive with inter
   prediction; until then setting 2 on a transform edge is not a simplification, it is the answer.

   The PICTURE boundary is deliberately not marked.  There is nothing on the other side of it, and
   a filter that reads there reads the row above."
  (declare (type ctx c) (type fixnum x0 y0 n) (optimize (speed 3) (safety 1)))
  (let ((pic (cx-pic c)))
    ;; bit 1 marks a TRANSFORM block edge, which is the only kind at which the coefficient test
    ;; applies; bit 0 marks a prediction unit edge.  The strength itself is derived at filter time,
    ;; because two of the three things it depends on — the motion either side, and whether either
    ;; side carried coefficients — are not known until the whole picture is decoded.
    (when (and (plusp x0) (zerop (logand x0 7)))
      (loop for y of-type fixnum from y0 below (+ y0 n) by 4
            do (let ((i (+ (* (ash y -2) (pic-bs-vw pic)) (ash x0 -3))))
                 (setf (aref (pic-bs-v pic) i) (logior 2 (aref (pic-bs-v pic) i))))))
    (when (and (plusp y0) (zerop (logand y0 7)))
      (loop for x of-type fixnum from x0 below (+ x0 n) by 4
            do (let ((i (+ (* (ash y0 -3) (pic-bs-hw pic)) (ash x -2))))
                 (setf (aref (pic-bs-h pic) i) (logior 2 (aref (pic-bs-h pic) i))))))
    ;; the quantiser, per eight-by-eight block, and whether this block may be filtered at all
    (let ((w (pic-blk-w pic))
          (nofilt (if (cx-cu-transquant-bypass c) 1 0)))
      (loop for y of-type fixnum from (logandc2 y0 7) below (+ y0 n) by 8
            do (loop for x of-type fixnum from (logandc2 x0 7) below (+ x0 n) by 8
                     do (let ((i (+ (* (ash y -3) w) (ash x -3))))
                          (setf (aref (pic-blk-qp pic) i) (max 0 (min 255 (cx-qp c)))
                                (aref (pic-blk-nofilt pic) i) nofilt)))))))

;;; ---- the transform tree ------------------------------------------------------------------------

(defun %transform-unit (c x0 y0 xb yb log2-size depth blk-idx cbf-luma cbf-cb cbf-cr)
  (declare (type fixnum x0 y0 xb yb log2-size depth blk-idx))
  (let* ((pps (cx-pps c))
         ;; At 4x4 the chroma flags handed down are the PARENT's, and they count for all four
         ;; siblings — not only for the one that carries the chroma residual.  Whether a quantiser
         ;; delta is sent depends on this, so restricting it to blkIdx 3 loses bits on the other
         ;; three blocks rather than merely mislabelling them.
         (cbf-chroma (or (plusp cbf-cb) (plusp cbf-cr)))
         ;; chroma at 4x4 covers the PARENT's area, and is sent once with the last sibling
         (chroma-here (or (> log2-size 2) (= blk-idx 3)))
         (clog2 (if (> log2-size 2) (1- log2-size) 2))
         (cx0 (ash (if (> log2-size 2) x0 xb) -1))
         (cy0 (ash (if (> log2-size 2) y0 yb) -1))
         (cmode (%chroma-intra-mode (%intra-mode-at c (cx-cu-x c) (cx-cu-y c))
                                    (cx-chroma-idx c))))
    (when (and (or (plusp cbf-luma) cbf-chroma)
               (pps-cu-qp-delta-enabled pps) (not (cx-qp-delta-coded c)))
      (%cu-qp-delta c))
    ;; luma
    (let ((skip (and (plusp cbf-luma)
                     (%residual-coding c x0 y0 log2-size 0 (%intra-mode-at c x0 y0)))))
      (%reconstruct c x0 y0 log2-size 0 (%intra-mode-at c x0 y0) (plusp cbf-luma) skip))
    ;; chroma, both components, in the order the bitstream sends them
    (when chroma-here
      (let ((skip (and (plusp cbf-cb) (%residual-coding c cx0 cy0 clog2 1 cmode))))
        (%reconstruct c cx0 cy0 clog2 1 cmode (plusp cbf-cb) skip))
      (let ((skip (and (plusp cbf-cr) (%residual-coding c cx0 cy0 clog2 2 cmode))))
        (%reconstruct c cx0 cy0 clog2 2 cmode (plusp cbf-cr) skip)))))

(defun %derive-qp (c)
  "Qp'Y for the current coding unit (8.6.1), and record it over the unit's area.

   The quantiser is PREDICTED, from the left and above neighbours of the quantisation group and
   from whichever unit was decoded last, so a delta of zero — which is what almost every unit sends
   — still tracks a picture whose quantiser drifts.  A decoder that simply accumulated the deltas
   would agree with the encoder until the first unit that has a neighbour it does not."
  (let* ((sps (cx-sps c))
         (s (sps-min-cb-log2 sps))
         (w (cx-min-cb-width c))
         (prev (cx-qp-y-prev c))
         (xq (cx-qg-x c)) (yq (cx-qg-y c))
         (ctb-mask (- (ash 1 (sps-ctb-log2 sps))))
         ;; a neighbour outside the current coding tree block does not count: the standard would
         ;; otherwise need a picture-wide row of quantisers kept live
         (a (if (and (plusp xq) (%gc c (1- xq) yq)
                     (= (logand (1- xq) ctb-mask) (logand xq ctb-mask))
                     (= (logand yq ctb-mask) (logand yq ctb-mask)))
                (aref (cx-qp-y c) (+ (* (ash yq (- s)) w) (ash (1- xq) (- s))))
                prev))
         (b (if (and (plusp yq) (%gc c xq (1- yq))
                     (= (logand (1- yq) ctb-mask) (logand yq ctb-mask)))
                (aref (cx-qp-y c) (+ (* (ash (1- yq) (- s)) w) (ash xq (- s))))
                prev))
         (qp (mod (+ (ash (+ a b 1) -1) (cx-cu-qp-delta c) 52) 52)))
    (setf (cx-qp c) qp)
    (let ((n (ash (ash 1 (cx-cu-log2 c)) (- s))))
      (dotimes (j n)
        (dotimes (i n)
          (setf (aref (cx-qp-y c)
                      (+ (* (+ (ash (cx-cu-y c) (- s)) j) w) (ash (cx-cu-x c) (- s)) i))
                qp))))
    qp))

(defun %cu-qp-delta (c)
  "cu_qp_delta_abs and its sign: five context-coded bins then an Exp-Golomb escape."
  (let ((v (let ((n 0))
             (loop while (and (< n 5)
                              (= 1 (%bin c (+ +ctx-cu-qp-delta+ (if (zerop n) 0 1)))))
                   do (incf n))
             n)))
    (when (= v 5) (incf v (%egk-bypass (cx-cabac c) 0)))
    (when (plusp v)
      (when (= 1 (%bypass c)) (setf v (- v))))
    (setf (cx-qp-delta-coded c) t
          (cx-cu-qp-delta c) v)
    (%derive-qp c)
    v))

(defun %transform-tree (c x0 y0 xb yb log2-size depth blk-idx parent-cb parent-cr)
  "transform_tree (7.3.8.8): a second quadtree, of transform blocks, inside a coding unit.

   The chroma coded-block flags are sent at every level and are CONDITIONAL ON THE PARENT'S: a
   sub-tree of a block that had no chroma residual cannot acquire one, so the flag is only sent
   where it could be either way.  And chroma stops subdividing at 4x4 — below that the four luma
   blocks share one chroma pair, which is why the leaf has to know its index among its siblings."
  (declare (type fixnum x0 y0 xb yb log2-size depth blk-idx))
  (let* ((sps (cx-sps c))
         ;; interSplitFlag: an inter unit whose transform hierarchy is one level deep and whose
         ;; prediction is SPLIT must still cut its transform to match, because a transform block
         ;; may not straddle two prediction units with different motion.  No flag is sent for it,
         ;; and a decoder that reads one where the encoder sent none loses the slice.
         (inter-split (and (not (cx-cu-intra c))
                           (zerop (sps-max-transform-depth-inter sps))
                           (/= (cx-part-mode c) 0)
                           (zerop depth)))
         (split (cond ((> log2-size (sps-max-tb-log2 sps)) 1)
                      ((and (cx-intra-split c) (zerop depth)) 1)
                      (inter-split 1)
                      ((and (<= log2-size (sps-max-tb-log2 sps))
                            (> log2-size (sps-min-tb-log2 sps))
                            (< depth (cx-max-trafo-depth c)))
                       (%bin c (+ +ctx-split-transform-flag+ (- 5 log2-size))))
                      (t 0)))
         (cbf-cb 0) (cbf-cr 0))
    (declare (type fixnum split cbf-cb cbf-cr))
    (when (> log2-size 2)
      (when (or (zerop depth) (plusp parent-cb))
        (setf cbf-cb (%bin c (+ +ctx-cbf-cb-cr+ depth))))
      (when (or (zerop depth) (plusp parent-cr))
        (setf cbf-cr (%bin c (+ +ctx-cbf-cb-cr+ depth)))))
    (when (= log2-size 2)
      ;; at 4x4 the flags are not sent; the parent's stand for all four siblings
      (setf cbf-cb parent-cb cbf-cr parent-cr))
    (if (plusp split)
        (let* ((half (ash 1 (1- log2-size)))
               (x1 (+ x0 half)) (y1 (+ y0 half)))
          (%transform-tree c x0 y0 x0 y0 (1- log2-size) (1+ depth) 0 cbf-cb cbf-cr)
          (%transform-tree c x1 y0 x0 y0 (1- log2-size) (1+ depth) 1 cbf-cb cbf-cr)
          (%transform-tree c x0 y1 x0 y0 (1- log2-size) (1+ depth) 2 cbf-cb cbf-cr)
          (%transform-tree c x1 y1 x0 y0 (1- log2-size) (1+ depth) 3 cbf-cb cbf-cr))
        (let ((cbf-luma 1))
          ;; an intra block always has its luma flag sent; an inter one at depth 0 with no chroma
          ;; residual is inferred to have luma, since it would otherwise have said so higher up
          (when (or (cx-cu-intra c) (plusp depth) (plusp cbf-cb) (plusp cbf-cr))
            (setf cbf-luma (%bin c (+ +ctx-cbf-luma+ (if (zerop depth) 1 0)))))
          (%transform-unit c x0 y0 xb yb log2-size depth blk-idx cbf-luma cbf-cb cbf-cr)))))

;;; ---- the coding unit ---------------------------------------------------------------------------

(defun cx-chroma-idx (c) (cx-%chroma-idx c))

(defun %coding-unit (c x0 y0 log2-size)
  "coding_unit (7.3.8.5), for an intra slice.

   Everything an intra coding unit says: whether it bypasses the transform entirely, whether it is
   one prediction block or four, its luma modes, its chroma mode, and then the transform tree."
  (declare (type fixnum x0 y0 log2-size))
  (let* ((sps (cx-sps c))
         (pps (cx-pps c))
         (size (ash 1 log2-size)))
    (setf (cx-cu-x c) x0 (cx-cu-y c) y0 (cx-cu-log2 c) log2-size
          (cx-cu-intra c) t
          (cx-cu-transquant-bypass c) nil
          (cx-part-mode c) 0
          (cx-intra-split c) nil)
    (incf (cx-n-cu c))
    ;; cu_transquant_bypass_flag comes BEFORE cu_skip_flag (7.3.8.5).  Two single-bin flags on
    ;; different contexts: swapping them reads the same number of bits, so a stream that never
    ;; enables the bypass hides the mistake completely, and one that does adapts both contexts
    ;; wrongly from its first coding unit onward.
    (when (pps-transquant-bypass pps)
      (setf (cx-cu-transquant-bypass c) (= 1 (%bin c +ctx-cu-transquant-bypass-flag+))))
    ;; ---- a SKIPPED unit: one merged prediction, no residual, nothing else to read
    (when (and (not (sh-i-slice-p (cx-sh c))) (= 1 (%cu-skip-flag c x0 y0)))
      (setf (cx-cu-intra c) nil)
      (let ((s (sps-min-cb-log2 sps)) (w (cx-min-cb-width c)))
        (dotimes (j (ash size (- s)))
          (dotimes (i (ash size (- s)))
            (setf (aref (cx-skip c) (+ (* (+ (ash y0 (- s)) j) w) (ash x0 (- s)) i)) 1))))
      (%derive-qp c)
      (%prediction-units c x0 y0 size 0 t)
      (%mark-inter-blocks c x0 y0 size nil)
      (return-from %coding-unit 0))
    ;; pred_mode_flag: 1 is MODE_INTRA, 0 is MODE_INTER.  Reading it the other way round is not a
    ;; parse error anywhere — both branches are valid syntax — so the slice decodes to the end of
    ;; something plausible and only the picture is wrong.
    (unless (sh-i-slice-p (cx-sh c))
      (setf (cx-cu-intra c) (= 1 (%bin c +ctx-pred-mode-flag+))))
    ;; part_mode is asked of every inter unit, and of an intra one only at the smallest size
    (when (or (not (cx-cu-intra c)) (= log2-size (sps-min-cb-log2 sps)))
      (setf (cx-part-mode c) (%part-mode c log2-size (cx-cu-intra c))))
    (setf (cx-intra-split c) (and (cx-cu-intra c) (= (cx-part-mode c) 3)))
    (unless (cx-cu-intra c)
      (%derive-qp c)
      (%prediction-units c x0 y0 size (cx-part-mode c) nil)
      (%mark-inter-blocks c x0 y0 size nil)
      (let ((root-cbf (if (and (= (cx-part-mode c) 0) (cx-merge-2nx2n c))
                          1
                          (%bin c +ctx-no-residual-data-flag+))))
        (when (plusp root-cbf)
          (setf (cx-max-trafo-depth c) (sps-max-transform-depth-inter sps))
          (%transform-tree c x0 y0 x0 y0 log2-size 0 0 1 1)))
      (return-from %coding-unit 0))
    ;; record MODE_INTRA over the whole coding unit before the modes are derived, because a later
    ;; block in the same unit asks — and again in the picture's motion field, which is where a
    ;; LATER PICTURE asks when it looks for a collocated candidate
    (let ((s (sps-min-cb-log2 sps)) (w (cx-min-cb-width c)))
      (dotimes (j (ash size (- s)))
        (dotimes (i (ash size (- s)))
          (setf (aref (cx-is-intra c) (+ (* (+ (ash y0 (- s)) j) w) (ash x0 (- s)) i)) 1))))
    (let ((pic (cx-pic c)))
      (dotimes (j (ash size -2))
        (dotimes (i (ash size -2))
          (setf (aref (pic-intra pic) (pic-mv-index pic (+ x0 (* 4 i)) (+ y0 (* 4 j)))) 1))))
    (when (and (sps-pcm-enabled sps) (= (cx-part-mode c) 0)
               (>= log2-size (sps-pcm-min-cb-log2 sps))
               (<= log2-size (sps-pcm-max-cb-log2 sps)))
      (when (= 1 (decode-terminate (cx-cabac c)))
        (%err "PCM coding units are not supported")))
    (let* ((n (if (= (cx-part-mode c) 3) 2 1))
           (pb (ash size (if (= n 2) -1 0)))
           (prev (make-array 4)) (mpm (make-array 4 :initial-element 0))
           (rem (make-array 4 :initial-element 0)))
      ;; all the flags first, then all the indices — the standard interleaves them that way so a
      ;; decoder can read the four flags as one run of the same context
      (dotimes (k (* n n))
        (setf (aref prev k) (= 1 (%bin c +ctx-prev-intra-luma-pred-flag+))))
      (dotimes (k (* n n))
        (if (aref prev k)
            (setf (aref mpm k) (let ((i 0))
                                 (loop while (and (< i 2) (= 1 (%bypass c))) do (incf i))
                                 i))
            (setf (aref rem k) (let ((v 0))
                                 (dotimes (i 5 v) (setf v (logior (ash v 1) (%bypass c))))))))
      (dotimes (j n)
        (dotimes (i n)
          (let* ((k (+ (* j n) i))
                 (px (+ x0 (* i pb))) (py (+ y0 (* j pb)))
                 (mode (%luma-intra-mode c px py (aref prev k) (aref mpm k) (aref rem k))))
            (%set-intra-mode c px py pb mode))))
      (setf (cx-%chroma-idx c) (%intra-chroma-pred-mode c)))
    (setf (cx-max-trafo-depth c)
          (+ (sps-max-transform-depth-intra sps) (if (cx-intra-split c) 1 0)))
    (%derive-qp c)
    (%transform-tree c x0 y0 x0 y0 log2-size 0 0 1 1)))

;;; ---- the coding quadtree -----------------------------------------------------------------------

(defun %coding-quadtree (c x0 y0 log2-size depth)
  (declare (type fixnum x0 y0 log2-size depth))
  (let* ((sps (cx-sps c))
         (pps (cx-pps c))
         (size (ash 1 log2-size))
         (fits (and (<= (+ x0 size) (sps-width sps)) (<= (+ y0 size) (sps-height sps))))
         (split (cond ((and fits (> log2-size (sps-min-cb-log2 sps)))
                       (= 1 (%split-cu-flag c x0 y0 depth)))
                      ;; a block that hangs off the edge of the picture is split without being told
                      ((not fits) (> log2-size (sps-min-cb-log2 sps)))
                      (t nil))))
    (when (and (pps-cu-qp-delta-enabled pps)
               (>= log2-size (- (sps-ctb-log2 sps) (pps-diff-cu-qp-delta-depth pps))))
      (setf (cx-qp-delta-coded c) nil
            (cx-cu-qp-delta c) 0
            (cx-qp-y-prev c) (cx-qp c)
            (cx-qg-x c) x0 (cx-qg-y c) y0))
    (if split
        (let* ((half (ash 1 (1- log2-size)))
               (x1 (+ x0 half)) (y1 (+ y0 half)))
          (%coding-quadtree c x0 y0 (1- log2-size) (1+ depth))
          (when (< x1 (sps-width sps)) (%coding-quadtree c x1 y0 (1- log2-size) (1+ depth)))
          (when (< y1 (sps-height sps)) (%coding-quadtree c x0 y1 (1- log2-size) (1+ depth)))
          (when (and (< x1 (sps-width sps)) (< y1 (sps-height sps)))
            (%coding-quadtree c x1 y1 (1- log2-size) (1+ depth))))
        (progn
          ;; record the depth over this whole block: split_cu_flag's context reads it
          (let ((s (sps-min-cb-log2 sps)) (w (cx-min-cb-width c)))
            (dotimes (j (ash size (- s)))
              (dotimes (i (ash size (- s)))
                (setf (aref (cx-ct-depth c) (+ (* (+ (ash y0 (- s)) j) w) (ash x0 (- s)) i))
                      depth))))
          (setf (cx-cu-depth c) depth)
          (%coding-unit c x0 y0 log2-size)))))

;;; ---- the slice ---------------------------------------------------------------------------------

(defun %sao (c addr rx ry)
  "sample_adaptive_offset (7.3.8.3): read and set aside for now.

   SAO is a post-filter with no counterpart in H.264 — a per-CTB table of offsets applied after
   deblocking, either by intensity band or by comparing each sample against two neighbours.  Its
   syntax has to be read whether or not anything is done with it, because every bit after it in the
   slice depends on how many bits it took.

   THE FOUR OFFSETS COME BEFORE THE FOUR SIGNS.  They are two separate loops in the standard, and
   writing them as one loop of \"magnitude then sign\" costs nothing until a coding tree unit
   actually uses band offset with more than one non-zero magnitude — at which point the arithmetic
   decoder is reading sign bits where magnitudes are and the slice is lost.  Edge offset has no
   signs at all, so a stream that only ever uses edge offset hides the mistake completely."
  (let ((sh (cx-sh c)) (merge-left nil) (merge-up nil)
        (wide (sps-ctbs-wide (cx-sps c)))
        (first-addr (cx-slice-addr c)))
    (when (or (sh-sao-luma sh) (sh-sao-chroma sh))
      ;; a neighbour outside this slice segment cannot be merged with, so the flag is not sent
      (when (and (plusp rx) (> addr first-addr))
        (setf merge-left (= 1 (%bin c +ctx-sao-merge-flag+))))
      (when (and (plusp ry) (not merge-left) (>= (- addr wide) first-addr))
        (setf merge-up (= 1 (%bin c +ctx-sao-merge-flag+))))
      (let ((pic (cx-pic c)))
        (cond
          ;; merging copies the neighbour's whole table, which is most of why SAO is cheap
          (merge-left (%sao-copy pic addr (1- addr)))
          (merge-up (%sao-copy pic addr (- addr wide)))
          (t
           (dotimes (comp 3)
             (when (if (zerop comp) (sh-sao-luma sh) (sh-sao-chroma sh))
               (let ((type (if (< comp 2)
                               (let ((b (%bin c +ctx-sao-type-idx+)))
                                 (if (zerop b) 0 (if (= 1 (%bypass c)) 2 1)))
                               ;; the two chroma components share one type, sent with Cb
                               (cx-sao-type-cr c))))
                 (when (= comp 1) (setf (cx-sao-type-cr c) type))
                 (setf (aref (pic-sao-type pic) (+ (* 3 addr) comp)) type)
                 (unless (zerop type)
                   (let ((abs (make-array 4 :element-type 'fixnum)))
                     (declare (dynamic-extent abs))
                     ;; sao_offset_abs: truncated Rice, cMax = (1 << (min(bitDepth,10) - 5)) - 1
                     (dotimes (i 4)
                       (let ((v 0))
                         (loop while (and (< v 7) (= 1 (%bypass c))) do (incf v))
                         (setf (aref abs i) v)))
                     (if (= type 1)
                         (progn
                           ;; band offset: a sign for each magnitude that is not zero, then which
                           ;; four adjacent intensity bands they apply to
                           (dotimes (i 4)
                             (when (and (plusp (aref abs i)) (= 1 (%bypass c)))
                               (setf (aref abs i) (- (aref abs i)))))
                           (setf (aref (pic-sao-param pic) (+ (* 3 addr) comp))
                                 (let ((v 0)) (dotimes (i 5 v) (setf v (logior (ash v 1)
                                                                               (%bypass c)))))))
                         ;; edge offset: the signs are implied by the direction being tested —
                         ;; the first two offsets are positive and the last two negative, because
                         ;; the four cases they cover are a valley, a step up, a step down and a peak
                         (progn
                           (setf (aref abs 2) (- (aref abs 2))
                                 (aref abs 3) (- (aref abs 3)))
                           (when (< comp 2)
                             (setf (aref (pic-sao-param pic) (+ (* 3 addr) comp))
                                   (logior (ash (%bypass c) 1) (%bypass c))))
                           (when (= comp 2)
                             (setf (aref (pic-sao-param pic) (+ (* 3 addr) 2))
                                   (aref (pic-sao-param pic) (+ (* 3 addr) 1))))))
                     (dotimes (i 4)
                       (setf (aref (pic-sao-off pic) (+ (* 12 addr) (* 4 comp) i))
                             (aref abs i)))))))))))) 
    nil))

(defun %sao-copy (pic to from)
  "sao_merge: this coding tree block uses its neighbour's table verbatim."
  (dotimes (comp 3)
    (setf (aref (pic-sao-type pic) (+ (* 3 to) comp))
          (aref (pic-sao-type pic) (+ (* 3 from) comp))
          (aref (pic-sao-param pic) (+ (* 3 to) comp))
          (aref (pic-sao-param pic) (+ (* 3 from) comp)))
    (dotimes (i 4)
      (setf (aref (pic-sao-off pic) (+ (* 12 to) (* 4 comp) i))
            (aref (pic-sao-off pic) (+ (* 12 from) (* 4 comp) i))))))

(defun decode-slice-data (sps pps sh br &optional pic list0 list1 collocated)
  "Walk one independent slice segment's coding tree units.

   Returns the context, whose counters are what stands in for a picture until reconstruction
   exists.  The check that matters is that end_of_slice_segment_flag comes up exactly at the last
   coding tree unit of the slice: a syntax error anywhere desynchronises the arithmetic decoder,
   and a desynchronised decoder does not stop in the right place."
  (when (or (pps-tiles-enabled pps) (pps-entropy-coding-sync pps))
    (%err "tiles and wavefront entropy coding are not supported"))
  (when (sh-dependent sh)
    (%err "dependent slice segments are not supported"))
  (when (and (not (sh-i-slice-p sh)) (zerop (length (or list0 #()))))
    (%err "an inter slice with no reference pictures"))
  (let* ((cab (init-cabac br (sh-qp sh) (sh-type sh) (sh-cabac-init sh)))
         (c (%make-ctx sps pps sh cab))
         (pic (or pic (make-picture-for sps)))
         (wide (sps-ctbs-wide sps))
         (total (sps-ctbs sps))
         (log2 (sps-ctb-log2 sps)))
    (setf (cx-slice-addr c) (sh-segment-address sh)
          (cx-pic c) pic
          (cx-list0 c) (or list0 #()) (cx-list1 c) (or list1 #())
          (cx-collocated c) collocated
          (cx-qg-x c) 0 (cx-qg-y c) 0)
    (loop for addr of-type fixnum from (sh-segment-address sh) below total do
      (let ((rx (mod addr wide)) (ry (floor addr wide)))
        (setf (aref (cx-ctb-slice c) addr) (cx-slice-addr c)
              (aref (pic-ctb-slice pic) addr) (cx-slice-addr c)
              (aref (pic-ctb-across pic) addr) (if (sh-loop-filter-across-slices sh) 1 0)
              ;; the two offsets are per slice and signed, packed with the disable flag so the
              ;; filter can read one number per coding tree block
              (aref (pic-ctb-dbf pic) addr)
              (logior (if (sh-deblocking-disabled sh) 1 0)
                      (ash (+ 32 (sh-beta-offset sh)) 8)
                      (ash (+ 32 (sh-tc-offset sh)) 16)))
        (setf (cx-qp-delta-coded c) nil)
        (%sao c addr rx ry)
        (%coding-quadtree c (ash rx log2) (ash ry log2) log2 0)
        (when (= 1 (decode-terminate cab))
          ;; T: the slice said it was finished.  A slice segment covers only its own run of coding
          ;; tree units, so how many that is says nothing about whether the parse was right —
          ;; whether it STOPPED ON ITS OWN, rather than by running out of picture, is what does.
          (return-from decode-slice-data
            (values c (1+ (- addr (sh-segment-address sh))) t)))))
    (values c (- total (sh-segment-address sh)) nil)))

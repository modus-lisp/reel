;;;; hevc/intra.lisp — intra prediction (8.4.4.2).
;;;;
;;;; Thirty-five modes against H.264's nine: planar, DC, and thirty-three angular directions.  The
;;;; directions are not evenly spaced in angle — they are evenly spaced in TANGENT, in thirty-seconds
;;;; of a sample per row — which is what lets every one of them be computed with the same two-tap
;;;; interpolation and no trigonometry anywhere.
;;;;
;;;; Three things here have no counterpart in H.264 and each is load-bearing:
;;;;
;;;;   * REFERENCE SUBSTITUTION.  H.264 forbids a mode whose neighbours are missing.  HEVC allows
;;;;     every mode everywhere and fills the missing samples in first, by propagating the nearest
;;;;     available one anticlockwise around the block.  So availability changes the samples rather
;;;;     than the choice of mode, and getting the propagation order wrong is silently wrong.
;;;;   * FILTERING THE REFERENCES.  A smoothing filter runs over the neighbours before prediction,
;;;;     for the modes and sizes where a sharp reference would show as a visible line.  Whether it
;;;;     runs is derived, never signalled.
;;;;   * THE BOUNDARY ADJUSTMENT for DC and for exactly vertical and horizontal, which pulls the
;;;;     first row or column back towards the neighbours it was predicted from.  It applies only to
;;;;     luma and only below 32x32.

(in-package #:reel.hevc)

(defparameter +intra-pred-angle+
  ;; Table 8-4, indexed by predModeIntra 2..34: the step in thirty-seconds of a sample per row
  (make-array 35 :element-type '(signed-byte 32)
              :initial-contents '(0 0
                                  32 26 21 17 13 9 5 2 0 -2 -5 -9 -13 -17 -21 -26
                                  -32 -26 -21 -17 -13 -9 -5 -2 0 2 5 9 13 17 21 26 32))
  "intraPredAngle for each prediction mode; entries 0 and 1 are planar and DC and are unused.")
(declaim (type (simple-array (signed-byte 32) (35)) +intra-pred-angle+))

(defparameter +inv-angle+
  ;; Table 8-5, for the modes with a negative angle: 8192 divided by the angle, used to project the
  ;; far side's samples onto the near side's reference line
  (make-array 35 :element-type '(signed-byte 32)
              :initial-contents '(0 0 0 0 0 0 0 0 0 0 0
                                  -4096 -1638 -910 -630 -482 -390 -315 -256
                                  -315 -390 -482 -630 -910 -1638 -4096
                                  0 0 0 0 0 0 0 0 0))
  "invAngle for predModeIntra 11..25.")
(declaim (type (simple-array (signed-byte 32) (35)) +inv-angle+))

;;; ---- availability -------------------------------------------------------------------------------

(defun %build-z-scan (sps)
  "MinTbAddrZs (6.5.2): the z-scan address of each smallest transform block.

   Availability is a question about DECODING ORDER, and in a quadtree that order is not raster.  A
   block's above-right neighbour may be decoded or not depending on where both sit in the tree, and
   comparing z-scan addresses is how the standard settles it without walking the tree again."
  (let* ((ctb-log2 (sps-ctb-log2 sps))
         (tb-log2 (sps-min-tb-log2 sps))
         (w (ash (sps-width sps) (- tb-log2)))
         (h (ash (sps-height sps) (- tb-log2)))
         (ctbs-wide (sps-ctbs-wide sps))
         (depth (- ctb-log2 tb-log2))
         (z (make-array (* w h) :element-type '(signed-byte 32))))
    (dotimes (y h z)
      (dotimes (x w)
        (let* ((tbx (ash (ash x tb-log2) (- ctb-log2)))
               (tby (ash (ash y tb-log2) (- ctb-log2)))
               (ctb (+ (* ctbs-wide tby) tbx))
               (p 0))
          ;; within a coding tree block the address interleaves the bits of x and y, which is what
          ;; z order IS: each level of the quadtree contributes one bit of each
          (dotimes (i depth)
            (let ((m (ash 1 i)))
              (incf p (+ (if (logtest m x) (* m m) 0)
                         (if (logtest m y) (* 2 m m) 0)))))
          (setf (aref z (+ (* y w) x)) (+ (ash ctb (* 2 depth)) p)))))))

(defun %available-p (c xc yc xn yn)
  "Is the luma sample at (XN,YN) available to a block whose top-left luma sample is (XC,YC)?

   Inside the picture, decoded before us in z-scan order, and in the same slice.  Under
   constrained_intra_pred an inter-coded neighbour is unavailable too, which is the same error
   resilience argument H.264 makes and the same trap: it must not be applied anywhere but here."
  (declare (type ctx c) (type fixnum xc yc xn yn))
  (let ((sps (cx-sps c)))
    (and (>= xn 0) (>= yn 0) (< xn (sps-width sps)) (< yn (sps-height sps))
         (let* ((s (sps-min-tb-log2 sps))
                (zw (cx-z-width c))
                (za (aref (cx-zscan c) (+ (* (ash yn (- s)) zw) (ash xn (- s)))))
                (zc (aref (cx-zscan c) (+ (* (ash yc (- s)) zw) (ash xc (- s))))))
           (and (<= za zc)
                (%gc c xn yn)                   ; and in this slice segment
                (or (not (pps-constrained-intra (cx-pps c)))
                    (%cu-intra-p c xn yn)))))))

;;; ---- the reference samples ----------------------------------------------------------------------

(defun %gather-references (c pic x0 y0 log2-size c-idx top left)
  "Collect p[-1][-1], p[x][-1] and p[-1][y] into LEFT and TOP, substituting what is missing.

   TOP holds 2*nTbS+1 samples with the corner at index 0; LEFT the same.  Returns NIL when nothing
   at all was available, in which case the caller uses the mid-grey the standard specifies.

   The substitution walks from the BOTTOM of the left column, up it, round the corner and along the
   top — anticlockwise — because that is the order in which a missing run can be filled from the
   sample before it.  Any other order leaves a hole or fills it from the wrong side."
  (declare (type ctx c) (type fixnum x0 y0 log2-size c-idx)
           (type (simple-array (unsigned-byte 8) (*)) top left)
           (optimize (speed 3) (safety 1)))
  (let* ((n (ash 1 log2-size))
         (n2 (* 2 n))
         (sub-w (if (zerop c-idx) 1 (sps-sub-width (cx-sps c))))
         (sub-h (if (zerop c-idx) 1 (sps-sub-height (cx-sps c))))
         (plane (pic-plane pic c-idx))
         (stride (pic-stride pic c-idx))
         (xl (* x0 sub-w)) (yl (* y0 sub-h))     ; the same corner in LUMA coordinates
         (avail-top (make-array (1+ n2) :element-type 'bit :initial-element 0))
         (avail-left (make-array (1+ n2) :element-type 'bit :initial-element 0))
         (any nil))
    (declare (dynamic-extent avail-top avail-left) (type fixnum n n2))
    (flet ((take (dx dy)
             ;; DX,DY are in this component's sample units, relative to the block's corner
             (when (%available-p c xl yl (+ xl (* dx sub-w)) (+ yl (* dy sub-h)))
               (setf any t)
               (aref plane (+ (* (+ y0 dy) stride) (+ x0 dx))))))
      (let ((corner (take -1 -1)))
        (when corner (setf (aref top 0) corner (aref left 0) corner
                           (aref avail-top 0) 1 (aref avail-left 0) 1)))
      (dotimes (i n2)
        (let ((v (take i -1)))
          (when v (setf (aref top (1+ i)) v (aref avail-top (1+ i)) 1)))
        (let ((v (take -1 i)))
          (when v (setf (aref left (1+ i)) v (aref avail-left (1+ i)) 1)))))
    (unless any (return-from %gather-references nil))
    ;; ---- substitution (8.4.4.2.2), anticlockwise from the bottom of the left column
    (when (zerop (aref avail-left n2))
      ;; the first available sample anywhere, searched in that same order
      (let ((fill nil))
        (loop for i from n2 downto 1
              until fill
              do (when (plusp (aref avail-left i)) (setf fill (aref left i))))
        (unless fill (when (plusp (aref avail-left 0)) (setf fill (aref left 0))))
        (unless fill
          (loop for i from 1 to n2
                until fill
                do (when (plusp (aref avail-top i)) (setf fill (aref top i)))))
        (setf (aref left n2) fill (aref avail-left n2) 1)))
    (loop for i from (1- n2) downto 0
          do (when (zerop (aref avail-left i))
               (setf (aref left i) (aref left (1+ i)) (aref avail-left i) 1)))
    (setf (aref top 0) (aref left 0) (aref avail-top 0) 1)
    (loop for i from 1 to n2
          do (when (zerop (aref avail-top i))
               (setf (aref top i) (aref top (1- i)) (aref avail-top i) 1)))
    t))

(defun %filter-references (c log2-size c-idx mode top left)
  "Smooth the reference samples, when the mode and size call for it (8.4.4.2.3).

   The test is a distance in MODES from exactly vertical or exactly horizontal, against a threshold
   that falls as the block grows: a big block predicted at a shallow angle shows the steps in its
   reference line, a small one does not.  Nothing is signalled — both ends derive it."
  (declare (type ctx c) (type fixnum log2-size c-idx mode)
           (type (simple-array (unsigned-byte 8) (*)) top left)
           (optimize (speed 3) (safety 1)))
  (let ((n (ash 1 log2-size)))
    (declare (type fixnum n))
    (when (or (plusp c-idx) (= mode 1) (= n 4))
      (return-from %filter-references nil))
    (let* ((dist (min (abs (- mode 26)) (abs (- mode 10))))
           (threshold (case n (8 7) (16 1) (32 0) (t 100))))
      (declare (type fixnum dist threshold))
      (unless (> dist threshold) (return-from %filter-references nil))
      (let ((n2 (* 2 n)))
        (declare (type fixnum n2))
        (if (and (sps-strong-intra-smoothing (cx-sps c))
                 (= n 32)
                 (< (abs (- (+ (aref top 0) (aref top n2)) (* 2 (aref top n)))) 8)
                 (< (abs (- (+ (aref left 0) (aref left n2)) (* 2 (aref left n)))) 8))
            ;; STRONG SMOOTHING: the reference line is already nearly straight, so replace it with
            ;; the straight line exactly.  Filtering a gradient with [1 2 1] leaves a faint stair
            ;; that a 32x32 flat area shows as banding; this is the fix for that and nothing else.
            (let ((c0 (aref top 0)) (ct (aref top n2)) (cl (aref left n2)))
              (declare (type fixnum c0 ct cl))
              ;; a straight line from the corner to each far end, in 64 steps — the reference line
              ;; is 2*32 samples long, so index i weighs (64 - i) of the corner against i of the end
              (loop for i from 1 below n2
                    do (setf (aref top i) (ash (+ (* (- 64 i) c0) (* i ct) 32) -6)
                             (aref left i) (ash (+ (* (- 64 i) c0) (* i cl) 32) -6))))
            (let ((ft (make-array (1+ n2) :element-type '(unsigned-byte 8)))
                  (fl (make-array (1+ n2) :element-type '(unsigned-byte 8))))
              (declare (dynamic-extent ft fl))
              (setf (aref ft 0) (ash (+ (aref left 1) (* 2 (aref top 0)) (aref top 1) 2) -2))
              (loop for i from 1 below n2
                    do (setf (aref ft i)
                             (ash (+ (aref top (1- i)) (* 2 (aref top i)) (aref top (1+ i)) 2) -2)
                             (aref fl i)
                             (ash (+ (aref left (1- i)) (* 2 (aref left i)) (aref left (1+ i)) 2)
                                  -2)))
              (setf (aref ft n2) (aref top n2) (aref fl n2) (aref left n2)
                    (aref fl 0) (aref ft 0))
              (replace top ft) (replace left fl)))
        t))))

;;; ---- the modes ----------------------------------------------------------------------------------

(defun %predict-planar (pred n log2-size top left)
  "Planar (8.4.4.2.4): a bilinear surface through the four edges, which is what a smooth gradient
   looks like and what DC cannot express."
  (declare (type (simple-array (signed-byte 32) (*)) pred)
           (type (simple-array (unsigned-byte 8) (*)) top left)
           (type fixnum n log2-size) (optimize (speed 3) (safety 1)))
  (let ((tr (aref top (1+ n)))                  ; p[nTbS][-1]
        (bl (aref left (1+ n))))                ; p[-1][nTbS]
    (declare (type fixnum tr bl))
    (dotimes (y n)
      (dotimes (x n)
        (setf (aref pred (+ (* y n) x))
              (ash (+ (* (- n 1 x) (aref left (1+ y)))
                      (* (1+ x) tr)
                      (* (- n 1 y) (aref top (1+ x)))
                      (* (1+ y) bl)
                      n)
                   (- (1+ log2-size))))))))

(defun %predict-dc (pred n log2-size c-idx top left)
  "DC (8.4.4.2.5), with the boundary smoothing that luma blocks below 32x32 get."
  (declare (type (simple-array (signed-byte 32) (*)) pred)
           (type (simple-array (unsigned-byte 8) (*)) top left)
           (type fixnum n log2-size c-idx) (optimize (speed 3) (safety 1)))
  (let ((sum n))
    (declare (type fixnum sum))
    (dotimes (i n) (incf sum (aref top (1+ i))) (incf sum (aref left (1+ i))))
    (let ((dc (ash sum (- (1+ log2-size)))))
      (declare (type fixnum dc))
      (dotimes (y n) (dotimes (x n) (setf (aref pred (+ (* y n) x)) dc)))
      (when (and (zerop c-idx) (< n 32))
        (setf (aref pred 0) (ash (+ (aref left 1) (* 2 dc) (aref top 1) 2) -2))
        (loop for x from 1 below n
              do (setf (aref pred x) (ash (+ (aref top (1+ x)) (* 3 dc) 2) -2)))
        (loop for y from 1 below n
              do (setf (aref pred (* y n)) (ash (+ (aref left (1+ y)) (* 3 dc) 2) -2)))))))

(defun %predict-angular (pred n c-idx mode top left)
  "The thirty-three angular modes (8.4.4.2.6).

   One reference LINE is built — the top edge for the near-vertical modes, the left for the
   near-horizontal ones — and each row is read from it at a fractional offset that grows by the
   mode's angle.  A negative angle needs samples from the other edge, and invAngle is what projects
   them onto this line; that projection is the part with no counterpart in H.264 at all."
  (declare (type (simple-array (signed-byte 32) (*)) pred)
           (type (simple-array (unsigned-byte 8) (*)) top left)
           (type fixnum n c-idx mode) (optimize (speed 3) (safety 1)))
  (let* ((angle (aref +intra-pred-angle+ mode))
         (vertical (>= mode 18))
         (main (if vertical top left))
         (side (if vertical left top))
         (ref (make-array (+ (* 3 n) 2) :element-type '(signed-byte 32)))
         ;; REF is indexed from -n; this offset makes that an ordinary array index
         (base n))
    (declare (type fixnum angle base) (dynamic-extent ref))
    (macrolet ((r (i) `(aref ref (+ base ,i))))
      (loop for x from 0 to n do (setf (r x) (aref main x)))
      (if (minusp angle)
          (when (< (ash (* n angle) -5) -1)
            (let ((inv (aref +inv-angle+ mode)))
              (declare (type fixnum inv))
              (loop for x from -1 downto (ash (* n angle) -5)
                    do (setf (r x) (aref side (ash (+ (* x inv) 128) -8))))))
          (loop for x from (1+ n) to (* 2 n) do (setf (r x) (aref main x))))
      (dotimes (y n)
        (let* ((pos (* (1+ y) angle))
               (idx (ash pos -5))
               (fact (logand pos 31)))
          (declare (type fixnum pos idx fact))
          (dotimes (x n)
            (let ((v (if (zerop fact)
                         (r (+ x idx 1))
                         (ash (+ (* (- 32 fact) (r (+ x idx 1))) (* fact (r (+ x idx 2))) 16) -5))))
              (declare (type fixnum v))
              (setf (aref pred (if vertical (+ (* y n) x) (+ (* x n) y))) v)))))
      ;; exactly vertical or exactly horizontal: pull the first line back towards the edge it was
      ;; predicted from, which is what stops a visible seam at the block boundary
      (when (and (zerop c-idx) (< n 32) (or (= mode 26) (= mode 10)))
        (let ((corner (aref main 0)))
          (declare (type fixnum corner))
          (dotimes (i n)
            (let* ((at (if (= mode 26) (* i n) i))
                   (v (+ (aref main 1) (ash (- (aref side (1+ i)) corner) -1))))
              (declare (type fixnum v))
              (setf (aref pred at) (max 0 (min 255 v))))))))))

(defun %intra-predict (c pic x0 y0 log2-size c-idx mode pred)
  "Fill PRED, nTbS by nTbS in raster order, with the prediction MODE calls for."
  (declare (type ctx c) (type fixnum x0 y0 log2-size c-idx mode)
           (type (simple-array (signed-byte 32) (*)) pred)
           (optimize (speed 3) (safety 1)))
  (let* ((n (ash 1 log2-size))
         (n2 (* 2 n))
         (top (make-array (1+ n2) :element-type '(unsigned-byte 8) :initial-element 128))
         (left (make-array (1+ n2) :element-type '(unsigned-byte 8) :initial-element 128)))
    (declare (type fixnum n n2) (dynamic-extent top left))
    (when (%gather-references c pic x0 y0 log2-size c-idx top left)
      (%filter-references c log2-size c-idx mode top left))
    (case mode
      (0 (%predict-planar pred n log2-size top left))
      (1 (%predict-dc pred n log2-size c-idx top left))
      (t (%predict-angular pred n c-idx mode top left)))))

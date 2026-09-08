;;;; vp9/loopfilter.lisp — the in-loop deblocking filter.
;;;;
;;;; VP9 filters TRANSFORM BLOCK edges, not macroblock edges — there are no macroblocks — and the
;;;; width of the filter at each edge depends on the transform sizes meeting there: sixteen samples
;;;; across a 32x32 boundary, eight across an 8x8 one, four inside a block of 4x4 transforms.  Which
;;;; edges get which width is worked out WHILE DECODING, as a bitmask per superblock, because by the
;;;; time the filter runs the block structure is gone.
;;;;
;;;; The mask is four bits deep per row of the superblock: one plane each for the sixteen-, eight-
;;;; and four-wide filters, and a fourth for the four-wide edges that fall INSIDE an eight-sample
;;;; step and so cannot be reached by the first three.  Each is a bit per column.  That fourth plane
;;;; is the one that looks redundant and is not: the filter walks eight samples at a time, and a 4x4
;;;; transform has an edge halfway through every step.
;;;;
;;;; THE ORDER IS ALL COLUMNS THEN ALL ROWS, per plane, per superblock.  Samples on a corner are
;;;; filtered twice and the vertical pass sees what the horizontal one left, so the two cannot be
;;;; interleaved and cannot be swapped.

(in-package #:reel.vp9)

(defparameter +wide-filter-col-mask+
  (make-array 2 :element-type '(signed-byte 32) :initial-contents '(#x11 #x01)))
(defparameter +wide-filter-row-mask+
  (make-array 2 :element-type '(signed-byte 32) :initial-contents '(#x03 #x07)))
(defparameter +row-masks+
  (make-array 4 :element-type '(signed-byte 32) :initial-contents '(#xff #x55 #x11 #x01))
  "Which columns of an eight-wide step carry an edge, by transform size.")
(declaim (type (simple-array (signed-byte 32) (2)) +wide-filter-col-mask+ +wide-filter-row-mask+)
         (type (simple-array (signed-byte 32) (4)) +row-masks+))

;;; ---- the two limits, from the level ---------------------------------------------------------------

(defun %filter-luts (sharpness)
  "The two thresholds each filter level implies (6.2.8).

   SHARPNESS narrows the inner limit — it is the encoder saying `filter less across detail' — and it
   is the only knob in VP9's filter that a stream sets globally rather than per block."
  (declare (type fixnum sharpness))
  (let ((lim (make-array 64 :element-type '(signed-byte 32) :initial-element 0))
        (mblim (make-array 64 :element-type '(signed-byte 32) :initial-element 0)))
    (loop for i of-type fixnum from 1 to 63
          do (let ((limit i))
               (declare (type fixnum limit))
               (when (plusp sharpness)
                 (setf limit (ash limit (- (ash (+ sharpness 3) -2))))
                 (setf limit (min limit (- 9 sharpness))))
               (setf limit (max limit 1))
               (setf (aref lim i) limit
                     (aref mblim i) (+ (* 2 (+ i 2)) limit))))
    (values lim mblim)))

;;; ---- the kernel ------------------------------------------------------------------------------------

(declaim (inline %clip7))
(defun %clip7 (v)
  "Clamp to a signed seven-bit range, which is what the specification's `clip to BIT_DEPTH-1' means
   at eight bits."
  (declare (type fixnum v) (optimize (speed 3) (safety 0)))
  (if (< v -128) -128 (if (> v 127) 127 v)))

(defun %lf-line (dst at stridea strideb e i h wd count)
  "COUNT lines across one edge, each independently decided.

   Three widths and three completely different arithmetics.  The narrow one nudges two samples on
   each side by a clipped difference; the eight-wide one replaces six samples with a seven-tap
   average; the sixteen-wide one replaces fourteen with a fifteen-tap average.  Which applies is not
   the caller's choice alone — the caller says how wide it MAY be, and the samples decide, by two
   flatness tests, whether they are smooth enough to deserve it.

   ONE OF THE TWO STRIDES IS ALWAYS ONE — a vertical edge has the samples across it adjacent, a
   horizontal edge is the transpose — and specialising on which, so that the thirty index multiplies
   per line become constant offsets, makes this SLOWER by about three per cent.  Two copies of a
   kernel this size do not fit where one does.  What the indices needed was not to be duplicated but
   to be declared: the THE FIXNUM below is the whole of the win, and it is worth eight per cent."
  (declare (type octets dst) (type fixnum at stridea strideb e i h wd count)
           (optimize (speed 3) (safety 0)))
  (macrolet ((s (k) `(aref dst (the fixnum (+ p (the fixnum (* ,k strideb)))))))
    (dotimes (n count)
      (let ((p (the fixnum (+ at (the fixnum (* n stridea))))))
        (declare (type fixnum p))
        (let ((p3 (s -4)) (p2 (s -3)) (p1 (s -2)) (p0 (s -1))
              (q0 (s 0)) (q1 (s 1)) (q2 (s 2)) (q3 (s 3)))
          (declare (type fixnum p3 p2 p1 p0 q0 q1 q2 q3))
          (when (and (<= (abs (- p3 p2)) i) (<= (abs (- p2 p1)) i)
                     (<= (abs (- p1 p0)) i) (<= (abs (- q1 q0)) i)
                     (<= (abs (- q2 q1)) i) (<= (abs (- q3 q2)) i)
                     (<= (+ (* 2 (abs (- p0 q0))) (ash (abs (- p1 q1)) -1)) e))
            (let ((flat-in (and (>= wd 8)
                                (<= (abs (- p3 p0)) 1) (<= (abs (- p2 p0)) 1)
                                (<= (abs (- p1 p0)) 1) (<= (abs (- q1 q0)) 1)
                                (<= (abs (- q2 q0)) 1) (<= (abs (- q3 q0)) 1)))
                  (flat-out nil)
                  (p7 0) (p6 0) (p5 0) (p4 0) (q4 0) (q5 0) (q6 0) (q7 0))
              (declare (type fixnum p7 p6 p5 p4 q4 q5 q6 q7))
              (when (>= wd 16)
                (setf p7 (s -8) p6 (s -7) p5 (s -6) p4 (s -5)
                      q4 (s 4) q5 (s 5) q6 (s 6) q7 (s 7))
                (setf flat-out (and (<= (abs (- p7 p0)) 1) (<= (abs (- p6 p0)) 1)
                                    (<= (abs (- p5 p0)) 1) (<= (abs (- p4 p0)) 1)
                                    (<= (abs (- q4 q0)) 1) (<= (abs (- q5 q0)) 1)
                                    (<= (abs (- q6 q0)) 1) (<= (abs (- q7 q0)) 1))))
              (cond
                ((and (>= wd 16) flat-out flat-in)
                 (macrolet ((av (&rest xs) `(ash (+ ,@xs 8) -4)))
                   (setf (s -7) (av p7 p7 p7 p7 p7 p7 p7 p6 p6 p5 p4 p3 p2 p1 p0 q0)
                         (s -6) (av p7 p7 p7 p7 p7 p7 p6 p5 p5 p4 p3 p2 p1 p0 q0 q1)
                         (s -5) (av p7 p7 p7 p7 p7 p6 p5 p4 p4 p3 p2 p1 p0 q0 q1 q2)
                         (s -4) (av p7 p7 p7 p7 p6 p5 p4 p3 p3 p2 p1 p0 q0 q1 q2 q3)
                         (s -3) (av p7 p7 p7 p6 p5 p4 p3 p2 p2 p1 p0 q0 q1 q2 q3 q4)
                         (s -2) (av p7 p7 p6 p5 p4 p3 p2 p1 p1 p0 q0 q1 q2 q3 q4 q5)
                         (s -1) (av p7 p6 p5 p4 p3 p2 p1 p0 p0 q0 q1 q2 q3 q4 q5 q6)
                         (s 0) (av p6 p5 p4 p3 p2 p1 p0 q0 q0 q1 q2 q3 q4 q5 q6 q7)
                         (s 1) (av p5 p4 p3 p2 p1 p0 q0 q1 q1 q2 q3 q4 q5 q6 q7 q7)
                         (s 2) (av p4 p3 p2 p1 p0 q0 q1 q2 q2 q3 q4 q5 q6 q7 q7 q7)
                         (s 3) (av p3 p2 p1 p0 q0 q1 q2 q3 q3 q4 q5 q6 q7 q7 q7 q7)
                         (s 4) (av p2 p1 p0 q0 q1 q2 q3 q4 q4 q5 q6 q7 q7 q7 q7 q7)
                         (s 5) (av p1 p0 q0 q1 q2 q3 q4 q5 q5 q6 q7 q7 q7 q7 q7 q7)
                         (s 6) (av p0 q0 q1 q2 q3 q4 q5 q6 q6 q7 q7 q7 q7 q7 q7 q7))))
                ((and (>= wd 8) flat-in)
                 (macrolet ((av (&rest xs) `(ash (+ ,@xs 4) -3)))
                   (setf (s -3) (av p3 p3 p3 p2 p2 p1 p0 q0)
                         (s -2) (av p3 p3 p2 p1 p1 p0 q0 q1)
                         (s -1) (av p3 p2 p1 p0 p0 q0 q1 q2)
                         (s 0) (av p2 p1 p0 q0 q0 q1 q2 q3)
                         (s 1) (av p1 p0 q0 q1 q1 q2 q3 q3)
                         (s 2) (av p0 q0 q1 q2 q2 q3 q3 q3))))
                (t
                 ;; the narrow filter, and the HIGH EDGE VARIANCE test that splits it: where the
                 ;; samples either side of the edge already differ sharply, only the two nearest
                 ;; are touched, because the step is probably real and not a coding artefact
                 (let* ((hev (or (> (abs (- p1 p0)) h) (> (abs (- q1 q0)) h)))
                        (f (if hev
                               (%clip7 (+ (* 3 (- q0 p0)) (%clip7 (- p1 q1))))
                               (%clip7 (* 3 (- q0 p0)))))
                        (f1 (ash (min (+ f 4) 127) -3))
                        (f2 (ash (min (+ f 3) 127) -3)))
                   (declare (type fixnum f f1 f2))
                   (setf (s -1) (%clip8 (+ p0 f2))
                         (s 0) (%clip8 (- q0 f1)))
                   (unless hev
                     (let ((g (ash (1+ f1) -1)))
                       (declare (type fixnum g))
                       (setf (s -2) (%clip8 (+ p1 g))
                             (s 1) (%clip8 (- q1 g)))))))))))))
    (values)))


;;; ---- which edges get filtered, and how wide ------------------------------------------------------

(defun %mask-edges (mask sb uv ss-h ss-v row7 col7 w h col-end row-end tx skip-inter)
  "Record one block's edges in the superblock's mask (6.4.x).

   THE FILTER WORKS ON EIGHT-SAMPLE EDGES, so a chroma plane at half resolution takes two subsampled
   blocks at a time and uses only the top-left one's information.  That is why a block smaller than
   16x16 contributes nothing from its odd half, and why the first thing here is a set of early
   returns that look like they are discarding information.  They are: the format discards it."
  (declare (type (simple-array (signed-byte 32) (* 2 2 8 4)) mask)
           (type fixnum sb uv ss-h ss-v row7 col7 w h col-end row-end tx)
           (optimize (speed 3) (safety 1)))
  (macrolet ((m (dir y k) `(aref mask sb uv ,dir ,y ,k)))
    (when (and (zerop tx) (plusp (logior ss-v ss-h)))
      (when (= h ss-v)
        (when (logbitp 0 row7) (return-from %mask-edges))
        (when (zerop row-end) (incf h)))
      (when (= w ss-h)
        (when (logbitp 0 col7) (return-from %mask-edges))
        (when (zerop col-end) (incf w))))
    (let ((tt (ash 1 col7)))
      (declare (type fixnum tt))
      (let ((m-col (- (ash tt w) tt)))
        (declare (type fixnum m-col))
        (cond
          ((and (zerop tx) (not skip-inter))
           (let* ((m-row-8 (logand m-col (aref +wide-filter-col-mask+ ss-h)))
                  (m-row-4 (- m-col m-row-8)))
             (declare (type fixnum m-row-8 m-row-4))
             ;; `2 - !(y & mask)' in the specification, which is TWO when the test passes and one
             ;; when it does not — the C negation inverts it and reading it as written the other
             ;; way round puts every four-wide horizontal edge in the eight-wide plane.
             (loop for y of-type fixnum from row7 below (+ h row7)
                   do (let ((id (if (logtest y (aref +wide-filter-row-mask+ ss-v)) 2 1)))
                        (declare (type fixnum id))
                        (setf (m 0 y 1) (logior (m 0 y 1) m-row-8)
                              (m 0 y 2) (logior (m 0 y 2) m-row-4))
                        ;; an odd column at the right edge that is skipped takes its odd row with
                        ;; it, which is a quirk of the format and not of this decoder
                        (setf (m 1 y id)
                              (logior (m 1 y id)
                                      (if (and (plusp (logand ss-h ss-v))
                                               (logbitp 0 col-end) (logbitp 0 y))
                                          (- (ash tt (1- w)) tt)
                                          m-col)))
                        (when (zerop ss-h) (setf (m 0 y 3) (logior (m 0 y 3) m-col)))
                        (when (zerop ss-v)
                          (setf (m 1 y 3)
                                (logior (m 1 y 3)
                                        (if (and (plusp ss-h) (logbitp 0 col-end))
                                            (- (ash tt (1- w)) tt)
                                            m-col))))))))
          ((not skip-inter)
           (let* ((id (if (= tx 1) 1 0))
                  (l2 (+ tx ss-h -1))
                  (m-row (logand m-col (aref +row-masks+ l2))))
             (declare (type fixnum id l2 m-row))
             (if (and (plusp ss-h) (> tx 1) (= 1 (logxor w (1- w))))
                 (let* ((m16 (logand (- (ash tt (1- w)) tt) (aref +row-masks+ l2)))
                        (m8 (- m-row m16)))
                   (declare (type fixnum m16 m8))
                   (loop for y of-type fixnum from row7 below (+ h row7)
                         do (setf (m 0 y 0) (logior (m 0 y 0) m16)
                                  (m 0 y 1) (logior (m 0 y 1) m8))))
                 (loop for y of-type fixnum from row7 below (+ h row7)
                       do (setf (m 0 y id) (logior (m 0 y id) m-row))))
             (let* ((l2v (+ tx ss-v -1)) (step (ash 1 l2v)))
               (declare (type fixnum l2v step))
               (if (and (plusp ss-v) (> tx 1) (= 1 (logxor h (1- h))))
                   (let ((y row7))
                     (declare (type fixnum y))
                     (loop while (< y (+ h row7 -1))
                           do (setf (m 1 y 0) (logior (m 1 y 0) m-col))
                              (incf y step))
                     (when (= (- y row7) (1- h))
                       (setf (m 1 y 1) (logior (m 1 y 1) m-col))))
                   (loop for y of-type fixnum from row7 below (+ h row7) by step
                         do (setf (m 1 y id) (logior (m 1 y id) m-col)))))))
          ((/= tx 0)
           (let ((id (if (or (= tx 1) (= h ss-v)) 1 0)))
             (declare (type fixnum id))
             (setf (m 1 row7 id) (logior (m 1 row7 id) m-col)))
           (let ((id (if (or (= tx 1) (= w ss-h)) 1 0)))
             (declare (type fixnum id))
             (loop for y of-type fixnum from row7 below (+ h row7)
                   do (setf (m 0 y id) (logior (m 0 y id) tt)))))
          (t
           (let* ((t8 (logand tt (aref +wide-filter-col-mask+ ss-h)))
                  (t4 (- tt t8)))
             (declare (type fixnum t8 t4))
             (loop for y of-type fixnum from row7 below (+ h row7)
                   do (setf (m 0 y 2) (logior (m 0 y 2) t4)
                            (m 0 y 1) (logior (m 0 y 1) t8)))
             (let ((id (if (logtest row7 (aref +wide-filter-row-mask+ ss-v)) 2 1)))
               (declare (type fixnum id))
               (setf (m 1 row7 id) (logior (m 1 row7 id) m-col)))))))))
  (values))

;;; ---- walking the mask ----------------------------------------------------------------------------

(defun %filter-cols (mask sb uv level col ss-h ss-v dst stride base lim mblim)
  "Every vertical edge in one superblock of one plane: the boundaries between columns."
  (declare (type (simple-array (signed-byte 32) (* 2 2 8 4)) mask)
           (type (simple-array (signed-byte 32) (* 8 8)) level)
           (type octets dst) (type (simple-array (signed-byte 32) (64)) lim mblim)
           (type fixnum sb uv col ss-h ss-v stride base)
           (optimize (speed 3) (safety 1)))
  (macrolet ((hm (y k) `(aref mask sb uv 0 ,y ,k))
             (lv (i) `(aref level sb (ash ,i -3) (logand ,i 7))))
    (let ((ystep (ash 2 ss-v)))
      (declare (type fixnum ystep))
      ;; SIXTEEN SAMPLE ROWS PER STEP whatever the subsampling, because the step itself is twice as
      ;; tall in a subsampled plane: two level rows of eight samples, or four of four.
      (loop for y of-type fixnum from 0 below 8 by ystep
            for dstrow of-type fixnum = (+ base (* (floor y ystep) 16 stride))
            for lvl0 of-type fixnum = (* y 8)
            do (let* ((y2 (+ y 1 ss-v))
                      (hm1 (logior (hm y 0) (hm y 1) (hm y 2)))
                      (hm13 (hm y 3))
                      (hm2 (logior (hm y2 1) (hm y2 2)))
                      (hm23 (hm y2 3))
                      (all (logior hm1 hm2 hm13 hm23))
                      (li 0) (x 1) (ptr dstrow))
                 (declare (type fixnum y2 hm1 hm13 hm2 hm23 all li x ptr))
                 (loop while (logtest all (lognot (1- x)))
                       do (when (or (plusp col) (> x 1))
                            (cond
                              ((logtest hm1 x)
                               (let* ((l (lv (+ lvl0 li))) (h (ash l -4))
                                      (e (aref mblim l)) (i (aref lim l)))
                                 (declare (type fixnum l h e i))
                                 (cond
                                   ((logtest (hm y 0) x)
                                    (if (logtest (hm y2 0) x)
                                        (%lf-line dst ptr stride 1 e i h 16 16)
                                        (%lf-line dst ptr stride 1 e i h 16 8)))
                                   ((logtest hm2 x)
                                    (let* ((l2 (lv (+ lvl0 (ash 8 ss-v) li)))
                                           (w1 (if (logtest (hm y 1) x) 8 4))
                                           (w2 (if (logtest (hm y2 1) x) 8 4)))
                                      (declare (type fixnum l2 w1 w2))
                                      (%lf-line dst ptr stride 1 e i h w1 8)
                                      (%lf-line dst (+ ptr (* 8 stride)) stride 1
                                                (aref mblim l2) (aref lim l2) (ash l2 -4) w2 8)))
                                   (t (%lf-line dst ptr stride 1 e i h
                                                (if (logtest (hm y 1) x) 8 4) 8)))))
                              ((logtest hm2 x)
                               (let* ((l (lv (+ lvl0 (ash 8 ss-v) li))) (h (ash l -4)))
                                 (declare (type fixnum l h))
                                 (%lf-line dst (+ ptr (* 8 stride)) stride 1
                                           (aref mblim l) (aref lim l) h
                                           (if (logtest (hm y2 1) x) 8 4) 8)))))
                          ;; the fourth mask plane: four-wide edges halfway through an eight-step
                          (if (plusp ss-h)
                              (when (logtest x #xaa) (incf li 2))
                              (progn
                                (cond
                                  ((logtest hm13 x)
                                   (let* ((l (lv (+ lvl0 li))) (h (ash l -4)))
                                     (declare (type fixnum l h))
                                     (if (logtest hm23 x)
                                         (let ((l2 (lv (+ lvl0 (ash 8 ss-v) li))))
                                           (declare (type fixnum l2))
                                           (%lf-line dst (+ ptr 4) stride 1
                                                     (aref mblim l) (aref lim l) h 4 8)
                                           (%lf-line dst (+ ptr 4 (* 8 stride)) stride 1
                                                     (aref mblim l2) (aref lim l2) (ash l2 -4) 4 8))
                                         (%lf-line dst (+ ptr 4) stride 1
                                                   (aref mblim l) (aref lim l) h 4 8))))
                                  ((logtest hm23 x)
                                   (let* ((l (lv (+ lvl0 (ash 8 ss-v) li))))
                                     (declare (type fixnum l))
                                     (%lf-line dst (+ ptr 4 (* 8 stride)) stride 1
                                               (aref mblim l) (aref lim l) (ash l -4) 4 8))))
                                (incf li)))
                          (setf x (ash x 1))
                          (incf ptr (ash 8 (- ss-h))))))))
  (values))

(defun %filter-rows (mask sb uv level row ss-h ss-v dst stride base lim mblim)
  "And every horizontal edge: the boundaries between rows."
  (declare (type (simple-array (signed-byte 32) (* 2 2 8 4)) mask)
           (type (simple-array (signed-byte 32) (* 8 8)) level)
           (type octets dst) (type (simple-array (signed-byte 32) (64)) lim mblim)
           (type fixnum sb uv row ss-h ss-v stride base)
           (optimize (speed 3) (safety 1)))
  (macrolet ((vm (y k) `(aref mask sb uv 1 ,y ,k))
             (lv (i) `(aref level sb (ash ,i -3) (logand ,i 7))))
    (let ((lvl0 0))
      (declare (type fixnum lvl0))
      (dotimes (y 8)
        (let* ((dstrow (+ base (* y (ash 8 (- ss-v)) stride)))
               (vm0 (logior (vm y 0) (vm y 1) (vm y 2)))
               (vm3 (vm y 3))
               (step (ash 2 ss-h))
               (x 1) (li 0) (ptr dstrow))
          (declare (type fixnum dstrow vm0 vm3 step x li ptr))
          (loop while (logtest vm0 (lognot (1- x)))
                do (let ((xn (ash x (1+ ss-h))))
                     (declare (type fixnum xn))
                     (when (or (plusp row) (plusp y))
                       (cond
                         ((logtest vm0 x)
                          (let* ((l (lv (+ lvl0 li))) (h (ash l -4))
                                 (e (aref mblim l)) (i (aref lim l)))
                            (declare (type fixnum l h e i))
                            (cond
                              ((logtest (vm y 0) x)
                               (if (logtest (vm y 0) xn)
                                   (%lf-line dst ptr 1 stride e i h 16 16)
                                   (%lf-line dst ptr 1 stride e i h 16 8)))
                              ((logtest vm0 xn)
                               (let* ((l2 (lv (+ lvl0 li 1 ss-h)))
                                      (w1 (if (logtest (vm y 1) x) 8 4))
                                      (w2 (if (logtest (vm y 1) xn) 8 4)))
                                 (declare (type fixnum l2 w1 w2))
                                 (%lf-line dst ptr 1 stride e i h w1 8)
                                 (%lf-line dst (+ ptr 8) 1 stride
                                           (aref mblim l2) (aref lim l2) (ash l2 -4) w2 8)))
                              (t (%lf-line dst ptr 1 stride e i h
                                           (if (logtest (vm y 1) x) 8 4) 8)))))
                         ((logtest vm0 xn)
                          (let* ((l (lv (+ lvl0 li 1 ss-h))) (h (ash l -4)))
                            (declare (type fixnum l h))
                            (%lf-line dst (+ ptr 8) 1 stride
                                      (aref mblim l) (aref lim l) h
                                      (if (logtest (vm y 1) xn) 8 4) 8)))))
                     (when (zerop ss-v)
                       (cond
                         ((logtest vm3 x)
                          (let* ((l (lv (+ lvl0 li))) (h (ash l -4)))
                            (declare (type fixnum l h))
                            (if (logtest vm3 xn)
                                (let ((l2 (lv (+ lvl0 li 1 ss-h))))
                                  (declare (type fixnum l2))
                                  (%lf-line dst (+ ptr (* 4 stride)) 1 stride
                                            (aref mblim l) (aref lim l) h 4 8)
                                  (%lf-line dst (+ ptr (* 4 stride) 8) 1 stride
                                            (aref mblim l2) (aref lim l2) (ash l2 -4) 4 8))
                                (%lf-line dst (+ ptr (* 4 stride)) 1 stride
                                          (aref mblim l) (aref lim l) h 4 8))))
                         ((logtest vm3 xn)
                          (let ((l (lv (+ lvl0 li 1 ss-h))))
                            (declare (type fixnum l))
                            (%lf-line dst (+ ptr (* 4 stride) 8) 1 stride
                                      (aref mblim l) (aref lim l) (ash l -4) 4 8)))))
                     (setf x (ash x step))
                     (incf ptr 16)
                     (incf li step)))
          (if (plusp ss-v)
              (when (logbitp 0 y) (incf lvl0 16))
              (incf lvl0 8))))))
  (values))

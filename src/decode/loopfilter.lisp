;;;; loopfilter.lisp — VP8 in-loop deblocking filter (RFC 6386 §15).
(in-package #:reel.decode)

;;; EVERY FUNCTION HERE DECLARES ITS SPEED, and the file did not.  Compiled at the default policy
;;; the kernels below were the single largest item in a VP8 decode — thirty-eight per cent of it —
;;; not because deblocking is expensive but because none of their arithmetic was inline.  The index
;;; types are narrow for the same reason: see the DIM in tables.lisp.

(declaim (inline c8))
(defun c8 (v) (declare (type fixnum v) (optimize (speed 3) (safety 0))) (cond ((< v -128) -128) ((> v 127) 127) (t v)))

;;; common_adjust (RFC §15.2): modifies P0,Q0 in place, returns the shift value.
(declaim (inline common-adjust))
(defun common-adjust (use-outer data p1i p0i q0i q1i)
  (declare (type (simple-array (signed-byte 32) (*)) data) (type dim p1i p0i q0i q1i)
           (optimize (speed 3) (safety 0)))
  (let* ((p1 (- (aref data p1i) 128)) (p0 (- (aref data p0i) 128))
         (q0 (- (aref data q0i) 128)) (q1 (- (aref data q1i) 128))
         (a (c8 (+ (if use-outer (c8 (- p1 q1)) 0) (* 3 (- q0 p0)))))
         (b (ash (c8 (+ a 3)) -3)))
    (declare (type fixnum p1 p0 q0 q1 a b))
    (setf a (ash (c8 (+ a 4)) -3))
    (setf (aref data q0i) (+ (c8 (- q0 a)) 128))
    (setf (aref data p0i) (+ (c8 (+ p0 b)) 128))
    a))

(declaim (inline lf-diff))
(defun lf-diff (data i j)
  (declare (type (simple-array (signed-byte 32) (*)) data) (type dim i j)
           (optimize (speed 3) (safety 0)))
  (abs (- (aref data i) (aref data j))))

(declaim (inline filter-yes))
(defun filter-yes (ilim elim data p3i p2i p1i p0i q0i q1i q2i q3i)
  (declare (type (signed-byte 16) ilim elim) (type dim p3i p2i p1i p0i q0i q1i q2i q3i)
           (optimize (speed 3) (safety 0)))
  (and (<= (+ (* 2 (lf-diff data p0i q0i)) (ash (lf-diff data p1i q1i) -1)) elim)
       (<= (lf-diff data p3i p2i) ilim) (<= (lf-diff data p2i p1i) ilim)
       (<= (lf-diff data p1i p0i) ilim) (<= (lf-diff data q3i q2i) ilim)
       (<= (lf-diff data q2i q1i) ilim) (<= (lf-diff data q1i q0i) ilim)))

(declaim (inline hev))
(defun hev (thr data p1i p0i q0i q1i)
  (declare (type (signed-byte 16) thr) (type dim p1i p0i q0i q1i)
           (optimize (speed 3) (safety 0)))
  (or (> (lf-diff data p1i p0i) thr) (> (lf-diff data q1i q0i) thr)))

;;; simple filter segment (luma only)
(defun kernel-simple (data q0i s elim)
  (declare (type (simple-array (signed-byte 32) (*)) data)
           (type dim q0i s) (type (signed-byte 16) elim)
           (optimize (speed 3) (safety 0)))
  (let ((p1i (- q0i s s)) (p0i (- q0i s)) (q1i (+ q0i s)))
    (when (<= (+ (* 2 (lf-diff data p0i q0i)) (ash (lf-diff data p1i q1i) -1)) elim)
      (common-adjust t data p1i p0i q0i q1i))))

;;; normal inter-subblock filter
(defun kernel-sub (data q0i s hthr ilim elim)
  (declare (type (simple-array (signed-byte 32) (*)) data)
           (type dim q0i s) (type (signed-byte 16) hthr ilim elim)
           (optimize (speed 3) (safety 0)))
  (let* ((p3i (- q0i (* 4 s))) (p2i (- q0i (* 3 s))) (p1i (- q0i (* 2 s))) (p0i (- q0i s))
         (q1i (+ q0i s)) (q2i (+ q0i (* 2 s))) (q3i (+ q0i (* 3 s))))
    (when (filter-yes ilim elim data p3i p2i p1i p0i q0i q1i q2i q3i)
      (let ((hv (hev hthr data p1i p0i q0i q1i)))
        (let ((a (ash (+ (common-adjust hv data p1i p0i q0i q1i) 1) -1)))
          (unless hv
            (setf (aref data q1i) (+ (c8 (- (- (aref data q1i) 128) a)) 128))
            (setf (aref data p1i) (+ (c8 (+ (- (aref data p1i) 128) a)) 128))))))))

;;; normal inter-macroblock filter
(defun kernel-mb (data q0i s hthr ilim elim)
  (declare (type (simple-array (signed-byte 32) (*)) data)
           (type dim q0i s) (type (signed-byte 16) hthr ilim elim)
           (optimize (speed 3) (safety 0)))
  (let* ((p3i (- q0i (* 4 s))) (p2i (- q0i (* 3 s))) (p1i (- q0i (* 2 s))) (p0i (- q0i s))
         (q1i (+ q0i s)) (q2i (+ q0i (* 2 s))) (q3i (+ q0i (* 3 s))))
    (when (filter-yes ilim elim data p3i p2i p1i p0i q0i q1i q2i q3i)
      (if (hev hthr data p1i p0i q0i q1i)
          (common-adjust t data p1i p0i q0i q1i)
          (let* ((p2 (- (aref data p2i) 128)) (p1 (- (aref data p1i) 128))
                 (p0 (- (aref data p0i) 128)) (q0 (- (aref data q0i) 128))
                 (q1 (- (aref data q1i) 128)) (q2 (- (aref data q2i) 128))
                 (w (c8 (+ (c8 (- p1 q1)) (* 3 (- q0 p0))))))
            (declare (type (signed-byte 32) p2 p1 p0 q0 q1 q2 w))
            (let ((a (c8 (ash (+ (* 27 w) 63) -7))))
              (setf (aref data q0i) (+ (c8 (- q0 a)) 128)
                    (aref data p0i) (+ (c8 (+ p0 a)) 128)))
            (let ((a (c8 (ash (+ (* 18 w) 63) -7))))
              (setf (aref data q1i) (+ (c8 (- q1 a)) 128)
                    (aref data p1i) (+ (c8 (+ p1 a)) 128)))
            (let ((a (c8 (ash (+ (* 9 w) 63) -7))))
              (setf (aref data q2i) (+ (c8 (- q2 a)) 128)
                    (aref data p2i) (+ (c8 (+ p2 a)) 128))))))))

;;; run a kernel along an edge: COUNT segments starting at index Q0, advancing
;;; ALONG per segment, with cross step CROSS (perpendicular to the edge).
(defmacro along-edge ((q0 along count) &body body)
  `(let ((idx ,q0))
     (declare (type fixnum idx))
     (dotimes (k ,count)
       (progn ,@body)
       (incf idx ,along))))

(defun filter-plane-edges (pl mbx mby is-luma simple hthr ilim mbelim subelim do-inner)
  "Filter the four edge groups for one plane block of the current MB."
  (declare (type plane pl) (type dim mbx mby)
           (type (signed-byte 16) hthr ilim mbelim subelim)
           (optimize (speed 3) (safety 1)))
  (let* ((data (pl-data pl)) (stride (pl-stride pl))
         (len (if is-luma 16 8))
         (bx (if is-luma (* mbx 16) (* mbx 8)))
         (by (if is-luma (* mby 16) (* mby 8)))
         (inner (if is-luma '(4 8 12) '(4))))
    (declare (type dim stride len bx by))
    (flet ((vidx (x y) (declare (type dim x y)) (+ (* (+ y 1) stride) (+ x 1))))
      ;; step 1: left MB edge (vertical), cross step 1, along = stride
      (when (> mbx 0)
        (along-edge ((vidx bx by) stride len)
          (if simple (kernel-simple data idx 1 mbelim)
              (kernel-mb data idx 1 hthr ilim mbelim))))
      ;; step 2: inner vertical edges
      (when do-inner
        (dolist (dx inner)
          (along-edge ((vidx (+ bx dx) by) stride len)
            (if simple (kernel-simple data idx 1 subelim)
                (kernel-sub data idx 1 hthr ilim subelim)))))
      ;; step 3: top MB edge (horizontal), cross step = stride, along = 1
      (when (> mby 0)
        (along-edge ((vidx bx by) 1 len)
          (if simple (kernel-simple data idx stride mbelim)
              (kernel-mb data idx stride hthr ilim mbelim))))
      ;; step 4: inner horizontal edges
      (when do-inner
        (dolist (dy inner)
          (along-edge ((vidx bx (+ by dy)) 1 len)
            (if simple (kernel-simple data idx stride subelim)
                (kernel-sub data idx stride hthr ilim subelim))))))))

(defun filter-strength (d seg i4x4)
  "Return (values level interior-limit hev-threshold) for a MB, or level 0."
  (let ((level (if (d-seg-enabled d)
                   (if (d-seg-abs d)
                       (aref (d-seg-filter d) seg)
                       (+ (d-filter-level d) (aref (d-seg-filter d) seg)))
                   (d-filter-level d))))
    (when (d-lf-delta-enabled d)
      (incf level (aref (d-ref-lf-delta d) 0))    ; INTRA_FRAME reference
      (when i4x4 (incf level (aref (d-mode-lf-delta d) 0))))
    (setf level (max 0 (min 63 level)))
    (if (zerop level)
        (values 0 0 0)
        (let ((ilim level) (sharp (d-sharpness d)) (hthr 0))
          (when (> sharp 0)
            (setf ilim (ash ilim (if (> sharp 4) -2 -1)))
            (when (> ilim (- 9 sharp)) (setf ilim (- 9 sharp))))
          (when (zerop ilim) (setf ilim 1))
          (cond ((>= level 40) (setf hthr 2))
                ((>= level 15) (setf hthr 1)))
          (values level ilim hthr)))))

(defun loop-filter (d)
  (when (zerop (d-filter-level d)) (return-from loop-filter))
  (let ((cols (d-mb-cols d)) (rows (d-mb-rows d)) (simple (d-filter-simple d)))
    (dotimes (mby rows)
      (dotimes (mbx cols)
        (let* ((mi (+ (* mby cols) mbx))
               (seg (aref (d-mb-seg d) mi))
               (i4x4 (aref (d-mb-i4x4 d) mi)))
          (multiple-value-bind (level ilim hthr) (filter-strength d seg i4x4)
            (when (> level 0)
              (let* ((mbelim (+ (* (+ level 2) 2) ilim))
                     (subelim (+ (* level 2) ilim))
                     (do-inner (or i4x4 (aref (d-mb-nonzero d) mi))))
                (filter-plane-edges (d-yplane d) mbx mby t simple
                                    hthr ilim mbelim subelim do-inner)
                (unless simple
                  (filter-plane-edges (d-uplane d) mbx mby nil nil
                                      hthr ilim mbelim subelim do-inner)
                  (filter-plane-edges (d-vplane d) mbx mby nil nil
                                      hthr ilim mbelim subelim do-inner))))))))))

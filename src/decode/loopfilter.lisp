;;;; loopfilter.lisp — VP8 in-loop deblocking filter (RFC 6386 §15).
(in-package #:reel.decode)

;;; EVERY FUNCTION HERE DECLARES ITS SPEED, and the file did not.  Compiled at the default policy
;;; the kernels below were the single largest item in a VP8 decode — thirty-eight per cent of it —
;;; not because deblocking is expensive but because none of their arithmetic was inline.  The index
;;; types are narrow for the same reason: see the DIM in tables.lisp.

(declaim (inline c8))
(defun c8 (v) (declare (type fixnum v) (optimize (speed 3) (safety 0))) (cond ((< v -128) -128) ((> v 127) 127) (t v)))

;;; ---- the kernels work on UNSIGNED samples, because the offsets cancel -------------------------
;;;
;;; RFC 6386 §15.2 is written on SIGNED bytes: subtract 128 from each sample, filter, add 128 back.
;;; Written that way this file did twelve additions per line that were not doing anything.  Every
;;; place a sample appears in the arithmetic it appears as a DIFFERENCE of two of them — (p1 - q1),
;;; (q0 - p0) — and the two 128s cancel; and at the other end, c8(x - 128) + 128 is exactly
;;; clamp255(x) for every integer x, because c8 clamps to [-128,127] and the 128 puts that back at
;;; [0,255].  So the conversion is not needed at either end, and what is left is the specification's
;;; arithmetic with two fewer operations per sample touched.

(declaim (inline %adjust))
(defun %adjust (use-outer p1 p0 q0 q1)
  "The common adjustment (§15.2).  Returns the new P0 and Q0 and the shift the sub-block filter
   needs afterwards, all in unsigned sample space."
  (declare (type (unsigned-byte 8) p1 p0 q0 q1) (optimize (speed 3) (safety 0)))
  (let* ((a (c8 (+ (if use-outer (c8 (- p1 q1)) 0) (* 3 (- q0 p0)))))
         (b (ash (c8 (+ a 3)) -3))
         (a2 (ash (c8 (+ a 4)) -3)))
    (declare (type fixnum a b a2))
    (values (clamp255 (+ p0 b)) (clamp255 (- q0 a2)) a2)))

(declaim (inline %edge-ok %hev))
(defun %edge-ok (ilim elim p3 p2 p1 p0 q0 q1 q2 q3)
  "The gate: a step across the edge no larger than ELIM, and no step within either side larger
   than ILIM.  Anything sharper is taken to be real detail and left alone."
  (declare (type (signed-byte 16) ilim elim)
           (type (unsigned-byte 8) p3 p2 p1 p0 q0 q1 q2 q3)
           (optimize (speed 3) (safety 0)))
  (and (<= (+ (* 2 (abs (- p0 q0))) (ash (abs (- p1 q1)) -1)) elim)
       (<= (abs (- p3 p2)) ilim) (<= (abs (- p2 p1)) ilim)
       (<= (abs (- p1 p0)) ilim) (<= (abs (- q3 q2)) ilim)
       (<= (abs (- q2 q1)) ilim) (<= (abs (- q1 q0)) ilim)))

(defun %hev (thr p1 p0 q0 q1)
  "High edge variance: the step either side is already sharp, so only the two nearest samples move."
  (declare (type (signed-byte 16) thr) (type (unsigned-byte 8) p1 p0 q0 q1)
           (optimize (speed 3) (safety 0)))
  (or (> (abs (- p1 p0)) thr) (> (abs (- q1 q0)) thr)))

;;; ---- one whole edge per call ------------------------------------------------------------------
;;;
;;; THE LINE LOOP LIVES INSIDE THE KERNEL, and that is the second thing this file learned.  Written
;;; the natural way — a kernel that filters one line, driven along the edge by the caller — a
;;; 640x360 picture makes about a hundred and seventy thousand calls a frame, each of which sets up
;;; its arguments and recomputes the seven sample offsets from the edge step it was handed.  None of
;;; that varies along an edge.  Passing the edge instead of the line hoists all of it and turns the
;;; hundred and seventy thousand calls into nine hundred.
;;;
;;; START is the index of q0 on the first line, ALONG the step from one line to the next, and S the
;;; step ACROSS the edge — one for a vertical edge, the stride for a horizontal one, and the caller
;;; passes them the other way round for the two directions.

(defmacro %along ((q0i start along count) &body body)
  `(do ((k 0 (1+ k)) (,q0i ,start (+ ,q0i ,along)))
       ((>= k ,count))
     (declare (type (integer 0 64) k) (type dim ,q0i))
     ,@body))

;;; simple filter (luma only): two samples either side, no interior test
(defun %edge-simple (data start along s count elim)
  (declare (type u8vec data) (type dim start along s) (type (integer 0 64) count)
           (type (signed-byte 16) elim) (optimize (speed 3) (safety 0)))
  (let ((s2 (* 2 s)))
    (declare (type dim s2))
    (%along (q0i start along count)
      (let* ((p1i (- q0i s2)) (p0i (- q0i s)) (q1i (+ q0i s))
             (p1 (aref data p1i)) (p0 (aref data p0i))
             (q0 (aref data q0i)) (q1 (aref data q1i)))
        (declare (type dim p1i p0i q1i) (type (unsigned-byte 8) p1 p0 q0 q1))
        (when (<= (+ (* 2 (abs (- p0 q0))) (ash (abs (- p1 q1)) -1)) elim)
          (multiple-value-bind (np0 nq0) (%adjust t p1 p0 q0 q1)
            (setf (aref data p0i) np0
                  (aref data q0i) nq0)))))))

;;; normal filter, sub-block edge: p1..q1 may move
(defun %edge-sub (data start along s count hthr ilim elim)
  (declare (type u8vec data) (type dim start along s) (type (integer 0 64) count)
           (type (signed-byte 16) hthr ilim elim) (optimize (speed 3) (safety 0)))
  (let ((s2 (* 2 s)) (s3 (* 3 s)) (s4 (* 4 s)))
    (declare (type dim s2 s3 s4))
    (%along (q0i start along count)
      (let* ((p3i (- q0i s4)) (p2i (- q0i s3)) (p1i (- q0i s2)) (p0i (- q0i s))
             (q1i (+ q0i s)) (q2i (+ q0i s2)) (q3i (+ q0i s3))
             (p3 (aref data p3i)) (p2 (aref data p2i)) (p1 (aref data p1i)) (p0 (aref data p0i))
             (q0 (aref data q0i)) (q1 (aref data q1i)) (q2 (aref data q2i)) (q3 (aref data q3i)))
        (declare (type dim p3i p2i p1i p0i q1i q2i q3i)
                 (type (unsigned-byte 8) p3 p2 p1 p0 q0 q1 q2 q3))
        (when (%edge-ok ilim elim p3 p2 p1 p0 q0 q1 q2 q3)
          (let ((hv (%hev hthr p1 p0 q0 q1)))
            (multiple-value-bind (np0 nq0 a0) (%adjust hv p1 p0 q0 q1)
              (declare (type fixnum a0))
              (setf (aref data p0i) np0
                    (aref data q0i) nq0)
              (unless hv
                (let ((a (ash (+ a0 1) -1)))
                  (declare (type fixnum a))
                  (setf (aref data q1i) (clamp255 (- q1 a))
                        (aref data p1i) (clamp255 (+ p1 a))))))))))))

;;; normal filter, macroblock edge: p2..q2 may move, by three different weights
(defun %edge-mb (data start along s count hthr ilim elim)
  (declare (type u8vec data) (type dim start along s) (type (integer 0 64) count)
           (type (signed-byte 16) hthr ilim elim) (optimize (speed 3) (safety 0)))
  (let ((s2 (* 2 s)) (s3 (* 3 s)) (s4 (* 4 s)))
    (declare (type dim s2 s3 s4))
    (%along (q0i start along count)
      (let* ((p3i (- q0i s4)) (p2i (- q0i s3)) (p1i (- q0i s2)) (p0i (- q0i s))
             (q1i (+ q0i s)) (q2i (+ q0i s2)) (q3i (+ q0i s3))
             (p3 (aref data p3i)) (p2 (aref data p2i)) (p1 (aref data p1i)) (p0 (aref data p0i))
             (q0 (aref data q0i)) (q1 (aref data q1i)) (q2 (aref data q2i)) (q3 (aref data q3i)))
        (declare (type dim p3i p2i p1i p0i q1i q2i q3i)
                 (type (unsigned-byte 8) p3 p2 p1 p0 q0 q1 q2 q3))
        (when (%edge-ok ilim elim p3 p2 p1 p0 q0 q1 q2 q3)
          (if (%hev hthr p1 p0 q0 q1)
              (multiple-value-bind (np0 nq0) (%adjust t p1 p0 q0 q1)
                (setf (aref data p0i) np0
                      (aref data q0i) nq0))
              (let ((w (c8 (+ (c8 (- p1 q1)) (* 3 (- q0 p0))))))
                (declare (type fixnum w))
                (let ((a (c8 (ash (+ (* 27 w) 63) -7))))
                  (setf (aref data q0i) (clamp255 (- q0 a))
                        (aref data p0i) (clamp255 (+ p0 a))))
                (let ((a (c8 (ash (+ (* 18 w) 63) -7))))
                  (setf (aref data q1i) (clamp255 (- q1 a))
                        (aref data p1i) (clamp255 (+ p1 a))))
                (let ((a (c8 (ash (+ (* 9 w) 63) -7))))
                  (setf (aref data q2i) (clamp255 (- q2 a))
                        (aref data p2i) (clamp255 (+ p2 a)))))))))))

(defun filter-plane-edges (pl mbx mby is-luma simple hthr ilim mbelim subelim do-inner)
  "Filter the four edge groups for one plane block of the current MB.

   THE ORDER IS NORMATIVE: every vertical edge left to right, then every horizontal edge top to
   bottom, and each one sees the samples the last one left behind."
  (declare (type plane pl) (type dim mbx mby)
           (type (signed-byte 16) hthr ilim mbelim subelim)
           (optimize (speed 3) (safety 1)))
  (let* ((data (pl-data pl)) (stride (pl-stride pl))
         (len (if is-luma 16 8))
         (bx (if is-luma (* mbx 16) (* mbx 8)))
         (by (if is-luma (* mby 16) (* mby 8)))
         (inner (if is-luma '(4 8 12) '(4)))
         (base (+ (* (+ by 1) stride) bx 1)))   ; the block's top-left visible sample
    (declare (type dim stride len bx by base))
    (if simple
        (progn
          (when (> mbx 0) (%edge-simple data base stride 1 len mbelim))
          (when do-inner
            (dolist (dx inner)
              (declare (type (integer 0 12) dx))
              (%edge-simple data (+ base dx) stride 1 len subelim)))
          (when (> mby 0) (%edge-simple data base 1 stride len mbelim))
          (when do-inner
            (dolist (dy inner)
              (declare (type (integer 0 12) dy))
              (%edge-simple data (+ base (* dy stride)) 1 stride len subelim))))
        (progn
          (when (> mbx 0) (%edge-mb data base stride 1 len hthr ilim mbelim))
          (when do-inner
            (dolist (dx inner)
              (declare (type (integer 0 12) dx))
              (%edge-sub data (+ base dx) stride 1 len hthr ilim subelim)))
          (when (> mby 0) (%edge-mb data base 1 stride len hthr ilim mbelim))
          (when do-inner
            (dolist (dy inner)
              (declare (type (integer 0 12) dy))
              (%edge-sub data (+ base (* dy stride)) 1 stride len hthr ilim subelim)))))))

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

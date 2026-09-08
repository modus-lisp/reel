;;;; mpeg2/motion.lisp — half-pel motion compensation.
;;;;
;;;; After H.264's six-tap quarter-pel filter this is almost restful.  MPEG-2 interpolates to HALF a
;;;; sample and does it by averaging: two neighbours for a half-step in one direction, four for a
;;;; half-step in both, rounded up.  There is no filter, no separable pass, and no intermediate
;;;; precision to get wrong.
;;;;
;;;; What is easy to get wrong is the CHROMA vector.  In 4:2:0 it is the luma vector divided by two,
;;;; and that division TRUNCATES TOWARD ZERO rather than flooring — so a luma vector of -3 half-pels
;;;; gives a chroma vector of -1 and not -2.  The two agree for every positive vector and for half
;;;; the negative ones, which is exactly the kind of error that decodes a still shot perfectly and
;;;; smears whenever the camera pans left.

(in-package #:reel.mpeg2)

(declaim (inline %ref))
(defun %ref (plane stride w h x y)
  "One reference sample, with the picture edge extended outwards indefinitely."
  (declare (type octets plane) (type fixnum stride w h x y)
           (optimize (speed 3) (safety 0)))
  (aref plane (+ (* (min (1- h) (max 0 y)) stride) (min (1- w) (max 0 x)))))

(defun predict-block (dst dstride dbase plane stride w h px py bw bh mvx mvy avg-p
                      &key (src-offset 0))
  "Predict a BW x BH block at (PX,PY) with the half-pel vector (MVX,MVY) and write it into DST.

   AVG-P averages with whatever is already there instead of overwriting, which is how a
   bidirectionally predicted macroblock is built: the forward prediction is written, then the
   backward one is averaged onto it.  Averaging the two FINISHED predictions is not the same as
   interpolating once from both references, and the specification asks for the former.

   SRC-OFFSET is added to every read and is how field prediction is expressed: the two fields of a
   picture are the same plane read at twice the stride, one of them starting a row lower."
  (declare (type octets dst plane)
           (type fixnum dstride dbase stride w h px py bw bh mvx mvy src-offset)
           (optimize (speed 3) (safety 0)))
  (let* ((sx (+ px (ash mvx -1))) (sy (+ py (ash mvy -1)))
         (hx (logand mvx 1)) (hy (logand mvy 1))
         ;; the fast path is the common one by far: a vector that keeps the block, and the extra
         ;; row and column a half-pel step reads, inside the picture
         (inside (and (>= sx 0) (>= sy 0)
                      (<= (+ sx bw hx) w) (<= (+ sy bh hy) h))))
    (declare (type fixnum sx sy hx hy))
    (macrolet ((each ((yv xv) form)
                 `(dotimes (,yv bh)
                    (declare (type fixnum ,yv))
                    (let ((o (+ dbase (* ,yv dstride))))
                      (declare (type fixnum o))
                      (dotimes (,xv bw)
                        (declare (type fixnum ,xv))
                        (let ((v ,form))
                          (declare (type fixnum v))
                          (setf (aref dst (+ o ,xv))
                                (if avg-p (ash (+ (aref dst (+ o ,xv)) v 1) -1) v))))))))
      (if inside
          (let ((b (+ src-offset (* sy stride) sx)))
            (declare (type fixnum b))
            (macrolet ((p (dy dx) `(aref plane (+ b (* (+ y ,dy) stride) x ,dx))))
              (cond ((and (zerop hx) (zerop hy)) (each (y x) (p 0 0)))
                    ((and (= 1 hx) (zerop hy))   (each (y x) (ash (+ (p 0 0) (p 0 1) 1) -1)))
                    ((and (zerop hx) (= 1 hy))   (each (y x) (ash (+ (p 0 0) (p 1 0) 1) -1)))
                    (t (each (y x) (ash (+ (p 0 0) (p 0 1) (p 1 0) (p 1 1) 2) -2))))))
          (macrolet ((p (dy dx) `(+ 0 (aref plane (+ src-offset
                                                    (* (min (1- h) (max 0 (+ sy y ,dy))) stride)
                                                    (min (1- w) (max 0 (+ sx x ,dx))))))))
            (cond ((and (zerop hx) (zerop hy)) (each (y x) (p 0 0)))
                  ((and (= 1 hx) (zerop hy))   (each (y x) (ash (+ (p 0 0) (p 0 1) 1) -1)))
                  ((and (zerop hx) (= 1 hy))   (each (y x) (ash (+ (p 0 0) (p 1 0) 1) -1)))
                  (t (each (y x) (ash (+ (p 0 0) (p 0 1) (p 1 0) (p 1 1) 2) -2)))))))))

(declaim (inline chroma-mv))
(defun chroma-mv (mv)
  "The 4:2:0 chroma component of a luma motion vector: halved, TOWARD ZERO."
  (declare (type fixnum mv))
  (truncate mv 2))

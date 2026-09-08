;;;; vp9/mc.lisp — motion compensation: eight taps, three filters, and two references at once.
;;;;
;;;; A vector addresses eighths of a luma sample, and a sample at a fractional position is an
;;;; eight-tap filter of its neighbours.  VP9 has THREE such filters and a block chooses between
;;;; them — regular, sharp, smooth — because the right one depends on the content: a sharp filter
;;;; keeps detail and rings on noise, a smooth one does the reverse.
;;;;
;;;; CHROMA VECTORS ARE IN SIXTEENTHS, not eighths, and that is not a separate convention: a luma
;;;; vector of one eighth of a luma sample IS one sixteenth of a chroma sample when chroma is at half
;;;; the resolution.  So the same number is read against a sixteen-phase filter instead of taking
;;;; every second phase, which is why chroma prediction is finer than luma prediction and not coarser.
;;;;
;;;; A COMPOUND BLOCK PREDICTS TWICE and averages, and the average is taken sample by sample as the
;;;; second prediction is computed rather than into a second buffer.  The rounding is up.

(in-package #:reel.vp9)

(declaim (inline %tap8))
(defun %tap8 (f fbase s0 s1 s2 s3 s4 s5 s6 s7)
  (declare (type (simple-array fixnum (3 16 8)) f) (type fixnum fbase)
           (type fixnum s0 s1 s2 s3 s4 s5 s6 s7) (optimize (speed 3) (safety 0)))
  (let ((v (+ (* (row-major-aref f (+ fbase 0)) s0) (* (row-major-aref f (+ fbase 1)) s1)
              (* (row-major-aref f (+ fbase 2)) s2) (* (row-major-aref f (+ fbase 3)) s3)
              (* (row-major-aref f (+ fbase 4)) s4) (* (row-major-aref f (+ fbase 5)) s5)
              (* (row-major-aref f (+ fbase 6)) s6) (* (row-major-aref f (+ fbase 7)) s7)
              64)))
    (declare (type fixnum v))
    (%clip8 (ash v -7))))

(defun %gather-window (out ref rstride w h x y bw bh)
  "Copy a BW by BH window of REF starting at (X,Y), CLAMPING coordinates to the picture.

   The clamp is the specification's own edge handling: a vector may point outside the picture
   entirely and the edge sample repeats for as far as it does.  It is done into a scratch buffer
   rather than by padding the planes because a VP9 vector can be larger than any padding worth
   allocating — the bound is the frame size, not a level limit."
  (declare (type octets out ref) (type fixnum rstride w h x y bw bh)
           (optimize (speed 3) (safety 1)))
  (dotimes (j bh)
    (let ((sy (max 0 (min (1- h) (+ y j))))
          (o (* j bw)))
      (declare (type fixnum sy o))
      (let ((row (* sy rstride)))
        (declare (type fixnum row))
        (dotimes (i bw)
          (let ((sx (max 0 (min (1- w) (+ x i)))))
            (declare (type fixnum sx))
            (setf (aref out (+ o i)) (aref ref (+ row sx)))))))))

(defun %mc-block (dst dstride dbase ref rstride rw rh x y mvx mvy bw bh filter avg eighths)
  "Predict a BW by BH block at DBASE from REF, displaced by (MVX,MVY).

   EIGHTHS says which resolution the vector is in: true for luma, where it addresses eighths of a
   sample and the filter's sixteen phases are stepped two at a time, and false for chroma, where it
   addresses sixteenths and every phase is reachable."
  (declare (type octets dst ref) (type fixnum dstride dbase rstride rw rh x y mvx mvy bw bh filter)
           (optimize (speed 3) (safety 1)))
  (let* ((shift (if eighths 3 4))
         (mask (1- (ash 1 shift)))
         (ix (+ x (ash mvx (- shift))))
         (iy (+ y (ash mvy (- shift))))
         (fx (logand mvx mask))
         (fy (logand mvy mask))
         (px (if eighths (* 2 fx) fx))
         (py (if eighths (* 2 fy) fy))
         (f +subpel-filters+))
    (declare (type fixnum shift mask ix iy fx fy px py))
    (if (and (zerop px) (zerop py))
        ;; a whole-sample vector filters nothing
        (let ((win (make-array (* bw bh) :element-type '(unsigned-byte 8))))
          (declare (dynamic-extent win))
          (%gather-window win ref rstride rw rh ix iy bw bh)
          (dotimes (j bh)
            (let ((o (+ dbase (* j dstride))) (s (* j bw)))
              (declare (type fixnum o s))
              (if avg
                  (dotimes (i bw)
                    (setf (aref dst (+ o i))
                          (ash (+ (aref dst (+ o i)) (aref win (+ s i)) 1) -1)))
                  (dotimes (i bw)
                    (setf (aref dst (+ o i)) (aref win (+ s i))))))))
        (let* ((ww (+ bw 7)) (wh (+ bh 7))
               (win (make-array (* ww wh) :element-type '(unsigned-byte 8)))
               (tmp (make-array (* bw wh) :element-type 'fixnum))
               (fxb (+ (* (* 16 8) filter) (* 8 px)))
               (fyb (+ (* (* 16 8) filter) (* 8 py))))
          (declare (type fixnum ww wh fxb fyb) (dynamic-extent win tmp))
          (%gather-window win ref rstride rw rh (- ix 3) (- iy 3) ww wh)
          (macrolet ((h-at (base i)
                       `(%tap8 f fxb (aref win (+ ,base ,i 0)) (aref win (+ ,base ,i 1))
                               (aref win (+ ,base ,i 2)) (aref win (+ ,base ,i 3))
                               (aref win (+ ,base ,i 4)) (aref win (+ ,base ,i 5))
                               (aref win (+ ,base ,i 6)) (aref win (+ ,base ,i 7)))))
            (cond
              ((zerop py)
               ;; horizontal only: the three rows of margin above are not read
               (dotimes (j bh)
                 (let ((s (* (+ j 3) ww)) (o (+ dbase (* j dstride))))
                   (declare (type fixnum s o))
                   (dotimes (i bw)
                     (let ((v (h-at s i)))
                       (declare (type fixnum v))
                       (setf (aref dst (+ o i))
                             (if avg (ash (+ (aref dst (+ o i)) v 1) -1) v)))))))
              ((zerop px)
               ;; vertical only, reading down the window's middle three columns of margin
               (dotimes (j bh)
                 (let ((o (+ dbase (* j dstride))))
                   (declare (type fixnum o))
                   (dotimes (i bw)
                     (let* ((c (+ i 3))
                            (v (%tap8 f fyb
                                      (aref win (+ (* j ww) c)) (aref win (+ (* (+ j 1) ww) c))
                                      (aref win (+ (* (+ j 2) ww) c)) (aref win (+ (* (+ j 3) ww) c))
                                      (aref win (+ (* (+ j 4) ww) c)) (aref win (+ (* (+ j 5) ww) c))
                                      (aref win (+ (* (+ j 6) ww) c)) (aref win (+ (* (+ j 7) ww) c)))))
                       (declare (type fixnum c v))
                       (setf (aref dst (+ o i))
                             (if avg (ash (+ (aref dst (+ o i)) v 1) -1) v)))))))
              (t
               ;; both: horizontal first over every row the vertical pass will need, and the
               ;; INTERMEDIATE IS CLIPPED to a sample, which is what makes the two passes
               ;; separable in the first place — a wider intermediate would not agree
               (dotimes (j wh)
                 (let ((s (* j ww)) (o (* j bw)))
                   (declare (type fixnum s o))
                   (dotimes (i bw) (setf (aref tmp (+ o i)) (h-at s i)))))
               (dotimes (j bh)
                 (let ((o (+ dbase (* j dstride))))
                   (declare (type fixnum o))
                   (dotimes (i bw)
                     (let ((v (%tap8 f fyb
                                     (aref tmp (+ (* j bw) i)) (aref tmp (+ (* (+ j 1) bw) i))
                                     (aref tmp (+ (* (+ j 2) bw) i)) (aref tmp (+ (* (+ j 3) bw) i))
                                     (aref tmp (+ (* (+ j 4) bw) i)) (aref tmp (+ (* (+ j 5) bw) i))
                                     (aref tmp (+ (* (+ j 6) bw) i)) (aref tmp (+ (* (+ j 7) bw) i)))))
                       (declare (type fixnum v))
                       (setf (aref dst (+ o i))
                             (if avg (ash (+ (aref dst (+ o i)) v 1) -1) v)))))))))))
    (values)))

(declaim (inline %rounded-div))
(defun %rounded-div (a b)
  "Round to nearest, away from zero, which is what averaging motion vectors wants."
  (declare (type fixnum a b) (optimize (speed 3) (safety 0)))
  (if (>= a 0) (floor (+ a (ash b -1)) b) (- (floor (+ (- a) (ash b -1)) b))))

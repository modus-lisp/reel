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
  (declare (type (simple-array (signed-byte 32) (3 16 8)) f) (type fixnum fbase)
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

(defun %mc-block (dst dstride dbase ref rstride rw rh x y mvx mvy bw bh filter avg eighths
                  tmp win)
  "Predict a BW by BH block at DBASE from REF, displaced by (MVX,MVY).

   EIGHTHS says which resolution the vector is in: true for luma, where it addresses eighths of a
   sample and the filter's sixteen phases are stepped two at a time, and false for chroma, where it
   addresses sixteenths and every phase is reachable.

   TWO COPIES OF THE SAME FOUR CASES, differing only in where a source row begins.  A vector may
   point outside the picture and the edge sample repeats for as far as it does, so in general every
   read has to be clamped — and a block near the middle of the picture, which is nearly all of them,
   cannot possibly reach the edge.  The clamped copy gathers a window first; the other reads the
   reference plane where it lies.  Which one runs is decided once per block.

   The abstraction is the ROW, not the sample.  Abstracting the sample instead puts the row's
   multiply inside the tap loop, which is eight multiplies per output instead of one per row and
   costs more than the clamping it was meant to avoid — measured, not guessed.

   IT RUNS WITHOUT BOUNDS CHECKING, which is safe here for a reason and not by assertion: a vector
   that points outside the picture fails the INSIDE test below and is served from a gathered window
   whose coordinates are clamped, so neither path can address the reference plane out of range, and
   the destination is a block position inside a superblock-aligned plane.  The scratch buffers are
   sized for the largest block the format has."
  (declare (type octets dst ref win) (type (simple-array (signed-byte 32) (*)) tmp)
           (type fixnum dstride dbase rstride rw rh x y mvx mvy bw bh filter)
           (optimize (speed 3) (safety 0)))
  (let* ((shift (if eighths 3 4))
         (mask (1- (ash 1 shift)))
         (ix (+ x (ash mvx (- shift))))
         (iy (+ y (ash mvy (- shift))))
         (px (if eighths (* 2 (logand mvx mask)) (logand mvx mask)))
         (py (if eighths (* 2 (logand mvy mask)) (logand mvy mask)))
         (f +subpel-filters+)
         (fxb (+ (* 128 filter) (* 8 px)))
         (fyb (+ (* 128 filter) (* 8 py)))
         ;; the filter reaches three samples back and four forward, and only along an axis it filters
         (lx (if (plusp px) 3 0)) (rx (if (plusp px) 4 0))
         (ly (if (plusp py) 3 0)) (ry (if (plusp py) 4 0))
         (inside (and (>= (- ix lx) 0) (>= (- iy ly) 0)
                      (<= (+ ix bw rx) rw) (<= (+ iy bh ry) rh)))
         (ww (+ bw 7)))
    (declare (type fixnum shift mask ix iy px py fxb fyb lx rx ly ry ww))
    (macrolet
        ((run (src rowbase step)
           `(let ((step ,step))
              (declare (type fixnum step))
              (cond
                ((and (zerop px) (zerop py))
                 (dotimes (j bh)
                   (let ((o (+ dbase (* j dstride))) (b (,rowbase j)))
                     (declare (type fixnum o b))
                     (if avg
                         (dotimes (i bw)
                           (setf (aref dst (+ o i))
                                 (ash (+ (aref dst (+ o i)) (aref ,src (+ b i)) 1) -1)))
                         (replace dst ,src :start1 o :end1 (+ o bw)
                                           :start2 b :end2 (+ b bw))))))
                ((zerop py)
                 (dotimes (j bh)
                   (let ((o (+ dbase (* j dstride))) (b (,rowbase j)))
                     (declare (type fixnum o b))
                     (dotimes (i bw)
                       (let ((k (+ b i))
                             (v 0))
                         (declare (type fixnum k v))
                         (setf v (%tap8 f fxb (aref ,src (- k 3)) (aref ,src (- k 2))
                                        (aref ,src (- k 1)) (aref ,src k) (aref ,src (+ k 1))
                                        (aref ,src (+ k 2)) (aref ,src (+ k 3))
                                        (aref ,src (+ k 4))))
                         (setf (aref dst (+ o i))
                               (if avg (ash (+ (aref dst (+ o i)) v 1) -1) v)))))))
                ((zerop px)
                 (dotimes (j bh)
                   (let* ((o (+ dbase (* j dstride)))
                          (b0 (,rowbase (- j 3))) (b1 (+ b0 step)) (b2 (+ b1 step))
                          (b3 (+ b2 step)) (b4 (+ b3 step)) (b5 (+ b4 step))
                          (b6 (+ b5 step)) (b7 (+ b6 step)))
                     (declare (type fixnum o b0 b1 b2 b3 b4 b5 b6 b7))
                     (dotimes (i bw)
                       (let ((v (%tap8 f fyb (aref ,src (+ b0 i)) (aref ,src (+ b1 i))
                                       (aref ,src (+ b2 i)) (aref ,src (+ b3 i))
                                       (aref ,src (+ b4 i)) (aref ,src (+ b5 i))
                                       (aref ,src (+ b6 i)) (aref ,src (+ b7 i)))))
                         (declare (type fixnum v))
                         (setf (aref dst (+ o i))
                               (if avg (ash (+ (aref dst (+ o i)) v 1) -1) v)))))))
                (t
                 ;; horizontal first, over every row the vertical pass will need, and THE
                 ;; INTERMEDIATE IS CLIPPED TO A SAMPLE — which is what makes the two passes
                 ;; separable at all; a wider intermediate would not agree
                 (dotimes (j (+ bh 7))
                   (let ((b (,rowbase (- j 3))) (o (* j bw)))
                     (declare (type fixnum b o))
                     (dotimes (i bw)
                       (let ((k (+ b i)))
                         (declare (type fixnum k))
                         (setf (aref tmp (+ o i))
                               (%tap8 f fxb (aref ,src (- k 3)) (aref ,src (- k 2))
                                      (aref ,src (- k 1)) (aref ,src k) (aref ,src (+ k 1))
                                      (aref ,src (+ k 2)) (aref ,src (+ k 3))
                                      (aref ,src (+ k 4))))))))
                 (dotimes (j bh)
                   (let* ((o (+ dbase (* j dstride)))
                          (t0 (* j bw)) (t1 (+ t0 bw)) (t2 (+ t1 bw)) (t3 (+ t2 bw))
                          (t4 (+ t3 bw)) (t5 (+ t4 bw)) (t6 (+ t5 bw)) (t7 (+ t6 bw)))
                     (declare (type fixnum o t0 t1 t2 t3 t4 t5 t6 t7))
                     (dotimes (i bw)
                       (let ((v (%tap8 f fyb (aref tmp (+ t0 i)) (aref tmp (+ t1 i))
                                       (aref tmp (+ t2 i)) (aref tmp (+ t3 i))
                                       (aref tmp (+ t4 i)) (aref tmp (+ t5 i))
                                       (aref tmp (+ t6 i)) (aref tmp (+ t7 i)))))
                         (declare (type fixnum v))
                         (setf (aref dst (+ o i))
                               (if avg (ash (+ (aref dst (+ o i)) v 1) -1) v))))))))))
         (direct-row (j) `(+ (* (+ iy ,j) rstride) ix))
         (clamped-row (j) `(+ (* (+ ,j 3) ww) 3)))
      (if inside
          (run ref direct-row rstride)
          (progn
            (%gather-window win ref rstride rw rh (- ix 3) (- iy 3) ww (+ bh 7))
            (run win clamped-row ww))))
    (values)))

(declaim (inline %rounded-div))
(defun %rounded-div (a b)
  "Round to nearest, away from zero, which is what averaging motion vectors wants."
  (declare (type fixnum a b) (optimize (speed 3) (safety 0)))
  (if (>= a 0) (floor (+ a (ash b -1)) b) (- (floor (+ (- a) (ash b -1)) b))))

;;;; ffv1/golomb.lisp — FFV1's other entropy coder.
;;;;
;;;; FFV1 has two, chosen per file, and they share nothing but the predictor above them.  The range
;;;; coder is what `-coder 1' selects and what archives use; this is what `-coder 0' selects, and it
;;;; is the DEFAULT — so a file made by someone who did not pass the flag is coded this way.
;;;;
;;;; It is Rice coding with an adaptive parameter, which is to say LOCO-I's, which is to say JPEG-LS's.
;;;; Each context keeps four running numbers: how many samples it has seen, the sum of the absolute
;;;; residuals, the accumulated signed drift, and a bias.  The count and the error sum give the Rice
;;;; parameter by the oldest trick in the family — double the count until it exceeds the error sum,
;;;; and the number of doublings is k.  The drift and the bias correct for the predictor being
;;;; consistently a little high or a little low in this context, which the range coder gets for free
;;;; from its adaptive models and this coder has to be told.
;;;;
;;;; The RUN MODE is the part with no counterpart on the other side.  In the flattest context — the
;;;; one where every neighbour agreed — samples are not coded individually at all: a run length is
;;;; coded, and the samples in it are whatever the predictor says.  On synthetic images and on the
;;;; flat borders of real ones that is most of the picture, and it is the reason this coder is
;;;; competitive with the range coder at all.

(in-package #:reel.ffv1)

;;; ---- a plain bit reader, most significant bit first ----------------------------------------------

(defstruct (bits (:conc-name bits-))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)                          ; in BITS, from the start of DATA
  (end 0 :type fixnum))

(defun make-bits-over (bytes start end)
  (declare (type octets bytes) (type fixnum start end))
  (make-bits :data bytes :pos (* 8 start) :end (* 8 end)))

(declaim (inline %peek32 %skip-bits %read-bits %read-bit))

(defun %peek32 (b)
  "The next thirty-two bits, left aligned, zero filled past the end.

   Thirty-two and not fewer because the Rice code's length is not known until its leading zeros have
   been counted, and a run of them may be as long as the escape threshold allows."
  (declare (type bits b) (optimize (speed 3) (safety 0)))
  (let* ((data (bits-data b)) (pos (bits-pos b))
         (byte (ash pos -3)) (bit (logand pos 7))
         (len (length data)) (v 0))
    (declare (type fixnum byte bit len) (type (unsigned-byte 40) v))
    (dotimes (i 5)
      (let ((k (+ byte i)))
        (setf v (logior (ash v 8) (if (< k len) (aref data k) 0)))))
    (logand (ash v (- (- 8 bit))) #xffffffff)))

(defun %skip-bits (b n)
  (declare (type bits b) (type fixnum n) (optimize (speed 3) (safety 0)))
  (incf (bits-pos b) n))

(defun %read-bits (b n)
  (declare (type bits b) (type (integer 0 32) n) (optimize (speed 3) (safety 0)))
  (if (zerop n)
      0
      (prog1 (ash (%peek32 b) (- (- 32 n))) (incf (bits-pos b) n))))

(defun %read-bit (b)
  (declare (type bits b) (optimize (speed 3) (safety 0)))
  (let* ((pos (bits-pos b)) (data (bits-data b)) (byte (ash pos -3)))
    (declare (type fixnum pos byte))
    (setf (bits-pos b) (1+ pos))
    (if (< byte (length data))
        (logand (ash (aref data byte) (- (- 7 (logand pos 7)))) 1)
        0)))

;;; ---- the Rice code ------------------------------------------------------------------------------

(defconstant +golomb-limit+ 12
  "How many leading zeros are allowed before the code escapes to a fixed-width field.  Twelve is not
   a tuning choice a decoder may revisit: it is what the encoder counted to.")

(defun %get-ur-golomb (b k esc-len)
  "An unsigned Rice code with parameter K: leading zeros, a one, then K bits — unless there are
   LIMIT of them, in which case an ESC-LEN-bit field follows instead and the value is offset."
  (declare (type bits b) (type fixnum k esc-len) (optimize (speed 3) (safety 1)))
  (let* ((buf (%peek32 b))
         (log (1- (integer-length buf))))
    (declare (type (unsigned-byte 32) buf) (type fixnum log))
    (if (> log (- 31 +golomb-limit+))
        ;; the ordinary path: (31 - log) zeros, then the one, then K bits
        (let ((v (+ (ash buf (- (- log k))) (ash (- 30 log) k))))
          (declare (type fixnum v))
          (%skip-bits b (+ 32 k (- log)))
          v)
        (progn (%skip-bits b +golomb-limit+)
               (+ (%read-bits b esc-len) +golomb-limit+ -1)))))

(declaim (inline %fold))
(defun %fold (diff bits)
  "Wrap a residual back into the range one sample can hold, as a signed value.

   The residual of a prediction is taken modulo the sample range, so that a wrap-around costs the
   same as a small difference rather than the width of the whole range."
  (declare (type fixnum diff bits) (optimize (speed 3) (safety 0)))
  (let ((mask (1- (ash 1 bits))))
    (declare (type fixnum mask))
    (let ((v (logand diff mask)))
      (if (>= v (ash 1 (1- bits))) (- v (ash 1 bits)) v))))

(defun %get-vlc-symbol (b state ctx bits)
  "One residual from context CTX, and the four running numbers updated by what it turned out to be.

   STATE holds, per context: the sum of absolute residuals, the signed drift, the bias, and the
   count.  The Rice parameter falls out of the first and the last; the middle two carry the
   correction for a predictor that is biased in this context, and the correction is applied by
   ADDING the bias and, separately, by conditionally inverting the sign — the second of which is
   the one that looks like a typo and is not."
  (declare (type bits b) (type (simple-array (signed-byte 32) (* 4)) state)
           (type fixnum ctx bits) (optimize (speed 3) (safety 1)))
  (let* ((error-sum (aref state ctx 0)) (drift (aref state ctx 1))
         (bias (aref state ctx 2)) (count (aref state ctx 3))
         (i count) (k 0))
    (declare (type fixnum error-sum drift bias count i k))
    (loop while (< i error-sum) do (incf k) (incf i i))
    (when (> k bits) (setf k bits))
    (let ((v (let ((u (%get-ur-golomb b k bits)))
               (declare (type fixnum u))
               (logxor (ash u -1) (- (logand u 1))))))
      (declare (type fixnum v))
      ;; the sign of `2*drift + count' inverts the residual: where the predictor has been running
      ;; low, a positive code means a negative residual and the encoder relied on it
      (when (minusp (+ (* 2 drift) count)) (setf v (lognot v)))
      (let ((ret (%fold (+ v bias) bits)))
        (declare (type fixnum ret))
        ;; ---- and now the update, which is JPEG-LS's exactly
        (incf error-sum (abs v))
        (incf drift v)
        (when (= count 128)
          (setf count (ash count -1) drift (ash drift -1) error-sum (ash error-sum -1)))
        (incf count)
        (cond ((<= drift (- count))
               (setf bias (max (1- bias) -128))
               (setf drift (max (+ drift count) (- 1 count))))
              ((plusp drift)
               (setf bias (min (1+ bias) 127))
               (setf drift (min (- drift count) 0))))
        (setf (aref state ctx 0) error-sum (aref state ctx 1) drift
              (aref state ctx 2) bias (aref state ctx 3) count)
        ret))))

(defparameter +log2-run+
  (make-array 41 :element-type '(signed-byte 32) :initial-contents
   '(0  0  0  0  1  1  1  1
     2  2  2  2  3  3  3  3
     4  4  5  5  6  6  7  7
     8  9 10 11 12 13 14 15
    16 17 18 19 20 21 22 23
    24))
  "How many bits a run length is worth at each level of the run index: the index climbs while runs
   keep succeeding and falls when one is cut short, so a flat picture reaches runs of sixteen
   million and a busy one never leaves the first few entries.")
(declaim (type (simple-array (signed-byte 32) (41)) +log2-run+))

(defun %fresh-vlc-states (count)
  "One context's four numbers, COUNT times.  The error sum starts at four and the count at one,
   which together mean a Rice parameter of two before anything has been seen."
  (let ((s (make-array (list (max 1 count) 4) :element-type '(signed-byte 32) :initial-element 0)))
    (dotimes (i (max 1 count) s)
      (setf (aref s i 0) 4 (aref s i 3) 1))))

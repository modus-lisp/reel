;;;; ffv1/rangecoder.lisp — the binary range coder FFV1 is built on, and the symbols above it.
;;;;
;;;; A RANGE CODER IS AN ARITHMETIC CODER WITH BYTES INSTEAD OF BITS, and FFV1's is a small one: an
;;;; interval, a probability held as one byte per context, and a renormalisation that emits or
;;;; consumes a whole byte at a time.  What makes it work as well as it does is the STATE TABLE —
;;;; each context's byte is not a probability that gets nudged, it is an index into a transition
;;;; table that says where to go after a zero and after a one.
;;;;
;;;; That table is not transmitted.  It is built from a single parameter, and the construction below
;;;; is a transcription of the specification's: walk a probability from one half towards one,
;;;; recording where each step lands, then fill the gaps, then mirror for the zero direction.
;;;;
;;;; Above it sits a symbol coder that spells an integer as: is it zero; if not, how many bits it
;;;; has, in unary; then those bits; then its sign.  Each of those questions gets its OWN context,
;;;; which is why a "state" here is a 32-byte array rather than one byte.

(in-package #:reel.ffv1)

(define-condition ffv1-error (error)
  ((message :initarg :message :reader ffv1-error-message))
  (:report (lambda (c s) (format s "reel/ffv1: ~a" (ffv1-error-message c)))))

(defun %err (fmt &rest args)
  (error 'ffv1-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defconstant +context-size+ 32
  "How many range-coder contexts one symbol costs: one for zero, ten for the length, eleven for the
   sign, ten for the bits.")

(defstruct (rangecoder (:conc-name rc-))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)
  (end 0 :type fixnum)
  (low 0 :type fixnum)
  (range #xff00 :type fixnum)
  (overread 0 :type fixnum)
  (zero-state (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)
              :type (simple-array (unsigned-byte 8) (256)))
  (one-state (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)
             :type (simple-array (unsigned-byte 8) (256))))

(defun build-rac-states (c factor max-p)
  "Fill the transition tables from one parameter (the specification's `build_rac_states').

   FACTOR is how fast a context's estimate moves; MAX-P bounds how certain it may become, because a
   context that reaches certainty can never recover from being wrong."
  (declare (type rangecoder c) (type integer factor) (type fixnum max-p))
  (let ((one (ash 1 32))
        (zs (rc-zero-state c)) (os (rc-one-state c)))
    (fill zs 0) (fill os 0)
    (let ((last-p8 0) (p (ash one -1)))
      (declare (type integer p) (type fixnum last-p8))
      (dotimes (i 128)
        (let ((p8 (ash (+ (* 256 p) (ash one -1)) -32)))
          (declare (type fixnum p8))
          (when (<= p8 last-p8) (setf p8 (1+ last-p8)))
          (when (and (plusp last-p8) (< last-p8 256) (<= p8 max-p))
            (setf (aref os last-p8) p8))
          (incf p (ash (+ (* (- one p) factor) (ash one -1)) -32))
          (setf last-p8 p8))))
    (loop for i of-type fixnum from (- 256 max-p) to max-p
          do (when (zerop (aref os i))
               (let* ((p (ash (+ (* i one) 128) -8))
                      (p2 (+ p (ash (+ (* (- one p) factor) (ash one -1)) -32)))
                      (p8 (ash (+ (* 256 p2) (ash one -1)) -32)))
                 (declare (type integer p p2) (type fixnum p8))
                 (when (<= p8 i) (setf p8 (1+ i)))
                 (when (> p8 max-p) (setf p8 max-p))
                 (setf (aref os i) p8))))
    ;; the zero direction is the one direction mirrored.  Where the mirror lands on an unused entry
    ;; the difference is 256, which is a zero in a byte — and an unused entry is exactly what it
    ;; should be, so the wrap is the answer rather than an overflow to guard against.
    (loop for i of-type fixnum from 1 below 255
          do (setf (aref zs i) (logand (- 256 (aref os (- 256 i))) 255)))
    c))

(defun make-decoder-over (bytes start end)
  "A range decoder over BYTES[START,END) with the default state table."
  (declare (type octets bytes) (type fixnum start end))
  (let ((c (make-rangecoder :data bytes :pos start :end end)))
    ;; TRUNCATED, NOT ROUNDED.  The specification's constant is 0.05 of two to the thirty-two, which
    ;; is 214748364.8, and it is passed as an integer — so it is 214748364 and not 214748365.  One
    ;; part in two hundred million, and it moves an entry or two of the state table, and the
    ;; arithmetic decoder then diverges within a few hundred symbols.
    (build-rac-states c (truncate (* 0.05d0 (ash 1 32))) (- 256 8))
    ;; the first two bytes prime the interval
    (setf (rc-low c) (logior (ash (if (< start end) (aref bytes start) 0) 8)
                             (if (< (1+ start) end) (aref bytes (1+ start)) 0))
          (rc-pos c) (+ start 2)
          (rc-range c) #xff00)
    (when (>= (rc-low c) #xff00)
      (setf (rc-low c) #xff00 (rc-end c) (rc-pos c)))
    c))

(declaim (inline %refill get-rac))
(defun %refill (c)
  (declare (type rangecoder c) (optimize (speed 3) (safety 0)))
  (setf (rc-range c) (ash (rc-range c) 8)
        (rc-low c) (ash (rc-low c) 8))
  (if (< (rc-pos c) (rc-end c))
      (progn (incf (rc-low c) (aref (rc-data c) (rc-pos c)))
             (incf (rc-pos c)))
      (incf (rc-overread c))))

(defun get-rac (c state i)
  "One bit, against the context at index I of STATE."
  (declare (type rangecoder c) (type (simple-array (unsigned-byte 8) (*)) state)
           (type fixnum i) (optimize (speed 3) (safety 0)))
  (let* ((s (aref state i))
         (r1 (ash (* (rc-range c) s) -8)))
    (declare (type fixnum s r1))
    (decf (rc-range c) r1)
    (cond
      ((< (rc-low c) (rc-range c))
       (setf (aref state i) (aref (rc-zero-state c) s))
       (when (< (rc-range c) #x100) (%refill c))
       0)
      (t
       (decf (rc-low c) (rc-range c))
       (setf (aref state i) (aref (rc-one-state c) s))
       (setf (rc-range c) r1)
       (when (< (rc-range c) #x100) (%refill c))
       1))))

(defun get-symbol (c state signed-p)
  "One integer, spelled as zero-or-not, then its length in unary, then its bits, then its sign.

   The contexts are not shared between those questions and that is the whole design: `how many bits
   does this number have' is a very different distribution from `what is bit three', and giving them
   one context each would average two things that have nothing to do with each other."
  (declare (type rangecoder c) (type (simple-array (unsigned-byte 8) (*)) state)
           (optimize (speed 3) (safety 1)))
  (if (= 1 (get-rac c state 0))
      0
      (let ((e 0) (a 1))
        (declare (type fixnum e a))
        (loop while (= 1 (get-rac c state (+ 1 (min e 9))))
              do (incf e)
                 (when (> e 31) (%err "a symbol claiming more than 32 bits")))
        (loop for i of-type fixnum from (1- e) downto 0
              do (setf a (+ a a (get-rac c state (+ 22 (min i 9))))))
        (if (and signed-p (= 1 (get-rac c state (+ 11 (min e 10)))))
            (- a)
            a))))

(defun fresh-state ()
  (make-array +context-size+ :element-type '(unsigned-byte 8) :initial-element 128))

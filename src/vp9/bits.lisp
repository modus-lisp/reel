;;;; vp9/bits.lisp — the two readers VP9 uses, which are not the same reader.
;;;;
;;;; A VP9 frame has an UNCOMPRESSED header read as plain bits, and everything after it read through
;;;; a binary arithmetic coder.  The split is unusual and deliberate: the uncompressed part carries
;;;; the frame size, the reference indices, the quantiser and the loop filter — everything a
;;;; container or a router might want to know without decoding — and it is readable with no state at
;;;; all.  Everything that costs bits is on the other side of the arithmetic coder.
;;;;
;;;; The arithmetic coder is VP8's, unchanged: an interval held in eight bits, a probability per
;;;; decision held in another eight, and a renormalisation that shifts in whole bytes.  What VP9 adds
;;;; above it is the TREE — a decision is rarely a single bit, and a symbol is read by walking a
;;;; small binary tree with one probability per node.

(in-package #:reel.vp9)

(define-condition vp9-error (error)
  ((message :initarg :message :reader vp9-error-message))
  (:report (lambda (c s) (format s "reel/vp9: ~a" (vp9-error-message c)))))

(defun %err (fmt &rest args)
  (error 'vp9-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun %o (n)
  "A zeroed octet vector, which is what every context and edge buffer here is."
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
;;; ---- why every small-integer array here is (SIGNED-BYTE 32) -------------------------------------
;;;
;;; SBCL STORES A `fixnum' ARRAY AS (SIGNED-BYTE 64), so (aref a i) on one has the type
;;; (signed-byte 64) and not FIXNUM — a fixnum is only sixty-two bits, and the compiler cannot prove
;;; that the sixty-four bit value it just loaded is one.  Every sum, product and shift of two such
;;; values is therefore an out-of-line call into the generic arithmetic, and the decoder is nothing
;;; but sums of values loaded from arrays.  Declaring the arrays thirty-two bits wide instead — which
;;; is what they hold: samples, levels, strides, motion vectors, coefficients, counts — makes the
;;; whole of that arithmetic inline, and halves the memory it touches.  It is worth about a quarter
;;; of the decode time and it is invisible: nothing about the source looks different.

(deftype dim ()
  "A picture dimension, a stride, or a coordinate in eight-sample units.

   VP9 states a frame's width and height in sixteen bits, so 65536 is the largest either can be and
   a superblock-aligned plane is no larger.  Saying so lets SBCL multiply a row by a stride without
   allowing for a bignum, which is the difference between an inline shift-and-add and an out-of-line
   call — and the index arithmetic around motion compensation does that several times per block."
  '(integer 0 65536))

(deftype coefs ()
  "A coefficient buffer.

   THIRTY-TWO BITS IS THE FORMAT'S OWN CONTRACT, not a choice made here: ffmpeg computes the inverse
   transforms in C `int', and the specification constrains a conformant stream so that no
   intermediate leaves that range.  Saying so in the type is what lets SBCL multiply a coefficient
   by a fourteen-bit cosine without checking for a bignum on every one of the six hundred
   multiplies a 32x32 transform performs — and halves the memory the buffer touches besides."
  '(simple-array (signed-byte 32) (*)))

;;; ---- plain bits, most significant first ----------------------------------------------------------

(defstruct (bitreader (:conc-name br-))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)                          ; in bits
  (end 0 :type fixnum))

(defun make-br (bytes &key (start 0) (end (length bytes)))
  (declare (type octets bytes))
  (make-bitreader :data bytes :pos (* 8 start) :end (* 8 end)))

(declaim (inline read-bit read-bits))
(defun read-bit (br)
  (declare (type bitreader br) (optimize (speed 3) (safety 0)))
  (let* ((pos (br-pos br)) (data (br-data br)) (byte (ash pos -3)))
    (declare (type fixnum pos byte))
    (setf (br-pos br) (1+ pos))
    (if (< byte (length data))
        (logand (ash (aref data byte) (- (- 7 (logand pos 7)))) 1)
        0)))

(defun read-bits (br n)
  (declare (type bitreader br) (type (integer 0 32) n) (optimize (speed 3) (safety 1)))
  (let ((v 0))
    (declare (type (unsigned-byte 40) v))
    (dotimes (i n v) (setf v (logior (ash v 1) (read-bit br))))))

(defun read-signed (br n)
  "N bits of magnitude and then a sign bit, which is how every delta in the header is coded.

   Not two's complement: the sign is a separate bit AFTER the value, so minus zero is expressible
   and means zero."
  (declare (type bitreader br) (type (integer 0 32) n))
  (let ((v (read-bits br n)))
    (if (plusp (read-bit br)) (- v) v)))

;;; ---- the arithmetic coder ------------------------------------------------------------------------

(defstruct (bool (:conc-name bd-) (:constructor %make-bool))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum) (end 0 :type fixnum)
  (high 255 :type (unsigned-byte 8))            ; the interval, eight bits
  (code 0 :type (unsigned-byte 32))
  (bits -16 :type (integer -32 32)))

(defun make-bool (bytes start end)
  "A decoder over BYTES[START,END).  The first three bytes prime the code word."
  (declare (type octets bytes) (type fixnum start end))
  (when (< (- end start) 1) (%err "an empty arithmetic-coded partition"))
  (let ((c (%make-bool :data bytes :pos (+ start 3) :end end)))
    (setf (bd-code c)
          (logior (ash (if (< start end) (aref bytes start) 0) 16)
                  (ash (if (< (+ start 1) end) (aref bytes (+ start 1)) 0) 8)
                  (if (< (+ start 2) end) (aref bytes (+ start 2)) 0)))
    c))

(declaim (inline %norm-shift bool-bit))
(defun %norm-shift (high)
  "How far the interval must move left to put its top bit back at position seven."
  (declare (type (unsigned-byte 8) high) (optimize (speed 3) (safety 0)))
  (if (zerop high) 8 (- 8 (integer-length high))))

(defun bool-bit (c prob)
  "One decision against PROB, which is the probability out of 256 that the answer is ZERO."
  (declare (type bool c) (type (unsigned-byte 8) prob) (optimize (speed 3) (safety 0)))
  ;; ---- renormalise first, not last: the interval from the previous decision may be as narrow as
  ;; one, and the split below needs eight significant bits to divide
  (let* ((shift (%norm-shift (bd-high c)))
         ;; the shift puts the interval's top bit back at position seven, so HIGH is in [128,255]
         ;; here and needs no mask; the code word does, because C wraps it at thirty-two bits and
         ;; a stream that reads past its end relies on that
         (high (ash (bd-high c) shift))
         (code (logand (ash (bd-code c) shift) #xffffffff))
         (bits (+ (bd-bits c) shift)))
    (declare (type (integer 0 8) shift) (type (unsigned-byte 16) high)
             (type (integer -32 32) bits) (type (unsigned-byte 32) code))
    (when (and (>= bits 0) (< (bd-pos c) (bd-end c)))
      (let* ((p (bd-pos c))
             (b0 (aref (bd-data c) p))
             (b1 (if (< (1+ p) (bd-end c)) (aref (bd-data c) (1+ p)) 0)))
        (setf code (logand (logior code (ash (logior (ash b0 8) b1) bits)) #xffffffff)
              (bd-pos c) (+ p 2)
              bits (- bits 16))))
    ;; ---- and now the split.  LOW is where the interval divides; the code word landing above it
    ;; is a one, below it a zero, and the interval becomes whichever side that was.
    (let* ((low (+ 1 (ash (* (1- high) prob) -8)))
           (lowsh (ash low 16))
           (bit (if (>= code lowsh) 1 0)))
      (declare (type fixnum low lowsh) (type (unsigned-byte 1) bit))
      (setf (bd-high c) (if (plusp bit) (- high low) low)
            (bd-code c) (if (plusp bit) (- code lowsh) code)
            (bd-bits c) bits)
      bit)))

(declaim (inline bool-flag))
(defun bool-flag (c)
  "One bit with no model: an even chance, which is how VP9 codes a literal."
  (declare (type bool c) (optimize (speed 3) (safety 0)))
  (bool-bit c 128))

(defun bool-literal (c n)
  "N bits, most significant first, each an even chance."
  (declare (type bool c) (type (integer 0 32) n) (optimize (speed 3) (safety 1)))
  (let ((v 0))
    (declare (type (unsigned-byte 40) v))
    (dotimes (i n v) (setf v (logior (ash v 1) (bool-flag c))))))

(defun bool-signed (c n)
  "A literal and then a sign bit."
  (let ((v (bool-literal c n)))
    (if (plusp (bool-flag c)) (- v) v)))

(defun bool-tree (c tree probs base)
  "One symbol, by walking TREE with one probability per node.

   A strictly positive entry is the next node; anything else is the negated symbol.  Node zero is
   the root and is never a branch target, which is what frees zero to mean symbol zero.

   PROBS is any probability array and BASE the row-major index of the row this tree reads, because
   VP9\'s models are indexed by three and four dimensions of context and a tree walks one row of
   whichever of them the caller has already selected."
  (declare (type bool c) (type (simple-array (signed-byte 32) (* 2)) tree)
           (type (simple-array (unsigned-byte 8)) probs) (type fixnum base)
           (optimize (speed 3) (safety 1)))
  (let ((i 0))
    (declare (type fixnum i))
    (loop (setf i (aref tree i (bool-bit c (row-major-aref probs (+ base i)))))
          (unless (plusp i) (return (- i))))))

(defun bool-end-p (c)
  "Has the coder read past the end of its partition?  A conforming stream never does."
  (declare (type bool c))
  (and (>= (bd-pos c) (bd-end c)) (>= (bd-bits c) 0)))

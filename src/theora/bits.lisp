;;;; theora/bits.lisp — the bit reader, and the canonical Huffman codes above it.
;;;;
;;;; Theora reads bits most-significant first, like everything else here.  What is unusual is how it
;;;; transmits its Huffman tables: not as code lengths, and not as codes, but as a TREE — a bit
;;;; saying "leaf" or "branch", and a leaf carries a five-bit token.  Eighty of them, in the setup
;;;; header, one after another.
;;;;
;;;; The tables that are NOT transmitted — run lengths, modes, motion vectors — are given as code
;;;; lengths only, and the codes are the canonical assignment for those lengths.  So there are two
;;;; builders here and they meet in the same lookup structure.

(in-package #:reel.theora)

(define-condition theora-error (error)
  ((message :initarg :message :reader theora-error-message))
  (:report (lambda (c s) (format s "reel/theora: ~a" (theora-error-message c)))))

(defun %err (fmt &rest args)
  (error 'theora-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))
(deftype fixnums () '(simple-array fixnum (*)))

(defstruct (bitreader (:conc-name br-))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)
  (end 0 :type fixnum))

(defun make-br (bytes &key (start 0) (end (length bytes)))
  (make-bitreader :data bytes :pos (* 8 start) :end (* 8 end)))

(declaim (inline br-eof-p peek-bits read-bits read-bit))
(defun br-eof-p (br) (>= (br-pos br) (br-end br)))

(defun peek-bits (br n)
  (declare (type bitreader br) (type (integer 0 24) n) (optimize (speed 3) (safety 0)))
  (let* ((data (br-data br)) (pos (br-pos br))
         (byte (ash pos -3)) (bit (logand pos 7))
         (len (length data)) (v 0))
    (declare (type fixnum byte bit len) (type (unsigned-byte 32) v))
    (dotimes (i 4)
      (let ((k (+ byte i)))
        (setf v (logior (ash v 8) (if (< k len) (aref data k) 0)))))
    (logand (ash v (- (- 32 bit n))) (1- (ash 1 n)))))

(defun read-bits (br n)
  (declare (type bitreader br) (type (integer 0 24) n) (optimize (speed 3) (safety 0)))
  (let ((v (peek-bits br n))) (incf (br-pos br) n) v))

(defun read-bit (br)
  (declare (type bitreader br) (optimize (speed 3) (safety 0)))
  (let* ((pos (br-pos br)) (data (br-data br)) (byte (ash pos -3)))
    (declare (type fixnum pos byte))
    (setf (br-pos br) (1+ pos))
    (if (< byte (length data))
        (logand (ash (aref data byte) (- (- 7 (logand pos 7)))) 1)
        0)))

(defun read-bits-long (br n)
  "More than twenty-four bits, which the frame rate and aspect ratio fields need."
  (let ((v 0))
    (loop while (plusp n)
          do (let ((k (min n 16)))
               (setf v (logior (ash v k) (read-bits br k)))
               (decf n k)))
    v))

;;; ---- Huffman ------------------------------------------------------------------------------------

(defstruct (huff (:conc-name hf-))
  (bits 0 :type fixnum)
  (table (make-array 0 :element-type 'fixnum) :type fixnums))

(defun build-huff (entries)
  "A lookup table from a list of (value length), peeked and indexed.

   Each cell holds (value << 6) | length; zero means no code matches.  Six bits for the length
   because a Theora coefficient code may be thirty-two bits long, which no other table here comes
   close to."
  ;; the LENGTH is the third field, not the second — the second is the token, and tokens run to 31,
  ;; which reads as a thirty-one-bit code and refuses a table that is perfectly well formed
  (let ((maxlen (reduce #'max entries :key #'third :initial-value 1)))
    (when (> maxlen 20) (%err "a Huffman code of ~d bits" maxlen))
    (let ((out (make-array (ash 1 maxlen) :element-type 'fixnum :initial-element 0)))
      (loop for e in entries
            do (destructuring-bind (code value len) e
                 (when (plusp len)
                   (let ((base (ash code (- maxlen len)))
                         (span (ash 1 (- maxlen len)))
                         (cell (logior (ash value 6) len)))
                     (dotimes (k span)
                       (unless (zerop (aref out (+ base k)))
                         (%err "a Huffman table that is not prefix-free"))
                       (setf (aref out (+ base k)) cell))))))
      (make-huff :bits maxlen :table out))))

(defun canonical-huff (lengths &key (values nil))
  "The canonical Huffman code for a list of lengths: shortest first, and within a length in order.

   This is the assignment the specification means when it prints a table of lengths and no codes,
   and it is the only assignment that both sides can agree on without transmitting anything."
  (let ((entries '()) (code 0) (prev 0))
    (let ((order (sort (loop for len across lengths for i from 0
                             when (plusp len) collect (cons len i))
                       (lambda (a b) (or (< (car a) (car b))
                                         (and (= (car a) (car b)) (< (cdr a) (cdr b))))))))
      (dolist (e order)
        (destructuring-bind (len . i) e
          (setf code (ash code (- len prev)) prev len)
          (push (list code (if values (aref values i) i) len) entries)
          (incf code))))
    (build-huff (nreverse entries))))

(declaim (inline read-huff))
(defun read-huff (br h)
  "One code from H.  Returns the value, or NIL if the bits ahead match nothing."
  (declare (type bitreader br) (type huff h) (optimize (speed 3) (safety 1)))
  (let* ((tbl (hf-table h))
         (cell (aref tbl (peek-bits br (hf-bits h)))))
    (declare (type fixnum cell))
    (if (zerop cell)
        nil
        (progn (incf (br-pos br) (logand cell 63))
               (ash cell -6)))))

(defun read-huff-tree (br)
  "One of the setup header's eighty coefficient tables, sent as a TREE.

   A one bit is a leaf carrying a five-bit token; a zero bit is a branch, and its two children
   follow.  So the code for a token is the path to it, and the table is transmitted in about a
   hundred bits rather than as thirty-two lengths."
  (let ((entries '()) (count 0))
    (labels ((walk (code len)
               (when (> len 31) (%err "a coefficient table deeper than thirty-two levels"))
               (if (= 1 (read-bit br))
                   (progn
                     (when (>= count 32) (%err "a coefficient table with more than 32 leaves"))
                     (incf count)
                     (push (list code (read-bits br 5) len) entries))
                   (progn (walk (ash code 1) (1+ len))
                          (walk (logior (ash code 1) 1) (1+ len))))))
      (walk 0 0))
    (build-huff (nreverse entries))))

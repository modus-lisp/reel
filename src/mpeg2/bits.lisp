;;;; mpeg2/bits.lisp — start codes, the bit reader, and the Huffman machinery above it.
;;;;
;;;; MPEG-2 is much older than H.264 and it shows here, mostly in ways that make this file SHORTER.
;;;; There is no emulation prevention: a start code is 00 00 01 and an encoder simply guarantees
;;;; that pattern never occurs inside coded data, so a decoder can scan for start codes in the raw
;;;; bytes and read the payload without unescaping anything first.
;;;;
;;;; What replaces it is variable length codes everywhere.  H.264 has one universal code — Exp-Golomb
;;;; — that every syntax element is spelled in, and CABAC for the rest.  MPEG-2 has EIGHT separate
;;;; Huffman tables, printed in the specification, and every one of them is decoded by peeking as
;;;; many bits as the longest code and indexing.  That is the whole reason this file has a table
;;;; builder in it: a linear walk down a Huffman table is correct and was 22% of decode time when
;;;; the H.264 decoder did it that way.

(in-package #:reel.mpeg2)

(define-condition mpeg2-error (error)
  ((message :initarg :message :reader mpeg2-error-message))
  (:report (lambda (c s) (format s "reel/mpeg2: ~a" (mpeg2-error-message c)))))

(defun %err (fmt &rest args)
  (error 'mpeg2-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))
(deftype fixnums () '(simple-array fixnum (*)))

;;; ---- start codes (Table 6-1) -----------------------------------------------------------------

(defconstant +sc-picture+ #x00)
(defconstant +sc-user-data+ #xb2)
(defconstant +sc-sequence+ #xb3)
(defconstant +sc-sequence-error+ #xb4)
(defconstant +sc-extension+ #xb5)
(defconstant +sc-sequence-end+ #xb7)
(defconstant +sc-group+ #xb8)
(declaim (inline slice-start-code-p))
(defun slice-start-code-p (v) (and (>= v 1) (<= v #xaf)))

(defun find-start-code (bytes from)
  "The index of the 00 00 01 at or after FROM, or NIL.  Returns the index of the first zero."
  (declare (type octets bytes) (type fixnum from) (optimize (speed 3) (safety 1)))
  (let ((n (length bytes)))
    (loop for i of-type fixnum from (max 0 from) below (- n 3)
          do (when (and (zerop (aref bytes i))
                        (zerop (aref bytes (+ i 1)))
                        (= 1 (aref bytes (+ i 2))))
               (return i)))))

;;; ---- the bit reader -----------------------------------------------------------------------
;;;
;;; Bit position rather than byte position plus offset, because every syntax element here is some
;;; number of bits and almost none of them are a whole number of bytes.

(defstruct (bitreader (:conc-name br-))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)                          ; in BITS
  (end 0 :type fixnum))                         ; in BITS

(defun make-br (bytes &key (start 0) (end (length bytes)))
  (make-bitreader :data bytes :pos (* 8 start) :end (* 8 end)))

(declaim (inline br-eof-p))
(defun br-eof-p (br) (>= (br-pos br) (br-end br)))

(declaim (inline peek-bits))
(defun peek-bits (br n)
  "The next N bits (N <= 24) as an integer, without consuming them.  Past the end reads as zero,
   which is what lets a decoder run off a truncated slice and stop rather than signal."
  (declare (type bitreader br) (type (integer 0 24) n) (optimize (speed 3) (safety 1)))
  (let* ((data (br-data br)) (pos (br-pos br))
         (byte (ash pos -3)) (bit (logand pos 7))
         (len (length data))
         (v 0))
    (declare (type fixnum byte bit len) (type (unsigned-byte 32) v))
    ;; four bytes always cover 24 bits from any bit offset, and reading past the end as zero is
    ;; what lets a truncated slice stop cleanly instead of signalling
    (dotimes (i 4)
      (let ((k (+ byte i)))
        (setf v (logior (ash v 8) (if (< k len) (aref data k) 0)))))
    (logand (ash v (- (- 32 bit n))) (1- (ash 1 n)))))

(declaim (inline skip-bits))
(defun skip-bits (br n)
  (declare (type bitreader br) (type fixnum n) (optimize (speed 3) (safety 1)))
  (incf (br-pos br) n))

(declaim (inline read-bits))
(defun read-bits (br n)
  (declare (type bitreader br) (type (integer 0 24) n) (optimize (speed 3) (safety 1)))
  (let ((v (peek-bits br n))) (incf (br-pos br) n) v))

(declaim (inline read-bit))
(defun read-bit (br)
  (declare (type bitreader br) (optimize (speed 3) (safety 1)))
  (let* ((pos (br-pos br)) (data (br-data br)) (byte (ash pos -3)))
    (declare (type fixnum pos byte))
    (setf (br-pos br) (1+ pos))
    (if (< byte (length data))
        (logand (ash (aref data byte) (- (- 7 (logand pos 7)))) 1)
        0)))

(defun read-signed (br n)
  "N bits as a two's complement signed integer."
  (declare (type bitreader br) (type (integer 0 24) n))
  (if (zerop n)
      0
      (let ((v (read-bits br n)))
        (if (logbitp (1- n) v) (- v (ash 1 n)) v))))

(defun read-dc-differential (br size)
  "The SIZE extra bits of a DC differential, which are not two's complement.

   A leading 1 means the value is positive and is those bits read plainly; a leading 0 means it is
   negative and is those bits minus 2^size plus one.  So the same bit pattern means different
   things at different sizes, and the size came from a Huffman code just before it."
  (declare (type bitreader br) (type (integer 0 24) size))
  (if (zerop size)
      0
      (let ((v (read-bits br size)))
        (if (logbitp (1- size) v) v (- v (ash 1 size) -1)))))

(defun byte-align (br)
  (setf (br-pos br) (* 8 (ceiling (br-pos br) 8))))

(defun next-start-code-p (br)
  "Are the next 23 bits all zero?  That is how a slice knows it has run out of macroblocks: the
   23-bit prefix of a start code cannot occur inside coded data."
  (zerop (peek-bits br 23)))

;;; ---- Huffman tables, peeked and indexed ---------------------------------------------------

(defun build-vlc (entries)
  "Turn a list of (code length [value]) into a lookup array indexed by the next MAX-LENGTH bits.

   Each cell holds (value << 5) | length, or 0 for a bit pattern no code matches.  The value
   defaults to the entry's position, which is how the specification prints every one of these
   tables — the code for macroblock_address_increment 7 is simply the seventh entry."
  (let* ((maxlen (reduce #'max entries :key #'second))
         (out (make-array (ash 1 maxlen) :element-type 'fixnum :initial-element 0)))
    (loop for e in entries for i from 0
          do (destructuring-bind (code len &optional (value i)) e
               (let ((base (ash code (- maxlen len)))
                     (span (ash 1 (- maxlen len)))
                     (cell (logior (ash value 5) len)))
                 (dotimes (k span)
                   (unless (zerop (aref out (+ base k)))
                     (%err "VLC table is not prefix-free at ~4,'0b" (+ base k)))
                   (setf (aref out (+ base k)) cell)))))
    (values out maxlen)))

(defmacro define-vlc (name entries doc)
  "A table and its width, both constant, built once at load time.

   NAME must not be the name the source list already has: the generated +NAME-TABLE+ would shadow
   it, be defined as NIL first, and then be built from nothing — which fails a long way from here
   with a complaint about MAX being called with no arguments."
  (let ((tbl (intern (format nil "+~a-TABLE+" name)))
        (bits (intern (format nil "+~a-BITS+" name))))
    (when (eq tbl entries)
      (error "DEFINE-VLC ~a would overwrite its own source list ~a" name entries))
    `(progn
       (defparameter ,tbl nil ,doc)
       (defparameter ,bits 0)
       (multiple-value-bind (a n) (build-vlc ,entries)
         (setf ,tbl a ,bits n))
       (declaim (type (or null fixnums) ,tbl) (type fixnum ,bits)))))

(declaim (inline %vlc))
(defun %vlc (br table bits)
  "Decode one code from TABLE, whose longest entry is BITS long.  Returns the value, or NIL if the
   bits ahead match no code in the table."
  (declare (type bitreader br) (type fixnums table) (type fixnum bits)
           (optimize (speed 3) (safety 1)))
  (let ((cell (aref table (peek-bits br bits))))
    (declare (type fixnum cell))
    (if (zerop cell)
        nil
        (progn (incf (br-pos br) (logand cell 31))
               (ash cell -5)))))

;;;; mpeg4/bits.lisp — start codes, the bit reader, and the Huffman machinery above it.
;;;;
;;;; MPEG-4 Part 2 borrows its framing from MPEG-2 — start codes of 00 00 01 followed by a type
;;;; byte, with the same guarantee that the pattern cannot occur inside coded data — and its
;;;; entropy coding from H.263, which means variable length codes everywhere and no arithmetic
;;;; coder at all.  So this file is MPEG-2's, and the codec above it is not.

(in-package #:reel.mpeg4)

(define-condition mpeg4-error (error)
  ((message :initarg :message :reader mpeg4-error-message))
  (:report (lambda (c s) (format s "reel/mpeg4: ~a" (mpeg4-error-message c)))))

(defun %err (fmt &rest args)
  (error 'mpeg4-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))
(deftype fixnums () '(simple-array (signed-byte 32) (*)))
(deftype dim ()
  "A picture dimension, a plane stride, or an offset into a plane.

   Twenty-six bits rather than FIXNUM, because SBCL cannot prove that the product of two fixnums is
   a fixnum and `(* row stride)\' — which every predicted sample goes through — therefore compiled
   to an out-of-line call.  Twenty-six bits holds any offset into any plane these formats permit,
   and two of them multiply to something a fixnum still holds."
  '(unsigned-byte 26))


;;; ---- start codes (Table 6-3) ------------------------------------------------------------------

(defconstant +sc-vo-start+ #x00 "video_object_start_code, 0x00..0x1F")
(defconstant +sc-vol-start+ #x20 "video_object_layer_start_code, 0x20..0x2F")
(defconstant +sc-vos-start+ #xb0 "visual_object_sequence_start_code")
(defconstant +sc-vos-end+ #xb1)
(defconstant +sc-user-data+ #xb2)
(defconstant +sc-gov+ #xb3 "group_of_vop_start_code")
(defconstant +sc-vo+ #xb5 "visual_object_start_code")
(defconstant +sc-vop+ #xb6 "vop_start_code")

(defun find-start-code (bytes from)
  "The index of the 00 00 01 at or after FROM, or NIL.  Returns the index of the first zero."
  (declare (type octets bytes) (type fixnum from) (optimize (speed 3) (safety 1)))
  (let ((n (length bytes)))
    (loop for i of-type fixnum from (max 0 from) below (- n 3)
          do (when (and (zerop (aref bytes i)) (zerop (aref bytes (+ i 1)))
                        (= 1 (aref bytes (+ i 2))))
               (return i)))))

;;; ---- the bit reader ---------------------------------------------------------------------------

(defstruct (bitreader (:conc-name br-))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)
  (end 0 :type fixnum))

(defun make-br (bytes &key (start 0) (end (length bytes)))
  (make-bitreader :data bytes :pos (* 8 start) :end (* 8 end)))

(declaim (inline br-eof-p peek-bits skip-bits read-bits read-bit))
(defun br-eof-p (br) (>= (br-pos br) (br-end br)))

(defun peek-bits (br n)
  "The next N bits (N <= 24) without consuming them.  Past the end reads as zero."
  (declare (type bitreader br) (type (integer 0 24) n) (optimize (speed 3) (safety 0)))
  (let* ((data (br-data br)) (pos (br-pos br))
         (byte (ash pos -3)) (bit (logand pos 7))
         (len (length data)) (v 0))
    (declare (type fixnum byte bit len) (type (unsigned-byte 32) v))
    (dotimes (i 4)
      (let ((k (+ byte i)))
        (setf v (logior (ash v 8) (if (< k len) (aref data k) 0)))))
    (logand (ash v (- (- 32 bit n))) (1- (ash 1 n)))))

(defun skip-bits (br n)
  (declare (type bitreader br) (type fixnum n) (optimize (speed 3) (safety 0)))
  (incf (br-pos br) n))

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

(defun read-signed (br n)
  (declare (type bitreader br) (type (integer 0 24) n))
  (if (zerop n)
      0
      (let ((v (read-bits br n)))
        (if (logbitp (1- n) v) (- v (ash 1 n)) v))))

(defun marker-bit (br)
  "A bit the specification requires to be 1.  Read and ignored: a stream that gets one wrong is
   damaged in a way this decoder cannot repair, and complaining about it would turn a picture that
   is merely slightly wrong into no picture at all."
  (read-bit br))

;;; ---- Huffman tables, peeked and indexed -------------------------------------------------------

(defun build-vlc (entries)
  "Turn a list of (code length [value]) into a lookup array indexed by the next MAX-LENGTH bits.
   Each cell is (value << 5) | length, or 0 where no code matches."
  (let* ((maxlen (reduce #'max entries :key #'second))
         (out (make-array (ash 1 maxlen) :element-type '(signed-byte 32) :initial-element 0)))
    (loop for e in entries for i from 0
          do (destructuring-bind (code len &optional (value i)) e
               (when (plusp len)
                 (let ((base (ash code (- maxlen len)))
                       (span (ash 1 (- maxlen len)))
                       (cell (logior (ash value 5) len)))
                   (dotimes (k span)
                     (unless (zerop (aref out (+ base k)))
                       (%err "VLC table is not prefix-free"))
                     (setf (aref out (+ base k)) cell))))))
    (values out maxlen)))

(defmacro define-vlc (name entries doc)
  (let ((tbl (intern (format nil "+~a-TABLE+" name)))
        (bits (intern (format nil "+~a-BITS+" name))))
    (when (eq tbl entries)
      (error "DEFINE-VLC ~a would overwrite its own source list" name))
    `(progn
       (defparameter ,tbl nil ,doc)
       (defparameter ,bits 0)
       (multiple-value-bind (a n) (build-vlc ,entries)
         (setf ,tbl a ,bits n))
       (declaim (type (or null fixnums) ,tbl) (type fixnum ,bits)))))

(declaim (inline %vlc))
(defun %vlc (br table bits)
  "Decode one code from TABLE.  Returns the value, or NIL if nothing matches."
  (declare (type bitreader br) (type fixnums table) (type fixnum bits)
           (optimize (speed 3) (safety 0)))
  (let ((cell (aref table (peek-bits br bits))))
    (declare (type fixnum cell))
    (if (zerop cell)
        nil
        (progn (incf (br-pos br) (logand cell 31))
               (ash cell -5)))))

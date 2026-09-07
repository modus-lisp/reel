;;;; h264/bits.lisp — NAL units, RBSP, and the bit reader everything above this is written in.
;;;;
;;;; H.264 is not byte-oriented anywhere above the NAL layer.  Every syntax element is some number
;;;; of bits, and most of them are Exp-Golomb codes whose length depends on their own value, so
;;;; "read the header" means "decode it" — there is no seeking past a field you do not care about
;;;; without decoding it first.  That is why this file exists before any of the others.
;;;;
;;;; THREE LAYERS, AND THEY ARE OFTEN CONFLATED:
;;;;
;;;;   * the BYTE STREAM: NAL units separated by start codes (00 00 01), which is what a `.h264`
;;;;     file and a broadcast are.  MP4 does not use it — an MP4 sample is a run of
;;;;     length-prefixed NAL units and the prefix width comes from the avcC box — so both forms
;;;;     are read here and both produce the same thing.
;;;;   * the NAL UNIT: one byte of header (ref_idc and type) then a payload.
;;;;   * the RBSP: that payload with EMULATION PREVENTION BYTES removed.  An encoder that would
;;;;     otherwise emit 00 00 00, 00 00 01, 00 00 02 or 00 00 03 inserts a 03 after the two zeros
;;;;     so a start code can never appear inside a payload.  Every reader must take them back out,
;;;;     and a decoder that forgets produces bitstreams that work on most frames and desync on the
;;;;     ones that happened to contain two zero bytes.

(in-package #:reel.h264)

(define-condition h264-error (error)
  ((message :initarg :message :reader h264-error-message))
  (:report (lambda (c s) (format s "reel/h264: ~a" (h264-error-message c)))))

(defun %err (fmt &rest args)
  (error 'h264-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun %octets (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;;; ---- NAL unit types (Table 7-1) -----------------------------------------------------------

(defconstant +nal-slice+ 1 "A coded slice of a non-IDR picture.")
(defconstant +nal-idr+ 5 "A coded slice of an IDR picture.")
(defconstant +nal-sei+ 6)
(defconstant +nal-sps+ 7)
(defconstant +nal-pps+ 8)
(defconstant +nal-aud+ 9)

(defstruct (nal (:conc-name nal-))
  (ref-idc 0 :type fixnum)
  (type 0 :type fixnum)
  (rbsp nil :type (or null octets)))

(defun nal-idr-p (n) (= (nal-type n) +nal-idr+))
(defun nal-slice-p (n) (member (nal-type n) (list +nal-slice+ +nal-idr+)))

;;; ---- emulation prevention ------------------------------------------------------------------

(defun rbsp-from (bytes start end)
  "The RBSP of a NAL payload: BYTES[START..END) with every emulation-prevention 03 removed.

   The rule is exact and worth stating, because \"strip every 03\" is the bug it invites: a 03 is
   removed ONLY when the two bytes before it are both zero.  A payload may legitimately contain
   00 03 or 01 03 or a 03 anywhere else, and removing those corrupts it."
  (declare (type octets bytes) (type fixnum start end))
  (let ((out (%octets (- end start))) (o 0) (zeros 0))
    (declare (type fixnum o zeros))
    (loop for i of-type fixnum from start below end
          for b of-type (unsigned-byte 8) = (aref bytes i)
          do (cond ((and (= b 3) (= zeros 2))
                    (setf zeros 0))              ; the inserted byte: drop it, and it resets the run
                   (t
                    (setf (aref out o) b)
                    (incf o)
                    (setf zeros (if (zerop b) (min 2 (1+ zeros)) 0)))))
    (subseq out 0 o)))

;;; ---- splitting a byte stream into NAL units --------------------------------------------------

(defun %start-code-at (bytes i end)
  "The length of the start code at I (3 or 4), or NIL."
  (cond ((and (<= (+ i 4) end) (zerop (aref bytes i)) (zerop (aref bytes (+ i 1)))
              (zerop (aref bytes (+ i 2))) (= 1 (aref bytes (+ i 3))))
         4)
        ((and (<= (+ i 3) end) (zerop (aref bytes i)) (zerop (aref bytes (+ i 1)))
              (= 1 (aref bytes (+ i 2))))
         3)
        (t nil)))

(defun annex-b-nals (bytes &key (start 0) (end (length bytes)))
  "Every NAL unit in an Annex B byte stream, in order."
  (declare (type octets bytes))
  (let ((out '()) (i start))
    ;; find the first start code
    (loop while (and (< i end) (not (%start-code-at bytes i end))) do (incf i))
    (loop while (< i end) do
      (let* ((sc (%start-code-at bytes i end))
             (payload (+ i sc))
             (next payload))
        ;; scan to the next start code, which ends this NAL
        (loop while (and (< next end) (not (%start-code-at bytes next end))) do (incf next))
        (when (> next payload)
          ;; a NAL may be followed by trailing zero bytes that belong to neither it nor the next
          (let ((stop next))
            (loop while (and (> stop (1+ payload)) (zerop (aref bytes (1- stop)))) do (decf stop))
            (push (parse-nal bytes payload stop) out)))
        (setf i next)))
    (nreverse out)))

(defun length-prefixed-nals (bytes &key (start 0) (end (length bytes)) (length-size 4))
  "Every NAL unit in an MP4 sample, where each is preceded by a LENGTH-SIZE-byte big-endian length."
  (declare (type octets bytes))
  (let ((out '()) (i start))
    (loop while (<= (+ i length-size) end) do
      (let ((n 0))
        (dotimes (k length-size) (setf n (logior (ash n 8) (aref bytes (+ i k)))))
        (incf i length-size)
        (when (or (zerop n) (> (+ i n) end)) (return))
        (push (parse-nal bytes i (+ i n)) out)
        (incf i n)))
    (nreverse out)))

(defun parse-nal (bytes start end)
  "One NAL unit: its header byte, and its payload with emulation prevention undone."
  (when (>= start end) (%err "empty NAL unit"))
  (let ((h (aref bytes start)))
    (when (logbitp 7 h) (%err "NAL unit with the forbidden_zero_bit set"))
    (make-nal :ref-idc (ldb (byte 2 5) h)
              :type (ldb (byte 5 0) h)
              :rbsp (rbsp-from bytes (1+ start) end))))

(defun avcc-parameter-sets (avcc)
  "The SPS and PPS NAL units carried in an MP4 `avcC' box, and the NAL length size it declares.
   Returns (values sps-list pps-list length-size)."
  (declare (type octets avcc))
  (when (< (length avcc) 7) (%err "avcC box too short"))
  (let* ((length-size (1+ (logand (aref avcc 4) 3)))
         (nsps (logand (aref avcc 5) #x1f))
         (p 6)
         (sps '()) (pps '()))
    (dotimes (i nsps)
      (let ((n (logior (ash (aref avcc p) 8) (aref avcc (+ p 1)))))
        (push (parse-nal avcc (+ p 2) (+ p 2 n)) sps)
        (incf p (+ 2 n))))
    (let ((npps (aref avcc p)))
      (incf p)
      (dotimes (i npps)
        (let ((n (logior (ash (aref avcc p) 8) (aref avcc (+ p 1)))))
          (push (parse-nal avcc (+ p 2) (+ p 2 n)) pps)
          (incf p (+ 2 n)))))
    (values (nreverse sps) (nreverse pps) length-size)))

;;; ---- the bit reader -------------------------------------------------------------------------
;;;
;;; MSB first, and never byte-aligned in practice.  Kept as a struct with an explicit bit position
;;; rather than a closure because every syntax element in the codec goes through it and the
;;; residual decoder calls it several times per coefficient.

(defstruct (bitreader (:conc-name br-) (:constructor %make-bitreader))
  (data nil :type (or null octets))
  (bit 0 :type fixnum)                  ; the next bit to read, counted from the start of DATA
  (end 0 :type fixnum))                 ; total bits available

(defun make-bitreader (data &key (start 0) (end (length data)))
  (%make-bitreader :data data :bit (* 8 start) :end (* 8 end)))

(declaim (inline br-eof-p))
(defun br-eof-p (br) (>= (br-bit br) (br-end br)))

(defun u1 (br)
  "One bit."
  (declare (type bitreader br) (optimize (speed 3) (safety 1)))
  (let ((i (br-bit br)))
    (when (>= i (br-end br)) (%err "read past the end of the bitstream"))
    (setf (br-bit br) (1+ i))
    (ldb (byte 1 (- 7 (logand i 7))) (aref (br-data br) (ash i -3)))))

(defun ub (br n)
  "N bits as an unsigned integer, MSB first — the specification's u(n)."
  (declare (type bitreader br) (type fixnum n) (optimize (speed 3) (safety 1)))
  (let ((v 0))
    (declare (type (integer 0) v))
    (dotimes (i n v) (setf v (logior (ash v 1) (u1 br))))))

(defun ue (br)
  "An unsigned Exp-Golomb code — the specification's ue(v).

   N leading zeros, a one, then N more bits: the value is 2^N - 1 plus those bits.  The leading
   zero count is bounded here rather than trusted, because a corrupt stream otherwise spins to the
   end of the buffer counting zeros and reports the wrong failure."
  (declare (type bitreader br))
  (let ((zeros 0))
    (declare (type fixnum zeros))
    (loop until (br-eof-p br)
          while (zerop (u1 br))
          do (incf zeros)
             (when (> zeros 32) (%err "Exp-Golomb code with ~d leading zeros" zeros)))
    (when (and (br-eof-p br) (plusp zeros)) (%err "Exp-Golomb code runs off the end"))
    (+ (1- (ash 1 zeros)) (ub br zeros))))

(defun se (br)
  "A signed Exp-Golomb code — se(v).  The codes alternate 0, 1, -1, 2, -2, ..."
  (let ((k (ue br)))
    (if (evenp k) (- (ash k -1)) (ash (1+ k) -1))))

(defun more-rbsp-data-p (br)
  "True while there is anything left but the rbsp_stop_one_bit and its zero padding.

   The trailing bits are a 1 followed by zeros to the byte boundary, so \"is there more data\" is
   \"is there a set bit after this position\" — which is why this has to scan rather than compare
   positions."
  (declare (type bitreader br))
  (when (br-eof-p br) (return-from more-rbsp-data-p nil))
  (let ((last-one nil))
    (loop for i of-type fixnum from (1- (br-end br)) downto (br-bit br)
          when (logbitp (- 7 (logand i 7)) (aref (br-data br) (ash i -3)))
            do (setf last-one i) (return))
    (and last-one (> last-one (br-bit br)))))

(defun byte-align (br)
  (setf (br-bit br) (* 8 (ceiling (br-bit br) 8))))

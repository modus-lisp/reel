;;;; hevc/bits.lisp — NAL units, RBSP, and the bit reader everything above this is written in.
;;;;
;;;; The transport layer is H.264's, almost exactly: an Annex B byte stream of NAL units separated
;;;; by start codes, or length-prefixed units inside an MP4 sample; emulation prevention bytes
;;;; removed the same way; the same Exp-Golomb codes above that.  What changed is the NAL HEADER,
;;;; which is two bytes rather than one, and it changed for a reason worth knowing.
;;;;
;;;; H.264's header spends two bits on nal_ref_idc — "how important is this picture as a
;;;; reference" — and five on the type.  HEVC drops the field entirely and encodes the same fact in
;;;; the TYPE: types 0 to 14 alternate, the even ones being sub-layer non-reference pictures and
;;;; the odd ones reference pictures.  The bits it frees go to nuh_layer_id, for the scalable and
;;;; multiview extensions, and nuh_temporal_id_plus1, which says which temporal sub-layer a picture
;;;; belongs to so a decoder can drop the top ones and get a lower frame rate without parsing them.
;;;;
;;;; So a reader written for H.264 will find HEVC's start codes correctly, cut the NAL units at the
;;;; right places, and then read every single syntax element one byte too early.

(in-package #:reel.hevc)

(define-condition hevc-error (error)
  ((message :initarg :message :reader hevc-error-message))
  (:report (lambda (c s) (format s "reel/hevc: ~a" (hevc-error-message c)))))

(defun %err (fmt &rest args)
  (error 'hevc-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun %octets (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;;; ---- NAL unit types (Table 7-1) -------------------------------------------------------------
;;;
;;; The VCL types are grouped so that a decoder can answer the questions it actually asks with a
;;; range test rather than a table: everything below 32 is a slice, everything from 16 to 23 starts
;;; a random access point, and within that 19 and 20 are the IDRs that also reset the order counts.

(defconstant +nal-trail-n+ 0) (defconstant +nal-trail-r+ 1)
(defconstant +nal-tsa-n+ 2)   (defconstant +nal-tsa-r+ 3)
(defconstant +nal-stsa-n+ 4)  (defconstant +nal-stsa-r+ 5)
(defconstant +nal-radl-n+ 6)  (defconstant +nal-radl-r+ 7)
(defconstant +nal-rasl-n+ 8)  (defconstant +nal-rasl-r+ 9)
(defconstant +nal-bla-w-lp+ 16)   (defconstant +nal-bla-w-radl+ 17)
(defconstant +nal-bla-n-lp+ 18)
(defconstant +nal-idr-w-radl+ 19) (defconstant +nal-idr-n-lp+ 20)
(defconstant +nal-cra+ 21)
(defconstant +nal-vps+ 32) (defconstant +nal-sps+ 33) (defconstant +nal-pps+ 34)
(defconstant +nal-aud+ 35) (defconstant +nal-eos+ 36) (defconstant +nal-eob+ 37)
(defconstant +nal-fd+ 38)
(defconstant +nal-sei-prefix+ 39) (defconstant +nal-sei-suffix+ 40)

(defstruct (nal (:conc-name nal-))
  (type 0 :type fixnum)
  (layer-id 0 :type fixnum)
  (temporal-id 0 :type fixnum)          ; nuh_temporal_id_plus1 - 1
  (rbsp nil :type (or null octets)))

(defun nal-slice-p (n) (< (nal-type n) 32))

(defun nal-base-layer-p (n)
  "Is this NAL unit part of the base layer — the one a single-layer decoder is supposed to decode?

   The scalable and multiview extensions send their extra layers as ordinary NAL units with
   nuh_layer_id set, interleaved with the base layer in the same byte stream.  Annex F is explicit
   that a decoder conforming to a single-layer profile decodes layer zero and DISCARDS the rest,
   which is the whole point of coding them that way.  A decoder that instead tries to parse them
   gets nonsense, because an enhancement layer's sequence parameter set does not even have the same
   syntax: what is sps_max_sub_layers_minus1 at layer zero is sps_ext_or_max_sub_layers_minus1
   above it, and the profile_tier_level that follows may be absent entirely."
  (zerop (nal-layer-id n)))

(defun nal-irap-p (n)
  "Does this NAL start an intra random access point — a picture a decoder may begin at?"
  (<= 16 (nal-type n) 23))

(defun nal-idr-p (n)
  (or (= (nal-type n) +nal-idr-w-radl+) (= (nal-type n) +nal-idr-n-lp+)))

(defun nal-bla-p (n) (<= +nal-bla-w-lp+ (nal-type n) +nal-bla-n-lp+))

(defun nal-reference-p (n)
  "Is this picture kept as a reference by its own sub-layer?

   For the trailing and leading types the answer is the LOW BIT of the type: HEVC spends no header
   field on it, having arranged the numbering so that _N is even and _R is odd.  Everything from 16
   up — the random access points — is always a reference."
  (let ((tp (nal-type n)))
    (if (< tp 16) (oddp tp) t)))

;;; ---- emulation prevention -------------------------------------------------------------------

(defun rbsp-from (bytes start end)
  "The RBSP of a NAL payload: BYTES[START..END) with every emulation-prevention 03 removed.

   The rule is exact and worth stating, because \"strip every 03\" is the bug it invites: a 03 is
   removed ONLY when the two bytes before it are both zero.  A payload may legitimately contain
   00 03 or 01 03 or a 03 anywhere else, and removing those corrupts it."
  (declare (type octets bytes) (type fixnum start end))
  (let ((out (%octets (max 0 (- end start)))) (o 0) (zeros 0))
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
    (loop while (and (< i end) (not (%start-code-at bytes i end))) do (incf i))
    (loop while (< i end) do
      (let* ((sc (%start-code-at bytes i end))
             (payload (+ i sc))
             (next payload))
        (loop while (and (< next end) (not (%start-code-at bytes next end))) do (incf next))
        (when (> next payload)
          ;; a NAL may be followed by trailing zero bytes that belong to neither it nor the next
          (let ((stop next))
            (loop while (and (> stop (+ payload 2)) (zerop (aref bytes (1- stop)))) do (decf stop))
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
  "One NAL unit: its two header bytes, and its payload with emulation prevention undone."
  (when (< (- end start) 2) (%err "NAL unit shorter than its header"))
  (let ((a (aref bytes start)) (b (aref bytes (1+ start))))
    (when (logbitp 7 a) (%err "NAL unit with the forbidden_zero_bit set"))
    (let ((tid (logand b 7)))
      (when (zerop tid) (%err "nuh_temporal_id_plus1 is zero, which is not a legal value"))
      (make-nal :type (ldb (byte 6 1) a)
                ;; nuh_layer_id straddles the two bytes: one bit at the bottom of the first and
                ;; five at the top of the second
                :layer-id (logior (ash (logand a 1) 5) (ldb (byte 5 3) b))
                :temporal-id (1- tid)
                :rbsp (rbsp-from bytes (+ start 2) end)))))

(defun hvcc-parameter-sets (hvcc)
  "The parameter set NAL units carried in an MP4 `hvcC' box, and the NAL length size it declares.

   Unlike avcC, which has a fixed SPS-then-PPS layout, hvcC carries an ARRAY OF ARRAYS: each entry
   names a NAL type and holds however many units of it, so VPS, SPS, PPS and prefix SEI all arrive
   through the same loop and in whatever order the muxer chose.  Returns (values nals length-size)."
  (declare (type octets hvcc))
  (when (< (length hvcc) 23) (%err "hvcC box too short"))
  (let* ((length-size (1+ (logand (aref hvcc 21) 3)))
         (num-arrays (aref hvcc 22))
         (p 23)
         (nals '()))
    (dotimes (i num-arrays)
      (when (> (+ p 3) (length hvcc)) (%err "hvcC box ends inside its array table"))
      (let ((count (logior (ash (aref hvcc (+ p 1)) 8) (aref hvcc (+ p 2)))))
        (incf p 3)
        (dotimes (k count)
          (when (> (+ p 2) (length hvcc)) (%err "hvcC box ends inside a parameter set"))
          (let ((n (logior (ash (aref hvcc p) 8) (aref hvcc (+ p 1)))))
            (when (> (+ p 2 n) (length hvcc)) (%err "hvcC parameter set runs past the box"))
            (push (parse-nal hvcc (+ p 2) (+ p 2 n)) nals)
            (incf p (+ 2 n))))))
    (values (nreverse nals) length-size)))

;;; ---- the bit reader ---------------------------------------------------------------------------
;;;
;;; MSB first, and never byte-aligned in practice.  Kept as a struct with an explicit bit position
;;; rather than a closure because every syntax element in the codec goes through it.

(defstruct (bitreader (:conc-name br-) (:constructor %make-bitreader))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
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
    (declare (type fixnum i))
    (when (>= i (br-end br)) (%err "read past the end of the bitstream"))
    (setf (br-bit br) (1+ i))
    (ldb (byte 1 (- 7 (logand i 7))) (aref (br-data br) (ash i -3)))))

(declaim (inline br-peek br-skip))
(defun br-peek (br n)
  "The next N bits (N <= 24) as an integer, MSB first, WITHOUT consuming them.

   Past the end of the available data the result is zero-padded rather than an error: a code near
   the end of a slice is shorter than the window a lookup peeks through, so refusing to peek would
   refuse the last legitimate code in the bitstream.  Consuming bits past the end is still an
   error, which is BR-SKIP's job."
  (declare (type bitreader br) (type (integer 0 24) n) (optimize (speed 3) (safety 0)))
  (let* ((i (br-bit br))
         (data (br-data br))
         (limit (ash (br-end br) -3))           ; first byte that is not ours to read
         (byte (ash i -3))
         (off (logand i 7)))
    (declare (type fixnum i limit byte off)
             (type (simple-array (unsigned-byte 8) (*)) data))
    (macrolet ((b (k) `(let ((j (+ byte ,k)))
                         (declare (type fixnum j))
                         (if (< j limit) (aref data j) 0))))
      (let ((w (logior (ash (b 0) 24) (ash (b 1) 16) (ash (b 2) 8) (b 3))))
        (declare (type (unsigned-byte 32) w))
        (logand (ash w (- (+ off n) 32)) (1- (ash 1 n)))))))

(defun br-skip (br n)
  "Consume N bits, having already looked at them."
  (declare (type bitreader br) (type fixnum n) (optimize (speed 3) (safety 1)))
  (let ((i (+ (br-bit br) n)))
    (when (> i (br-end br)) (%err "read past the end of the bitstream"))
    (setf (br-bit br) i)))

(defun ub (br n)
  "N bits as an unsigned integer, MSB first — the specification's u(n)."
  (declare (type bitreader br) (type fixnum n) (optimize (speed 3) (safety 1)))
  (cond ((zerop n) 0)
        ((<= n 24) (let ((v (br-peek br n))) (br-skip br n) v))
        (t (let* ((hi (- n 24)) (a (ub br hi)) (b (ub br 24)))
             (logior (ash a 24) b)))))

(defun ue (br)
  "An unsigned Exp-Golomb code — the specification's ue(v).

   N leading zeros, a one, then N more bits: the value is 2^N - 1 plus those bits.  The leading
   zero count is bounded rather than trusted, because a corrupt stream otherwise spins to the end
   of the buffer counting zeros and reports the wrong failure."
  (declare (type bitreader br) (optimize (speed 3) (safety 1)))
  (let* ((w (br-peek br 24))
         (zeros (if (zerop w) 24 (- 24 (integer-length w)))))
    (declare (type (unsigned-byte 24) w) (type (integer 0 24) zeros))
    (when (>= zeros 24)
      (br-skip br 24)
      (let ((more 24))
        (declare (type fixnum more))
        (loop until (br-eof-p br)
              while (zerop (u1 br))
              do (incf more)
                 (when (> more 32) (%err "Exp-Golomb code with ~d leading zeros" more)))
        (when (br-eof-p br) (%err "Exp-Golomb code runs off the end"))
        (return-from ue (+ (1- (ash 1 more)) (ub br more)))))
    (br-skip br (1+ zeros))                     ; the zeros and the one that ends them
    (+ (1- (ash 1 zeros)) (ub br zeros))))

(defun se (br)
  "A signed Exp-Golomb code — se(v).  The codes alternate 0, 1, -1, 2, -2, ..."
  (let ((k (ue br)))
    (if (evenp k) (- (ash k -1)) (ash (1+ k) -1))))

(defun more-rbsp-data-p (br)
  "True while there is anything left but the rbsp_stop_one_bit and its zero padding."
  (declare (type bitreader br))
  (when (br-eof-p br) (return-from more-rbsp-data-p nil))
  (let ((last-one nil))
    (loop for i of-type fixnum from (1- (br-end br)) downto (br-bit br)
          when (logbitp (- 7 (logand i 7)) (aref (br-data br) (ash i -3)))
            do (setf last-one i) (return))
    (and last-one (> last-one (br-bit br)))))

(defun byte-align (br)
  (setf (br-bit br) (* 8 (ceiling (br-bit br) 8))))

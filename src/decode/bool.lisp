;;;; decode/bool.lisp — the VP8 frame header and the boolean entropy decoder (RFC 6386 s7, s9.1).
;;;;
;;;; Came from webp-pure, where it sat under the RIFF reader because a lossy WebP is exactly one
;;;; VP8 key frame.  The container half stayed there; this half is the codec's and lives here.

(in-package #:reel.decode)

(define-condition vp8-error (error)
  ((message :initarg :message :reader vp8-error-message))
  (:report (lambda (c s) (format s "reel: ~a" (vp8-error-message c)))))

(defun %err (fmt &rest args)
  (error 'vp8-error :message (apply #'format nil fmt args)))

(deftype u8 () '(unsigned-byte 8))
(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline %octets))
(defun %octets (n)
  "A fresh zeroed octet vector.  Spelled with a prefix because OCTETS is the type above, and a
function and a type sharing a name is legal but reads like a mistake."
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;;; The two little-endian readers the frame header needs.  webp-pure has its own copy for the
;;; RIFF chunks it walks; three inlined arithmetic functions are not a dependency worth having.
(declaim (inline u16le u24le))
(defun u16le (b i) (logior (aref b i) (ash (aref b (+ i 1)) 8)))
(defun u24le (b i) (logior (aref b i) (ash (aref b (+ i 1)) 8) (ash (aref b (+ i 2)) 16)))

;;; ---- VP8 frame header (RFC 6386 §9.1) ----------------------------------

(defstruct (frame (:conc-name fr-))
  key-frame version show-frame part0-size
  width height xscale yscale
  data off0)                 ; the VP8 payload octets and the offset of partition 0

(defun parse-frame-header (bytes off size)
  "Parse the VP8 bitstream at BYTES[OFF..OFF+SIZE): the 3-byte frame tag and, for
   a key frame, the start code and dimensions.  Returns a FRAME."
  (declare (ignore size))
  (let* ((tag (u24le bytes off))
         (key-frame (zerop (logand tag 1)))       ; 0 => key frame
         (version (logand (ash tag -1) 7))
         (show (logand (ash tag -4) 1))
         (part0 (ash tag -5)))
    (unless key-frame (%err "only key frames are supported"))
    (let ((p (+ off 3)))
      (unless (and (= (aref bytes p) #x9d) (= (aref bytes (+ p 1)) #x01)
                   (= (aref bytes (+ p 2)) #x2a))
        (%err "bad VP8 key-frame start code"))
      (let ((w-raw (u16le bytes (+ p 3))) (h-raw (u16le bytes (+ p 5))))
        (make-frame :key-frame key-frame :version version :show-frame (= show 1)
                    :part0-size part0
                    :width (logand w-raw #x3fff) :xscale (ash w-raw -14)
                    :height (logand h-raw #x3fff) :yscale (ash h-raw -14)
                    :data bytes :off0 (+ p 7))))))   ; partition 0 starts after dims

;;; ---- boolean entropy decoder (RFC 6386 §7) -----------------------------

;;; Slot widths are declared as tightly as the arithmetic allows: a 63-bit
;;; FIXNUM slot means (- range 1) may legally be a bignum, so the compiler keeps
;;; a tag test and an overflow check on every step; with RANGE < 2^9, VALUE <
;;; 2^16 and POS/END < 2^31 every intermediate provably fits and the checks go.
;;; Normalization shift per range (libvpx vp8_norm): the count that brings
;;; RANGE back into [128, 255].  One table lookup and one variable shift per
;;; decoded bit instead of the RFC's shift-by-one loop.
(declaim (type (simple-array (unsigned-byte 8) (*)) +bd-norm+))
(defparameter +bd-norm+
  (let ((v (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for r from 1 below 256 do (setf (aref v r) (- 8 (integer-length r))))
    v))

(defstruct (bool-dec (:conc-name bd-))
  (buf nil :type (or null octets))
  (pos 0 :type (unsigned-byte 31))       ; next byte to load
  (end 0 :type (unsigned-byte 31))
  (range 255 :type (unsigned-byte 9))    ; 128..255 between calls
  (value 0 :type (unsigned-byte 16))     ; the 16-bit window (< range << 8)
  (bit-count 0 :type (unsigned-byte 4))  ; bits shifted since the last byte load, 0..7
  (norm +bd-norm+ :type octets))         ; the normalization table, reached through the struct (a global read is a generic array access on Modus)

(defun bool-init (buf pos end)
  "A boolean decoder over BUF[POS..END)."
  (let ((bd (make-bool-dec :buf buf :pos pos :end end :range 255 :bit-count 0)))
    (setf (bd-value bd)
          (logior (ash (if (< pos end) (aref buf pos) 0) 8)
                  (if (< (1+ pos) end) (aref buf (1+ pos)) 0)))
    (incf (bd-pos bd) 2)
    bd))

(declaim (inline bool-bit))
(defun bool-bit (bd prob)
  "Decode one boolean with probability PROB/256 of being 0 (RFC 6386 §7.3).
   Normalization is batched: SHIFT = norm[range] bits at once (libvpx), with
   the byte refill placed at the bit position the per-bit process would have
   moved it to (bit-count + shift - 8)."
  (declare (type bool-dec bd) (type (unsigned-byte 8) prob))
  (let* ((range (bd-range bd))
         (split (+ 1 (ash (* (- range 1) prob) -8)))
         (bigsplit (ash split 8))
         (value (bd-value bd))
         (bit 0))
    (declare (type (unsigned-byte 9) range) (type (unsigned-byte 8) split)
             (type (unsigned-byte 16) bigsplit value)
             (type (unsigned-byte 1) bit))
    (cond ((>= value bigsplit)
           (setf bit 1 range (- range split) value (- value bigsplit)))
          (t (setf range split)))
    (let* ((norm (bd-norm bd)) (shift (aref norm range)))   ; NORM as a declared local: the u8 fast path needs a variable
      (declare (type octets norm) (type (unsigned-byte 3) shift))
      (when (> shift 0)
        (setf range (ash range shift)
              value (ash value shift))
        (let ((bc (+ (bd-bit-count bd) shift)))
          (declare (type (unsigned-byte 4) bc))
          (when (>= bc 8)
            (setf bc (- bc 8))
            (when (< (bd-pos bd) (bd-end bd))
              (setf value (logior value (ash (aref (bd-buf bd) (bd-pos bd)) bc)))
              (incf (bd-pos bd))))
          (setf (bd-bit-count bd) bc))))
    (setf (bd-range bd) range (bd-value bd) value)
    bit))

;;; Cached decoder state (libvpx keeps value/range/count in registers across a
;;; whole token loop and writes the struct once per block).  WITH-BD-CACHE
;;; binds the state in locals for BODY — BD-BIT* decodes on those locals —
;;; and writes it back at the end.  The body must not call anything that
;;; reads BD through the struct (BOOL-BIT, BOOL-LITERAL, ...) while cached.

(defmacro with-bd-cache (bd &body body)   ; flat lambda list: Modus's DEFMACRO does not destructure
  `(let ((%range (bd-range ,bd)) (%value (bd-value ,bd)) (%bc (bd-bit-count ,bd))
         (%pos (bd-pos ,bd)) (%end (bd-end ,bd)) (%buf (bd-buf ,bd)) (%norm (bd-norm ,bd)))
     (declare (type (unsigned-byte 9) %range) (type (unsigned-byte 16) %value)
              (type (unsigned-byte 4) %bc) (type (unsigned-byte 31) %pos %end)
              (type octets %buf %norm))
     ,@body
     (setf (bd-range ,bd) %range (bd-value ,bd) %value
           (bd-bit-count ,bd) %bc (bd-pos ,bd) %pos)))

(defmacro bd-bit* (prob)
  "BOOL-BIT on the WITH-BD-CACHE locals; PROB must be an (unsigned-byte 8) form."
  `(let* ((%prob ,prob)
          (%split (+ 1 (ash (* (- %range 1) %prob) -8)))
          (%bigsplit (ash %split 8))
          (%bit 0))
     (declare (type (unsigned-byte 8) %prob %split) (type (unsigned-byte 16) %bigsplit)
              (type (unsigned-byte 1) %bit))
     (cond ((>= %value %bigsplit)
            (setf %bit 1 %range (- %range %split) %value (- %value %bigsplit)))
           (t (setf %range %split)))
     (let ((%shift (aref %norm %range)))
       (declare (type (unsigned-byte 3) %shift))
       (when (> %shift 0)
         (setf %range (ash %range %shift)
               %value (ash %value %shift)
               %bc (+ %bc %shift))
         (when (>= %bc 8)
           (setf %bc (- %bc 8))
           (when (< %pos %end)
             (setf %value (logior %value (ash (aref %buf %pos) %bc)))
             (setf %pos (+ %pos 1))))))
     %bit))

(defun bool-literal (bd nbits)
  "Read NBITS as an unsigned literal (flat probability 128), MSB first."
  (let ((v 0))
    (dotimes (i nbits) (setf v (logior (ash v 1) (bool-bit bd 128))))
    v))

(defun bool-signed (bd nbits)
  "Read an NBITS magnitude followed by a sign bit; negative when the sign is 1."
  (let ((v (bool-literal bd nbits)))
    (if (= 1 (bool-bit bd 128)) (- v) v)))

(defun bool-flag (bd) (= 1 (bool-bit bd 128)))


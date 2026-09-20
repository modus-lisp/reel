;;;; hevc/cabac.lisp — the arithmetic decoder, and the binarizations built on it.
;;;;
;;;; The ENGINE is H.264's, unchanged: the same sixty-four probability states, the same range table,
;;;; the same renormalisation, the same three operations — a context-coded decision, a bypass bin
;;;; with no model at all, and the terminate decision that ends a slice.  If that seems like a
;;;; missed opportunity to improve something, it is not: the engine is the part that was already at
;;;; the entropy limit, and keeping it identical is what let every HEVC implementation reuse a
;;;; decade of hardware.
;;;;
;;;; What HEVC changed is the layer above.  H.264 has one context set per slice type and a handful
;;;; of syntax elements; HEVC has 179 contexts and derives most of the interesting ones from the
;;;; POSITION of a coefficient inside its block rather than from what the neighbours did.  And the
;;;; initialisation is no longer a table of probabilities per quantiser — it is a line, evaluated
;;;; at the slice quantiser, with the slope and intercept packed into one byte.

(in-package #:reel.hevc)

(defstruct (cabac (:conc-name cb-) (:constructor %make-cabac))
  (br nil)
  (range 510 :type fixnum)
  (offset 0 :type fixnum)
  ;; one byte per context: pStateIdx in bits 1..6, valMPS in bit 0 — the packing costs nothing and
  ;; keeps the two halves of a context together, which is how they are always used
  (state (make-array +cabac-contexts+ :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  ;; the Rice parameter that coeff_abs_level_remaining adapts, one per coefficient category.  It
  ;; persists across transform blocks within a slice, which is what makes it adaptive at all.
  (stat-coeff (make-array 4 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (*))))

(defun %init-type (slice-type cabac-init-flag)
  "Which of the three initialisation sets this slice uses (9.3.2.2).

   The mapping reads oddly because slice_type numbers I as 2 and B as 0: initType is 2 - slice_type,
   so an I slice takes set 0.  A P or B slice may then SWAP its set with the other inter one, which
   is what cabac_init_flag is for — an encoder that knows a particular P slice looks more like a
   typical B slice can say so and start closer to the truth."
  (let ((init (- 2 slice-type)))
    (if (and cabac-init-flag (/= slice-type 2)) (logxor init 3) init)))

(defun init-cabac (br qp slice-type cabac-init-flag)
  "Start the arithmetic decoder at the current (byte-aligned) position, with fresh contexts.

   Each context's initValue is a slope and an offset, and the state is the line they describe
   evaluated at the slice quantiser: a coarse slice and a fine one have genuinely different
   statistics, and this is how one table serves both without being repeated fifty-two times."
  (declare (type fixnum qp slice-type) (optimize (speed 3) (safety 1)))
  (byte-align br)
  (let* ((c (%make-cabac :br br))
         (st (cb-state c))
         (init (%init-type slice-type cabac-init-flag))
         (q (max 0 (min 51 qp))))
    (declare (type fixnum init q))
    (dotimes (i +cabac-contexts+)
      (let* ((v (aref +cabac-init+ init i))
             (m (- (* 5 (ash v -4)) 45))
             (n (- (ash (logand v 15) 3) 16))
             (pre (max 1 (min 126 (+ (ash (* m q) -4) n)))))
        (declare (type fixnum v m n pre))
        ;; preCtxState above 63 means the more probable symbol is 1, and the state index is measured
        ;; up from 64; at or below 63 it is 0 and the index is measured DOWN from 63.
        (setf (aref st i)
              (if (> pre 63)
                  (logior (ash (- pre 64) 1) 1)
                  (ash (- 63 pre) 1)))))
    (fill (cb-stat-coeff c) 0)
    (setf (cb-range c) 510
          (cb-offset c) (ub br 9))
    c))

(declaim (inline %renorm %read-bit))
(defun %read-bit (br)
  (declare (optimize (speed 3) (safety 0)))
  ;; past the end reads as zero: a slice's last bins can legitimately need bits the RBSP does not
  ;; physically contain, because the encoder stopped once the decoder could not be in doubt
  (if (>= (br-bit br) (br-end br)) 0 (u1 br)))

(defun %renorm (c)
  (declare (type cabac c) (optimize (speed 3) (safety 0)))
  (let ((r (cb-range c)) (o (cb-offset c)) (br (cb-br c)))
    (declare (type fixnum r o))
    (loop while (< r 256)
          do (setf r (ash r 1)
                   o (logior (ash o 1) (%read-bit br))))
    (setf (cb-range c) r (cb-offset c) o)))

(defun decode-decision (c ctx)
  "One context-coded bin (9.3.4.3.2)."
  (declare (type cabac c) (type fixnum ctx) (optimize (speed 3) (safety 1)))
  (let* ((st (cb-state c))
         (s (aref st ctx))
         (pstate (ash s -1))
         (mps (logand s 1))
         (r (cb-range c))
         (lps (aref +cabac-range-lps+ pstate (logand (ash r -6) 3)))
         (bit 0))
    (declare (type fixnum s pstate mps r lps bit))
    (decf r lps)
    (cond
      ((>= (cb-offset c) r)
       (setf bit (- 1 mps))
       (decf (cb-offset c) r)
       (setf r lps)
       (when (zerop pstate) (setf mps (- 1 mps)))
       (setf (aref st ctx) (logior (ash (aref +cabac-trans-lps+ pstate) 1) mps)))
      (t
       (setf bit mps)
       (setf (aref st ctx) (logior (ash (aref +cabac-trans-mps+ pstate) 1) mps))))
    (setf (cb-range c) r)
    (%renorm c)
    bit))

(defun decode-bypass (c)
  "A bin with no context at all (9.3.4.3.4): an even chance, so nothing has to adapt."
  (declare (type cabac c) (optimize (speed 3) (safety 1)))
  (let ((o (logior (ash (cb-offset c) 1) (%read-bit (cb-br c)))))
    (declare (type fixnum o))
    (cond ((>= o (cb-range c)) (setf (cb-offset c) (- o (cb-range c))) 1)
          (t (setf (cb-offset c) o) 0))))

(defun decode-bypass-bits (c n)
  "N bypass bins as one unsigned integer, most significant first."
  (declare (type cabac c) (type fixnum n) (optimize (speed 3) (safety 1)))
  (let ((v 0))
    (declare (type fixnum v))
    (dotimes (i n v) (setf v (logior (ash v 1) (decode-bypass c))))))

(defun decode-terminate (c)
  "The end-of-slice decision (9.3.4.3.5), which is also how a PCM block is signalled."
  (declare (type cabac c) (optimize (speed 3) (safety 1)))
  (let ((r (- (cb-range c) 2)))
    (declare (type fixnum r))
    (cond ((>= (cb-offset c) r) 1)
          (t (setf (cb-range c) r) (%renorm c) 0))))

;;; ---- binarizations ----------------------------------------------------------------------------

(defun %tu (c ctx-fn limit)
  "Truncated unary: ones until a zero or until LIMIT, CTX-FN giving the context for each bin."
  (declare (type function ctx-fn) (type fixnum limit) (optimize (speed 3) (safety 1)))
  (let ((n 0))
    (declare (type fixnum n))
    (loop while (< n limit)
          do (if (zerop (decode-decision c (funcall ctx-fn n))) (return) (incf n)))
    n))

(defun %tu-bypass (c limit)
  "Truncated unary in bypass."
  (declare (type fixnum limit) (optimize (speed 3) (safety 1)))
  (let ((n 0))
    (declare (type fixnum n))
    (loop while (< n limit)
          do (if (zerop (decode-bypass c)) (return) (incf n)))
    n))

(defun %egk-bypass (c k)
  "The order-K Exp-Golomb suffix, in bypass (9.3.3.3).

   A prefix of ones counts how many times the range has doubled, then that many bits give the
   position inside it.  Reading it as \"unary then K bits\" — the order-0 shape — is the mistake to
   avoid: the suffix WIDENS with the prefix, which is the whole reason a large coefficient does not
   cost its own magnitude in bits."
  (declare (type cabac c) (type fixnum k) (optimize (speed 3) (safety 1)))
  (let ((prefix 0))
    (declare (type fixnum prefix))
    (loop while (= 1 (decode-bypass c))
          do (incf prefix)
             (when (> prefix 32) (%err "runaway Exp-Golomb prefix in a bypass suffix")))
    (if (zerop prefix)
        (decode-bypass-bits c k)
        (+ (ash (1- (ash 1 prefix)) k)
           (decode-bypass-bits c (+ prefix k))))))

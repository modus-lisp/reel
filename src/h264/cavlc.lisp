;;;; h264/cavlc.lisp — residual block decoding, Context-Adaptive Variable Length Coding (9.2).
;;;;
;;;; This is where an H.264 macroblock's coefficients actually come from, and it is the part of
;;;; Baseline that has no counterpart in VP8: instead of a binary arithmetic coder driven by
;;;; adaptive probabilities, H.264's Baseline profile uses a set of fixed VLC tables and switches
;;;; between them on context — hence "context adaptive VARIABLE LENGTH".  The context is the
;;;; number of coefficients in the blocks ABOVE and LEFT, which is why the caller has to keep a
;;;; per-block count and hand it in as nC.
;;;;
;;;; A block is coded in five parts, in this order, and each one's coding depends on the last:
;;;;
;;;;   1. coeff_token      how many coefficients there are, and how many of the trailing ones
;;;;                       are +/-1.  The table is chosen by nC.
;;;;   2. trailing ones    one sign bit each, and nothing else: their magnitude is known to be 1.
;;;;   3. levels           the remaining magnitudes, in an escape code whose suffix WIDENS as the
;;;;                       levels get bigger — the adaptation that makes this competitive with
;;;;                       arithmetic coding on the blocks that matter.
;;;;   4. total_zeros      how many zeros are scattered before the last coefficient.
;;;;   5. run_before       how they are distributed, from the end backwards.
;;;;
;;;; Coefficients arrive in REVERSE scan order — highest frequency first — because that is the end
;;;; the run lengths are measured from.

(in-package #:reel.h264)

;;; ---- VLC decoding ---------------------------------------------------------------------------

(defun %vlc (br lens bits row n)
  "Decode one variable-length code from ROW of the (LENS, BITS) table pair, which has N entries.

   Reads one bit at a time, accumulating a candidate code, and stops at the first entry whose
   length and value both match.  These tables are prefix codes, so the first match is the only
   match and there is no backtracking; that property is what makes reading forward legal."
  (declare (type bitreader br) (type (simple-array (unsigned-byte 8) (* *)) lens bits)
           (type fixnum row n))
  (let ((code 0))
    (declare (type fixnum code))
    (dotimes (len 17)
      (declare (ignorable len))
      (setf code (logior (ash code 1) (u1 br)))
      (let ((want (1+ len)))
        (dotimes (i n)
          (when (and (= (aref lens row i) want) (= (aref bits row i) code))
            (return-from %vlc i)))))
    (%err "no CAVLC code matched in ~d bits" 17)))

(defun %coeff-token (br nc)
  "coeff_token: (values total-coeff trailing-ones).

   NC is the context — the mean of the coefficient counts of the block above and the block left —
   and it selects one of four tables, or the separate chroma-DC table when it is -1.  The ranges
   are the specification's and are not a heuristic: a block whose neighbours were busy is coded
   with a table that expects to be busy too."
  (declare (type fixnum nc))
  (if (minusp nc)
      (let ((i (%vlc br +chroma-dc-coeff-token-len+ +chroma-dc-coeff-token-bits+ 0 20)))
        (values (ash i -2) (logand i 3)))
      (let* ((row (cond ((< nc 2) 0) ((< nc 4) 1) ((< nc 8) 2) (t 3)))
             (i (%vlc br +coeff-token-len+ +coeff-token-bits+ row 68)))
        (values (ash i -2) (logand i 3)))))

(defun %level (br i trailing-ones suffix-length)
  "One non-trailing coefficient level, and the suffix length to use for the next one.

   The escape coding here is the fiddliest arithmetic in Baseline.  A unary prefix, a suffix whose
   width starts at 0 or 1 and grows as levels get large, and three separate special cases: prefix
   14 with a zero-width suffix reads four bits anyway, prefix 15 and above reads a wider suffix
   and adds a bias, and the FIRST non-trailing level is nudged by 2 when there were fewer than
   three trailing ones — because in that case a magnitude of 1 was already impossible."
  (declare (type fixnum i trailing-ones suffix-length))
  (let ((prefix 0))
    (declare (type fixnum prefix))
    (loop until (br-eof-p br)
          while (zerop (u1 br))
          do (incf prefix)
             (when (> prefix 32) (%err "level_prefix of ~d" prefix)))
    (let* ((suffix-size (cond ((and (= prefix 14) (zerop suffix-length)) 4)
                              ((>= prefix 15) (- prefix 3))
                              (t suffix-length)))
           (code (ash (min 15 prefix) suffix-length)))
      (declare (type fixnum suffix-size code))
      (when (plusp suffix-size) (incf code (ub br suffix-size)))
      (when (and (>= prefix 15) (zerop suffix-length)) (incf code 15))
      (when (>= prefix 16) (incf code (- (ash 1 (- prefix 3)) 4096)))
      ;; the first level after fewer than three trailing ones cannot be +/-1
      (when (and (= i trailing-ones) (< trailing-ones 3)) (incf code 2))
      (let ((level (if (evenp code) (ash (+ code 2) -1) (ash (- (- code) 1) -1)))
            (next suffix-length))
        (declare (type fixnum level next))
        (when (zerop next) (setf next 1))
        (when (and (> (abs level) (ash 3 (1- next))) (< next 6)) (incf next))
        (values level next)))))

(defun %total-zeros (br total-coeff max-coeff)
  "How many zeros lie before the last coefficient in scan order."
  (declare (type fixnum total-coeff max-coeff))
  (if (= max-coeff 4)
      (%vlc br +chroma-dc-total-zeros-len+ +chroma-dc-total-zeros-bits+ (1- total-coeff) 4)
      (%vlc br +total-zeros-len+ +total-zeros-bits+ (1- total-coeff) 16)))

(defun %run-before (br zeros-left)
  (declare (type fixnum zeros-left))
  (if (zerop zeros-left)
      0
      (%vlc br +run-len+ +run-bits+ (1- (min zeros-left 7)) 16)))

;;; ---- a residual block --------------------------------------------------------------------------

(defun residual-block (br coeffs nc max-coeff &key (start 0))
  "Decode one residual block into COEFFS, a 16-element fixnum array in SCAN order (not raster).

   NC is the coeff_token context, or -1 for the 2x2 chroma DC block.  MAX-COEFF is 16 for a full
   4x4, 15 for the AC-only block of an Intra16x16 macroblock (whose DC is coded separately), and 4
   for chroma DC.  START is the scan position the block begins at, which is 1 for those AC blocks.

   Returns the number of coefficients decoded, which the caller stores as the nC of this block for
   its neighbours to read."
  (declare (type (simple-array fixnum (*)) coeffs) (type fixnum nc max-coeff start))
  (fill coeffs 0)
  (multiple-value-bind (total-coeff trailing-ones) (%coeff-token br nc)
    (declare (type fixnum total-coeff trailing-ones))
    (when (zerop total-coeff) (return-from residual-block 0))
    (when (> total-coeff max-coeff)
      (%err "coeff_token says ~d coefficients in a block that holds ~d" total-coeff max-coeff))
    (let ((levels (make-array total-coeff :element-type 'fixnum))
          (runs (make-array total-coeff :element-type 'fixnum :initial-element 0)))
      (declare (dynamic-extent levels runs))
      ;; the trailing ones: a sign bit each, magnitude known
      (dotimes (i trailing-ones)
        (setf (aref levels i) (if (= 1 (u1 br)) -1 1)))
      ;; the rest: escape-coded magnitudes with a widening suffix
      (let ((suffix-length (if (and (> total-coeff 10) (< trailing-ones 3)) 1 0)))
        (loop for i from trailing-ones below total-coeff
              do (multiple-value-bind (level next) (%level br i trailing-ones suffix-length)
                   (setf (aref levels i) level suffix-length next))))
      ;; where the zeros are
      (let ((zeros-left (if (< total-coeff max-coeff)
                            (%total-zeros br total-coeff max-coeff)
                            0)))
        (declare (type fixnum zeros-left))
        (loop for i from 0 below (1- total-coeff)
              while (plusp zeros-left)
              do (let ((r (%run-before br zeros-left)))
                   (setf (aref runs i) r)
                   (decf zeros-left r)))
        ;; the last coefficient absorbs whatever zeros are left over
        (setf (aref runs (1- total-coeff)) zeros-left))
      ;; Place them (9.2.4).  The levels came out highest-frequency FIRST, and each run is the
      ;; number of zeros before its own coefficient, so walking the decoded arrays BACKWARDS
      ;; accumulates scan positions forwards from zero.  Reading this loop the other way round is
      ;; the classic way to get a mirrored block that still decodes without erroring.
      (let ((coeff-num -1))
        (declare (type fixnum coeff-num))
        (loop for i of-type fixnum from (1- total-coeff) downto 0
              do (incf coeff-num (1+ (aref runs i)))
                 (let ((pos (+ start coeff-num)))
                   (when (>= pos (+ start max-coeff))
                     (%err "a coefficient landed at scan position ~d, past the ~d this block holds"
                           pos max-coeff))
                   (setf (aref coeffs pos) (aref levels i)))))
      total-coeff)))

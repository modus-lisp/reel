;;;; mpeg4/slice.lisp — the macroblock layer of ISO/IEC 14496-2.
;;;;
;;;; Three things here are genuinely unlike MPEG-2, and they are what the codec is FOR.
;;;;
;;;; INTRA BLOCKS PREDICT FROM THEIR NEIGHBOURS.  MPEG-2 predicts only the DC, and only from the
;;;; previous block of the same component in coding order.  MPEG-4 predicts the DC from whichever of
;;;; the left and above neighbours the gradient says is closer, and may predict the whole first row
;;;; or column of AC coefficients as well — which is why there are three scan orders and why a block
;;;; must remember its own edge coefficients for the next one to use.
;;;;
;;;; A MACROBLOCK MAY CARRY FOUR MOTION VECTORS.  One per 8x8 block, each predicted as the MEDIAN of
;;;; three neighbours rather than from a running predictor.  The median is what makes it robust: a
;;;; single wrong neighbour cannot drag the prediction with it.
;;;;
;;;; VECTORS MAY POINT OUTSIDE THE PICTURE.  MPEG-2 forbids it; MPEG-4 allows it and defines the
;;;; reference as edge-extended indefinitely, which is why motion compensation here clamps rather
;;;; than assuming.

(in-package #:reel.mpeg4)

(define-vlc dc-luma +dc-size-luma+ "dct_dc_size_luminance")
(define-vlc dc-chroma +dc-size-chroma+ "dct_dc_size_chrominance")
(define-vlc mcbpc-i +mcbpc-intra+ "MCBPC in an I-VOP")
(define-vlc mcbpc-p +mcbpc-inter+ "MCBPC in a P-VOP")
(define-vlc cbp-y +cbpy-table+ "CBPY")
(define-vlc mvd +mv-table+ "the motion vector difference magnitude")
(define-vlc btype +b-mb-type+ "the prediction mode of a B-VOP macroblock")
(define-vlc icoeff +intra-coeff-vlc+ "intra DCT coefficients")
(define-vlc pcoeff +inter-coeff-vlc+ "inter DCT coefficients")

;;; ---- the escape tables, derived rather than transmitted ---------------------------------------
;;;
;;; MPEG-4's escape is three escapes.  The first says "this level, plus the largest one the ordinary
;;; table can express for this run"; the second says "this run, plus the largest for this level";
;;; the third spells both out.  So a decoder needs to know, for every (last, run), the largest level
;;; the table holds — and those two tables are not transmitted, they are a property of the VLC table
;;; and are computed here once.

(defun %build-lmax (run level split)
  (let ((out (make-array '(2 64) :element-type 'fixnum :initial-element 0)))
    (dotimes (i 102 out)
      (let ((last (if (>= i split) 1 0)) (r (aref run i)) (l (aref level i)))
        (when (and (< r 64) (> l (aref out last r))) (setf (aref out last r) l))))))

(defun %build-rmax (run level split)
  (let ((out (make-array '(2 64) :element-type 'fixnum :initial-element 0)))
    (dotimes (i 102 out)
      (let ((last (if (>= i split) 1 0)) (r (aref run i)) (l (aref level i)))
        (when (and (< l 64) (> r (aref out last l))) (setf (aref out last l) r))))))

(defparameter +intra-lmax+ (%build-lmax +intra-coeff-run+ +intra-coeff-level+ +intra-last-split+))
(defparameter +intra-rmax+ (%build-rmax +intra-coeff-run+ +intra-coeff-level+ +intra-last-split+))
(defparameter +inter-lmax+ (%build-lmax +inter-coeff-run+ +inter-coeff-level+ +inter-last-split+))
(defparameter +inter-rmax+ (%build-rmax +inter-coeff-run+ +inter-coeff-level+ +inter-last-split+))
(declaim (type (simple-array fixnum (2 64))
               +intra-lmax+ +intra-rmax+ +inter-lmax+ +inter-rmax+))

;;; ---- a decoded picture ------------------------------------------------------------------------

(defstruct (frame (:conc-name fr-))
  (width 0 :type fixnum) (height 0 :type fixnum)
  (cwidth 0 :type fixnum) (cheight 0 :type fixnum)
  (y (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (u (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (v (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (ystride 0 :type fixnum) (cstride 0 :type fixnum)
  (coding-type 0 :type fixnum)
  ;; the motion of every 8x8 block, so that a B-VOP's direct mode can read it back
  (mvx (make-array 0 :element-type 'fixnum) :type fixnums)
  (mvy (make-array 0 :element-type 'fixnum) :type fixnums)
  (timestamp nil))

(defun make-frame-for (v)
  (let* ((cw (* 16 (vol-mb-width v))) (ch (* 16 (vol-mb-height v)))
         (nb (* 4 (vol-mb-width v) (vol-mb-height v))))
    (make-frame :width (vol-width v) :height (vol-height v)
                :cwidth cw :cheight ch
                :y (make-array (* cw ch) :element-type '(unsigned-byte 8) :initial-element 0)
                :u (make-array (* (ash cw -1) (ash ch -1)) :element-type '(unsigned-byte 8)
                               :initial-element 128)
                :v (make-array (* (ash cw -1) (ash ch -1)) :element-type '(unsigned-byte 8)
                               :initial-element 128)
                :ystride cw :cstride (ash cw -1)
                :mvx (make-array nb :element-type 'fixnum :initial-element 0)
                :mvy (make-array nb :element-type 'fixnum :initial-element 0))))

;;; ---- the state one picture carries -------------------------------------------------------------

(defstruct (state (:conc-name st-) (:constructor %make-state))
  br vol vop
  cur forward backward
  (mbx 0 :type fixnum) (mby 0 :type fixnum)
  (qscale 1 :type fixnum)
  (ac-pred nil)
  (intra-p nil)
  ;; where the current VIDEO PACKET began, and whether we are still on its first row.  A packet is
  ;; MPEG-4's resynchronisation unit: an encoder may cut a picture into several, and a decoder must
  ;; treat each one as a fresh start for prediction or a lost packet corrupts the rest of the
  ;; picture instead of just itself.
  (resync-mbx 0 :type fixnum) (resync-mby 0 :type fixnum)
  (first-line t)
  (dc-dir 0 :type fixnum)   ; which neighbour the DC prediction chose: 0 left, 1 above
  ;; the motion of every 8x8 block of THIS picture, on a grid with a zero border on the left and
  ;; top so that a neighbour off the edge reads as a vector of zero without a test
  (mv-w 0 :type fixnum)
  (mvx (make-array 0 :element-type 'fixnum) :type fixnums)
  (mvy (make-array 0 :element-type 'fixnum) :type fixnums)
  ;; the DC of every block, likewise bordered — with 1024 rather than 0, because that is the value
  ;; the specification says an absent neighbour predicts
  (dc-w 0 :type fixnum) (dcc-w 0 :type fixnum)
  (dc-y (make-array 0 :element-type 'fixnum) :type fixnums)
  (dc-u (make-array 0 :element-type 'fixnum) :type fixnums)
  (dc-v (make-array 0 :element-type 'fixnum) :type fixnums)
  ;; the quantiser each macroblock used, because AC prediction across a quantiser change has to
  ;; rescale what it borrows
  (mb-qscale (make-array 0 :element-type 'fixnum) :type fixnums)
  ;; the edge coefficients each block kept, for the next block to predict from: fourteen per block,
  ;; the left column at 1..7 and the top row at 9..15
  (ac-y (make-array 0 :element-type 'fixnum) :type fixnums)
  (ac-u (make-array 0 :element-type 'fixnum) :type fixnums)
  (ac-v (make-array 0 :element-type 'fixnum) :type fixnums)
  (block (make-array 64 :element-type 'fixnum) :type (simple-array fixnum (64)))
  (mv (make-array 8 :element-type 'fixnum) :type fixnums))   ; four (x,y) pairs

(defun make-state (v p cur forward backward)
  (let* ((mbw (vol-mb-width v)) (mbh (vol-mb-height v))
         (bw (+ (* 2 mbw) 2)) (bh (+ (* 2 mbh) 1))
         (cw (1+ mbw)) (chh (1+ mbh)))
    (%make-state :vol v :vop p :cur cur :forward forward :backward backward
                 :qscale (vop-qscale p)
                 :mv-w bw
                 :mvx (make-array (* bw bh) :element-type 'fixnum :initial-element 0)
                 :mvy (make-array (* bw bh) :element-type 'fixnum :initial-element 0)
                 :dc-w bw :dcc-w cw
                 :dc-y (make-array (* bw bh) :element-type 'fixnum :initial-element 1024)
                 :dc-u (make-array (* cw chh) :element-type 'fixnum :initial-element 1024)
                 :dc-v (make-array (* cw chh) :element-type 'fixnum :initial-element 1024)
                 :mb-qscale (make-array (* mbw mbh) :element-type 'fixnum
                                        :initial-element (vop-qscale p))
                 :ac-y (make-array (* 16 bw bh) :element-type 'fixnum :initial-element 0)
                 :ac-u (make-array (* 16 cw chh) :element-type 'fixnum :initial-element 0)
                 :ac-v (make-array (* 16 cw chh) :element-type 'fixnum :initial-element 0))))

;;; ---- block addressing --------------------------------------------------------------------------

(declaim (inline %blk-index %chroma-index))
(defun %blk-index (st n)
  "The bordered-grid index of luma block N of the current macroblock."
  (declare (type fixnum n) (optimize (speed 3) (safety 0)))
  (let ((bx (+ (* 2 (st-mbx st)) (logand n 1) 1))
        (by (+ (* 2 (st-mby st)) (ash n -1) 1)))
    (+ (* by (st-mv-w st)) bx)))

(defun %chroma-index (st)
  (declare (optimize (speed 3) (safety 0)))
  (+ (* (1+ (st-mby st)) (st-dcc-w st)) (1+ (st-mbx st))))

;;; ---- coefficients ------------------------------------------------------------------------------

(defun %scan-for (st)
  "Which of the three scans this block used.

   A block that predicted its AC coefficients from the block ABOVE has its first row accounted for,
   so what remains is scanned horizontally; one that predicted from the LEFT, vertically; and one
   that predicted nothing uses the zig-zag.  The choice is per BLOCK and follows the direction the
   DC prediction chose, which is why it cannot be decided in the picture header."
  (cond ((not (st-ac-pred st)) (if (vop-alternate-scan (st-vop st)) +alt-vertical-scan+ +zigzag+))
        ((= 0 (st-dc-dir st)) +alt-vertical-scan+)
        (t +alt-horizontal-scan+)))

(defun decode-block (st n intra-p coded dc-vlc-p)
  "One 8x8 block into (ST-BLOCK ST), as QUANTISED levels, with intra prediction already added.

   Quantised, not dequantised, and that is not an oversight: an intra block's coefficients may be
   predicted from a neighbour's, and the prediction is defined on the LEVELS.  Dequantisation
   therefore cannot happen until the prediction has.

   The DC of an intra block is coded with its own table when the quantiser is fine, and folded into
   the coefficient table when it is coarse — a per-picture switch that changes the shape of every
   intra block.  Either way the direction its prediction came from has to be known BEFORE the
   coefficients are read, because it decides which of the three scans they were written in."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (st-br st)) (blk (st-block st))
         (table (if intra-p +icoeff-table+ +pcoeff-table+))
         (bits (if intra-p +icoeff-bits+ +pcoeff-bits+))
         (runs (if intra-p +intra-coeff-run+ +inter-coeff-run+))
         (levels (if intra-p +intra-coeff-level+ +inter-coeff-level+))
         (split (if intra-p +intra-last-split+ +inter-last-split+))
         (lmax (if intra-p +intra-lmax+ +inter-lmax+))
         (rmax (if intra-p +intra-rmax+ +inter-rmax+))
         (dc-pred 0)
         (i -1))
    (declare (type fixnum bits split i dc-pred)
             (type (simple-array (unsigned-byte 8) (102)) runs levels)
             (type (simple-array fixnum (2 64)) lmax rmax))
    (fill blk 0)
    (when intra-p
      (if dc-vlc-p
          (progn (setf (aref blk 0) (%decode-dc st n)) (setf i 0))
          (multiple-value-bind (pred dir) (predict-dc st n)
            (setf (st-dc-dir st) dir dc-pred pred))))
    (let ((scan (%scan-for st)))
      (declare (type (simple-array (unsigned-byte 8) (64)) scan))
      (when coded
        (loop
          (let ((sym (or (%vlc br table bits)
                         (%err "a DCT coefficient at macroblock (~d,~d)" (st-mbx st) (st-mby st)))))
            (declare (type fixnum sym))
            (multiple-value-bind (last run level)
                (if (/= sym +coeff-escape+)
                    (values (if (>= sym split) 1 0) (aref runs sym)
                            (let ((l (aref levels sym))) (if (= 1 (read-bit br)) (- l) l)))
                    ;; THREE ESCAPES, told apart by one or two bits.
                    (cond
                      ((zerop (read-bit br))
                       ;; the first adds to the LEVEL: an ordinary code follows, and the level it
                       ;; names is the difference from the largest that run can otherwise express
                       (let ((s2 (or (%vlc br table bits)
                                     (%err "escape 1 at (~d,~d)" (st-mbx st) (st-mby st)))))
                         (declare (type fixnum s2))
                         (when (= s2 +coeff-escape+) (%err "an escape inside an escape"))
                         (let* ((lst (if (>= s2 split) 1 0)) (r (aref runs s2))
                                (l (+ (aref levels s2) (aref lmax lst r))))
                           (values lst r (if (= 1 (read-bit br)) (- l) l)))))
                      ((zerop (read-bit br))
                       ;; the second adds to the RUN, symmetrically
                       (let ((s2 (or (%vlc br table bits)
                                     (%err "escape 2 at (~d,~d)" (st-mbx st) (st-mby st)))))
                         (declare (type fixnum s2))
                         (when (= s2 +coeff-escape+) (%err "an escape inside an escape"))
                         (let* ((lst (if (>= s2 split) 1 0)) (l (aref levels s2))
                                (r (+ (aref runs s2) (aref rmax lst l) 1)))
                           (values lst r (if (= 1 (read-bit br)) (- l) l)))))
                      (t
                       ;; and the third spells both out, between marker bits
                       (let* ((lst (read-bit br)) (run (read-bits br 6)))
                         (marker-bit br)
                         (let ((level (read-signed br 12)))
                           (marker-bit br)
                           (values lst run level))))))
              (declare (type fixnum last run level))
              (incf i (1+ run))
              (when (> i 63)
                (%err "a run past the end of a block at (~d,~d)" (st-mbx st) (st-mby st)))
              (setf (aref blk (aref scan i)) level)
              (when (= 1 last) (return)))))))
    (when intra-p
      (unless dc-vlc-p
        (setf (aref blk 0) (+ (aref blk 0) (%dc-round dc-pred (%dc-scale st n))))
        (%store-dc st n (aref blk 0)))
      (predict-ac st n (st-dc-dir st)))
    blk))


;;; ---- intra prediction, on the levels ------------------------------------------------------------

(defun %dc-grid (st n)
  "(values grid index width) for the DC of block N."
  (if (< n 4)
      (values (st-dc-y st) (%blk-index st n) (st-mv-w st))
      (values (if (= n 4) (st-dc-u st) (st-dc-v st)) (%chroma-index st) (st-dcc-w st))))

(defun %ac-grid (st n)
  (if (< n 4)
      (values (st-ac-y st) (%blk-index st n) (st-mv-w st))
      (values (if (= n 4) (st-ac-u st) (st-ac-v st)) (%chroma-index st) (st-dcc-w st))))

(defun predict-dc (st n)
  "The DC predictor for block N, and which neighbour it came from (7.4.3.1).

   THE GRADIENT DECIDES, not the position: whichever of the left and above neighbours differs less
   from the corner between them is the better predictor, because a small difference there means the
   picture is flat in that direction.  That choice then also decides which way the AC coefficients
   are predicted and which scan the block was written in, so getting it wrong is not a small error."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-bind (grid idx w) (%dc-grid st n)
    (declare (type fixnums grid) (type fixnum idx w))
    (let ((a (aref grid (- idx 1)))          ; left
          (b (aref grid (- idx w 1)))        ; above-left
          (c (aref grid (- idx w))))         ; above
      (declare (type fixnum a b c))
      ;; A PACKET BOUNDARY IS AN EDGE.  On the first row of a video packet the blocks above are in
      ;; another packet and may not be predicted from, and at its first column the same is true to
      ;; the left — so those neighbours read as 1024, which is the value the specification gives an
      ;; absent one.  The rules are per NEIGHBOUR and not per block, which is why they read oddly.
      (when (st-first-line st)
        (when (/= n 3)
          (when (/= n 2) (setf b 1024 c 1024))
          (when (and (/= n 1) (= (st-mbx st) (st-resync-mbx st))) (setf b 1024 a 1024))))
      (when (and (= (st-mbx st) (st-resync-mbx st))
                 (= (st-mby st) (1+ (st-resync-mby st)))
                 (or (= n 0) (= n 4) (= n 5)))
        (setf b 1024))
      (if (< (abs (- a b)) (abs (- b c)))
          (values c 1)
          (values a 0)))))

(defun %decode-dc (st n)
  "The DC differential, added to the prediction and stored for the next block to predict from."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (st-br st))
         (size (or (if (< n 4)
                       (%vlc br +dc-luma-table+ +dc-luma-bits+)
                       (%vlc br +dc-chroma-table+ +dc-chroma-bits+))
                   (%err "dct_dc_size at macroblock (~d,~d)" (st-mbx st) (st-mby st))))
         (level 0))
    (declare (type fixnum size level))
    (unless (zerop size)
      (let ((v (read-bits br size)))
        (setf level (if (logbitp (1- size) v) v (- v (ash 1 size) -1))))
      ;; a long DC is followed by a marker, because eleven zero bits would otherwise look like the
      ;; start of a resync marker
      (when (> size 8) (marker-bit br)))
    (multiple-value-bind (pred dir) (predict-dc st n)
      (declare (type fixnum pred dir))
      (setf (st-dc-dir st) dir)
      (let ((dc (+ level (%dc-round pred (%dc-scale st n)))))
        (%store-dc st n dc)
        dc))))

(declaim (inline %dc-scale %dc-round))
(defun %dc-scale (st n)
  (if (< n 4) (aref +y-dc-scale+ (st-qscale st)) (aref +c-dc-scale+ (st-qscale st))))
(defun %dc-round (pred scale)
  (declare (type fixnum pred scale))
  (floor (+ pred (ash scale -1)) scale))

(defun %store-dc (st n level)
  "Keep this block's reconstructed DC, at full scale, for its neighbours."
  (declare (type fixnum level) (optimize (speed 3) (safety 1)))
  (multiple-value-bind (grid idx w) (%dc-grid st n)
    (declare (ignore w) (type fixnums grid))
    (setf (aref grid idx) (max 0 (min 2047 (* level (%dc-scale st n)))))))

(defun predict-ac (st n dir)
  "Add the neighbour's edge coefficients to this block's, and then keep this block's for the next.

   Only the first row or the first column is predicted — seven coefficients — and only when the
   macroblock said so.  The rescaling when the neighbour used a different quantiser is what makes
   this survive a rate-controlled encode: the levels mean different amounts on either side of a
   quantiser change, and adding them raw would be adding two different units together."
  (declare (optimize (speed 3) (safety 1)))
  (let ((blk (st-block st)))
    (multiple-value-bind (grid idx w) (%ac-grid st n)
      (declare (type fixnums grid) (type fixnum idx w))
      (when (st-ac-pred st)
        (let* ((mbw (vol-mb-width (st-vol st)))
               (mbi (+ (* (st-mby st) mbw) (st-mbx st)))
               (nidx (if (zerop dir) (- idx 1) (- idx w)))
               (nqp (cond ((zerop dir)
                           (if (zerop (st-mbx st)) (st-qscale st)
                               (aref (st-mb-qscale st) (1- mbi))))
                          (t (if (zerop (st-mby st)) (st-qscale st)
                                 (aref (st-mb-qscale st) (- mbi mbw))))))
               ;; within a macroblock the neighbour is this same macroblock, so no rescale
               (same (or (= nqp (st-qscale st))
                         (and (zerop dir) (or (= n 1) (= n 3)))
                         (and (= dir 1) (or (= n 2) (= n 3))))))
          (declare (type fixnum nidx nqp))
          (if (zerop dir)
              (loop for i of-type fixnum from 1 below 8
                    do (incf (aref blk (* i 8))
                             (let ((v (aref grid (+ (* 16 nidx) i))))
                               (if same v (round (* v nqp) (st-qscale st))))))
              (loop for i of-type fixnum from 1 below 8
                    do (incf (aref blk i)
                             (let ((v (aref grid (+ (* 16 nidx) 8 i))))
                               (if same v (round (* v nqp) (st-qscale st)))))))))
      ;; kept whether or not anything was predicted: the NEXT block may want them
      (loop for i of-type fixnum from 1 below 8
            do (setf (aref grid (+ (* 16 idx) i)) (aref blk (* i 8))
                     (aref grid (+ (* 16 idx) 8 i)) (aref blk i))))))

;;; ---- dequantisation ------------------------------------------------------------------------------

(defun dequant-intra (st n)
  "H.263-style dequantisation of an intra block (7.4.4).

   The DC is scaled by a table the quantiser indexes; every AC coefficient is doubled by the
   quantiser and pushed away from zero by an odd offset, which is what keeps the reconstruction
   levels odd and bounds the drift between decoders."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((blk (st-block st)) (q (st-qscale st))
         (qmul (* 2 q)) (qadd (logior (1- q) 1)))
    (declare (type fixnum q qmul qadd))
    (setf (aref blk 0) (* (aref blk 0) (%dc-scale st n)))
    (loop for i of-type fixnum from 1 below 64
          do (let ((l (aref blk i)))
               (declare (type fixnum l))
               (unless (zerop l)
                 (setf (aref blk i) (if (minusp l) (- (* l qmul) qadd) (+ (* l qmul) qadd))))))
    blk))

(defun dequant-intra-mpeg (st n)
  "The MPEG-style dequantiser a stream may choose instead of H.263's (7.4.4).

   Weight matrices rather than a flat step, and no odd offset — which is why a stream that uses it
   also needs mismatch control on its inter blocks, exactly as MPEG-2 does, and gets it below."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((blk (st-block st)) (q (* 2 (st-qscale st)))
         (m (or (vol-intra-matrix (st-vol st)) +default-intra-matrix+)))
    (declare (type fixnum q) (type (simple-array (unsigned-byte 8) (64)) m))
    (setf (aref blk 0) (* (aref blk 0) (%dc-scale st n)))
    (loop for i of-type fixnum from 1 below 64
          do (let ((l (aref blk i)))
               (declare (type fixnum l))
               (unless (zerop l)
                 (setf (aref blk i)
                       (if (minusp l)
                           (- (ash (* (- l) q (aref m i)) -4))
                           (ash (* l q (aref m i)) -4))))))
    blk))

(defun dequant-inter-mpeg (st)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((blk (st-block st)) (q (* 2 (st-qscale st)))
         (m (or (vol-inter-matrix (st-vol st)) +default-inter-matrix+))
         (sum -1))
    (declare (type fixnum q sum) (type (simple-array (unsigned-byte 8) (64)) m))
    (loop for i of-type fixnum from 0 below 64
          do (let ((l (aref blk i)))
               (declare (type fixnum l))
               (unless (zerop l)
                 (let ((v (if (minusp l)
                              (- (ash (* (1+ (* 2 (- l))) q (aref m i)) -5))
                              (ash (* (1+ (* 2 l)) q (aref m i)) -5))))
                   (setf (aref blk i) v)
                   (incf sum v)))))
    ;; mismatch control, the same device MPEG-2 uses and for the same reason
    (setf (aref blk 63) (logxor (aref blk 63) (logand sum 1)))
    blk))

(defun dequant-inter (st)
  "And of a non-intra block, where there is no DC to treat apart."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((blk (st-block st)) (q (st-qscale st))
         (qmul (* 2 q)) (qadd (logior (1- q) 1)))
    (declare (type fixnum q qmul qadd))
    (loop for i of-type fixnum from 0 below 64
          do (let ((l (aref blk i)))
               (declare (type fixnum l))
               (unless (zerop l)
                 (setf (aref blk i) (if (minusp l) (- (* l qmul) qadd) (+ (* l qmul) qadd))))))
    blk))

;;; ---- motion vectors ------------------------------------------------------------------------------

(defun decode-mv-component (st pred fcode)
  "One motion vector component: a Huffman magnitude, an optional residual, and a modulo wrap."
  (declare (type fixnum pred fcode) (optimize (speed 3) (safety 1)))
  (let* ((br (st-br st))
         (code (or (%vlc br +mvd-table+ +mvd-bits+)
                   (%err "a motion vector at macroblock (~d,~d)" (st-mbx st) (st-mby st)))))
    (declare (type fixnum code))
    (when (zerop code) (return-from decode-mv-component pred))
    (let* ((sign (read-bit br))
           (shift (1- fcode))
           (val code))
      (declare (type fixnum sign shift val))
      (when (plusp shift)
        (setf val (logior (ash (1- val) shift) (read-bits br shift)))
        (incf val))
      (when (= 1 sign) (setf val (- val)))
      (incf val pred)
      ;; the difference is reduced modulo the range f_code allows, so a vector that would overflow
      ;; comes back round the other side; sign-extending to 5 + f_code bits is what recovers it
      (let* ((bits (+ 5 fcode))
             (m (logand val (1- (ash 1 bits)))))
        (declare (type fixnum bits m))
        (if (logbitp (1- bits) m) (- m (ash 1 bits)) m)))))

(declaim (inline %mid3))
(defun %mid3 (a b c)
  "The median of three, which is what MPEG-4 predicts a motion vector with.

   Not the mean: a median cannot be dragged by a single wrong neighbour, and the neighbour above-
   right is often in a macroblock that moved for a different reason."
  (declare (type fixnum a b c) (optimize (speed 3) (safety 0)))
  (max (min a b) (min (max a b) c)))

(defun predict-mv (st n)
  "(values px py) for block N: the median of left, above, and above-right (7.5.1).

   The top row of a picture is the exception, and it is not the general rule with zeros substituted.
   Blocks 0 and 1 there take the LEFT neighbour alone, because a median of one real value and two
   zeros is zero and would pull every vector in the first row towards nothing."
  (declare (type fixnum n) (optimize (speed 3) (safety 1)))
  (let* ((w (st-mv-w st))
         (idx (%blk-index st n))
         (offc (case n (0 2) (1 1) (2 1) (t -1)))
         (a (- idx 1)) (b (- idx w)) (c (+ idx offc (- w))))
    (declare (type fixnum w idx offc a b c))
    (cond
      ((and (st-first-line st) (< n 3))
       (cond
         ((= n 2)
          ;; block 2's above and above-right are inside this macroblock, so only the left is at
          ;; risk, and it is only at risk at the packet's first column
          (let ((ax (if (= (st-mbx st) (st-resync-mbx st)) 0 (aref (st-mvx st) a)))
                (ay (if (= (st-mbx st) (st-resync-mbx st)) 0 (aref (st-mvy st) a))))
            (values (%mid3 ax (aref (st-mvx st) b) (aref (st-mvx st) c))
                    (%mid3 ay (aref (st-mvy st) b) (aref (st-mvy st) c)))))
         ((and (zerop n) (= (st-mbx st) (st-resync-mbx st))) (values 0 0))
         ((= (1+ (st-mbx st)) (st-resync-mbx st))
          ;; the packet begins in the NEXT macroblock, so the above-right is available and the
          ;; above is not.  Rare, and the only reason this branch is not `use the left'.
          (if (zerop (st-mbx st))
              (values (aref (st-mvx st) c) (aref (st-mvy st) c))
              (values (%mid3 (aref (st-mvx st) a) 0 (aref (st-mvx st) c))
                      (%mid3 (aref (st-mvy st) a) 0 (aref (st-mvy st) c)))))
         (t
          ;; THE LEFT ALONE, not a median with two zeros.  A median of one real value and two
          ;; absent ones is zero, and it would pull every vector in a packet's first row to nothing.
          (values (aref (st-mvx st) a) (aref (st-mvy st) a)))))
      (t
       (values (%mid3 (aref (st-mvx st) a) (aref (st-mvx st) b) (aref (st-mvx st) c))
               (%mid3 (aref (st-mvy st) a) (aref (st-mvy st) b) (aref (st-mvy st) c)))))))

(defun %set-mv (st n mx my)
  (declare (type fixnum n mx my) (optimize (speed 3) (safety 1)))
  (let ((idx (%blk-index st n)))
    (setf (aref (st-mvx st) idx) mx (aref (st-mvy st) idx) my)))

(defparameter +chroma-round+
  (make-array 16 :element-type '(unsigned-byte 8)
              :initial-contents '(0 0 0 1 1 1 1 1 0 0 0 0 0 0 1 1))
  "The rounding a FOUR-vector macroblock's chroma vector uses (7.5.5).  Not a shift: the four luma
   vectors are summed and this table decides where the sum lands, which is not the same answer as
   halving each one and averaging.")

(declaim (inline %round-chroma))
(defun %round-chroma (sum)
  (declare (type fixnum sum) (optimize (speed 3) (safety 0)))
  (+ (aref +chroma-round+ (logand sum 15)) (ash sum -3)))

;;; ---- motion compensation --------------------------------------------------------------------------

(defun predict-block (dst dstride dbase plane stride w h px py bw bh mvx mvy rounding)
  "Predict a BW x BH block at (PX,PY) with the half-pel vector (MVX,MVY).

   ROUNDING is the picture's rounding_type, and it is not a detail: it flips the tie-break in every
   averaged sample, and a decoder that ignores it drifts a little further from the encoder in every
   predicted picture until the next intra one.  Vectors may point OUTSIDE the picture — MPEG-2
   forbids that and MPEG-4 does not — so the reference is read with its edges extended."
  (declare (type octets dst plane)
           (type fixnum dstride dbase stride w h px py bw bh mvx mvy rounding)
           (optimize (speed 3) (safety 1)))
  (let* ((sx (+ px (ash mvx -1))) (sy (+ py (ash mvy -1)))
         (hx (logand mvx 1)) (hy (logand mvy 1))
         (r (- 1 rounding))
         (inside (and (>= sx 0) (>= sy 0) (<= (+ sx bw hx) w) (<= (+ sy bh hy) h))))
    (declare (type fixnum sx sy hx hy r))
    (macrolet ((each ((yv xv) form)
                 `(dotimes (,yv bh)
                    (declare (type fixnum ,yv))
                    (let ((o (+ dbase (* ,yv dstride))))
                      (declare (type fixnum o))
                      (dotimes (,xv bw)
                        (declare (type fixnum ,xv))
                        (setf (aref dst (+ o ,xv)) ,form))))))
      (if inside
          (let ((b (+ (* sy stride) sx)))
            (declare (type fixnum b))
            (macrolet ((p (dy dx) `(aref plane (+ b (* (+ y ,dy) stride) x ,dx))))
              (cond ((and (zerop hx) (zerop hy)) (each (y x) (p 0 0)))
                    ((and (= 1 hx) (zerop hy)) (each (y x) (ash (+ (p 0 0) (p 0 1) r) -1)))
                    ((and (zerop hx) (= 1 hy)) (each (y x) (ash (+ (p 0 0) (p 1 0) r) -1)))
                    (t (each (y x) (ash (+ (p 0 0) (p 0 1) (p 1 0) (p 1 1) r 1) -2))))))
          (macrolet ((p (dy dx) `(aref plane (+ (* (min (1- h) (max 0 (+ sy y ,dy))) stride)
                                                (min (1- w) (max 0 (+ sx x ,dx)))))))
            (cond ((and (zerop hx) (zerop hy)) (each (y x) (p 0 0)))
                  ((and (= 1 hx) (zerop hy)) (each (y x) (ash (+ (p 0 0) (p 0 1) r) -1)))
                  ((and (zerop hx) (= 1 hy)) (each (y x) (ash (+ (p 0 0) (p 1 0) r) -1)))
                  (t (each (y x) (ash (+ (p 0 0) (p 0 1) (p 1 0) (p 1 1) r 1) -2)))))))))

(defun %predict-macroblock (st ref four-mv-p)
  "Motion compensate this macroblock from REF."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((cur (st-cur st)) (mbx (st-mbx st)) (mby (st-mby st))
         (ys (fr-ystride cur)) (cs (fr-cstride cur))
         (cw (fr-cwidth cur)) (ch (fr-cheight cur))
         (ccw (ash cw -1)) (cch (ash ch -1))
         (r (vop-rounding (st-vop st)))
         (mv (st-mv st))
         (ybase (+ (* mby 16 ys) (* mbx 16)))
         (cbase (+ (* mby 8 cs) (* mbx 8))))
    (declare (type fixnum ys cs cw ch ccw cch r ybase cbase))
    (if four-mv-p
        (dotimes (i 4)
          (let ((bx (* 8 (logand i 1))) (by (* 8 (ash i -1))))
            (predict-block (fr-y cur) ys (+ ybase (* by ys) bx) (fr-y ref) ys cw ch
                           (+ (* mbx 16) bx) (+ (* mby 16) by) 8 8
                           (aref mv (* 2 i)) (aref mv (1+ (* 2 i))) r)))
        (predict-block (fr-y cur) ys ybase (fr-y ref) ys cw ch
                       (* mbx 16) (* mby 16) 16 16 (aref mv 0) (aref mv 1) r))
    ;; ONE RULE FOR ONE VECTOR AND FOR FOUR.  The chroma vector is the SUM of the four luma vectors
    ;; put through a rounding table, and a macroblock with one vector simply has four copies of it.
    ;; Halving the single vector instead looks equivalent and is not: at a luma vector of one they
    ;; disagree, which is every odd vector, which is half of them.  The luma is then perfect and the
    ;; chroma alone is soft — a symptom that reads like a colour-space bug rather than a motion one.
    (multiple-value-bind (cx cy)
        (if four-mv-p
            (values (%round-chroma (+ (aref mv 0) (aref mv 2) (aref mv 4) (aref mv 6)))
                    (%round-chroma (+ (aref mv 1) (aref mv 3) (aref mv 5) (aref mv 7))))
            (values (%round-chroma (* 4 (aref mv 0))) (%round-chroma (* 4 (aref mv 1)))))
      (declare (type fixnum cx cy))
      (predict-block (fr-u cur) cs cbase (fr-u ref) cs ccw cch
                     (* mbx 8) (* mby 8) 8 8 cx cy r)
      (predict-block (fr-v cur) cs cbase (fr-v ref) cs ccw cch
                     (* mbx 8) (* mby 8) 8 8 cx cy r))))

;;; ---- one macroblock ------------------------------------------------------------------------------

(defparameter +dquant-table+ (make-array 4 :element-type 'fixnum :initial-contents '(-1 -2 1 2))
  "What the two bits after a `dquant' macroblock type do to the quantiser.")
(declaim (type (simple-array fixnum (4)) +dquant-table+))

(defun %block-base (st n)
  "(values plane stride base) for block N of the current macroblock."
  (let* ((cur (st-cur st)) (ys (fr-ystride cur)) (cs (fr-cstride cur))
         (ybase (+ (* (st-mby st) 16 ys) (* (st-mbx st) 16)))
         (cbase (+ (* (st-mby st) 8 cs) (* (st-mbx st) 8))))
    (declare (type fixnum ys cs ybase cbase))
    (if (< n 4)
        (values (fr-y cur) ys (+ ybase (* (ash n -1) 8 ys) (* 8 (logand n 1))))
        (values (if (= n 4) (fr-u cur) (fr-v cur)) cs cbase))))

(defun %set-qscale (st delta)
  (setf (st-qscale st) (max 1 (min 31 (+ (st-qscale st) delta))))
  (setf (aref (st-mb-qscale st)
              (+ (* (st-mby st) (vol-mb-width (st-vol st))) (st-mbx st)))
        (st-qscale st)))

(defun %clear-motion (st)
  "An intra macroblock has no motion, and its neighbours must see zero rather than stale vectors."
  (dotimes (i 4) (%set-mv st i 0 0)))

(defun decode-intra-macroblock (st mcbpc dquant)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (st-br st))
         (ac-pred (= 1 (read-bit br)))
         (cbpy (or (%vlc br +cbp-y-table+ +cbp-y-bits+)
                   (%err "CBPY at macroblock (~d,~d)" (st-mbx st) (st-mby st))))
         (cbp (logior (logand mcbpc 3) (ash cbpy 2)))
         (dc-vlc-p (< (st-qscale st) (vop-intra-dc-threshold (st-vop st)))))
    (declare (type fixnum cbpy cbp))
    (setf (st-ac-pred st) ac-pred (st-intra-p st) t)
    (when dquant (%set-qscale st (aref +dquant-table+ (read-bits br 2))))
    ;; the threshold is read against the quantiser AFTER dquant, which is easy to get backwards
    (setf dc-vlc-p (< (st-qscale st) (vop-intra-dc-threshold (st-vop st))))
    (%clear-motion st)
    (dotimes (n 6)
      (decode-block st n t (logbitp (- 5 n) cbp) dc-vlc-p)
      (if (vol-mpeg-quant (st-vol st)) (dequant-intra-mpeg st n) (dequant-intra st n))
      (multiple-value-bind (plane stride base) (%block-base st n)
        (reel.mpeg2:idct-put plane stride base (st-block st))))))

(defun decode-inter-macroblock (st mcbpc dquant)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((br (st-br st))
         (cbpy (logxor 15 (or (%vlc br +cbp-y-table+ +cbp-y-bits+)
                              (%err "CBPY at macroblock (~d,~d)" (st-mbx st) (st-mby st)))))
         (cbp (logior (logand mcbpc 3) (ash cbpy 2)))
         (four-mv-p (logbitp 4 mcbpc))
         (fcode (vop-f-code (st-vop st)))
         (mv (st-mv st)))
    (declare (type fixnum cbpy cbp fcode))
    (setf (st-ac-pred st) nil (st-intra-p st) nil)
    (when dquant (%set-qscale st (aref +dquant-table+ (read-bits br 2))))
    ;; THE VECTORS COME BEFORE THE COEFFICIENTS, and each is predicted from neighbours that include
    ;; the earlier blocks of this same macroblock — so they must be recorded as they are read
    (if four-mv-p
        (dotimes (i 4)
          (multiple-value-bind (px py) (predict-mv st i)
            (let ((mx (decode-mv-component st px fcode))
                  (my (decode-mv-component st py fcode)))
              (setf (aref mv (* 2 i)) mx (aref mv (1+ (* 2 i))) my)
              (%set-mv st i mx my))))
        (multiple-value-bind (px py) (predict-mv st 0)
          (let ((mx (decode-mv-component st px fcode))
                (my (decode-mv-component st py fcode)))
            (dotimes (i 4)
              (setf (aref mv (* 2 i)) mx (aref mv (1+ (* 2 i))) my)
              (%set-mv st i mx my)))))
    (unless (st-forward st) (%err "a predicted macroblock with no reference picture"))
    (%predict-macroblock st (st-forward st) four-mv-p)
    (dotimes (n 6)
      (when (logbitp (- 5 n) cbp)
        (decode-block st n nil t nil)
        (if (vol-mpeg-quant (st-vol st)) (dequant-inter-mpeg st) (dequant-inter st))
        (multiple-value-bind (plane stride base) (%block-base st n)
          (reel.mpeg2:idct-add plane stride base (st-block st)))))))

(defun decode-skip-macroblock (st)
  "A macroblock the stream did not code: a zero vector from the forward reference, and nothing else."
  (declare (optimize (speed 3) (safety 1)))
  (setf (st-ac-pred st) nil (st-intra-p st) nil)
  (let ((mv (st-mv st)))
    (dotimes (i 8) (setf (aref mv i) 0))
    (%clear-motion st))
  (unless (st-forward st) (%err "a skipped macroblock with no reference picture"))
  (%predict-macroblock st (st-forward st) nil))

(defun decode-macroblock (st)
  "One macroblock of an I- or P-VOP."
  (declare (optimize (speed 3) (safety 1)))
  (let ((br (st-br st)) (type (vop-coding-type (st-vop st))))
    (cond
      ((= type +vop-i+)
       (let ((mcbpc (loop for v = (or (%vlc br +mcbpc-i-table+ +mcbpc-i-bits+)
                                      (%err "MCBPC at macroblock (~d,~d)" (st-mbx st) (st-mby st)))
                          ;; value 8 is macroblock stuffing, which exists so that an encoder can
                          ;; pad to a byte without inventing a macroblock
                          until (/= v 8) finally (return v))))
         (declare (type fixnum mcbpc))
         (decode-intra-macroblock st mcbpc (logbitp 2 mcbpc))))
      (t
       (when (= 1 (read-bit br))
         (decode-skip-macroblock st)
         (return-from decode-macroblock))
       (let ((mcbpc (loop for v = (or (%vlc br +mcbpc-p-table+ +mcbpc-p-bits+)
                                      (%err "MCBPC at macroblock (~d,~d)" (st-mbx st) (st-mby st)))
                          until (/= v 20) finally (return v))))
         (declare (type fixnum mcbpc))
         (if (logbitp 2 mcbpc)
             (decode-intra-macroblock st mcbpc (logbitp 3 mcbpc))
             (decode-inter-macroblock st mcbpc (logbitp 3 mcbpc))))))))

(defun decode-vop (st)
  "Every macroblock of one video object plane, across however many video packets it was cut into."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((v (st-vol st)) (mbw (vol-mb-width v)) (total (* mbw (vol-mb-height v))))
    (declare (type fixnum mbw total))
    (let ((mb 0))
      (declare (type fixnum mb))
      (loop while (< mb total)
            do (when (%at-resync-p st)
                 (setf mb (%read-packet-header st))
                 (setf (st-resync-mbx st) (mod mb mbw)
                       (st-resync-mby st) (floor mb mbw)
                       (st-first-line st) t))
               (setf (st-mbx st) (mod mb mbw) (st-mby st) (floor mb mbw))
               ;; FIRST LINE is not "the first row of the packet": it stays true until the
               ;; macroblock directly BELOW the one the packet started at.  For a packet that
               ;; begins mid-row that includes part of the next row, which is exactly the region
               ;; whose neighbours above are in the previous packet.
               (when (and (= (st-mbx st) (st-resync-mbx st))
                          (= (st-mby st) (1+ (st-resync-mby st))))
                 (setf (st-first-line st) nil))
               (setf (aref (st-mb-qscale st) mb) (st-qscale st))
               (decode-macroblock st)
               (incf mb)))
    ;; the motion of every block, kept on the picture so that a later B-VOP's direct mode can ask
    (let ((cur (st-cur st)) (mbw (vol-mb-width v)))
      (dotimes (mby (vol-mb-height v))
        (dotimes (mbx mbw)
          (dotimes (n 4)
            (let ((src (+ (* (+ (* 2 mby) (ash n -1) 1) (st-mv-w st))
                          (+ (* 2 mbx) (logand n 1) 1)))
                  (dst (+ (* 4 (+ (* mby mbw) mbx)) n)))
              (setf (aref (fr-mvx cur) dst) (aref (st-mvx st) src)
                    (aref (fr-mvy cur) dst) (aref (st-mvy st) src)))))))))

;;; ---- video packets -----------------------------------------------------------------------------

(defparameter +resync-prefix+
  (make-array 8 :element-type 'fixnum
              :initial-contents '(#x7f00 #x7e00 #x7c00 #x7800 #x7000 #x6000 #x4000 #x0000))
  "What the next sixteen bits look like at each bit offset when a resync marker follows.

   Not the marker itself: MPEG-4 pads to a byte boundary before one with a ZERO followed by ONES,
   so what a decoder actually sees is that padding and then the marker's leading zeros.  Which
   pattern depends on how far into the byte we are, hence a table of eight.")
(declaim (type (simple-array fixnum (8)) +resync-prefix+))

(defun %packet-prefix-length (st)
  "How many zeros a resync marker has, which depends on the picture type and its f_code."
  (let ((p (st-vop st)))
    (case (vop-coding-type p)
      (#.+vop-i+ 16)
      (#.+vop-b+ (+ 15 (max (vop-f-code p) (vop-b-code p) 2)))
      (t (+ 15 (vop-f-code p))))))

(defun %at-resync-p (st)
  (and (vol-resync-marker (st-vol st))
       (= (peek-bits (st-br st) 16)
          (aref +resync-prefix+ (logand (br-pos (st-br st)) 7)))))

(defun %read-packet-header (st)
  "The header that follows a resync marker (6.2.5.2).  Returns the macroblock it restarts at.

   The packet may repeat the picture header — that is what header_extension_code is for, and it
   exists so that a decoder that JOINED the stream mid-picture can still decode the rest of it.
   Everything in the repeat is already known here, so it is read and dropped; but read it must be."
  (let* ((br (st-br st)) (v (st-vol st)) (p (st-vop st))
         (mb-num (* (vol-mb-width v) (vol-mb-height v)))
         (num-bits (max 1 (integer-length (1- (max 2 mb-num))))))
    (declare (type fixnum mb-num num-bits))
    ;; the stuffing, then the marker's zeros and the one that ends them
    (read-bit br)
    (setf (br-pos br) (* 8 (ceiling (br-pos br) 8)))
    (let ((len 0))
      (declare (type fixnum len))
      (loop while (and (< len 32) (zerop (read-bit br))) do (incf len))
      (unless (= len (%packet-prefix-length st))
        (%err "a resync marker of ~d zeros where ~d were expected" len (%packet-prefix-length st))))
    (let ((n (read-bits br num-bits)))
      (declare (type fixnum n))
      (when (or (zerop n) (>= n mb-num))
        (%err "a video packet restarting at macroblock ~d of ~d" n mb-num))
      (let ((q (read-bits br (vol-quant-precision v))))
        (when (plusp q) (setf (st-qscale st) q)))
      (when (= 1 (read-bit br))                  ; header_extension_code
        (loop while (= 1 (read-bit br)))         ; modulo_time_base
        (marker-bit br)
        (read-bits br (vol-time-increment-bits v))
        (marker-bit br)
        (read-bits br 2)                         ; vop_coding_type
        (read-bits br 3)                         ; intra_dc_vlc_thr
        (unless (= (vop-coding-type p) +vop-i+) (read-bits br 3))
        (when (= (vop-coding-type p) +vop-b+) (read-bits br 3)))
      n)))

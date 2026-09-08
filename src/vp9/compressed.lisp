;;;; vp9/compressed.lisp — the second header, the one inside the arithmetic coder.
;;;;
;;;; VP9 does not send its probabilities.  It keeps FOUR saved contexts, a frame names one of them,
;;;; and the compressed header sends DIFFERENCES from that context — after which the frame may
;;;; write its adapted models back for the next frame to start from.  So a probability in a VP9
;;;; stream is a running quantity with a history, and a decoder that gets one wrong does not produce
;;;; a slightly wrong picture: it desynchronises at the first block that reads it and stays wrong
;;;; until the next key frame.
;;;;
;;;; THE DIFFERENCE IS CODED CLEVERLY AND THE CLEVERNESS IS LOAD-BEARING.  A probability lives in
;;;; [1,255], so the distance to any other value is at most 254 — but the distance is not symmetric
;;;; about the current value, and the part that IS symmetric is coded as twice the magnitude plus a
;;;; sign, with the remainder stacked on top.  That absolute difference is then sent as a variable
;;;; length code, because a large change is unlikely.  `%update-prob' below is that, and
;;;; +INV-MAP-TABLE+ is the table it indexes.
;;;;
;;;; The motion vector models do NOT use it.  They send seven bits and set the low bit, which is a
;;;; different scheme for no reason anyone has written down, and following the pattern instead of
;;;; the specification puts the decoder one bit out for the rest of the header.

(in-package #:reel.vp9)

(defconstant +tx-4x4+ 0) (defconstant +tx-8x8+ 1)
(defconstant +tx-16x16+ 2) (defconstant +tx-32x32+ 3)
(defconstant +tx-switchable+ 4)

(defconstant +pred-single+ 0) (defconstant +pred-comp+ 1) (defconstant +pred-switchable+ 2)

;;; ---- the models ----------------------------------------------------------------------------------

(defmacro %def-probs ()
  "The probability context, as a struct of named arrays mirroring the specification's own layout.

   One array per model rather than one flat block, because every one of them is indexed by a
   different number of context dimensions and naming them is the only thing that keeps the reader
   honest about which."
  (let ((slots '((y-mode (4 9)) (uv-mode (10 9)) (filter (4 2)) (mv-mode (7 3))
                 (intra (4)) (comp (5)) (single-ref (5 2)) (comp-ref (5))
                 (tx32p (2 3)) (tx16p (2 2)) (tx8p (2)) (skip (3)) (mv-joint (3))
                 (mv-sign (2)) (mv-classes (2 10)) (mv-class0 (2)) (mv-bits (2 10))
                 (mv-class0-fp (2 2 3)) (mv-fp (2 3)) (mv-class0-hp (2)) (mv-hp (2))
                 (partition (4 4 3)))))
    `(progn
       (defstruct (probs (:conc-name pr-))
         ,@(loop for (name dims) in slots
                 collect `(,name (make-array ',dims :element-type '(unsigned-byte 8))
                                 :type (simple-array (unsigned-byte 8) ,dims)))
         ;; ELEVEN, not three: the three that are transmitted and the eight derived from the third
         (coef (make-array '(4 2 2 6 6 11) :element-type '(unsigned-byte 8))
               :type (simple-array (unsigned-byte 8) (4 2 2 6 6 11))))
       (defun %reset-probs (p)
         "Every model back to the value the format begins from."
         ,@(loop for (name dims) in slots
                 collect `(%copy-into (,(intern (format nil "PR-~a" name)) p)
                                      ,(intern (format nil "+DEFAULT-~a+" name))))
         p)
       (defun %copy-probs (dst src)
         "Every model of SRC into DST, which is how a frame starts from a saved context."
         ,@(loop for (name dims) in slots
                 collect `(%copy-into (,(intern (format nil "PR-~a" name)) dst)
                                      (,(intern (format nil "PR-~a" name)) src)))
         (%copy-into (pr-coef dst) (pr-coef src))
         dst))))

(declaim (inline %copy-into))
(defun %copy-into (dst src)
  "Copy one probability array into another of the same shape, whatever its rank."
  (declare (type (simple-array (unsigned-byte 8)) dst src))
  (dotimes (i (array-total-size dst))
    (setf (row-major-aref dst i) (row-major-aref src i))))

(%def-probs)

(defstruct (context (:conc-name ctx-))
  "One of the four saved probability contexts: the models as of the last frame that wrote here.

   The coefficient models are kept in their TRANSMITTED form, three per context, because that is
   what a later frame sends differences against — the eight derived ones would be redundant and
   would drift if they were saved and restored separately."
  (p (%reset-probs (make-probs)) :type probs)
  (coef (make-array '(4 2 2 6 6 3) :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (4 2 2 6 6 3))))

(defun make-default-context ()
  (let ((c (make-context)))
    (%reset-probs (ctx-p c))
    (%copy-into (ctx-coef c) +default-coef-probs+)
    c))

;;; ---- reading an update ---------------------------------------------------------------------------

(declaim (inline %inv-recenter))
(defun %inv-recenter (v m)
  "Undo the recentring that made small changes cheap: V is a distance from M, folded so that the
   two directions interleave."
  (declare (type fixnum v m) (optimize (speed 3) (safety 0)))
  (cond ((> v (* 2 m)) v)
        ((oddp v) (- m (ash (1+ v) -1)))
        (t (+ m (ash v -1)))))

(defun %update-prob (c p)
  "One probability, as a difference from P (6.3.5).

   Four escapes, each buying a wider field: four bits, four more, five, then seven with a further
   doubling above sixty-five.  A large change is possible and expensive, which is the right way
   round."
  (declare (type bool c) (type fixnum p) (optimize (speed 3) (safety 1)))
  (let ((d (cond ((zerop (bool-flag c)) (bool-literal c 4))
                 ((zerop (bool-flag c)) (+ 16 (bool-literal c 4)))
                 ((zerop (bool-flag c)) (+ 32 (bool-literal c 5)))
                 (t (let ((v (bool-literal c 7)))
                      (declare (type fixnum v))
                      (when (>= v 65) (setf v (+ (- (* 2 v) 65) (bool-flag c))))
                      (+ v 64))))))
    (declare (type fixnum d))
    (when (>= d 255) (%err "a probability update of ~d, which is past the table" d))
    (let ((m (aref +inv-map-table+ d)))
      (declare (type fixnum m))
      (if (<= p 128)
          (+ 1 (%inv-recenter m (1- p)))
          (- 255 (%inv-recenter m (- 255 p)))))))

(defmacro %maybe-update (c place)
  "Update PLACE if the stream says so.  The gate is a single decision at probability 252, which is
   to say `almost never' — most models go a whole frame untouched."
  `(when (plusp (bool-bit ,c 252)) (setf ,place (%update-prob ,c ,place))))

(defmacro %maybe-set-mv (c place)
  "The same gate, with the motion vector models' own encoding behind it: seven bits, doubled, with
   the low bit set — so an update can never make a probability even and can never make it zero."
  `(when (plusp (bool-bit ,c 252))
     (setf ,place (logior (ash (bool-literal ,c 7) 1) 1))))

;;; ---- the compressed header -----------------------------------------------------------------------

(defstruct (frame-probs (:conc-name fp-))
  "What the compressed header produces: the models this frame will decode with, and the two mode
   decisions that are only made here."
  (p (make-probs) :type probs)
  (tx-mode 0 :type fixnum)
  (comp-pred-mode 0 :type fixnum))

(defun %read-coef-probs (c live saved tx-mode)
  "The coefficient models, which are the bulk of the header and are read in transform-size order.

   THE LOOP STOPS AT THE FRAME'S TRANSFORM SIZE.  A frame that never uses a 32x32 transform does not
   send probabilities for one, so the four sizes are walked in order and the walk ends at the size
   the frame declared — reading all four regardless consumes bits belonging to the next field."
  (declare (type bool c)
           (type (simple-array (unsigned-byte 8) (4 2 2 6 6 11)) live)
           (type (simple-array (unsigned-byte 8) (4 2 2 6 6 3)) saved)
           (type fixnum tx-mode))
  (dotimes (i 4)
    (let ((update (plusp (bool-flag c))))
      (dotimes (j 2)
        (dotimes (k 2)
          (dotimes (l 6)
            (dotimes (m 6)
              ;; the DC band has three neighbour contexts and not six
              (when (and (zerop l) (>= m 3)) (return))
              (dotimes (n 3)
                (let ((r (aref saved i j k l m n)))
                  (setf (aref live i j k l m n)
                        (if (and update (plusp (bool-bit c 252))) (%update-prob c r) r))))
              ;; and the other eight follow from the third, by the Pareto model
              (let ((third (aref live i j k l m 2)))
                (dotimes (n 8)
                  (setf (aref live i j k l m (+ 3 n)) (aref +model-pareto8+ third n)))))))))
    (when (= tx-mode i) (return))))

(defun read-compressed-header (bytes start size h ctx)
  "The compressed header of one frame: BYTES[START,START+SIZE) read as one arithmetic-coded
   partition, against the saved context CTX.

   Returns a FRAME-PROBS.  The saved context is not modified — a frame that refreshes it does so
   after decoding, with the models it ended up with rather than the ones it started from."
  (declare (type octets bytes) (type fixnum start size))
  (let* ((c (make-bool bytes start (+ start size)))
         (fp (make-frame-probs))
         (p (fp-p fp))
         (key-or-intra (or (h-keyframe h) (h-intra-only h))))
    (%copy-probs p (ctx-p ctx))
    ;; a marker bit that must be zero: the one place the format checks that the partition begins
    ;; where the uncompressed header said it did
    (when (plusp (bool-flag c)) (%err "a compressed header whose marker bit is set"))
    ;; ---- transform size
    (if (h-lossless h)
        (setf (fp-tx-mode fp) +tx-4x4+)
        (let ((m (bool-literal c 2)))
          (when (= m 3) (incf m (bool-flag c)))
          (setf (fp-tx-mode fp) m)
          (when (= m +tx-switchable+)
            (dotimes (i 2) (%maybe-update c (aref (pr-tx8p p) i)))
            (dotimes (i 2) (dotimes (j 2) (%maybe-update c (aref (pr-tx16p p) i j))))
            (dotimes (i 2) (dotimes (j 3) (%maybe-update c (aref (pr-tx32p p) i j)))))))
    ;; ---- coefficients
    (%read-coef-probs c (pr-coef p) (ctx-coef ctx) (fp-tx-mode fp))
    ;; ---- skip, and then everything an inter frame needs and a key frame does not
    (dotimes (i 3) (%maybe-update c (aref (pr-skip p) i)))
    (unless key-or-intra
      (dotimes (i 7) (dotimes (j 3) (%maybe-update c (aref (pr-mv-mode p) i j))))
      (when (h-filter-switchable h)
        (dotimes (i 4) (dotimes (j 2) (%maybe-update c (aref (pr-filter p) i j)))))
      (dotimes (i 4) (%maybe-update c (aref (pr-intra p) i)))
      (if (h-allow-comp-inter h)
          (let ((m (bool-flag c)))
            (when (plusp m) (incf m (bool-flag c)))
            (setf (fp-comp-pred-mode fp) m)
            (when (= m +pred-switchable+)
              (dotimes (i 5) (%maybe-update c (aref (pr-comp p) i)))))
          (setf (fp-comp-pred-mode fp) +pred-single+))
      (unless (= (fp-comp-pred-mode fp) +pred-comp+)
        (dotimes (i 5)
          (%maybe-update c (aref (pr-single-ref p) i 0))
          (%maybe-update c (aref (pr-single-ref p) i 1))))
      (unless (= (fp-comp-pred-mode fp) +pred-single+)
        (dotimes (i 5) (%maybe-update c (aref (pr-comp-ref p) i))))
      (dotimes (i 4) (dotimes (j 9) (%maybe-update c (aref (pr-y-mode p) i j))))
      ;; THE PARTITION LEVELS ARRIVE BACKWARDS, smallest block first, which is the one place in the
      ;; header where the order is not the order of the array
      (dotimes (i 4)
        (dotimes (j 4)
          (dotimes (k 3) (%maybe-update c (aref (pr-partition p) (- 3 i) j k)))))
      ;; ---- and the motion vector models, on their own encoding
      (dotimes (i 3) (%maybe-set-mv c (aref (pr-mv-joint p) i)))
      (dotimes (i 2)
        (%maybe-set-mv c (aref (pr-mv-sign p) i))
        (dotimes (j 10) (%maybe-set-mv c (aref (pr-mv-classes p) i j)))
        (%maybe-set-mv c (aref (pr-mv-class0 p) i))
        (dotimes (j 10) (%maybe-set-mv c (aref (pr-mv-bits p) i j))))
      (dotimes (i 2)
        (dotimes (j 2) (dotimes (k 3) (%maybe-set-mv c (aref (pr-mv-class0-fp p) i j k))))
        (dotimes (j 3) (%maybe-set-mv c (aref (pr-mv-fp p) i j))))
      (when (h-high-precision-mv h)
        (dotimes (i 2)
          (%maybe-set-mv c (aref (pr-mv-class0-hp p) i))
          (%maybe-set-mv c (aref (pr-mv-hp p) i)))))
    (values fp c)))

;;;; vp9/decode.lisp — a packet in, pictures out, and the eight reference slots between.
;;;;
;;;; VP9 keeps EIGHT reference slots and a frame says which of them it reads and which it writes, as
;;;; a bitmask.  That is more than any other codec here: H.264 has a sliding window over a list,
;;;; MPEG-2 has two slots and a rule.  Eight named slots with an explicit refresh mask is what lets
;;;; VP9 keep a long-term golden frame and an alt-ref built from the future at the same time, and it
;;;; is why a VP9 stream can be cut at almost any frame and still be decodable a few frames later.
;;;;
;;;; It also keeps FOUR probability contexts, and a frame names one to read and may write it back.
;;;; When the stream sets frame-parallel mode the write-back is the forward-updated probabilities —
;;;; what the compressed header produced — and nothing depends on the frame having been decoded.
;;;; When it does not, the write-back is the ADAPTED probabilities, computed from the symbol counts
;;;; of the frame just decoded, and the next frame cannot start until this one has finished.  Which
;;;; of those a stream chose is one bit in the uncompressed header, and getting it wrong makes every
;;;; frame after the first wrong in a way that looks like a coefficient bug.

(in-package #:reel.vp9)

(defstruct (decoder (:conc-name d-) (:constructor %make-decoder))
  (ctxs (make-array 4) :type simple-vector)     ; the four saved probability contexts
  (refs (make-array 8 :initial-element nil) :type simple-vector)   ; the eight reference frames
  (ref-sizes (make-array 8 :initial-element nil) :type simple-vector)
  last-frame                                    ; the frame decoded before this one
  mvref-prev segmap-prev                        ; and its motion field and segment map
  (last-invisible nil)
  (last-keyframe nil)                           ; whether the frame before this one was a key frame
  (frames 0 :type fixnum)
  (comp-blocks 0 :type fixnum))                 ; how many blocks used two references

(defun make-vp9-decoder ()
  (let ((d (%make-decoder)))
    (dotimes (i 4 d) (setf (aref (d-ctxs d) i) (make-default-context)))))

(defun %reset-contexts (d which)
  "Which of the four contexts a frame's reset code clears (6.2.2).

   A key frame clears all four; an intra-only frame clears all four for code three and only its own
   for code two.  The point of the codes is that a stream can drop back to known probabilities
   without sending a key frame."
  (declare (type decoder d) (type fixnum which))
  (dotimes (i 4)
    (when (or (= which -1) (= which i))
      (setf (aref (d-ctxs d) i) (make-default-context)))))

(defun decode-frame (d bytes start end)
  "One VP9 frame — not one packet: a packet may be a superframe of several.  Returns a FRAME, or NIL
   for a frame that produces no picture."
  (declare (type octets bytes) (type fixnum start end))
  (let ((h (parse-header bytes start end :ref-sizes (d-ref-sizes d))))
    ;; ---- a frame that only shows a reference again, which is how a hidden alt-ref becomes visible
    (when (h-show-existing h)
      (let ((f (aref (d-refs d) (h-show-existing h))))
        (unless f (%err "a frame showing reference ~d, which is not there" (h-show-existing h)))
        (setf (d-last-invisible d) nil)
        (return-from decode-frame f)))
    (when (h-keyframe h) (%reset-contexts d -1))
    (when (and (h-intra-only h) (plusp (h-reset-context h)))
      (%reset-contexts d (if (= (h-reset-context h) 3) -1 (h-frame-context h))))
    (let* ((hb (+ start (h-header-bytes h)))
           (fp (read-compressed-header bytes hb (h-compressed-size h) h
                                       (aref (d-ctxs d) (h-frame-context h))))
           (st (make-state h fp)))
      ;; THE CONTEXT IS SAVED BEFORE THE TILES ARE READ, not after, when the stream is in
      ;; frame-parallel mode: what it saves is the forward-updated probabilities, so a decoder can
      ;; start the next frame's header without waiting for this frame's samples.
      (when (and (h-refresh-context h) (h-parallel-mode h))
        (%save-context (aref (d-ctxs d) (h-frame-context h)) fp (fp-tx-mode fp)))
      ;; the motion field of the previous frame, which vector prediction may read
      (setf (st-mvref-prev st) (d-mvref-prev d)
            (st-segmap-prev st) (d-segmap-prev d)
            (st-use-last-mvs st)
            (and (not (h-error-resilient h))
                 (not (d-last-invisible d))
                 (d-last-frame d)
                 (= (fr-width (d-last-frame d)) (h-width h))
                 (= (fr-height (d-last-frame d)) (h-height h))
                 (d-mvref-prev d)
                 t))
      ;; the references this frame predicts from
      (unless (or (h-keyframe h) (h-intra-only h))
        (dotimes (i 3)
          (let ((f (aref (d-refs d) (aref (h-ref-idx h) i))))
            (unless f (%err "reference ~d is not there" (aref (h-ref-idx h) i)))
            (unless (and (= (fr-width f) (h-width h)) (= (fr-height f) (h-height h)))
              (%err "a reference at ~dx~d for a ~dx~d frame — scaled prediction is not supported"
                    (fr-width f) (fr-height f) (h-width h) (h-height h)))
            (setf (aref (st-ref-frames st) i) f))))
      (decode-tiles bytes (+ hb (h-compressed-size h)) end st)
      (when (and (h-refresh-context h) (not (h-parallel-mode h)))
        (%adapt-probs (aref (d-ctxs d) (h-frame-context h)) st fp h (d-last-keyframe d)))
      ;; ---- and the slots this frame writes
      (let ((f (st-frame st)))
        (dotimes (i 8)
          (when (logbitp i (h-refresh-mask h))
            (setf (aref (d-refs d) i) f
                  (aref (d-ref-sizes d) i) (cons (h-width h) (h-height h)))))
        (incf (d-comp-blocks d) (st-comp-blocks st))
        (setf (d-last-frame d) f
              (d-last-keyframe d) (h-keyframe h)
              (d-mvref-prev d) (st-mvref st)
              (d-segmap-prev d) (st-segmap st)
              (d-last-invisible d) (not (h-show-frame h)))
        (incf (d-frames d))
        (if (h-show-frame h) f nil)))))

(defun %save-context (ctx fp tx-mode)
  "The forward-updated probabilities, into a saved context.

   The coefficient models are saved in their TRANSMITTED form — three per context, not the eleven
   the decoder uses — because that is what the next frame will send differences against.  And the
   loop stops at the frame's transform size for the same reason the read did."
  (declare (type context ctx) (type fixnum tx-mode))
  (%copy-probs (ctx-p ctx) (fp-p fp))
  (let ((live (pr-coef (fp-p fp))) (saved (ctx-coef ctx)))
    (dotimes (i 4)
      (dotimes (j 2)
        (dotimes (k 2)
          (dotimes (l 6)
            (dotimes (m 6)
              (dotimes (n 3)
                (setf (aref saved i j k l m n) (aref live i j k l m n)))))))
      (when (= tx-mode i) (return))))
  (values))

(defun %adapt-probs (ctx st fp h last-keyframe)
  "Every model, from this frame's counts (6.4.x).

   The order is the specification's and the groupings are not arbitrary: a probability that splits a
   tree node is adapted against the counts of everything BELOW that node on each side, so the sums
   are taken as the tree is descended and each one is the previous minus what has just been used."
  (declare (type context ctx) (type state st) (type header h))
  (let* ((cn (st-counts st)) (p (ctx-p ctx)) (sv (ctx-coef ctx)) (live (fp-p fp))
         (keyish (or (h-keyframe h) (h-intra-only h)))
         ;; a frame that follows a key frame, or is one, trusts its counts a little less: the
         ;; models it started from were the defaults rather than something learned
         (uf (if (or keyish (not last-keyframe)) 112 128)))
    (declare (type fixnum uf))
    ;; ---- coefficients, which are most of the symbols in a frame
    (dotimes (i 4)
      (dotimes (j 2)
        (dotimes (k 2)
          (dotimes (l 6)
            (dotimes (m 6)
              (when (and (zerop l) (>= m 3)) (return))
              (%adapt (aref sv i j k l m 0) (aref (cn-eob cn) i j k l m 0)
                      (aref (cn-eob cn) i j k l m 1) 24 uf)
              (%adapt (aref sv i j k l m 1) (aref (cn-coef cn) i j k l m 0)
                      (+ (aref (cn-coef cn) i j k l m 1) (aref (cn-coef cn) i j k l m 2)) 24 uf)
              (%adapt (aref sv i j k l m 2) (aref (cn-coef cn) i j k l m 1)
                      (aref (cn-coef cn) i j k l m 2) 24 uf))))))
    ;; ---- a key frame adapts nothing else: it COPIES the three models that an intra block still
    ;; uses, because it has no inter statistics to learn from
    (when keyish
      (dotimes (i 3) (setf (aref (pr-skip p) i) (aref (pr-skip live) i)))
      (dotimes (i 2)
        (setf (aref (pr-tx8p p) i) (aref (pr-tx8p live) i))
        (dotimes (j 3) (setf (aref (pr-tx32p p) i j) (aref (pr-tx32p live) i j)))
        (dotimes (j 2) (setf (aref (pr-tx16p p) i j) (aref (pr-tx16p live) i j))))
      (return-from %adapt-probs))
    (dotimes (i 3)
      (%adapt (aref (pr-skip p) i) (aref (cn-skip cn) i 0) (aref (cn-skip cn) i 1)))
    (dotimes (i 4)
      (%adapt (aref (pr-intra p) i) (aref (cn-intra cn) i 0) (aref (cn-intra cn) i 1)))
    (when (= (fp-comp-pred-mode fp) +pred-switchable+)
      (dotimes (i 5)
        (%adapt (aref (pr-comp p) i) (aref (cn-comp cn) i 0) (aref (cn-comp cn) i 1))))
    (unless (= (fp-comp-pred-mode fp) +pred-single+)
      (dotimes (i 5)
        (%adapt (aref (pr-comp-ref p) i) (aref (cn-comp-ref cn) i 0) (aref (cn-comp-ref cn) i 1))))
    (unless (= (fp-comp-pred-mode fp) +pred-comp+)
      (dotimes (i 5)
        (dotimes (j 2)
          (%adapt (aref (pr-single-ref p) i j)
                  (aref (cn-single-ref cn) i j 0) (aref (cn-single-ref cn) i j 1)))))
    (dotimes (i 4)
      (dotimes (j 4)
        (let ((c (cn-partition cn)))
          (%adapt (aref (pr-partition p) i j 0) (aref c i j 0)
                  (+ (aref c i j 1) (aref c i j 2) (aref c i j 3)))
          (%adapt (aref (pr-partition p) i j 1) (aref c i j 1)
                  (+ (aref c i j 2) (aref c i j 3)))
          (%adapt (aref (pr-partition p) i j 2) (aref c i j 2) (aref c i j 3)))))
    (when (= (fp-tx-mode fp) +tx-switchable+)
      (dotimes (i 2)
        (%adapt (aref (pr-tx8p p) i) (aref (cn-tx8p cn) i 0) (aref (cn-tx8p cn) i 1))
        (%adapt (aref (pr-tx16p p) i 0) (aref (cn-tx16p cn) i 0)
                (+ (aref (cn-tx16p cn) i 1) (aref (cn-tx16p cn) i 2)))
        (%adapt (aref (pr-tx16p p) i 1) (aref (cn-tx16p cn) i 1) (aref (cn-tx16p cn) i 2))
        (%adapt (aref (pr-tx32p p) i 0) (aref (cn-tx32p cn) i 0)
                (+ (aref (cn-tx32p cn) i 1) (aref (cn-tx32p cn) i 2) (aref (cn-tx32p cn) i 3)))
        (%adapt (aref (pr-tx32p p) i 1) (aref (cn-tx32p cn) i 1)
                (+ (aref (cn-tx32p cn) i 2) (aref (cn-tx32p cn) i 3)))
        (%adapt (aref (pr-tx32p p) i 2) (aref (cn-tx32p cn) i 2) (aref (cn-tx32p cn) i 3))))
    (when (= 3 (h-filter-mode h))
      (dotimes (i 4)
        (%adapt (aref (pr-filter p) i 0) (aref (cn-filter cn) i 0)
                (+ (aref (cn-filter cn) i 1) (aref (cn-filter cn) i 2)))
        (%adapt (aref (pr-filter p) i 1) (aref (cn-filter cn) i 1) (aref (cn-filter cn) i 2))))
    ;; the inter modes are counted in bitstream order and adapted in TREE order, which is why the
    ;; indices below look shuffled: zero motion is the tree's first branch and the third symbol
    (dotimes (i 7)
      (let ((c (cn-mv-mode cn)))
        (%adapt (aref (pr-mv-mode p) i 0) (aref c i 2)
                (+ (aref c i 1) (aref c i 0) (aref c i 3)))
        (%adapt (aref (pr-mv-mode p) i 1) (aref c i 0) (+ (aref c i 1) (aref c i 3)))
        (%adapt (aref (pr-mv-mode p) i 2) (aref c i 1) (aref c i 3))))
    (let ((c (cn-mv-joint cn)))
      (%adapt (aref (pr-mv-joint p) 0) (aref c 0) (+ (aref c 1) (aref c 2) (aref c 3)))
      (%adapt (aref (pr-mv-joint p) 1) (aref c 1) (+ (aref c 2) (aref c 3)))
      (%adapt (aref (pr-mv-joint p) 2) (aref c 2) (aref c 3)))
    (dotimes (i 2)
      (%adapt (aref (pr-mv-sign p) i) (aref (cn-mv-sign cn) i 0) (aref (cn-mv-sign cn) i 1))
      (let ((c (cn-mv-classes cn)) (sum 0))
        (declare (type fixnum sum))
        (loop for k of-type fixnum from 1 to 10 do (incf sum (aref c i k)))
        (%adapt (aref (pr-mv-classes p) i 0) (aref c i 0) sum)
        (decf sum (aref c i 1))
        (%adapt (aref (pr-mv-classes p) i 1) (aref c i 1) sum)
        (decf sum (+ (aref c i 2) (aref c i 3)))
        (%adapt (aref (pr-mv-classes p) i 2) (+ (aref c i 2) (aref c i 3)) sum)
        (%adapt (aref (pr-mv-classes p) i 3) (aref c i 2) (aref c i 3))
        (decf sum (+ (aref c i 4) (aref c i 5)))
        (%adapt (aref (pr-mv-classes p) i 4) (+ (aref c i 4) (aref c i 5)) sum)
        (%adapt (aref (pr-mv-classes p) i 5) (aref c i 4) (aref c i 5))
        (decf sum (aref c i 6))
        (%adapt (aref (pr-mv-classes p) i 6) (aref c i 6) sum)
        (%adapt (aref (pr-mv-classes p) i 7) (+ (aref c i 7) (aref c i 8))
                (+ (aref c i 9) (aref c i 10)))
        (%adapt (aref (pr-mv-classes p) i 8) (aref c i 7) (aref c i 8))
        (%adapt (aref (pr-mv-classes p) i 9) (aref c i 9) (aref c i 10)))
      (%adapt (aref (pr-mv-class0 p) i) (aref (cn-mv-class0 cn) i 0) (aref (cn-mv-class0 cn) i 1))
      (dotimes (j 10)
        (%adapt (aref (pr-mv-bits p) i j)
                (aref (cn-mv-bits cn) i j 0) (aref (cn-mv-bits cn) i j 1)))
      (dotimes (j 2)
        (let ((c (cn-mv-class0-fp cn)))
          (%adapt (aref (pr-mv-class0-fp p) i j 0) (aref c i j 0)
                  (+ (aref c i j 1) (aref c i j 2) (aref c i j 3)))
          (%adapt (aref (pr-mv-class0-fp p) i j 1) (aref c i j 1)
                  (+ (aref c i j 2) (aref c i j 3)))
          (%adapt (aref (pr-mv-class0-fp p) i j 2) (aref c i j 2) (aref c i j 3))))
      (let ((c (cn-mv-fp cn)))
        (%adapt (aref (pr-mv-fp p) i 0) (aref c i 0)
                (+ (aref c i 1) (aref c i 2) (aref c i 3)))
        (%adapt (aref (pr-mv-fp p) i 1) (aref c i 1) (+ (aref c i 2) (aref c i 3)))
        (%adapt (aref (pr-mv-fp p) i 2) (aref c i 2) (aref c i 3)))
      (when (h-high-precision-mv h)
        (%adapt (aref (pr-mv-class0-hp p) i)
                (aref (cn-mv-class0-hp cn) i 0) (aref (cn-mv-class0-hp cn) i 1))
        (%adapt (aref (pr-mv-hp p) i) (aref (cn-mv-hp cn) i 0) (aref (cn-mv-hp cn) i 1))))
    ;; ---- and the intra modes, whose tree is walked in a different order again
    (macrolet ((modes (probs cnt n)
                 `(dotimes (i ,n)
                    (let ((c ,cnt) (sum 0) (s2 0))
                      (declare (type fixnum sum s2))
                      (setf sum (+ (aref c i 0) (aref c i 1) (aref c i 3) (aref c i 4)
                                   (aref c i 5) (aref c i 6) (aref c i 7) (aref c i 8)
                                   (aref c i 9)))
                      (%adapt (aref ,probs i 0) (aref c i 2) sum)
                      (decf sum (aref c i 9))
                      (%adapt (aref ,probs i 1) (aref c i 9) sum)
                      (decf sum (aref c i 0))
                      (%adapt (aref ,probs i 2) (aref c i 0) sum)
                      (setf s2 (+ (aref c i 1) (aref c i 4) (aref c i 5)))
                      (decf sum s2)
                      (%adapt (aref ,probs i 3) s2 sum)
                      (decf s2 (aref c i 1))
                      (%adapt (aref ,probs i 4) (aref c i 1) s2)
                      (%adapt (aref ,probs i 5) (aref c i 4) (aref c i 5))
                      (decf sum (aref c i 3))
                      (%adapt (aref ,probs i 6) (aref c i 3) sum)
                      (decf sum (aref c i 7))
                      (%adapt (aref ,probs i 7) (aref c i 7) sum)
                      (%adapt (aref ,probs i 8) (aref c i 6) (aref c i 8))))))
      (modes (pr-y-mode p) (cn-y-mode cn) 4)
      (modes (pr-uv-mode p) (cn-uv-mode cn) 10)))
  (values))

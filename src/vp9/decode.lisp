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
  (frames 0 :type fixnum))

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
        (%adapt-context (aref (d-ctxs d) (h-frame-context h)) st fp h))
      ;; ---- and the slots this frame writes
      (let ((f (st-frame st)))
        (dotimes (i 8)
          (when (logbitp i (h-refresh-mask h))
            (setf (aref (d-refs d) i) f
                  (aref (d-ref-sizes d) i) (cons (h-width h) (h-height h)))))
        (setf (d-last-frame d) f
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

(defun %adapt-context (ctx st fp h)
  "Backward adaptation: this frame's symbol COUNTS, nudging the saved context towards them.

   Not implemented, and refused rather than skipped.  A stream that asks for it and does not get it
   decodes every frame after the first against probabilities that drift further from the encoder's
   with each one — which looks like a coefficient bug and is not.  Streams from libvpx set
   frame-parallel mode and take the forward path instead, which is why this has not been needed yet."
  (declare (ignore ctx st fp h))
  (%err "this stream refreshes its probability context by BACKWARD ADAPTATION, which is not ~
         implemented; only the frame-parallel forward refresh is"))

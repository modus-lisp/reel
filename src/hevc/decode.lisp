;;;; hevc/decode.lisp — NAL units in, pictures out.
;;;;
;;;; The picture-level bookkeeping, which is where a decoder that gets every block right can still
;;;; produce the wrong video.  Three things happen once per picture and not once per slice: the
;;;; order count is derived, the reference picture set decides what survives in the buffer, and the
;;;; two loop filters run over the finished thing.  H.264 taught this the hard way — running the
;;;; per-picture epilogue per slice deblocked a three-slice picture three times over — so here it
;;;; is a separate function called from exactly one place.

(in-package #:reel.hevc)

(defvar *skip-loop-filters* nil
  "When true, neither in-loop filter runs.  For separating a reconstruction error from a filter
   error: ffmpeg's -skip_loop_filter gives the matching reference, and the difference between the
   two comparisons says which half of the decoder to look at.")

(defun %finish-picture (d)
  "Filter the picture currently open, put it in the buffer, and hand out whatever is now due."
  (let ((pic (dec-current d))
        (sh (dec-cur-sh d)))
    (when (and pic sh)
      (unless *skip-loop-filters*
        (deblock-picture pic (sh-pps sh) sh)
        (sao-picture pic sh))
      (incf (dec-frames d))
      (push pic (dec-dpb d))
      (setf (dec-current d) nil (dec-cur-sh d) nil)
      ;; A picture may be output once every picture that precedes it on SCREEN has been decoded.
      ;; The sequence parameter set says how far decoding order may run ahead of display order, and
      ;; that number is what makes this bounded rather than "wait for the end of the stream".
      (let* ((reorder (sps-max-num-reorder (sh-sps sh)))
             (pending (remove-if #'pic-output-done (dec-dpb d))))
        (loop while (> (length pending) reorder)
              do (let ((next (first (sort (copy-list pending) #'< :key #'pic-poc))))
                   (setf (pic-output-done next) t)
                   (setf (dec-out d) (append (dec-out d) (list next)))
                   (setf pending (remove next pending)))))
      pic)))

(defun %random-access-gate (d nal poc)
  "True when this picture is a RANDOM ACCESS SKIPPED LEADING picture that cannot be decoded.

   A stream may legitimately begin at a clean random access point rather than an IDR, and the
   pictures that FOLLOW that point in decode order but PRECEDE it on screen were encoded to predict
   from pictures before it — which a decoder joining here has never seen.  They are not corrupt and
   they are not an error; they are addressed to a decoder that started earlier.  Decoding them
   anyway produces plausible garbage and, worse, shifts every frame after them by one.

   The gate opens once: whichever picture arrives first fixes the boundary, and an IDR fixes it at
   nothing to skip because an IDR's leading pictures can all be decoded."
  (declare (type decoder d))
  (when (= (dec-max-ra d) most-positive-fixnum)
    (cond ((or (nal-cra-p nal) (nal-bla-p nal)) (setf (dec-max-ra d) poc))
          ((nal-idr-p nal) (setf (dec-max-ra d) most-negative-fixnum))))
  (cond
    ((and (nal-rasl-p nal) (<= poc (dec-max-ra d))) t)
    (t (when (and (= (nal-type nal) +nal-rasl-r+) (> poc (dec-max-ra d)))
         (setf (dec-max-ra d) most-negative-fixnum))
       nil)))

(defun %start-picture (d sh nal poc)
  "Everything a new picture needs before its first macroblock: its order count, the buffer it may
   predict from, and the two reference lists."
  (let* ((sps (sh-sps sh))
         (pic (make-picture-for sps)))
    (setf (pic-poc pic) poc
          (pic-reference-p pic) (nal-reference-p nal)
          ;; a slice segment never inherits across a picture boundary
          (dec-indep-sh d) nil
          (dec-ctx d) nil)
    (multiple-value-bind (before after) (%apply-rps d sh poc nal)
      ;; the current picture goes in only after the set has been applied, so it cannot keep itself
      (setf (dec-current d) pic)
      (%build-lists d sh before after))
    pic))

(defun feed-nal (d nal)
  "Give one NAL unit to the decoder.  Returns a picture when one became ready, else NIL."
  (cond
    ((not (nal-base-layer-p nal)) nil)          ; an enhancement layer is not ours to decode
    ((= (nal-type nal) +nal-sps+)
     (let ((s (parse-sps (nal-rbsp nal)))) (setf (gethash (sps-id s) (dec-sps d)) s))
     nil)
    ((= (nal-type nal) +nal-pps+)
     (let ((p (parse-pps (nal-rbsp nal)))) (setf (gethash (pps-id p) (dec-pps d)) p))
     nil)
    ((nal-slice-p nal)
     (let* ((br (make-bitreader (nal-rbsp nal)))
            (sh (%inherit-slice-header d (parse-slice-header br nal (dec-sps d) (dec-pps d)))))
       (when (sh-first-in-pic sh)
         (%finish-picture d)
         (let ((poc (%picture-order-count d sh nal)))
           (setf (dec-skipping d) (%random-access-gate d nal poc))
           (unless (dec-skipping d)
             (%start-picture d sh nal poc))))
       (when (dec-skipping d) (return-from feed-nal nil))
       (unless (dec-current d) (%err "a slice arrived before any picture was started"))
       (setf (dec-cur-sh d) sh)
       (unless (sh-dependent sh) (setf (dec-indep-sh d) sh))
       (setf (dec-ctx d)
             (decode-slice-data (sh-sps sh) (sh-pps sh) sh br (dec-current d)
                                (dec-list0 d) (dec-list1 d) (%collocated d sh)
                                (dec-ctx d)))
       (pop (dec-out d))))
    (t nil)))

(defun %inherit-slice-header (d sh)
  "Fill in what a DEPENDENT slice segment did not send.

   It sends its picture parameter set, its address, its entry points and its byte alignment, and
   nothing else: the slice type, the quantiser, the reference lists, the SAO flags and both
   deblocking offsets all belong to the last independent segment and are simply carried forward.
   Which is why the flag has to be honoured at the picture level rather than in the parser — the
   parser has no way of knowing what came before it."
  (if (not (sh-dependent sh))
      sh
      (let ((base (or (dec-indep-sh d)
                      (%err "a dependent slice segment with no independent one before it"))))
        (let ((merged (copy-slice base)))
          (setf (sh-dependent merged) t
                (sh-first-in-pic merged) nil
                (sh-nal merged) (sh-nal sh)
                (sh-segment-address merged) (sh-segment-address sh)
                (sh-entry-points merged) (sh-entry-points sh))
          merged))))

(defun %collocated (d sh)
  "The picture a temporal merge candidate comes from, named by list and index in the slice header."
  (when (sh-temporal-mvp sh)
    (let* ((list (if (or (not (sh-b-slice-p sh)) (sh-collocated-from-l0 sh))
                     (dec-list0 d) (dec-list1 d)))
           (i (sh-collocated-ref-idx sh)))
      (and (< i (length list)) (aref list i)))))

(defun flush-decoder (d)
  "Every picture still held, in display order.  Without this the tail of every stream is lost in
   the reorder buffer, which is a real bug and an easy one not to notice."
  (%finish-picture d)
  (dolist (p (sort (remove-if #'pic-output-done (dec-dpb d)) #'< :key #'pic-poc))
    (setf (pic-output-done p) t)
    (setf (dec-out d) (append (dec-out d) (list p))))
  (prog1 (dec-out d) (setf (dec-out d) '())))

(defun decode-annex-b (bytes &key (limit most-positive-fixnum))
  "Decode an Annex B byte stream.  Returns a list of PICTUREs in DISPLAY order."
  (let ((d (make-hevc-decoder))
        (out '()))
    (dolist (nal (annex-b-nals bytes))
      (when (>= (length out) limit) (return))
      (let ((p (feed-nal d nal))) (when p (push p out))))
    (dolist (p (flush-decoder d)) (push p out))
    (nreverse out)))

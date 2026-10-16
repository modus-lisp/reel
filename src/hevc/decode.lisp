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
      ;; A PICTURE MAY BE DECODED AND NOT SHOWN.  pic_output_flag says so, and the reason it exists
      ;; is that a picture can be worth decoding purely as a reference — an encoder can insert one
      ;; that nothing displays but everything after predicts from.  Marking it as already handed
      ;; out is how it stays in the buffer for as long as the reference set wants it while never
      ;; reaching the output: the alternative, dropping it, loses the pictures that depend on it.
      (unless (sh-pic-output sh) (setf (pic-output-done pic) t))
      (setf (pic-needed pic) t)         ; until the next reference picture set says otherwise
      (push pic (dec-dpb d))
      (setf (dec-current d) nil (dec-cur-sh d) nil)
      ;; A picture may be output once every picture that precedes it on SCREEN has been decoded.
      ;; The sequence parameter set says how far decoding order may run ahead of display order, and
      ;; that number is what makes this bounded rather than "wait for the end of the stream".
      ;; TWO REASONS TO HAND A PICTURE OUT, and only one of them is about reordering.  The obvious
      ;; one is that more pictures are waiting than the sequence said could ever be out of order,
      ;; so the earliest of them can no longer be overtaken.  The other is that THE BUFFER IS FULL:
      ;; the sequence also states how many pictures a decoder must be able to hold, and a decoder
      ;; that holds more than that is not the decoder the encoder was writing for.  Bumping only on
      ;; the first condition keeps pictures the encoder expected to be gone — which is invisible
      ;; until something discards the buffer, and then they are lost instead of late.
      (let* ((sps (sh-sps sh))
             (reorder (sps-max-num-reorder sps))
             (cap (sps-max-dec-pic-buffering sps)))
        (loop for pending = (remove-if #'pic-output-done (dec-dpb d))
              while (and pending
                         (or (> (length pending) reorder)
                             ;; STRICTLY greater, because the picture just decoded is already in
                             ;; the buffer here and the standard asks this question before storing
                             ;; it.  Counting it bumps one picture too many, which is invisible
                             ;; until a random access point discards what is left and the extra
                             ;; one has already escaped.
                             (> (length (dec-dpb d)) cap)))
              do (let ((next (first (sort (copy-list pending) #'< :key #'pic-poc))))
                   (setf (pic-output-done next) t)
                   (setf (dec-out d) (append (dec-out d) (list next)))
                   ;; a picture that has been shown and is no longer a reference leaves the buffer
                   ;; at once, which is what makes room and stops this loop
                   (unless (pic-needed next)
                     (setf (dec-dpb d) (remove next (dec-dpb d)))))))
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
    (multiple-value-bind (before after ltcurr) (%apply-rps d sh poc nal)
      ;; the current picture goes in only after the set has been applied, so it cannot keep itself
      (setf (dec-current d) pic
            (dec-before d) before
            (dec-after d) after
            (dec-ltcurr d) ltcurr))
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
       ;; THE LISTS ARE PER SLICE.  The reference picture SET is the picture's — it says what
       ;; survives in the buffer — but 8.3.4 runs for each slice, and two slices of one picture may
       ;; use different numbers of references, permute them differently, or name a different
       ;; collocated picture.  Building them once from the first slice's header gives every slice
       ;; after it somebody else's references.
       (%build-lists d sh (dec-before d) (dec-after d) (dec-ltcurr d))
       (setf (dec-ctx d)
             (decode-slice-data (sh-sps sh) (sh-pps sh) sh br (dec-current d)
                                (dec-list0 d) (dec-list1 d) (%collocated d sh)
                                (dec-ctx d)))
       (pop (dec-out d))))
    (t nil)))

(defun inherit-slice-header (base sh)
  "Fill in what a DEPENDENT slice segment did not send, from BASE, the last independent one.

   It sends its picture parameter set, its address, its entry points and its byte alignment, and
   nothing else: the slice type, the quantiser, the reference lists, the SAO flags and both
   deblocking offsets all belong to the last independent segment and are simply carried forward.
   Which is why the flag has to be honoured above the parser — the parser has no way of knowing
   what came before it, so anything that walks slice segments has to do this itself."
  (if (not (sh-dependent sh))
      sh
      (let ((merged (copy-slice
                     (or base (%err "a dependent slice segment with no independent one before it")))))
        (setf (sh-dependent merged) t
              (sh-first-in-pic merged) nil
              (sh-nal merged) (sh-nal sh)
              (sh-segment-address merged) (sh-segment-address sh)
              (sh-entry-points merged) (sh-entry-points sh))
        merged)))

(defun %inherit-slice-header (d sh)
  (inherit-slice-header (dec-indep-sh d) sh))

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

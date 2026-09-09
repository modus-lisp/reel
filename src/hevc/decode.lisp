;;;; hevc/decode.lisp — NAL units in, pictures out.
;;;;
;;;; The picture-level bookkeeping, which is where a decoder that gets every block right can still
;;;; produce the wrong video.  Three things happen once per picture and not once per slice: the
;;;; order count is derived, the reference picture set decides what survives in the buffer, and the
;;;; two loop filters run over the finished thing.  H.264 taught this the hard way — running the
;;;; per-picture epilogue per slice deblocked a three-slice picture three times over — so here it
;;;; is a separate function called from exactly one place.

(in-package #:reel.hevc)

(defun %finish-picture (d)
  "Filter the picture currently open, put it in the buffer, and hand out whatever is now due."
  (let ((pic (dec-current d))
        (sh (dec-cur-sh d)))
    (when (and pic sh)
      (deblock-picture pic (sh-pps sh) sh)
      (sao-picture pic sh)
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

(defun %start-picture (d sh nal)
  "Everything a new picture needs before its first macroblock: its order count, the buffer it may
   predict from, and the two reference lists."
  (let* ((sps (sh-sps sh))
         (poc (%picture-order-count d sh nal))
         (pic (make-picture-for sps)))
    (setf (pic-poc pic) poc
          (pic-reference-p pic) (nal-reference-p nal))
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
            (sh (parse-slice-header br nal (dec-sps d) (dec-pps d))))
       (when (sh-first-in-pic sh)
         (%finish-picture d)
         (%start-picture d sh nal))
       (unless (dec-current d) (%err "a slice arrived before any picture was started"))
       (setf (dec-cur-sh d) sh)
       (decode-slice-data (sh-sps sh) (sh-pps sh) sh br (dec-current d)
                          (dec-list0 d) (dec-list1 d) (%collocated d sh))
       (pop (dec-out d))))
    (t nil)))

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

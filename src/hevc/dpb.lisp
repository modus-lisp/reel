;;;; hevc/dpb.lisp — picture order counts, the reference picture set, and the reference lists.
;;;;
;;;; HEVC's reference management is not H.264's with new names.  H.264 tells a decoder what has
;;;; CHANGED — a sliding window, or explicit commands to retire one picture and keep another — so a
;;;; decoder that misses one message drifts from the encoder and never recovers.  HEVC has every
;;;; picture state the whole set it needs: the short-term reference picture set lists, by distance,
;;;; exactly which earlier pictures must still be there.  Anything not named is discarded.
;;;;
;;;; That costs bits and buys two things.  A decoder joining mid-stream knows immediately what it
;;;; is missing, and a lost picture is detectable rather than silently poisoning everything after
;;;; it.  It is the same argument as intra refresh, applied to the buffer instead of the picture.

(in-package #:reel.hevc)

(defstruct (decoder (:conc-name dec-))
  (sps (make-hash-table) :type hash-table)
  (pps (make-hash-table) :type hash-table)
  ;; every picture still needed, as references or because it has not been output yet
  (dpb '() :type list)
  (current nil)
  (cur-sh nil)
  (prev-poc-msb 0 :type fixnum) (prev-poc-lsb 0 :type fixnum)
  ;; the reference lists in force for the slice being decoded
  (list0 #() :type simple-vector) (list1 #() :type simple-vector)
  (out '() :type list)                  ; decoded and ready, in output order
  ;; the highest order count whose RANDOM ACCESS SKIPPED LEADING pictures cannot be decoded, and
  ;; whether the picture currently being read is one of them
  (max-ra most-positive-fixnum :type fixnum)
  (skipping nil)
  ;; the last INDEPENDENT slice segment header, which the dependent segments after it inherit, and
  ;; the decoding context they carry on from.  Both belong to the picture, not to the sequence.
  (indep-sh nil)
  (ctx nil)
  ;; the reference picture set this picture may predict from, kept because the LISTS built out of
  ;; it are per SLICE and not per picture: two slices of one picture may name different numbers of
  ;; references and permute them differently
  (before #() :type simple-vector)
  (after #() :type simple-vector)
  (ltcurr #() :type simple-vector)
  (frames 0 :type fixnum))

(defun make-hevc-decoder () (make-decoder))

(defun %picture-order-count (d sh nal)
  "PicOrderCntVal (8.3.1): the low bits are sent and the high bits are inferred.

   The inference is the same wrap detection H.264 uses, and the reason both need it is the same:
   the count has to keep increasing across a whole sequence but only a few of its bits are worth
   sending per picture.  A random access point resets the predictors, which is what makes it a
   place a decoder may start."
  (let* ((sps (sh-sps sh))
         (max-lsb (ash 1 (sps-log2-max-poc-lsb sps)))
         (half (ash max-lsb -1))
         (lsb (if (nal-idr-p nal) 0 (sh-poc-lsb sh)))
         (irap (or (nal-idr-p nal) (nal-bla-p nal)))
         (prev-msb (dec-prev-poc-msb d))
         (prev-lsb (dec-prev-poc-lsb d))
         ;; AT A RANDOM ACCESS POINT THAT RESETS, THE HIGH BITS ARE ZERO — not "derived from zeroed
         ;; predictors", which is a different thing and sometimes a negative number.  The wrap
         ;; detection asks whether the low bits went backwards by more than half their range and
         ;; concludes the high bits must have moved; against a predictor of zero, any count past
         ;; the halfway mark looks like it wrapped DOWNWARDS and lands a whole cycle below zero.
         ;; A picture at 220 of 256 comes out at -36, and every picture after it inherits that.
         (msb (if irap
                  0
                  (cond ((and (< lsb prev-lsb) (>= (- prev-lsb lsb) half)) (+ prev-msb max-lsb))
                        ((and (> lsb prev-lsb) (> (- lsb prev-lsb) half)) (- prev-msb max-lsb))
                        (t prev-msb)))))
    ;; Only a picture at the base temporal sub-layer that is neither a leading picture nor a
    ;; SUB-LAYER NON-REFERENCE one updates the predictors.  Both exclusions have the same
    ;; motivation: a decoder that discards pictures the stream says are discardable must still
    ;; derive the same order counts as one that keeps them, so nothing discardable may be a
    ;; predictor.  Leaving the sub-layer non-reference types in gets the count right until the
    ;; first stream that has one, and then puts one picture in the wrong place on screen while
    ;; every picture still decodes bit-exactly — which looks like a reconstruction bug and is not.
    (when (and (zerop (nal-temporal-id nal))
               (not (member (nal-type nal) (list +nal-trail-n+ +nal-tsa-n+ +nal-stsa-n+
                                                 +nal-radl-n+ +nal-radl-r+
                                                 +nal-rasl-n+ +nal-rasl-r+))))
      (setf (dec-prev-poc-msb d) msb (dec-prev-poc-lsb d) lsb))
    (+ msb lsb)))

(defun %apply-rps (d sh poc nal)
  "Decide what stays in the buffer, and sort what stays into the two current sets (8.3.2).

   Returns (values before after), the pictures earlier and later than this one in output order that
   this picture may predict from, each nearest first.  Everything the set does not name is dropped
   — which is the whole design: the encoder states the buffer rather than editing it."
  (let ((strps (sh-strps sh))
        (ltcurr '()))
    (when (or (nal-idr-p nal) (nal-bla-p nal))
      ;; A random access point that resets: nothing before it may be PREDICTED FROM again.  That is
      ;; not the same as nothing before it surviving — a picture still waiting to be displayed
      ;; normally has to stay until it has been, and discarding it loses frames from the middle of
      ;; the output rather than producing a wrong one, which is harder to notice.
      ;;
      ;; UNLESS THE PICTURE SAYS OTHERWISE.  no_output_of_prior_pics_flag is how a stream says the
      ;; pictures still waiting are not to be shown at all — which is what a splice wants: the
      ;; material before the join was decoded to get here and is not part of what anyone asked to
      ;; watch.  Ignoring it emits frames the stream asked to be thrown away.
      (setf (dec-dpb d)
            (if (sh-no-output-prior sh)
                '()
                (remove-if #'pic-output-done (dec-dpb d))))
      (dolist (p (dec-dpb d)) (setf (pic-needed p) nil))
      (return-from %apply-rps (values #() #() #())))
    (unless (or strps (plusp (length (sh-long-term sh))))
      (return-from %apply-rps (values #() #() #())))
    (let ((before '()) (after '()) (keep '()))
      (flet ((find-poc (p)
               (find p (dec-dpb d) :key #'pic-poc :test #'=)))
        ;; the long-term entries first, because they are named by ORDER COUNT rather than by
        ;; distance and so have to be matched against the buffer differently: with the high bits
        ;; sent, against the whole count; without them, against its low bits alone, which is what
        ;; lets an encoder name a picture from long ago in a handful of bits
        (let* ((sps (sh-sps sh))
               (max-lsb (ash 1 (sps-log2-max-poc-lsb sps)))
               (mask (1- max-lsb)))
          ;; long-term is a marking, not a property, and it is restated by every picture: one that
          ;; this picture does not name is no longer long-term even if the last one said it was
          (dolist (p (dec-dpb d)) (setf (pic-long-term p) nil))
          (loop for e across (sh-long-term sh)
                do (destructuring-bind (lsb used msb-present cycle) e
                     (let ((pic (if msb-present
                                    (find-poc (+ (- poc (* cycle max-lsb) (logand poc mask)) lsb))
                                    (find lsb (dec-dpb d)
                                          :key (lambda (p) (logand (pic-poc p) mask))
                                          :test #'=))))
                       (when pic
                         (setf (pic-long-term pic) t)
                         (push pic keep)
                         (when used (push pic ltcurr)))))))
        (loop for i from 0 below (length (if strps (strps-negative strps) #()))
              do (let* ((want (- poc (aref (strps-negative strps) i)))
                        (pic (find-poc want)))
                   (when pic
                     (push pic keep)
                     (when (aref (strps-used-neg strps) i) (push pic before)))))
        (loop for i from 0 below (length (if strps (strps-positive strps) #()))
              do (let* ((want (+ poc (aref (strps-positive strps) i)))
                        (pic (find-poc want)))
                   (when pic
                     (push pic keep)
                     (when (aref (strps-used-pos strps) i) (push pic after)))))
        ;; the lists were built by walking the set, which is already sorted by distance.
        ;; A picture the set does not name is no longer a reference; it is only DISCARDED once it
        ;; has also been output.
        (dolist (p (dec-dpb d)) (setf (pic-needed p) (and (member p keep) t)))
        (setf (dec-dpb d)
              (remove-if-not (lambda (p) (or (pic-needed p) (not (pic-output-done p))))
                             (dec-dpb d)))
        (values (coerce (nreverse before) 'simple-vector)
                (coerce (nreverse after) 'simple-vector)
                (coerce (nreverse ltcurr) 'simple-vector))))))

(defun %build-lists (d sh before after ltcurr)
  "RefPicList0 and RefPicList1 (8.3.4).

   List 0 counts BACKWARDS from this picture and then forwards; list 1 the other way round.  So
   index 0 of each is the nearest picture in that direction, and a bi-predicted block that wants
   the two obvious neighbours spends the cheapest index in both lists.  The list is filled by
   repeating the whole set until it is long enough, which is why an index may legitimately name the
   same picture twice."
  (let* ((n0 (sh-num-ref-idx-l0 sh))
         (n1 (sh-num-ref-idx-l1 sh))
         (total (+ (length before) (length after) (length ltcurr))))
    (when (zerop total)
      (setf (dec-list0 d) #() (dec-list1 d) #())
      (return-from %build-lists nil))
    (labels ((nth-of (k parts)
               ;; the k-th picture of the segments in order, which is what the standard builds its
               ;; temporary list out of before any permutation is applied
               (loop for part in parts
                     do (when (< k (length part)) (return (aref part k)))
                        (decf k (length part)))))
      (flet ((fill-list (n parts entries)
               ;; ENTRIES, when the slice sent one, says which of the implied list each index
               ;; takes.  Without it index i takes the i-th, the same thing said the short way.
               (let ((v (make-array n)))
                 (dotimes (i n v)
                   (setf (aref v i)
                         (nth-of (mod (if entries (aref entries i) i) total) parts))))))
        ;; LONG-TERM ENTRIES COME LAST IN BOTH LISTS, after both short-term halves.  They are the
        ;; most expensive index to reach, which is right: a long-term reference is for the block
        ;; that needs it, not for the picture as a whole.
        (setf (dec-list0 d) (fill-list n0 (list before after ltcurr) (sh-list-entry-l0 sh)))
        (setf (dec-list1 d) (if (sh-b-slice-p sh)
                                (fill-list n1 (list after before ltcurr) (sh-list-entry-l1 sh))
                                #()))))))

(defun %output-order (pictures)
  (sort (copy-list pictures) #'< :key #'pic-poc))

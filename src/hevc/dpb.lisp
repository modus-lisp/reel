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
         (prev-msb (if irap 0 (dec-prev-poc-msb d)))
         (prev-lsb (if irap 0 (dec-prev-poc-lsb d)))
         (msb (cond ((and (< lsb prev-lsb) (>= (- prev-lsb lsb) half)) (+ prev-msb max-lsb))
                    ((and (> lsb prev-lsb) (> (- lsb prev-lsb) half)) (- prev-msb max-lsb))
                    (t prev-msb))))
    ;; only a picture at the base temporal sub-layer that is not a leading picture updates the
    ;; predictors, because a decoder that drops the upper sub-layers must still agree about them
    (when (and (zerop (nal-temporal-id nal))
               (not (member (nal-type nal) (list +nal-radl-n+ +nal-radl-r+
                                                 +nal-rasl-n+ +nal-rasl-r+))))
      (setf (dec-prev-poc-msb d) msb (dec-prev-poc-lsb d) lsb))
    (+ msb lsb)))

(defun %apply-rps (d sh poc nal)
  "Decide what stays in the buffer, and sort what stays into the two current sets (8.3.2).

   Returns (values before after), the pictures earlier and later than this one in output order that
   this picture may predict from, each nearest first.  Everything the set does not name is dropped
   — which is the whole design: the encoder states the buffer rather than editing it."
  (let ((strps (sh-strps sh)))
    (when (or (nal-idr-p nal) (nal-bla-p nal))
      ;; a random access point that resets: nothing before it survives
      (setf (dec-dpb d) '())
      (return-from %apply-rps (values #() #())))
    (unless strps (return-from %apply-rps (values #() #())))
    (let ((before '()) (after '()) (keep '()))
      (flet ((find-poc (p)
               (find p (dec-dpb d) :key #'pic-poc :test #'=)))
        (loop for i from 0 below (length (strps-negative strps))
              do (let* ((want (- poc (aref (strps-negative strps) i)))
                        (pic (find-poc want)))
                   (when pic
                     (push pic keep)
                     (when (aref (strps-used-neg strps) i) (push pic before)))))
        (loop for i from 0 below (length (strps-positive strps))
              do (let* ((want (+ poc (aref (strps-positive strps) i)))
                        (pic (find-poc want)))
                   (when pic
                     (push pic keep)
                     (when (aref (strps-used-pos strps) i) (push pic after)))))
        ;; the lists were built by walking the set, which is already sorted by distance
        (setf (dec-dpb d) (remove-if-not (lambda (p) (member p keep)) (dec-dpb d)))
        (values (coerce (nreverse before) 'simple-vector)
                (coerce (nreverse after) 'simple-vector))))))

(defun %build-lists (d sh before after)
  "RefPicList0 and RefPicList1 (8.3.4).

   List 0 counts BACKWARDS from this picture and then forwards; list 1 the other way round.  So
   index 0 of each is the nearest picture in that direction, and a bi-predicted block that wants
   the two obvious neighbours spends the cheapest index in both lists.  The list is filled by
   repeating the whole set until it is long enough, which is why an index may legitimately name the
   same picture twice."
  (let* ((n0 (sh-num-ref-idx-l0 sh))
         (n1 (sh-num-ref-idx-l1 sh))
         (total (+ (length before) (length after))))
    (when (zerop total)
      (setf (dec-list0 d) #() (dec-list1 d) #())
      (return-from %build-lists nil))
    (flet ((fill-list (n first second)
             (let ((v (make-array n)))
               (dotimes (i n v)
                 (let ((k (mod i total)))
                   (setf (aref v i)
                         (if (< k (length first))
                             (aref first k)
                             (aref second (- k (length first))))))))))
      (setf (dec-list0 d) (fill-list n0 before after))
      (setf (dec-list1 d) (if (sh-b-slice-p sh) (fill-list n1 after before) #())))))

(defun %output-order (pictures)
  (sort (copy-list pictures) #'< :key #'pic-poc))

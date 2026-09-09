;;;; h264/decode.lisp — the decoder: NAL units in, pictures out.
;;;;
;;;; What this handles today is INTRA ONLY: an all-I-frame stream, Constrained Baseline, CAVLC,
;;;; 4:2:0, 8-bit, progressive.  That is a real class of file — anything encoded with `-g 1`, a
;;;; screen recorder's keyframe-only output, the first frame of everything — and it is the whole
;;;; foundation for inter frames, which need everything here plus motion compensation and a
;;;; reference list.  What is refused is refused loudly, in params.lisp, rather than decoded wrong.
;;;;
;;;; The deblocking filter runs (deblock.lisp), so what comes out is what a conforming decoder
;;;; outputs, not an approximation of it.

(in-package #:reel.h264)

(defstruct (decoder (:conc-name h264-) (:constructor %make-decoder))
  (sps (make-hash-table) :type hash-table)
  (pps (make-hash-table) :type hash-table)
  (picture nil)
  ;; The slice header of the picture currently being assembled.  A picture may be several slices,
  ;; and everything that happens once the picture is COMPLETE — the loop filter, reference marking,
  ;; reordering — needs one of its headers to work from; any of them will do for the parts that are
  ;; picture-wide.  See %END-PICTURE.
  (cur-sh nil)
  ;; Decoded pictures kept as references, most recent first.  Baseline marking is a sliding
  ;; window: a new reference picture goes on the front and the oldest falls off the back once
  ;; there are more than the sequence parameter set allows.  An IDR empties it.
  (refs '() :type list)
  ;; Decoded but not yet handed out, newest first.  B pictures are decoded AFTER the picture they
  ;; are displayed before, so a decoder that hands each picture over as it finishes shows them in
  ;; the wrong order.  See %BUMP.
  (pending '() :type list)
  (out '() :type list)                  ; bumped and ready, oldest first
  (reorder 0 :type fixnum)
  ;; the running predictors the order count is derived from
  (prev-poc-msb 0 :type fixnum) (prev-poc-lsb 0 :type fixnum)
  (prev-frame-num 0 :type fixnum) (prev-frame-num-offset 0 :type fixnum)
  (frames 0 :type fixnum))

(defun make-decoder () (%make-decoder))

(defun decoder-width (d) (let ((p (h264-picture d))) (and p (pic-width p))))
(defun decoder-height (d) (let ((p (h264-picture d))) (and p (pic-height p))))

(defun %picture-order-count (d sh nal)
  "PicOrderCnt for this picture (8.2.1): where it belongs on screen.

   Only a REFERENCE picture updates the predictors, because a non-reference picture is not in the
   chain the next one measures itself against."
  (let ((sps (sh-sps sh)))
    (case (sps-poc-type sps)
      (0
       (let* ((max-lsb (ash 1 (sps-log2-max-poc-lsb sps)))
              (half (ash max-lsb -1))
              (lsb (sh-poc-lsb sh))
              (prev-msb (if (nal-idr-p nal) 0 (h264-prev-poc-msb d)))
              (prev-lsb (if (nal-idr-p nal) 0 (h264-prev-poc-lsb d)))
              (msb (cond ((and (< lsb prev-lsb) (>= (- prev-lsb lsb) half)) (+ prev-msb max-lsb))
                         ((and (> lsb prev-lsb) (> (- lsb prev-lsb) half)) (- prev-msb max-lsb))
                         (t prev-msb))))
         (when (plusp (nal-ref-idc nal))
           (setf (h264-prev-poc-msb d) msb (h264-prev-poc-lsb d) lsb))
         (+ msb lsb)))
      (1
       ;; 8.2.1.2.  The sequence describes a repeating CYCLE of order-count steps once, in the
       ;; parameter set, and each picture's count is where frame_num lands in that cycle plus a
       ;; correction it carries itself.  It is how a stream states a fixed display pattern — two
       ;; B pictures between every pair of references, say — without spending bits restating it.
       ;;
       ;; frame_num counts CODED pictures and wraps, so it cannot be used directly; FrameNumOffset
       ;; accumulates the wraps.  A non-reference picture steps back one, because it is not in the
       ;; cycle the following pictures measure themselves against.
       (let* ((max-fn (ash 1 (sps-log2-max-frame-num sps)))
              (fn (sh-frame-num sh))
              (cycle (sps-poc-cycle sps))
              (n (length cycle))
              (offset (cond ((nal-idr-p nal) 0)
                            ((> (h264-prev-frame-num d) fn) (+ (h264-prev-frame-num-offset d) max-fn))
                            (t (h264-prev-frame-num-offset d))))
              (abs-fn (if (plusp n) (+ offset fn) 0))
              (expected 0))
         ;; the predictors follow the previous picture in DECODING order, reference or not
         (setf (h264-prev-frame-num-offset d) offset (h264-prev-frame-num d) fn)
         (when (and (zerop (nal-ref-idc nal)) (plusp abs-fn)) (decf abs-fn))
         (when (plusp abs-fn)
           (setf expected (* (floor (1- abs-fn) n) (reduce #'+ cycle :initial-value 0)))
           (loop for step in cycle
                 repeat (1+ (mod (1- abs-fn) n))
                 do (incf expected step)))
         (when (zerop (nal-ref-idc nal))
           (incf expected (sps-offset-for-non-ref-pic sps)))
         ;; frame_mbs_only is asserted, so this is a frame and its count is the earlier of its two
         ;; notional fields
         (let* ((top (+ expected (sh-delta-poc-0 sh)))
                (bottom (+ top (sps-offset-for-top-to-bottom sps) (sh-delta-poc-1 sh))))
           (min top bottom))))
      (2
       ;; type 2 asserts that decoding order IS display order, so there is nothing to reorder
       (let* ((max-fn (ash 1 (sps-log2-max-frame-num sps)))
              (fn (sh-frame-num sh))
              (offset (cond ((nal-idr-p nal) 0)
                            ((> (h264-prev-frame-num d) fn) (+ (h264-prev-frame-num-offset d) max-fn))
                            (t (h264-prev-frame-num-offset d)))))
         (setf (h264-prev-frame-num-offset d) offset (h264-prev-frame-num d) fn)
         (cond ((nal-idr-p nal) 0)
               ((zerop (nal-ref-idc nal)) (1- (* 2 (+ offset fn))))
               (t (* 2 (+ offset fn))))))
      (t (%err "pic_order_cnt_type ~d is not supported" (sps-poc-type sps))))))

;;; ---- how far display order may run behind decoding order --------------------------------------
;;;
;;; THIS IS A PROPERTY OF THE SEQUENCE, NOT OF THE SLICE IN HAND.  Deriving it from whether the
;;; current picture is a B slice looks reasonable and is wrong in a specific, quiet way: in a stream
;;; coded I P B P B, the depth drops to zero on every P, the P is handed out immediately, and it
;;; arrives before the B that precedes it on screen.  Every picture is decoded perfectly and the
;;; order is wrong in pairs — which is what three of the JVT conformance streams turned out to be.
;;;
;;; When the sequence states max_num_reorder_frames in its VUI, that is the answer.  When it does
;;; not — and most streams do not — the standard's default is the full decoded picture buffer,
;;; which is MaxDpbMbs for the level divided by the picture area, capped at sixteen.  A file decode
;;; pays for that in latency and nothing else, and both an IDR and the end of the stream drain it.

(defparameter +max-dpb-mbs+
  '((10 . 396) (11 . 900) (12 . 2376) (13 . 2376) (20 . 2376) (21 . 4752) (22 . 8100)
    (30 . 8100) (31 . 18000) (32 . 20480) (40 . 32768) (41 . 32768) (42 . 34816)
    (50 . 110400) (51 . 184320) (52 . 184320) (60 . 696320) (61 . 696320) (62 . 696320))
  "Table A-1: the decoded picture buffer size each level allows, in macroblocks.")

(defun %reorder-depth (sps)
  (or (sps-num-reorder-frames sps)
      (let* ((mbs (* (sps-mb-width sps) (sps-mb-height sps)))
             (level (sps-level sps))
             (dpb (or (cdr (assoc level +max-dpb-mbs+))
                      ;; an unlisted level: take the next one up rather than under-buffer
                      (cdr (find-if (lambda (e) (>= (car e) level)) +max-dpb-mbs+))
                      696320)))
        (max 0 (min 16 (floor dpb (max 1 mbs)))))))

(defun %bump (d &optional flush)
  "Hand out the pending picture with the smallest order count, if it is time.

   Time is when more pictures are held than the sequence says display order can ever run behind
   decoding order by.  With that many in hand, the smallest order count present cannot be beaten
   by anything still to arrive, so it is safe to show.  FLUSH empties the buffer at the end of a
   stream or before an IDR resets the counts."
  (let ((n (length (h264-pending d))))
    (when (or (and flush (plusp n)) (> n (h264-reorder d)))
      (let ((best (first (h264-pending d))))
        (dolist (p (h264-pending d))
          (when (< (pic-poc p) (pic-poc best)) (setf best p)))
        (setf (h264-pending d) (remove best (h264-pending d)))
        best))))

(defun %drain (d &optional flush)
  "Move whatever is ready from the reorder buffer onto the output queue."
  (loop for p = (%bump d flush) while p do (setf (h264-out d) (append (h264-out d) (list p)))))

(declaim (ftype function %mark-references))

(defun %end-picture (d)
  "Finish the picture currently open, if there is one.

   EVERYTHING HERE IS ONCE PER PICTURE, NOT ONCE PER SLICE, and the difference is invisible until
   a stream splits a picture into several slices.  Running the loop filter after each slice filters
   the whole picture again every time: in a three-slice picture the first slice's macroblocks come
   out filtered three times over, which is a small error everywhere rather than an obvious one
   somewhere, and it survives every single-slice conformance stream there is."
  (let ((pic (h264-picture d))
        (sh (h264-cur-sh d)))
    (when (and pic sh)
      (deblock-picture pic sh)
      (incf (h264-frames d))
      ;; the filtered picture is what later pictures predict from, so this happens after the
      ;; loop filter and not before it
      (when (nal-idr-p (sh-nal sh)) (setf (h264-refs d) '()))
      (when (plusp (nal-ref-idc (sh-nal sh)))
        (%mark-references d sh (sh-sps sh)))
      (push pic (h264-pending d))
      (%drain d)
      (setf (h264-picture d) nil (h264-cur-sh d) nil)
      pic)))

(defun flush-decoder (d)
  "Every picture still held, in display order.  Call at the end of a stream: without it the last
   few pictures of every file stay in the reorder buffer, which is a real bug and an easy one to
   not notice, because it only ever loses the ending."
  (%end-picture d)
  (%drain d t)
  (prog1 (h264-out d) (setf (h264-out d) '())))

(defun pending-pictures (d)
  "How many decoded pictures are waiting to be handed out."
  (+ (length (h264-out d)) (length (h264-pending d))))

(defun %pic-num-entries (d sh)
  "Every short-term reference as (PicNum . picture).  PicNum is frame_num made monotonic: it wraps,
   and a reference coded before the wrap has to compare as older than one coded after it."
  (let ((max-pic-num (ash 1 (sps-log2-max-frame-num (sh-sps sh))))
        (curr (sh-frame-num sh)))
    (mapcar (lambda (p)
              (let ((fn (pic-frame-num p)))
                (cons (if (> fn curr) (- fn max-pic-num) fn) p)))
            (h264-refs d))))

(defun %apply-list-modification (init ops entries curr max-pic-num n-active)
  "The reordering of 8.2.4.3.1, shared by both lists and both slice types.

   THE RESULT CAN BE LONGER THAN THE NUMBER OF PICTURES IN IT.  Each operation inserts at a
   position and shifts the rest along, then drops only the copy that comes AFTER the insertion
   point — so an earlier copy survives and the same picture legitimately appears at two indices.
   That is how weighted prediction gets two weights from one reference, and treating the list as a
   set decodes most streams and then fails on ordinary ones.

   The running predictor and the current picture's number are two different things: the predictor
   moves with each operation, the current number does not."
  (if (null ops)
      (coerce (subseq init 0 (min n-active (length init))) 'vector)
      (let ((work (make-array (1+ n-active) :initial-element nil))
            (refidx 0)
            (pred curr))
        (loop for p in init for i from 0 while (<= i n-active) do (setf (aref work i) p))
        (dolist (op ops)
          (let ((kind (car op)) (delta (1+ (cdr op))))
            (unless (or (= kind 0) (= kind 1))
              (%err "long-term reference pictures are not supported"))
            (let* ((nowrap (if (zerop kind)
                               (let ((x (- pred delta))) (if (< x 0) (+ x max-pic-num) x))
                               (let ((x (+ pred delta))) (if (>= x max-pic-num) (- x max-pic-num) x))))
                   (picnum (if (> nowrap curr) (- nowrap max-pic-num) nowrap))
                   (hit (cdr (assoc picnum entries :test #'=))))
              (setf pred nowrap)
              (unless hit
                (%err "the reference list names PicNum ~d, which is not in the buffer" picnum))
              (loop for c of-type fixnum from n-active downto (1+ refidx)
                    do (setf (aref work c) (aref work (1- c))))
              (setf (aref work refidx) hit)
              (incf refidx)
              (let ((n refidx))
                (loop for c of-type fixnum from refidx to n-active
                      do (let ((e (aref work c)))
                           (unless (eq e hit) (setf (aref work n) e) (incf n))))
                (loop for c of-type fixnum from n to n-active do (setf (aref work c) nil))))))
        (coerce (remove nil (subseq work 0 n-active)) 'vector))))

(defun build-ref-list-0 (d sh)
  "List 0 for a P slice: the short-term references by DESCENDING PicNum, most recently coded first,
   then whatever the slice reorders."
  (let* ((entries (%pic-num-entries d sh))
         (init (mapcar #'cdr (sort (copy-list entries) #'> :key #'car))))
    (%apply-list-modification init (sh-ref-list-reordering sh) entries
                              (sh-frame-num sh) (ash 1 (sps-log2-max-frame-num (sh-sps sh)))
                              (max 1 (or (sh-num-ref-idx-l0 sh) 1)))))

(defun build-ref-lists-b (d sh)
  "(values list0 list1) for a B slice (8.2.4.2.3).

   A B slice orders its references by WHERE THEY SIT ON SCREEN, not by when they were coded, which
   is the whole difference from a P slice.  List 0 counts backwards from the current picture and
   then forwards; list 1 counts forwards and then backwards.  So index 0 of each list is the
   nearest picture in that direction, which is what makes the cheapest index the most useful one.

   When the two lists come out identical and there is more than one entry, list 1's first two are
   swapped — otherwise both lists would name the same picture and bi-prediction would average a
   picture with itself."
  (let* ((entries (%pic-num-entries d sh))
         (curr (pic-poc (h264-picture d)))
         (refs (h264-refs d))
         (before (sort (remove-if-not (lambda (p) (< (pic-poc p) curr)) (copy-list refs))
                       #'> :key #'pic-poc))
         (after (sort (remove-if-not (lambda (p) (>= (pic-poc p) curr)) (copy-list refs))
                      #'< :key #'pic-poc))
         (init0 (append before after))
         (init1 (append after before)))
    (when (and (> (length init1) 1) (equal init0 init1))
      (setf init1 (list* (second init1) (first init1) (cddr init1))))
    (let ((max-pic-num (ash 1 (sps-log2-max-frame-num (sh-sps sh))))
          (fn (sh-frame-num sh)))
      (values (%apply-list-modification init0 (sh-ref-list-reordering sh) entries fn max-pic-num
                                        (max 1 (or (sh-num-ref-idx-l0 sh) 1)))
              (%apply-list-modification init1 (sh-ref-list-reordering-l1 sh) entries fn max-pic-num
                                        (max 1 (or (sh-num-ref-idx-l1 sh) 1)))))))

(defun %mark-references (d sh sps)
  "Add the picture just decoded to the reference set, and decide what leaves (8.2.5).

   TWO MARKING PROCESSES, and a stream picks one per picture.  The sliding window simply keeps the
   most recent max_num_ref_frames pictures, which is what every simple stream does.  Adaptive
   marking instead names the picture to retire, and it is not an exotic feature: an encoder using a
   B PYRAMID — B pictures that are themselves references — retires each one this way as soon as the
   pictures depending on it are decoded.  x264 does this by default.

   Ignoring the operations does not desynchronise anything, which is what makes it so easy to miss.
   The reference set simply drifts: a picture the encoder dropped stays, the window evicts a
   different one, and the lists agree with the encoder's for the first few pictures and then quietly
   do not.  What that looks like is a handful of macroblocks wrong in the later pictures of a
   sequence — the ones that happened to name a reference index the two sides disagree about."
  (let ((limit (max 1 (sps-max-ref-frames sps))))
    (cond
      ((sh-adaptive-ref-marking sh)
       (let ((entries (%pic-num-entries d sh))
             (curr (sh-frame-num sh)))
         (dolist (op (sh-mmco sh))
           (when (= 1 (car op))
             ;; the picture is named by how far back it is, not by where it sits in the buffer
             (let* ((picnum (- curr (1+ (cdr op))))
                    (hit (cdr (assoc picnum entries :test #'=))))
               (unless hit
                 (%err "reference marking retires PicNum ~d, which is not in the buffer" picnum))
               (setf (h264-refs d) (remove hit (h264-refs d))))))
         (push (h264-picture d) (h264-refs d))
         (when (> (length (h264-refs d)) limit)
           (%err "~d reference pictures after adaptive marking, but only ~d are allowed"
                 (length (h264-refs d)) limit))))
      (t
       (push (h264-picture d) (h264-refs d))
       (when (> (length (h264-refs d)) limit)
         (setf (h264-refs d) (subseq (h264-refs d) 0 limit)))))))

(defun feed-nal (d nal)
  "Give one NAL unit to the decoder.  Returns a PICTURE when this NAL completed one, else NIL."
  (cond
    ((= (nal-type nal) +nal-sps+)
     (let ((s (parse-sps (nal-rbsp nal))))
       (setf (gethash (sps-id s) (h264-sps d)) s))
     nil)
    ((= (nal-type nal) +nal-pps+)
     (let ((p (parse-pps (nal-rbsp nal))))
       (setf (gethash (pps-id p) (h264-pps d)) p))
     nil)
    ((nal-slice-p nal)
     (let* ((br (make-bitreader (nal-rbsp nal)))
            (sh (parse-slice-header br nal (h264-sps d) (h264-pps d)))
            (sps (sh-sps sh)))
       ;; a new picture starts at first_mb_in_slice 0; with one slice per picture that is every
       ;; slice, but the test is the right one for a stream that splits pictures into several
       (when (zerop (sh-first-mb sh))
         ;; a new picture starting means the previous one is complete
         (%end-picture d)
         ;; an IDR restarts the order counts from zero, so everything already decoded has to be
         ;; handed out before it, or the two sequences interleave by number and come out shuffled
         (when (nal-idr-p nal) (%drain d t))
         (setf (h264-picture d) (make-picture-for sps))
         (setf (pic-poc (h264-picture d)) (%picture-order-count d sh nal)
               (pic-frame-num (h264-picture d)) (sh-frame-num sh)
               (pic-ref-p (h264-picture d)) (plusp (nal-ref-idc nal)))
         (setf (h264-reorder d) (%reorder-depth sps)))
       (unless (h264-picture d) (%err "a slice arrived before any picture was started"))
       (setf (h264-cur-sh d) sh)
       (if (sh-b-slice-p sh)
           (multiple-value-bind (l0 l1) (build-ref-lists-b d sh)
             (decode-slice (h264-picture d) sh br l0 l1))
           (decode-slice (h264-picture d) sh br (build-ref-list-0 d sh)))
       ;; The loop filter, reference marking and reordering all wait for the picture to END —
       ;; %END-PICTURE, run by the next picture's first slice or by FLUSH-DECODER.  So what comes
       ;; out here is whatever an EARLIER picture's turn made ready, which is what the reorder
       ;; buffer was always handing back anyway.
       (pop (h264-out d))))
    (t nil)))                                   ; SEI, AUD, and everything else: not our business

(defun decode-picture (d nals)
  "Feed a picture's worth of NAL units and return the PICTURE, or NIL if none completed.

   The deblocking filter runs, so this matches a conforming decoder's output and not merely its
   unfiltered reconstruction."
  (let ((out nil))
    (dolist (n nals out)
      (let ((p (feed-nal d n)))
        (when p (setf out p))))))

(defun as-picture (pic)
  "This decoder's picture as a REEL.DECODE:PICTURE, so a caller has one picture type whichever
   codec produced it.  The planes are SHARED, not copied — same lifetime rules as VP8's."
  (reel.decode::%make-shared-picture
   :width (pic-width pic) :height (pic-height pic)
   :y (pic-y pic) :u (pic-u pic) :v (pic-v pic)
   :y-stride (pic-ystride pic) :uv-stride (pic-cstride pic)
   :y-offset (pic-yoff pic) :uv-offset (pic-coff pic)))

(defun decode-annex-b (bytes &key (threads (default-decode-threads)))
  "Decode every picture in an Annex B byte stream.  Returns a list of PICTUREs.

   When every picture in the stream is independently decodable — which for now means every slice is
   an I slice — they are decoded CONCURRENTLY, because such pictures have nothing to say to each
   other.  A stream that is not gets the ordinary serial walk.  Pass :THREADS 1 for the serial path
   whatever the stream is; the two produce identical pictures, which is what the conformance test
   checks."
  (let* ((nals (annex-b-nals bytes))
         (d (make-decoder)))
    (multiple-value-bind (params aus) (split-access-units nals)
      (or (and (> threads 1)
               (parameter-sets-stable-p params)
               ;; the parameter sets first and serially: every worker reads them, none writes them
               (progn (dolist (n params) (feed-nal d n))
                      (decode-independent d aus :threads threads)))
          ;; THE SERIAL PATH WALKS THE NALS AS THEY CAME, parameter sets included and in place.
          ;; It must not use the split above: that hoists every parameter set to the front, and a
          ;; stream that re-sends one with different contents would then be decoded entirely under
          ;; the last version of it.
          (let ((d (make-decoder))
                (out (list)))
            (dolist (n nals)
              (let ((p (feed-nal d n))) (when p (push p out))))
            ;; and the tail the reorder buffer is still holding
            (dolist (p (flush-decoder d)) (push p out))
            (nreverse out))))))

;;; ---- getting the samples out --------------------------------------------------------------------

(defun picture->yuv420 (pic)
  "The visible picture as tightly packed I420 octets.

   The crop the SPS declares is applied here and nowhere else: the planes are macroblock-aligned
   throughout decoding, because prediction and the loop filter both need the padding, and the
   caller wants the picture the file says it is."
  (let* ((w (pic-width pic)) (h (pic-height pic))
         (cw (ceiling w 2)) (ch (ceiling h 2))
         (out (make-array (+ (* w h) (* 2 cw ch)) :element-type '(unsigned-byte 8)))
         (o 0))
    (dotimes (y h)
      (replace out (pic-y pic) :start1 o
                               :start2 (+ (pic-yoff pic) (* y (pic-ystride pic)))
                               :end2 (+ (pic-yoff pic) (* y (pic-ystride pic)) w))
      (incf o w))
    (dolist (plane (list (pic-u pic) (pic-v pic)))
      (dotimes (y ch)
        (replace out plane :start1 o
                           :start2 (+ (pic-coff pic) (* y (pic-cstride pic)))
                           :end2 (+ (pic-coff pic) (* y (pic-cstride pic)) cw))
        (incf o cw)))
    out))

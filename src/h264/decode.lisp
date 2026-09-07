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
  ;; Decoded pictures kept as references, most recent first.  Baseline marking is a sliding
  ;; window: a new reference picture goes on the front and the oldest falls off the back once
  ;; there are more than the sequence parameter set allows.  An IDR empties it.
  (refs '() :type list)
  (frames 0 :type fixnum))

(defun make-decoder () (%make-decoder))

(defun decoder-width (d) (let ((p (h264-picture d))) (and p (pic-width p))))
(defun decoder-height (d) (let ((p (h264-picture d))) (and p (pic-height p))))

(defun build-ref-list-0 (d sh)
  "List 0 for a P slice (8.2.4.2.1 and 8.2.4.3.1), as a vector of PICTUREs.

   The list starts as the short-term references ordered by DESCENDING PicNum — most recently coded
   first — and the slice may then move particular pictures around inside it.

   THE LIST CAN BE LONGER THAN THE NUMBER OF PICTURES IN IT.  The reordering inserts at a position
   and shifts the rest along, dropping only the copy that comes AFTER the insertion point, so an
   earlier copy survives and the same picture legitimately appears twice.  That is not a curiosity:
   it is how weighted prediction gets two different weights out of one reference picture, and it is
   what an ordinary x264 file does. A decoder that treats the list as a set decodes most streams and
   then fails on ones with weighting turned on.

   PicNum is frame_num made monotonic: frame_num wraps, and a reference coded before the wrap has
   to compare as older than one coded after it. Note also that the running predictor and the
   current picture's number are two different things — the predictor moves with each operation,
   the current number does not."
  (let* ((sps (sh-sps sh))
         (max-pic-num (ash 1 (sps-log2-max-frame-num sps)))
         (curr (sh-frame-num sh))
         (n-active (max 1 (or (sh-num-ref-idx-l0 sh) 1)))
         (entries (mapcar (lambda (p)
                            (let ((fn (pic-frame-num p)))
                              (cons (if (> fn curr) (- fn max-pic-num) fn) p)))
                          (h264-refs d)))
         (init (mapcar #'cdr (sort (copy-list entries) #'> :key #'car))))
    (when (null (sh-ref-list-reordering sh))
      (return-from build-ref-list-0
        (coerce (subseq init 0 (min n-active (length init))) 'vector)))
    (let ((work (make-array (1+ n-active) :initial-element nil))
          (refidx 0)
          (pred curr))
      (loop for p in init
            for i from 0
            while (<= i n-active)
            do (setf (aref work i) p))
      (dolist (op (sh-ref-list-reordering sh))
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
            ;; shift everything at and after REFIDX one place along, then plant the picture
            (loop for c of-type fixnum from n-active downto (1+ refidx)
                  do (setf (aref work c) (aref work (1- c))))
            (setf (aref work refidx) hit)
            (incf refidx)
            ;; and close the gap by dropping this picture's LATER copy only
            (let ((n refidx))
              (loop for c of-type fixnum from refidx to n-active
                    do (let ((e (aref work c)))
                         (unless (eq e hit)
                           (setf (aref work n) e)
                           (incf n))))
              (loop for c of-type fixnum from n to n-active do (setf (aref work c) nil))))))
      (let ((out (subseq work 0 n-active)))
        (coerce (remove nil out) 'vector)))))

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
         (setf (h264-picture d) (make-picture-for sps)))
       (unless (h264-picture d) (%err "a slice arrived before any picture was started"))
       (setf (pic-frame-num (h264-picture d)) (sh-frame-num sh))
       (decode-slice (h264-picture d) sh br (build-ref-list-0 d sh))
       ;; the loop filter runs over the whole picture once its macroblocks are reconstructed, and
       ;; never during: intra prediction reads UNFILTERED neighbours (8.3), so filtering as we go
       ;; would feed the next macroblock samples the encoder never predicted from
       (deblock-picture (h264-picture d) sh)
       (incf (h264-frames d))
       ;; the filtered picture is what later pictures predict from, so this happens after the
       ;; loop filter and not before it
       (when (nal-idr-p nal) (setf (h264-refs d) '()))
       (when (plusp (nal-ref-idc nal))
         (push (h264-picture d) (h264-refs d))
         (let ((limit (max 1 (sps-max-ref-frames sps))))
           (when (> (length (h264-refs d)) limit)
             (setf (h264-refs d) (subseq (h264-refs d) 0 limit)))))
       (h264-picture d)))
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
      ;; the parameter sets first and serially: every worker reads them, none writes them
      (dolist (n params) (feed-nal d n))
      (or (and (> threads 1) (decode-independent d aus :threads threads))
          (let ((out (list)))
            (dolist (au aus (nreverse out))
              (dolist (n au)
                (let ((p (feed-nal d n))) (when p (push p out))))))))))

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

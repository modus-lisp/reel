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
  (frames 0 :type fixnum))

(defun make-decoder () (%make-decoder))

(defun decoder-width (d) (let ((p (h264-picture d))) (and p (pic-width p))))
(defun decoder-height (d) (let ((p (h264-picture d))) (and p (pic-height p))))

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
       (decode-slice (h264-picture d) sh br)
       ;; the loop filter runs over the whole picture once its macroblocks are reconstructed, and
       ;; never during: intra prediction reads UNFILTERED neighbours (8.3), so filtering as we go
       ;; would feed the next macroblock samples the encoder never predicted from
       (deblock-picture (h264-picture d) sh)
       (incf (h264-frames d))
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

(defun decode-annex-b (bytes)
  "Decode every picture in an Annex B byte stream.  Returns a list of PICTUREs."
  (let ((d (make-decoder)) (out '()) (pending '()))
    (dolist (n (annex-b-nals bytes))
      (cond ((nal-slice-p n)
             (let ((p (feed-nal d n)))
               (when p (push p out))))
            (t (feed-nal d n) (push n pending))))
    (nreverse out)))

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

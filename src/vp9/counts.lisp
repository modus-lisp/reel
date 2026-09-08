;;;; vp9/counts.lisp — how often each symbol was decoded, which is the next frame's model.
;;;;
;;;; A stream that does not set frame-parallel mode refreshes its probability context from the
;;;; COUNTS of the frame just decoded rather than from the forward updates in its header.  So the
;;;; decoder has to tally every decision it makes — which coefficient token, which partition, which
;;;; intra mode, which motion vector class — and at the end of the frame nudge each probability
;;;; towards what the counts say it should have been.
;;;;
;;;; THE NUDGE IS PROPORTIONAL TO CONFIDENCE.  A context seen twice moves a little; one seen twenty
;;;; times or more moves the full step.  That is the `max_count' in the arithmetic below, and it is
;;;; twenty-four for coefficients and twenty for everything else — coefficients are seen so much more
;;;; often that they would otherwise saturate on the first superblock.
;;;;
;;;; Counting is not optional for such a stream and cannot be approximated: the next frame's decoder
;;;; is reading against these numbers, so being one out anywhere desynchronises the frame after this
;;;; one entirely.

(in-package #:reel.vp9)

(defmacro %def-counts ()
  (let ((slots '((coef (4 2 2 6 6 3)) (eob (4 2 2 6 6 2))
                 (skip (3 2)) (intra (4 2))
                 (tx8p (2 2)) (tx16p (2 3)) (tx32p (2 4))
                 (partition (4 4 4))
                 (y-mode (4 10)) (uv-mode (10 10))
                 (comp (5 2)) (comp-ref (5 2)) (single-ref (5 2 2))
                 (filter (4 3)) (mv-mode (7 4))
                 (mv-joint (4))
                 (mv-sign (2 2)) (mv-classes (2 11)) (mv-class0 (2 2)) (mv-bits (2 10 2))
                 (mv-class0-fp (2 2 4)) (mv-fp (2 4)) (mv-class0-hp (2 2)) (mv-hp (2 2)))))
    `(defstruct (counts (:conc-name cn-))
       ,@(loop for (name dims) in slots
               collect `(,name (make-array ',dims :element-type '(signed-byte 32) :initial-element 0)
                               :type (simple-array (signed-byte 32) ,dims))))))
(%def-counts)

;;; ---- one probability, moved towards what the counts say -------------------------------------------

(defun %adapt-prob (p ct0 ct1 max-count factor)
  "Move P towards the ratio the counts imply, by as much as the count justifies (6.4.x).

   The new estimate is `ct0 out of ct0+ct1' as a probability over 256, clamped to [1,255] because
   an arithmetic coder cannot use certainty; the step towards it is scaled by how many observations
   there were, up to MAX-COUNT."
  (declare (type fixnum p ct0 ct1 max-count factor) (optimize (speed 3) (safety 1)))
  (let ((ct (+ ct0 ct1)))
    (declare (type fixnum ct))
    (if (zerop ct)
        p
        (let* ((uf (floor (* factor (min ct max-count)) max-count))
               (p2 (max 1 (min 255 (floor (+ (ash ct0 8) (ash ct -1)) ct)))))
          (declare (type fixnum uf p2))
          (+ p (ash (+ (* (- p2 p) uf) 128) -8))))))

(defmacro %adapt (place ct0 ct1 &optional (max 20) (factor 128))
  `(setf ,place (%adapt-prob ,place ,ct0 ,ct1 ,max ,factor)))

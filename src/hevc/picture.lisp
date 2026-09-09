;;;; hevc/picture.lisp — the decoded picture.
;;;;
;;;; Three planes, no padding.  Intra prediction never reads outside the picture — it asks whether
;;;; a neighbouring sample is available and substitutes when it is not — so the border that motion
;;;; compensation will eventually need is not needed yet, and adding it before there is something
;;;; to read it would only be a guess at what shape it should be.

(in-package #:reel.hevc)

(deftype samples () '(simple-array (unsigned-byte 8) (*)))

(defstruct (picture (:conc-name pic-))
  (width 0 :type fixnum) (height 0 :type fixnum)          ; coded, a whole number of coding blocks
  ;; the conformance window: where the picture the file claims begins inside the coded one, and
  ;; how big it is.  HEVC's spelling of H.264's frame cropping, and it has an origin for the same
  ;; reason and with the same trap.
  (crop-x 0 :type fixnum) (crop-y 0 :type fixnum)
  (disp-width 0 :type fixnum) (disp-height 0 :type fixnum)
  (y (make-array 0 :element-type '(unsigned-byte 8)) :type samples)
  (u (make-array 0 :element-type '(unsigned-byte 8)) :type samples)
  (v (make-array 0 :element-type '(unsigned-byte 8)) :type samples)
  (ystride 0 :type fixnum) (cstride 0 :type fixnum)
  (poc 0 :type fixnum))

(defun make-picture-for (sps)
  (let* ((w (sps-width sps)) (h (sps-height sps))
         (cw (floor w (sps-sub-width sps))) (ch (floor h (sps-sub-height sps))))
    (make-picture
     :width w :height h
     :crop-x (* (sps-sub-width sps) (sps-conf-left sps))
     :crop-y (* (sps-sub-height sps) (sps-conf-top sps))
     :disp-width (sps-display-width sps) :disp-height (sps-display-height sps)
     :y (make-array (* w h) :element-type '(unsigned-byte 8) :initial-element 128)
     :u (make-array (* cw ch) :element-type '(unsigned-byte 8) :initial-element 128)
     :v (make-array (* cw ch) :element-type '(unsigned-byte 8) :initial-element 128)
     :ystride w :cstride cw)))

(declaim (inline pic-plane pic-stride))
(defun pic-plane (pic c-idx)
  (case c-idx (0 (pic-y pic)) (1 (pic-u pic)) (t (pic-v pic))))
(defun pic-stride (pic c-idx)
  (if (zerop c-idx) (pic-ystride pic) (pic-cstride pic)))

(defun picture->yuv420 (pic)
  "The visible picture as tightly packed I420 octets, the conformance window applied."
  (let* ((w (pic-disp-width pic)) (h (pic-disp-height pic))
         (cw (ceiling w 2)) (ch (ceiling h 2))
         (out (make-array (+ (* w h) (* 2 cw ch)) :element-type '(unsigned-byte 8)))
         (o 0))
    (let ((base (+ (* (pic-crop-y pic) (pic-ystride pic)) (pic-crop-x pic))))
      (dotimes (y h)
        (replace out (pic-y pic) :start1 o
                                 :start2 (+ base (* y (pic-ystride pic)))
                                 :end2 (+ base (* y (pic-ystride pic)) w))
        (incf o w)))
    (let ((base (+ (* (ash (pic-crop-y pic) -1) (pic-cstride pic)) (ash (pic-crop-x pic) -1))))
      (dolist (plane (list (pic-u pic) (pic-v pic)))
        (dotimes (y ch)
          (replace out plane :start1 o
                             :start2 (+ base (* y (pic-cstride pic)))
                             :end2 (+ base (* y (pic-cstride pic)) cw))
          (incf o cw))))
    out))

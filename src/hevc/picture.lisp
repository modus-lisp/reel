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
  (poc 0 :type fixnum)
  ;; ---- what the loop filters need, recorded as the picture is decoded
  ;;
  ;; They run over the WHOLE PICTURE after every slice of it is decoded, not per block as it goes,
  ;; because deblocking a vertical edge needs samples the block to its right has not written yet
  ;; and because both filters cross slice boundaries when the slice header says they may.  So the
  ;; per-block facts they consult have to outlive the block.
  ;;
  ;; The boundary strengths are per FOUR SAMPLES along an edge, on the eight-sample grid: BS-V
  ;; indexes the vertical edge at luma x = 8i covering rows 4j, BS-H the horizontal edge at y = 8j
  ;; covering columns 4i.
  (bs-v (make-array 0 :element-type '(unsigned-byte 8)) :type samples)
  (bs-h (make-array 0 :element-type '(unsigned-byte 8)) :type samples)
  (bs-vw 0 :type fixnum) (bs-hw 0 :type fixnum)
  ;; per eight-by-eight luma block: the quantiser it was reconstructed at, and whether it is
  ;; exempt from filtering at all (PCM with the filter disabled, or a transquant bypass block)
  (blk-qp (make-array 0 :element-type '(unsigned-byte 8)) :type samples)
  (blk-nofilt (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (blk-w 0 :type fixnum)
  ;; per coding tree block: the slice that coded it, its deblocking controls, and its SAO table
  (ctb-slice (make-array 0 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (ctb-dbf (make-array 0 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))   ; disabled | beta<<8 | tc<<16, packed
  (ctb-across (make-array 0 :element-type 'bit) :type simple-bit-vector)
  ;; SAO: three components per coding tree block, each a type, four offsets and one parameter
  (sao-type (make-array 0 :element-type '(unsigned-byte 8)) :type samples)
  (sao-off (make-array 0 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (sao-param (make-array 0 :element-type '(unsigned-byte 8)) :type samples)
  (ctbs-wide 0 :type fixnum) (ctbs-high 0 :type fixnum) (ctb-log2 0 :type fixnum)
  ;; ---- the motion field, per 4x4 block and per list
  ;;
  ;; Kept on the PICTURE rather than in the slice state because a later picture reads it: the
  ;; temporal merge candidate comes from the collocated block of another picture, so a reference
  ;; picture's motion has to outlive its decode.  REF-POC holds what the index pointed AT rather
  ;; than the index itself, because an index only means something relative to a list that is gone.
  (mv (make-array 0 :element-type '(signed-byte 16)) :type (simple-array (signed-byte 16) (*)))
  (ref-idx (make-array 0 :element-type '(signed-byte 8)) :type (simple-array (signed-byte 8) (*)))
  (ref-poc (make-array 0 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (mv-w 0 :type fixnum) (mv-h 0 :type fixnum)
  (reference-p nil)
  (output-done nil)                     ; has this picture been handed out yet
  ;; true where a 4x4 block was coded intra, which the candidate derivations test constantly
  (intra (make-array 0 :element-type 'bit) :type simple-bit-vector)
  ;; and true where its transform block carried any non-zero coefficient, which is one of the
  ;; three things the deblocking filter's boundary strength is derived from
  (cbf (make-array 0 :element-type 'bit) :type simple-bit-vector))

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
     :ystride w :cstride cw
     :bs-v (make-array (* (ash w -3) (ash h -2)) :element-type '(unsigned-byte 8)
                                                 :initial-element 0)
     :bs-h (make-array (* (ash w -2) (ash h -3)) :element-type '(unsigned-byte 8)
                                                 :initial-element 0)
     :bs-vw (ash w -3) :bs-hw (ash w -2)
     :blk-qp (make-array (* (ash w -3) (ash h -3)) :element-type '(unsigned-byte 8)
                                                   :initial-element 26)
     :blk-nofilt (make-array (* (ash w -3) (ash h -3)) :element-type 'bit :initial-element 0)
     :blk-w (ash w -3)
     :ctb-slice (make-array (sps-ctbs sps) :element-type '(signed-byte 32) :initial-element -1)
     :ctb-dbf (make-array (sps-ctbs sps) :element-type '(signed-byte 32) :initial-element 0)
     :ctb-across (make-array (sps-ctbs sps) :element-type 'bit :initial-element 0)
     :sao-type (make-array (* 3 (sps-ctbs sps)) :element-type '(unsigned-byte 8)
                                                :initial-element 0)
     :sao-off (make-array (* 12 (sps-ctbs sps)) :element-type '(signed-byte 32)
                                                :initial-element 0)
     :sao-param (make-array (* 3 (sps-ctbs sps)) :element-type '(unsigned-byte 8)
                                                 :initial-element 0)
     :ctbs-wide (sps-ctbs-wide sps) :ctbs-high (sps-ctbs-high sps)
     :ctb-log2 (sps-ctb-log2 sps)
     :mv (make-array (* 4 (ash w -2) (ash h -2)) :element-type '(signed-byte 16)
                                                 :initial-element 0)
     :ref-idx (make-array (* 2 (ash w -2) (ash h -2)) :element-type '(signed-byte 8)
                                                      :initial-element -1)
     :ref-poc (make-array (* 2 (ash w -2) (ash h -2)) :element-type '(signed-byte 32)
                                                      :initial-element 0)
     :mv-w (ash w -2) :mv-h (ash h -2)
     :intra (make-array (* (ash w -2) (ash h -2)) :element-type 'bit :initial-element 0)
     :cbf (make-array (* (ash w -2) (ash h -2)) :element-type 'bit :initial-element 0))))

(declaim (inline pic-mv-index))
(defun pic-mv-index (pic x y)
  "The motion field index of the 4x4 block containing luma sample (X,Y)."
  (declare (type fixnum x y))
  (+ (* (ash y -2) (pic-mv-w pic)) (ash x -2)))

(defun pic-motion (pic x y lx)
  "(values mvx mvy ref-idx ref-poc) for the 4x4 block at (X,Y) in list LX.
   A REF-IDX of -1 means this block does not predict from that list."
  (declare (type fixnum x y lx))
  (let ((i (pic-mv-index pic x y)))
    (values (aref (pic-mv pic) (+ (* 4 i) (* 2 lx)))
            (aref (pic-mv pic) (+ (* 4 i) (* 2 lx) 1))
            (aref (pic-ref-idx pic) (+ (* 2 i) lx))
            (aref (pic-ref-poc pic) (+ (* 2 i) lx)))))

(defun set-pic-motion (pic x y lx mvx mvy idx poc)
  (declare (type fixnum x y lx mvx mvy idx poc))
  (let ((i (pic-mv-index pic x y)))
    (setf (aref (pic-mv pic) (+ (* 4 i) (* 2 lx))) mvx
          (aref (pic-mv pic) (+ (* 4 i) (* 2 lx) 1)) mvy
          (aref (pic-ref-idx pic) (+ (* 2 i) lx)) idx
          (aref (pic-ref-poc pic) (+ (* 2 i) lx)) poc)))

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

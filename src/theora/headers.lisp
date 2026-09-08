;;;; theora/headers.lisp — the three packets that configure a Theora decoder.
;;;;
;;;; Theora sends its configuration once, in three packets at the head of the stream: an
;;;; IDENTIFICATION packet with the picture geometry and frame rate, a COMMENT packet which a
;;;; decoder can ignore entirely, and a SETUP packet carrying the loop filter limits, the
;;;; quantisation machinery, and all eighty coefficient Huffman tables.
;;;;
;;;; THE QUANTISATION MACHINERY IS THE PART WORTH READING TWICE.  A stream does not send a matrix
;;;; per quantiser: it sends a pool of BASE matrices and, for each of the six (intra or inter) x
;;;; (Y, Cb, Cr) combinations, a list of quantiser RANGES saying which base matrix each range starts
;;;; from.  A matrix for a given quantiser is then interpolated between the two bases its range sits
;;;; between.  Sixty-four matrices per combination, from three or four transmitted ones.

(in-package #:reel.theora)

(defstruct (info (:conc-name inf-))
  (version 0 :type fixnum)
  (width 0 :type fixnum) (height 0 :type fixnum)          ; coded, a whole number of macroblocks
  (picture-width 0 :type fixnum) (picture-height 0 :type fixnum)
  (offset-x 0 :type fixnum) (offset-y 0 :type fixnum)
  (fps-num 25 :type fixnum) (fps-den 1 :type fixnum)
  (pixel-format 0 :type fixnum)                           ; 0 = 4:2:0, 2 = 4:2:2, 3 = 4:4:4
  (quality 0 :type fixnum)
  (keyframe-shift 6 :type fixnum)
  ;; derived
  (mb-width 0 :type fixnum) (mb-height 0 :type fixnum)
  (fragment-width 0 :type fixnum) (fragment-height 0 :type fixnum)
  (sb-width 0 :type fixnum) (sb-height 0 :type fixnum)
  ;; from the setup packet
  (filter-limits nil) (ac-scale nil) (dc-scale nil)
  (base-matrices nil)
  (qr-count nil) (qr-size nil) (qr-base nil)
  (huff nil))                                             ; 80 coefficient tables

(defun parse-identification (bytes)
  "The first header packet (6.2): geometry, rate, and how the granule position is packed."
  (declare (type octets bytes))
  (let ((br (make-br bytes :start 7 :end (length bytes)))   ; past the 0x80 and "theora"
        (i (make-info)))
    (setf (inf-version i) (logior (ash (read-bits br 8) 16) (ash (read-bits br 8) 8)
                                  (read-bits br 8)))
    (when (< (inf-version i) #x030200)
      ;; before alpha 3 the picture was stored upside down relative to everything since
      (%err "Theora bitstreams older than 3.2.0 are not supported"))
    (setf (inf-mb-width i) (read-bits br 16)
          (inf-mb-height i) (read-bits br 16))
    (setf (inf-width i) (* 16 (inf-mb-width i))
          (inf-height i) (* 16 (inf-mb-height i)))
    (setf (inf-picture-width i) (read-bits br 24)
          (inf-picture-height i) (read-bits br 24)
          (inf-offset-x i) (read-bits br 8)
          (inf-offset-y i) (read-bits br 8))
    (setf (inf-fps-num i) (read-bits-long br 32)
          (inf-fps-den i) (read-bits-long br 32))
    (read-bits br 24) (read-bits br 24)                    ; pixel aspect ratio
    (read-bits br 8)                                       ; colour space
    (read-bits br 24)                                      ; nominal bit rate
    (setf (inf-quality i) (read-bits br 6))
    (setf (inf-keyframe-shift i) (read-bits br 5))
    (setf (inf-pixel-format i) (read-bits br 2))
    (unless (zerop (inf-pixel-format i))
      (%err "chroma format ~d is not supported (4:2:0 only)" (inf-pixel-format i)))
    ;; a fragment is an 8x8 block; a superblock is four by four of them
    (setf (inf-fragment-width i) (ash (inf-width i) -3)
          (inf-fragment-height i) (ash (inf-height i) -3))
    (setf (inf-sb-width i) (ceiling (inf-fragment-width i) 4)
          (inf-sb-height i) (ceiling (inf-fragment-height i) 4))
    i))

(defun parse-setup (bytes i)
  "The third header packet (6.4): loop filter limits, quantisation, and eighty Huffman tables."
  (declare (type octets bytes))
  (let ((br (make-br bytes :start 7 :end (length bytes))))
    ;; loop filter limits, sent as a bit width and then sixty-four values of it
    (let ((n (read-bits br 3))
          (lim (make-array 64 :element-type 'fixnum :initial-element 0)))
      (when (plusp n) (dotimes (k 64) (setf (aref lim k) (read-bits br n))))
      (setf (inf-filter-limits i) lim))
    (let ((n (1+ (read-bits br 4)))
          (ac (make-array 64 :element-type 'fixnum)))
      (dotimes (k 64) (setf (aref ac k) (read-bits br n)))
      (setf (inf-ac-scale i) ac))
    (let ((n (1+ (read-bits br 4)))
          (dc (make-array 64 :element-type 'fixnum)))
      (dotimes (k 64) (setf (aref dc k) (read-bits br n)))
      (setf (inf-dc-scale i) dc))
    (let* ((matrices (1+ (read-bits br 9)))
           (base (make-array (list matrices 64) :element-type 'fixnum)))
      (when (> matrices 384) (%err "~d base quantiser matrices" matrices))
      (dotimes (j matrices)
        (dotimes (k 64) (setf (aref base j k) (read-bits br 8))))
      (setf (inf-base-matrices i) base)
      ;; the quantiser ranges, for each of inter x plane
      (let ((qr-count (make-array '(2 3) :element-type 'fixnum :initial-element 0))
            (qr-size (make-array '(2 3 64) :element-type 'fixnum :initial-element 0))
            (qr-base (make-array '(2 3 65) :element-type 'fixnum :initial-element 0)))
        (dotimes (inter 2)
          (dotimes (plane 3)
            (let ((newqr 1))
              (when (or (plusp inter) (plusp plane)) (setf newqr (read-bit br)))
              (if (zerop newqr)
                  ;; this combination reuses another's ranges, named either directly or by position
                  (multiple-value-bind (qtj plj)
                      (if (and (plusp inter) (= 1 (read-bit br)))
                          (values 0 plane)
                          (values (floor (+ (* 3 inter) plane -1) 3) (mod (+ plane 2) 3)))
                    (setf (aref qr-count inter plane) (aref qr-count qtj plj))
                    (dotimes (k 64) (setf (aref qr-size inter plane k) (aref qr-size qtj plj k)))
                    (dotimes (k 65) (setf (aref qr-base inter plane k) (aref qr-base qtj plj k))))
                  (let ((qri 0) (qi 0)
                        (bits (1+ (integer-length (1- (max 2 matrices))))))
                    (declare (ignorable bits))
                    (loop
                      (let ((idx (read-bits br (max 1 (integer-length (1- (max 2 matrices)))))))
                        (when (>= idx matrices) (%err "base matrix index ~d" idx))
                        (setf (aref qr-base inter plane qri) idx)
                        (when (>= qi 63) (return))
                        (let ((sz (1+ (read-bits br (max 1 (integer-length (- 63 qi)))))))
                          (setf (aref qr-size inter plane qri) sz)
                          (incf qri)
                          (incf qi sz))))
                    (when (> qi 63) (%err "quantiser ranges covering ~d" qi))
                    (setf (aref qr-count inter plane) qri))))))
        (setf (inf-qr-count i) qr-count (inf-qr-size i) qr-size (inf-qr-base i) qr-base)))
    ;; EIGHTY coefficient tables: sixteen groups of five, and a picture picks a group per plane
    (let ((tables (make-array 80)))
      (dotimes (k 80) (setf (aref tables k) (read-huff-tree br)))
      (setf (inf-huff i) tables))
    i))

(defun parse-headers (packets)
  "The identification and setup packets, which is all a decoder needs; the comment is skipped."
  (let ((id (first packets)) (setup (third packets)))
    (unless (and id setup) (%err "a Theora stream with fewer than three header packets"))
    (parse-setup setup (parse-identification id))))

;;; ---- quantisation matrices ----------------------------------------------------------------------

(defun dequant-matrix (i inter plane qi)
  "The 64 weights for one quantiser index, interpolated between the two base matrices its range
   lies between (6.4.3).

   The interpolation is the reason a stream can send three matrices and mean sixty-four: the ranges
   partition the quantiser scale, and within a range the matrix slides linearly from the base at one
   end to the base at the other."
  (declare (type fixnum inter plane qi))
  (let* ((count (aref (inf-qr-count i) inter plane))
         (base (inf-base-matrices i))
         (out (make-array 64 :element-type 'fixnum))
         (qri 0) (qstart 0))
    (declare (type fixnum count qri qstart))
    (loop while (and (< qri (1- count))
                     (>= qi (+ qstart (aref (inf-qr-size i) inter plane qri))))
          do (incf qstart (aref (inf-qr-size i) inter plane qri))
             (incf qri))
    (let* ((size (aref (inf-qr-size i) inter plane qri))
           (b0 (aref (inf-qr-base i) inter plane qri))
           (b1 (aref (inf-qr-base i) inter plane (1+ qri)))
           (frac (- qi qstart)))
      (declare (type fixnum size b0 b1 frac))
      (dotimes (k 64 out)
        (setf (aref out k)
              (if (zerop size)
                  (aref base b0 k)
                  (floor (+ (* (- size frac) (aref base b0 k))
                            (* frac (aref base b1 k))
                            (ash size -1))
                         size)))))))

;;;; decode/inter.lisp — inter frames, and the decoder that ties both halves together (RFC 6386): key frames
;;;; and inter frames, golden/altref references, split motion vectors,
;;;; six-tap and bilinear sub-pixel prediction, probability persistence.
;;;;
;;;; The intra pipeline (boolean decoder, token decoding, intra prediction,
;;;; transforms, loop-filter kernels) sits beside this in decode/; this file
;;;; adds what a still image never needed: the inter-frame header, mode/MV decoding,
;;;; motion compensation from bordered reference frames, the per-macroblock
;;;; loop-filter deltas, and reference-buffer management.
;;;;
;;;; The current frame is reconstructed in webp-pure PLANEs (fixnum rasters
;;;; with the 127/129 intra border), loop filtered in place, then copied into
;;;; an RFRAME — octet planes with a replicated border — which is what motion
;;;; compensation reads and what the caller gets as a PICTURE.  Vectors that
;;;; reach past the border fall back to clamped fetches, i.e. unbounded
;;;; replication, which is what the specification defines.
(in-package #:reel.decode)

(deftype u8vec () '(simple-array (unsigned-byte 8) (*)))
(deftype fxvec () '(simple-array (signed-byte 32) (*)))

(defconstant +border+ 32)                       ; luma border pixels
(defconstant +cborder+ 16)                      ; chroma border pixels

;;; ---- reference frames -------------------------------------------------------

(defstruct (rframe (:conc-name rf-))
  (y nil :type (or null u8vec)) (u nil :type (or null u8vec)) (v nil :type (or null u8vec))
  (ystride 0 :type fixnum) (uvstride 0 :type fixnum)
  (aw 0 :type fixnum) (ah 0 :type fixnum)       ; macroblock-aligned dimensions
  (yrows 0 :type fixnum) (uvrows 0 :type fixnum))

(defun make-rframe* (aw ah)
  (let* ((ys (+ aw (* 2 +border+))) (yr (+ ah (* 2 +border+)))
         (cw (ash aw -1)) (ch (ash ah -1))
         (cs (+ cw (* 2 +cborder+))) (cr (+ ch (* 2 +cborder+))))
    (make-rframe :y (make-array (* ys yr) :element-type '(unsigned-byte 8))
                 :u (make-array (* cs cr) :element-type '(unsigned-byte 8))
                 :v (make-array (* cs cr) :element-type '(unsigned-byte 8))
                 :ystride ys :uvstride cs :aw aw :ah ah :yrows yr :uvrows cr)))

(defun plane->rframe-plane (pl dst stride border w h)
  "Copy the visible WxH of plane PL into DST (stride STRIDE, BORDER pixels on
   each side) and replicate its edges into the border."
  (declare (type plane pl) (type u8vec dst) (type fixnum stride border w h)
           (optimize (speed 3) (safety 0)))
  (let ((src (pl-data pl)) (ps (pl-stride pl)))
    (declare (type fxvec src) (type fixnum ps))
    (dotimes (y h)
      (let ((si (+ (* (1+ y) ps) 1)) (di (+ (* (+ y border) stride) border)))
        (declare (type fixnum si di))
        (dotimes (x w)
          (setf (aref dst (+ di x)) (the (unsigned-byte 8) (aref src (+ si x)))))
        ;; left and right border of this row
        (let ((l (aref dst di)) (r (aref dst (+ di w -1))))
          (dotimes (i border)
            (setf (aref dst (+ di (- i) -1)) l
                  (aref dst (+ di w i)) r)))))
    ;; top and bottom rows
    (let ((top (* border stride)) (bot (* (+ border h -1) stride)))
      (declare (type fixnum top bot))
      (dotimes (i border)
        (replace dst dst :start1 (* i stride) :end1 (* (1+ i) stride) :start2 top)
        (replace dst dst :start1 (* (+ border h i) stride) :end1 (* (+ border h i 1) stride) :start2 bot)))
    dst))

;;; ---- pictures (what the caller sees) -------------------------------------------

(defstruct (picture (:conc-name picture-) (:constructor make-picture)
                    (:constructor %make-shared-picture))
  (width 0 :type fixnum) (height 0 :type fixnum)
  y u v                                         ; octet planes (shared with the decoder's reference)
  (y-stride 0 :type fixnum) (uv-stride 0 :type fixnum)
  (y-offset 0 :type fixnum) (uv-offset 0 :type fixnum)
  (timestamp nil))                              ; seconds (double) when known

(defun rframe->picture (rf width height)
  (make-picture :width width :height height :y (rf-y rf) :u (rf-u rf) :v (rf-v rf)
                :y-stride (rf-ystride rf) :uv-stride (rf-uvstride rf)
                :y-offset (+ (* +border+ (rf-ystride rf)) +border+)
                :uv-offset (+ (* +cborder+ (rf-uvstride rf)) +cborder+)))

(defun picture->yuv420 (pic)
  "Tightly packed I420 octets (Y then U then V) of the visible picture."
  (let* ((w (picture-width pic)) (h (picture-height pic))
         (cw (ceiling w 2)) (ch (ceiling h 2))
         (out (%octets (+ (* w h) (* 2 cw ch))))
         (o 0))
    (flet ((copy-plane (src stride off pw ph)
             (dotimes (y ph)
               (replace out src :start1 o :start2 (+ off (* y stride)) :end2 (+ off (* y stride) pw))
               (incf o pw))))
      (copy-plane (picture-y pic) (picture-y-stride pic) (picture-y-offset pic) w h)
      (copy-plane (picture-u pic) (picture-uv-stride pic) (picture-uv-offset pic) cw ch)
      (copy-plane (picture-v pic) (picture-uv-stride pic) (picture-uv-offset pic) cw ch))
    out))

(declaim (inline %rgb-clamp))
(defun %rgb-clamp (v) (declare (type fixnum v)) (max 0 (min 255 (ash v -6))))

(defun picture->rgb-into (pic rgb &key (offset 0) (stride nil) (channels 3))
  "Convert PIC to 8-bit RGB (or RGBA when CHANNELS is 4) into RGB (octets) at
   OFFSET with row STRIDE (bytes, default packed).  Nearest-neighbour chroma;
   the BT.601 fixed-point matrix libwebp uses."
  (declare (type picture pic) (type u8vec rgb) (optimize (speed 3) (safety 0)))
  (let* ((w (picture-width pic)) (h (picture-height pic))
         (yp (picture-y pic)) (up (picture-u pic)) (vp (picture-v pic))
         (ys (picture-y-stride pic)) (cs (picture-uv-stride pic))
         (yo (picture-y-offset pic)) (co (picture-uv-offset pic))
         (stride (or stride (* w channels))))
    (declare (type u8vec yp up vp) (type fixnum w h ys cs yo co stride channels offset))
    (dotimes (y h)
      (let ((yi (+ yo (* y ys))) (ci (+ co (* (ash y -1) cs))) (oi (+ offset (* y stride))))
        (declare (type fixnum yi ci oi))
        (dotimes (x w)
          (let* ((yy (ash (* (aref yp (+ yi x)) 19077) -8))
                 (u (aref up (+ ci (ash x -1)))) (v (aref vp (+ ci (ash x -1))))
                 (r (%rgb-clamp (+ yy (ash (* v 26149) -8) -14234)))
                 (g (%rgb-clamp (+ yy (- (ash (* u 6419) -8)) (- (ash (* v 13320) -8)) 8708)))
                 (b (%rgb-clamp (+ yy (ash (* u 33050) -8) -17685))))
            (declare (type fixnum yy u v r g b))
            (setf (aref rgb oi) r (aref rgb (+ oi 1)) g (aref rgb (+ oi 2)) b)
            (when (= channels 4) (setf (aref rgb (+ oi 3)) 255))
            (incf oi channels)))))
    rgb))

(defun picture->rgb (pic &key (channels 3))
  "Packed 8-bit RGB (or RGBA) octets of PIC."
  (picture->rgb-into pic (%octets (* (picture-width pic) (picture-height pic) channels))
                     :channels channels))

;;; ---- decoder state ------------------------------------------------------------

(defstruct (decoder (:conc-name vd-) (:constructor %make-decoder))
  d                                             ; webp-pure DEC: planes, contexts, probs, seg/lf headers
  (width 0 :type fixnum) (height 0 :type fixnum)
  (mb-cols 0 :type fixnum) (mb-rows 0 :type fixnum)
  (version 0 :type fixnum)
  filters                                       ; +sixtap-filters+ or +bilinear-filters+
  (full-pixel nil)
  (key-frame nil)
  ;; entropy state that persists across frames (coefficient probs live in D)
  (ymode-probs (copy-seq +default-y-mode-probs+))
  (uv-mode-probs (copy-seq +default-uv-mode-probs+))
  (mv-probs (copy-seq +default-mv-probs+))
  (prob-intra 0 :type fixnum) (prob-last 0 :type fixnum) (prob-gf 0 :type fixnum)
  saved-coeff-probs saved-ymode-probs saved-uv-mode-probs saved-mv-probs
  ;; reference header
  (refresh-last t) (refresh-golden t) (refresh-altref t) (copy-golden 0) (copy-altref 0)
  (refresh-entropy t)
  (sign-bias (make-array 4 :element-type '(signed-byte 32) :initial-element 0))
  ;; reference frames
  last golden altref current last-output
  (pool '())
  ;; per-macroblock records (row-major, mb-cols x mb-rows)
  mb-ymode mb-uvmode mb-ref mb-mvr mb-mvc mb-split-r mb-split-c mb-segment mb-skip
  ;; scratch
  (near-r (make-array 4 :element-type '(signed-byte 32))) (near-c (make-array 4 :element-type '(signed-byte 32)))
  (cnt (make-array 4 :element-type '(signed-byte 32)))
  (mc-tmp (make-array (* 16 21) :element-type '(signed-byte 32)))
  (mc-scratch (make-array (* 21 21) :element-type '(unsigned-byte 8)))
  (split-r (make-array 16 :element-type '(signed-byte 32))) (split-c (make-array 16 :element-type '(signed-byte 32)))
  (frame-count 0 :type fixnum)
  (shown-count 0 :type fixnum))

(defun make-decoder ()
  "A fresh decoder; dimensions are taken from the first key frame."
  (%make-decoder))

(defun vd-init-dimensions (vd width height)
  (let* ((cols (ceiling width 16)) (rows (ceiling height 16)) (n (* cols rows)))
    (setf (vd-width vd) width (vd-height vd) height
          (vd-mb-cols vd) cols (vd-mb-rows vd) rows
          (vd-d vd) (make-dec
                     :mb-cols cols :mb-rows rows :width width :height height
                     :yplane (make-plane* (* cols 16) (* rows 16) 4)
                     :uplane (make-plane* (* cols 8) (* rows 8) 0)
                     :vplane (make-plane* (* cols 8) (* rows 8) 0)
                     :coeff-probs (copy-seq +default-coeff-probs+)
                     :above-y (make-array (* cols 4) :element-type '(signed-byte 32) :initial-element 0)
                     :above-u (make-array (* cols 2) :element-type '(signed-byte 32) :initial-element 0)
                     :above-v (make-array (* cols 2) :element-type '(signed-byte 32) :initial-element 0)
                     :above-y2 (make-array cols :element-type '(signed-byte 32) :initial-element 0)
                     :above-bmode (make-array (* cols 4) :element-type '(signed-byte 32) :initial-element 0)
                     :mb-i4x4 (make-array n)
                     :mb-nonzero (make-array n)
                     :mb-seg (make-array n :element-type '(signed-byte 32) :initial-element 0))
          (vd-mb-ymode vd) (make-array n :element-type '(signed-byte 32) :initial-element 0)
          (vd-mb-uvmode vd) (make-array n :element-type '(signed-byte 32) :initial-element 0)
          (vd-mb-ref vd) (make-array n :element-type '(signed-byte 32) :initial-element 0)
          (vd-mb-mvr vd) (make-array n :element-type '(signed-byte 32) :initial-element 0)
          (vd-mb-mvc vd) (make-array n :element-type '(signed-byte 32) :initial-element 0)
          (vd-mb-split-r vd) (make-array (* 16 n) :element-type '(signed-byte 32) :initial-element 0)
          (vd-mb-split-c vd) (make-array (* 16 n) :element-type '(signed-byte 32) :initial-element 0)
          (vd-mb-segment vd) (make-array n :element-type '(signed-byte 32) :initial-element 0)
          (vd-mb-skip vd) (make-array n :initial-element nil)
          (vd-last vd) nil (vd-golden vd) nil (vd-altref vd) nil (vd-current vd) nil
          (vd-last-output vd) nil (vd-pool vd) '())))

(defun vd-take-frame (vd)
  "A reference frame buffer nobody else is holding."
  (let ((live (list (vd-last vd) (vd-golden vd) (vd-altref vd) (vd-last-output vd))))
    (or (loop for f in (vd-pool vd) unless (member f live) do (return f))
        (let ((f (make-rframe* (* 16 (vd-mb-cols vd)) (* 16 (vd-mb-rows vd)))))
          (push f (vd-pool vd))
          f))))

;;; ---- frame header (RFC 6386 s9 / s19.2) -----------------------------------------

(defun parse-frame-tag (bytes start end)
  "Returns (values key-frame-p version show-p part0-size width height data-start)."
  (when (< (- end start) 3) (%err "VP8 frame shorter than its tag"))
  (let* ((tag (u24le bytes start))
         (key (zerop (logand tag 1)))
         (version (logand (ash tag -1) 7))
         (show (= 1 (logand (ash tag -4) 1)))
         (part0 (ash tag -5)))
    (if key
        (progn
          (when (< (- end start) 10) (%err "VP8 key frame shorter than its header"))
          (unless (and (= (aref bytes (+ start 3)) #x9d) (= (aref bytes (+ start 4)) #x01)
                       (= (aref bytes (+ start 5)) #x2a))
            (%err "bad VP8 key-frame start code"))
          (let ((w (logand (u16le bytes (+ start 6)) #x3fff))
                (h (logand (u16le bytes (+ start 8)) #x3fff)))
            (values t version show part0 w h (+ start 10))))
        (values nil version show part0 nil nil (+ start 3)))))

(defun read-header (vd bd key)
  "Read the compressed part of the frame header from BD.  Returns the number
   of token partitions."
  (let ((d (vd-d vd)))
    (when key
      (bool-bit bd 128) (bool-bit bd 128)                       ; colour space, clamping type
      ;; key frames reset segmentation and loop-filter deltas
      (setf (d-seg-enabled d) nil (d-seg-update-map d) nil (d-seg-abs d) nil)
      (fill (d-seg-quant d) 0) (fill (d-seg-filter d) 0)
      (fill (d-ref-lf-delta d) 0) (fill (d-mode-lf-delta d) 0))
    ;; segmentation (s9.3)
    (setf (d-seg-enabled d) (= 1 (bool-bit bd 128)))
    (cond
      ((d-seg-enabled d)
       (let ((update-map (= 1 (bool-bit bd 128)))
             (update-data (= 1 (bool-bit bd 128))))
         (setf (d-seg-update-map d) update-map)
         (when update-data
           (setf (d-seg-abs d) (= 1 (bool-bit bd 128)))
           (dotimes (i 4)
             (setf (aref (d-seg-quant d) i) (if (= 1 (bool-bit bd 128)) (bool-signed bd 7) 0)))
           (dotimes (i 4)
             (setf (aref (d-seg-filter d) i) (if (= 1 (bool-bit bd 128)) (bool-signed bd 6) 0))))
         (when update-map
           (dotimes (i 3)
             (setf (aref (d-seg-tree-probs d) i)
                   (if (= 1 (bool-bit bd 128)) (bool-literal bd 8) 255))))))
      (t (setf (d-seg-update-map d) nil)))
    ;; loop filter (s9.4)
    (setf (d-filter-simple d) (= 1 (bool-bit bd 128))
          (d-filter-level d) (bool-literal bd 6)
          (d-sharpness d) (bool-literal bd 3)
          (d-lf-delta-enabled d) (= 1 (bool-bit bd 128)))
    (when (and (d-lf-delta-enabled d) (= 1 (bool-bit bd 128)))
      (dotimes (i 4)
        (when (= 1 (bool-bit bd 128)) (setf (aref (d-ref-lf-delta d) i) (bool-signed bd 6))))
      (dotimes (i 4)
        (when (= 1 (bool-bit bd 128)) (setf (aref (d-mode-lf-delta d) i) (bool-signed bd 6)))))
    (let ((nparts (ash 1 (bool-literal bd 2))))
      ;; quantiser indices (s9.6)
      (let* ((yac (bool-literal bd 7))
             (ydc (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0))
             (y2dc (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0))
             (y2ac (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0))
             (uvdc (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0))
             (uvac (if (= 1 (bool-bit bd 128)) (bool-signed bd 4) 0)))
        (unless (d-seg-dq d) (setf (d-seg-dq d) (make-array 4)))
        (dotimes (s 4)
          (let ((q (cond ((not (d-seg-enabled d)) yac)
                         ((d-seg-abs d) (aref (d-seg-quant d) s))
                         (t (+ yac (aref (d-seg-quant d) s))))))
            (setf (aref (d-seg-dq d) s) (segment-dequant q ydc y2dc y2ac uvdc uvac)))))
      ;; reference header (s9.7-9.8)
      (cond
        (key
         (setf (vd-refresh-golden vd) t (vd-refresh-altref vd) t
               (vd-copy-golden vd) 0 (vd-copy-altref vd) 0)
         (fill (vd-sign-bias vd) 0)
         (setf (vd-refresh-entropy vd) (= 1 (bool-bit bd 128))
               (vd-refresh-last vd) t))
        (t
         (setf (vd-refresh-golden vd) (= 1 (bool-bit bd 128))
               (vd-refresh-altref vd) (= 1 (bool-bit bd 128)))
         (setf (vd-copy-golden vd) (if (vd-refresh-golden vd) 0 (bool-literal bd 2)))
         (setf (vd-copy-altref vd) (if (vd-refresh-altref vd) 0 (bool-literal bd 2)))
         (setf (aref (vd-sign-bias vd) +golden-frame+) (bool-bit bd 128)
               (aref (vd-sign-bias vd) +altref-frame+) (bool-bit bd 128))
         (setf (vd-refresh-entropy vd) (= 1 (bool-bit bd 128))
               (vd-refresh-last vd) (= 1 (bool-bit bd 128)))))
      ;; key frames restore the default probabilities before any update
      (when key
        (replace (d-coeff-probs d) +default-coeff-probs+)
        (replace (vd-mv-probs vd) +default-mv-probs+)
        (replace (vd-ymode-probs vd) +default-y-mode-probs+)
        (replace (vd-uv-mode-probs vd) +default-uv-mode-probs+))
      ;; a frame whose updates are not persistent: save the state to restore later
      (unless (vd-refresh-entropy vd)
        (setf (vd-saved-coeff-probs vd) (copy-seq (d-coeff-probs d))
              (vd-saved-mv-probs vd) (copy-seq (vd-mv-probs vd))
              (vd-saved-ymode-probs vd) (copy-seq (vd-ymode-probs vd))
              (vd-saved-uv-mode-probs vd) (copy-seq (vd-uv-mode-probs vd))))
      ;; coefficient probability updates (s13.4)
      (let ((cp (d-coeff-probs d)))
        (dotimes (i 1056)
          (when (= 1 (bool-bit bd (aref +coeff-update-probs+ i)))
            (setf (aref cp i) (bool-literal bd 8)))))
      ;; skip flag probability
      (setf (d-mb-no-skip d) (= 1 (bool-bit bd 128)))
      (setf (d-prob-skip d) (if (d-mb-no-skip d) (bool-literal bd 8) 0))
      ;; inter-frame probabilities
      (unless key
        (setf (vd-prob-intra vd) (bool-literal bd 8)
              (vd-prob-last vd) (bool-literal bd 8)
              (vd-prob-gf vd) (bool-literal bd 8))
        (when (= 1 (bool-bit bd 128))
          (dotimes (i 4) (setf (aref (vd-ymode-probs vd) i) (bool-literal bd 8))))
        (when (= 1 (bool-bit bd 128))
          (dotimes (i 3) (setf (aref (vd-uv-mode-probs vd) i) (bool-literal bd 8))))
        (let ((mvp (vd-mv-probs vd)))
          (dotimes (i 38)
            (when (= 1 (bool-bit bd (aref +mv-update-probs+ i)))
              (let ((x (bool-literal bd 7)))
                (setf (aref mvp i) (if (zerop x) 1 (ash x 1))))))))
      nparts)))

;;; ---- motion vector decoding (RFC 6386 s16.3, s17) -------------------------------

(defun read-mv-component (bd probs base)
  "One vector component in eighth-pel units (the wire quarter-pel value doubled)."
  (declare (type fxvec probs) (type fixnum base))
  (let ((x 0))
    (declare (type fixnum x))
    (cond
      ((= 1 (bool-bit bd (aref probs base)))          ; long form
       (dotimes (i 3)
         (setf x (logior x (ash (bool-bit bd (aref probs (+ base 9 i))) i))))
       (loop for i from 9 downto 4 do
         (setf x (logior x (ash (bool-bit bd (aref probs (+ base 9 i))) i))))
       (when (or (zerop (logand x #xfff0)) (= 1 (bool-bit bd (aref probs (+ base 9 3)))))
         (incf x 8)))
      (t (setf x (treed-read bd +small-mv-tree+ probs (+ base 2)))))
    (when (and (/= x 0) (= 1 (bool-bit bd (aref probs (+ base 1)))))
      (setf x (- x)))
    (ash x 1)))

(declaim (inline clamp-mv))
(defun clamp-mv (v lo hi) (declare (type fixnum v lo hi)) (max lo (min hi v)))

(defun mb-index (vd mbx mby) (+ (* mby (vd-mb-cols vd)) mbx))

(defun find-near-mvs (vd mbx mby ref)
  "Fill VD's near-r/near-c (index 0 best, 1 nearest, 2 near) and cnt for the
   macroblock at (MBX,MBY) referencing REF, following the reference decoder
   including sign-bias correction."
  (declare (type fixnum mbx mby ref))
  (let* ((nr (vd-near-r vd)) (nc (vd-near-c vd)) (cnt (vd-cnt vd))
         (cols (vd-mb-cols vd))
         (refs (vd-mb-ref vd)) (mvr (vd-mb-mvr vd)) (mvc (vd-mb-mvc vd)) (modes (vd-mb-ymode vd))
         (bias (vd-sign-bias vd))
         (n 0))
    (declare (type fxvec nr nc cnt refs mvr mvc modes bias) (type fixnum cols n))
    (fill nr 0) (fill nc 0) (fill cnt 0)
    (flet ((neighbour (dx dy weight)
             (let ((bx (+ mbx dx)) (by (+ mby dy)))
               (when (and (>= bx 0) (>= by 0))
                 (let ((i (+ (* by cols) bx)))
                   (when (/= (aref refs i) +intra-frame+)
                     (let ((r (aref mvr i)) (c (aref mvc i)))
                       (cond
                         ((or (/= r 0) (/= c 0))
                          (when (/= (aref bias (aref refs i)) (aref bias ref))
                            (setf r (- r) c (- c)))
                          ;; above always opens a new slot; left/above-left only when different
                          (if (or (= n 0) (/= r (aref nr n)) (/= c (aref nc n)))
                              (progn (incf n) (setf (aref nr n) r (aref nc n) c)
                                     (incf (aref cnt n) weight))
                              (incf (aref cnt n) weight)))
                         (t (incf (aref cnt 0) weight))))))))))
      (neighbour 0 -1 2)
      (neighbour -1 0 2)
      (neighbour -1 -1 1))
    ;; three distinct vectors: merge above-left with nearest when equal
    (when (and (> (aref cnt 3) 0)
               (= (aref nr 3) (aref nr 1)) (= (aref nc 3) (aref nc 1)))
      (incf (aref cnt 1)))
    (setf (aref cnt 3)
          (+ (* 2 (+ (if (and (> mby 0) (= (aref modes (+ (* (1- mby) cols) mbx)) +splitmv+)) 1 0)
                     (if (and (> mbx 0) (= (aref modes (+ (* mby cols) (1- mbx))) +splitmv+)) 1 0)))
             (if (and (> mbx 0) (> mby 0) (= (aref modes (+ (* (1- mby) cols) (1- mbx))) +splitmv+)) 1 0)))
    (when (> (aref cnt 2) (aref cnt 1))
      (rotatef (aref cnt 1) (aref cnt 2))
      (rotatef (aref nr 1) (aref nr 2))
      (rotatef (aref nc 1) (aref nc 2)))
    (when (>= (aref cnt 1) (aref cnt 0))
      (setf (aref nr 0) (aref nr 1) (aref nc 0) (aref nc 1)))
    (values)))

(defun left-block-mv (vd mi mbx mby k splitr splitc)
  "MV (values row col) of the subblock left of subblock K in the MB being decoded."
  (declare (type fixnum mi mbx mby k) (type fxvec splitr splitc))
  (cond
    ((/= 0 (logand k 3)) (values (aref splitr (1- k)) (aref splitc (1- k))))
    ((= mbx 0) (values 0 0))
    (t (let* ((li (1- mi)))
         (if (= (aref (vd-mb-ymode vd) li) +splitmv+)
             (values (aref (vd-mb-split-r vd) (+ (* 16 li) k 3))
                     (aref (vd-mb-split-c vd) (+ (* 16 li) k 3)))
             (values (aref (vd-mb-mvr vd) li) (aref (vd-mb-mvc vd) li)))))
    ))

(defun above-block-mv (vd mi mbx mby k splitr splitc)
  (declare (type fixnum mi mbx mby k) (type fxvec splitr splitc) (ignorable mbx))
  (cond
    ((>= k 4) (values (aref splitr (- k 4)) (aref splitc (- k 4))))
    ((= mby 0) (values 0 0))
    (t (let ((ai (- mi (vd-mb-cols vd))))
         (if (= (aref (vd-mb-ymode vd) ai) +splitmv+)
             (values (aref (vd-mb-split-r vd) (+ (* 16 ai) k 12))
                     (aref (vd-mb-split-c vd) (+ (* 16 ai) k 12)))
             (values (aref (vd-mb-mvr vd) ai) (aref (vd-mb-mvc vd) ai)))))))

(defun decode-split-mv (vd bd mi mbx mby best-r best-c)
  "Read a SPLITMV macroblock's partitioning and its per-subblock vectors into
   VD's split-r/split-c scratch.  Returns the partition id."
  (let* ((splitr (vd-split-r vd)) (splitc (vd-split-c vd))
         (part-id (treed-read bd +split-mv-tree+ +split-mv-probs+ 0))
         (nparts (aref +mv-partition-count+ part-id))
         (mvp (vd-mv-probs vd)))
    (declare (type fxvec splitr splitc) (type fixnum part-id nparts))
    (dotimes (j nparts)
      ;; first subblock of partition j
      (let ((k (loop for i from 0 below 16 when (= (aref +mv-partitions+ part-id i) j) do (return i))))
        (multiple-value-bind (lr lc) (left-block-mv vd mi mbx mby k splitr splitc)
          (multiple-value-bind (ar ac) (above-block-mv vd mi mbx mby k splitr splitc)
            (let* ((lez (and (= lr 0) (= lc 0)))
                   (aez (and (= ar 0) (= ac 0)))
                   (lea (and (= lr ar) (= lc ac)))
                   (ctx (cond ((and lea lez) 4) (lea 3) (aez 2) (lez 1) (t 0)))
                   (sub-mode (let ((i 0))
                               (declare (type fixnum i))
                               (loop
                                 (setf i (aref +submv-ref-tree+
                                               (+ i (bool-bit bd (aref +submv-ref-probs+ ctx (ash i -1))))))
                                 (when (<= i 0) (return (- i))))))
                   (r 0) (c 0))
              (declare (type fixnum r c))
              (ecase sub-mode
                (0 (setf r lr c lc))                          ; LEFT4X4
                (1 (setf r ar c ac))                          ; ABOVE4X4
                (2 (setf r 0 c 0))                            ; ZERO4X4
                (3 (setf r (+ best-r (read-mv-component bd mvp 0))
                         c (+ best-c (read-mv-component bd mvp 19)))))
              (loop for i from k below 16
                    when (= (aref +mv-partitions+ part-id i) j)
                      do (setf (aref splitr i) r (aref splitc i) c)))))))
    part-id))

(defun read-inter-modes (vd bd mi mbx mby)
  "Read the reference frame, prediction mode and vector(s) for an inter MB.
   Stores them into the per-MB arrays.  Returns the y mode."
  (let* ((ref (if (= 1 (bool-bit bd (vd-prob-last vd)))
                  (+ 2 (bool-bit bd (vd-prob-gf vd)))
                  +last-frame+))
         (nr (vd-near-r vd)) (nc (vd-near-c vd)) (cnt (vd-cnt vd))
         (to-left (- (ash (1+ mbx) 7))) (to-right (ash (- (vd-mb-cols vd) mbx) 7))
         (to-top (- (ash (1+ mby) 7))) (to-bottom (ash (- (vd-mb-rows vd) mby) 7)))
    (declare (type fixnum ref to-left to-right to-top to-bottom))
    (find-near-mvs vd mbx mby ref)
    (let* ((mode (let ((i 0))
                   (declare (type fixnum i))
                   (loop
                     (setf i (aref +mv-ref-tree+
                                   (+ i (bool-bit bd (aref +mode-contexts+ (aref cnt (ash i -1)) (ash i -1))))))
                     (when (<= i 0) (return (- i))))))
           (mvr 0) (mvc 0))
      (declare (type fixnum mode mvr mvc))
      (flet ((clamped (i) (values (clamp-mv (aref nr i) to-top to-bottom)
                                  (clamp-mv (aref nc i) to-left to-right))))
        (ecase mode
          (#.+nearestmv+ (multiple-value-setq (mvr mvc) (clamped 1)))
          (#.+nearmv+ (multiple-value-setq (mvr mvc) (clamped 2)))
          (#.+zeromv+)
          (#.+newmv+
           (multiple-value-bind (br bc) (clamped 0)
             (setf mvr (+ br (read-mv-component bd (vd-mv-probs vd) 0))
                   mvc (+ bc (read-mv-component bd (vd-mv-probs vd) 19)))))
          (#.+splitmv+
           (multiple-value-bind (br bc) (clamped 0)
             (decode-split-mv vd bd mi mbx mby br bc)
             (let ((sr (vd-split-r vd)) (sc (vd-split-c vd)))
               (replace (vd-mb-split-r vd) sr :start1 (* 16 mi))
               (replace (vd-mb-split-c vd) sc :start1 (* 16 mi))
               (setf mvr (aref sr 15) mvc (aref sc 15)))))))
      (setf (aref (vd-mb-ref vd) mi) ref
            (aref (vd-mb-ymode vd) mi) mode
            (aref (vd-mb-uvmode vd) mi) mode
            (aref (vd-mb-mvr vd) mi) mvr
            (aref (vd-mb-mvc vd) mi) mvc)
      mode)))

(defun read-intra-modes-inter-frame (vd bd)
  "Intra MB inside an inter frame: modes with transmitted probabilities and no
   subblock context.  Fills D's bmodes for B_PRED.  Returns (values ymode uvmode)."
  (let* ((d (vd-d vd))
         (ymode (treed-read bd +y-mode-tree+ (vd-ymode-probs vd) 0)))
    (when (= ymode +b-pred+)
      (let ((bm (d-bmodes d)))
        (dotimes (i 16)
          (setf (aref bm i) (treed-read bd +bmode-tree+ +default-b-mode-probs+ 0)))))
    (values ymode (treed-read bd +uv-mode-tree+ (vd-uv-mode-probs vd) 0))))

;;; ---- motion compensation (RFC 6386 s18) ---------------------------------------------

(defun mc-filter (ref rstride rbase w h fx fy filters dst dstride dbase tmp)
  "Predict a WxH block from REF (octets, top-left sample at RBASE, row stride
   RSTRIDE) with eighth-pel fractions FX/FY into the fixnum raster DST at
   DBASE.  Two-pass separable filter, each pass rounded and clamped to 0..255."
  (declare (type u8vec ref) (type fxvec dst tmp) (type (simple-array (signed-byte 32) (8 6)) filters)
           (type dim rstride rbase dstride dbase) (type (integer 0 64) w h)
           (type (integer 0 7) fx fy)
           (optimize (speed 3) (safety 0)))
  (cond
    ((and (zerop fx) (zerop fy))
     (dotimes (r h)
       (let ((s (+ rbase (* r rstride))) (o (+ dbase (* r dstride))))
         (declare (type fixnum s o))
         (dotimes (c w) (setf (aref dst (+ o c)) (aref ref (+ s c)))))))
    ((zerop fy)                                    ; horizontal only
     (let ((f0 (aref filters fx 0)) (f1 (aref filters fx 1)) (f2 (aref filters fx 2))
           (f3 (aref filters fx 3)) (f4 (aref filters fx 4)) (f5 (aref filters fx 5)))
       (declare (type (signed-byte 16) f0 f1 f2 f3 f4 f5))
       (dotimes (r h)
         (let ((s (+ rbase (* r rstride))) (o (+ dbase (* r dstride))))
           (declare (type fixnum s o))
           (dotimes (c w)
             (let ((p (+ s c)))
               (declare (type fixnum p))
               (setf (aref dst (+ o c))
                     (max 0 (min 255 (ash (+ 64 (* f0 (aref ref (- p 2))) (* f1 (aref ref (- p 1)))
                                             (* f2 (aref ref p)) (* f3 (aref ref (+ p 1)))
                                             (* f4 (aref ref (+ p 2))) (* f5 (aref ref (+ p 3))))
                                          -7))))))))))
    ((zerop fx)                                    ; vertical only
     (let ((f0 (aref filters fy 0)) (f1 (aref filters fy 1)) (f2 (aref filters fy 2))
           (f3 (aref filters fy 3)) (f4 (aref filters fy 4)) (f5 (aref filters fy 5))
           (s2 (* 2 rstride)) (s3 (* 3 rstride)))
       (declare (type (signed-byte 16) f0 f1 f2 f3 f4 f5) (type dim s2 s3))
       (dotimes (r h)
         (let ((s (+ rbase (* r rstride))) (o (+ dbase (* r dstride))))
           (declare (type fixnum s o))
           (dotimes (c w)
             (let ((p (+ s c)))
               (declare (type fixnum p))
               (setf (aref dst (+ o c))
                     (max 0 (min 255 (ash (+ 64 (* f0 (aref ref (- p s2))) (* f1 (aref ref (- p rstride)))
                                             (* f2 (aref ref p)) (* f3 (aref ref (+ p rstride)))
                                             (* f4 (aref ref (+ p s2))) (* f5 (aref ref (+ p s3))))
                                          -7))))))))))
    (t                                             ; both: horizontal into TMP (h+5 rows), then vertical
     (let ((h0 (aref filters fx 0)) (h1 (aref filters fx 1)) (h2 (aref filters fx 2))
           (h3 (aref filters fx 3)) (h4 (aref filters fx 4)) (h5 (aref filters fx 5))
           (v0 (aref filters fy 0)) (v1 (aref filters fy 1)) (v2 (aref filters fy 2))
           (v3 (aref filters fy 3)) (v4 (aref filters fy 4)) (v5 (aref filters fy 5)))
       (declare (type fixnum h0 h1 h2 h3 h4 h5 v0 v1 v2 v3 v4 v5))
       (dotimes (r (+ h 5))
         (let ((s (+ rbase (* (- r 2) rstride))) (o (* r w)))
           (declare (type fixnum s o))
           (dotimes (c w)
             (let ((p (+ s c)))
               (declare (type fixnum p))
               (setf (aref tmp (+ o c))
                     (max 0 (min 255 (ash (+ 64 (* h0 (aref ref (- p 2))) (* h1 (aref ref (- p 1)))
                                             (* h2 (aref ref p)) (* h3 (aref ref (+ p 1)))
                                             (* h4 (aref ref (+ p 2))) (* h5 (aref ref (+ p 3))))
                                          -7))))))))
       (let ((w2 (* 2 w)) (w3 (* 3 w)))
         (declare (type fixnum w2 w3))
         (dotimes (r h)
           (let ((q (* (+ r 2) w)) (o (+ dbase (* r dstride))))
             (declare (type fixnum q o))
             (dotimes (c w)
               (let ((k (+ q c)))
                 (declare (type fixnum k))
                 (setf (aref dst (+ o c))
                       (max 0 (min 255 (ash (+ 64 (* v0 (aref tmp (- k w2))) (* v1 (aref tmp (- k w)))
                                               (* v2 (aref tmp k)) (* v3 (aref tmp (+ k w)))
                                               (* v4 (aref tmp (+ k w2))) (* v5 (aref tmp (+ k w3))))
                                            -7))))))))))))
  (values))

(defun predict-inter-block (vd ref rstride rrows border bx by w h mvr mvc pl)
  "Motion-compensate the WxH block whose top-left visible pixel is (BX,BY)
   from reference plane REF (octets, stride RSTRIDE, RROWS rows, BORDER) with
   eighth-pel vector (MVR,MVC), writing into plane PL."
  (declare (type u8vec ref) (type fixnum rstride rrows border bx by w h mvr mvc)
           (type plane pl) (optimize (speed 3) (safety 0)))
  (let* ((fx (logand mvc 7)) (fy (logand mvr 7))
         (rx (+ bx (ash mvc -3) border))        ; bordered coordinates of the block
         (ry (+ by (ash mvr -3) border))
         (dst (pl-data pl)) (dstride (pl-stride pl))
         (dbase (+ (* (1+ by) dstride) (1+ bx)))
         (tmp (vd-mc-tmp vd)))
    (declare (type fixnum fx fy rx ry dstride dbase))
    (cond
      ((and (>= (- rx 2) 0) (<= (+ rx w 3) rstride) (>= (- ry 2) 0) (<= (+ ry h 3) rrows))
       (mc-filter ref rstride (+ (* ry rstride) rx) w h fx fy (vd-filters vd) dst dstride dbase tmp))
      (t
       ;; the vector points beyond the border: build the (w+5)x(h+5) source with
       ;; coordinates clamped to the bordered buffer, which replicates the edge
       (let* ((sw (+ w 5)) (scratch (vd-mc-scratch vd)) (maxx (1- rstride)) (maxy (1- rrows)))
         (declare (type u8vec scratch) (type fixnum sw maxx maxy))
         (dotimes (r (+ h 5))
           (let ((sy (max 0 (min maxy (+ ry r -2)))))
             (declare (type fixnum sy))
             (dotimes (c sw)
               (let ((sx (max 0 (min maxx (+ rx c -2)))))
                 (declare (type fixnum sx))
                 (setf (aref scratch (+ (* r sw) c)) (aref ref (+ (* sy rstride) sx)))))))
         (mc-filter scratch sw (+ (* 2 sw) 2) w h fx fy (vd-filters vd) dst dstride dbase tmp))))
    (values)))

(declaim (inline uv-half uv-avg4))
(defun uv-half (x)
  "Chroma component of a whole-MB luma vector: halve, rounding away from zero."
  (declare (type fixnum x))
  (if (>= x 0) (ash (1+ x) -1) (- (ash (1+ (- x)) -1))))
(defun uv-avg4 (s)
  "Chroma component from the sum S of four split luma vectors."
  (declare (type fixnum s))
  (if (>= s 0) (ash (+ s 4) -3) (- (ash (+ (- s) 4) -3))))

(defun predict-inter-mb (vd mi mbx mby mode)
  "Write the inter prediction for the MB into the current frame's planes."
  (declare (type fixnum mi mbx mby mode))
  (let* ((d (vd-d vd))
         (ref (ecase (aref (vd-mb-ref vd) mi)
                (#.+last-frame+ (vd-last vd))
                (#.+golden-frame+ (vd-golden vd))
                (#.+altref-frame+ (vd-altref vd))))
         (yp (d-yplane d)) (up (d-uplane d)) (vp (d-vplane d))
         (mx (* mbx 16)) (my (* mby 16)) (cx (* mbx 8)) (cy (* mby 8))
         (full (vd-full-pixel vd)))
    (unless ref (%err "inter frame references a frame that was never decoded"))
    (let ((ys (rf-ystride ref)) (yr (rf-yrows ref)) (cs (rf-uvstride ref)) (cr (rf-uvrows ref))
          (ry (rf-y ref)) (ru (rf-u ref)) (rv (rf-v ref)))
      (cond
        ((/= mode +splitmv+)
         (let* ((mvr (aref (vd-mb-mvr vd) mi)) (mvc (aref (vd-mb-mvc vd) mi))
                (ur (uv-half mvr)) (uc (uv-half mvc)))
           (when full (setf ur (logand ur -8) uc (logand uc -8)))
           (predict-inter-block vd ry ys yr +border+ mx my 16 16 mvr mvc yp)
           (predict-inter-block vd ru cs cr +cborder+ cx cy 8 8 ur uc up)
           (predict-inter-block vd rv cs cr +cborder+ cx cy 8 8 ur uc vp)))
        (t
         (let ((sr (vd-mb-split-r vd)) (sc (vd-mb-split-c vd)) (base (* 16 mi)))
           (dotimes (b 16)
             (let ((bx (+ mx (* 4 (logand b 3)))) (by (+ my (* 4 (ash b -2)))))
               (predict-inter-block vd ry ys yr +border+ bx by 4 4
                                    (aref sr (+ base b)) (aref sc (+ base b)) yp)))
           ;; chroma: one vector per 2x2 group of luma subblocks
           (dotimes (b 4)
             (let* ((k (+ (* 8 (ash b -1)) (* 2 (logand b 1))))   ; top-left luma subblock: 0 2 8 10
                    (r (uv-avg4 (+ (aref sr (+ base k)) (aref sr (+ base k 1))
                                   (aref sr (+ base k 4)) (aref sr (+ base k 5)))))
                    (c (uv-avg4 (+ (aref sc (+ base k)) (aref sc (+ base k 1))
                                   (aref sc (+ base k 4)) (aref sc (+ base k 5)))))
                    (bx (+ cx (* 4 (logand b 1)))) (by (+ cy (* 4 (ash b -1)))))
               (when full (setf r (logand r -8) c (logand c -8)))
               (predict-inter-block vd ru cs cr +cborder+ bx by 4 4 r c up)
               (predict-inter-block vd rv cs cr +cborder+ bx by 4 4 r c vp)))))))
    (values)))

(defun add-inter-residual (d mx my cx cy)
  "Add the decoded residual of every block of an inter MB onto its prediction."
  (let ((yc (d-ycoeffs d)) (yp (d-yplane d)))
    (dotimes (b 16)
      (let ((blk (aref yc b)))
        (declare (type (simple-array (signed-byte 32) (16)) blk))
        (unless (every #'zerop blk)
          (add-residual yp (+ mx (* 4 (logand b 3))) (+ my (* 4 (ash b -2))) (vp8-idct blk)))))
    (dolist (spec (list (cons (d-uplane d) (d-ublocks d)) (cons (d-vplane d) (d-vblocks d))))
      (let ((pl (car spec)) (blocks (cdr spec)))
        (dotimes (b 4)
          (let ((blk (aref blocks b)))
            (declare (type (simple-array (signed-byte 32) (16)) blk))
            (unless (every #'zerop blk)
              (add-residual pl (+ cx (* 4 (logand b 1))) (+ cy (* 4 (ash b -1))) (vp8-idct blk)))))))))

;;; ---- macroblock loop ----------------------------------------------------------------

(defun decode-macroblocks (vd part0 tokens nparts key)
  (let* ((d (vd-d vd)) (cols (vd-mb-cols vd)) (rows (vd-mb-rows vd))
         (segs (vd-mb-segment vd)))
    (fill (d-above-y d) 0) (fill (d-above-u d) 0) (fill (d-above-v d) 0)
    (fill (d-above-y2 d) 0) (fill (d-above-bmode d) 0)
    (dotimes (mby rows)
      (let ((bd (aref tokens (mod mby nparts))))
        (fill (d-left-y d) 0) (fill (d-left-u d) 0) (fill (d-left-v d) 0)
        (setf (d-left-y2 d) 0)
        (fill (d-left-bmode d) 0)
        (dotimes (mbx cols)
          (let* ((mi (+ (* mby cols) mbx))
                 (mx (* mbx 16)) (my (* mby 16)))
            ;; segment id: read when the map is updated, else persistent (0 on key frames)
            (cond ((and (d-seg-enabled d) (d-seg-update-map d))
                   (setf (aref segs mi) (treed-read part0 +mb-segment-tree+ (d-seg-tree-probs d) 0)))
                  (key (setf (aref segs mi) 0)))
            (let* ((seg (if (d-seg-enabled d) (aref segs mi) 0))
                   (skip (and (d-mb-no-skip d) (= 1 (bool-bit part0 (d-prob-skip d)))))
                   (ymode 0) (uvmode 0) (inter nil))
              (declare (type fixnum ymode uvmode))
              (cond
                (key
                 (multiple-value-setq (ymode uvmode) (read-mb-modes d part0 mbx))
                 (setf (aref (vd-mb-ref vd) mi) +intra-frame+
                       (aref (vd-mb-mvr vd) mi) 0 (aref (vd-mb-mvc vd) mi) 0
                       (aref (vd-mb-ymode vd) mi) ymode (aref (vd-mb-uvmode vd) mi) uvmode))
                ((= 1 (bool-bit part0 (vd-prob-intra vd)))
                 (setf inter t
                       ymode (read-inter-modes vd part0 mi mbx mby)))
                (t
                 (multiple-value-setq (ymode uvmode) (read-intra-modes-inter-frame vd part0))
                 (setf (aref (vd-mb-ref vd) mi) +intra-frame+
                       (aref (vd-mb-mvr vd) mi) 0 (aref (vd-mb-mvc vd) mi) 0
                       (aref (vd-mb-ymode vd) mi) ymode (aref (vd-mb-uvmode vd) mi) uvmode)))
              (let* ((has-y2 (and (/= ymode +b-pred+) (/= ymode +splitmv+)))
                     (dq (aref (d-seg-dq d) seg))
                     (nz (decode-residue d bd mbx skip has-y2 dq)))
                (cond
                  (inter
                   (predict-inter-mb vd mi mbx mby ymode)
                   (unless skip
                     (when has-y2 (scatter-y2 d))
                     (add-inter-residual d mx my (* mbx 8) (* mby 8))))
                  (t
                   (let ((has-above (> mby 0)) (has-left (> mbx 0)))
                     (when (and has-y2 (not skip)) (scatter-y2 d))
                     (when (and has-y2 skip)
                       (dotimes (idx 16) (setf (aref (aref (d-ycoeffs d) idx) 0) 0)))
                     (if (= ymode +b-pred+)
                         (reconstruct-bpred d mx my)
                         (reconstruct-luma16 d mx my ymode has-above has-left))
                     (reconstruct-chroma d mbx mby uvmode has-above has-left))))
                ;; records for the loop filter
                (setf (aref (d-mb-i4x4 d) mi) (or (= ymode +b-pred+) (= ymode +splitmv+))
                      (aref (d-mb-nonzero d) mi) nz
                      (aref (d-mb-seg d) mi) seg
                      (aref (vd-mb-skip vd) mi) skip)))))
        (pad-right (d-yplane d) (* mby 16) 16)))))

;;; ---- loop filter with per-MB deltas (RFC 6386 s15, s9.4) --------------------------

(defun mb-filter-params (vd d mi key)
  "Return (values level interior-limit hev-threshold) for the MB at MI."
  (let* ((seg (aref (d-mb-seg d) mi))
         (ymode (aref (vd-mb-ymode vd) mi))
         (ref (aref (vd-mb-ref vd) mi))
         (level (d-filter-level d)))
    (declare (type fixnum seg ymode ref level))
    (when (d-seg-enabled d)
      (setf level (if (d-seg-abs d)
                      (aref (d-seg-filter d) seg)
                      (+ level (aref (d-seg-filter d) seg)))))
    (setf level (max 0 (min 63 level)))
    (when (d-lf-delta-enabled d)
      (incf level (aref (d-ref-lf-delta d) ref))
      (cond ((= ref +intra-frame+)
             (when (= ymode +b-pred+) (incf level (aref (d-mode-lf-delta d) 0))))
            ((= ymode +zeromv+) (incf level (aref (d-mode-lf-delta d) 1)))
            ((= ymode +splitmv+) (incf level (aref (d-mode-lf-delta d) 3)))
            (t (incf level (aref (d-mode-lf-delta d) 2))))
      (setf level (max 0 (min 63 level))))
    (if (zerop level)
        (values 0 0 0)
        (let ((ilim level) (sharp (d-sharpness d)) (hthr 0))
          (declare (type fixnum ilim sharp hthr))
          (when (> sharp 0)
            (setf ilim (ash ilim (if (> sharp 4) -2 -1)))
            (when (> ilim (- 9 sharp)) (setf ilim (- 9 sharp))))
          (when (< ilim 1) (setf ilim 1))
          (when (>= level 15) (incf hthr))
          (when (>= level 40) (incf hthr))
          (when (and (>= level 20) (not key)) (incf hthr))
          (values level ilim hthr)))))

(defun loop-filter-frame (vd key)
  (let ((d (vd-d vd)))
    (when (zerop (d-filter-level d)) (return-from loop-filter-frame))
    (let ((cols (vd-mb-cols vd)) (rows (vd-mb-rows vd)) (simple (d-filter-simple d)))
      (dotimes (mby rows)
        (dotimes (mbx cols)
          (let ((mi (+ (* mby cols) mbx)))
            (multiple-value-bind (level ilim hthr) (mb-filter-params vd d mi key)
              (when (> level 0)
                (let* ((mbelim (+ (* (+ level 2) 2) ilim))
                       (subelim (+ (* level 2) ilim))
                       (do-inner (or (aref (d-mb-i4x4 d) mi) (aref (d-mb-nonzero d) mi))))
                  (filter-plane-edges (d-yplane d) mbx mby t simple hthr ilim mbelim subelim do-inner)
                  (unless simple
                    (filter-plane-edges (d-uplane d) mbx mby nil nil hthr ilim mbelim subelim do-inner)
                    (filter-plane-edges (d-vplane d) mbx mby nil nil hthr ilim mbelim subelim do-inner)))))))))))

;;; ---- frame decode -------------------------------------------------------------------

(defun frame-info (bytes &key (start 0) (end (length bytes)))
  "Peek at a frame without decoding it: (values key-frame-p show-p width height version)."
  (multiple-value-bind (key version show part0 w h) (parse-frame-tag bytes start end)
    (declare (ignore part0))
    (values key show w h version)))

(defun decode-frame (vd bytes &key (start 0) (end (length bytes)) timestamp)
  "Decode one compressed VP8 frame.  Returns (values picture shown-p): PICTURE
   is the newly reconstructed frame (or, for a zero-length 'dropped' frame,
   the previous output) and SHOWN-P is false for frames the stream hides
   (altref updates).  The picture's planes are valid until the frame after
   the next one is decoded.

   BYTES may be any octet vector.  A demuxer hands over SUBSEQs, which are simple arrays and go
   straight through; the ENCODER in this same system hands back an adjustable buffer with a fill
   pointer, and refusing that would make `encode then decode' — the most obvious thing to try —
   a type error instead of a round trip."
  (unless (typep bytes '(simple-array (unsigned-byte 8) (*)))
    (setf bytes (coerce (subseq bytes start end) '(simple-array (unsigned-byte 8) (*)))
          start 0 end (length bytes)))
  (locally (declare (type u8vec bytes))
  (when (= start end)
    ;; a dropped frame: repeat the last output
    (return-from decode-frame (values (vd-last-output vd) (and (vd-last-output vd) t))))
  (multiple-value-bind (key version show part0-size width height data-start)
      (parse-frame-tag bytes start end)
    (cond
      (key
       (when (or (null (vd-d vd)) (/= width (vd-width vd)) (/= height (vd-height vd)))
         (vd-init-dimensions vd width height))
       (setf (vd-version vd) version
             (vd-filters vd) (if (zerop version) +sixtap-filters+ +bilinear-filters+)
             (vd-full-pixel vd) (= version 3)))
      ((null (vd-d vd)) (%err "inter frame before the first key frame")))
    (setf (vd-key-frame vd) key)
    (let* ((d (vd-d vd))
           (part0-end (+ data-start part0-size)))
      (when (> part0-end end) (%err "truncated first partition"))
      (let* ((part0 (bool-init bytes data-start part0-end))
             (nparts (read-header vd part0 key))
             (tbl part0-end)
             (cur (+ tbl (* 3 (1- nparts))))
             (tokens (make-array nparts)))
        (when (> cur end) (%err "truncated partition table"))
        (dotimes (i nparts)
          (let ((sz (if (< i (1- nparts)) (u24le bytes (+ tbl (* 3 i))) (- end cur))))
            (when (or (< sz 0) (> (+ cur sz) end)) (%err "truncated token partition ~d" i))
            (setf (aref tokens i) (bool-init bytes cur (+ cur sz)))
            (incf cur sz)))
        (decode-macroblocks vd part0 tokens nparts key)
        (loop-filter-frame vd key)
        ;; the finished frame becomes a reference buffer
        (let ((rf (vd-take-frame vd)))
          (plane->rframe-plane (d-yplane d) (rf-y rf) (rf-ystride rf) +border+ (rf-aw rf) (rf-ah rf))
          (plane->rframe-plane (d-uplane d) (rf-u rf) (rf-uvstride rf) +cborder+ (ash (rf-aw rf) -1) (ash (rf-ah rf) -1))
          (plane->rframe-plane (d-vplane d) (rf-v rf) (rf-uvstride rf) +cborder+ (ash (rf-aw rf) -1) (ash (rf-ah rf) -1))
          (setf (vd-current vd) rf)
          ;; entropy restore
          (unless (vd-refresh-entropy vd)
            (replace (d-coeff-probs d) (vd-saved-coeff-probs vd))
            (replace (vd-mv-probs vd) (vd-saved-mv-probs vd))
            (replace (vd-ymode-probs vd) (vd-saved-ymode-probs vd))
            (replace (vd-uv-mode-probs vd) (vd-saved-uv-mode-probs vd)))
          ;; reference updates, in the reference decoder's order
          (let ((old-last (vd-last vd)) (old-golden (vd-golden vd)) (old-altref (vd-altref vd)))
            (case (vd-copy-altref vd)
              (1 (setf (vd-altref vd) old-last))
              (2 (setf (vd-altref vd) old-golden)))
            (case (vd-copy-golden vd)
              (1 (setf (vd-golden vd) old-last))
              (2 (setf (vd-golden vd) old-altref)))
            (when (vd-refresh-golden vd) (setf (vd-golden vd) rf))
            (when (vd-refresh-altref vd) (setf (vd-altref vd) rf))
            (when (vd-refresh-last vd) (setf (vd-last vd) rf)))
          (incf (vd-frame-count vd))
          (let ((pic (rframe->picture rf (vd-width vd) (vd-height vd))))
            (setf (picture-timestamp pic) timestamp)
            (when show
              (incf (vd-shown-count vd))
              (setf (vd-last-output vd) rf))
            (values pic show))))))))

;;; ---- the readers a caller outside this package uses ------------------------------------------

(defun decoder-width (d) (vd-width d))
(defun decoder-height (d) (vd-height d))
(defun decoder-frame-count (d) (vd-frame-count d))

;;;; vp9/headers.lisp — the superframe index, and the uncompressed frame header.
;;;;
;;;; TWO THINGS HAPPEN BEFORE ANY PICTURE IS DECODED, and both are peculiar to VP9.
;;;;
;;;; A container packet may hold SEVERAL frames.  VP9 codes hidden reference frames — an alt-ref is
;;;; a frame the encoder builds from the future and never displays — and a container has no place to
;;;; put a frame that produces no picture, so the encoder glues them onto the front of the next
;;;; visible frame and appends an INDEX at the very end of the packet saying where each began.  A
;;;; decoder that does not look for that index decodes the first frame of the group and throws away
;;;; the rest, which for a typical encode is most of the reference structure.
;;;;
;;;; And the frame header itself is in PLAIN BITS, not in the arithmetic coder: the frame size, the
;;;; reference indices, the quantiser, the loop filter and the tiling are all readable without any
;;;; decoding state at all.  That is why a VP9 stream can be rewritten, spliced or routed by
;;;; something that has no decoder in it.

(in-package #:reel.vp9)

(defconstant +sync-code+ #x498342 "The three bytes that follow a key frame's profile.")

;;; ---- superframes -----------------------------------------------------------------------------

(defun split-superframe (bytes &key (start 0) (end (length bytes)))
  "The frames in one container packet, as a list of (start . end).

   The index is at the END of the packet and is bracketed by two identical marker bytes, which is
   how a decoder tells an index from picture data that happens to end the right way: read the last
   byte, work out how long the index would be, and check that the byte that far back is the same.
   A packet with no index is one frame."
  (declare (type octets bytes) (type fixnum start end))
  (let ((size (- end start)))
    (declare (type fixnum size))
    (when (<= size 0) (return-from split-superframe '()))
    (let ((marker (aref bytes (1- end))))
      (declare (type fixnum marker))
      (when (= (logand marker #xe0) #xc0)
        (let* ((len-size (1+ (logand (ash marker -3) 3)))
               (n (1+ (logand marker 7)))
               (idx-size (+ 2 (* n len-size))))
          (declare (type fixnum len-size n idx-size))
          (when (and (>= size idx-size) (= (aref bytes (- end idx-size)) marker))
            (let ((at (+ (- end idx-size) 1)) (o start) (out '()) (total 0))
              (declare (type fixnum at o total))
              (dotimes (i n)
                (let ((len 0))
                  (declare (type fixnum len))
                  (dotimes (k len-size)
                    (setf len (logior len (ash (aref bytes (+ at (* i len-size) k)) (* 8 k)))))
                  (incf total len)
                  (when (or (<= len 0) (> total (- size idx-size)))
                    (%err "a superframe index claiming ~d bytes of ~d" total (- size idx-size)))
                  (push (cons o (+ o len)) out)
                  (incf o len)))
              (return-from split-superframe (nreverse out))))))
      (list (cons start end)))))

;;; ---- the frame header --------------------------------------------------------------------------

(defstruct (header (:conc-name h-))
  (profile 0 :type fixnum)
  (show-existing nil)                           ; index of a reference to display, or NIL
  (keyframe nil) (show-frame nil) (error-resilient nil)
  (intra-only nil)
  (reset-context 0 :type fixnum)
  (width 0 :type fixnum) (height 0 :type fixnum)
  (render-width 0 :type fixnum) (render-height 0 :type fixnum)
  (bpp 8 :type fixnum)
  (subsampling-x 1 :type fixnum) (subsampling-y 1 :type fixnum)
  (colorspace 0 :type fixnum) (full-range nil)
  (refresh-mask 0 :type fixnum)                 ; which of the eight reference slots this updates
  (ref-idx (make-array 3 :element-type '(signed-byte 32) :initial-element 0) :type (simple-array (signed-byte 32) (3)))
  (sign-bias (make-array 3 :element-type '(signed-byte 32) :initial-element 0)
             :type (simple-array (signed-byte 32) (3)))
  (high-precision-mv nil)
  (filter-mode 0 :type fixnum)                  ; 0..2 a fixed filter, 3 switchable per block
  (allow-comp-inter nil)
  (fix-comp-ref 0 :type fixnum)
  (var-comp-ref (make-array 2 :element-type '(signed-byte 32) :initial-element 0)
                :type (simple-array (signed-byte 32) (2)))
  (refresh-context nil) (parallel-mode nil) (frame-context 0 :type fixnum)
  ;; loop filter
  (filter-level 0 :type fixnum) (sharpness 0 :type fixnum)
  (lf-delta-enabled nil) (lf-delta-updated nil)
  (lf-ref-delta (make-array 4 :element-type '(signed-byte 32) :initial-contents '(1 0 -1 -1))
                :type (simple-array (signed-byte 32) (4)))
  (lf-mode-delta (make-array 2 :element-type '(signed-byte 32) :initial-element 0)
                 :type (simple-array (signed-byte 32) (2)))
  ;; quantiser
  (base-q 0 :type fixnum)
  (ydc-delta 0 :type fixnum) (uvdc-delta 0 :type fixnum) (uvac-delta 0 :type fixnum)
  (lossless nil)
  ;; segmentation
  (seg-enabled nil) (seg-update-map nil) (seg-temporal nil) (seg-abs nil)
  (seg-tree-probs (make-array 7 :element-type '(unsigned-byte 8) :initial-element 255)
                  :type (simple-array (unsigned-byte 8) (7)))
  (seg-pred-probs (make-array 3 :element-type '(unsigned-byte 8) :initial-element 255)
                  :type (simple-array (unsigned-byte 8) (3)))
  (seg-feature (make-array '(8 4) :element-type '(signed-byte 32) :initial-element 0)
               :type (simple-array (signed-byte 32) (8 4)))     ; q, loop filter, reference, skip
  (seg-feature-on (make-array '(8 4) :element-type '(signed-byte 32) :initial-element 0)
                  :type (simple-array (signed-byte 32) (8 4)))
  ;; tiling and the compressed header's size
  (log2-tile-cols 0 :type fixnum) (log2-tile-rows 0 :type fixnum)
  (compressed-size 0 :type fixnum)
  (header-bytes 0 :type fixnum))                ; where the compressed header begins

(defun %read-colour (br h)
  "Colour space and, for the profiles that allow it, the chroma subsampling.

   Profile 0 is 4:2:0 eight bit and says none of this; the others send the subsampling here, which
   is the only place a decoder learns that a stream is 4:2:2 or 4:4:4."
  (when (>= (h-profile h) 2) (setf (h-bpp h) (if (plusp (read-bit br)) 12 10)))
  (setf (h-colorspace h) (read-bits br 3))
  (if (= (h-colorspace h) 7)                    ; sRGB
      (progn
        (setf (h-full-range h) t (h-subsampling-x h) 0 (h-subsampling-y h) 0)
        (when (member (h-profile h) '(1 3)) (read-bit br))   ; reserved
        (%err "an sRGB VP9 stream is 4:4:4, which is not supported (4:2:0 only)"))
      (progn
        (setf (h-full-range h) (plusp (read-bit br)))
        (if (member (h-profile h) '(1 3))
            (progn (setf (h-subsampling-x h) (read-bit br)
                         (h-subsampling-y h) (read-bit br))
                   (read-bit br))               ; reserved
            (setf (h-subsampling-x h) 1 (h-subsampling-y h) 1)))))

(defun %read-loop-filter (br h)
  (setf (h-filter-level h) (read-bits br 6)
        (h-sharpness h) (read-bits br 3))
  (setf (h-lf-delta-enabled h) (plusp (read-bit br)))
  (when (h-lf-delta-enabled h)
    (setf (h-lf-delta-updated h) (plusp (read-bit br)))
    (when (h-lf-delta-updated h)
      (dotimes (i 4)
        (when (plusp (read-bit br)) (setf (aref (h-lf-ref-delta h) i) (read-signed br 6))))
      (dotimes (i 2)
        (when (plusp (read-bit br)) (setf (aref (h-lf-mode-delta h) i) (read-signed br 6)))))))

(defun %read-quant (br h)
  (setf (h-base-q h) (read-bits br 8)
        (h-ydc-delta h) (if (plusp (read-bit br)) (read-signed br 4) 0)
        (h-uvdc-delta h) (if (plusp (read-bit br)) (read-signed br 4) 0)
        (h-uvac-delta h) (if (plusp (read-bit br)) (read-signed br 4) 0))
  ;; LOSSLESS IS NOT A FLAG.  It is the quantiser index being zero with every delta zero, and it
  ;; changes the transform: a lossless picture uses a Walsh-Hadamard and nothing else.
  (setf (h-lossless h) (and (zerop (h-base-q h)) (zerop (h-ydc-delta h))
                            (zerop (h-uvdc-delta h)) (zerop (h-uvac-delta h)))))

(defun %read-segmentation (br h)
  (setf (h-seg-enabled h) (plusp (read-bit br)))
  (unless (h-seg-enabled h)
    (setf (h-seg-temporal h) nil (h-seg-update-map h) nil)
    (return-from %read-segmentation))
  (setf (h-seg-update-map h) (plusp (read-bit br)))
  (when (h-seg-update-map h)
    (dotimes (i 7)
      (setf (aref (h-seg-tree-probs h) i) (if (plusp (read-bit br)) (read-bits br 8) 255)))
    (setf (h-seg-temporal h) (plusp (read-bit br)))
    (when (h-seg-temporal h)
      (dotimes (i 3)
        (setf (aref (h-seg-pred-probs h) i) (if (plusp (read-bit br)) (read-bits br 8) 255)))))
  (when (plusp (read-bit br))
    (setf (h-seg-abs h) (plusp (read-bit br)))
    (dotimes (i 8)
      ;; the four features in order: quantiser, loop filter level, reference frame, and skip —
      ;; and the last is a flag with no value, which is why the widths below are not uniform
      (loop for (k width signed) in '((0 8 t) (1 6 t) (2 2 nil) (3 0 nil))
            do (let ((on (plusp (read-bit br))))
                 (setf (aref (h-seg-feature-on h) i k) (if on 1 0))
                 (when (and on (plusp width))
                   (setf (aref (h-seg-feature h) i k)
                         (if signed (read-signed br width) (read-bits br width)))))))))

(defun %read-tiling (br h sb-cols)
  "How many tile columns and rows the frame is cut into (6.2.14).

   The column count is coded as `keep going' bits between a minimum implied by the frame width and a
   maximum implied by the same, which is why it needs the frame size to have been read first."
  (let ((min 0) (max 0))
    (declare (type fixnum min max))
    (loop while (> sb-cols (ash 64 min)) do (incf min))
    (loop while (>= (ash sb-cols (- max)) 4) do (incf max))
    (setf max (max 0 (1- max)))
    (let ((cols min))
      (declare (type fixnum cols))
      (loop while (> max cols)
            do (if (plusp (read-bit br)) (incf cols) (return)))
      (setf (h-log2-tile-cols h) cols))
    (setf (h-log2-tile-rows h)
          (let ((v (read-bit br))) (if (zerop v) 0 (+ 1 (read-bit br)))))))

(defun parse-header (bytes start end &key ref-sizes)
  "The uncompressed header of one frame.  REF-SIZES is a vector of eight (width . height) or NIL,
   because an inter frame may say `the same size as reference two' rather than send one."
  (declare (type octets bytes) (type fixnum start end))
  (let ((br (make-br bytes :start start :end end))
        (h (make-header)))
    (unless (= 2 (read-bits br 2)) (%err "a frame that does not begin with VP9's marker"))
    (let ((p (logior (read-bit br) (ash (read-bit br) 1))))
      (when (= p 3) (incf p (read-bit br)))
      (setf (h-profile h) p))
    (when (plusp (h-profile h))
      (%err "VP9 profile ~d is not supported (profile 0 only: eight bits, 4:2:0)" (h-profile h)))
    ;; a frame that codes nothing and simply shows a reference again, which is how an alt-ref
    ;; becomes visible without being re-sent
    (when (plusp (read-bit br))
      (setf (h-show-existing h) (read-bits br 3))
      (return-from parse-header h))
    ;; BOTH OF THESE ARE INVERTED IN THE BITSTREAM and only one of them looks it: a zero says key
    ;; frame, and a one says shown.  Reading the second the same way as the first costs nothing on a
    ;; visible frame and desynchronises on the next one, because whether a frame is shown decides
    ;; whether an intra-only bit follows.
    (setf (h-keyframe h) (zerop (read-bit br))
          (h-show-frame h) (plusp (read-bit br))
          (h-error-resilient h) (plusp (read-bit br)))
    (flet ((sync () (unless (= +sync-code+ (read-bits br 24))
                      (%err "a frame whose sync code is wrong")))
           (size ()
             (setf (h-width h) (1+ (read-bits br 16))
                   (h-height h) (1+ (read-bits br 16)))
             (if (plusp (read-bit br))
                 (setf (h-render-width h) (1+ (read-bits br 16))
                       (h-render-height h) (1+ (read-bits br 16)))
                 (setf (h-render-width h) (h-width h) (h-render-height h) (h-height h)))))
      (cond
        ((h-keyframe h)
         (sync) (%read-colour br h)
         (setf (h-refresh-mask h) #xff)
         (size))
        (t
         (setf (h-intra-only h) (and (not (h-show-frame h)) (plusp (read-bit br))))
         (setf (h-reset-context h) (if (h-error-resilient h) 0 (read-bits br 2)))
         (cond
           ((h-intra-only h)
            (sync)
            (if (>= (h-profile h) 1)
                (%read-colour br h)
                (setf (h-bpp h) 8 (h-subsampling-x h) 1 (h-subsampling-y h) 1))
            (setf (h-refresh-mask h) (read-bits br 8))
            (size))
           (t
            (setf (h-refresh-mask h) (read-bits br 8))
            (dotimes (i 3)
              (setf (aref (h-ref-idx h) i) (read-bits br 3)
                    (aref (h-sign-bias h) i)
                    (if (and (plusp (read-bit br)) (not (h-error-resilient h))) 1 0)))
            ;; THE SIZE MAY BE INHERITED, one reference at a time, and the three bits are read in
            ;; order until one says yes — so a decoder that skips them because it already knows the
            ;; size loses the bitstream, not merely the size
            (let ((got nil))
              (dotimes (i 3)
                (unless got
                  (when (plusp (read-bit br))
                    (setf got t)
                    (let ((s (and ref-sizes (aref ref-sizes (aref (h-ref-idx h) i)))))
                      (unless s (%err "a frame sized from a reference that is not there"))
                      (setf (h-width h) (car s) (h-height h) (cdr s))))))
              (if got
                  (if (plusp (read-bit br))
                      (setf (h-render-width h) (1+ (read-bits br 16))
                            (h-render-height h) (1+ (read-bits br 16)))
                      (setf (h-render-width h) (h-width h) (h-render-height h) (h-height h)))
                  (size)))
            (setf (h-high-precision-mv h) (plusp (read-bit br)))
            (setf (h-filter-mode h) (if (plusp (read-bit br)) 3 (read-bits br 2)))
            ;; two references may be combined only if they point in different directions in time,
            ;; which is what the sign biases say
            (let ((sb (h-sign-bias h)))
              (setf (h-allow-comp-inter h)
                    (or (/= (aref sb 0) (aref sb 1)) (/= (aref sb 0) (aref sb 2))))
              (when (h-allow-comp-inter h)
                (cond ((= (aref sb 0) (aref sb 1))
                       (setf (h-fix-comp-ref h) 2
                             (aref (h-var-comp-ref h) 0) 0 (aref (h-var-comp-ref h) 1) 1))
                      ((= (aref sb 0) (aref sb 2))
                       (setf (h-fix-comp-ref h) 1
                             (aref (h-var-comp-ref h) 0) 0 (aref (h-var-comp-ref h) 1) 2))
                      (t (setf (h-fix-comp-ref h) 0
                               (aref (h-var-comp-ref h) 0) 1
                               (aref (h-var-comp-ref h) 1) 2)))))))))
      (setf (h-refresh-context h) (and (not (h-error-resilient h)) (plusp (read-bit br)))
            (h-parallel-mode h) (or (h-error-resilient h) (plusp (read-bit br)))
            (h-frame-context h) (read-bits br 2))
      (when (or (h-keyframe h) (h-intra-only h)) (setf (h-frame-context h) 0))
      (%read-loop-filter br h)
      (%read-quant br h)
      (%read-segmentation br h)
      (%read-tiling br h (ceiling (h-width h) 64))
      (setf (h-compressed-size h) (read-bits br 16))
      (when (zerop (h-compressed-size h)) (%err "a frame with an empty compressed header"))
      ;; The uncompressed header is padded to a byte, and the compressed header begins there.
      ;; RELATIVE TO THE FRAME, not to the buffer: a frame inside a superframe does not start at
      ;; zero, and a decoder that adds the buffer offset twice reads a later frame's data as this
      ;; one's compressed header — which is exactly what the two-frame superframe test caught.
      (setf (h-header-bytes h) (ash (+ (- (br-pos br) (* 8 start)) 7) -3))
      (when (> (+ (h-header-bytes h) (h-compressed-size h)) (- end start))
        (%err "a compressed header of ~d bytes past the end of a ~d byte frame"
              (h-compressed-size h) (- end start)))
      h)))

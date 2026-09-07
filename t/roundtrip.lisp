;;;; t/roundtrip.lisp — the codec against itself, with nothing else in the image.
;;;;
;;;; reel is the one system here that depends on nothing, so its test should need nothing either:
;;;; no ffmpeg, no container, no framebuffer.  It encodes frames with the encoder and decodes them
;;;; with the decoder, and asserts on the pixels that come back.
;;;;
;;;; THIS IS NOT A CONFORMANCE TEST AND MUST NOT BE MISTAKEN FOR ONE.  Two halves of one codebase
;;;; agreeing proves they agree; it does not prove either is VP8.  What proves that is ffmpeg, and
;;;; those tests live downstream where ffmpeg is already a fixture: cassette's inspect/test-decode
;;;; asserts the decoder is bit-exact with libvpx over eight clips, and inspect/test-encode-mux
;;;; asserts ffmpeg decodes what this encoder produces.  What THIS catches is the thing those
;;;; cannot: a change that breaks encoder and decoder in the same direction, and a reel that has
;;;; quietly stopped loading on its own.
;;;;
;;;;   sbcl --dynamic-space-size 2048 --non-interactive --load t/roundtrip.lisp

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :reel)))

(defpackage #:reel-test (:use #:cl)) (in-package #:reel-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defparameter *w* 96) (defparameter *h* 64)

(defun gray-ramp (w h &optional (phase 0))
  "A plane with structure in both axes, so a decode that loses a macroblock shows up."
  (let ((v (make-array (* w h) :element-type '(unsigned-byte 8))))
    (dotimes (y h v)
      (dotimes (x w)
        (setf (aref v (+ (* y w) x))
              (logand 255 (+ 30 phase (* 11 (logand (ash x -3) 7)) (* 5 (logand (ash y -3) 7)))))))))

(defun flat (w h value)
  (make-array (* w h) :element-type '(unsigned-byte 8) :initial-element value))

(defun mean-abs-diff (a b)
  (let ((n (min (length a) (length b))) (s 0))
    (dotimes (i n (/ (float s) (max 1 n)))
      (incf s (abs (- (aref a i) (aref b i)))))))

(defun decode-one (bytes &optional (dec (reel:make-decoder)))
  "Decode one frame and return (values luma-plane picture decoder)."
  (multiple-value-bind (pic shown) (reel:decode-frame dec bytes)
    (declare (ignore shown))
    (let* ((w (reel:picture-width pic)) (h (reel:picture-height pic))
           (y (make-array (* w h) :element-type '(unsigned-byte 8)))
           (src (reel:picture-y pic)) (stride (reel:picture-y-stride pic))
           (off (reel:picture-y-offset pic)))
      (dotimes (r h) (replace y src :start1 (* r w) :start2 (+ off (* r stride))
                                    :end2 (+ off (* r stride) w)))
      (values y pic dec))))

(format t "~&== a key frame the decoder agrees with~%")
(let* ((src (gray-ramp *w* *h*))
       (u (flat (ceiling *w* 2) (ceiling *h* 2) 128))
       (v (flat (ceiling *w* 2) (ceiling *h* 2) 128))
       (bytes (reel:encode-gray-frame src *w* *h* :qi 8 :u u :v v)))
  (ok "the encoder produced a frame" (and bytes (plusp (length bytes))))
  (multiple-value-bind (key show w h version) (reel:frame-info bytes)
    (ok "frame-info reads it as a shown key frame" (and key show))
    (ok (format nil "frame-info reads ~ax~a version ~a" w h version)
        (and (= w *w*) (= h *h*) (= version 0))))
  (multiple-value-bind (y pic) (decode-one bytes)
    (ok "the picture is the size that went in"
        (and (= (reel:picture-width pic) *w*) (= (reel:picture-height pic) *h*)))
    (let ((d (mean-abs-diff src y)))
      ;; lossy at qi 8, but a whole-frame mean error above a couple of levels means something
      ;; structural came back wrong rather than merely quantized
      (ok (format nil "decoded luma is within a quantizer of the source (mean |diff| ~,2f)" d)
          (< d 3.0)))))

(format t "~&== a solid frame — see README, this helper is known broken~%")
(let* ((bytes (reel:encode-keyframe-solid *w* *h* 200 128 128 :qi 0)))
  (multiple-value-bind (y) (decode-one bytes)
    ;; ENCODE-KEYFRAME-SOLID does not produce a solid frame, at any quantizer.  ffmpeg decodes it
    ;; to the same wrong pixels we do, so the defect is the encoder's and our decoder is not
    ;; flattering it.  Asserted as a KNOWN state rather than deleted, so that fixing the helper
    ;; makes this line fail and say so instead of passing silently.
    (ok "known: the solid-frame helper is not solid (encoder defect, ffmpeg agrees)"
        (> (length (remove-duplicates (coerce (subseq y 0 *w*) 'list))) 1))))

(format t "~&== inter frames against a reference~%")
(let* ((enc nil) (dec (reel:make-decoder)) (frames '()))
  ;; the key frame, and the encoder's reference primed from its own reconstruction
  (let* ((src (gray-ramp *w* *h*))
         (u (flat (ceiling *w* 2) (ceiling *h* 2) 128)) (v (flat (ceiling *w* 2) (ceiling *h* 2) 128)))
    (multiple-value-bind (bytes ry ru rv) (reel:encode-gray-frame src *w* *h* :qi 8 :u u :v v)
      (setf enc (reel:make-encoder *w* *h*))
      (setf (reel:ve-ref-y enc) ry (reel:ve-ref-u enc) ru (reel:ve-ref-v enc) rv)
      (replace (reel:ve-prev-y enc) src) (replace (reel:ve-prev-u enc) u) (replace (reel:ve-prev-v enc) v)
      (setf (reel:ve-have-ref enc) t)
      (push bytes frames)))
  ;; three inter frames over a scene that shifts, which is what ZEROMV cannot carry alone
  (dotimes (i 3)
    (let* ((src (gray-ramp *w* *h* (* 8 (1+ i))))
           (u (flat (ceiling *w* 2) (ceiling *h* 2) 128)) (v (flat (ceiling *w* 2) (ceiling *h* 2) 128)))
      (push (reel:encode-inter-frame enc src u v :qi 8) frames)))
  (setf frames (nreverse frames))
  (ok "four frames encoded" (= (length frames) 4))
  (ok "only the first is a key frame"
      (and (reel:frame-info (first frames))
           (notany #'reel:frame-info (rest frames))))
  (let ((sizes (mapcar #'length frames)))
    (ok (format nil "inter frames are smaller than the key frame ~a" sizes)
        (every (lambda (n) (< n (first sizes))) (rest sizes))))
  ;; decode the sequence and check the last frame still resembles what was encoded
  (let ((y nil))
    (dolist (f frames) (setf y (decode-one f dec)))
    (let ((d (mean-abs-diff (gray-ramp *w* *h* 24) y)))
      (ok (format nil "after three inter frames the picture still tracks the source (mean |diff| ~,2f)" d)
          (< d 4.0)))
    (ok "the decoder counted four frames" (= (reel:decoder-frame-count dec) 4))))

(format t "~&== the bool coder round-trips~%")
(let* ((probs '(128 200 30 255 1 90 170))
       (bits (loop repeat 400 collect (random 2)))
       (bw (reel:make-bwriter)))
  (loop for b in bits for i from 0 do (reel:bwrite-bit bw (nth (mod i (length probs)) probs) b))
  (let* ((bytes (reel:bwrite-finish bw))
         (br (reel:make-breader* bytes)))
    (ok "400 bits came back in order"
        (loop for b in bits for i from 0
              always (= b (reel:bread-bit br (nth (mod i (length probs)) probs)))))))

(format t "~&~a~%" (if (zerop *fails*) "REEL OK" (format nil "REEL: ~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))

;;;; t/conformance.lisp — the decoders against the formats' own conformance suites.
;;;;
;;;; Every fixture in this repository and in cassette's is something ffmpeg, x264 or libvpx
;;;; produced, and a mainstream encoder uses a narrow slice of the standard it implements.  Passing
;;;; those is a weaker claim than it looks.  This runs the official suites instead.
;;;;
;;;; TWO THINGS ABOUT THE ORACLE, both learned the hard way here.  Generate the reference decodes
;;;; with `-fps_mode passthrough' or ffmpeg duplicates frames to a constant rate and every
;;;; comparison after the first repeat is nonsense.  And do not compute a frame count as
;;;; size / (width * height * 3/2): VP9 changes picture size from frame to frame, so a reference
;;;; decode is not a stack of equal-sized pictures.  Walk it picture by picture, using each
;;;; picture's own length, which is what the code below does.
;;;;
;;;;   t/fetch-conformance.sh && sbcl --dynamic-space-size 8192 \
;;;;     --non-interactive --load t/conformance.lisp
(require :asdf)
(push (truename "../cassette/") asdf:*central-registry*)
(asdf:load-asd (merge-pathnames "reel.asd" (or *load-truename* *default-pathname-defaults*)))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :cassette))

(defpackage #:reel-conformance (:use #:cl)) (in-package #:reel-conformance)

(defvar *conf* (or (sb-posix:getenv "CONF") "/tmp/conf"))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun compare-pictures (pictures oracle)
  "PICTURES is a list of I420 vectors in output order.  Returns (values exact total first-bad)."
  (let ((n 0) (exact 0) (off 0) (bad nil))
    (dolist (y pictures (values exact n bad))
      (incf n)
      (cond ((> (+ off (length y)) (length oracle))
             (unless bad (setf bad "output runs past the reference")))
            ((loop for i below (length y) always (= (aref y i) (aref oracle (+ off i))))
             (incf exact))
            (t (unless bad (setf bad (format nil "picture ~d" n)))))
      (incf off (length y)))))

;;; ---- VP8: the seventeen official libvpx vectors, read straight out of IVF -------------------------

(defun ivf-frames (bytes)
  (let ((p (logior (aref bytes 6) (ash (aref bytes 7) 8))) (out '()))
    (loop while (<= (+ p 12) (length bytes))
          do (let ((sz (logior (aref bytes p) (ash (aref bytes (+ p 1)) 8)
                               (ash (aref bytes (+ p 2)) 16) (ash (aref bytes (+ p 3)) 24))))
               (when (> (+ p 12 sz) (length bytes)) (return))
               (push (subseq bytes (+ p 12) (+ p 12 sz)) out)
               (incf p (+ 12 sz))))
    (nreverse out)))

(defun run-vp8 ()
  (let ((pass 0) (fail 0))
    (dolist (f (sort (directory (format nil "~a/vp8/*.ivf" *conf*)) #'string< :key #'namestring))
      (let ((ref (probe-file (format nil "~a.ref.yuv" (namestring f)))))
        (handler-case
            (let ((d (reel.decode:make-decoder)) (pics '()))
              (dolist (pk (ivf-frames (slurp f)))
                (multiple-value-bind (pic shown) (reel.decode:decode-frame d pk)
                  (when (and shown pic) (push (reel.decode:picture->yuv420 pic) pics))))
              (multiple-value-bind (exact n bad) (compare-pictures (nreverse pics)
                                                                  (if ref (slurp ref) #()))
                (if (and (plusp n) (= exact n) (null bad))
                    (progn (incf pass) (format t "~&  ok   ~a: ~d frames~%" (pathname-name f) n))
                    (progn (incf fail)
                           (format t "~&  FAIL ~a: ~d of ~d~@[, first bad: ~a~]~%"
                                   (pathname-name f) exact n bad)))))
          (error (e) (incf fail) (format t "~&  FAIL ~a: ~a~%" (pathname-name f) e)))))
    (format t "~&VP8: ~d passed, ~d failed~%" pass fail)
    fail))

;;; ---- VP9: the official feature vectors, through the container ------------------------------------

(defun run-vp9 ()
  (let ((pass 0) (fail 0) (refused 0))
    (dolist (f (sort (directory (format nil "~a/vp9/*.webm" *conf*)) #'string< :key #'namestring))
      (let ((ref (probe-file (format nil "~a.ref.yuv" (namestring f)))))
        (handler-case
            (let ((p (cassette:open-media f :audio nil)) (pics '()))
              (loop for pic = (cassette:next-video-frame p) while pic
                    do (push (cassette:picture->yuv420 pic) pics))
              (multiple-value-bind (exact n bad) (compare-pictures (nreverse pics)
                                                                  (if ref (slurp ref) #()))
                (if (and (plusp n) (= exact n) (null bad))
                    (progn (incf pass) (format t "~&  ok   ~a: ~d pictures~%" (pathname-name f) n))
                    (progn (incf fail)
                           (format t "~&  FAIL ~a: ~d of ~d~@[, first bad: ~a~]~%"
                                   (pathname-name f) exact n bad)))))
          ;; profiles 1, 2 and 3 are refused by name and that is the right answer, not a failure
          (reel.vp9::vp9-error (e) (incf refused)
            (format t "~&  --   ~a: refused — ~a~%" (pathname-name f) e))
          (error (e) (incf fail) (format t "~&  FAIL ~a: ~a~%" (pathname-name f) e)))))
    (format t "~&VP9: ~d passed, ~d failed, ~d correctly refused~%" pass fail refused)
    fail))

;;; ---- H.264: a spread of the JVT conformance streams ----------------------------------------------

(defun run-h264 ()
  (let ((pass 0) (fail 0) (refused 0))
    (dolist (f (sort (append (directory (format nil "~a/h264/*.264" *conf*))
                             (directory (format nil "~a/h264/*.jsv" *conf*)))
                     #'string< :key #'namestring))
      (let ((ref (probe-file (format nil "~a.ref.yuv" (namestring f)))))
        (handler-case
            (let ((pics (mapcar #'reel.h264:picture->yuv420
                                (reel.h264:decode-annex-b (slurp f)))))
              (multiple-value-bind (exact n bad) (compare-pictures pics (if ref (slurp ref) #()))
                (if (and (plusp n) (= exact n) (null bad))
                    (progn (incf pass) (format t "~&  ok   ~a: ~d pictures~%" (file-namestring f) n))
                    (progn (incf fail)
                           (format t "~&  FAIL ~a: ~d of ~d~@[, first bad: ~a~]~%"
                                   (file-namestring f) exact n bad)))))
          (reel.h264::h264-error (e) (incf refused)
            (format t "~&  --   ~a: refused — ~a~%" (file-namestring f) e))
          (error (e) (incf fail) (format t "~&  FAIL ~a: ~a~%" (file-namestring f) e)))))
    (format t "~&H.264: ~d passed, ~d failed, ~d refused~%" pass fail refused)
    fail))

(let ((bad 0))
  (format t "~&== VP8, the official libvpx vectors~%")   (incf bad (run-vp8))
  (format t "~&== VP9, the official feature vectors~%")  (incf bad (run-vp9))
  (format t "~&== H.264, a spread of the JVT streams~%") (incf bad (run-h264))
  (format t "~&~:[CONFORMANCE OK~;CONFORMANCE: ~:*~d streams decode incorrectly~]~%"
          (if (plusp bad) bad nil)))

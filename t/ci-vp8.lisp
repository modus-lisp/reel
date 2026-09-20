;;;; t/ci-vp8.lisp — the cheap tier of the decoder's own oracle, for CI.
;;;;
;;;; t/conformance.lisp is the real thing: the official suites for four codecs, against
;;;; ffmpeg-produced reference decodes.  It needs the cassette and reed repositories beside
;;;; this one and it downloads film excerpts, so it is not something every push can run.
;;;;
;;;; This runs ONE official libvpx vector and writes each decoded picture to its own file so
;;;; the caller can MD5 them against the vector's own .md5 sidecar.  That is a weaker claim
;;;; than conformance — one stream, not seventeen — but it is the exact check that caught two
;;;; COMPILER miscompiles in September 2026 that nothing else saw: a unary-minus rewrite that
;;;; zeroed negative coefficients, and a commutative-swap in accumulate-into-register that
;;;; evaluated a call into the accumulator's own register.  Both produced a decoder that still
;;;; ran and still produced pictures.  A checksum is what tells those apart from correct ones.
;;;;
;;;; Per-picture files rather than one concatenated blob because a VP9 stream may change
;;;; picture size from frame to frame, so "frame N is at N * w * h * 3/2" is not a fact about
;;;; video in general -- see the note at the head of t/conformance.lisp.
;;;;
;;;;   VEC=/tmp/v.ivf OUT=/tmp/frames sbcl --non-interactive --load t/ci-vp8.lisp

(require :asdf)
(let* ((here (directory-namestring (or *load-truename* *default-pathname-defaults*)))
       ;; the .asd is in the REPOSITORY ROOT; this file lives in t/, so try the parent
       ;; first and fall back to here for anyone who runs it from the root
       (asd (or (probe-file (merge-pathnames "../reel.asd" here))
                (probe-file (merge-pathnames "reel.asd" here))
                (error "reel.asd not found relative to ~a" here))))
  (asdf:load-asd asd))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :reel))

(defpackage #:reel-ci (:use #:cl)) (in-package #:reel-ci)

(defvar *vec* (or (sb-ext:posix-getenv "VEC") (error "set VEC to an .ivf vector")))
(defvar *out* (or (sb-ext:posix-getenv "OUT") "/tmp/frames"))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun le32 (v o)
  (logior (aref v o) (ash (aref v (+ o 1)) 8) (ash (aref v (+ o 2)) 16) (ash (aref v (+ o 3)) 24)))

;;; IVF: a 32-byte file header, then per frame a 12-byte header whose first u32 is the size.
(let ((ivf (slurp *vec*)) (off 32) (n 0))
  (ensure-directories-exist (format nil "~a/" *out*))
  (let ((vd (reel:make-decoder)))
    (loop
      (when (>= (+ off 12) (length ivf)) (return))
      (let* ((sz (le32 ivf off)) (s (+ off 12)) (e (+ s sz))
             (pic (reel:decode-frame vd ivf :start s :end e)))
        (when pic
          (with-open-file (o (format nil "~a/f~5,'0d.i420" *out* n)
                             :direction :output :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
            (write-sequence (reel.decode::picture->yuv420 pic) o))
          (incf n))
        (setq off e))))
  (format t "~&CI-VP8 decoded ~d pictures to ~a~%" n *out*)
  (when (zerop n) (sb-ext:quit :unix-status 1)))

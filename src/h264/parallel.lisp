;;;; h264/parallel.lisp — decoding independent pictures at the same time.
;;;;
;;;; WHAT MAKES THIS LEGAL, and the reason it is a separate file rather than a flag: an I picture
;;;; is decodable entirely on its own.  It predicts only from itself, and the loop filter reads
;;;; only its own samples.  Two of them therefore have nothing to say to each other, and can be
;;;; decoded on two cores with no ordering and no locking — the parallelism is in the DATA, not in
;;;; anything clever done here.
;;;;
;;;; WHAT MAKES IT ILLEGAL is the moment a picture predicts from another one.  A P slice reads its
;;;; reference picture's samples, so frame-level parallelism stops being available the day inter
;;;; prediction lands; the honest thing then is slice-level or wavefront parallelism inside a
;;;; picture, which is a different piece of work.  So this checks rather than assumes: every access
;;;; unit in the batch must be an I slice, and if any is not, the caller gets NIL and decodes the
;;;; ordinary way.  Nothing here silently produces a wrong picture.
;;;;
;;;; The parameter sets are the only shared state, and they are read-only for the whole batch: they
;;;; are parsed serially before any worker starts, and a slice decode never writes them.
;;;;
;;;; Threads come from SBCL itself, so reel still depends on nothing.

(in-package #:reel.h264)

(defconstant +i-slice-type+ 2)
(defconstant +i-slice-type-all+ 7
  "slice_type 7 is 2 plus 5: the same I slice, asserting that every slice in the picture is one.")

(defun %slice-first-mb-and-type (nal)
  "first_mb_in_slice and slice_type of a slice NAL, read directly rather than by parsing the whole
   header — which cannot be done yet, because it needs the picture parameter set this is deciding
   how to route.  They are the first two Exp-Golomb codes of the slice header (7.3.3)."
  (handler-case
      (let ((br (make-bitreader (nal-rbsp nal))))
        (let* ((first-mb (ue br))
               (slice-type (ue br)))
          (values first-mb slice-type)))
    (error () (values nil nil))))

(defun %i-slice-type-p (slice-type)
  (and slice-type (or (= slice-type +i-slice-type+) (= slice-type +i-slice-type-all+))))

(defun split-access-units (nals)
  "Split a NAL stream into (values parameter-nals access-units).

   An access unit is one picture's worth of slice NALs.  A slice whose first_mb_in_slice is 0
   starts a new picture, which is the test the specification gives and is right for a stream that
   splits a picture across several slices as well as the usual one-slice-per-picture."
  (let ((params '()) (aus '()) (current '()))
    (dolist (n nals)
      (cond
        ((nal-slice-p n)
         (multiple-value-bind (first-mb type) (%slice-first-mb-and-type n)
           (declare (ignore type))
           (when (and (eql first-mb 0) current)
             (push (nreverse current) aus)
             (setf current '()))
           (push n current)))
        (t (push n params))))
    (when current (push (nreverse current) aus))
    (values (nreverse params) (nreverse aus))))

(defun %parameter-nal-p (n)
  (or (= (nal-type n) +nal-sps+) (= (nal-type n) +nal-pps+)))

(defun access-units-independent-p (aus)
  "Is every access unit here decodable without reference to any other?

   Three things have to hold.  Every access unit must carry at least one slice — an SEI on its own
   decodes to no picture and would silently shorten the batch.  Every slice must be an I slice,
   which is what makes the pictures independent at all.  And none may carry a parameter set: those
   are the one piece of state the workers SHARE, so a picture that redefines an SPS mid-batch would
   have workers writing what other workers are reading.  Such a batch goes down the serial path,
   where the parameter set takes effect in the order the stream intended.

   Anything this cannot read — including a slice header it cannot parse — answers NIL, because the
   safe answer to an unknown is the ordinary decoder."
  (and aus
       (every (lambda (au)
                (and au
                     (notany #'%parameter-nal-p au)
                     (some #'nal-slice-p au)
                     ;; only the slices are asked about; SEI, AUD and the rest decode to nothing
                     (every (lambda (n)
                              (or (not (nal-slice-p n))
                                  (multiple-value-bind (first-mb type) (%slice-first-mb-and-type n)
                                    (declare (ignore first-mb))
                                    (%i-slice-type-p type))))
                            au)))
              aus)))

;;; ---- the worker pool -----------------------------------------------------------------------------

(defun %processor-count ()
  "How many processors this machine has, or NIL if we cannot tell.

   Read from /proc rather than through an alien symbol, because the symbol's name is an SBCL
   internal that has moved between versions, and being wrong about it is how this silently decided
   a 116-core machine had four."
  (ignore-errors
   (with-open-file (s "/proc/cpuinfo" :if-does-not-exist nil)
     (when s
       (let ((n 0))
         (loop for line = (read-line s nil nil)
               while line
               do (when (and (> (length line) 9) (string= "processor" line :end2 9)) (incf n)))
         (and (plusp n) n))))))

(defun default-decode-threads ()
  "How many workers to use when the caller does not say.

   Capped rather than unbounded: past a handful of pictures in flight the gain is memory bandwidth
   bound, and every worker in flight holds a whole decoded picture."
  #+sb-thread (max 1 (min 16 (or (%processor-count) 4)))
  #-sb-thread 1)

(defun %decode-one-au (d nals)
  "Decode one access unit with a private decoder that SHARES this one's parameter sets.

   Sharing is safe because nothing in a slice decode writes them, and it is necessary because the
   picture and the per-slice scratch must not be shared."
  (let ((w (%make-decoder :sps (h264-sps d) :pps (h264-pps d)))
        (out nil))
    (dolist (n nals)
      (let ((p (feed-nal w n)))
        (when p (setf out p))))
    ;; AND THEN FLUSH.  These access units are independently decodable, so decoding order is display
    ;; order and there is nothing to reorder — but the reorder buffer does not know that, and a
    ;; sequence whose level allows a deep buffer will hold the picture rather than hand it over.
    ;; The picture is finished either way; it just has to be asked for.
    (or out (first (flush-decoder w)))))

#+sb-thread
(defun %pmap-vector (fn items nthreads)
  "FN over ITEMS on NTHREADS workers, results in order.

   Work is claimed from a shared counter rather than split up front, because access units differ in
   cost — a busy picture can be several times a quiet one — and a static split would leave workers
   idle waiting for the slowest chunk.

   A condition in a worker is CAUGHT AND KEPT, not signalled where it happens: it would otherwise
   surface on whichever thread lost the race, and the caller would see a different error from run
   to run. They are re-signalled in order once every worker has finished, so a batch fails the same
   way every time and the same way the serial path would."
  (let* ((n (length items))
         (out (make-array n :initial-element nil))
         (errs (make-array n :initial-element nil))
         (next 0)
         (lock (sb-thread:make-mutex :name "reel-h264-batch")))
    (declare (type fixnum n next))
    (flet ((worker ()
             (loop
               (let ((i (sb-thread:with-mutex (lock)
                          (when (< next n) (prog1 next (incf next))))))
                 (unless i (return))
                 (handler-case (setf (aref out i) (funcall fn (aref items i)))
                   (serious-condition (e) (setf (aref errs i) e)))))))
      (let ((threads (loop repeat (max 1 (min nthreads n))
                           collect (sb-thread:make-thread #'worker :name "reel-h264"))))
        (mapc #'sb-thread:join-thread threads)))
    (dotimes (i n)
      (when (aref errs i) (error (aref errs i))))
    out))

(defun decode-independent (d aus &key (threads (default-decode-threads)))
  "Decode a batch of independently decodable access units, returning their pictures IN ORDER.

   Returns NIL — decoding nothing — if the batch is not independently decodable, so a caller can
   use this as the fast path and fall back without having to know why."
  (unless (access-units-independent-p aus) (return-from decode-independent nil))
  (let ((v (coerce aus 'vector)))
    #+sb-thread
    (if (and (> (length v) 1) (> threads 1))
        (coerce (%pmap-vector (lambda (au) (%decode-one-au d au)) v threads) 'list)
        (map 'list (lambda (au) (%decode-one-au d au)) v))
    #-sb-thread
    (map 'list (lambda (au) (%decode-one-au d au)) v)))

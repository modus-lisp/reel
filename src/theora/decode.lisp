;;;; theora/decode.lisp — a Theora frame, from the packet to the picture.
;;;;
;;;; Theora divides a picture into 8x8 FRAGMENTS, groups them four by four into SUPERBLOCKS visited
;;;; along a Hilbert curve, and separately groups them two by two into MACROBLOCKS which carry the
;;;; coding mode and the motion.  Three overlapping partitions of the same picture, each with its own
;;;; traversal order, and every stage of decoding uses a different one.  That is the thing to hold on
;;;; to: superblocks say WHICH fragments are coded, macroblocks say HOW, and the coefficients arrive
;;;; in a fourth order again — all the DC coefficients of the picture, then all the first AC
;;;; coefficients, and so on to the sixty-third.
;;;;
;;;; That last one is worth dwelling on.  Every other codec here codes a block's coefficients
;;;; together.  Theora interleaves them across the whole picture by frequency, so a block's DC and
;;;; its last AC coefficient are thousands of bits apart, and a decoder cannot finish any block until
;;;; it has walked all sixty-four passes.  The pay-off is that each pass has its own statistics and
;;;; its own Huffman table, which is why the setup header carries eighty of them.
;;;;
;;;; THE PICTURE IS STORED UPSIDE DOWN here, as Theora means it: row zero is the bottom row.  Every
;;;; offset below is in those coordinates and the flip happens once, on output.  Doing it the other
;;;; way costs a sign flip on every motion vector and gets it wrong somewhere.

(in-package #:reel.theora)

(defconstant +mode-copy+ 8
  "Not a coding mode the bitstream can send: the decoder's name for a fragment this picture does not
   code at all, which is copied from the previous one.")

(defconstant +border+ 32
  "Padding around each plane, replicated after decoding, so that a motion vector reaching outside
   the picture reads a copy of the edge rather than needing a special case.  Vectors run to
   thirty-one half-pixels, so fifteen whole ones, plus a block and the half-pixel's extra column.")

;;; ---- the picture buffers ------------------------------------------------------------------------

(defstruct (pic (:conc-name pic-))
  (planes (make-array 3) :type simple-vector)
  (stride (make-array 3 :element-type 'fixnum) :type fixnums)
  (org (make-array 3 :element-type 'fixnum) :type fixnums)
  (width (make-array 3 :element-type 'fixnum) :type fixnums)
  (height (make-array 3 :element-type 'fixnum) :type fixnums))

(defun make-pic-for (w h)
  "A picture of a coded size, each plane padded on all four sides."
  (let ((p (make-pic)))
    (dotimes (i 3 p)
      (let* ((pw (if (zerop i) w (ash w -1)))
             (ph (if (zerop i) h (ash h -1)))
             (stride (+ pw (* 2 +border+)))
             (rows (+ ph (* 2 +border+))))
        (setf (aref (pic-planes p) i)
              (make-array (* stride rows) :element-type '(unsigned-byte 8)
                                          :initial-element (if (zerop i) 0 128))
              (aref (pic-stride p) i) stride
              (aref (pic-org p) i) (+ (* +border+ stride) +border+)
              (aref (pic-width p) i) pw
              (aref (pic-height p) i) ph)))))

(defun %extend-borders (p)
  "Replicate each plane's edge outwards.  This is edge emulation done once per picture instead of
   once per block: the specification clamps a motion vector's reads to the coded plane, and a
   replicated border is the same thing said in advance."
  (declare (type pic p) (optimize (speed 3) (safety 1)))
  (dotimes (i 3)
    (let* ((plane (the octets (aref (pic-planes p) i)))
           (stride (aref (pic-stride p) i))
           (org (aref (pic-org p) i))
           (w (aref (pic-width p) i))
           (h (aref (pic-height p) i)))
      (declare (type fixnum stride org w h))
      (dotimes (y h)
        (let ((row (+ org (* y stride))))
          (declare (type fixnum row))
          (fill plane (aref plane row) :start (- row +border+) :end row)
          (fill plane (aref plane (+ row w -1)) :start (+ row w) :end (+ row w +border+))))
      (let ((top (- org +border+)) (bot (+ org (* (1- h) stride) (- +border+))))
        (declare (type fixnum top bot))
        (dotimes (k +border+)
          (replace plane plane :start1 (- top (* (1+ k) stride)) :end1 (- top (* k stride))
                               :start2 top :end2 (+ top stride))
          (replace plane plane :start1 (+ bot (* (1+ k) stride)) :end1 (+ bot (* (1+ k) stride) stride)
                               :start2 bot :end2 (+ bot stride)))))))

;;; ---- the decoder --------------------------------------------------------------------------------

(defstruct (decoder (:conc-name d-) (:constructor %make-decoder))
  info
  ;; geometry, per plane
  (frag-w (make-array 3 :element-type 'fixnum) :type fixnums)
  (frag-h (make-array 3 :element-type 'fixnum) :type fixnums)
  (frag-start (make-array 3 :element-type 'fixnum) :type fixnums)
  (sb-w (make-array 3 :element-type 'fixnum) :type fixnums)
  (sb-h (make-array 3 :element-type 'fixnum) :type fixnums)
  (sb-start (make-array 3 :element-type 'fixnum) :type fixnums)
  (sb-count 0 :type fixnum)
  (frag-count 0 :type fixnum)
  (mb-w 0 :type fixnum) (mb-h 0 :type fixnum) (mb-count 0 :type fixnum)
  (sb-frag (make-array 0 :element-type 'fixnum) :type fixnums)   ; superblock*16 -> fragment or -1
  ;; per-fragment state
  (coding (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (qpi (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (dc (make-array 0 :element-type 'fixnum) :type fixnums)
  (nextzz (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (endzz (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (coeffs (make-array 0 :element-type '(signed-byte 16)) :type (simple-array (signed-byte 16) (*)))
  (mvx (make-array 0 :element-type 'fixnum) :type fixnums)
  (mvy (make-array 0 :element-type 'fixnum) :type fixnums)
  (mb-coding (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (sb-coding (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (coded (make-array 0 :element-type 'fixnum) :type fixnums)
  (coded-start (make-array 3 :element-type 'fixnum) :type fixnums)
  (coded-count (make-array 3 :element-type 'fixnum) :type fixnums)
  (total-coded 0 :type fixnum)
  ;; the fixed Huffman tables
  sb-run frag-run mode-code mv-code
  ;; quantisation: [qpi][inter][plane] -> 64 weights in raster order
  (qmat (make-array '(3 2 3)) :type (simple-array t (3 2 3)))
  (qps (make-array 3 :element-type 'fixnum :initial-element -1) :type fixnums)
  (nqps 1 :type fixnum)
  (bounding (make-array 512 :element-type 'fixnum) :type fixnums)
  (filter-limit -1 :type fixnum)
  ;; frames
  (pool (make-array 3) :type simple-vector)
  cur last golden
  (keyframe nil)
  (frames 0 :type fixnum)
  (block (make-array 64 :element-type 'fixnum) :type fixnums))

(declaim (inline %huff))
(defun %huff (br h what)
  (or (read-huff br h) (%err "a ~a code that matches nothing" what)))

(defun make-theora-decoder (info)
  "Everything that depends only on the headers: the three partitions of the picture, the map from
   superblocks to fragments, and the four Huffman tables Theora does not transmit."
  (let* ((d (%make-decoder :info info))
         (fw (inf-fragment-width info)) (fh (inf-fragment-height info))
         (cfw (ash fw -1)) (cfh (ash fh -1)))
    (setf (aref (d-frag-w d) 0) fw (aref (d-frag-h d) 0) fh
          (aref (d-frag-w d) 1) cfw (aref (d-frag-h d) 1) cfh
          (aref (d-frag-w d) 2) cfw (aref (d-frag-h d) 2) cfh)
    (setf (aref (d-frag-start d) 0) 0
          (aref (d-frag-start d) 1) (* fw fh)
          (aref (d-frag-start d) 2) (+ (* fw fh) (* cfw cfh)))
    (setf (d-frag-count d) (+ (* fw fh) (* 2 cfw cfh)))
    (dotimes (p 3)
      (setf (aref (d-sb-w d) p) (ceiling (aref (d-frag-w d) p) 4)
            (aref (d-sb-h d) p) (ceiling (aref (d-frag-h d) p) 4)))
    (let ((n 0))
      (dotimes (p 3)
        (setf (aref (d-sb-start d) p) n)
        (incf n (* (aref (d-sb-w d) p) (aref (d-sb-h d) p))))
      (setf (d-sb-count d) n))
    (setf (d-mb-w d) (inf-mb-width info) (d-mb-h d) (inf-mb-height info)
          (d-mb-count d) (* (inf-mb-width info) (inf-mb-height info)))
    ;; the superblock-to-fragment map, once: sixteen slots per superblock, -1 where the curve
    ;; wanders off the edge of a plane that is not a whole number of superblocks wide
    (let ((map (make-array (* 16 (d-sb-count d)) :element-type 'fixnum :initial-element -1))
          (j 0))
      (dotimes (p 3)
        (let ((w (aref (d-frag-w d) p)) (h (aref (d-frag-h d) p))
              (start (aref (d-frag-start d) p)))
          (dotimes (sby (aref (d-sb-h d) p))
            (dotimes (sbx (aref (d-sb-w d) p))
              (dotimes (i 16)
                (let ((x (+ (* 4 sbx) (aref +superblock-scan+ i 0)))
                      (y (+ (* 4 sby) (aref +superblock-scan+ i 1))))
                  (setf (aref map j) (if (and (< x w) (< y h)) (+ start (* y w) x) -1))
                  (incf j)))))))
      (setf (d-sb-frag d) map))
    (macrolet ((alloc (slot n type &optional (init 0))
                 `(setf (,slot d) (make-array ,n :element-type ',type :initial-element ,init))))
      (alloc d-coding (d-frag-count d) (unsigned-byte 8) +mode-copy+)
      (alloc d-qpi (d-frag-count d) (unsigned-byte 8))
      (alloc d-dc (d-frag-count d) fixnum)
      (alloc d-nextzz (d-frag-count d) (unsigned-byte 8))
      (alloc d-endzz (d-frag-count d) (unsigned-byte 8))
      (alloc d-coeffs (* 64 (d-frag-count d)) (signed-byte 16))
      (alloc d-mvx (d-frag-count d) fixnum)
      (alloc d-mvy (d-frag-count d) fixnum)
      (alloc d-mb-coding (d-mb-count d) (unsigned-byte 8) +mode-copy+)
      (alloc d-sb-coding (d-sb-count d) (unsigned-byte 8))
      (alloc d-coded (d-frag-count d) fixnum))
    (setf (d-sb-run d) (canonical-huff +superblock-run-lengths+
                                       :values (let ((v (make-array 34)))
                                                 (dotimes (k 34 v) (setf (aref v k) (1+ k)))))
          (d-frag-run d) (canonical-huff +fragment-run-lengths+)
          (d-mode-code d) (canonical-huff +mode-code-lengths+)
          (d-mv-code d) (let ((lens (make-array (length +motion-vector-vlc+)
                                                :element-type '(unsigned-byte 8)))
                              (vals (make-array (length +motion-vector-vlc+))))
                          (loop for (sym len) in +motion-vector-vlc+ for k from 0
                                do (setf (aref lens k) len (aref vals k) (- sym 31)))
                          (canonical-huff lens :values vals)))
    (dotimes (i 3) (setf (aref (d-pool d) i) (make-pic-for (inf-width info) (inf-height info))))
    d))

;;; ---- quantisation -------------------------------------------------------------------------------

(defun %init-dequant (d qpi)
  "The six weight matrices for one of the picture's up to three quantiser indices (6.4.3).

   The DC weight of every matrix is forced to the FIRST quantiser's, whatever this one's index says.
   That is not an optimisation: DC coefficients are predicted from their neighbours, and neighbours
   may carry different quantiser indices, so a per-index DC weight would make the prediction depend
   on which index each neighbour happened to use."
  (let* ((i (d-info d)) (qi (aref (d-qps d) qpi))
         (ac (aref (inf-ac-scale i) qi)) (dc (aref (inf-dc-scale i) qi)))
    (dotimes (inter 2)
      (dotimes (plane 3)
        (let ((base (dequant-matrix i inter plane qi))
              (out (make-array 64 :element-type 'fixnum)))
          (dotimes (k 64)
            (let ((qmin (ash 8 (+ inter (if (zerop k) 1 0))))
                  (qscale (if (zerop k) dc ac)))
              (setf (aref out k)
                    (max qmin (min 4096 (* 4 (floor (* qscale (aref base k)) 100)))))))
          (setf (aref (d-qmat d) qpi inter plane) out))))
    ;; and now the DC, from the first index
    (unless (zerop qpi)
      (dotimes (inter 2)
        (dotimes (plane 3)
          (setf (aref (the fixnums (aref (d-qmat d) qpi inter plane)) 0)
                (aref (the fixnums (aref (d-qmat d) 0 inter plane)) 0)))))))

(defun %init-loop-filter (d limit)
  "The saturating table the loop filter's correction passes through (7.10).

   Below LIMIT a difference passes unchanged; above it the table falls back to zero and then stops.
   So a small step across a block edge is smoothed and a large one — an actual edge in the picture —
   is left alone, which is the whole idea in one array."
  (let ((b (d-bounding d)))
    (fill b 0)
    (macrolet ((bv (x) `(aref b (+ 256 ,x))))
      (loop for x from 0 below limit do (setf (bv (- x)) (- x) (bv x) x))
      (loop for x from limit for value downfrom limit
            while (and (< x 128) (plusp value))
            do (setf (bv x) value (bv (- x)) (- value))))
    (setf (d-filter-limit d) limit)))

;;; ---- which fragments are coded ------------------------------------------------------------------

(defconstant +sb-not-coded+ 0)
(defconstant +sb-partial+ 1)
(defconstant +sb-full+ 2)
(defconstant +long-run+ 4129
  "The longest run a single code can express: thirty-three, plus the twelve-bit escape's four
   thousand and ninety-five.  A run of exactly this length is the one case where the encoder cannot
   tell whether the next run continues the same value, so the next bit is sent explicitly instead of
   being the toggle it otherwise always is.")

(defun %read-sb-run (br d)
  (let ((r (%huff br (d-sb-run d) "superblock run length")))
    (if (= r 34) (+ r (read-bits br 12)) r)))

(defun %unpack-superblocks (br d)
  "Two run-length passes over the superblocks, then one over the fragments inside the partial ones.

   The first pass marks superblocks PARTIALLY coded; the second, over only what the first did not
   claim, marks the rest fully coded or not coded at all.  Both are runs of a toggling bit, which is
   why neither sends the bit: it alternates, except after a run so long it hit the ceiling."
  (let ((sbc (d-sb-coding d)) (bit 0) (run 0) (partial 0))
    (declare (type fixnum bit run partial))
    (if (d-keyframe d)
        (fill sbc +sb-full+)
        (progn
          (setf bit (logxor 1 (read-bit br)))
          (let ((at 0))
            (declare (type fixnum at))
            (loop while (and (< at (d-sb-count d)) (not (br-eof-p br)))
                  do (if (= run +long-run+) (setf bit (read-bit br)) (setf bit (logxor bit 1)))
                     (setf run (%read-sb-run br d))
                     (when (> run (- (d-sb-count d) at))
                       (%err "a run of ~d partially coded superblocks with ~d left"
                             run (- (d-sb-count d) at)))
                     (fill sbc bit :start at :end (+ at run))
                     (incf at run)
                     (when (plusp bit) (incf partial run))))
          (when (< partial (d-sb-count d))
            (let ((done 0) (at 0))
              (declare (type fixnum done at))
              (setf bit (logxor 1 (read-bit br)) run 0)
              (loop while (and (< done (- (d-sb-count d) partial)) (not (br-eof-p br)))
                    do (if (= run +long-run+) (setf bit (read-bit br)) (setf bit (logxor bit 1)))
                       (setf run (%read-sb-run br d))
                       ;; the run counts only superblocks the first pass left alone
                       (let ((j 0))
                         (declare (type fixnum j))
                         (loop while (< j run)
                               do (when (>= at (d-sb-count d))
                                    (%err "a run of fully coded superblocks past the last one"))
                                  (when (= (aref sbc at) +sb-not-coded+)
                                    (setf (aref sbc at) (* 2 bit))
                                    (incf j))
                                  (incf at)))
                       (incf done run))))
          ;; the fragment runs inside the partial superblocks share one toggling bit across the
          ;; whole picture, primed here and toggled by the first run below
          (when (plusp partial)
            (setf run 0 bit (logxor 1 (read-bit br))))))
    ;; walk every superblock of every plane and record what is coded
    (fill (d-mb-coding d) +mode-copy+)
    (setf (d-total-coded d) 0)
    (let ((next 0))
      (declare (type fixnum next))
      (dotimes (plane 3)
        (setf (aref (d-coded-start d) plane) next)
        (let ((start (aref (d-sb-start d) plane))
              (n (* (aref (d-sb-w d) plane) (aref (d-sb-h d) plane)))
              (count 0))
          (declare (type fixnum start n count))
          (dotimes (k n)
            (let ((sb (+ start k)))
              (dotimes (j 16)
                (let ((f (aref (d-sb-frag d) (+ (* 16 sb) j))))
                  (unless (minusp f)
                    (let ((coded (aref (d-sb-coding d) sb)))
                      (when (= coded +sb-partial+)
                        (when (zerop run)
                          (setf bit (logxor bit 1)
                                run (1+ (%huff br (d-frag-run d) "fragment run length"))))
                        (decf run)
                        (setf coded bit))
                      (if (plusp coded)
                          (progn (setf (aref (d-coding d) f) +mode-inter-no-mv+)
                                 (setf (aref (d-coded d) next) f)
                                 (incf next) (incf count))
                          (setf (aref (d-coding d) f) +mode-copy+))))))))
          (setf (aref (d-coded-count d) plane) count)
          (incf (d-total-coded d) count))))))

;;; ---- how each macroblock is coded ---------------------------------------------------------------

(defmacro %do-macroblocks ((d mb-x mb-y mb) &body body)
  "The macroblocks of a picture in the order modes and vectors are sent: superblock by superblock
   over the LUMA superblock grid, and within each, four macroblocks in the same folded order the
   fragment scan uses."
  (let ((sbx (gensym)) (sby (gensym)) (j (gensym)))
    `(dotimes (,sby (aref (d-sb-h ,d) 0))
       (dotimes (,sbx (aref (d-sb-w ,d) 0))
         (dotimes (,j 4)
           (let* ((,mb-x (+ (* 2 ,sbx) (ash ,j -1)))
                  (,mb-y (+ (* 2 ,sby) (logand (+ (ash ,j -1) ,j) 1)))
                  (,mb (+ (* ,mb-y (d-mb-w ,d)) ,mb-x)))
             (declare (type fixnum ,mb-x ,mb-y ,mb))
             (when (and (< ,mb-x (d-mb-w ,d)) (< ,mb-y (d-mb-h ,d)))
               ,@body)))))))

(declaim (inline %luma-frag))
(defun %luma-frag (d mb-x mb-y k)
  (+ (* (+ (* 2 mb-y) (ash k -1)) (aref (d-frag-w d) 0)) (* 2 mb-x) (logand k 1)))

(defun %unpack-modes (br d)
  "The coding mode of every macroblock that codes at least one luma fragment.

   A macroblock with no coded luma fragment sends nothing and is inter with a zero vector, which is
   the rule that makes the mode stream so much shorter than one code per macroblock."
  (if (d-keyframe d)
      (fill (d-coding d) +mode-intra+)
      (let* ((scheme (read-bits br 3))
             (alphabet (make-array 8 :element-type 'fixnum)))
        (cond ((zerop scheme)
               ;; the picture sends its own ordering, as a rank for each mode in turn
               (fill alphabet +mode-inter-no-mv+)
               (dotimes (i 8) (setf (aref alphabet (read-bits br 3)) i)))
              ((= scheme 7))                        ; three raw bits per macroblock, no alphabet
              (t (dotimes (i 8) (setf (aref alphabet i) (aref +mode-alphabets+ (1- scheme) i)))))
        (%do-macroblocks (d mb-x mb-y mb)
          (let ((any nil))
            (dotimes (k 4)
              (unless (= (aref (d-coding d) (%luma-frag d mb-x mb-y k)) +mode-copy+)
                (setf any t) (return)))
            (if (not any)
                (setf (aref (d-mb-coding d) mb) +mode-inter-no-mv+)
                (let ((mode (if (= scheme 7)
                                (read-bits br 3)
                                (aref alphabet (%huff br (d-mode-code d) "macroblock mode")))))
                  (setf (aref (d-mb-coding d) mb) mode)
                  (dotimes (k 4)
                    (let ((f (%luma-frag d mb-x mb-y k)))
                      (unless (= (aref (d-coding d) f) +mode-copy+)
                        (setf (aref (d-coding d) f) mode))))
                  ;; one chroma fragment per macroblock in 4:2:0, in each of the two planes
                  (let ((cf (+ (* mb-y (aref (d-frag-w d) 1)) mb-x)))
                    (dotimes (p 2)
                      (let ((f (+ (aref (d-frag-start d) (1+ p)) cf)))
                        (unless (= (aref (d-coding d) f) +mode-copy+)
                          (setf (aref (d-coding d) f) mode))))))))))))

;;; ---- motion -------------------------------------------------------------------------------------

(declaim (inline %rshift))
(defun %rshift (a b)
  "Round to nearest, away from zero on a tie — which is what averaging four vectors into one wants."
  (declare (type fixnum a b) (optimize (speed 3) (safety 0)))
  (let ((half (ash 1 (1- b))))
    (if (plusp a) (ash (+ a half) (- b)) (ash (+ a half -1) (- b)))))

(defun %read-mv (br d mode)
  (if (zerop mode)
      (%huff br (d-mv-code d) "motion vector")
      (aref +fixed-motion-vectors+ (read-bits br 6))))

(defun %unpack-vectors (br d)
  "The motion vectors, in the same macroblock order as the modes.

   Two of the eight modes send no vector and instead name one already sent — the LAST vector or the
   one before it — so a picture panning uniformly sends a vector once and refers to it thereafter.
   The bookkeeping for those two registers is the fiddly part: which modes advance them, and which
   read without advancing, is not symmetric and not guessable."
  (when (d-keyframe d) (return-from %unpack-vectors))
  (let ((coding-mode (read-bit br))
        (last-x 0) (last-y 0) (prior-x 0) (prior-y 0)
        (mx (make-array 4 :element-type 'fixnum)) (my (make-array 4 :element-type 'fixnum)))
    (declare (type fixnum last-x last-y prior-x prior-y))
    (%do-macroblocks (d mb-x mb-y mb)
      (let ((mode (aref (d-mb-coding d) mb)))
        (unless (= mode +mode-copy+)
          (fill mx 0) (fill my 0)
          (cond
            ((or (= mode +mode-golden-mv+) (= mode +mode-inter-plus-mv+))
             (setf (aref mx 0) (%read-mv br d coding-mode)
                   (aref my 0) (%read-mv br d coding-mode))
             ;; the registers follow the ordinary inter vector only; a golden one does not disturb them
             (when (= mode +mode-inter-plus-mv+)
               (setf prior-x last-x prior-y last-y
                     last-x (aref mx 0) last-y (aref my 0))))
            ((= mode +mode-inter-fourmv+)
             (setf prior-x last-x prior-y last-y)
             (dotimes (k 4)
               (let ((f (%luma-frag d mb-x mb-y k)))
                 (cond ((= (aref (d-coding d) f) +mode-copy+)
                        (setf (aref mx k) 0 (aref my k) 0))
                       (t (setf (aref mx k) (%read-mv br d coding-mode)
                                (aref my k) (%read-mv br d coding-mode)
                                last-x (aref mx k) last-y (aref my k)))))))
            ((= mode +mode-inter-last-mv+)
             (setf (aref mx 0) last-x (aref my 0) last-y))
            ((= mode +mode-inter-prior-last+)
             (setf (aref mx 0) prior-x (aref my 0) prior-y)
             (setf prior-x last-x prior-y last-y
                   last-x (aref mx 0) last-y (aref my 0)))
            (t (setf (aref mx 0) 0 (aref my 0) 0)))
          ;; the four luma fragments
          (dotimes (k 4)
            (let ((f (%luma-frag d mb-x mb-y k))
                  (i (if (= mode +mode-inter-fourmv+) k 0)))
              (setf (aref (d-mvx d) f) (aref mx i)
                    (aref (d-mvy d) f) (aref my i))))
          ;; and the chroma vector, which is the average of the four halved — HALVED WITH THE LOW
          ;; BIT STICKY, `(v >> 1) | (v & 1)', so that a half-pixel luma vector stays a half-pixel
          ;; chroma one instead of rounding to a whole pixel
          (let ((cx (if (= mode +mode-inter-fourmv+)
                        (%rshift (+ (aref mx 0) (aref mx 1) (aref mx 2) (aref mx 3)) 2)
                        (aref mx 0)))
                (cy (if (= mode +mode-inter-fourmv+)
                        (%rshift (+ (aref my 0) (aref my 1) (aref my 2) (aref my 3)) 2)
                        (aref my 0))))
            (setf cx (logior (ash cx -1) (logand cx 1))
                  cy (logior (ash cy -1) (logand cy 1)))
            (let ((cf (+ (* mb-y (aref (d-frag-w d) 1)) mb-x)))
              (dotimes (p 2)
                (let ((f (+ (aref (d-frag-start d) (1+ p)) cf)))
                  (setf (aref (d-mvx d) f) cx (aref (d-mvy d) f) cy))))))))))

;;; ---- per-block quantiser index ------------------------------------------------------------------

(defun %unpack-qpis (br d)
  "Which of the picture's quantiser indices each coded fragment uses (7.4).

   Sent as up to two run-length passes over the coded fragments in coding order, each pass promoting
   some of them one index further along.  A picture with one index sends nothing at all."
  (let ((remaining (d-total-coded d)))
    (declare (type fixnum remaining))
    (dotimes (qpi (1- (d-nqps d)))
      (when (<= remaining 0) (return))
      (let ((i 0) (decoded 0) (at-qpi 0) (bit (logxor 1 (read-bit br))) (run 0))
        (declare (type fixnum i decoded at-qpi bit run))
        (loop
          (if (= run +long-run+) (setf bit (read-bit br)) (setf bit (logxor bit 1)))
          (setf run (%read-sb-run br d))
          (incf decoded run)
          (when (zerop bit) (incf at-qpi run))
          (let ((j 0))
            (declare (type fixnum j))
            (loop while (< j run)
                  do (when (>= i (d-total-coded d))
                       (%err "a quantiser index run past the last coded fragment"))
                     (let ((f (aref (d-coded d) i)))
                       (when (= (aref (d-qpi d) f) qpi)
                         (incf (aref (d-qpi d) f) bit)
                         (incf j)))
                     (incf i)))
          (unless (and (< decoded remaining) (not (br-eof-p br))) (return)))
        (decf remaining at-qpi)))))

;;; ---- the coefficients ---------------------------------------------------------------------------

(defun %unpack-level (br d plane zzi huff eob-run)
  "One pass of the coefficient interleave: scan position ZZI of every fragment of PLANE that has not
   already ended, in coding order.

   EOB-RUN crosses in and out because a run of ended blocks may begin in one plane at one scan
   position and finish in another at the next — the interleave is one continuous stream and the
   plane and level boundaries are not synchronisation points."
  (declare (type fixnum plane zzi eob-run) (optimize (speed 3) (safety 1)))
  (let* ((list (d-coded d)) (nextzz (d-nextzz d)) (endzz (d-endzz d)) (coeffs (d-coeffs d))
         (start (aref (d-coded-start d) plane))
         (end (+ start (aref (d-coded-count d) plane))))
    (declare (type fixnum start end))
    (loop for idx of-type fixnum from start below end
          for f of-type fixnum = (aref list idx)
          do (when (= (aref nextzz f) zzi)
               (cond
                 ((plusp eob-run) (setf (aref nextzz f) 64 (aref endzz f) zzi) (decf eob-run))
                 (t
                  (when (br-eof-p br) (%err "the coefficients ran out mid-picture"))
                  (let ((token (%huff br huff "coefficient")))
                    (declare (type fixnum token))
                    (if (<= token 6)
                        (let ((v (aref +eob-run+ token 0))
                              (bits (aref +eob-run+ token 1)))
                          (declare (type fixnum v bits))
                          (when (plusp bits) (incf v (read-bits br bits)))
                          (setf eob-run (if (zerop v) most-positive-fixnum v))
                          (setf (aref nextzz f) 64 (aref endzz f) zzi)
                          (decf eob-run))
                        (let* ((nbits (aref +coeff-bits+ token))
                               (raw (if (plusp nbits) (read-bits br nbits) 0))
                               (coeff (if (zerop nbits)
                                          (aref +coeff-base+ token)
                                          (let ((half (ash 1 (1- nbits)))
                                                (b (aref +coeff-base+ token)))
                                            (if (< raw half) (+ b raw) (- (+ b (- raw half)))))))
                               (zrun (+ (aref +zero-run-base+ token)
                                        (let ((zb (aref +zero-run-bits+ token)))
                                          (if (plusp zb) (read-bits br zb) 0))))
                               (pos (+ zzi zrun)))
                          (declare (type fixnum nbits raw coeff zrun pos))
                          (when (> pos 63) (%err "a run of ~d zeros from scan position ~d" zrun zzi))
                          ;; a coefficient is THE DC only when it sits at scan position zero
                          ;; with no run of zeros in front of it; a token at level zero that
                          ;; carries a run codes an AC coefficient further along and leaves the
                          ;; DC at nothing, which is worth stating because writing it to the DC
                          ;; instead is a silent corruption of exactly the blocks that use it
                          (if (and (zerop zzi) (zerop zrun))
                              (setf (aref (d-dc d) f) coeff)
                              (setf (aref coeffs (+ (* 64 f) pos)) coeff))
                          (setf (aref nextzz f) (min 64 (1+ pos)))
                          (when (>= pos 63) (setf (aref endzz f) 63))))))))
          finally (return eob-run))))

(defparameter +predictor-weights+
  (make-array '(16 4) :element-type 'fixnum :initial-contents
   '((   0   0   0   0)
     (   0   0   0 128)     ; left
     (   0   0 128   0)     ; up-right
     (   0   0  53  75)     ; up-right, left
     (   0 128   0   0)     ; up
     (   0  64   0  64)     ; up, left
     (   0 128   0   0)     ; up, up-right
     (   0   0  53  75)     ; up, up-right, left
     ( 128   0   0   0)     ; up-left
     (   0   0   0 128)     ; up-left, left
     (  64   0  64   0)     ; up-left, up-right
     (   0   0  53  75)     ; up-left, up-right, left
     (   0 128   0   0)     ; up-left, up
     (-104 116   0 116)     ; up-left, up, left
     (  24  80  24   0)     ; up-left, up, up-right
     (-104 116   0 116)))   ; all four
  "Weights over one hundred and twenty-eight for the up-left, up, up-right and left neighbours, one
   row per subset of them that is available.  Several rows ignore neighbours they have — the
   three-neighbour cases fall back to the two-neighbour weights — which is not an oversight but the
   table VP3 shipped and Theora froze.")
(declaim (type (simple-array fixnum (16 4)) +predictor-weights+))

(defparameter +dc-reference-class+
  (make-array 9 :element-type 'fixnum :initial-contents '(1 0 1 1 1 2 2 1 3))
  "Which picture a fragment's DC is relative to, by coding mode: the previous one, itself (intra),
   or the golden one.  A DC may only be predicted from a neighbour in the same class, because the
   two are then differences from the same thing.")
(declaim (type (simple-array fixnum (9)) +dc-reference-class+))

(defun %reverse-dc-prediction (d plane)
  "Turn each coded fragment's transmitted DC difference back into a DC, in raster order (7.9.2).

   The predictor is a weighted sum of up to four already-decoded neighbours, and when none of them
   is usable it falls back to the last DC decoded IN THE SAME CLASS — a per-class running value that
   crosses rows and is never reset within a plane."
  (let* ((w (aref (d-frag-w d) plane)) (h (aref (d-frag-h d) plane))
         (start (aref (d-frag-start d) plane))
         (dc (d-dc d)) (coding (d-coding d))
         (last (make-array 4 :element-type 'fixnum :initial-element 0)))
    (declare (type fixnum w h start) (optimize (speed 3) (safety 1)))
    (dotimes (y h)
      (dotimes (x w)
        (let ((i (+ start (* y w) x)))
          (declare (type fixnum i))
          (unless (= (aref coding i) +mode-copy+)
            (let ((class (aref +dc-reference-class+ (aref coding i)))
                  (transform 0) (vl 0) (vul 0) (vu 0) (vur 0))
              (declare (type fixnum class transform vl vul vu vur))
              (macrolet ((compatible (j)
                           `(= (aref +dc-reference-class+ (aref coding ,j)) class)))
                (when (plusp x)
                  (let ((l (- i 1)))
                    (setf vl (aref dc l))
                    (when (compatible l) (setf transform (logior transform 1)))))
                (when (plusp y)
                  (let ((u (- i w)))
                    (setf vu (aref dc u))
                    (when (compatible u) (setf transform (logior transform 4)))
                    (when (plusp x)
                      (let ((ul (- i w 1)))
                        (setf vul (aref dc ul))
                        (when (compatible ul) (setf transform (logior transform 8)))))
                    (when (< (1+ x) w)
                      (let ((ur (+ (- i w) 1)))
                        (setf vur (aref dc ur))
                        (when (compatible ur) (setf transform (logior transform 2))))))))
              (let ((pred
                      (if (zerop transform)
                          (aref last class)
                          (let ((p (truncate (+ (* (aref +predictor-weights+ transform 0) vul)
                                                (* (aref +predictor-weights+ transform 1) vu)
                                                (* (aref +predictor-weights+ transform 2) vur)
                                                (* (aref +predictor-weights+ transform 3) vl))
                                             128)))
                            (declare (type fixnum p))
                            ;; the two four-term predictors can run away from every term they were
                            ;; built from; when they do, the nearest single neighbour stands in
                            (when (or (= transform 15) (= transform 13))
                              (cond ((> (abs (- p vu)) 128) (setf p vu))
                                    ((> (abs (- p vl)) 128) (setf p vl))
                                    ((> (abs (- p vul)) 128) (setf p vul))))
                            p))))
                (declare (type fixnum pred))
                (incf (aref dc i) pred)
                (setf (aref last class) (aref dc i))))))))))

(defun %unpack-coefficients (br d)
  "All sixty-four passes, and the DC prediction that sits between the first and the second."
  (let* ((huff (inf-huff (d-info d)))
         (dc-y (read-bits br 4)) (dc-c (read-bits br 4))
         (eob 0))
    (setf eob (%unpack-level br d 0 0 (aref huff dc-y) eob))
    (%reverse-dc-prediction d 0)
    (setf eob (%unpack-level br d 1 0 (aref huff dc-c) eob))
    (setf eob (%unpack-level br d 2 0 (aref huff dc-c) eob))
    (%reverse-dc-prediction d 1)
    (%reverse-dc-prediction d 2)
    (let ((ac-y (read-bits br 4)) (ac-c (read-bits br 4)))
      ;; the sixty-three AC positions share four tables between them, in bands: the first five
      ;; positions, then nine, then thirteen, then the remaining thirty-six
      (flet ((group (zzi) (cond ((<= zzi 5) 16) ((<= zzi 14) 32) ((<= zzi 27) 48) (t 64))))
        (loop for zzi from 1 to 63
              do (let ((y (aref huff (+ ac-y (group zzi))))
                       (c (aref huff (+ ac-c (group zzi)))))
                   (setf eob (%unpack-level br d 0 zzi y eob))
                   (setf eob (%unpack-level br d 1 zzi c eob))
                   (setf eob (%unpack-level br d 2 zzi c eob))))))))

;;; ---- the inverse transform ----------------------------------------------------------------------
;;;
;;; VP3's transform is not an integer transform in the sense H.264's is.  It is the floating-point
;;; DCT with the cosines held as sixteen-bit fixed-point constants, and every product is taken back
;;; down by sixteen bits immediately.  So it is exactly specified — two decoders agree bit for bit —
;;; but the specification is a particular rounding of a real-valued transform rather than something
;;; that inverts an integer forward transform exactly.  The consequence is that the shifts below are
;;; not free to move: `>> 16' after each multiply, `>> 4' at the very end, and an eight added once in
;;; the second pass and not the first.

(defconstant +c1+ 64277) (defconstant +c2+ 60547) (defconstant +c3+ 54491)
(defconstant +c4+ 46341) (defconstant +c5+ 36410) (defconstant +c6+ 25080)
(defconstant +c7+ 12785)

(declaim (inline %m %clamp8))
(defun %m (a b) (declare (type fixnum a b) (optimize (speed 3) (safety 0))) (ash (* a b) -16))
(defun %clamp8 (v) (declare (type fixnum v) (optimize (speed 3) (safety 0)))
  (if (< v 0) 0 (if (> v 255) 255 v)))

(defmacro %idct-butterfly (d0 d1 d2 d3 d4 d5 d6 d7 bias &body emit)
  "The eight-point butterfly, shared by both passes.  EMIT receives the eight outputs in order."
  `(let* ((a (+ (%m +c1+ ,d1) (%m +c7+ ,d7)))
          (b (- (%m +c7+ ,d1) (%m +c1+ ,d7)))
          (c (+ (%m +c3+ ,d3) (%m +c5+ ,d5)))
          (dd (- (%m +c3+ ,d5) (%m +c5+ ,d3)))
          (ad (%m +c4+ (- a c)))
          (bd (%m +c4+ (- b dd)))
          (cd (+ a c))
          (ddd (+ b dd))
          (e (+ (%m +c4+ (+ ,d0 ,d4)) ,bias))
          (f (+ (%m +c4+ (- ,d0 ,d4)) ,bias))
          (g (+ (%m +c2+ ,d2) (%m +c6+ ,d6)))
          (h (- (%m +c6+ ,d2) (%m +c2+ ,d6)))
          (ed (- e g)) (gd (+ e g))
          (add (+ f ad)) (bdd (- bd h))
          (fd (- f ad)) (hd (+ bd h)))
     (declare (type fixnum a b c dd ad bd cd ddd e f g h ed gd add bdd fd hd))
     (macrolet ((%out (k) (ecase k
                            (0 '(+ gd cd)) (1 '(+ add hd)) (2 '(- add hd)) (3 '(+ ed ddd))
                            (4 '(- ed ddd)) (5 '(+ fd bdd)) (6 '(- fd bdd)) (7 '(- gd cd)))))
       ,@emit)))

(defun %idct (block dst stride base intra)
  "The inverse transform of one fragment, added to (or placed over) the eight by eight samples at
   BASE.  BLOCK is dequantised coefficients in raster order and is left zeroed."
  (declare (type fixnums block) (type octets dst) (type fixnum stride base)
           (optimize (speed 3) (safety 1)))
  (dotimes (r 8)
    (let ((i (* r 8)))
      (declare (type fixnum i))
      (unless (and (zerop (aref block i)) (zerop (aref block (+ i 1)))
                   (zerop (aref block (+ i 2))) (zerop (aref block (+ i 3)))
                   (zerop (aref block (+ i 4))) (zerop (aref block (+ i 5)))
                   (zerop (aref block (+ i 6))) (zerop (aref block (+ i 7))))
        (let ((d0 (aref block i)) (d1 (aref block (+ i 1))) (d2 (aref block (+ i 2)))
              (d3 (aref block (+ i 3))) (d4 (aref block (+ i 4))) (d5 (aref block (+ i 5)))
              (d6 (aref block (+ i 6))) (d7 (aref block (+ i 7))))
          (declare (type fixnum d0 d1 d2 d3 d4 d5 d6 d7))
          (%idct-butterfly d0 d1 d2 d3 d4 d5 d6 d7 0
            (setf (aref block i) (%out 0) (aref block (+ i 1)) (%out 1)
                  (aref block (+ i 2)) (%out 2) (aref block (+ i 3)) (%out 3)
                  (aref block (+ i 4)) (%out 4) (aref block (+ i 5)) (%out 5)
                  (aref block (+ i 6)) (%out 6) (aref block (+ i 7)) (%out 7)))))))
  (let ((bias (if intra (+ 8 (* 16 128)) 8)))
    (declare (type fixnum bias))
    (dotimes (c 8)
      (let ((o (+ base c)))
        (declare (type fixnum o))
        (if (and (zerop (aref block (+ 8 c))) (zerop (aref block (+ 16 c)))
                 (zerop (aref block (+ 24 c))) (zerop (aref block (+ 32 c)))
                 (zerop (aref block (+ 40 c))) (zerop (aref block (+ 48 c)))
                 (zerop (aref block (+ 56 c))))
            ;; a column with nothing but its own DC: one value, eight times
            (let ((v (ash (+ (* +c4+ (aref block c)) (ash 8 16)) -20)))
              (declare (type fixnum v))
              (cond (intra
                     (let ((s (%clamp8 (+ 128 v))))
                       (dotimes (r 8) (setf (aref dst (+ o (* r stride))) s))))
                    ((not (zerop v))
                     (dotimes (r 8)
                       (let ((k (+ o (* r stride))))
                         (setf (aref dst k) (%clamp8 (+ (aref dst k) v))))))))
            (let ((d0 (aref block c)) (d1 (aref block (+ 8 c))) (d2 (aref block (+ 16 c)))
                  (d3 (aref block (+ 24 c))) (d4 (aref block (+ 32 c))) (d5 (aref block (+ 40 c)))
                  (d6 (aref block (+ 48 c))) (d7 (aref block (+ 56 c))))
              (declare (type fixnum d0 d1 d2 d3 d4 d5 d6 d7))
              (%idct-butterfly d0 d1 d2 d3 d4 d5 d6 d7 bias
                (macrolet ((put (r v) `(let ((k (+ o (* ,r stride))))
                                         (setf (aref dst k)
                                               (if intra (%clamp8 (ash ,v -4))
                                                   (%clamp8 (+ (aref dst k) (ash ,v -4))))))))
                  (put 0 (%out 0)) (put 1 (%out 1)) (put 2 (%out 2)) (put 3 (%out 3))
                  (put 4 (%out 4)) (put 5 (%out 5)) (put 6 (%out 6)) (put 7 (%out 7))))))))))

(defun %idct-dc-add (dst stride base dc)
  "The whole transform of a block whose only coefficient is a DC that was never transmitted — it
   came from the prediction — which is common enough in an inter picture to be worth the shortcut."
  (declare (type octets dst) (type fixnum stride base dc) (optimize (speed 3) (safety 1)))
  (let ((v (ash (+ dc 15) -5)))
    (declare (type fixnum v))
    (unless (zerop v)
      (dotimes (r 8)
        (let ((o (+ base (* r stride))))
          (declare (type fixnum o))
          (dotimes (col 8)
            (setf (aref dst (+ o col)) (%clamp8 (+ (aref dst (+ o col)) v)))))))))

;;; ---- motion compensation ------------------------------------------------------------------------

(defun %mc (dst src stride dbase sbase half diag-alt)
  "Copy an eight by eight block, at whole or half sample resolution.

   HALF is two bits, one per axis.  Where both are set VP3 does NOT average the four surrounding
   samples as every other codec does: it averages two of them along ONE diagonal, chosen by whether
   the vector's two components have the same sign.  That is a genuine quirk of the format and the
   reason a diagonal half-sample vector looks slightly sharper here than it would elsewhere."
  (declare (type octets dst src) (type fixnum stride dbase sbase half diag-alt)
           (optimize (speed 3) (safety 1)))
  (case half
    (0 (dotimes (r 8)
         (replace dst src :start1 (+ dbase (* r stride)) :end1 (+ dbase (* r stride) 8)
                          :start2 (+ sbase (* r stride)) :end2 (+ sbase (* r stride) 8))))
    (1 (dotimes (r 8)
         (let ((d (+ dbase (* r stride))) (s (+ sbase (* r stride))))
           (dotimes (c 8)
             (setf (aref dst (+ d c)) (ash (+ (aref src (+ s c)) (aref src (+ s c 1))) -1))))))
    (2 (dotimes (r 8)
         (let ((d (+ dbase (* r stride))) (s (+ sbase (* r stride))))
           (dotimes (c 8)
             (setf (aref dst (+ d c))
                   (ash (+ (aref src (+ s c)) (aref src (+ s c stride))) -1))))))
    (t (let ((a (- diag-alt)) (b (+ stride 1 diag-alt)))
         (declare (type fixnum a b))
         (dotimes (r 8)
           (let ((d (+ dbase (* r stride))) (s (+ sbase (* r stride))))
             (dotimes (c 8)
               (setf (aref dst (+ d c))
                     (ash (+ (aref src (+ s c a)) (aref src (+ s c b))) -1)))))))))

;;; ---- the loop filter ----------------------------------------------------------------------------

(defun %filter-edge (plane at step across bounding)
  "Eight samples across one block edge (7.10).

   STEP walks along the edge, ACROSS crosses it.  The correction is a three-tap difference passed
   through the saturating table, added on one side and subtracted on the other, so the filter moves
   samples towards each other without changing their sum."
  (declare (type octets plane) (type fixnums bounding)
           (type fixnum at step across) (optimize (speed 3) (safety 1)))
  (dotimes (k 8)
    (let* ((p (+ at (* k step)))
           (v (+ (- (aref plane (- p across across)) (aref plane (+ p across)))
                 (* 3 (- (aref plane p) (aref plane (- p across))))))
           (fv (aref bounding (+ 256 (ash (+ v 4) -3)))))
      (declare (type fixnum p v fv))
      (setf (aref plane (- p across)) (%clamp8 (+ (aref plane (- p across)) fv))
            (aref plane p) (%clamp8 (- (aref plane p) fv))))))

(defun %apply-loop-filter (d pic plane ystart yend)
  "Filter the coded fragments of rows YSTART up to but not including YEND.

   The order is the awkward part and it is load-bearing: a fragment filters its own left and top
   edges always, and its right and bottom edges ONLY when the neighbour there is not itself coded.
   Some samples are therefore filtered twice, and which of the two passes happens first changes the
   answer, so the traversal is part of the format."
  (let* ((w (aref (d-frag-w d) plane)) (h (aref (d-frag-h d) plane))
         (start (aref (d-frag-start d) plane))
         (coding (d-coding d))
         (data (aref (pic-planes pic) plane))
         (stride (aref (pic-stride pic) plane))
         (org (aref (pic-org pic) plane))
         (bounding (d-bounding d)))
    (declare (type fixnum w h start stride org) (type octets data))
    (loop for y of-type fixnum from (max 0 ystart) below (min yend h)
          do (loop for x of-type fixnum from 0 below w
                   for f of-type fixnum = (+ start (* y w) x)
                   do (unless (= (aref coding f) +mode-copy+)
                        (let ((at (+ org (* 8 y stride) (* 8 x))))
                          (declare (type fixnum at))
                          (when (plusp x) (%filter-edge data at stride 1 bounding))
                          (when (plusp y) (%filter-edge data at 1 stride bounding))
                          (when (and (< (1+ x) w) (= (aref coding (1+ f)) +mode-copy+))
                            (%filter-edge data (+ at 8) stride 1 bounding))
                          (when (and (< (1+ y) h) (= (aref coding (+ f w)) +mode-copy+))
                            (%filter-edge data (+ at (* 8 stride)) 1 stride bounding))))))))

;;; ---- reconstruction -----------------------------------------------------------------------------

(defun %render-superblock-row (d plane sby)
  "One row of superblocks of one plane: predict, transform, and add, fragment by fragment along the
   Hilbert curve."
  (let* ((cur (d-cur d)) (last (d-last d)) (golden (d-golden d))
         (dst (aref (pic-planes cur) plane))
         (stride (aref (pic-stride cur) plane))
         (org (aref (pic-org cur) plane))
         (w (aref (d-frag-w d) plane)) (h (aref (d-frag-h d) plane))
         (start (aref (d-frag-start d) plane))
         (block (d-block d))
         (last-plane (aref (pic-planes last) plane))
         (golden-plane (aref (pic-planes golden) plane))
         (coding (d-coding d)) (coeffs (d-coeffs d)))
    (declare (type octets dst last-plane golden-plane) (type fixnums block)
             (type fixnum stride org w h start))
    (dotimes (sbx (aref (d-sb-w d) plane))
      (dotimes (j 16)
        (let ((x (+ (* 4 sbx) (aref +superblock-scan+ j 0)))
              (y (+ (* 4 sby) (aref +superblock-scan+ j 1))))
          (declare (type fixnum x y))
          (when (and (< x w) (< y h))
            (let* ((f (+ start (* y w) x))
                   (mode (aref coding f))
                   (base (+ org (* 8 y stride) (* 8 x))))
              (declare (type fixnum f mode base))
              (if (= mode +mode-copy+)
                  (dotimes (r 8)
                    (let ((o (+ base (* r stride))))
                      (replace dst last-plane :start1 o :end1 (+ o 8) :start2 o :end2 (+ o 8))))
                  (let ((src (if (or (= mode +mode-using-golden+) (= mode +mode-golden-mv+))
                                 golden-plane last-plane))
                        (half 0) (diag 0) (sbase base))
                    (declare (type fixnum half diag sbase))
                    (when (and (> mode +mode-intra+) (/= mode +mode-using-golden+))
                      (let ((mx (aref (d-mvx d) f)) (my (aref (d-mvy d) f)))
                        (declare (type fixnum mx my))
                        (setf half (logior (logand mx 1) (ash (logand my 1) 1))
                              diag (if (minusp (logxor mx my)) -1 0))
                        (incf sbase (+ (ash mx -1) (* (ash my -1) stride)))))
                    (unless (= mode +mode-intra+)
                      (%mc dst src stride base sbase half diag))
                    ;; dequantise into the scratch block and transform
                    (let* ((intra (= mode +mode-intra+))
                           (inter (if intra 0 1))
                           (qm (the fixnums (aref (d-qmat d) (aref (d-qpi d) f) inter plane)))
                           (dc (* (aref (d-dc d) f) (aref qm 0)))
                           (endzz (aref (d-endzz d) f)))
                      (declare (type fixnum inter dc endzz))
                      (if (and (not intra) (zerop endzz))
                          (%idct-dc-add dst stride base dc)
                          (progn
                            (fill block 0)
                            (loop for zzi of-type fixnum from 1 to 63
                                  for c of-type fixnum = (aref coeffs (+ (* 64 f) zzi))
                                  do (unless (zerop c)
                                       (let ((k (aref +zigzag+ zzi)))
                                         (setf (aref block k) (* c (aref qm k))))))
                            (setf (aref block 0) dc)
                            (%idct block dst stride base intra))))))))))))
  (values))

;;; ---- output -------------------------------------------------------------------------------------

(defstruct (frame (:conc-name fr-))
  (width 0 :type fixnum) (height 0 :type fixnum)
  (cwidth 0 :type fixnum) (cheight 0 :type fixnum)
  (y (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (u (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (v (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (ystride 0 :type fixnum) (cstride 0 :type fixnum)
  (keyframe nil)
  (timestamp nil))

(defun %emit-frame (d pic)
  "Crop to the displayed picture and turn it the right way up.

   Both happen here and nowhere else.  Theora codes a whole number of macroblocks and displays a
   window inside that, and it numbers rows from the bottom; carrying either convention outwards
   would put a sign or an offset into every other file that touches a picture."
  (let* ((i (d-info d))
         (pw (inf-picture-width i)) (ph (inf-picture-height i))
         (ox (inf-offset-x i)) (oy (inf-offset-y i))
         (cw (ash (+ pw 1) -1)) (ch (ash (+ ph 1) -1))
         (f (make-frame :width pw :height ph :cwidth cw :cheight ch
                        :ystride pw :cstride cw :keyframe (d-keyframe d)
                        :y (make-array (* pw ph) :element-type '(unsigned-byte 8))
                        :u (make-array (* cw ch) :element-type '(unsigned-byte 8))
                        :v (make-array (* cw ch) :element-type '(unsigned-byte 8)))))
    (flet ((copy (plane out ow oh px py ostride)
             (let ((src (aref (pic-planes pic) plane))
                   (stride (aref (pic-stride pic) plane))
                   (org (aref (pic-org pic) plane)))
               (dotimes (r oh)
                 (let ((s (+ org (* (+ py (- oh 1 r)) stride) px)))
                   (replace out src :start1 (* r ostride) :end1 (+ (* r ostride) ow)
                                    :start2 s :end2 (+ s ow)))))))
      (copy 0 (fr-y f) pw ph ox oy pw)
      (copy 1 (fr-u f) cw ch (ash ox -1) (ash oy -1) cw)
      (copy 2 (fr-v f) cw ch (ash ox -1) (ash oy -1) cw))
    f))

;;; ---- one picture --------------------------------------------------------------------------------

(defun %frame-header (br d)
  "The handful of bits before the picture proper: whether it is a key frame, and its up to three
   quantiser indices."
  (when (plusp (read-bit br)) (%err "a header packet handed to the frame decoder"))
  (setf (d-keyframe d) (zerop (read-bit br)))
  (let ((old (copy-seq (d-qps d))) (n 0))
    (declare (type fixnum n))
    (loop (setf (aref (d-qps d) n) (read-bits br 6))
          (incf n)
          (unless (and (< n 3) (plusp (read-bit br))) (return)))
    (setf (d-nqps d) n)
    (loop for k from n below 3 do (setf (aref (d-qps d) k) -1))
    ;; the filter limit and the weight matrices both follow the first index, and the matrices follow
    ;; all of them; rebuilding is cheap but not free, so only when something moved
    (let ((limit (aref (inf-filter-limits (d-info d)) (aref (d-qps d) 0))))
      (unless (= limit (d-filter-limit d)) (%init-loop-filter d limit)))
    (dotimes (k n)
      (when (or (/= (aref (d-qps d) k) (aref old k))
                (/= (aref (d-qps d) 0) (aref old 0)))
        (%init-dequant d k))))
  (when (d-keyframe d)
    (when (plusp (read-bit br)) (%err "an unknown key frame coding type"))
    (read-bits br 2)))

(defun %pick-buffer (d)
  "A picture buffer that is neither of the two references.  Three is always enough: a picture reads
   at most the previous one and the golden one."
  (dotimes (i 3 (%err "no free picture buffer"))
    (let ((p (aref (d-pool d) i)))
      (unless (or (eq p (d-last d)) (eq p (d-golden d))) (return p)))))

(defun decode-frame (d packet &key (start 0) (end (length packet)))
  "One Theora data packet in, one picture out — or NIL for a packet that codes nothing."
  (declare (type octets packet))
  (when (<= (- end start) 0)
    ;; a zero-length packet repeats the previous picture exactly, which is how a still section of a
    ;; video costs nothing at all
    (return-from decode-frame (and (d-last d) (%emit-frame d (d-last d)))))
  (let ((br (make-br packet :start start :end end)))
    (%frame-header br d)
    (unless (or (d-keyframe d) (d-last d))
      (%err "an inter picture before any key frame"))
    (setf (d-cur d) (%pick-buffer d))
    (unless (d-last d) (setf (d-last d) (d-cur d)))
    (unless (d-golden d) (setf (d-golden d) (d-cur d)))
    (fill (d-qpi d) 0)
    (fill (d-nextzz d) 0)
    (fill (d-endzz d) 0)
    (fill (d-dc d) 0)
    (fill (d-coeffs d) 0)
    (fill (d-mvx d) 0)
    (fill (d-mvy d) 0)
    (%unpack-superblocks br d)
    (%unpack-modes br d)
    (%unpack-vectors br d)
    (%unpack-qpis br d)
    (%unpack-coefficients br d)
    ;; the picture is reconstructed in slices of one chroma superblock row, and the loop filter
    ;; follows a superblock row behind so that a fragment's neighbours below are already there
    (let ((filter (plusp (d-filter-limit d))))
      (dotimes (slice (aref (d-sb-h d) 1))
        (dotimes (plane 3)
          (let* ((first (if (zerop plane) (* 2 slice) slice))
                 (rows (if (zerop plane) 2 1))
                 (h (aref (d-frag-h d) plane)))
            (dotimes (k rows)
              (let ((sby (+ first k)))
                (when (< sby (aref (d-sb-h d) plane))
                  (%render-superblock-row d plane sby)
                  (when filter
                    (%apply-loop-filter d (d-cur d) plane
                                        (- (* 4 sby) (if (plusp sby) 1 0))
                                        (min (+ (* 4 sby) 3) (1- h))))))))))
      (when filter
        (dotimes (plane 3)
          (let ((row (1- (aref (d-frag-h d) plane))))
            (%apply-loop-filter d (d-cur d) plane row (1+ row))))))
    (%extend-borders (d-cur d))
    (setf (d-last d) (d-cur d))
    (when (d-keyframe d) (setf (d-golden d) (d-cur d)))
    (incf (d-frames d))
    (%emit-frame d (d-cur d))))


(defun as-picture (f)
  "This decoder's frame as a REEL.DECODE:PICTURE, sharing planes rather than copying them."
  (reel.decode::%make-shared-picture
   :width (fr-width f) :height (fr-height f)
   :y (fr-y f) :u (fr-u f) :v (fr-v f)
   :y-stride (fr-ystride f) :uv-stride (fr-cstride f)
   :y-offset 0 :uv-offset 0))

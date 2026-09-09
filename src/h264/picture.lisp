;;;; h264/picture.lisp — the decoded picture, the slice state, and the per-block accessors.
;;;;
;;;; These live in their own file for a reason that is entirely about the compiler and not at all
;;;; about the format: EVERY SAMPLE THE DECODER TOUCHES goes through one of these slots, and a
;;;; caller that has not yet seen the DEFSTRUCT cannot inline the access or know what type comes
;;;; back.  When this was at the top of slice.lisp — which loads AFTER motion.lisp — the whole of
;;;; motion compensation, the single hottest path in the decoder, read the reference picture through
;;;; out-of-line accessor calls returning objects of unknown type, and every arithmetic operation on
;;;; the results was generic.  Moving the definitions ahead of their users cut the decode time of a
;;;; 1080p stream by more than half without changing a line of the arithmetic.
;;;;
;;;; The rule that follows: a structure whose slots are read in an inner loop belongs in a file that
;;;; precedes every file with such a loop.  SBCL says so itself, in a style warning that is easy to
;;;; read past.

(in-package #:reel.h264)

;;; ---- where a macroblock's 4x4 blocks live ---------------------------------------------------

(defparameter +blk-x+
  ;; luma block index (0..15) to its x position in 4-sample units: 8x8 sub-blocks in raster
  ;; order, and 4x4 blocks in raster order inside each of those
  (make-array 16 :element-type '(signed-byte 32)
                 :initial-contents '(0 1 0 1  2 3 2 3  0 1 0 1  2 3 2 3)))
(defparameter +blk-y+
  (make-array 16 :element-type '(signed-byte 32)
                 :initial-contents '(0 0 1 1  0 0 1 1  2 2 3 3  2 2 3 3)))

(defparameter +blk-index+
  ;; the inverse of +BLK-X+/+BLK-Y+: which block index sits at (x, y) in 4x4 units.  Needed to ask
  ;; "has that neighbour been decoded yet", which is a question about DECODING ORDER, and decoding
  ;; order here is 8x8 sub-blocks in raster with 4x4s in raster inside them — not raster overall.
  (make-array '(4 4) :element-type '(signed-byte 32)
                     :initial-contents '((0 1 4 5)
                                         (2 3 6 7)
                                         (8 9 12 13)
                                         (10 11 14 15))))

(defparameter +intra4x4-cbp+
  ;; Table 9-4: the mapping from the coded_block_pattern code number to the pattern itself, for
  ;; Intra_4x4 macroblocks.  Six bits: four luma 8x8 flags, then 0/1/2 for chroma.
  (make-array 48 :element-type '(unsigned-byte 8)
                 :initial-contents '(47 31 15 0  23 27 29 30  7 11 13 14  39 43 45 46
                                     16 3 5 10  12 19 21 26  28 35 37 42  44 1 2 4
                                     8 17 18 20  24 6 9 22  25 32 33 34  36 40 38 41)))

;;; ---- the picture being decoded into ----------------------------------------------------------

(defconstant +pad+ 16 "Border samples around each plane, so a neighbour read at -1 is in bounds.")

(defconstant +no-ref-poc+ (- (ash 1 31))
  "Stands in the reference-picture array for `this list predicts nothing here'.  A real picture
order count can be negative, so absence needs a value no picture can hold — and one that fits in
the thirty-two bits the array is, which is why it is not MOST-NEGATIVE-FIXNUM.")

(deftype fixnums () '(simple-array (signed-byte 32) (*)))

(declaim (inline %empty8 %emptyfx))
(defun %empty8 () (make-array 0 :element-type '(unsigned-byte 8)))
(defun %emptyfx () (make-array 0 :element-type '(signed-byte 32)))

(deftype dim ()
  "A picture dimension, a plane stride, or an offset into a plane.

   TWENTY-SIX BITS, not FIXNUM, and the difference is the point: SBCL cannot prove that the sum or
   product of two fixnums is a fixnum, so `(+ yoff (* row ystride))' — which every sample read in
   the decoder goes through — compiled to out-of-line generic arithmetic.  Twenty-six bits holds
   any offset into any plane H.264 permits (4096x2304 padded is under ten million samples) and two
   of them multiply to something a fixnum still holds, so the whole of that arithmetic is inline."
  '(unsigned-byte 26))

(defstruct (picture (:conc-name pic-))
  (width 0 :type dim) (height 0 :type dim)                ; displayed, after cropping
  ;; Where the displayed picture STARTS inside the coded one, in luma samples.  Cropping has an
  ;; origin as well as a size, and almost every encoder ever written leaves this at zero — it crops
  ;; only the right and bottom, because that is where macroblock alignment puts the waste.  A
  ;; decoder that ignores it is right about every stream it is likely to meet and wrong about the
  ;; conformance stream written to check.
  (crop-x 0 :type dim) (crop-y 0 :type dim)
  (mb-width 0 :type dim) (mb-height 0 :type dim)
  ;; TYPED, and it matters more than it looks.  Every sample the decoder reads or writes goes
  ;; through one of these slots, and an untyped slot makes each of those a generic array dispatch
  ;; at run time — it measured at 4% of decode time all by itself.
  (y (%empty8) :type octets) (u (%empty8) :type octets) (v (%empty8) :type octets)
  (ystride 0 :type dim) (cstride 0 :type dim)
  (yoff 0 :type dim) (coff 0 :type dim)
  ;; per-4x4-block coefficient counts, for nC
  (nz-y (%emptyfx) :type fixnums) (nz-u (%emptyfx) :type fixnums) (nz-v (%emptyfx) :type fixnums)
  ;; per-4x4-block Intra4x4 prediction modes, and per-macroblock facts the loop filter needs
  (modes (%emptyfx) :type fixnums)
  ;; MB-TYPES doubles as "has this macroblock been decoded": -1 means not yet, a value >= 0 is an
  ;; intra type, and an inter one is stored negative.  That keeps availability and Intra4x4 mode
  ;; prediction correct without either of them having to learn about inter macroblocks.
  (mb-types (%emptyfx) :type fixnums)
  ;; Which SLICE coded each macroblock, as that slice's first_mb_in_slice — distinct per slice by
  ;; construction, so it needs no counter.  A neighbour in a different slice is NOT available: the
  ;; whole point of slices is that each decodes without reference to the others, so intra
  ;; prediction, nC and motion prediction must all stop at the boundary.  Getting this wrong does
  ;; not degrade quality, it DESYNCHRONISES: a stale nC picks the wrong coeff_token table and the
  ;; bitstream is lost from the second slice's first macroblock onward.
  (mb-slice (%emptyfx) :type fixnums)
  ;; The loop filter's three per-SLICE parameters, recorded per macroblock because one picture may
  ;; be several slices and each carries its own: disable_deblocking_filter_idc and the two offsets.
  ;; idc 1 turns the filter off for that slice's macroblocks, idc 2 keeps it on but stops it at the
  ;; slice boundary.
  (dbf-idc (%emptyfx) :type fixnums)
  (dbf-alpha (%emptyfx) :type fixnums)
  (dbf-beta (%emptyfx) :type fixnums)
  (mb-qps (%emptyfx) :type fixnums)
  (frame-num 0 :type dim)               ; this picture's frame_num, which is its PicNum for a frame
  (poc 0 :type (signed-byte 30))                  ; picture order count: where it goes on SCREEN, not in the stream
  (ref-p nil)                           ; was it kept as a reference picture
  ;; Motion, per 4x4 block, for BOTH reference lists.  A B slice can predict a block from two
  ;; pictures at once, so every one of these is two deep: mvs holds l0x l0y l1x l1y, refs and
  ;; ref-pics hold one entry per list, and -1 in refs means that list is unused for this block.
  ;; A P slice only ever touches list 0 and reads exactly as it did before.
  (mvs (%emptyfx) :type fixnums)
  (refs (%emptyfx) :type fixnums)
  ;; the coded DIFFERENCES, which only CABAC needs: the context for a vector difference is chosen
  ;; from how large the neighbouring differences were, not from the vectors themselves
  (mvds (%emptyfx) :type fixnums)
  ;; WHICH PICTURE each block referred to, not which index referred to it.  The loop filter asks
  ;; whether two blocks came from the same picture (8.7.2.1), and with a reordered list the same
  ;; picture can sit at two different indices — which is exactly what weighted prediction does.
  (ref-pics (%emptyfx) :type fixnums)
  ;; Per macroblock, and only CABAC reads them: it chooses a context for nearly every syntax
  ;; element from what the neighbouring macroblocks decoded, so facts CAVLC could forget as soon
  ;; as it used them have to survive here.
  ;; Was this 4x4 block predicted in DIRECT mode?  Only the reference-index context asks, and it
  ;; asks per block rather than per macroblock because a B_8x8 can be direct in some of its four
  ;; partitions and not others.
  (blk-direct (%emptyfx) :type fixnums)
  (mb-cbp (%emptyfx) :type fixnums)       ; coded_block_pattern, for the CBP contexts
  (mb-chroma-mode (%emptyfx) :type fixnums) ; intra_chroma_pred_mode
  (mb-dc-cbf (%emptyfx) :type fixnums)   ; bit 0 luma DC, 1 Cb DC, 2 Cr DC
  ;; transform_size_8x8_flag, per macroblock.  Two things read it back: CABAC, whose context for
  ;; the flag counts how many neighbours set it, and the loop filter, which must not filter the
  ;; internal 4x4 edges of a macroblock that had no 4x4 edges to begin with.
  (mb-tf8 (%emptyfx) :type fixnums))

(defun make-picture-for (sps)
  (let* ((mbw (sps-mb-width sps)) (mbh (sps-mb-height sps))
         (aw (* 16 mbw)) (ah (* 16 mbh))
         (ys (+ aw (* 2 +pad+))) (cs (+ (ash aw -1) (* 2 +pad+))))
    (make-picture
     :width (sps-width sps) :height (sps-height sps)
     ;; the crop offsets are in chroma sample units for 4:2:0, so two luma samples each
     :crop-x (* 2 (sps-crop-left sps)) :crop-y (* 2 (sps-crop-top sps))
     :mb-width mbw :mb-height mbh
     :y (make-array (* ys (+ ah (* 2 +pad+))) :element-type '(unsigned-byte 8) :initial-element 128)
     :u (make-array (* cs (+ (ash ah -1) (* 2 +pad+))) :element-type '(unsigned-byte 8) :initial-element 128)
     :v (make-array (* cs (+ (ash ah -1) (* 2 +pad+))) :element-type '(unsigned-byte 8) :initial-element 128)
     :ystride ys :cstride cs
     :yoff (+ (* +pad+ ys) +pad+) :coff (+ (* +pad+ cs) +pad+)
     :nz-y (make-array (* mbw 4 mbh 4) :element-type '(signed-byte 32) :initial-element 0)
     :nz-u (make-array (* mbw 2 mbh 2) :element-type '(signed-byte 32) :initial-element 0)
     :nz-v (make-array (* mbw 2 mbh 2) :element-type '(signed-byte 32) :initial-element 0)
     :modes (make-array (* mbw 4 mbh 4) :element-type '(signed-byte 32) :initial-element 2)
     :mb-types (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element -1)
     :mb-slice (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element -1)
     :dbf-idc (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element 0)
     :dbf-alpha (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element 0)
     :dbf-beta (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element 0)
     :mb-qps (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element 0)
     :mvs (make-array (* mbw 4 mbh 4 4) :element-type '(signed-byte 32) :initial-element 0)
     :refs (make-array (* mbw 4 mbh 4 2) :element-type '(signed-byte 32) :initial-element -1)
     :mvds (make-array (* mbw 4 mbh 4 4) :element-type '(signed-byte 32) :initial-element 0)
     :ref-pics (make-array (* mbw 4 mbh 4 2) :element-type '(signed-byte 32)
                           :initial-element +no-ref-poc+)
     :blk-direct (make-array (* mbw 4 mbh 4) :element-type '(signed-byte 32) :initial-element 0)
     :mb-cbp (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element 0)
     :mb-chroma-mode (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element 0)
     :mb-dc-cbf (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element 0)
     :mb-tf8 (make-array (* mbw mbh) :element-type '(signed-byte 32) :initial-element 0))))

(declaim (inline pic-y-base pic-c-base))
(defun pic-y-base (p mbx mby)
  
  (declare (optimize (speed 3) (safety 1)))(+ (pic-yoff p) (* (* 16 mby) (pic-ystride p)) (* 16 mbx)))
(defun pic-c-base (p mbx mby)
  
  (declare (optimize (speed 3) (safety 1)))(+ (pic-coff p) (* (* 8 mby) (pic-cstride p)) (* 8 mbx)))

;;; ---- the decoding state for one slice -----------------------------------------------------------

(defstruct (slice-state (:conc-name ss-))
  pic sh br
  ref0                                          ; list-0 entry 0, the common case
  (reflist #() :type simple-vector)              ; the whole of list 0, indexed by ref_idx
  ;; Which of this macroblock's sixteen 4x4 blocks have had their motion assigned yet, as a bit
  ;; per block in raster order.  6.4.11.7 marks a neighbouring partition NOT AVAILABLE while it is
  ;; still undecoded, and that is not the same as available-with-a-zero-vector: an unavailable C
  ;; makes D stand in for it, and a zero one does not.  The difference only shows on partitions
  ;; smaller than 8x8, where a partition's above-right neighbour can be a later partition of the
  ;; same macroblock.
  (mb-done 0 :type fixnum)
  cabac                                         ; the arithmetic decoder, or NIL for CAVLC
  (mbx 0 :type fixnum) (mby 0 :type fixnum)
  ;; this slice's first_mb_in_slice, used as its identity when testing neighbour availability
  (slice-id 0 :type fixnum)
  ;; constrained_intra_pred_flag, lifted out of the PPS because INTRA-MB-AVAILABLE-P asks for it
  ;; several times per block and SS-SH is an untyped slot
  (constrained-intra nil)
  (qp 26 :type fixnum)
  ;; typed for the same reason the picture's planes are: these are read and written per block
  (coeffs (make-array 16 :element-type '(signed-byte 32)) :type fixnums)   ; scan-order scratch
  (block (make-array 16 :element-type '(signed-byte 32)) :type fixnums)    ; raster-order scratch
  (luma-dc (make-array 16 :element-type '(signed-byte 32)) :type fixnums)
  (chroma-dc (make-array 4 :element-type '(signed-byte 32)) :type fixnums)
  ;; the second plane's, so neither is per-macroblock
  (chroma-dc-v (make-array 4 :element-type '(signed-byte 32)) :type fixnums)
  (pred (make-array 16 :element-type '(unsigned-byte 8)) :type octets)
  (pt (make-array 9 :element-type '(unsigned-byte 8)) :type octets)
  (pl (make-array 4 :element-type '(unsigned-byte 8)) :type octets)
  ;; somewhere to put the second prediction while the two are averaged: a bi-predicted partition
  ;; is the mean of a list-0 and a list-1 prediction, and neither may be written over the other
  (bi-y (make-array 256 :element-type '(unsigned-byte 8)) :type octets)
  (bi-u (make-array 64 :element-type '(unsigned-byte 8)) :type octets)
  (bi-v (make-array 64 :element-type '(unsigned-byte 8)) :type octets)
  ;; The 8x8 transform, per macroblock.  Held on the state rather than passed down because it
  ;; changes what four separate stages do — mode prediction, residual parsing, reconstruction and
  ;; deblocking — and threading a boolean through all four reads worse than one slot does.
  (tf8 nil)
  ;; 8x8 scratch, the 4x4 scratch widened.  PT8 is sixteen wide because the diagonal modes reach
  ;; past the block into the above-right neighbour, and PTL8 is a one-element array only so that
  ;; GATHER-8X8-NEIGHBOURS can return the filtered corner by writing to it.
  (coeffs8 (make-array 64 :element-type '(signed-byte 32)) :type fixnums)
  (block8 (make-array 64 :element-type '(signed-byte 32)) :type fixnums)
  (pred8 (make-array 64 :element-type '(unsigned-byte 8)) :type octets)
  (pt8 (make-array 16 :element-type '(unsigned-byte 8)) :type octets)
  (pl8 (make-array 8 :element-type '(unsigned-byte 8)) :type octets)
  (ptl8 (make-array 1 :element-type '(unsigned-byte 8)) :type octets)
  reflist1                                      ; list 1, for a B slice
  (direct-spatial t))

(defun mb-available-p (ss dx dy)
  "Is the macroblock at (mbx+DX, mby+DY) decoded and in this slice?

   Three conditions, and the third is the one that is easy to leave out: it must be inside the
   picture, it must already be decoded, and it must belong to the SAME SLICE as the macroblock
   asking.  A slice is defined to be independently decodable, so a neighbour across a slice
   boundary is unavailable even though it is sitting right there, fully decoded, in the same
   picture buffer."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((x (+ (ss-mbx ss) dx)) (y (+ (ss-mby ss) dy))
         (pic (ss-pic ss)))
    (and (>= x 0) (>= y 0) (< x (pic-mb-width pic)) (< y (pic-mb-height pic))
         (let ((n (+ (* y (pic-mb-width pic)) x)))
           (and (/= -1 (aref (pic-mb-types pic) n))
                (= (the fixnum (aref (pic-mb-slice pic) n)) (ss-slice-id ss)))))))

;;; EVERY ONE OF THESE IS INLINE AND FULLY DECLARED, and that is the whole point of them being
;;; here.  They are read a few times per 4x4 block by motion prediction and again by the loop
;;; filter, which at 1080p is millions of calls a second; without the declarations each one is a
;;; function call returning an object of unknown type, and the index arithmetic around it is
;;; generic.  Together they were about a sixth of decode time.
(declaim (inline mb-intra-p %mv-index blk-mv blk-ref-poc set-blk-mv mb-skipped-p))
(defun mb-intra-p (pic mbx mby)
  "Was the macroblock at MBX,MBY coded intra?  Undecoded counts as not-intra and callers check
   availability separately."
  (declare (type picture pic) (type fixnum mbx mby) (optimize (speed 3) (safety 0)))
  (>= (aref (pic-mb-types pic) (+ (* mby (pic-mb-width pic)) mbx)) 0))

(defun %mv-index (pic bx by)
  "Index of the 4x4 block at picture block coordinates BX,BY in the motion arrays."
  (declare (type picture pic) (type fixnum bx by) (optimize (speed 3) (safety 0)))
  (+ (* by (* 4 (pic-mb-width pic))) bx))

(defun blk-mv (pic bx by &optional (lx 0))
  "(values mvx mvy ref) for list LX of the 4x4 block at BX,BY, in quarter-pel units."
  (declare (type picture pic) (type fixnum bx by lx) (optimize (speed 3) (safety 0)))
  (let ((i (%mv-index pic bx by)))
    (declare (type fixnum i))
    (values (aref (pic-mvs pic) (+ (* 4 i) (* 2 lx)))
            (aref (pic-mvs pic) (+ (* 4 i) (* 2 lx) 1))
            (aref (pic-refs pic) (+ (* 2 i) lx)))))

(defun blk-ref-poc (pic bx by &optional (lx 0))
  "Which picture list LX of this block came from, as its order count, or +NO-REF-POC+."
  (declare (type picture pic) (type fixnum bx by lx) (optimize (speed 3) (safety 0)))
  (aref (pic-ref-pics pic) (+ (* 2 (%mv-index pic bx by)) lx)))

(defun set-blk-mv (pic bx by mvx mvy ref &optional (lx 0) (poc +no-ref-poc+))
  (declare (type picture pic) (type fixnum bx by mvx mvy ref lx poc)
           (optimize (speed 3) (safety 0)))
  (let ((i (%mv-index pic bx by)))
    (declare (type fixnum i))
    (setf (aref (pic-mvs pic) (+ (* 4 i) (* 2 lx))) mvx
          (aref (pic-mvs pic) (+ (* 4 i) (* 2 lx) 1)) mvy
          (aref (pic-refs pic) (+ (* 2 i) lx)) ref
          (aref (pic-ref-pics pic) (+ (* 2 i) lx)) poc)))

(defun clear-blk-motion (pic bx by)
  "Mark a block as predicting from nothing, which is what an intra block does."
  (declare (type picture pic) (type fixnum bx by) (optimize (speed 3) (safety 0)))
  (setf (aref (pic-blk-direct pic) (%mv-index pic bx by)) 0)
  (dotimes (lx 2) (set-blk-mv pic bx by 0 0 -1 lx +no-ref-poc+))
  (let ((i (%mv-index pic bx by)))
    (dotimes (k 4) (setf (aref (pic-mvds pic) (+ (* 4 i) k)) 0))))

(declaim (inline blk-mvd set-blk-mvd))
(defun mb-skipped-p (pic mbx mby)
  "Was that macroblock a P_Skip?  -2 is reserved for it; other inter types are -3 and below, so
   the two are distinguishable, which the skip-flag context needs and nothing else does."
  (declare (type picture pic) (type fixnum mbx mby) (optimize (speed 3) (safety 0)))
  (= -2 (aref (pic-mb-types pic) (+ (* mby (pic-mb-width pic)) mbx))))

(defun blk-mvd (pic bx by &optional (lx 0))
  (declare (type picture pic) (type fixnum bx by lx) (optimize (speed 3) (safety 0)))
  (let ((i (%mv-index pic bx by)))
    (declare (type fixnum i))
    (values (aref (pic-mvds pic) (+ (* 4 i) (* 2 lx)))
            (aref (pic-mvds pic) (+ (* 4 i) (* 2 lx) 1)))))

(defun set-blk-mvd (pic bx by dx dy &optional (lx 0))
  (declare (type picture pic) (type fixnum bx by dx dy lx) (optimize (speed 3) (safety 0)))
  (let ((i (%mv-index pic bx by)))
    (declare (type fixnum i))
    (setf (aref (pic-mvds pic) (+ (* 4 i) (* 2 lx))) dx
          (aref (pic-mvds pic) (+ (* 4 i) (* 2 lx) 1)) dy)))

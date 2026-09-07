;;;; packages.lisp — reel: the pure Common Lisp VP8 codec.
;;;;
;;;; TWO PACKAGES, ON PURPOSE, AND THE REASON IS TYPES.  The encoder and the decoder each carry
;;;; VP8's constant tables, and they disagree about how to hold them: the encoder declaims
;;;; +COEFF-UPDATE-PROBS+ as (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (1056)) because that is the innermost
;;;; loop of a 687k-bool keyframe, and the decoder declaims the same name as (SIMPLE-ARRAY FIXNUM
;;;; (*)) because its DEC struct's slots are fixnum arrays that get COPY-SEQ'd from it and then
;;;; mutated.  Both are right for their half; neither survives being the other.  Merging them into
;;;; one package would mean re-typing one side's proven, bit-exact code to suit the other's
;;;; declaims — for the sake of saving ~10 KB of duplicated tables.
;;;;
;;;; So: #:REEL is the encoder and the public face, #:REEL.DECODE is the decoder, and the two
;;;; share nothing but the specification.  The eight duplicated tables were checked value-identical
;;;; before the split.
;;;;
;;;; The #:VP8 nickname is on #:REEL because webrtc-media's RTP payload packer reaches for the
;;;; encoder's internals as VP8::VE-PREV-Y and friends.  Those are exported below, so the reference
;;;; is now to an exported symbol rather than an internal one, and that file needed no edit.

(defpackage #:reel.decode
  (:use #:cl)
  (:export
   ;; the boolean decoder
   #:bool-dec #:bool-init #:bool-bit #:bool-literal #:bool-signed #:bool-flag
   #:bd-pos #:bd-end
   ;; the key-frame header, for a container that wants to peek without decoding
   #:frame #:fr-key-frame #:fr-version #:fr-show-frame #:fr-part0-size #:fr-width #:fr-height
   #:fr-data #:fr-off0 #:parse-frame-header #:frame-info
   ;; planes, which is what a still-image decoder above this needs to convert
   #:plane #:pl-data #:pl-stride #:pl-w #:pl-h #:pget
   ;; the key-frame pixel pipeline, as webp-pure drives it
   #:dec #:d-width #:d-height #:d-mb-cols #:d-mb-rows #:d-yplane #:d-uplane #:d-vplane
   #:decode-key-frame
   ;; the full decoder: key and inter frames, references, a picture out
   #:decoder #:make-decoder #:decode-frame #:decoder-width #:decoder-height #:decoder-frame-count
   #:picture #:picture-p #:picture-width #:picture-height #:picture-y #:picture-u #:picture-v
   #:picture-y-stride #:picture-uv-stride #:picture-y-offset #:picture-uv-offset
   #:picture-timestamp #:picture->rgb #:picture->rgb-into #:picture->yuv420
   ;; conditions
   #:vp8-error #:vp8-error-message))

(defpackage #:reel
  (:use #:cl)
  (:nicknames #:vp8)
  (:import-from #:reel.decode
   #:decoder #:make-decoder #:decode-frame #:frame-info
   #:decoder-width #:decoder-height #:decoder-frame-count
   #:picture #:picture-p #:picture-width #:picture-height #:picture-y #:picture-u #:picture-v
   #:picture-y-stride #:picture-uv-stride #:picture-y-offset #:picture-uv-offset
   #:picture-timestamp #:picture->rgb #:picture->rgb-into #:picture->yuv420)
  (:export
   ;; the boolean entropy coder (RFC 6386 s7 decoder, s13.2 encoder)
   #:make-bwriter #:bwrite-bit #:bwrite-literal #:bwrite-flag #:bwrite-finish
   #:make-breader* #:bread-bit #:bread-literal #:bread-flag
   ;; whole-frame encoder entry points
   #:encode-gray-frame #:encode-keyframe-gray #:encode-keyframe-solid #:encode-keyframe-checker
   ;; the stateful encoder: a reference frame, and inter frames against it
   #:make-encoder #:encode-inter-frame #:predict-motion
   #:ve-width #:ve-height #:ve-mb-cols #:ve-mb-rows #:ve-have-ref #:ve-stale-mbs
   #:ve-ref-y #:ve-ref-u #:ve-ref-v #:ve-prev-y #:ve-prev-u #:ve-prev-v
   ;; the bitstream parser kept as a debugging oracle
   #:parse-keyframe
   ;; the decoder's, imported above so one package is the whole codec
   #:decoder #:make-decoder #:decode-frame #:frame-info
   #:decoder-width #:decoder-height #:decoder-frame-count
   #:picture #:picture-p #:picture-width #:picture-height #:picture-y #:picture-u #:picture-v
   #:picture-y-stride #:picture-uv-stride #:picture-y-offset #:picture-uv-offset
   #:picture-timestamp #:picture->rgb #:picture->rgb-into #:picture->yuv420))


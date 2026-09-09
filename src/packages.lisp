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

(defpackage #:reel.h264
  (:use #:cl)
  (:export
   ;; the bitstream layer
   #:h264-error #:h264-error-message
   #:nal #:nal-ref-idc #:nal-type #:nal-rbsp #:nal-idr-p #:nal-slice-p
   #:annex-b-nals #:length-prefixed-nals #:parse-nal #:avcc-parameter-sets #:rbsp-from
   #:bitreader #:make-bitreader #:u1 #:ub #:ue #:se #:more-rbsp-data-p #:byte-align #:br-eof-p
   #:+nal-slice+ #:+nal-idr+ #:+nal-sps+ #:+nal-pps+ #:+nal-sei+
   ;; the headers
   #:sps #:parse-sps #:sps-id #:sps-profile #:sps-level #:sps-width #:sps-height
   #:sps-mb-width #:sps-mb-height #:sps-log2-max-frame-num #:sps-poc-type #:sps-max-ref-frames
   #:sps-crop-left #:sps-crop-right #:sps-crop-top #:sps-crop-bottom
   #:pps #:parse-pps #:pps-id #:pps-sps-id #:pps-cabac #:pps-init-qp #:pps-chroma-qp-offset
   #:pps-deblocking-control #:pps-constrained-intra #:pps-chroma-qp-offset-for
   #:slice-header #:parse-slice-header #:slice-type-name #:sh-i-slice-p #:sh-p-slice-p #:sh-b-slice-p
   #:sh-first-mb #:sh-slice-type #:sh-frame-num #:sh-qp #:sh-sps #:sh-pps #:sh-nal
   #:sh-disable-deblocking #:sh-alpha-offset #:sh-beta-offset
   ;; residual decoding
   #:residual-block #:+zigzag-4x4+ #:+dequant-coeff+ #:+dequant-class+ #:+qpc-from-qpy+
   ;; the decoder
   #:decoder #:make-decoder #:feed-nal #:decode-picture #:decode-annex-b
   #:flush-decoder #:pending-pictures #:pic-poc #:decode-independent #:split-access-units #:access-units-independent-p #:default-decode-threads
   #:decoder-width #:decoder-height
   #:picture #:pic-width #:pic-height #:pic-y #:pic-u #:pic-v #:picture->yuv420
   #:deblock-picture #:as-picture))

(defpackage #:reel.theora
  (:use #:cl)
  (:export
   #:theora-error #:theora-error-message
   #:info #:parse-headers #:parse-identification #:parse-setup
   #:inf-width #:inf-height #:inf-picture-width #:inf-picture-height
   #:inf-offset-x #:inf-offset-y #:inf-fps-num #:inf-fps-den #:inf-version
   #:decoder #:make-theora-decoder #:decode-frame
   #:frame #:fr-width #:fr-height #:fr-cwidth #:fr-cheight #:fr-keyframe
   #:fr-y #:fr-u #:fr-v #:fr-ystride #:fr-cstride #:fr-timestamp #:as-picture))

(defpackage #:reel.vp9
  (:use #:cl)
  (:export
   #:vp9-error #:vp9-error-message
   #:split-superframe
   #:header #:parse-header
   #:h-profile #:h-keyframe #:h-show-frame #:h-show-existing #:h-intra-only
   #:h-width #:h-height #:h-render-width #:h-render-height
   #:h-base-q #:h-lossless #:h-filter-level #:h-refresh-mask
   #:h-log2-tile-cols #:h-log2-tile-rows #:h-compressed-size #:h-header-bytes #:h-frame-context
   #:read-compressed-header #:make-default-context #:fp-tx-mode #:fp-comp-pred-mode
   #:make-state #:decode-tiles #:st-blocks #:st-tile-slack #:st-frame
   #:decoder #:make-vp9-decoder #:decode-frame #:d-comp-blocks
   #:frame #:fr-width #:fr-height #:picture->yuv420 #:as-picture))

(defpackage #:reel.ffv1
  (:use #:cl)
  (:export
   #:ffv1-error #:ffv1-error-message
   #:config #:parse-configuration #:cfg-version #:cfg-width #:cfg-height #:cfg-colorspace
   #:cfg-bits #:cfg-h-slices #:cfg-v-slices #:cfg-ac #:cfg-chroma-h-shift #:cfg-chroma-v-shift
   #:decoder #:make-ffv1-decoder #:make-ffv1-decoder-for-frames #:decode-frame
   #:parse-frame-header
   #:frame #:fr-width #:fr-height #:fr-cwidth #:fr-cheight
   #:fr-y #:fr-u #:fr-v #:fr-ystride #:fr-cstride #:picture->yuv420 #:as-picture))

(defpackage #:reel.mpeg4
  (:use #:cl)
  (:export
   #:mpeg4-error #:mpeg4-error-message
   #:find-start-code #:bitreader #:make-br
   #:vol #:parse-vol-header #:vol-width #:vol-height #:vol-mb-width #:vol-mb-height
   #:vop #:parse-vop-header #:vop-coding-type #:vop-type-name
   #:decoder #:make-decoder #:feed-bytes #:flush-decoder #:decode-elementary-stream
   #:decoder-width #:decoder-height
   #:frame #:fr-width #:fr-height #:fr-y #:fr-u #:fr-v #:fr-ystride #:fr-cstride
   #:fr-coding-type #:fr-timestamp #:picture->yuv420 #:as-picture))

(defpackage #:reel.mpeg2
  (:use #:cl)
  (:export
   #:mpeg2-error #:mpeg2-error-message
   ;; the bitstream layer
   #:find-start-code #:bitreader #:make-br #:read-bits #:read-bit #:peek-bits
   ;; the headers
   #:sequence-header #:parse-sequence-header #:seq-width #:seq-height #:seq-mb-width
   #:seq-mb-height #:seq-frame-rate #:seq-mpeg2-p #:seq-progressive-p
   #:picture-header #:ph-coding-type #:ph-structure #:picture-type-name
   ;; the decoder
   #:decoder #:make-decoder #:feed-bytes #:flush-decoder #:decode-elementary-stream
   #:decoder-width #:decoder-height
   #:frame #:fr-width #:fr-height #:fr-y #:fr-u #:fr-v #:fr-ystride #:fr-cstride
   #:fr-coding-type #:fr-timestamp #:picture->yuv420 #:as-picture
   ;; the inverse transform, which MPEG-4 Part 2 shares exactly
   #:idct-8x8 #:idct-put #:idct-add #:clamp255))

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


;;; HEVC / H.265.  Its own package for the same reason every other codec here has one: the names
;;; collide with H.264's almost everywhere — SPS, PPS, PARSE-SLICE-HEADER, MAKE-BITREADER — and the
;;; two mean different things by them.
(defpackage #:reel.hevc
  (:use #:cl)
  (:export
   #:hevc-error #:hevc-error-message
   ;; the bitstream layer
   #:nal #:nal-type #:nal-layer-id #:nal-temporal-id #:nal-rbsp
   #:nal-slice-p #:nal-irap-p #:nal-idr-p #:nal-bla-p #:nal-reference-p #:nal-base-layer-p
   #:annex-b-nals #:length-prefixed-nals #:parse-nal #:hvcc-parameter-sets #:rbsp-from
   #:bitreader #:make-bitreader #:u1 #:ub #:ue #:se #:byte-align #:more-rbsp-data-p
   ;; parameter sets
   #:sps #:parse-sps #:sps-id #:sps-width #:sps-height
   #:sps-display-width #:sps-display-height
   #:sps-chroma-format #:sps-bit-depth-luma #:sps-bit-depth-chroma
   #:sps-ctb-log2 #:sps-ctb-size #:sps-ctbs-wide #:sps-ctbs-high #:sps-ctbs
   #:sps-min-cb-log2 #:sps-min-tb-log2 #:sps-max-tb-log2
   #:sps-sao-enabled #:sps-amp-enabled #:sps-pcm-enabled #:sps-strong-intra-smoothing
   #:sps-ptl #:ptl-profile-idc #:ptl-level #:ptl-tier
   #:pps #:parse-pps #:pps-id #:pps-sps-id
   #:decode-slice-data #:ctx #:cx-n-cu #:cx-n-tu #:cx-n-coeff
   #:picture #:make-picture-for #:picture->yuv420 #:pic-disp-width #:pic-disp-height
   #:deblock-picture #:sao-picture
   #:make-hevc-decoder #:feed-nal #:flush-decoder #:decode-annex-b #:pic-poc
   #:slice #:parse-slice-header #:sh-type #:sh-qp #:sh-first-in-pic #:sh-dependent
   #:sh-segment-address #:sh-poc-lsb #:sh-i-slice-p #:sh-p-slice-p #:sh-b-slice-p))

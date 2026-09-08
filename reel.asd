;;;; reel.asd — the pure Common Lisp VP8 codec.
;;;;
;;;; reel is to video what pigment is to images and reed is to audio: the codec, with no
;;;; transport and no container above it and no FFI under it.  webrtc-media packetizes what
;;;; this encodes; cassette demuxes the containers this decodes out of; webp-pure is a still
;;;; image that happens to be one VP8 key frame.  None of them are dependencies of this.

(asdf:defsystem :reel
  :description "VP8 in pure Common Lisp: a key-frame and inter-frame encoder with motion
estimation, and a complete decoder (RFC 6386) — boolean coder, transforms, intra and inter
prediction, six-tap motion compensation, the loop filter, and reference-frame management.
Verified bit-exact against ffmpeg/libvpx in both directions."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  ;; NOTHING.  The codec is arithmetic on octet vectors; a dependency here would be a dependency
  ;; for every image, video and transport user above it.
  :depends-on ()
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     ;; ---- the encoder, and the boolean coder both halves are built on
     (:file "tables")       ; VP8's constant probability tables (RFC 6386 s13)
     (:file "bool")         ; the boolean entropy coder: writer and reader
     (:file "dct")          ; forward DCT/WHT and quantization
     (:file "tokens")       ; coefficient token coding
     (:file "frame")        ; the key-frame header and whole-frame encoders
     (:file "encode")       ; the real encoder: prediction, reconstruction, DC+AC
     (:file "inter")        ; inter frames: motion search, global vectors, rate control
     (:file "parse")        ; a bitstream parser, kept as a debugging oracle
     ;; ---- the decoder, in its own package (see src/packages.lisp for why)
     (:module "decode"
      :serial t
      :components
      ((:file "tables")        ; the same tables again, as fixnum arrays the DEC struct copies
       (:file "bool")          ; frame header + boolean decoder
       (:file "transform")     ; inverse DCT and WHT
       (:file "intra")         ; the key-frame pixel pipeline
       (:file "loopfilter")    ; the in-loop deblocking filter
       (:file "inter-tables")  ; mode contexts, MV probabilities, sub-pixel filters
       (:file "inter")))     ; inter frames, references, and the decoder proper
     ;; ---- H.264 / AVC, its own package again and for the same reason: a second codec
     (:module "h264"
      :serial t
      :components
      ((:file "bits")         ; NAL units, RBSP, the Exp-Golomb bit reader
       ;; the pure-data files come first: the parameter sets need the scan order and the default
       ;; scaling matrices to turn a transmitted scaling list into something dequantisation can use
       (:file "tables")       ; the CAVLC VLC tables, the scan order, the dequant tables
       (:file "scaling-tables")  ; GENERATED: the default scaling matrices
       (:file "transform8-tables") ; GENERATED: the 8x8 scan and normalisation
       (:file "params")       ; sequence and picture parameter sets, slice headers
       ;; THE PICTURE STRUCT COMES EARLY ON PURPOSE.  Motion compensation reads a reference
       ;; picture's slots in its innermost loop; a file compiled before the DEFSTRUCT is seen
       ;; cannot inline those reads and does not know what type they return, so the arithmetic on
       ;; them goes generic.  It measured at more than half the decode time at 1080p.
       (:file "picture")      ; the decoded picture, the slice state, and per-block accessors
       (:file "cavlc")        ; residual blocks: coeff_token, levels, runs
       (:file "cabac-tables") ; GENERATED: the normative CABAC constants
       (:file "transform")    ; dequantisation, the inverse 4x4 and the DC transforms
       (:file "transform8")   ; the 8x8 transform, High profile's other half
       (:file "intra")        ; the nine 4x4, four 16x16 and four chroma prediction modes
       (:file "intra8")       ; the nine 8x8 modes, over filtered reference samples
       (:file "motion")       ; inter prediction: vector prediction and quarter-pel resampling
       (:file "slice")        ; the macroblock layer and the slice loop
       (:file "cabac")        ; the arithmetic decoder, and the syntax read through it
       (:file "deblock-tables")  ; the loop filter's alpha/beta/tc0 thresholds
       (:file "deblock")      ; the in-loop deblocking filter
       (:file "decode")      ; NAL units in, pictures out
       (:file "parallel"))    ; independent pictures, decoded at the same time
      )
     ;; ---- MPEG-1 and MPEG-2 video, which are one bitstream with one of them extended
     (:module "mpeg2"
      :serial t
      :components
      ((:file "tables")      ; GENERATED: the scans, the weight matrices and eight Huffman tables
       (:file "bits")        ; start codes, the bit reader, and the Huffman machinery
       (:file "headers")     ; sequence, group, picture, and the extensions that make it MPEG-2
       (:file "idct")        ; the inverse transform, and why it cannot be bit-exact by definition
       (:file "motion")      ; half-pel prediction
       (:file "slice")       ; the macroblock layer
       (:file "decode"))    ; start codes in, pictures out
      )
     ;; ---- MPEG-4 Part 2: the codec behind DivX and XviD
     (:module "mpeg4"
      :serial t
      :components
      ((:file "tables")      ; GENERATED: the scans, the matrices and seven Huffman tables
       (:file "bits")        ; start codes, the bit reader, the Huffman machinery
       (:file "headers")     ; the video object layer and the video object plane
       (:file "slice")       ; the macroblock layer: prediction, motion, coefficients
       (:file "decode"))   ; start codes in, pictures out
      )
     ;; ---- FFV1, the lossless codec archives keep masters in
     (:module "ffv1"
      :serial t
      :components
      ((:file "rangecoder")  ; the binary range coder, and integers spelled through it
       (:file "golomb")      ; the OTHER entropy coder: adaptive Rice codes and a run mode
       (:file "decode"))   ; the configuration record, slices, and prediction
      )
     ;; ---- VP9, headers only for now: the codec itself is refused
     (:module "vp9"
      :serial t
      :components
      ((:file "tables")      ; GENERATED: the quantiser lookups and the symbol trees
       (:file "bits")        ; the plain bit reader, and VP8's arithmetic coder above it
       (:file "probs")       ; GENERATED: the default probability models
       (:file "headers")     ; the superframe index and the uncompressed frame header
       (:file "compressed")  ; the second header, read through the arithmetic coder
       (:file "counts")      ; symbol counts, and the backward adaptation they feed
       (:file "scans")       ; GENERATED: the coefficient scans and their neighbour tables
       (:file "picture")     ; the decoded picture, ahead of everything that writes into it
       (:file "transform")   ; GENERATED (one dimension at a time) plus the two-pass wrapper
       (:file "intra")        ; the fifteen intra prediction modes at four sizes
       (:file "loopfilter")  ; the deblocking filter, and the masks that say where it goes
       (:file "filters")     ; GENERATED: the three eight-tap interpolation filters
       (:file "mc")          ; motion compensation, and the average two references make
       (:file "intertab")    ; GENERATED: the inter-mode contexts and the vector search order
       (:file "refctx")      ; GENERATED: which probability a reference decision uses
       (:file "inter")       ; inter blocks: references, modes, and motion vectors
       (:file "block")       ; tiles, the partition quadtree, block modes and coefficients
       (:file "recon")       ; edge samples, prediction and the inverse transform
       (:file "decode"))     ; a packet in, pictures out, and the eight reference slots
      )
     ;; ---- Theora, a VP3 descendant, in Ogg
     (:module "theora"
      :serial t
      :components
      ((:file "tables")      ; GENERATED: the fixed codes and VP3's defaults
       (:file "bits")        ; the bit reader and two kinds of Huffman table
       (:file "headers")     ; the three packets that configure a decoder
       (:file "decode")))))));  superblocks, modes, vectors, coefficients, and the picture

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
       (:file "inter")))))))   ; inter frames, references, and the decoder proper

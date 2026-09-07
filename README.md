# reel

**Video codecs in pure Common Lisp.** VP8 both directions — an encoder built for real-time
desktop streaming, and a decoder bit-exact with libvpx — plus an H.264 decoder that is
bit-exact with ffmpeg on intra-only streams. No FFI, and no dependencies at all: a codec is
arithmetic over octet vectors, and a dependency here would be one for every image, video and
transport user above it.

*A reel is what film is wound on.* It is the video sibling of
[pigment](../pigment) (images) and [reed](../reed) (audio); the shell that holds reels and
hands their tracks back in step is [cassette](../cassette).

## Where it came from

Both halves of VP8 existed already and neither was in a codec library. The encoder was
eight files inside [webrtc-media](../webrtc-media), which is a *transport* — so encoding a
frame to a file meant loading SRTP to do it. The decoder's intra half was inside
[webp-pure](../webp-pure), because a lossy WebP is exactly one VP8 key frame — so decoding
video meant depending on an *image* library. This repo is those two halves in the one place
they both belong, plus the inter-frame decoding that neither had.

## What it does

| | |
|---|---|
| **Decode** | Key *and* inter frames (RFC 6386): boolean decoder, coefficient tokens, intra prediction, inverse DCT/WHT, six-tap and bilinear motion compensation, split motion vectors, golden/altref references, per-macroblock loop-filter deltas, probability persistence. **Bit-exact with ffmpeg/libvpx** on the libvpx-encoded vectors and on Big Buck Bunny at 640x360, ~45 fps on one core. |
| **Encode** | Key frames with real DC+AC coefficients, and inter frames with skip, ZEROMV and a global motion vector, under a byte budget with deferral. Verified by ffmpeg and decodable by Safari. |
| **Parse** | A bitstream reader kept as a debugging oracle. |
| **H.264** | *Decode only, intra only.* Constrained Baseline, CAVLC, 4:2:0, 8-bit, progressive: NAL and RBSP handling, Exp-Golomb, SPS/PPS/slice headers, CAVLC residuals, the integer 4x4 transform and both Hadamard DC transforms, all nine Intra4x4 modes plus Intra16x16 and chroma, and the in-loop deblocking filter. **Bit-exact with ffmpeg's filtered output** on every frame of five fixtures and on an MP4's video track, at about 35 fps on 640x360. P and B slices, CABAC, MBAFF and FMO are refused loudly rather than decoded wrong. |

## Three packages, and why

`#:reel` is the encoder and the public face; `#:reel.decode` is the decoder. They share
nothing but the specification, and the reason is types: the encoder declaims VP8's
probability tables `(unsigned-byte 8)` because that is the innermost loop of a 687k-bool
key frame, and the decoder declaims the same names `fixnum` because its state struct
copies from them and then mutates. Both are right for their half and neither survives
being the other. The eight duplicated tables were checked value-identical before the
split; the cost of keeping two copies is about 10 KB.

`#:reel` re-exports the decoder's entry points, so a caller needs one package. The `#:vp8`
nickname is on `#:reel` because webrtc-media's RTP packer reaches for encoder internals as
`vp8::ve-prev-y` and friends; those are exported now, and that file needed no edit.

`#:reel.h264` is the third, and it shares nothing with either — a different specification,
a different entropy coder, a different transform. What it does share is the *output*:
`reel.h264:as-picture` hands back a `reel.decode:picture`, so a caller downstream has one
picture type whichever codec produced it. See `src/h264/NOTES.md` for what is verified,
which three bugs cost the most time, and what remains.

## Using it

```lisp
(asdf:load-system :reel)

;; decode
(let ((d (reel:make-decoder)))
  (multiple-value-bind (picture shown) (reel:decode-frame d frame-octets)
    (when shown (reel:picture->rgb picture))))       ; or picture->yuv420

;; peek without decoding
(reel:frame-info frame-octets)                        ; => key-p show-p width height version

;; encode
(reel:encode-gray-frame luma width height :qi 8 :u chroma-u :v chroma-v)
```

`decode-frame` takes any octet vector. A picture shares planes with the decoder's
reference buffers and stays valid until the frame after the next one, so copy it out if
you keep it.

## Tests

```sh
sbcl --dynamic-space-size 2048 --non-interactive --load t/roundtrip.lisp
```

That one needs nothing but reel, and it is deliberately **not** a conformance test: two
halves of one codebase agreeing proves only that they agree. Conformance is asserted
downstream where ffmpeg is already a fixture — `cassette/inspect/test-decode.lisp` for the
decoder against libvpx over eight clips, `cassette/inspect/test-encode-mux.lisp` for
ffmpeg decoding what this encoder produced, and `cassette/inspect/test-h264.lisp` for the
H.264 decoder against ffmpeg.

## Known

- **`encode-keyframe-solid` does not produce a solid frame**, at any quantizer. ffmpeg
  decodes it to the same wrong pixels this decoder does, so the defect is the encoder's.
  It has no callers; the production paths are `encode-gray-frame` and `encode-inter-frame`,
  which round-trip exactly. `t/roundtrip.lisp` asserts the broken behaviour as a known
  state, so a fix makes that line fail and say so.
- **Two W3C web-platform-test clips decode with small differences** (±1 on skipped ZEROMV
  macroblocks, larger on one B_PRED macroblock inside an inter frame). The partition parse
  stays in sync, so it is a reconstruction detail rather than a desync. Vectors and the
  comparison harness are in cassette.
- No VP9, no AV1. The encoder emits integer-pel motion vectors only.

# H.264 — what decodes, what does not, and where the edges are

**Status: intra only, and bit-exact within that.** Constrained Baseline, CAVLC, 4:2:0, 8-bit,
progressive, I slices. Every frame of every fixture matches ffmpeg's ordinary output — deblocking
filter and all — sample for sample. What is not supported is refused loudly in `params.lisp`
rather than decoded wrong.

Intra-only is a real class of file: anything encoded with `-g 1`, a screen recorder's
keyframe-only output, the first frame of everything. It is also the whole foundation for inter
frames, which need all of this plus motion compensation and reference lists.

## Verified

Compare against ffmpeg's *filtered* output, which is what a conforming decoder produces. While the
loop filter was being written it was useful to compare against `ffmpeg -skip_loop_filter all`
instead, which is the reconstruction before filtering; if a change ever breaks the test, that flag
tells you in one run whether the damage is in the filter or under it.

`cassette/inspect/test-h264.lisp` is the test. Five Annex B streams spread deliberately across a
macroblock-aligned size and a cropped one, a fine quantiser and a coarse one, synthetic bars and a
mandelbrot, plus 320x240 so more than a handful of macroblocks are in play. Then twenty frames of
an MP4 through the container, and a B-frame stream that must be refused rather than decoded wrong.

## Three bugs worth remembering

These cost the most time, and each one is invisible in a way that matters.

**The flat weightScale.** `LevelScale` carries a factor of 16 for the flat (non-scaling-list) case.
Missing it put luma at 0.3% exact. Adding it took luma to 35%.

**Both chroma DC blocks come first.** 7.3.5.3 orders the chroma residual Cb-DC, Cr-DC, Cb-AC,
Cr-AC — not Cb-DC, Cb-AC, Cr-DC, Cr-AC. Getting it wrong is a genuine bitstream desync and shows up
as "no CAVLC code matched in 17 bits" some macroblocks later, nowhere near the cause.

**dcPredModePredictedFlag is per macroblock, not per neighbour.** 8.3.1.1: when *either* neighbouring
macroblock is unavailable, *both* predicted modes become DC. Applying it per neighbour instead —
substituting DC only for the missing side — left luma at 3.5% exact. This one is nasty because a
wrong prediction mode corrupts pixels *without desyncing the bitstream*: the flag/remainder bit
count is identical either way. That is exactly why chroma stayed bit-exact through it, and why the
symptom looked like a transform bug rather than a parsing one.

**The 4x4 Hadamard's output order.** After the mode fix, frame 0 was exact and frames 1-4 drifted.
`luma-dc-transform`'s butterfly had outputs 1 and 3 swapped; the correct row is
`[s0+s1, s0-s1, s2-s3, s2+s3]`. Invisible whenever only the DC coefficient is nonzero, which is
every block in a flat first frame.

## The tool for the next bug

`%guide` in `slice.lisp` tries all nine Intra4x4 modes per block against ffmpeg's unfiltered output
and reports which one the encoder actually chose. It can write the true mode back, so corruption
does not propagate and every later block yields clean data instead of noise. Bind `*oracle*` to
ffmpeg's luma plane and `*oracle-w*` to its width.

## Not started

P slices — they parse, and are refused. Inter prediction, reference lists, quarter-pel motion
compensation, and the inter boundary-strength derivation the deblocking filter would then need.
That is the larger half of Constrained Baseline. B slices, CABAC, MBAFF and FMO are outside
Constrained Baseline and are refused too.

## Performance

Through the container, with the read-ahead batch on:

| | serial, before any of this | now |
|---|---|---|
| 640x360 | 20.6 fps | 117.7 |
| 720p | — | 94.5 |
| 1080p | — | 70.9 |

Single-threaded it is 38 fps at 640x360 and 10.6 at 1080p; the rest is parallelism. Allocation is
0.87 MB per picture, down from 3.83.

**Where the single-thread wins came from.** In order of what each was worth:

| change | why it mattered |
|---|---|
| CAVLC lookup tables | the table walk read a bit and rescanned every entry, up to 68 deep and 16 bits long — 22% of decode. Peeking the row's longest code and indexing directly costs ~87k entries total, under 200 KB. |
| `declaim` on the tables | they are special variables, so every `aref` on one was `HAIRY-DATA-VECTOR-REF`, a generic dispatch per lookup. |
| typed `picture` and `slice-state` slots | every sample the decoder touches goes through those slots; untyped, each access dispatched generically. |
| four specialised deblocking loops | `chroma-p` and "is bS 4" are constant per edge but were tested per line, and at 1080p the filter is a third of decode: 1.6 million filtered lines a picture. |
| a byte-gathering `br-peek` | reading a bit at a time cost an array reference per bit, sixteen for one CAVLC code. |
| narrowed arithmetic in `dequant-4x4` | fixnum times fixnum may be a bignum, so `fixnum` alone still called generic multiply and shift. |
| bounded dequantisation, DC-only transform | `residual-block` now reports its highest scan position, so neither walks 16 positions for a block holding two. A lone DC coefficient makes the inverse transform a `fill`. |
| `(speed 3)` on the hot functions | most had type declarations but no policy, so the default kept them slow. |

Almost all of that is *telling the compiler what was already true*, not changing an algorithm — the
CAVLC tables are the one real algorithm change. None of it altered an output sample, which is why
the bit-exactness test could be run after every step, and was.

**Parallelism** is in `parallel.lisp` and is worth more than everything above put together, but it
is not general: it decodes whole pictures at once, which is legal only because an I picture is
independent of every other. It checks rather than assumes — every access unit must be all-I slices
and carry no parameter set — and hands back NIL for anything else, so an inter-coded file simply
takes the serial path. **The day P slices land, this stops applying** and the honest replacement is
slice-level or wavefront parallelism inside a picture.

Two bugs in it are worth remembering because both failed *quietly*, by falling back rather than
breaking. The independence check asked "is this an I slice?" of every NAL including SEI, so every
real stream answered no. And the processor count came from an SBCL internal alien symbol that did
not resolve, so a 116-core machine silently decided it had four.

`%vlc` stays at `safety 1` deliberately. Every index in it is bounded by construction, but this
decodes files off the internet and the checks measured at about 3%.

# What plays, what does not, and why

This is the map of the media stack: [reel](.) decodes pictures, [cassette](../cassette) holds the
containers they arrive in, [reed](../reed) does audio. It exists because "can it play this file?" is
the only question anyone actually asks, and the answer lives in three repositories.

It began as a road map with six gaps in it. They are all closed. What is left below is the record of
what plays, what is turned away and on what grounds, and the two or three things that would be worth
doing next if anything were.

Proportions below are from experience, not from measuring a corpus. Treat them as ordering, not
as data.

## Plays today

| | verified against |
|---|---|
| **VP8**, key and inter frames | libvpx, bit-exact on eight clips and on Big Buck Bunny |
| **H.264 Baseline, Main and High** | ffmpeg, bit-exact on twenty-two fixtures, on a 7672-frame YouTube file and on 300 frames of 640x360 High profile, every frame |
| **MPEG-1 and MPEG-2 video** | ffmpeg, bit-exact on thirteen fixtures, every frame |
| **MPEG-4 Part 2** (DivX, XviD) | ffmpeg, bit-exact on eleven fixtures, every frame |
| **WebM and Matroska** | they are the same demuxer; `.mkv` with H.264 decodes today |
| **MP4**, including fragmented | ffprobe, packet for packet |
| **MPEG program and transport streams** | `.mpg`, `.vob`, `.ts` open and play, end to end |
| **AVI**, including OpenDML | `.avi` opens and plays, whatever codec is inside |
| **FFV1**, the preservation codec | ffmpeg, bit-exact on twelve configurations, every frame |
| **Theora** in Ogg | ffmpeg, bit-exact on six fixtures, every frame |
| **VP9** | ffmpeg, bit-exact on seven clips, every picture |
| **Opus, AAC, MP3, MPEG audio Layer II, Vorbis, FLAC, AC-3, G.711** | reed's own suites; FLAC bit-exact on sixteen fixtures and against the MD5 the encoder recorded |

H.264 covers CAVLC and CABAC, P and B slices, both direct modes, weighted and implicit weighted
prediction, reference list reordering, adaptive reference marking, the 8x8 transform, Intra_8x8,
scaling matrices, and the in-loop filter.

That set is what x264 produces with no options at all, which is the point: High is x264's default
profile, so most H.264 encoded since about 2010 is High rather than Main.

Speed, on one machine and one synthetic source, so read it as ratios and not as a specification.
Every decoder, one thread, 640x360, sixty pictures, CPU time, best of several runs — before and
after the type work described below:

| | before | after |
|---|---|---|
| MPEG-2 | 283 fps | 611 |
| MPEG-4 Part 2 | 255 fps | 556 |
| Theora | 308 fps | 398 |
| VP8 | 184 fps | 348 |
| VP9 | 125 fps | 238 |
| H.264, P and B pictures | 43 fps | 57 |
| FFV1, lossless | 23.5 fps | 32 |

At other sizes: VP9 goes 31 to 60 fps at 1280x720 and 13.7 to 27 at 1920x1080; VP8 11 to 15 at
1920x1080; H.264 with P and B pictures runs about 6.5 fps at 1920x1080. FFV1 is last because it is
lossless — it carries every sample of every picture and there is nothing to skip.

Pictures are decoded concurrently when they are independently decodable, which today means
all-intra; on an idle machine that was worth about eight times on sixteen threads. A stream with P
or B pictures is a chain and gets the serial walk, so it gains nothing — and that is every real
stream, which is why per-picture concurrency is still on the list at the end rather than off it.

**Where all of that came from, and it is nearly all one thing.** SBCL stores a `fixnum` array as
`(signed-byte 64)`, so a value read out of one has a type the compiler cannot prove is a fixnum —
a fixnum is sixty-two bits — and every sum, product or shift of two such values compiled to an
out-of-line call into the generic arithmetic. A video decoder is nothing but sums of values loaded
from arrays. Declaring the arrays thirty-two bits wide, which is what they all hold, and the plane
strides and offsets twenty-six, was most of the table above; the rest was bounding a handful of
variable shifts so they could become machine shifts, and one file — VP8's loop filter, thirty-eight
per cent of a VP8 decode — that turned out to carry no optimisation declaration at all.

VP8 then went further, because its planes were holding one sample per machine word: an octet plane
is 3.1 MB at 1920x1080 where a thirty-two bit one is 12.3 and the `fixnum` one it started as was
24.6. That is worth about 1.25 times at 640x360 and 1.37 at 1920x1080 — the gain grows with the
picture, which is what a memory-traffic change looks like. Its deblocking filter then took an edge
per call rather than a line, and stopped converting every sample to signed and back for arithmetic
in which the offsets cancel. `src/decode/NOTES.md` has all of it, including the two further steps
that were costed and declined.

Nothing about the decoded pictures changed: every fixture in every suite is still bit-exact against
ffmpeg. `src/vp9/NOTES.md` has the long version, including the two changes that were measured,
found slower than what they replaced, and reverted.

MPEG-1 and MPEG-2 are one decoder, because they are one bitstream: an MPEG-2 sequence header is byte
for byte an MPEG-1 one and everything MPEG-2 added arrives afterwards in extensions. It covers I, P
and B pictures, both scan orders, both quantiser ladders, custom weight matrices, and interlaced
coding — field DCT and field motion inside a frame picture, which is what DVDs are.

Its inverse transform is the one place in the stack where bit-exactness is not a meaningful goal:
MPEG-2 requires only the accuracy of IEEE 1180, so two conforming decoders may legitimately differ
by one in a sample. This one matches ffmpeg's `simple` transform exactly, which is what lets the
comparison mean something; `src/mpeg2/idct.lisp` says which three parts of its arithmetic are not
what the mathematics alone would suggest.

MPEG-4 Part 2 covers Simple and Advanced Simple: one and four motion vectors, both quantisers, B
pictures with direct mode, video packets, and quarter-sample motion. That is what DivX and XviD
produce.

Theora is VP3 with a header: On3's 2001 codec, frozen by Xiph in 2004 and unchanged since. It
covers key and inter pictures, golden frames, all four motion vector modes including four vectors
per macroblock, the per-block quantiser indices, and the loop filter. `.ogv` opens and plays.

FFV1 covers versions 0, 1 and 3, both entropy coders, 4:2:0, 4:2:2 and 4:4:4, any slice layout,
either state table, and lossless RGB through the reversible colour transform. `-level 3 -coder 1` is
the command national libraries actually use; `-coder 0` is what ffmpeg does when nobody says, and
versions 0 and 1 keep their header in the first key frame rather than in the container, which is why
such a file has an empty CodecPrivate and configures the decoder as it plays.

## Refused, and refused loudly

Anything below is turned away with a reason rather than decoded wrong. That distinction is the
whole discipline of this decoder: a wrong picture that keeps playing is worse than no picture,
because nobody knows to disbelieve it.

- H.264: more than 8 bits per sample, 4:2:2 and 4:4:4, MBAFF and field coding, FMO, long-term
  references, and `memory_management_control_operation` 5. Each is refused on the FLAG that turns it
  on rather than on the profile that permits it, so a High profile stream using none of them decodes
  here.
- MPEG-2: field pictures, dual-prime motion vectors, and anything but 4:2:0.
- MPEG-4 Part 2: sprites and global motion, interlaced objects, data partitioning, scalability, and
  arbitrary shapes. Also Microsoft's pre-standard MPEG-4 variants (DIV3, MP42), which share a name
  and not a bitstream.
- Theora: chroma layouts other than 4:2:0, bitstreams older than 3.2.0 (which stored the picture
  upside down relative to everything since), and VP4 — which ffmpeg decodes with the same code and
  which shares with Theora a name and not a bitstream.
- FFV1: version 2, which was experimental and never shipped; more than 8 bits per sample; and Bayer. The player additionally
  refuses anything but 4:2:0, because one picture type serves every codec here and it is 4:2:0 —
  the decoder itself handles the others.
- VP9: profiles 1, 2 and 3 — anything but eight bits and 4:2:0, refused on the profile field, and an
  sRGB colour space refused on its own — and prediction from a reference of a different size.
- DTS, which a DVD may carry instead of MPEG audio or AC-3. A file's video plays and the audio
  track is named as undecodable rather than guessed at.
- Enhanced AC-3, which is a different format wearing AC-3's sync word, refused on its bitstream id.
- Vorbis floor 0, the line spectral pair representation. No encoder in use has emitted one since
  the format was frozen and libvorbis never has, so it is refused by name rather than half-written.

## The gaps

None left. One reopened briefly: this document claimed Vorbis played when there was no Vorbis
decoder anywhere in the stack — Ogg and Matroska both recognise the track and name it, which is
exactly why nothing ever failed loudly enough to catch it. It was found by costing what to build
next rather than by a test, which is its own small lesson about which claims here are load-bearing.
There is a Vorbis decoder now, and a `.webm` from before about 2013 — VP8 and Vorbis, the original
pairing — plays with its sound.

What was here — H.264 High profile, MPEG-2 in program and transport streams, AVI with MPEG-4 Part
2, FFV1, Theora, VP9 — is above.

### VP9 — the last one, and the largest

A hundred and ninety pictures across seven clips decode bit-exact against ffmpeg: 176x144, 352x288
with a switchable transform mode, 1280x720 across four tile columns, a lossless encode, a stream
with alt-ref frames, one that refreshes its probabilities by backward adaptation rather than
forward, and one where fifty-three of sixty-five inter frames predict from two references at once.
`.webm` with VP9 opens and plays.

That covers both headers, superframes, tiles, the partition quadtree, every block mode, every
coefficient, all four transform sizes with both the DCT and the ADST, the fifteen intra predictors,
motion vector prediction with its eight-neighbour search, the three eight-tap interpolation filters,
the eight reference slots, compound prediction, the loop filter with all three of its widths, and
both ways a frame may refresh its probability context.

The one thing left out is prediction from a reference of a different size, which is refused with the
sizes named.

## Measured against the standards, not against ffmpeg

Every fixture above is something ffmpeg, x264 or libvpx produced. A mainstream encoder uses a
narrow slice of the standard it implements, so bit-exactness against its output is a weaker claim
than it looks. `t/fetch-conformance.sh` pulls the official suites; here is what they say.

| | result |
|---|---|
| **VP8**, the seventeen official libvpx vectors | **17 of 17 bit-exact**, 833 frames |
| **VP9**, the official feature vectors | 29 of 34 bit-exact; 11 profile 1/2/3 streams correctly refused |
| **H.264**, a spread of 37 JVT conformance streams | 17 bit-exact, 15 refused, 5 wrong |
| **AC-3**, three real Dolby-encoded tracks | two at 0.9998; one is a near-silent excerpt where correlation measures rounding |

What that exercise found, none of which the synthetic corpus could:

- **VP9 had three interpolation filters and the format has four.** Bilinear is used at very low bit
  rates and in speed-oriented encoder modes, so no ordinary encode contains one. Worse, the frame
  header's "switchable" flag was stored in the same field as the filter number, using 3 as its
  sentinel — and 3 is bilinear. Fixed.
- **VP9 threw away the state a frame is supposed to inherit.** The loop filter deltas and the
  segmentation feature data persist between frames; only `setup_past_independence` — a key frame, an
  intra-only frame that asks for a reset, or an error-resilient frame — returns them to their
  defaults. Building the header struct fresh each frame reset both silently, which is correct for a
  stream where every frame updates them and wrong for one that sets them once and relies on it.
  Fixed, along with a related slip: a segmentation feature that is switched off must have its data
  zeroed, or it keeps acting through whatever reads it next.
- **AC-3's dynamic range compression was entirely wrong**, and could not have been caught here:
  ffmpeg's AC-3 encoder never emits the field, and Dolby's uses it constantly. Fixed.
- **Vorbis crashed on truncated Ogg files** rather than decoding what was there. Fixed.

And what it found that is *not* fixed, which is the honest part:

- **VP9 fails five feature vectors**: `show_existing_frame`, segmentation with an alternate
  quantiser on key frames, one other segmentation case, one tiling case, and one regression
  bitstream. Every one is a feature this decoder claims and gets wrong on content no encoder here
  produces. Two of them change picture size from frame to frame, which is worth knowing before
  reading the numbers: a reference decode of such a file is not a stack of equal-sized pictures, and
  measuring one as though it were gives a frame count that is simply wrong.
- **H.264 refuses more than this document admits.** Alongside the documented refusals it turns away
  I_PCM macroblocks, `pic_order_cnt_type` 1, and several streams whose reference lists it cannot
  build — and five streams decode to the wrong picture rather than being refused, which is worse
  than either.

## Not worth it, and why

- **HEVC / H.265.** Larger than all of H.264 put together: coding tree units with quadtrees, 35
  intra directions, a different CABAC context set, transform unit trees, sample adaptive offset. If
  another modern codec is wanted, VP9 costs less and appears more often.
- **Flash-era video** (Sorenson Spark, VP6 in FLV), **RealVideo**, **Windows Media and VC-1**,
  **MJPEG**. Real content exists in all of them, but each is a separate codec for a shrinking
  audience. Curiosities rather than plans.
- **Microsoft's pre-standard MPEG-4** (DIV3, MP42, MPG4). Named like MPEG-4 Part 2 and not the same
  bitstream. A separate decoder for a format that existed for about three years.

## Worth doing next, if anything

- **A serial H.264 stream is still a serial decode.** Concurrency here is per PICTURE, so it does
  nothing for a stream with P or B pictures — which is every real stream. VP9 has the same shape and
  the same limit. Slice-level or wavefront parallelism would help both, and neither is small.
- **A vector primitive.** FFV1 spends 84 per cent of its time in `%decode-line` and VP8 about forty
  in its deblocking kernel, and in both cases that is now the arithmetic the format specifies. The
  next step for either is lane-parallel work on several samples at once, which needs an unaligned
  machine-word load from an octet array — something SBCL does not offer, since `%vector-raw-bits`
  is indexed in whole words. That is the wall, and it is a different kind of wall from the ones
  above.

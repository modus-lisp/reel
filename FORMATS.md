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
| **Opus, AAC, MP3, MPEG audio Layer II, Vorbis** | reed's own suites; Layer II within 0.0002 RMS of ffmpeg |

H.264 covers CAVLC and CABAC, P and B slices, both direct modes, weighted and implicit weighted
prediction, reference list reordering, adaptive reference marking, the 8x8 transform, Intra_8x8,
scaling matrices, and the in-loop filter.

Speed, on one machine and one synthetic source, so read it as ratios and not as a specification:

| | one thread | sixteen |
|---|---|---|
| 640x360, P and B pictures | 38 fps | 40 |
| 1920x1080, P and B pictures | 4.5 fps | 4.5 |
| 640x360, all intra | 61 fps | 526 |
| 1920x1080, all intra | 7.9 fps | 68 |

The two columns differ only where the stream allows it. Pictures are decoded concurrently when they
are independently decodable, which today means all-intra; a stream with P or B pictures is a chain
and gets the serial walk, so the second column is the first plus noise.

The inter rows are about 2.7 times what they were before the motion-compensation path was tuned;
the intra rows are unchanged, because nothing that was tuned runs in them.

That set is what x264 produces with no options at all, which is the point: High is x264's default
profile, so most H.264 encoded since about 2010 is High rather than Main.

VP9, on the same machine, one thread, sixty pictures of the same synthetic source at each size:

| | before | after |
|---|---|---|
| 640x360 | 125 fps | 222 |
| 1280x720 | 31 fps | 61 |
| 1920x1080 | 13.7 fps | 26.8 |

About 1.9 times, with the output bit-identical at every step, and almost none of it algorithmic:
SBCL stores a `fixnum` array as sixty-four bits, so every value read out of one is wider than a
fixnum and every sum of two of them was an out-of-line call. Declaring the arrays thirty-two bits
wide — which is what they hold — was fourteen per cent on its own. `src/vp9/NOTES.md` has the rest,
including the two changes that were reverted for being slower than what they replaced.

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
- AC-3 and DTS, which a DVD may carry instead of MPEG audio. A file's video plays and the audio
  track is named as undecodable rather than guessed at.

## The gaps

None left. What was here — H.264 High profile, MPEG-2 in program and transport streams, AVI with
MPEG-4 Part 2, FFV1, Theora, VP9 — is above.

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
- **The same twenty minutes spent on the other decoders.** What doubled VP9's speed was almost
  entirely telling the compiler what its arrays hold, and nothing about VP9 made that true of it
  alone: H.264, MPEG-2, MPEG-4 Part 2, Theora and FFV1 all still declare their small-integer arrays
  `fixnum`, which in SBCL means sixty-four bits and generic arithmetic on everything read out of
  them.

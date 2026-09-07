# What plays, what does not, and what it would take

This is the road map for the media stack: [reel](.) decodes pictures, [cassette](../cassette)
holds the containers they arrive in, [reed](../reed) does audio. It exists because "can it play
this file?" is the only question anyone actually asks, and the answer lives in three repositories.

Proportions below are from experience, not from measuring a corpus. Treat them as ordering, not
as data.

## Plays today

| | verified against |
|---|---|
| **VP8**, key and inter frames | libvpx, bit-exact on eight clips and on Big Buck Bunny |
| **H.264 Baseline and Main** | ffmpeg, bit-exact on seventeen fixtures and on a 7672-frame YouTube file, every frame |
| **WebM and Matroska** | they are the same demuxer; `.mkv` with H.264 decodes today |
| **MP4**, including fragmented | ffprobe, packet for packet |
| **Opus, AAC, MP3, Vorbis** | reed's own suites |

H.264 covers CAVLC and CABAC, P and B slices, both direct modes, weighted and implicit weighted
prediction, reference list reordering, and the in-loop filter. About 118 fps at 640x360 and 71 at
1080p, decoding independent pictures concurrently.

## Refused, and refused loudly

Anything below is turned away with a reason rather than decoded wrong. That distinction is the
whole discipline of this decoder: a wrong picture that keeps playing is worse than no picture,
because nobody knows to disbelieve it.

- H.264 **High profile**: the 8x8 transform, and scaling matrices. Refused on the FLAG, not the
  profile — a High stream using neither decodes here.
- More than 8 bits per sample, 4:2:2 and 4:4:4, MBAFF and field coding, FMO, long-term references.

## The gaps, in the order I would close them

### 1. H.264 High profile

**The single biggest unlock for the least work.** x264's default profile is High, so most H.264
encoded since about 2010 is High, not Main. Everything else in the decoder is already there.

Four pieces: the 8x8 integer transform with its own scan and dequantisation, Intra_8x8 prediction
with its reference-sample filtering, the per-macroblock `transform_size_8x8_flag` in both entropy
coders, and scaling lists. The deblocking filter also stops filtering the internal 4x4 edges of an
8x8-transformed macroblock.

### 2. MPEG-2, in program and transport streams

**The volume play for anything archival.** DVD rips, broadcast captures, `.mpg`, `.vob`, `.ts`.
Enormous in older collections and entirely absent here. It is also a far simpler codec than H.264 —
closer in size to the VP8 work than to B slices — because it predates everything that made H.264
hard: no CABAC, no quarter-pel, no in-loop filter, no multiple references.

Needs an MPEG program-stream and transport-stream demuxer, which cassette does not have.

### 3. AVI, and MPEG-4 Part 2

The DivX and XviD era, which is to say the whole 2000s. The container is small work. The codec is
roughly MPEG-2 with extras, so it is much cheaper once MPEG-2 exists than before it.

### 4. FFV1 in Matroska

The one people forget. FFV1 is the actual **preservation** codec — lossless, and what national
libraries and film archives keep masters in. The container is already supported, so this is codec
work only, and it is a range-coded lossless codec rather than a transform codec, so almost nothing
in reel is reusable. Worth it for the archival case, not the consumption case.

### 5. Theora in Ogg

The Archive's own older open-format derivatives. A VP3 descendant, so genuinely close to the VP8
code already here.

### 6. VP9

The other half of what the web actually serves. Big, but the better target than HEVC if the goal is
playing what people link you.

## Not worth it, and why

- **HEVC / H.265.** Larger than all of H.264 put together: coding tree units with quadtrees, 35
  intra directions, a different CABAC context set, transform unit trees, sample adaptive offset. If
  another modern codec is wanted, VP9 costs less and appears more often.
- **Flash-era video** (Sorenson Spark, VP6 in FLV), **RealVideo**, **Windows Media and VC-1**,
  **MJPEG**. Real content exists in all of them, but each is a separate codec for a shrinking
  audience. Curiosities rather than plans.
- **MPEG-4 Part 2 without AVI.** The codec is only worth having because of the container it lives
  in; do them together or not at all.

## Small things that are nearly free

- **AAC inside Matroska.** cassette already decodes AAC from MP4; in MKV the track is currently
  named unsupported. This is routing, not decoding.
- **H.264 decode speed at 1080p.** 71 fps concurrently, but single-threaded it is 10.6, and the
  deblocking filter is a third of that. Matters for a single-core or latency-bound path.

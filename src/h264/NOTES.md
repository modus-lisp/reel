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

## Inter prediction: P slices

**Done, and bit-exact.** P slices decode with motion compensation, multiple reference pictures,
every partition size down to 4x4, intra macroblocks inside P slices, skip runs, and the inter
boundary-strength derivation the loop filter needs. Verified against ffmpeg's ordinary filtered
output on 180 frames across six fixtures, including 90 frames of real motion with three references
and every partition size in play.

Reference handling is baseline's sliding window: a reference picture goes on the front of the list
and the oldest falls off once there are more than the sequence parameter set allows; an IDR empties
it. List reordering and long-term references are refused rather than ignored.

### The two bugs, both of which parse cleanly and decode wrong

**The reference count defaulted to 1.** The slice header slot for `num_ref_idx_l0` was initialised
to 1, which made the fall-back to the picture parameter set unreachable — so a stream with three
references read no reference indices at all and desynchronised. This one at least fails loudly, a
few macroblocks later, as an impossible mb_type.

**A neighbour that is not yet decoded is NOT AVAILABLE, which is not the same as zero** (6.4.11.7).
A sub-partition's above-right neighbour can lie inside the current macroblock, in a partition
decoded later. Reading its uninitialised entry gives a zero vector with reference -1, which is
exactly what an unavailable neighbour contributes to the median — so it looks equivalent, and it is
not: an *unavailable* C makes D stand in for it, and a zero C does not. Only partitions smaller
than 8x8 can hit it. `ss-mb-done` is the fix, a bit per 4x4 block of the current macroblock.

The way both were found is worth keeping: encode fixtures that vary ONE encoder feature at a time
(`ref=1` against `ref=3`, `partitions=none` against `p8x8` against `p4x4`) and see which one turns
red. That took the second bug from "somewhere in inter prediction" to "sub-8x8 partitions only" in
two ffmpeg invocations, and `*skip-loop-filter*` had already ruled out the filter in one.

## CABAC

**Done, and bit-exact**, for both I and P slices. Forty frames of intra across quantisers 12 to 34,
and 100 frames of inter including real motion with three references and every partition size.

The failure mode shaped how it was built. A wrong context index does not corrupt one block — it
feeds the wrong probability to the arithmetic decoder, which returns the wrong bin, and everything
after it in the slice is garbage. There is no partial credit and no bisecting a picture to find the
fault. So `cabac-tables.lisp` is **generated**, not typed, and checked against invariants the
specification states independently of the numbers. Those held, and the decoder was then bit-exact
first time on intra.

Three things worth remembering:

- `cabac_init_idc` sits between the reference picture marking and `slice_qp_delta` in 7.3.3.
  Reading it anywhere else costs the quantiser and everything after it.
- `coded_block_flag` is the one element where an ABSENT neighbour contributes 1 for intra and 0 for
  inter. That is the opposite of the others, which is why the rule is written at the point of use.
- Every reference index of a macroblock is read before any vector difference. A partition's index
  context asks what the partition beside it chose, so the indices must be recorded as they are read
  and not when the vectors arrive. Same shape as the sub-8x8 bug in the CAVLC path.

## Weighted prediction

Done, with the reference list modification it forces. **The list can be longer than the number of
pictures in it**: reordering inserts at a position and drops only the copy *after* the insertion
point, so the same picture legitimately appears at two indices — which is how weighting gets two
different weights from one reference. Treating the list as a set decodes most streams and then
fails on ordinary ones.

That also means the loop filter must compare reference PICTURES, not indices (8.7.2.1). Comparing
indices filters edges between blocks that came from the same place. It showed as a handful of
samples of magnitude two, and was only findable because it appeared identically under both entropy
coders, which ruled out everything above the reconstruction.

## B slices

**Done, and bit-exact** on seven fixtures: both direct modes, both entropy coders, three
references, every partition size, implicit weighted bi-prediction, and a scene cut that puts intra
macroblocks inside B slices and gives one macroblock four differently-predicted 8x8 partitions.

Four things arrived together. Picture order count and an **output reorder buffer**, because a B
picture is decoded after the picture it is displayed before — `feed-nal` now returns whichever
picture's turn it is, which is usually not the one just decoded, and `flush-decoder` is not
optional: without it the last few pictures of every file stay in the buffer. Two reference lists
ordered by where pictures sit on screen rather than when they were coded. Bi-prediction, which is
the mean of two whole predictions rather than a blend made as they are computed, so that it rounds
once. And the two direct modes, where spatial asks the neighbours what is going on and temporal
asks the future picture what happened and interpolates backwards through it.

### The bugs, which are all one bug

Every one was a neighbour that had been decoded but did not look decoded, or the reverse:

- A partition counts as decoded **once it is reached**, whichever lists it uses. Marking it only
  when a vector was stored made a list-1-only partition look undecoded during the list-0 pass. An
  unavailable neighbour is not the same as one with no reference in that list, because
  unavailability is what makes the above-left neighbour stand in for the above-right one.
- The **directional** cases of 8.4.1.3 apply to a B slice's two-partition types exactly as to a P
  slice's. Leaving them out takes the median where the specification takes one named neighbour.
- **Direct partitions must be derived before any vector difference is read**, because a later
  partition predicts from them and they need no bits of their own.
- The loop filter compares the **sets of reference pictures**, and a block using one picture twice
  can be matched to its neighbour's two either way round.

`%ref-idx-ctx-inc` excluding skipped neighbours is load-bearing rather than an optimisation
(9.3.3.1.1.6): a skipped B macroblock does have a direct-derived reference index, and counting it
desynchronises within a couple of dozen slices. That was verified by breaking it on purpose.

## Fixed: reference identity was stored as frame_num, not order count

Temporal direct with more than one reference picture used to decode wrong. The cause was that the
per-block "which picture did this come from" array held **frame_num** on the P path and **picture
order count** on the B path. Temporal direct READS THAT BACK from a previously decoded picture and
looks it up in the current list 0, so it compared one kind of number against the other, missed, and
silently fell back to index 0.

Invisible with one reference picture, because index 0 is the only answer there is. Wrong with
several. Frame_num does not identify a picture anyway — every non-reference picture between two
references shares one — so the order count is the right thing to store, and `%ref-picture-id` now
returns it.

Three fixtures hold that down: `bt-r3`, `bs-r3` and `bt-r1`. Neither temporal direct nor multiple
references fails alone, which is why all three are kept.

## Fixed: a direct-predicted neighbour has a reference index nobody told it

The last desynchronisation. In a B slice, the context for `ref_idx` counts a neighbour that used a
reference other than the first — but 9.3.3.1.1.6 excludes an intra neighbour, a SKIPPED one, and
**any partition predicted in direct mode**. The third exclusion was missing.

It takes three things at once to bite: a CODED direct macroblock rather than a skipped one, sitting
beside a macroblock that codes a reference index, in a stream with more than one reference so the
inferred index can exceed zero. With a single reference picture it can never happen, which is why
every simple fixture passed.

The flag has to be per 4x4 block, not per macroblock, because a B_8x8 can be direct in some of its
four partitions and not others.

## How the last two bugs were actually found

Both were single wrong bins hundreds of macroblocks into a stream, and the same method found both.

1. **Narrow with encoder features.** Encode fixtures varying ONE x264 setting at a time and see
   which turns red. Temporal direct and multiple references were each fine alone and broke
   together; weighted prediction turned out to be a red herring that merely changed the content
   enough to reach the bug.
2. **Locate with ffmpeg's debug maps.** `-debug qp` and `-debug mb_type`, with `-threads 1` because
   frame threads interleave the log into nonsense. Comparing the quantiser map against the
   decoder's own `pic-mb-qps` located a fault to one macroblock in a single run.
3. **Confirm with pixels.** Comparing against `-skip_loop_filter all` output says which macroblock
   first goes wrong, which is often earlier than where the decoder throws.

Two details of the maps are not guessable and cost hours if assumed:

- **`>` is list-0-only and `<` is list-1-only** — the opposite of what the arrows suggest.
- **Lowercase `d` is direct AND skipped; uppercase `D` is direct but coded.** The suffix is the
  partition shape: `+` 8x8, `-` 16x8, `|` 8x16, space 16x16. Without it a B_8x8 and a bi-predicted
  16x16 look identical while consuming wildly different numbers of bins.

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

## High profile

Four things, and only one of them was hard.

**The 8x8 transform** shares nothing with the 4x4 one. Its own scan, six position classes rather
than three, its own butterfly, and a dequantisation shift that pivots at quantiser 36 rather than
24 because the transform carries six more bits. It went in bit-exact first try, which is what
generating the tables mechanically and checking their invariants buys.

**Intra_8x8** is the nine 4x4 modes widened, with one addition that has no 4x4 counterpart: the
reference samples are filtered with a 1-2-1 kernel before any mode looks at them. Skip it and every
block is slightly wrong in a way that reads as a rounding bug in the modes rather than a missing
stage. It also takes sixteen samples from the row above rather than eight, because the diagonal and
vertical-left modes reach past the block into the above-right neighbour.

**Scaling lists** cost a day for a reason worth writing down. A picture parameter set carries
`6 + 2 * transform_8x8_mode_flag` lists, not always eight. Reading two that were not there consumed
the bits belonging to `second_chroma_qp_index_offset`, so the **Cr plane alone** decoded at the
wrong quantiser while luma and Cb stayed perfect. Comparing the planes separately is what made that
one obvious instead of mysterious; a single "how many bytes differ" number would have hidden it.

**CABAC for the 8x8 block** was the one that bit. Three things about it are not the 4x4 rules:

- No `coded_block_flag`. The coded block pattern already said the block has coefficients and
  7.3.5.3.3 does not send it twice.
- Significance and last-significance live at ctxIdx 402 and 417, not at an offset from 105 and 166.
- Sixty-three scan positions share fifteen significance contexts and NINE last-significance ones,
  by two maps.

I wrote the last map from memory with five values instead of nine. The symptom was precise and
misleading: every syntax element before the residual — macroblock type, the transform size flag,
all four prediction modes, the chroma mode, the coded block pattern, the quantiser delta — decoded
CORRECTLY, and the residual was wrong from its DC onwards. That is what a wrong significance map
does: the run of significance flags is decoded against the wrong probabilities, the block ends up
with the wrong number of coefficients, and the magnitudes are then read backwards from the wrong
starting point.

**The check that would have caught it without the bitstream**: the context COUNTS are derivable
from the ctxIdx layout alone. Last-significance owns 417 through 425 because coeff_abs_level_minus1
starts at 426, so its map must reach 8. Mine reached 4. Counting the slots between two known
offsets is a real invariant and it costs nothing.

## The bug the High profile work uncovered, which was not a High profile bug

The last fixture to fail was `fast.mp4`, which had been the file used to prove the refusal path
worked and so had never once been decoded. Twelve frames of sixty were wrong, in a handful of
scattered macroblocks, in the last two groups of pictures only.

Narrowing by re-encoding the same source with one x264 feature turned off at a time found it in
five minutes: `b-pyramid=none` fixed it and nothing else did. A B pyramid means B pictures that are
themselves references, and an encoder retires those with `memory_management_control_operation`
rather than by sliding a window. Those operations were being parsed — so the bit position stayed
right — and then discarded.

**Nothing desynchronises**, which is what makes it hard. The reference set drifts: a picture the
encoder dropped stays in the buffer, the window evicts a different one, and the lists agree with
the encoder's for the first two groups of pictures and then quietly do not. Only macroblocks that
name a reference index the two sides disagree about come out wrong.

x264 turns the B pyramid on by default, so this sat in the way of most real High profile files, and
in a session about the 8x8 transform it would have been read as an 8x8 transform bug. Two things
saved time: the failing macroblocks all had `transform_size_8x8_flag` clear, which said the new
code was not involved; and comparing with the loop filter disabled said the fault was underneath it.

## Making it 2.7 times faster without changing a line of arithmetic

Motion compensation is the whole cost of an inter-coded stream, and four things were wrong with it —
none of them in the maths, all of them in what the compiler could see.

**The picture structure was defined after its users.** `slice.lisp` held the `picture` DEFSTRUCT and
loads *after* `motion.lisp`. So every `(declare (type picture ref))` in the motion path named an
unknown type, every `pic-y` was a full call returning an object of unknown type, and every piece of
arithmetic on the result went generic. Moving the definitions into `picture.lisp`, ahead of every
file that reads them in a loop, was worth 1.76x on its own. SBCL says so in a style warning — "the
structure definition was not yet seen" — which is very easy to read past.

**The per-block accessors were undeclared.** `blk-mv`, `blk-ref-poc`, `set-blk-mv`, `%mv-index` are
read a few times per 4x4 block by motion prediction and again by the loop filter: millions of calls
a second at 1080p. They are now inline and fully typed. Another 1.1x.

**Every reference sample was clamped.** The specification's rule is that a vector may point outside
the picture and the edge sample repeats — so every read is clamped, and a thirty-six tap filter pays
that thirty-six times per output sample. When the whole source rectangle is inside the picture, which
it is for nearly every block of nearly every frame, the clamp cannot fire. There are now two copies
of the sampler, generated from one body by a macro, and the choice is made once per partition.

**A whole-sample vector is a copy.** Integer motion needs no filter at all, and static or slowly
panning content is full of it. `replace` per row instead of the sampler per sample.

The remaining hot spots, in case this is picked up again: the six-tap itself, the chroma bilinear,
and the boundary-strength computation, in that order. None of them is obviously wasteful any more.

**What did NOT change: all-intra decoding, at all.** None of the above runs when there is no motion.
That is worth stating because it is the thing a benchmark on the wrong clip would hide — the two
all-intra measurements moved by less than the noise, and only the inter ones moved at all.

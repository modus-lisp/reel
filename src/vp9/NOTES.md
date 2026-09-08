# VP9

Stage one: the tables, the two readers, and the uncompressed frame header. The codec itself is
refused, and that refusal is kept as a test so that it stops being true when the frame decoder
lands.

## Two readers, and the split between them is the interesting part

A VP9 frame has an **uncompressed** header read as plain bits, and everything after it read through a
binary arithmetic coder. The uncompressed part carries the frame size, the reference indices, the
quantiser, the loop filter, the segmentation and the tiling — everything a container, a router or a
bitstream filter might want — and it is readable with no decoding state at all. That is why a VP9
stream can be spliced or repackaged by something with no decoder in it, and it is why this stage is
worth having on its own.

The arithmetic coder is VP8's, unchanged. What VP9 adds above it is the **tree**: a decision is
rarely one bit, and a symbol is read by walking a small binary tree with one probability per node. A
strictly positive entry is the next node, anything else the negated symbol — and the test being
`> 0` rather than `>= 0` is the whole encoding, because node zero is the root and can never be a
branch target, which frees the value zero to mean symbol zero.

## Superframes

A container packet may hold several frames. VP9 codes hidden reference frames — an alt-ref is built
from the future and never displayed — and a container has nowhere to put a frame that produces no
picture, so the encoder glues them onto the front of the next visible frame and appends an **index**
at the very end of the packet.

The index is bracketed by two identical marker bytes, which is how a decoder tells an index from
picture data that happens to end the right way: read the last byte, work out how long the index
would be, and check that the byte that far back is the same. A decoder that does not look decodes
the first frame of the group and silently throws the rest away — which for a typical encode is most
of the reference structure, and which looks like a decode bug rather than a demux one.

## Two inversions worth knowing about

The key-frame flag and the shown flag are **both** inverted in the bitstream, and only the first
looks it: a zero says key frame, and a **one** says shown. Reading the second the same way as the
first costs nothing on a visible frame and desynchronises on the next one, because whether a frame
is shown decides whether an intra-only bit follows.

And the frame size may be **inherited** from a reference rather than sent — three bits are read in
order until one says yes. A decoder that skips them because the container already told it the size
loses the bitstream, not merely the size.

## What is refused

Profiles 1, 2 and 3 — that is, anything but eight bits and 4:2:0 — on the profile field itself, and
an sRGB colour space on its own field, because that implies 4:4:4 whatever the profile says.

## The compressed header

Stage two. VP9 does not send its probabilities: it keeps **four** saved contexts, a frame names one,
and the compressed header sends *differences* from it. So a probability is a running quantity with a
history, and one wrong entry does not make a slightly wrong picture — it desynchronises at the first
block that reads it and stays wrong until the next key frame.

The difference coding is worth reading twice. A probability lives in [1,255], so the distance to any
other value is at most 254, but it is not symmetric about the current value; the symmetric part is
sent as twice the magnitude plus a sign and the remainder is stacked on top, and that absolute
difference is then sent as a variable-length code because a large change is unlikely. The first
twenty entries of the inverse table step by thirteen and the rest fill in every value between, so a
cheap code buys a coarse update and an expensive one an exact value.

**The motion vector models do not use it.** They send seven bits and set the low bit — a different
scheme, for no reason anyone has written down, and following the pattern instead of the
specification puts the decoder one bit out for the rest of the header.

Three other things that are easy to get wrong and silent when you do:

- The coefficient loop **stops at the frame's transform size**. A frame that never uses a 32x32
  transform does not send probabilities for one, and reading all four sizes regardless eats the next
  field.
- The DC band has **three** neighbour contexts, not six. There is one DC per block, so there is less
  to know about it.
- The partition levels arrive **backwards**, smallest block first — the one place in the header where
  the transmission order is not the array order.

A coefficient context needs eleven probabilities and three are sent. The other eight come from a
table indexed by the third: coefficient magnitudes are close enough to a Pareto distribution that one
parameter fixes the tail. That is what makes updating coefficient probabilities affordable at all.

### How this is verified without a picture

The compressed header is an arithmetic-coded partition whose length the uncompressed header states,
and a correct parse consumes it **exactly**. Any field read at the wrong width, in the wrong order,
or under the wrong condition leaves the coder somewhere else. Across seventy headers ranging from six
bytes to nearly three hundred, landing on the last byte every time is not a coincidence — it is as
strong a check as a decoded picture would be, and it is available now.

## Tiles, the partition quadtree, and coefficients

Stage three. A frame is a grid of 64x64 superblocks and each is a quadtree: at every level a block
may be left whole, split in two across, split in two down, or split into four and the question asked
again. Thirteen block sizes result, most of them not square, which is why so much of `block.lisp` is
a table indexed by size.

**Tiles are independent and that is the point.** A frame may be cut into tile columns and rows, each
its own arithmetic-coded partition with its own left-edge context, so they can be decoded in parallel
and a lost one damages only its own rectangle. Every tile but the last carries its length in four
bytes in front of it — findable, like the frame header, without decoding anything.

The contexts are most of the work. Nearly every symbol is coded against what the blocks above and to
the left did, at a granularity that differs per symbol: per 4 samples for prediction modes and
coefficient counts, per 8 for skip and transform size, and a packed bitfield per superblock level for
the partition. The above contexts span the frame and the left ones span one superblock row of one
tile, and that asymmetry is exactly what makes a tile independent.

Three things that cost time:

- **A block writes its full width to the context, not the part inside the picture.** A block hanging
  over the right edge still writes what the block below it will read.
- **The band counter runs one past the end.** It advances after the last coefficient of a block as
  well as between them, so the band-count table needs two trailing zeros — landing on one advances it
  no further. ffmpeg gets this from array padding; here it is written down.
- **A 32x32 transform's coefficients are halved** on the way out of the entropy decoder, because its
  own scaling is one bit larger than the other sizes' and the inverse transform wants them all in one
  range.

### How this is verified, still without a picture

The same invariant as the compressed header, one level down and much sharper: a tile is an
arithmetic-coded partition of a stated length, so a correct walk of the quadtree — every partition
decision, every block mode, every transform size, every coefficient of every transform block — ends
on its last byte. A 1280x720 key frame is thirty-four thousand bytes and twenty-three hundred blocks;
one symbol read at the wrong width anywhere in it does not land there.

## Reconstruction

Stage four: the intra predictors, the inverse transforms, and the edge samples that feed them.

**VP9 uses two transforms in the same block.** The DCT is what every codec has; the ADST — an
asymmetric discrete sine transform — is used along whichever axis an intra prediction ran, because
the residual a directional prediction leaves is small at the predicted edge and large away from it,
and a sine basis fits that where a cosine basis does not. A vertically predicted block gets ADST down
its columns and DCT across its rows, and the intra mode chooses the pair. The first pass is the
columns and the second the rows, and which of them takes the ADST is the low bit and the high bit of
the transform type respectively — swapping them decodes a picture that looks almost right, because
the two transforms agree on flat content and differ only where the prediction had a direction.

The one-dimensional passes are transcribed **by parsing ffmpeg's C**, not by hand: six hundred lines
of straight-line arithmetic in which every constant and every shift is normative, and one transposed
digit gives a decoder wrong on one coefficient in a thousand.

The edge samples are most of the remaining difficulty. Any of the row above, the column to the left,
the corner, or the four samples above-right may be outside the picture, outside the tile, or not yet
decoded, and each case has its own rule: a missing row is a flat 127, a missing column a flat 129, a
missing corner 127 or 129 depending on which side survives, and a row that runs off the right repeats
its last real sample. Ten codeable modes become fifteen once the substitutions are applied, and the
three flat constants differ by one in each direction rather than all being 128 — which is what makes
substituting the wrong one visible.

And one rule that is not about availability: **a block in the first row of a superblock row reads the
row above it from a copy taken before the loop filter ran**, not from the picture. Intra prediction is
defined on unfiltered samples, and by the time that block is decoded the filter has already been over
that line. A decoder that reads the picture instead is right until the first frame whose filter level
is not zero, and then it is wrong everywhere, faintly.

### Verified on a lossless key frame

A lossless encode is the one case that can be compared before the loop filter exists, because a
lossless frame has a filter level of zero and ffmpeg's own output is therefore unfiltered too. It is
bit-exact. That proves the partition walk, every block mode, every coefficient, the edge gathering
with all its substitutions, the fifteen predictors, the Walsh-Hadamard, and the crop on the way out —
everything except the DCT and ADST themselves, which a lossless frame does not use, and the filter.

## The loop filter

Stage five, and with it **every intra frame of every fixture decodes bit-exact against ffmpeg** —
including 1280x720 across four tile columns and a stream whose transform mode is switchable. That is
the whole decoder except inter prediction.

VP9 filters TRANSFORM BLOCK edges, not macroblock edges — there are no macroblocks — and the width at
each edge depends on the transform sizes meeting there: sixteen samples across a 32x32 boundary,
eight across an 8x8 one, four inside a block of 4x4 transforms. Which edges get which width is worked
out **while decoding**, as a bitmask per superblock, because by the time the filter runs the block
structure is gone.

The mask is four planes deep per row of the superblock: one each for the sixteen-, eight- and
four-wide filters, and a fourth for the four-wide edges that fall *inside* an eight-sample step and
so cannot be reached by the first three. That fourth plane looks redundant and is not: the filter
walks eight samples at a time, and a 4x4 transform has an edge halfway through every step.

Columns before rows, per plane, per superblock, and never interleaved — a sample on a corner is
filtered twice and the horizontal pass must see what the vertical one left.

### Three bugs worth recording

**`2 - !(y & mask)` is two when the test passes.** The C negation inverts it, and reading it as
written the other way round puts every four-wide horizontal edge into the eight-wide plane. This was
the last bug and cost the most: it left about a tenth of a percent of samples wrong, scattered, which
looks exactly like a rounding difference somewhere in the transforms.

**The column pass advances sixteen sample rows per step whatever the subsampling**, because the step
is twice as tall in a subsampled plane: two level rows of eight samples, or four of four.

**A tile's left edge is in eight-sample units.** Storing it in superblocks makes every block at the
left edge of a tile believe it has a neighbour — which is invisible until a frame uses more than one
tile column *and* a switchable transform size, because only then does availability reach the
entropy decoder.

## Inter prediction

Stage six, and with it **every picture of every fixture decodes bit-exact against ffmpeg** — a
hundred and ten of them across five clips, including 1280x720 across four tile columns and a stream
encoded with alt-ref frames.

VP9 predicts a motion vector before it codes one, and the prediction is a **search** rather than a
formula. Up to eight neighbouring blocks are visited in an order that depends on the block's shape —
nearer edge first — and the first that used the same reference supplies the vector. If none did, the
search runs again accepting a different reference and negating the vector when the two point opposite
ways in time. Then the previous frame's vector at this position. Only then does the predictor answer
zero, and even that is clamped, which for a block at the picture's edge is not zero.

The search returns the **second distinct** answer when the block asked for the near vector rather
than the nearest, which is why it is written as early returns over a remembered first candidate
rather than as a list. Building the list and taking the second element is not the same function: the
order of comparison is observable.

Two of ffmpeg's comments in that function say the behaviour is a bug in libvpx. They are transcribed
as they stand, because libvpx is what encoders were written against and a decoder that fixes the bug
decodes those streams wrongly.

**Chroma vectors are in sixteenths, not eighths** — and that is not a separate convention. A luma
vector of one eighth of a luma sample *is* one sixteenth of a chroma sample at half the resolution.
So the same number is read against a sixteen-phase filter instead of taking every second phase, and
chroma prediction is finer than luma prediction rather than coarser.

A block smaller than 8x8 predicts its luma in two or four pieces with a vector each, and its chroma
in **one** piece with those vectors averaged, because at half the resolution the pieces would be two
samples wide and an eight-tap filter needs more than that.

### The nearly-two hundred lines nobody should type

The reference-frame and comparison contexts are four decision trees over what the neighbours did, and
no branch of them is ever obviously wrong. They are transcribed by **parsing ffmpeg's C**, the same
way the inverse transforms are, with a name table mapping its context arrays onto this decoder's. One
inverted test there gives a decoder that is right on most content, and nothing in the shape of the
code catches it.

### Two things that were wrong and silent

**The filter level of an inter block depends on its reference and on whether its vector is zero** —
`lflvl[segment][ref+1][mv != 0]`, not the single intra entry. And a **skipped inter block has no
transform edges inside it**, only its own boundary: there is no residual, so nothing was transformed
and nothing needs smoothing. Together those two were half a percent of samples wrong on the first
inter frame, growing with each frame after it as the error propagated through the references.

## Backward probability adaptation

Stage seven, and the last. A stream that does not set frame-parallel mode refreshes its probability
context from the symbol **counts** of the frame just decoded rather than from the forward updates in
its header. So the decoder tallies every decision it makes — which coefficient token, which
partition, which intra mode, which motion vector class — and at the end of the frame nudges each
probability towards what the counts say it should have been.

The nudge is proportional to confidence: a context seen twice moves a little, one seen twenty times
or more moves the full step. The threshold is twenty-four for coefficients and twenty for everything
else, because coefficients are seen so much more often that they would otherwise saturate on the
first superblock. And a frame that is or follows a key frame trusts its counts a little less — a
factor of 112 rather than 128 — because the models it started from were the defaults rather than
something learned.

Two things in the counting are not what a careful implementer would write.

**The sixteenth of a motion vector is counted even when it is not coded.** ffmpeg marks it as a bug
in libvpx; it is therefore the format, because a decoder that counts honestly adapts differently from
every encoder in existence.

**The inter modes are counted in bitstream order and adapted in tree order**, so the indices in the
adaptation look shuffled against the ones in the decoder: zero motion is the tree's first branch and
the third symbol.

A probability that splits a tree node is adapted against the counts of everything *below* that node
on each side, so the sums are taken as the tree is descended and each is the previous minus what has
just been used. That is why the intra-mode adaptation reads as a running subtraction rather than as
nine independent ratios.

## What is left

- **Reference scaling.** A frame may predict from a reference of a different size, rescaling as it
  goes. Refused, on the size comparison, with the sizes named.
- **Profiles 1, 2 and 3** — anything but eight bits and 4:2:0 — refused on the profile field, and an
  sRGB colour space refused on its own field because that implies 4:4:4 whatever the profile says.
## Compound prediction, and why it took so long to prove

A block may predict from **two** references and average them, and that needs two references whose
sign biases differ — one pointing back in time and one forward. Which needs an alt-ref. Which needs
`-auto-alt-ref`, which libvpx accepts and then silently ignores unless the encode is **two-pass**.
The only place that is written down is the option's own help text, in parentheses.

So for a long stretch this path was written and unreachable: every fixture had sign biases of
`(0 0 0)`, every inter frame refreshed only slot zero, and there was no way to tell from a passing
test whether the code had ever run. What settled it was not searching for a clip but instrumenting
the question — printing the reference indices, the sign biases and the refresh mask per frame, at
which point `refresh 00000001` on every single inter frame said plainly that no alt-ref existed and
the search should be for an encoder setting rather than for content.

A two-pass encode gives sign biases of `(0 0 1)`, fifty-three of sixty-five inter frames choosing
compound prediction, and — on the first run, with no correction needed — sixty pictures bit-exact.

The fixture now **counts** the blocks that used two references and the test asserts the count is
positive. That is the real fix: a stream re-encoded with different settings would otherwise stop
covering the path and nothing would say so, which is exactly how it went unproven for as long as it
did.

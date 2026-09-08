# Theora

Theora is VP3 with a header. On3 released VP3 in 2001, Xiph took the bitstream unchanged, wrapped
it in three configuration packets and froze it in 2004, and nothing has moved since. So the decoder
is a VP3 decoder, and the parts that read like Xiph — the setup header with its interpolated
quantiser matrices and its eighty transmitted Huffman tables — sit on top of a coding layer that
predates them and does not know they are there.

## Three partitions of the same picture

An 8x8 block is a **fragment**. Fragments group four-by-four into **superblocks**, and two-by-two
into **macroblocks**. All three exist at once and each stage of decoding uses a different one:

- **superblocks** say *which* fragments are coded, run-length coded along a Hilbert curve;
- **macroblocks** say *how* — the coding mode and the motion vector;
- **fragments** carry the coefficients.

The Hilbert curve is not decoration. Consecutive positions in the scan are adjacent on the picture,
so a run-length code over it describes a coded *region* rather than a coded *row*, and that is why
the coded-block signalling is as cheap as it is. `tables.lisp` checks the property rather than
trusting it: every step of `+superblock-scan+` must move to a neighbouring cell.

## The coefficients arrive by frequency, not by block

Every other codec in this repository codes a block's coefficients together. Theora interleaves them
across the whole picture: all the DC coefficients, then all the first AC coefficients, then all the
second, to the sixty-third. Sixty-four passes over three planes, and a block's DC and its last
coefficient are thousands of bits apart.

Two consequences. First, no block can be finished until all sixty-four passes are done, so the
decoder holds the coefficients for the entire picture rather than reconstructing as it reads.
Second, each pass has its own statistics, which is what the setup header's **eighty** Huffman tables
are for: sixteen alternatives, times a DC group and four AC bands, and a picture names which of the
sixteen it used for luma and for chroma.

`%unpack-level` walks a plane's coded fragments and takes those whose next scan position is the one
being read. ffmpeg keeps a counter per (plane, level) and a deferred token stream instead; the two
are the same walk, and the counter is derivable from the walk, so the walk is what is here.

## The bug that hid behind a correct chroma plane

A coefficient token carries a run of zeros *and* a value. At scan position zero with no run, the
value is the DC. At scan position zero **with** a run, it is an AC coefficient further along and the
DC stays at nothing. Writing it to the DC instead — which is the obvious misreading, since the level
being decoded is zero — corrupts exactly the blocks that use that token and no others.

It presented as: chroma bit-exact, luma wrong in a rectangle at one corner of the picture, and the
error was a constant offset per block, which is the signature of a wrong DC. The rectangle was the
tail of the DC prediction's raster walk, where one wrong DC had propagated into every fragment
predicted from it. Finding it took splitting the error count by plane, then dumping the predicted
DC of the first wrong fragment and checking the prediction arithmetic by hand against its
neighbours: the arithmetic was right, so its own transmitted difference was wrong.

The lesson is the one that keeps recurring: **a correct plane is not evidence that shared code is
correct**, it is evidence that the shared code is correct *for the values that plane happened to
use*. Chroma never used a zero-run token at level zero.

## The DC predictor is a table VP3 shipped, not a rule

`+predictor-weights+` has one row per subset of the four available neighbours. Several rows ignore
neighbours they have — the three-neighbour cases fall back to the two-neighbour weights, and
`up-left, up-right` is a plain average of two neighbours that are not adjacent to each other. It is
not a mistake to fix; it is the table both encoders and decoders have agreed on since 2001.

Two rows (`up-left, up, left` and all four) can produce a prediction outside the range of every term
they were built from. When that happens by more than 128 the predictor is thrown away and the
nearest single neighbour used instead, tried in a fixed order. That clause fires rarely, which makes
it exactly the kind of thing that decodes a thousand frames correctly and then does not.

A fragment may only predict from a neighbour that is a difference from the *same* picture — intra
from intra, ordinary inter from ordinary inter, golden from golden. `+dc-reference-class+` says
which, and there is a separate running fall-back value per class, carried across rows and never
reset within a plane.

## The transform is fixed-point, not integer

H.264's transform is defined so that an integer forward transform inverts exactly. Theora's is the
real-valued DCT with its cosines held as sixteen-bit constants, and every product taken back down by
sixteen bits at once. It is exactly specified — two decoders agree bit for bit — but the shifts are
part of the specification rather than a consequence of it, and none of them can move: `>> 16` after
each multiply, `>> 4` at the end, and an 8 added in the second pass only.

The row and column passes both skip work when their inputs are zero, and *the tests are not the
same*: the row pass checks all eight values, the column pass checks all but the first. A column
carrying only a DC takes a separate path with its own rounding, and an inter block whose EOB arrived
at scan position zero takes a third one. Those three are not interchangeable — they agree to within
a sample and not always — so which one a block takes is part of the format, and the decoder records
where each block ended in order to make the same choice.

## Half-sample motion is not an average of four

Where both components of a motion vector are odd, every other codec here averages the four
surrounding samples. VP3 averages **two** of them, along one diagonal, chosen by whether the two
components have the same sign. There is no rounding term. A diagonal half-sample block is therefore
slightly sharper and slightly wrong-looking compared to what a bilinear filter would give, and it is
what the format says.

Chroma vectors are the luma vector halved with the low bit *sticky* — `(v >> 1) | (v & 1)` — so that
a half-sample luma vector stays a half-sample chroma vector rather than rounding to a whole one.

## The loop filter's order is load-bearing

A fragment filters its own left and top edges always, and its right and bottom edges only when the
neighbour there is not itself coded. Some samples are filtered twice, and which pass runs first
changes the result, so the traversal is part of the format and not an implementation choice. The
filter runs a superblock row behind reconstruction, and the picture's last fragment row is filtered
at the very end, after every plane is complete — a separate pass, easy to miss, and its absence
shows up as one wrong row at one edge.

## What is not here

- **VP4**, which ffmpeg decodes with the same code. It is a different bitstream from the frame
  header down and shares only a name.
- **Chroma layouts other than 4:2:0.** Refused on the flag in the identification header.
- **Bitstreams older than 3.2.0**, which stored the picture the other way up.

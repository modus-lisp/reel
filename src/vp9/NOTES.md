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

## What is next

1. The compressed header: probability updates for every model, read through the arithmetic coder.
2. Tiles, and the superblock partition tree.
3. Intra modes and reconstruction — ten modes, four transform sizes, four transform types.
4. Motion vectors and inter prediction, including compound.
5. The loop filter, which in VP9 is applied per transform-block edge rather than per macroblock.

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

## What is next

1. Tiles, and the superblock partition tree.
2. Intra modes and reconstruction — ten modes, four transform sizes, four transform types.
3. Motion vectors and inter prediction, including compound.
4. The loop filter, which in VP9 is applied per transform-block edge rather than per macroblock.
5. Backward probability adaptation, which is what makes a frame's counts the next frame's model.

Not yet exercised by any fixture: **compound prediction**. It needs references whose sign biases
differ — an alt-ref pointing forward — and libvpx here does not emit one for these clips. The path is
transcribed and unproven, and it is the first thing to check against real-world content.

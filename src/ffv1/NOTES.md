# FFV1: four things, none of them a transform

FFV1 has no DCT, no motion and no reference picture. Every sample is predicted from three
already-decoded neighbours and the difference is entropy coded against one of several thousand
adaptive models chosen by quantising the gradients around it. That is the whole codec. What cost
time was four details, and three of them are outside the sample loop entirely.

## The state table's constant is truncated, not rounded

The range coder's transition table is built from one parameter: `0.05 * (1 << 32)`, which is
214748364.8. It is passed as an integer, so it is **214748364** and not 214748365.

One part in two hundred million. It moves an entry or two of the table, the arithmetic decoder
diverges within a few hundred symbols, and the configuration record parses as version 3, micro 4,
and then a colorspace of twelve.

## Slice zero continues the frame's coder

A packet begins with one bit saying whether the frame carries a header. Slices one and up each get a
fresh range decoder over their own bytes — and slice zero does **not**. It continues the coder that
read that bit.

A decoder that starts slice zero at the first byte reads the bit as picture data. Every other slice
decodes perfectly, which makes it look like a bug in slice zero rather than a missing bit at the
front of the packet.

## The context models outlive the frame

FFV1 has no inter prediction, but it does have inter **adaptation**: the models are cleared only on
a key frame. A non-key frame starts from the models the last one finished with, and on video that is
a large part of what it saves.

So frame zero decoded bit-exactly and frame one failed, which reads as a bug in the second frame and
is really a missing line in the first.

## Two planes share one set of models, running

`plane_count` is the number of **context sets**, not of image planes: the two chroma planes share
one, and so do two of the three RGB planes. And they share them *running* — the states the U plane
finishes with are the states the V plane starts from. A decoder that gives them a set each decodes
the first perfectly and the second not at all.

## And one detail of the RGB path

The three planes are decoded **line by line across the planes** — all three lines of row zero, then
all three of row one — because they share one range coder, so the interleaving is the order the bits
are in and not an implementation choice. The two difference planes are nine bits wide rather than
eight, because a difference of two eight-bit values needs the extra one.

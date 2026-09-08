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

## The other entropy coder

FFV1 has two, chosen per file, and they share nothing below the predictor. `-coder 1` selects the
range coder, which is what preservation guidance asks for and what `rangecoder.lisp` implements.
`-coder 0` selects adaptive Rice codes — and it is ffmpeg's **default**, so a file made by someone
who did not pass the flag is coded that way, which makes it at least as common as the archival
setting.

The Rice coder is LOCO-I's, which is to say JPEG-LS's. Each context keeps four running numbers: a
count, the sum of the absolute residuals, the accumulated signed drift, and a bias. The count and
the error sum give the Rice parameter by doubling the count until it passes the error sum. The drift
and the bias correct for a predictor that is consistently high or low in this context, which the
range coder learns for free and this one has to be told; and the correction is applied twice, once
by adding the bias and once by conditionally inverting the residual's sign, which is the part that
looks like a typo and is not.

The **run mode** has no counterpart on the range-coded side. In the flattest context — every
quantised gradient zero — samples are not coded at all: a length is coded and the samples in it are
whatever the predictor says. The run level climbs while runs keep succeeding and falls when one is
cut short, so a flat picture reaches runs of millions and a busy one never leaves the first few
levels. The residual that ends a run is coded with one added to it, because a residual of zero
would not have ended it.

### The two coders share a slice, one after the other

Even a Golomb-Rice file **range codes its slice header**. The Rice bits begin where the range coder
finished — specifically one byte *before* its read position, because the coder has always read one
byte further than it has consumed. Version 3.2 and later spend one extra range-coded bit at the
handover, purely to flush that byte to a known place.

### RGB is nine bits wide in all three planes

Two of the three planes need it: the reversible colour transform's differences span twice the sample
range. The third does not, and every version before 4.8 widened it anyway. Under the range coder the
width is only a mask and the extra bit costs nothing, so getting it wrong there is invisible; under
the Rice coder the width is the escape field's size, and getting it wrong desynchronises the stream
at the first sample. That is exactly how it was found.

## Versions 0 and 1

They keep the header in the first key frame instead of the container, so a version 0 or 1 track in
Matroska has an *empty* CodecPrivate — the forty bytes of BITMAPINFOHEADER and nothing after it.
That absence is the signal, and it is what `make-ffv1-decoder-for-frames` exists for: the decoder is
built knowing only the picture size, and configures itself when the first key frame arrives.

The in-frame header is the configuration record's first half, same fields, same order, same coder.
What is missing is everything that came later: no micro version, no slice geometry, no
error-detection flag, no per-plane quantiser table choice — one table set serves every plane. And
version 0 has no sample-depth field at all, because eight bits is all it could code.

There is no per-slice header before version 3, either. One slice, the whole picture.

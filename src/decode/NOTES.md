# Notes on the VP8 decoder

## Samples are octets, and were not

The planes held one sample per `fixnum` — eight bytes for a number that never leaves 0..255 — and
then, after the repository-wide type sweep, per `(signed-byte 32)`. They are `(unsigned-byte 8)`
now, which is what the format says they are.

The working set at 1920x1080 went from 24.6 MB to 3.1 MB for the three current-picture planes; at
640x360, from 2.8 MB to 348 kB. That is the whole of the difference between a picture that misses
cache on every pass over it and one that mostly does not, and it shows up exactly where you would
expect it to: the change is worth about 1.25x at 640x360 and about 1.37x at 1920x1080.

Every value written into a plane is already the output of a clamp — a prediction (the only
unbounded one, TM_PRED, clamps), a filtered sample, or a residual added and clamped — so nothing
had to change to make the narrower type correct. `plane->rframe-plane` had been asserting it all
along, with a `(the (unsigned-byte 8) ...)` around the read, and that copy is now a block move
rather than a loop over samples: another two per cent.

## The deblocking kernels load their eight samples once

Written the obvious way, the filter had an `LF-DIFF` taking the array and two indices. The gate
calls it seven times, the high-edge-variance test twice more, and then the filter itself reads six
of the same samples again — about twenty-four loads of eight values. They are in L1 either way, but
this is the busiest function in the decoder by a wide margin: a macroblock edge is filtered
sixty-four times per macroblock and a 640x360 picture has nine hundred macroblocks, so the kernels
run about three and a half million times in sixty pictures.

Loading the eight into locals and passing values rather than indices took `KERNEL-MB` from 708
profile samples to 582 without changing a single arithmetic operation.

## The struct slots that had no types

`above-y`, `above-u`, `above-v`, `above-y2`, `above-bmode`, `mb-seg`, `ycoeffs`, `ublocks`,
`vblocks`, `y2coeffs`, `bmodes` and the prediction buffers were declared with no `:type` at all, so
every `aref` on them was a full call to `HAIRY-DATA-VECTOR-REF`. `DECODE-RESIDUE`, which touches
them once per coefficient block, was spending about four per cent of the decode inside those calls.
Typing the slots — and giving each an empty array of the right type as its default, so the type is
unconditional rather than `(or null ...)` — removed all of it.

## The edge is the unit of work, not the line

The kernels filtered one line and the caller drove them along the edge. That made about a hundred
and seventy thousand calls a frame at 640x360, each of which set up its arguments and recomputed the
seven sample offsets from the edge step it had been handed — none of which varies along an edge.
Passing the edge instead of the line turns those into nine hundred calls and hoists all of it: ten
per cent at 640x360.

## The ±128 was not doing anything

RFC 6386 §15.2 is written on SIGNED bytes: subtract 128 from each sample, filter, add 128 back.
Written that way the kernel did twelve additions per line that cancel. Every place a sample appears
in the filter's arithmetic it appears as a DIFFERENCE of two of them — `(p1 - q1)`, `(q0 - p0)` —
and the offsets cancel there; and at the other end `c8(x - 128) + 128` is exactly `clamp255(x)` for
every integer x, because `c8` clamps to [-128,127] and the 128 puts that range back at [0,255]. So
the conversion is unnecessary at both ends. Five per cent, and the kernel is shorter for it.

## What is left, and why it stops here

`%EDGE-MB` is about forty per cent of a VP8 decode and it is now the arithmetic §15 specifies.

Two things were considered and are not worth doing, both for reasons worth writing down.

**Reordering the gate does nothing, because the gate almost always passes.** Counted over sixty
pictures of 640x360: 3,708,640 edges tested, 3,195,749 filtered — **86.2 per cent** — of which only
13.8 per cent take the narrow high-edge-variance path. Short-circuiting an `AND` helps when it
short-circuits; this one does not.

**Filtering several lines at once needs a load this implementation cannot make.** For a horizontal
edge the eight lines are contiguous, so the eight rows `p3..q3` are laid out exactly as eight
eight-lane words — the arrangement libvpx's SSE2 version uses. SBCL has no unaligned sixty-four bit
load from a `(unsigned-byte 8)` array: `%vector-raw-bits` is indexed in whole words, and the edge
offsets are arbitrary. Assembling each word from eight byte loads and shifts costs more than the
lane-parallel arithmetic saves, and the arithmetic itself would need signed saturating add emulated
by hand — a great deal of intricate masking, in the one function where being wrong diverges the
picture everywhere. It wants a vector primitive, not more cleverness.

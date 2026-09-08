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

## What is left

`KERNEL-MB` is still 38 per cent of a VP8 decode, and it is now genuinely the arithmetic RFC 6386
§15 specifies rather than anything the compiler was failing to see. Going further would mean
filtering more than one line at a time — for a vertical edge the eight samples are contiguous and
could be loaded as one machine word — which is a different kind of change from any of the above.

# MPEG-4 Part 2: the four things that were not obvious

The codec is roughly MPEG-2's size with three genuinely new ideas — intra prediction from
neighbours, four motion vectors per macroblock, and vectors that may leave the picture — and one
that is not in the specification's description of the codec at all.

## Resync markers are not optional to implement

ffmpeg's encoder cuts every picture into video packets by default, so the first attempt decoded
exactly one macroblock row and then failed on a macroblock type that was actually a resync marker.

A packet is a fresh start for prediction, and the rules for that are **per neighbour** rather than
per block: on a packet's first line the blocks above are in another packet, at its first column the
same is true to the left, and "first line" runs until the macroblock directly *below* the one the
packet started at — which for a packet beginning mid-row includes part of the next row.

**And in a B picture a marker may not be ours yet.** A skipped B macroblock costs no bits, so a run
of them at the end of a packet leaves the marker sitting immediately after the last *coded*
macroblock, several macroblocks before the decoder's count reaches it. Taking the marker as soon as
it appears drops that run silently, and the packet after it then decodes perfectly into the wrong
place.

## The chroma vector is derived, not halved

A macroblock with one motion vector derives its chroma vector by the same rule as one with four:
the vectors are **summed** and put through a rounding table. Halving the single vector looks
equivalent and disagrees at every odd vector, which is half of them.

The symptom is worth remembering: **luma perfect, chroma soft**. That reads like a colour-space bug
rather than a motion one. Splitting the error count by plane found it in one step.

With quarter-sample motion there are *two* steps down rather than one — halve toward zero into
half-sample, then halve again with a sticky low bit — and the sticky bit is what stops a vector of
one becoming zero.

## Quarter-sample interpolation cascades

The obvious reading of the diagonal quarter positions is a four-way average of the integer samples,
the horizontally filtered ones, the vertically filtered ones and the doubly filtered ones. That is
what they look like, and it is not what they are. They are built in stages: the horizontal filter is
averaged with the integer samples *first*, the vertical filter is applied to that average, and the
result is averaged with it. The difference is only in the rounding — but ffmpeg keeps both forms,
the four-way one under the name `_old_c`, and the cascade is the one that is registered.

The average with the integer samples happens **only when a vertical stage follows**. With no
vertical stage the average happens once, at the end; doing it in both places softens every such
block by a rounding step.

## The filter's edges mirror, and the two mirrors are not symmetric

The eight-tap kernel is applied to a window one sample wider than the block, and taps falling
outside that window take the reflection rather than the edge sample. Both reflections are about the
**outer half-sample positions**, which makes them look asymmetric written down: below the window the
mirror is about -0.5, so `s[-1]` is `s[0]`; above it the mirror is about `n+0.5`, so `s[n+1]` is
`s[n]`.

Reflecting the top about `n` instead — the natural guess, and what you get if you read the bottom as
reflecting about 0 — is wrong by one sample in three of the eight taps, at every block edge, which
is everywhere. It cost about a hundred and thirty wrong samples per picture out of thirty-eight
thousand: small, structured, and completely invisible without a bit-exact comparison.

#!/bin/bash
# Fetch the official conformance suites into $CONF (default /tmp/conf).
#
# WHY THIS EXISTS.  Every fixture in this repository is something ffmpeg or x264 or libvpx produced,
# and a mainstream encoder uses a narrow slice of the standard it implements.  A decoder can be
# bit-exact on all of them and still be missing features that no such encoder emits — which is
# exactly what happened here: VP8 passed seventeen of seventeen official vectors, but VP9 failed
# seven of the feature vectors and H.264 five of the JVT streams, and AC-3's dynamic range was
# entirely wrong because ffmpeg's encoder never writes the field.
#
# These are not committed.  Some are film excerpts; all of them are large; and they are served from
# ffmpeg's FATE suite, which is where they should be fetched from rather than mirrored here.
set -e
CONF="${CONF:-/tmp/conf}"
BASE=https://fate-suite.ffmpeg.org
get() { mkdir -p "$CONF/$1"; ( cd "$CONF/$1" && curl -sSLO "$BASE/$2/$3" ); }

echo "VP8: the seventeen official libvpx vectors"
for i in $(seq -w 1 17); do get vp8 vp8-test-vectors-r1 "vp80-00-comprehensive-0$i.ivf"; done

echo "VP9: the feature vectors, plus a sample of the quantiser and size sweeps"
curl -sSL "$BASE/vp9-test-vectors/" | grep -oE 'href="vp9[^"]+\.webm"' | sed 's/href="//;s/"//' \
  > "$CONF/vp9-all.txt"
{ grep -vE "00-quantizer|0[23]-size" "$CONF/vp9-all.txt"
  grep "00-quantizer" "$CONF/vp9-all.txt" | head -4
  grep "02-size"     "$CONF/vp9-all.txt" | head -4
  grep "03-size"     "$CONF/vp9-all.txt" | head -4; } | while read -r f; do
  get vp9 vp9-test-vectors "$f"
done

echo "H.264: a spread of the JVT conformance streams"
for f in BA1_FT_C.264 BA1_Sony_D.jsv BA2_Sony_F.jsv BA3_SVA_C.264 BAMQ1_JVC_C.264 \
         BASQP1_Sony_C.jsv CI_MW_D.264 CI1_FT_B.264 MIDR_Mitsubishi_D.264 NL1_Sony_D.jsv \
         NRF_MW_E.264 SVA_BA1_B.264 SVA_BA2_D.264 SVA_Base_B.264 SVA_CL1_E.264 SVA_FM1_E.264 \
         SVA_NL1_B.264 SVA_NL2_E.264 CABA1_SVA_B.264 CABA1_Sony_D.jsv CABA2_SVA_B.264 \
         CABA3_SVA_B.264 CABA3_TOSHIBA_E.264 CABACI3_Sony_B.jsv CABAST3_Sony_E.jsv \
         CABASTBR3_Sony_B.jsv CACQP3_Sony_D.jsv CAFI1_SVA_C.264 CAMA1_Sony_C.jsv \
         CANL1_SVA_B.264 CANL1_Sony_E.jsv CANL2_SVA_B.264 CANL3_SVA_B.264 CANL4_SVA_B.264 \
         CAPCM1_Sand_E.264 CAQP1_Sony_B.jsv CAWP1_TOSHIBA_E.264 CAWP5_TOSHIBA_E.264; do
  get h264 h264-conformance "$f"
done

echo "AC-3: real Dolby-encoded material, which ffmpeg's encoder cannot stand in for"
for f in millers_crossing_4.0.ac3 monsters_inc_2.0_192_small.ac3 monsters_inc_5.1_448_small.ac3; do
  get ac3 ac3 "$f"
done

echo "Vorbis: including two deliberately truncated files"
for f in 1.0-test_small.ogg 1.0.1-test_small.ogg 6.ogg; do get audio vorbis "$f"; done

echo "the reference decodes"
for f in "$CONF"/vp8/*.ivf "$CONF"/vp9/*.webm "$CONF"/h264/*.264 "$CONF"/h264/*.jsv; do
  [ -f "$f" ] || continue
  ffmpeg -v error -y -i "$f" -f rawvideo -pix_fmt yuv420p "$f.ref.yuv" 2>/dev/null || true
done
echo "done: $(du -sh "$CONF" | cut -f1) in $CONF"

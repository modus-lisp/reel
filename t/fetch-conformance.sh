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
# -f, so that a 404 is an error rather than an HTML page saved under the name of a video file.
# Without it a mistyped fixture name produces a 196-byte "Not Found" document that every tool
# downstream reports as a corrupt stream, which is a long way from the real problem.
get() {
  mkdir -p "$CONF/$1"
  if ! ( cd "$CONF/$1" && curl -sSfLO "$BASE/$2/$3" ); then
    echo "  could not fetch $3" >&2
    rm -f "$CONF/$1/$3"
  fi
}

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
         BASQP1_Sony_C.jsv CI_MW_D.264 CI1_FT_B.264 NL1_Sony_D.jsv \
         NRF_MW_E.264 SVA_BA1_B.264 SVA_BA2_D.264 SVA_Base_B.264 SVA_CL1_E.264 SVA_FM1_E.264 \
         SVA_NL1_B.264 SVA_NL2_E.264 CABA1_SVA_B.264 CABA1_Sony_D.jsv CABA2_SVA_B.264 \
         CABA3_SVA_B.264 CABA3_TOSHIBA_E.264 CABACI3_Sony_B.jsv CABAST3_Sony_E.jsv \
         CABASTBR3_Sony_B.jsv CACQP3_Sony_D.jsv CAFI1_SVA_C.264 CAMA1_Sony_C.jsv \
         CANL1_SVA_B.264 CANL1_Sony_E.jsv CANL2_SVA_B.264 CANL3_SVA_B.264 CANL4_SVA_B.264 \
         CAPCM1_Sand_E.264 CAQP1_Sony_B.jsv CAWP1_TOSHIBA_E.264 CAWP5_TOSHIBA_E.264 \
         AUD_MW_E.264 BAMQ2_JVC_C.264 BA_MW_D.264 BANM_MW_D.264 CABA2_Sony_E.jsv \
         CABA3_Sony_C.jsv CANL1_TOSHIBA_G.264 CANL2_Sony_E.jsv CANL3_Sony_C.jsv \
         CAPCMNL1_Sand_E.264 CAPM3_Sony_D.jsv CVBS3_Sony_C.jsv CVFC1_Sony_C.jsv \
         CVPCMNL1_SVA_C.264 CVPCMNL2_SVA_C.264 CVSE2_Sony_B.jsv CVSE3_Sony_H.jsv \
         CVSEFDFT3_Sony_E.jsv CVWP1_TOSHIBA_E.264 CVWP2_TOSHIBA_E.264 CVWP3_TOSHIBA_E.264 \
         CVWP5_TOSHIBA_E.264 HCBP1_HHI_A.264 HCBP2_HHI_A.264 HCMP1_HHI_A.264 LS_SVA_D.264 \
         MIDR_MW_D.264 MPS_MW_A.264 MR1_MW_A.264 NL2_Sony_H.jsv NL3_SVA_E.264 \
         NLMQ1_JVC_C.264 NLMQ2_JVC_C.264 SL1_SVA_B.264; do
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
  # -fps_mode passthrough, AND AFTER -i, or ffmpeg pads the output to a constant frame rate by
  # repeating pictures and the reference no longer matches the stream picture for picture.  Before
  # -i it is silently accepted and does nothing.  This cost a day the first time.
  ffmpeg -v error -y -i "$f" -fps_mode passthrough -f rawvideo -pix_fmt yuv420p "$f.ref.yuv" \
    2>/dev/null || true
done
# CVFC1 is the one stream in the suite that crops from the LEFT as well as the right, and ffmpeg
# does not apply frame_crop_left_offset: it emits 326-wide frames where the sequence parameter set
# says 300, with the picture sitting 26 samples in.  Its own stream metadata says 300, so this is an
# inconsistency in the oracle rather than a disagreement about the standard.  The decoder is right,
# so the reference is corrected here rather than the decoder being bent to match it.
if [ -f "$CONF/h264/CVFC1_Sony_C.jsv" ]; then
  ffmpeg -v error -y -i "$CONF/h264/CVFC1_Sony_C.jsv" -vf "crop=300:168:26:0" \
    -fps_mode passthrough -f rawvideo -pix_fmt yuv420p \
    "$CONF/h264/CVFC1_Sony_C.jsv.ref.yuv" 2>/dev/null || true
fi

echo "done: $(du -sh "$CONF" | cut -f1) in $CONF"

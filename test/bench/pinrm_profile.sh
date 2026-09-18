#!/bin/bash
# pinrm_profile.sh -- perf record/report for one arm x one API path at t=96.
# usage: pinrm_profile.sh <arm> <indiv|batch> <shared|private> [secs]
# Prints the top self-time symbols and the __memp_fget + pin-atomics share,
# which is the premise this re-measurement is testing (BATCHED-READS-RESULTS.md
# claims ~63% of self time after the cursor mutex was removed).
set -u
ARM=${1:?arm}
MODE=${2:?indiv|batch}
ENVK=${3:-shared}
SECS=${4:-8}
BIN=/home/admin/pin_bench-$ARM
case "$ARM" in base2) BIN=/home/admin/pin_bench-base ;; esac
D=/home/admin/runs/prof-$ARM-$MODE-$ENVK
OUT=/home/admin/runs/prof-$ARM-$MODE-$ENVK.txt
mkdir -p "$D"; find "$D" -mindepth 1 -delete
if [ "$ENVK" = private ]; then export PIN_PRIVATE=1; else unset PIN_PRIVATE; fi
cd "$D" || exit 1
PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="prof-$ARM/$ENVK/p" \
  perf record -q -F 97 --call-graph fp -o "$D/perf.data" -- \
  "$BIN" "$MODE" 200000 16 2 "$SECS" 96 >"$OUT" 2>"$OUT.err"
{
  echo "=== arm=$ARM mode=$MODE env=$ENVK"
  grep -E '^RESULT|^FAIL' "$OUT" || echo "NO RESULT LINE (failed run)"
  echo "--- self time (top 25)"
  perf report -i "$D/perf.data" --no-children --percent-limit 0.10 \
    --stdio 2>/dev/null | grep -E '^ +[0-9]' | head -25
} >"$OUT.rep" 2>&1
cat "$OUT.rep"
# The premise check: memp_fget + pin atomics share of self time.
perf report -i "$D/perf.data" --no-children --stdio 2>/dev/null \
 | grep -E '^ +[0-9]' \
 | awk -v arm="$ARM" -v mode="$MODE" -v envk="$ENVK" '
   { pct=$1; sub("%","",pct); sym=$NF; tot+=pct;
     if (sym ~ /memp_fget|os_atomic|atomic_read|atomic_dec|atomic_inc/) pin+=pct;
     if (sym ~ /tas_mutex/) mtx+=pct }
   END { printf "VERDICT profile arm=%s mode=%s env=%s pin_share=%.2f%% mutex_share=%.2f%% accounted=%.2f%%\n",
         arm, mode, envk, pin, mtx, tot }' | tee -a "$OUT.rep"

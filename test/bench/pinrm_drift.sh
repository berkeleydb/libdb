#!/bin/bash
# pinrm_drift.sh -- find the cause of the cross-run baseline discrepancy.
#
# The sweep measured base at private/batch/96 = 7.86-7.95M (two independent
# runs, CV 1.3%); the confirm run measured the SAME binary at 4.85M (CV 0.6%).
# Both are internally tight, so something differs between the runs rather than
# the numbers being noisy.  The two candidate differences are:
#
#   dir     the sweep gives every rep a FRESH environment directory
#           (env-...-r$rep), the confirm run REUSES one directory per cell
#           (cf-...) after emptying it;
#   order   the sweep runs shared before private for each arm, the confirm run
#           runs private first.
#
# Test both, interleaved, in one run so box drift cannot masquerade as either.
set -u
REPS=${1:-4}
OUT=${2:-/home/admin/runs/drift.txt}
BIN=/home/admin/pin_bench-base
mkdir -p "$(dirname "$OUT")"
echo "# drift probe start $(date -Is) reps=$REPS" >>"$OUT"
for rep in $(seq 1 "$REPS"); do
  if [ $((rep % 2)) = 1 ]; then order="fresh reuse"; else order="reuse fresh"; fi
  for variant in $order; do
    case $variant in
      fresh) D=/home/admin/runs/dr-fresh-$rep ;;
      reuse) D=/home/admin/runs/dr-reuse ;;
    esac
    mkdir -p "$D"; find "$D" -mindepth 1 -delete
    PIN_PRIVATE=1 PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="$variant/private/rep$rep" \
      timeout 300 "$BIN" batch 200000 16 2 5 96 >>"$OUT" 2>>"$OUT.err" \
      || echo "FAIL variant=$variant rep=$rep exit=$?" >>"$OUT"
    find "$D" -mindepth 1 -delete
  done
  # And the same cell reached the way the sweep reaches it: shared first, in
  # the same process-sequence position, with a fresh dir.
  D=/home/admin/runs/dr-seq-$rep
  mkdir -p "$D"; find "$D" -mindepth 1 -delete
  PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="seqshared/shared/rep$rep" \
    timeout 300 "$BIN" batch 200000 16 2 5 1 32 96 >>"$OUT" 2>>"$OUT.err"
  find "$D" -mindepth 1 -delete
  D=/home/admin/runs/dr-seqp-$rep
  mkdir -p "$D"; find "$D" -mindepth 1 -delete
  PIN_PRIVATE=1 PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="seqsweep/private/rep$rep" \
    timeout 300 "$BIN" batch 200000 16 2 5 1 32 96 >>"$OUT" 2>>"$OUT.err"
  find "$D" -mindepth 1 -delete
done
echo "# drift probe done $(date -Is)" >>"$OUT"
grep -E '^RESULT|^FAIL' "$OUT" | tail -40
echo DRIFT-DONE

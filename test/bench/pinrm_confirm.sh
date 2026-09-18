#!/bin/bash
# pinrm_confirm.sh -- settle the cross-run discrepancy and attribute the gain in
# ONE alternating run.  The first sweep and the kill-switch A/B disagreed on the
# same bhpin binary at private/batch/96 (10.76M vs 7.29M), so those two runs are
# not comparable to each other: only arms measured inside the SAME run are.
# This script therefore puts all four arms that matter in one rep loop:
#
#   base    v2026.09.6 baseline
#   base2   the same baseline binary again (within-run noise floor)
#   r1on    bhpin build, optimistic path allowed to fire
#   r1off   bhpin build, DB_NO_BHPIN=1 (same binary, path disabled)
#
# so base-vs-r1on (cross-build) and r1on-vs-r1off (same-binary kill switch) are
# both measured under identical box conditions, and the two attributions can be
# checked against each other.
# usage: pinrm_confirm.sh <reps> <outfile> [secs]
set -u
REPS=${1:-5}
OUT=${2:-/home/admin/runs/confirm.txt}
SECS=${3:-5}
ARMS="base base2 r1on r1off"
mkdir -p "$(dirname "$OUT")"
echo "# confirm start $(date -Is) reps=$REPS secs=$SECS" >>"$OUT"
n=$(echo $ARMS | wc -w)
for rep in $(seq 1 "$REPS"); do
  set -- $ARMS
  for _ in $(seq 1 $(( (rep - 1) % n )) ); do
    first=$1; shift; set -- "$@" "$first"
  done
  order="$*"
  echo "# rep=$rep order=$order $(date -Is)" >>"$OUT"
  for arm in $order; do
    case $arm in
      base|base2) BIN=/home/admin/pin_bench-base;  NOBH=0 ;;
      r1on)       BIN=/home/admin/pin_bench-bhpin; NOBH=0 ;;
      r1off)      BIN=/home/admin/pin_bench-bhpin; NOBH=1 ;;
    esac
    for envk in private shared; do
      for mode in indiv batch; do
        D=/home/admin/runs/cf-$arm-$envk-$mode
        mkdir -p "$D"; find "$D" -mindepth 1 -delete
        if [ "$envk" = private ]; then export PIN_PRIVATE=1; else unset PIN_PRIVATE; fi
        if [ "$NOBH" = 1 ]; then export DB_NO_BHPIN=1; else unset DB_NO_BHPIN; fi
        PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="$arm/$envk/rep$rep" \
          timeout 300 "$BIN" "$mode" 200000 16 2 "$SECS" 1 32 96 \
          >>"$OUT" 2>>"$OUT.err" \
          || echo "FAIL arm=$arm env=$envk mode=$mode rep=$rep exit=$?" >>"$OUT"
        find "$D" -mindepth 1 -delete
      done
    done
  done
done
echo "# confirm done $(date -Is)" >>"$OUT"
echo CONFIRM-DONE

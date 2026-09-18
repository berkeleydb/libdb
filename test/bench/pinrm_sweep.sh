#!/bin/bash
# pinrm_sweep.sh -- the measurement.  6 arms x 2 API paths x 2 env kinds x
# 3 thread counts x N reps, with the ARM ORDER ROTATED every rep so no arm is
# systematically favoured by box warmth.
#
# Arms: base, base2 (the SAME baseline binary again -- this is the noise floor,
# baseline measured against itself), bhpin, rsnap, lockrp, mpoolp.
#
# usage: pinrm_sweep.sh <reps> <outfile> [warmup] [secs]
set -u
REPS=${1:-5}
OUT=${2:-/home/admin/runs/sweep.txt}
WARM=${3:-2}
SECS=${4:-5}
NKEYS=200000
BATCH=16
THREADS="1 32 96"

ARMS="base base2 bhpin rsnap lockrp mpoolp"
binof() { case $1 in base2) echo /home/admin/pin_bench-base ;;
                     *)     echo /home/admin/pin_bench-$1 ;; esac; }

mkdir -p "$(dirname "$OUT")"
echo "# pinrm sweep start $(date -Is) reps=$REPS warm=${WARM}s secs=${SECS}s nkeys=$NKEYS batch=$BATCH" >>"$OUT"

n=$(echo $ARMS | wc -w)
for rep in $(seq 1 "$REPS"); do
  # rotate: shift the arm list left by (rep-1) positions
  set -- $ARMS
  for _ in $(seq 1 $(( (rep - 1) % n )) ); do
    first=$1; shift; set -- "$@" "$first"
  done
  order="$*"
  echo "# rep=$rep order=$order $(date -Is)" >>"$OUT"
  for arm in $order; do
    bin=$(binof "$arm")
    for envk in shared private; do
      for mode in indiv batch; do
        D=/home/admin/runs/env-$arm-$envk-$mode-r$rep
        mkdir -p "$D"; find "$D" -mindepth 1 -delete
        if [ "$envk" = private ]; then export PIN_PRIVATE=1; else unset PIN_PRIVATE; fi
        PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="$arm/$envk/rep$rep" \
          timeout 300 "$bin" "$mode" "$NKEYS" "$BATCH" "$WARM" "$SECS" $THREADS \
          >>"$OUT" 2>>"$OUT.err" \
          || echo "FAIL arm=$arm env=$envk mode=$mode rep=$rep exit=$?" >>"$OUT"
        find "$D" -mindepth 1 -delete
      done
    done
  done
done
echo "# pinrm sweep done $(date -Is)" >>"$OUT"
echo SWEEP-DONE

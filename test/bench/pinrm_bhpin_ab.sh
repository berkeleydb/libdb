#!/bin/bash
# pinrm_bhpin_ab.sh -- attribute perf/bhpin-r1's gain to the fast path itself,
# using the branch's OWN kill switch: the SAME binary, the same build, the same
# environment, with and without DB_NO_BHPIN=1.  This removes build variance,
# compiler-layout luck and cross-arm environment differences from the
# comparison -- the only thing that changes is whether the optimistic path is
# allowed to fire.  Arms alternate order per rep.
# usage: pinrm_bhpin_ab.sh <reps> <outfile> [secs]
set -u
REPS=${1:-5}
OUT=${2:-/home/admin/runs/bhpin_ab.txt}
SECS=${3:-5}
BIN=/home/admin/pin_bench-bhpin
mkdir -p "$(dirname "$OUT")"
echo "# bhpin kill-switch A/B start $(date -Is) reps=$REPS secs=$SECS" >>"$OUT"
for rep in $(seq 1 "$REPS"); do
  if [ $((rep % 2)) = 1 ]; then order="on off"; else order="off on"; fi
  echo "# rep=$rep order=$order" >>"$OUT"
  for arm in $order; do
    for envk in private shared; do
      for mode in indiv batch; do
        D=/home/admin/runs/ab-$arm-$envk-$mode
        mkdir -p "$D"; find "$D" -mindepth 1 -delete
        if [ "$envk" = private ]; then export PIN_PRIVATE=1; else unset PIN_PRIVATE; fi
        if [ "$arm" = off ]; then export DB_NO_BHPIN=1; else unset DB_NO_BHPIN; fi
        PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="r1$arm/$envk/rep$rep" \
          timeout 300 "$BIN" "$mode" 200000 16 2 "$SECS" 1 32 96 \
          >>"$OUT" 2>>"$OUT.err" \
          || echo "FAIL arm=$arm env=$envk mode=$mode rep=$rep exit=$?" >>"$OUT"
        find "$D" -mindepth 1 -delete
      done
    done
  done
done
echo "# bhpin kill-switch A/B done $(date -Is)" >>"$OUT"
echo AB-DONE

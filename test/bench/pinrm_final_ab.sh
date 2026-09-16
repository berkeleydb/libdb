#!/bin/bash
# pinrm_final_ab.sh -- the ONLY comparison on this box that is not confounded by
# memory layout.
#
# Discovery that forces this: DB_PRIVATE throughput at t=96 is BIMODAL -- 7.9M
# or 4.8M, a 1.65x step, ~0% spread within each mode -- and which mode you get
# is selected by the LENGTH of the $PIN_HOME path (38 chars -> 4.7M, 42 chars ->
# 7.9M, independent of the name's content; ASLR is off, so this is deterministic
# region placement, not noise).  Both earlier scripts gave different arms
# different directory names, so every cross-arm DB_PRIVATE ratio they produced
# is confounded with a 1.65x layout artifact.
#
# So: ONE binary (the bhpin build), ONE directory reused by both arms, the only
# variable being DB_NO_BHPIN.  Arms alternate per rep.  Repeated for two
# directory-name lengths, one from each mode, so the result is reported per mode
# rather than averaged across a step change.
set -u
REPS=${1:-6}
OUT=${2:-/home/admin/runs/final_ab.txt}
SECS=${3:-5}
BIN=/home/admin/pin_bench-bhpin
mkdir -p "$(dirname "$OUT")"
echo "# final A/B start $(date -Is) reps=$REPS secs=$SECS" >>"$OUT"
# Two homes whose LENGTHS sit in the two observed modes (38 and 42 chars).
H38=/home/admin/runs/cf-base-private-batch
H42=/home/admin/runs/env-base-private-batch-r1
for rep in $(seq 1 "$REPS"); do
  if [ $((rep % 2)) = 1 ]; then order="on off"; else order="off on"; fi
  echo "# rep=$rep order=$order" >>"$OUT"
  for H in "$H38" "$H42"; do
    for arm in $order; do
      for envk in private shared; do
        for mode in indiv batch; do
          mkdir -p "$H"; find "$H" -mindepth 1 -delete
          if [ "$envk" = private ]; then export PIN_PRIVATE=1
          else unset PIN_PRIVATE; fi
          if [ "$arm" = off ]; then export DB_NO_BHPIN=1
          else unset DB_NO_BHPIN; fi
          PIN_HOME=$H PIN_CACHE_MB=512 \
            PIN_TAG="r1$arm-len${#H}/$envk/rep$rep" \
            timeout 300 "$BIN" "$mode" 200000 16 2 "$SECS" 1 32 96 \
            >>"$OUT" 2>>"$OUT.err" \
            || echo "FAIL arm=$arm len=${#H} env=$envk mode=$mode rep=$rep exit=$?" >>"$OUT"
          find "$H" -mindepth 1 -delete
        done
      done
    done
  done
done
echo "# final A/B done $(date -Is)" >>"$OUT"
echo FINAL-AB-DONE

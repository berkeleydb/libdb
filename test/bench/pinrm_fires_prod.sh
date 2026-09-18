#!/bin/bash
# pinrm_fires_prod.sh -- how often does R1's fast path fire, WITHOUT the
# DIAGNOSTIC build?  Needed because the DIAGNOSTIC build panics as soon as the
# path fires (mp_fput.c:197 unlocks a shared latch R1 never took), so the
# DIAGNOSTIC counters cannot be read from a completed private-env run.
#
# Instead count it from the production build with perf: the optimistic path is
# the ONLY caller of __memp_fget that returns without entering
# __db_tas_mutex_readlock_int on bhp->mtx_buf, so the readlock sample count per
# read is the observable.  Compare R1 ON vs OFF on the same binary and the same
# directory: a large drop in shared-latch samples with unchanged read volume is
# the path firing.
set -u
BIN=/home/admin/pin_bench-bhpin
H=/home/admin/runs/env-base-private-batch-r1   # 42-char home (upper layout mode)
for arm in on off; do
  mkdir -p "$H"; find "$H" -mindepth 1 -delete
  if [ "$arm" = off ]; then export DB_NO_BHPIN=1; else unset DB_NO_BHPIN; fi
  export PIN_PRIVATE=1
  O=/home/admin/runs/fires-prod-$arm
  ( cd "$H" && PIN_HOME=$H PIN_CACHE_MB=512 PIN_TAG="prod$arm/private/p" \
    perf record -q -F 997 -o "$O.data" -- \
    "$BIN" batch 200000 16 2 6 96 ) >"$O.txt" 2>"$O.err"
  echo "=== arm=$arm"
  grep -E '^RESULT|^FAIL' "$O.txt" || echo "NO RESULT LINE (failed run)"
  perf report -i "$O.data" --no-children --stdio 2>/dev/null \
    | grep -E '^ +[0-9]' \
    | awk -v arm="$arm" '
      { pct=$1; sub("%","",pct); sym=$NF;
        if (sym ~ /tas_mutex_readlock/) rl+=pct;
        if (sym ~ /tas_mutex/) mtx+=pct;
        if (sym ~ /memp_fget/) fg+=pct }
      END { printf "VERDICT fires-prod arm=%s readlock_self=%.2f%% all_mutex_self=%.2f%% memp_fget_self=%.2f%%\n", arm, rl, mtx, fg }'
  find "$H" -mindepth 1 -delete
done

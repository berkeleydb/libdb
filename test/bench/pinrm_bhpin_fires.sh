#!/bin/bash
# pinrm_bhpin_fires.sh -- does perf/bhpin-r1's optimistic fast path actually
# FIRE on this workload?  A neutral result from a path that never executes is
# not a measurement of the path.  The branch keeps DIAGNOSTIC-only counters
# (__memp_bhpin_hits / __memp_bhpin_attempts), so build --enable-diagnostic and
# read them from a driver that runs the same read workload.
set -u
WT=/home/admin/wt-bhpin-diag
B=$WT/build_unix
if [ ! -d "$WT" ]; then
  ( cd /home/admin/libdb && git worktree add --detach "$WT" rm/bhpin-r1 ) \
    >/dev/null 2>&1 || { echo "WORKTREE-FAIL"; exit 1; }
  ( cd "$WT" && git checkout -f -- build_windows/ 2>/dev/null; true )
fi
if [ ! -f "$B/libdb.a" ]; then
  mkdir -p "$B"
  ( cd "$B" && ../dist/configure --enable-diagnostic --disable-shared \
      LIBS=-luring CFLAGS="-O2 -g" >conf.log 2>&1 && make -j96 >make.log 2>&1 ) \
    || { echo "BUILD-FAIL (see $B/make.log)"; exit 1; }
fi
cc -O2 -g -pthread -I"$B" /home/admin/pin_fires.c "$B/libdb.a" \
   -lpthread -ldl -luring -o /home/admin/pin_fires 2>&1 | tail -5
[ -x /home/admin/pin_fires ] || { echo "CC-FAIL"; exit 1; }
for envk in private shared; do
  for mode in indiv batch; do
    D=/home/admin/runs/fires-$envk-$mode
    mkdir -p "$D"; find "$D" -mindepth 1 -delete
    if [ "$envk" = private ]; then export PIN_PRIVATE=1; else unset PIN_PRIVATE; fi
    PIN_HOME=$D timeout 300 /home/admin/pin_fires "$mode" 200000 16 3 8 \
      2>&1 | grep -E 'VERDICT|FAIL|error'
    find "$D" -mindepth 1 -delete
  done
done

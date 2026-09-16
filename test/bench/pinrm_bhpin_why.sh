#!/bin/bash
# pinrm_bhpin_why.sh -- WHY does R1's optimistic path never hit?  Same
# DIAGNOSTIC build, but with R1_BHPIN_BREAKDOWN so the branch's own per-reason
# bail counters (notwired / badflag / chain) are compiled in.
set -u
WT=/home/admin/wt-bhpin-why
B=$WT/build_unix
if [ ! -d "$WT" ]; then
  ( cd /home/admin/libdb && git worktree add --detach "$WT" rm/bhpin-r1 ) \
    >/dev/null 2>&1 || { echo "WORKTREE-FAIL"; exit 1; }
  ( cd "$WT" && git checkout -f -- build_windows/ 2>/dev/null; true )
fi
if [ ! -f "$B/libdb.a" ]; then
  mkdir -p "$B"
  ( cd "$B" && ../dist/configure --enable-diagnostic --disable-shared \
      LIBS=-luring CFLAGS="-O2 -g -DR1_BHPIN_BREAKDOWN=1" >conf.log 2>&1 \
    && make -j96 >make.log 2>&1 ) \
    || { echo "BUILD-FAIL (see $B/make.log)"; exit 1; }
fi
cc -O2 -g -pthread -DR1_BHPIN_BREAKDOWN=1 -I"$B" /home/admin/pin_why.c \
   "$B/libdb.a" -lpthread -ldl -luring -o /home/admin/pin_why 2>&1 | tail -5
[ -x /home/admin/pin_why ] || { echo "CC-FAIL"; exit 1; }
for envk in private shared; do
  D=/home/admin/runs/why-$envk
  mkdir -p "$D"; find "$D" -mindepth 1 -delete
  if [ "$envk" = private ]; then export PIN_PRIVATE=1; else unset PIN_PRIVATE; fi
  PIN_HOME=$D timeout 300 /home/admin/pin_why indiv 200000 16 4 6 \
    2>&1 | grep -E 'VERDICT|FAIL|error'
  find "$D" -mindepth 1 -delete
done

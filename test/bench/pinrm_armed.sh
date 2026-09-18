#!/bin/bash
# pinrm_armed.sh -- prove every arm's build differs from the baseline in the way
# its branch claims.  An arm that is byte-identical to base would give a
# perfectly reproducible NULL result that means nothing at all.
set -u
for n in base bhpin rsnap lockrp mpoolp; do
  WT=/home/admin/wt-$n
  B=$WT/build_unix
  EXTRA=""
  [ "$n" = mpoolp ] && EXTRA="-DMPOOL_HOTFIELDS_ISOLATED=1"
  cc -O2 -I"$B" -I"$WT/src" $EXTRA /home/admin/pin_armed.c "$B/libdb.a" \
     -lpthread -ldl -luring -o /home/admin/pin_armed-$n 2>&1 | tail -3
  if [ ! -x /home/admin/pin_armed-$n ]; then echo "ARMED-CC-FAIL $n"; continue; fi
  printf '%-7s ' "$n"
  /home/admin/pin_armed-$n
done

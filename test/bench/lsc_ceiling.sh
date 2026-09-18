#!/bin/sh
# lsc_ceiling.sh -- how big is the prize?  ONE binary, ONE directory reused by
# both arms, only the runtime switch LSC_META_EARLY varying.  That is the only
# unconfounded A/B form on this box (see run_bench.sh's DB_PRIVATE warning).
set -e
B=$HOME/wt-lockscope/test/bench
cd $B
touch commit_bench && make BDB=../../build_unix commit_bench >/dev/null 2>&1 || true

D=/tmp/lscceil
T=${T:-96}
SECS=${SECS:-15}
REPS=${REPS:-3}

for rep in $(seq 1 $REPS); do
  for arm in base early; do
	rm -f $D/* 2>/dev/null || true
	mkdir -p $D
	find $D -mindepth 1 -delete 2>/dev/null || true
	if [ $arm = early ]; then E=1; else E=0; fi
	echo "REP=$rep ARM=$arm LSC_META_EARLY=$E"
	LSC_META_EARLY=$E PREPOP=0 KEYRANGE=1000000 \
	    ./commit_bench $D $T $SECS sync 2>&1 | grep -E 'mode=|PHASE'
  done
done
find $D -mindepth 1 -delete 2>/dev/null || true
echo CEILING_DONE

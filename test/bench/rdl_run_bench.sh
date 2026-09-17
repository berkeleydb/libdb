#!/bin/sh
# rdl_run_bench.sh -- the two-regime x six-isolation-level measurement.
#
# Alternates the arms WITHIN each rep (so a drift in machine state hits every arm
# equally rather than one arm), REPS>=5, and reuses ONE environment directory of
# ONE path length for every arm -- the DB_PRIVATE layout artifact documented at
# the top of run_bench.sh is not in play here (shared env, not DB_PRIVATE), but
# reusing one directory removes the question entirely.
set -e
W=/home/admin/rdl-wt
B=${B:-$W/bp}
S=/tmp/rdl-bench-env
BIN=/tmp/rdl_bench.bin
REPS=${REPS:-5}
SECS=${SECS:-10}
NKEYS=${NKEYS:-100000}
THREADS=${THREADS:-"1 8 32 96"}
ISOS=${ISOS:-"none plain rc uncom si ssi"}
REGIMES=${REGIMES:-"uniform hot"}

rm -f $BIN
gcc -O2 -g -fno-omit-frame-pointer -o $BIN $W/test/bench/rdl_bench.c \
    -I$B -L$B -ldb -lpthread -luring
test -x $BIN || { echo "COMPILE FAILED"; exit 1; }

mkdir -p $S
for rep in $(seq 1 $REPS); do
	for rg in $REGIMES; do
		for iso in $ISOS; do
			find $S -mindepth 1 -delete
			printf "rep=%s " "$rep"
			timeout 900 $BIN $rg $iso $NKEYS $SECS $S $THREADS \
			    2>/dev/null | grep -v setup || echo "ARM FAILED $rg $iso"
		done
	done
done
echo RDL_BENCH_DONE

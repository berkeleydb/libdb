#!/bin/sh
# lsc_repro.sh -- reproduce the convoy and census the locks it forms.
#
# Runs the unwarmed (growing-tree) arm at t=96 and, WHILE it runs, samples
# db_stat -Co (locks grouped by object) so we see which pgno's the waiters
# are queued behind, plus a stack census for caller attribution.
set -e
WT=$HOME/wt-lockscope
B=$WT/test/bench
S=$WT/build_unix
OUT=${OUT:-/tmp/lsc-repro}
T=${T:-96}
SECS=${SECS:-20}
mkdir -p $OUT
D=/tmp/lscenv-$$
mkdir -p $D

cd $B
( PREPOP=0 KEYRANGE=1000000 ./commit_bench $D $T $SECS sync > $OUT/bench.t$T.txt 2>&1 ) &
BPID=$!
sleep 6
# Lock census: 5 samples of "locks grouped by object", 1s apart.
for i in 1 2 3 4 5; do
	$S/db_stat -h $D -Co > $OUT/objects.$i.txt 2>&1 || true
	$S/db_stat -h $D -cl > $OUT/lockstat.$i.txt 2>&1 || true
	sleep 1
done
# Stack census of the benchmark process.
for i in 1 2; do
	timeout 60 gdb -p $BPID -batch -ex 'thread apply all bt 8' \
	    > $OUT/stacks.$i.txt 2>&1 || true
	sleep 1
done
wait $BPID
$S/db_stat -h $D -cl > $OUT/lockstat.final.txt 2>&1 || true
echo "--- bench"; cat $OUT/bench.t$T.txt
echo "--- env: $D"
echo "$D" > $OUT/envdir
echo REPRO_DONE

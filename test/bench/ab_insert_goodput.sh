#!/bin/sh -
#
# ab_insert_goodput.sh -- corrected insert-heavy A/B for RFC 0017 change 1.
#
# The first A/B counted every attempt as throughput, including
# deadlock-aborted txns, which inflated the base arm.  hash_conc_bench now
# reports GOODPUT (committed ops/s) and aborts/s separately; this harness
# re-runs the insert-heavy cells (hash txn + notxn, every thread count) and
# the B-tree insert control, ABAB, 9 reps, base vs branch, same binaries.
#
# Env: BRANCH_BIN BASE_BIN OUT RUNDIR REPS SECS KEYSPACE
# Usage: sh ab_insert_goodput.sh
set -u
BRANCH_BIN=${BRANCH_BIN:?}; BASE_BIN=${BASE_BIN:?}
OUT=${OUT:-/nvme/ab_out}; RUNDIR=${RUNDIR:-/nvme/ab_run}
REPS=${REPS:-9}; SECS=${SECS:-6}; KEYSPACE=${KEYSPACE:-400000}
THREADS="1 2 4 8 16 32"
mkdir -p "$OUT" "$RUNDIR"
RAW="$OUT/raw_goodput.tsv"
echo "arm	type	mix	txn	rep	thr	goodput_per_s	aborts_per_s	p50_us	p99_us	p999_us	max_us	deadlocks	lock_waits" > "$RAW"
one(){ arm=$1;bin=$2;ty=$3;mx=$4;tx=$5;r=$6; cd "$RUNDIR"; find . -mindepth 1 -delete 2>/dev/null
  "$bin" "$ty" "$mx" "$tx" "$KEYSPACE" "$SECS" $THREADS 2>/dev/null |
  awk -v a="$arm" -v ty="$ty" -v mx="$mx" -v tx="$tx" -v r="$r" '/^#/{next}{print a"\t"ty"\t"mx"\t"tx"\t"r"\t"$0}' >> "$RAW"; }
for cell in "hash txn" "hash notxn" "btree txn" "btree notxn"; do
  set -- $cell; ty=$1; tx=$2
  r=1; while [ $r -le $REPS ]; do
    one branch "$BRANCH_BIN" "$ty" insert "$tx" $r
    one base   "$BASE_BIN"   "$ty" insert "$tx" $r
    r=$((r+1)); done
done
echo "ab_insert_goodput.sh: done -> $RAW"

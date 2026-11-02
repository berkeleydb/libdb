#!/bin/sh -
#
# ab_hash_conc.sh -- RFC 0017 change-1 A/B benchmark harness.
#
# Runs hash_conc_bench from the BRANCH library and the BASE (origin/master)
# library in alternation (ABAB...), REPS per cell, and writes every rep as a
# TSV row to $OUT/raw.tsv.  A separate pass runs BASE-vs-BASE as the noise
# floor.  The median and CV are computed later by ab_summary.py.
#
# Env:
#   BRANCH_BIN   path to the branch hash_conc_bench
#   BASE_BIN     path to the base   hash_conc_bench
#   OUT          output dir (default /nvme/ab_out)
#   REPS         reps per cell per arm (default 5)
#   SECS         seconds per run (default 10)
#   KEYSPACE     key space (default 2000000)
#   RUNDIR       scratch dir on the NVMe (default /nvme/ab_run)
#
# Usage: sh ab_hash_conc.sh

set -u

BRANCH_BIN=${BRANCH_BIN:?set BRANCH_BIN}
BASE_BIN=${BASE_BIN:?set BASE_BIN}
OUT=${OUT:-/nvme/ab_out}
REPS=${REPS:-5}
SECS=${SECS:-10}
KEYSPACE=${KEYSPACE:-2000000}
RUNDIR=${RUNDIR:-/nvme/ab_run}
THREADS="1 2 4 8 16 32"

mkdir -p "$OUT" "$RUNDIR"
RAW="$OUT/raw.tsv"
: > "$RAW"
echo "arm	type	mix	txn	rep	thr	ops_per_s	p50_us	p99_us	p999_us	max_us	deadlocks	lock_waits" >> "$RAW"

# Run one cell (a single hash_conc_bench invocation sweeps all thread counts)
# and tag each output row with arm/type/mix/txn/rep.
one() {
	arm=$1; bin=$2; type=$3; mix=$4; txn=$5; rep=$6
	cd "$RUNDIR"
	find . -mindepth 1 -delete 2>/dev/null
	"$bin" "$type" "$mix" "$txn" "$KEYSPACE" "$SECS" $THREADS 2>/dev/null |
	    awk -v a="$arm" -v ty="$type" -v mx="$mix" -v tx="$txn" -v r="$rep" \
	    '/^#/ {next} {print a"\t"ty"\t"mx"\t"tx"\t"r"\t"$0}' >> "$RAW"
}

# Main A/B: alternate branch and base per rep, for every cell.
for type in hash btree; do
  for mix in insert lookup; do
    for txn in txn notxn; do
      rep=1
      while [ "$rep" -le "$REPS" ]; do
        one branch "$BRANCH_BIN" "$type" "$mix" "$txn" "$rep"
        one base   "$BASE_BIN"   "$type" "$mix" "$txn" "$rep"
        rep=$((rep + 1))
      done
    done
  done
done

# Noise floor: base vs base (same binary both arms), hash insert txn only.
rep=1
while [ "$rep" -le "$REPS" ]; do
	one noiseA "$BASE_BIN" hash insert txn "$rep"
	one noiseB "$BASE_BIN" hash insert txn "$rep"
	rep=$((rep + 1))
done

echo "ab_hash_conc.sh: done -> $RAW"

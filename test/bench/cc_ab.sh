#!/bin/bash
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# cc_ab.sh -- A/B driver for the RFC 0021 transient-cursor cache.
# base = origin/master, branch = perf/cursor-cache.  Non-transactional per-key
# DB->get path (batch_bench indiv), which is the path the cache targets (the
# 85%-cursor-mutex profile in BATCHED-READS-RESULTS.md is this workload).
# Interleaved, arm order rotated each rep; fresh load per rep per arm.
set -u
OUT=/data/cc_out; mkdir -p "$OUT"
HOME_DIR=/data/bbhome
BASE=/data/base-batch_bench
BRANCH=/data/branch-batch_bench
NKEYS=${NKEYS:-1000000}
SECS=${SECS:-10}
REPS=${REPS:-6}
THREADS=${THREADS:-"1 4 16 32 64 96"}
MODE=${MODE:-indiv}          # indiv = per-key get (cache target)
BATCH=${BATCH:-1}

F="$OUT/cc_${MODE}.tsv"
[ -f "$F" ] || echo -e "arm\trep\tmode\tthreads\tbatch\tkeys_per_s\tp50_us\tp99_us\tnotfound" > "$F"

fresh() { mkdir -p "$1"; find "$1" -mindepth 1 -delete; }

run_arm() {  # run_arm <bin> <arm> <rep>
	local bin=$1 arm=$2 rep=$3 t row
	fresh "$HOME_DIR"
	# One invocation loads once then runs every thread count in order.
	BATCH_HOME="$HOME_DIR" "$bin" "$MODE" "$NKEYS" "$BATCH" "$SECS" $THREADS \
	    2>>"$OUT/cc.err" | while read -r line; do
		case "$line" in
		RESULT*)
			t=$(echo "$line" | sed -n 's/.*thr=\([0-9]*\).*/\1/p')
			kps=$(echo "$line" | sed -n 's/.*keys_per_sec=\([0-9]*\).*/\1/p')
			p50=$(echo "$line" | sed -n 's/.*batch_p50_us=\([0-9.]*\).*/\1/p')
			p99=$(echo "$line" | sed -n 's/.*batch_p99_us=\([0-9.]*\).*/\1/p')
			nf=$(echo "$line" | sed -n 's/.*notfound=\([0-9]*\).*/\1/p')
			echo -e "$arm\t$rep\t$MODE\t$t\t$BATCH\t$kps\t$p50\t$p99\t$nf" >> "$F"
			;;
		esac
	done
}

for r in $(seq 1 "$REPS"); do
	# Rotate order each rep so host drift cannot favour one arm.
	if [ $(( r % 2 )) -eq 1 ]; then ORDER="base branch"; else ORDER="branch base"; fi
	for arm in $ORDER; do
		case "$arm" in
		base)   run_arm "$BASE"   base   "$r" ;;
		branch) run_arm "$BRANCH" branch "$r" ;;
		esac
	done
	echo "rep $r done: $(date -u +%H:%M:%S)"
done
touch "$OUT/cc_${MODE}.done"
echo "ALL DONE -> $F"

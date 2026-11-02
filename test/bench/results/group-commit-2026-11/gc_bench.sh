#!/bin/bash
# W7 group-commit A/B benchmark: 5.3.28 vs master, DB_TXN_SYNC, tiny txn.
# Sweeps thread counts, interleaves arms within each rep, >=5 reps.
# Two devices: NVMe XFS (/data) and gp3 EBS (/mnt/gp3data).
# Emits one TSV row per (device,threads,rep,arm) to stdout.
set -u

MLIB=/data/master-src/build/.libs
L53=/data/db-5.3.28/build_unix/.libs
CB_M=/data/cb_master
CB_5=/data/cb_5328

THREADS="${THREADS:-1 2 4 8 16 32 64 96 128}"
REPS="${REPS:-5}"
SECS="${SECS:-10}"
KEYRANGE="${KEYRANGE:-1000000}"

run() {   # run <arm> <lib> <bin> <dir>
	local arm=$1 lib=$2 bin=$3 dir=$4 t=$5
	find "$dir" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$dir"
	LD_LIBRARY_PATH="$lib" KEYRANGE="$KEYRANGE" "$bin" "$dir" "$t" "$SECS" sync \
	    2>/dev/null | grep flush_per_commit
}

echo -e "device\tthreads\trep\tarm\tops_sec\tflush_per_commit\tmaxcpf\tp50_us\tp99_us"
for dev in nvme:/data/bench_nvme gp3:/mnt/gp3data/bench_gp3; do
	devname=${dev%%:*}; base=${dev##*:}
	for t in $THREADS; do
		for r in $(seq 1 "$REPS"); do
			# rotate arm order each rep to cancel ordering bias
			if [ $((r % 2)) -eq 0 ]; then order="5328 master"; else order="master 5328"; fi
			for arm in $order; do
				if [ "$arm" = master ]; then
					line=$(run master "$MLIB" "$CB_M" "$base/m" "$t")
				else
					line=$(run 5328 "$L53" "$CB_5" "$base/f" "$t")
				fi
				ops=$(echo "$line" | grep -oE 'ops_sec=[0-9.]+' | cut -d= -f2)
				fpc=$(echo "$line" | grep -oE 'flush_per_commit=[0-9.]+' | cut -d= -f2)
				mcpf=$(echo "$line" | grep -oE 'maxcpf=[0-9]+' | cut -d= -f2)
				p50=$(echo "$line" | grep -oE 'p50_us=[0-9]+' | cut -d= -f2)
				p99=$(echo "$line" | grep -oE 'p99_us=[0-9]+' | cut -d= -f2)
				echo -e "$devname\t$t\t$r\t$arm\t${ops:-NA}\t${fpc:-NA}\t${mcpf:-NA}\t${p50:-NA}\t${p99:-NA}"
			done
		done
	done
done

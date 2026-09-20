#!/bin/sh
# P5 batch A/B: rows-per-transaction arms, alternating within each rep.
#
#   p5_batch_ab.sh <build_dir> <out.tsv> <secs> <reps> <batch-csv> <threads-csv> [mode]
#
# [mode] is passed through to the probe (nosync|notdur|...).  With mode=notdur
# the same arms run with log appends suppressed but transactions and locking
# intact, which isolates the log's share of the per-transaction cost.
#
# Arms are batch sizes.  Passing the same value twice gives the noise floor
# (base against itself).  Throughput is reported in ROWS/sec for every arm, so
# arms with different batch sizes are compared on the same unit of user work.
BUILD=${1:?build dir}
OUT=${2:?out tsv}
SECS=${3:-10}
REPS=${4:-5}
ARMS=${5:-1,4}
THREADS=${6:-1,2,4,8,16,32,64,96}
MODE=${7:-nosync}

BIN=/tmp/p5bab.$$
LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
SONAME=$(basename "$LIB" | sed 's/^lib//; s/\.so.*//')
cc -O2 -pthread /nvme/p5_log_bench.c -I"$BUILD" -L"$BUILD/.libs" \
    -l"$SONAME" -o "$BIN" || exit 1

printf 'rep\tbatch\tthreads\trows_per_sec\trec_per_row\trecords\twcount\tscount\tregion_wait\tregion_nowait\tmb_per_sec\n' > "$OUT"

r=1
while [ "$r" -le "$REPS" ]; do
	for t in $(echo "$THREADS" | tr ',' ' '); do
		for b in $(echo "$ARMS" | tr ',' ' '); do
			D=/nvme/p5bab.$$.$r.$t.$b
			mkdir -p "$D"; find "$D" -mindepth 1 -delete
			o=$(LD_LIBRARY_PATH="$BUILD/.libs" timeout 300 \
			    "$BIN" "$D" "$t" "$SECS" 100 0 "$MODE" 0 "$b" 2>/dev/null)
			ops=$(echo "$o" | sed -n 's/.*ops_per_sec=\([0-9.]*\).*/\1/p')
			if [ -z "$ops" ]; then
				echo "RUN FAILED rep=$r batch=$b t=$t" >&2
				echo "$o" >&2
				find "$D" -mindepth 1 -delete; rmdir "$D"; continue
			fi
			rpr=$(echo "$o" | sed -n 's/.*records_per_row=\([0-9.]*\).*/\1/p')
			rec=$(echo "$o" | sed -n 's/.*records=\([0-9]*\).*/\1/p')
			wc=$(echo "$o" | sed -n 's/.*[^_]wcount=\([0-9]*\).*/\1/p')
			sc=$(echo "$o" | sed -n 's/.*scount=\([0-9]*\).*/\1/p')
			rw=$(echo "$o" | sed -n 's/.*region_wait=\([0-9]*\).*/\1/p')
			rn=$(echo "$o" | sed -n 's/.*region_nowait=\([0-9]*\).*/\1/p')
			mb=$(echo "$o" | sed -n 's/.*MB_per_sec=\([0-9.]*\).*/\1/p')
			printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
			    "$r" "$b" "$t" "$ops" "$rpr" "$rec" "$wc" "$sc" \
			    "$rw" "$rn" "$mb" >> "$OUT"
			find "$D" -mindepth 1 -delete; rmdir "$D"
		done
	done
	r=$((r + 1))
done
rm -f "$BIN"
echo "wrote $OUT"

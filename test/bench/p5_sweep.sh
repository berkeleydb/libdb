#!/bin/sh
# P5 measurement driver.  Builds the probe against a given build tree and runs
# a thread sweep, optionally sweeping the log buffer size.
#
#   p5_sweep.sh <build_dir> <out.tsv> <lg_bsize> <mode> <secs> <reps> <spins> [threads...]
#
# Each rep gets a fresh environment directory (a log that has already rolled
# over behaves differently from a cold one, and we want every rep comparable).
set -e

BUILD=${1:?build dir}
OUT=${2:?out tsv}
BSIZE=${3:-0}
MODE=${4:-nosync}
SECS=${5:-10}
REPS=${6:-3}
SPINS=${7:-0}
shift 7 || true
THREADS="${*:-1 2 4 8 16 32 64 96}"

BIN=/tmp/p5bench.$$
LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
SONAME=$(basename "$LIB" | sed 's/^lib//; s/\.so.*//')

cc -O2 -pthread /nvme/p5_log_bench.c -I"$BUILD" -L"$BUILD/.libs" \
    -l"$SONAME" -o "$BIN"

printf 'rep\tthreads\tbsize\tmode\tspins\tops_per_sec\trecords\twcount\twcount_fill\tscount\tregion_wait\tregion_nowait\tmb_per_sec\twrites_per_sec\tavg_write_bytes\n' > "$OUT"

r=1
while [ "$r" -le "$REPS" ]; do
	for t in $THREADS; do
		D=/nvme/p5run.$$.$r.$t
		rm -f "$D"/* 2>/dev/null || true
		mkdir -p "$D"
		o=$(LD_LIBRARY_PATH="$BUILD/.libs" timeout 300 \
		    "$BIN" "$D" "$t" "$SECS" 100 "$BSIZE" "$MODE" "$SPINS" 2>/dev/null)
		ops=$(echo "$o" | sed -n 's/.*ops_per_sec=\([0-9.]*\).*/\1/p')
		rec=$(echo "$o" | sed -n 's/.*records=\([0-9]*\).*/\1/p')
		wc=$(echo "$o" | sed -n 's/.*[^_]wcount=\([0-9]*\).*/\1/p')
		wcf=$(echo "$o" | sed -n 's/.*wcount_fill=\([0-9]*\).*/\1/p')
		sc=$(echo "$o" | sed -n 's/.*scount=\([0-9]*\).*/\1/p')
		rw=$(echo "$o" | sed -n 's/.*region_wait=\([0-9]*\).*/\1/p')
		rn=$(echo "$o" | sed -n 's/.*region_nowait=\([0-9]*\).*/\1/p')
		mb=$(echo "$o" | sed -n 's/.*MB_per_sec=\([0-9.]*\).*/\1/p')
		wps=$(echo "$o" | sed -n 's/.*writes_per_sec=\([0-9.]*\).*/\1/p')
		aw=$(echo "$o" | sed -n 's/.*avg_write_bytes=\([0-9.]*\).*/\1/p')
		printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
		    "$r" "$t" "$BSIZE" "$MODE" "$SPINS" "$ops" "$rec" "$wc" "$wcf" \
		    "$sc" "$rw" "$rn" "$mb" "$wps" "$aw" >> "$OUT"
		find "$D" -mindepth 1 -delete
		rmdir "$D"
	done
	r=$((r + 1))
done
rm -f "$BIN"
echo "wrote $OUT"

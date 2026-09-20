#!/bin/sh
# P5 A/B driver: alternate spin-count arms WITHIN each rep, so a drift in
# machine state (page cache, thermal, neighbour noise) hits both arms equally.
# This is the same discipline P1-FIX/P4-FIX used; see test/bench/NOISE.md.
#
#   p5_ab.sh <build_dir> <out.tsv> <secs> <reps> <arms-csv> <threads-csv>
#
# <arms-csv> is a list of tas_spins values; 0 means "library default"
# (cpu_count * MUTEX_SPINS_PER_PROCESSOR, i.e. 4800 on a 96-vCPU box).
# Passing the same value twice is the noise-floor control (base against itself).

BUILD=${1:?build dir}
OUT=${2:?out tsv}
SECS=${3:-10}
REPS=${4:-5}
ARMS=${5:-0,1}
THREADS=${6:-1,2,4,8,16,32,64,96}

BIN=/tmp/p5ab.$$
LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
SONAME=$(basename "$LIB" | sed 's/^lib//; s/\.so.*//')
cc -O2 -pthread /nvme/p5_log_bench.c -I"$BUILD" -L"$BUILD/.libs" \
    -l"$SONAME" -o "$BIN"

printf 'rep\tarm\tthreads\tops_per_sec\trecords\twcount\twcount_fill\tscount\tregion_wait\tregion_nowait\tmb_per_sec\n' > "$OUT"

r=1
while [ "$r" -le "$REPS" ]; do
	for t in $(echo "$THREADS" | tr ',' ' '); do
		# Arms alternate here, innermost, so the two arms for a given
		# (rep, threads) run back-to-back on the same machine state.
		for a in $(echo "$ARMS" | tr ',' ' '); do
			D=/nvme/p5ab.$$.$r.$t.$a
			mkdir -p "$D"
			find "$D" -mindepth 1 -delete
			o=$(LD_LIBRARY_PATH="$BUILD/.libs" timeout 300 \
			    "$BIN" "$D" "$t" "$SECS" 100 0 nosync "$a" 2>/dev/null)
			ops=$(echo "$o" | sed -n 's/.*ops_per_sec=\([0-9.]*\).*/\1/p')
			if [ -z "$ops" ]; then
				echo "RUN FAILED rep=$r arm=$a t=$t" >&2
				echo "$o" >&2
				find "$D" -mindepth 1 -delete; rmdir "$D"
				continue
			fi
			rec=$(echo "$o" | sed -n 's/.*records=\([0-9]*\).*/\1/p')
			wc=$(echo "$o" | sed -n 's/.*[^_]wcount=\([0-9]*\).*/\1/p')
			wcf=$(echo "$o" | sed -n 's/.*wcount_fill=\([0-9]*\).*/\1/p')
			sc=$(echo "$o" | sed -n 's/.*scount=\([0-9]*\).*/\1/p')
			rw=$(echo "$o" | sed -n 's/.*region_wait=\([0-9]*\).*/\1/p')
			rn=$(echo "$o" | sed -n 's/.*region_nowait=\([0-9]*\).*/\1/p')
			mb=$(echo "$o" | sed -n 's/.*MB_per_sec=\([0-9.]*\).*/\1/p')
			printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
			    "$r" "$a" "$t" "$ops" "$rec" "$wc" "$wcf" "$sc" \
			    "$rw" "$rn" "$mb" >> "$OUT"
			find "$D" -mindepth 1 -delete
			rmdir "$D"
		done
	done
	r=$((r + 1))
done
rm -f "$BIN"
echo "wrote $OUT"

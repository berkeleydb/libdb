#!/bin/sh
#
# opt_c2c.sh -- prove "reads perform no shared writes" from the HARDWARE, not
# from source, and capture the before/after profiles.
#
# RFC 0007 requires the no-shared-write claim be shown with perf c2c or
# equivalent cacheline evidence.  Reading the source is not sufficient: the first
# version of this branch DID perform two shared writes on the optimistic path
# (++c_mp->put_counter and a per-MPOOLFILE stat counter) and the source read as
# if it did not.
#
# Produces, for each arm (base = DB_NO_OPTREAD, opt):
#   $OUT/<arm>.perf.txt   perf record -g, symbol self-time, for the pin's share
#   $OUT/<arm>.c2c.txt    perf c2c report, when the PMU supports it
#   $OUT/<arm>.rfo.txt    l2_rqsts.rfo_miss counters (the portable fallback)
#
# perf c2c NEEDS PEBS MEMORY EVENTS, which a virtualized instance usually does
# not expose: it fails with "memory events not supported" and produces nothing.
# Measured, not assumed -- it failed on the c7i-class box this was developed on.
# So the primary evidence here is the RFO counter, which is available:
#
#   l2_rqsts.rfo_miss  -- Read-For-Ownership requests that MISS L2.  An RFO is
#     issued when this core needs a line in a writable state; it misses L2
#     exactly when another core owns the line.  That IS the cross-core
#     write-sharing this RFC claims to remove, counted in hardware.  A reader
#     that writes nothing to a shared frame cannot generate an RFO for it.
#
# Normalize by keys/sec before comparing: the faster arm does more work per
# second, so raw counts understate the improvement.  The figure to report is
# RFO misses PER READ.
#
# Usage: ./opt_c2c.sh [-b BUILD] [-o OUTDIR] [-t THREADS] [-s SECS] [-k NKEYS]
set -e

BUILD=${BUILD:-../../build_unix}
OUT=${OUT:-/tmp/optc2c}
THREADS=32
SECS=10
NKEYS=2000000
HOME_DIR=${OPT_AB_HOME:-/tmp/optab}

while [ $# -gt 0 ]; do
	case "$1" in
	-b) BUILD=$2; shift 2;;
	-o) OUT=$2; shift 2;;
	-t) THREADS=$2; shift 2;;
	-s) SECS=$2; shift 2;;
	-k) NKEYS=$2; shift 2;;
	*) echo "usage: see header" >&2; exit 2;;
	esac
done

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
BUILD=$(CDPATH= cd -- "$BUILD" && pwd)
mkdir -p "$OUT" "$HOME_DIR"

LIBS="-lpthread"
grep -q "HAVE_IO_URING" "$BUILD/db_config.h" 2>/dev/null && LIBS="$LIBS -luring"

BIN=$OUT/bench
rm -f "$BIN"
# -fno-omit-frame-pointer so perf -g call graphs are readable.
cc -O2 -g -fno-omit-frame-pointer -w -I "$BUILD" -o "$BIN" \
    "$HERE/pin_bench.c" "$BUILD/libdb.a" $LIBS
test -x "$BIN" || { echo "driver did not build" >&2; exit 1; }

run_arm() {
	arm=$1
	case "$arm" in
	opt) extra="";;
	*)   extra="DB_NO_OPTREAD=1";;
	esac

	echo "=== $arm: perf record (self time)"
	# The same env home for both arms -- see the layout warning in opt_ab.sh.
	env $extra PIN_HOME="$HOME_DIR" PIN_TAG="$arm" \
	    PIN_CACHE_MB="${OPT_AB_CACHE_MB:-4096}" \
	    perf record -q -g --call-graph fp -F 499 -o "$OUT/$arm.data" -- \
	    "$BIN" indiv "$NKEYS" 1 3 "$SECS" "$THREADS" \
	    > "$OUT/$arm.bench.txt" 2>&1 || true
	grep -E "^(RESULT|FAIL)" "$OUT/$arm.bench.txt" || true
	perf report -i "$OUT/$arm.data" --no-children --percent-limit 0.3 \
	    --stdio > "$OUT/$arm.perf.txt" 2>&1 || true
	echo "--- top self time ($arm)"
	grep -E "^ +[0-9]+\.[0-9]+%" "$OUT/$arm.perf.txt" | head -12 || true

	echo "=== $arm: RFO misses (cross-core write sharing, in hardware)"
	env $extra PIN_HOME="$HOME_DIR" PIN_TAG="$arm-rfo" \
	    PIN_CACHE_MB="${OPT_AB_CACHE_MB:-4096}" \
	    perf stat -e l2_rqsts.rfo_miss,l2_rqsts.all_rfo,cache-misses -- \
	    "$BIN" indiv "$NKEYS" 1 3 "$SECS" "$THREADS" \
	    > "$OUT/$arm.rfo.txt" 2>&1 || true
	grep -E "^(RESULT|FAIL)|rfo_miss|all_rfo|cache-misses|seconds time" \
	    "$OUT/$arm.rfo.txt" || true

	echo "=== $arm: perf c2c (only if the PMU exposes memory events)"
	env $extra PIN_HOME="$HOME_DIR" PIN_TAG="$arm-c2c" \
	    PIN_CACHE_MB="${OPT_AB_CACHE_MB:-4096}" \
	    perf c2c record -q -o "$OUT/$arm.c2c.data" -- \
	    "$BIN" indiv "$NKEYS" 1 3 "$SECS" "$THREADS" \
	    > "$OUT/$arm.c2cbench.txt" 2>&1 || true
	if [ -f "$OUT/$arm.c2c.data" ]; then
		perf c2c report -i "$OUT/$arm.c2c.data" --stdio \
		    > "$OUT/$arm.c2c.txt" 2>&1 || true
		sed -n '1,40p' "$OUT/$arm.c2c.txt" | grep -Ei \
		    "hitm|load operations|store operations" || true
	else
		echo "  (skipped: $(head -1 "$OUT/$arm.c2cbench.txt" 2>/dev/null))"
	fi
}

# Arms alternate here too, so a drift over the capture cannot be read as an arm
# effect.
run_arm base
run_arm opt
run_arm base
run_arm opt

echo "artifacts in $OUT"

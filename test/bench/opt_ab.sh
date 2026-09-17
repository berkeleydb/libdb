#!/bin/sh
#
# opt_ab.sh -- the A/B measurement for RFC 0007 phase 1.
#
# METHODOLOGY, and why each rule is here rather than the obvious alternative:
#
#  * ONE binary, ONE env-home directory, ONE runtime switch (DB_NO_OPTREAD).
#    Giving each arm its own directory -- the tidy thing to do -- can report a
#    1.65x "win" that is pure memory layout: DB_PRIVATE throughput on this class
#    of box is bimodal in the LENGTH of the env-home path, with ~0% spread within
#    each mode, so each arm looks tight and trustworthy while the cross-arm ratio
#    is an artifact.  See the warning atop run_bench.sh.  Same reason the arms are
#    not two builds: two builds are two binaries and two layouts.
#
#  * Arms ALTERNATE WITHIN each rep (base, opt, base, opt...), never grouped, so
#    a thermal or neighbour-noise drift over the run cannot masquerade as an arm
#    effect.
#
#  * A NOISE FLOOR arm: base measured against ITSELF (tags base and base2) in the
#    same alternation.  Any base-vs-opt difference smaller than base-vs-base2 is
#    noise, and reporting it as a win would be dishonest.
#
#  * Every rep is PRINTED AS IT COMPLETES and appended to the output file.  A
#    sibling agent buffered five reps to a remote file, lost the box, and lost
#    four of them.
#
#  * Each RESULT line carries opt_tries/opt_pages/opt_invalid.  An "opt" arm
#    whose opt_pages is 0 did not run the code under test, and the report script
#    treats that as a failed run, not as a neutral result.
#
# Usage: ./opt_ab.sh [-b BUILD] [-o OUT.tsv] [-r REPS] [-s SECS] [-n "1 8 32 96"]
#                    [-k NKEYS] [-m indiv|batch] [-H HOME]
set -e

BUILD=${BUILD:-../../build_unix}
OUT=/dev/stdout
REPS=5
SECS=10
WARM=3
THREADS="1 8 32 96"
NKEYS=2000000
MODE=indiv
HOME_DIR=${OPT_AB_HOME:-/tmp/optab}

while [ $# -gt 0 ]; do
	case "$1" in
	-b) BUILD=$2; shift 2;;
	-o) OUT=$2; shift 2;;
	-r) REPS=$2; shift 2;;
	-s) SECS=$2; shift 2;;
	-n) THREADS=$2; shift 2;;
	-k) NKEYS=$2; shift 2;;
	-m) MODE=$2; shift 2;;
	-H) HOME_DIR=$2; shift 2;;
	*) echo "usage: see header" >&2; exit 2;;
	esac
done

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
BUILD=$(CDPATH= cd -- "$BUILD" && pwd)

LIBS="-lpthread"
grep -q "HAVE_IO_URING" "$BUILD/db_config.h" 2>/dev/null && LIBS="$LIBS -luring"

BIN=/tmp/opt_ab_bench.$$
rm -f "$BIN"
cc -O2 -w -I "$BUILD" -o "$BIN" "$HERE/pin_bench.c" "$BUILD/libdb.a" $LIBS
test -x "$BIN" || { echo "driver did not build" >&2; exit 1; }

mkdir -p "$HOME_DIR"

hdr() {
	echo "# opt_ab $(date -u +%FT%TZ) host=$(hostname) mode=$MODE"
	echo "# nkeys=$NKEYS secs=$SECS warm=$WARM reps=$REPS threads=$THREADS"
	echo "# home=$HOME_DIR homelen=${#HOME_DIR} build=$BUILD"
	echo "# governor=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor 2>/dev/null || echo n/a)"
	echo "# libdb=$(cd "$BUILD/.." && git log --oneline -1 2>/dev/null || echo n/a)"
	echo "# ONE binary, ONE home dir, arms differ ONLY by DB_NO_OPTREAD."
}
hdr | tee -a "$OUT"

# One arm at one thread count.  $1 = tag, $2 = threads, $3 = rep
one() {
	tag=$1; thr=$2; rep=$3
	# The home dir is REUSED (never recreated per arm) so the layout that
	# selects the bimodal mode is identical for every arm and every rep.
	case "$tag" in
	opt*)	env_extra="";;
	*)	env_extra="DB_NO_OPTREAD=1";;
	esac
	line=$(cd "$HOME_DIR" && env $env_extra PIN_HOME="$HOME_DIR" \
	    PIN_TAG="$tag" PIN_CACHE_MB="${OPT_AB_CACHE_MB:-4096}" \
	    "$BIN" "$MODE" "$NKEYS" 1 "$WARM" "$SECS" "$thr" 2>&1 |
	    grep -E "^(RESULT|FAIL)" || true)
	if [ -z "$line" ]; then
		line="FAIL tag=$tag thr=$thr no output"
	fi
	# Stream it: printed AND appended the moment it exists.
	echo "rep=$rep $line" | tee -a "$OUT"
}

rep=1
while [ "$rep" -le "$REPS" ]; do
	for thr in $THREADS; do
		# Alternating within the rep, with the noise-floor arm inline.
		one base "$thr" "$rep"
		one opt "$thr" "$rep"
		one base2 "$thr" "$rep"
		one opt2 "$thr" "$rep"
	done
	rep=$((rep + 1))
done

rm -f "$BIN"
echo "# done $(date -u +%FT%TZ)" | tee -a "$OUT"

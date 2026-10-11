#!/bin/sh
# RFC 0015 S7 regression: db_stat must describe the latch it actually reads.
#
#  (b) c_locker ("locker allocations that required waiting") must be the SUM
#      over all 64 locker stripes.  After P12 it read stripe 0 alone
#      (lock_stat.c:168), a 1/64 sample quoted as the whole.
#  (a) -c/-t/-l "region locks that required waiting" are ONE mutex,
#      renv->mtx_regenv; each line must say so.
#
# Single-threaded and exact: it compares the reported counts with the sum of
# the stripe mutexes read independently, so no timing or load is involved.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
# $1, else $BUILD (what run_all.sh sets), else the in-tree build_unix.
# Honouring $BUILD matters: with only $1, run_all.sh -- which passes no
# argument -- silently tested a stale ../../build_unix instead of the build
# under test, or failed outright with a sibling build dir.
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/s7_stat_truth.c

LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a ; do
	[ -f "$cand" ] && { LIB="$cand" ; break ; }
done
if [ -z "$LIB" ] ; then
	for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib ; do
		[ -f "$cand" ] && { LIB="$cand" ; break ; }
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "run_s7_stat_truth.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/s7_stat_truth
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$HERE/../../src" \
    "$SRC" "$LIB" -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_s7_stat_truth.sh: FAIL compile" ; exit 1 ; }
# A stale binary would be a false pass.
test -x "$BIN" || { echo "run_s7_stat_truth.sh: FAIL no binary produced" ; exit 1 ; }

# A fresh home: the counters compared are cumulative over the region's life.
work=$BUILD/TESTDIR_s7
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"
out=$(cd "$BUILD" && "$BIN" 2>&1)
rc=$?
echo "$out" | sed 's/^/  /'

# rc is checked as well as the verdict: a signal death produces no verdict line.
if [ "$rc" -ge 128 ] ; then
	echo "run_s7_stat_truth.sh: FAIL died on signal $((rc - 128))"
	exit 1
fi

v=$(echo "$out" | grep '^VERDICT s7_stat_truth' | head -1)
[ -n "$v" ] || { echo "run_s7_stat_truth.sh: FAIL no VERDICT line (exit $rc)" ; exit 1 ; }
case "$v" in
*PASS*)	echo "run_s7_stat_truth.sh: PASS" ; exit 0 ;;
*SKIP*)	echo "run_s7_stat_truth.sh: SKIP $v" ; exit 0 ;;
esac
echo "run_s7_stat_truth.sh: FAIL"
exit 1

#!/bin/sh
# RFC 0015 S3 regression: the two per-op mpool counters.
#
#  (b) st_hash_examined was never incremented -- __memp_fget folded it into
#      st_hash_searches (mp_fget.c:492).  After N chain-walking lookups it
#      must be > 0; pre-fix it is 0 for any workload.
#  (a) put_counter (the allocator's give-up signal) was bumped on EVERY fput;
#      S3 bumps it only on the put that releases a buffer.  After real
#      get/put work the summed counter must still be > 0.
#
# Single-threaded and deterministic: no timing or load is involved.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
# $1, else $BUILD (what run_all.sh sets), else the in-tree build_unix.
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/s3_mpool_counters.c

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
[ -n "$LIB" ] || { echo "run_s3_mpool_counters.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/s3_mpool_counters
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$HERE/../../src" \
    "$SRC" "$LIB" -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_s3_mpool_counters.sh: FAIL compile" ; exit 1 ; }
test -x "$BIN" || { echo "run_s3_mpool_counters.sh: FAIL no binary produced" ; exit 1 ; }

# A fresh home: the counters compared are cumulative over the region's life.
work=$BUILD/TESTDIR_s3
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"
out=$(cd "$BUILD" && "$BIN" 2>&1)
rc=$?
echo "$out" | sed 's/^/  /'

if [ "$rc" -ge 128 ] ; then
	echo "run_s3_mpool_counters.sh: FAIL died on signal $((rc - 128))"
	exit 1
fi

v=$(echo "$out" | grep '^VERDICT s3_mpool_counters' | head -1)
[ -n "$v" ] || { echo "run_s3_mpool_counters.sh: FAIL no VERDICT line (exit $rc)" ; exit 1 ; }
case "$v" in
*PASS*)	echo "run_s3_mpool_counters.sh: PASS" ; exit 0 ;;
*SKIP*)	echo "run_s3_mpool_counters.sh: SKIP $v" ; exit 0 ;;
esac
echo "run_s3_mpool_counters.sh: FAIL"
exit 1

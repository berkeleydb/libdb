#!/bin/sh -
#
# $Id$
#
# run_evict_bound.sh --
#	Regression gate for R3 (XREL-2026-10.md) / review item O-MP1 -- the
#	COOL-first CLOCK eviction sweep (RFC 0002 §11) examined 80-520 hash
#	buckets per allocation under bulk insert, vs 1-2 in 5.3.28, because it
#	re-proved "the whole cache is HOT" on every allocation.
#
#	Fills a small cache with fresh dirty btree pages and asserts the
#	buckets-examined-per-allocation ratio (st_alloc_buckets / st_alloc, the
#	ratio db_stat -m reports) stays under a bound.  Fails on master (~460),
#	passes with the bounded sweep (rfc/0002/R3-FIX.md).
#
# Exits non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}
# Anchor to THIS script's directory, not the caller's cwd, so ../test/db/X.c
# resolves with a build dir that is a sibling of the source tree.
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/evict_bound.c}
TIMEOUT=${TIMEOUT:-300}

# Prefer the STATIC library (see run_qam_readpath_bound.sh for the macOS
# install-name reason); fall back to the shared library.
LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-5.3.a "$BUILD"/.libs/libdb-*.a; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
if [ -z "$LIB" ]; then
	for cand in "$BUILD"/.libs/libdb-5.3.so "$BUILD"/.libs/libdb-5.3.dylib \
	    "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib; do
		if [ -f "$cand" ]; then LIB="$cand"; break; fi
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "FAIL: no libdb library (static or shared) found under $BUILD"; exit 1; }

# A static libdb needs its transitive deps named explicitly.
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling evict_bound against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/evict_bound"

if command -v timeout >/dev/null 2>&1; then
	TIMEOUT_CMD="timeout"
elif command -v gtimeout >/dev/null 2>&1; then
	TIMEOUT_CMD="gtimeout"
else
	TIMEOUT_CMD=""
fi
run_with_timeout() {
	if [ -n "$TIMEOUT_CMD" ]; then
		"$TIMEOUT_CMD" "$@"
	else
		shift	# drop the seconds argument
		"$@"
	fi
}

echo "Running evict_bound (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/evict_bound"; then
	echo "run_evict_bound.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_evict_bound.sh: FAIL (rc=$rc)"
	exit $rc
fi

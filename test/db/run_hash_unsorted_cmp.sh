#!/bin/sh -
#
# $Id$
#
# run_hash_unsorted_cmp.sh --
#	Build and run hash_unsorted_cmp.c, the regression test for
#	https://github.com/berkeleydb/libdb/issues/139 --
#	__ham_getindex_unsorted() (src/hash/hash_page.c) called the
#	application's DB->set_h_compare comparator for an inline key on a
#	legacy P_HASH_UNSORTED page but dropped its result, so an equal key
#	was reported as absent: DB->get returned DB_NOTFOUND for a key that
#	was present and DB->put(DB_NOOVERWRITE) returned success and stored a
#	second record with identical key bytes in a no-duplicates database.
#
#	The driver builds its own legacy fixture (no old library, no committed
#	binary blob): it creates a current-format Hash database, then rewrites
#	each bucket page's PAGE.type byte from P_HASH to P_HASH_UNSORTED and
#	the metadata version back to the 4.5.20 hash version 8 -- the same
#	technique test/db/run_upgrade.sh uses for its old-format fixtures.
#	Each check runs twice, with and without the comparator, so a failure
#	is attributable to the comparator path rather than to the fixture.
#
# Usage (from build_unix):
#	sh ../test/db/run_hash_unsorted_cmp.sh
#
# Exits non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/db/hash_unsorted_cmp.c}
HOME_DIR=${HOME_DIR:-HASH_UNSORTED_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

# Shared-library suffix is platform-dependent: .so on Linux, .dylib on macOS.
LIB=""
for cand in "$BUILD"/.libs/libdb-5.3.so "$BUILD"/.libs/libdb-5.3.dylib \
    "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
[ -n "$LIB" ] || { echo "FAIL: libdb shared library (.so/.dylib) not found in $BUILD/.libs"; exit 1; }

echo "Compiling hash_unsorted_cmp against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/hash_unsorted_cmp"

rm -f "$HOME_DIR"/*.db 2>/dev/null || true
mkdir -p "$HOME_DIR"

echo "Running hash_unsorted_cmp (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/hash_unsorted_cmp"; then
	echo "run_hash_unsorted_cmp.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_hash_unsorted_cmp.sh: FAIL (rc=$rc)"
	exit $rc
fi

#!/bin/sh -
#
# $Id$
#
# run_xa_direct.sh --
#	Build and run the Tuxedo-free XA driver (xa_direct.c), which exercises
#	Berkeley DB's db_xa_switch (src/xa/xa.c + xa_map.c) and the internal
#	two-phase-commit / recovery path that XA drives.  Unlike chk.xa this
#	needs NO Tuxedo install.
#
# Usage (from build_unix):
#	sh ../test/xa/run_xa_direct.sh
#
# It compiles xa_direct against the just-built libdb in ./.libs, runs it in a
# guaranteed-clean home under a timeout, and reports PASS/FAIL.  Exits non-zero
# on failure or hang.

set -e

BUILD=${BUILD:-.}			# build_unix dir (cwd by default)
SRC=${SRC:-../test/xa/xa_direct.c}
HOME_DIR=${HOME_DIR:-XA_TESTDIR}
TIMEOUT=${TIMEOUT:-90}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	# Fall back to the versioned .so actually present.
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

echo "Compiling xa_direct against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" -I../src/dbinc "$SRC" "$LIB" \
    -luring -lpthread -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/xa_direct"

# Guaranteed-clean home: remove env/log/db artifacts (not rm -rf).
rm -f "$HOME_DIR"/__db.* "$HOME_DIR"/log.* "$HOME_DIR"/*.db \
    "$HOME_DIR"/DB_CONFIG 2>/dev/null || true
mkdir -p "$HOME_DIR"

echo "Running xa_direct (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/xa_direct"; then
	echo "run_xa_direct.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_xa_direct.sh: FAIL (rc=$rc)"
	exit $rc
fi

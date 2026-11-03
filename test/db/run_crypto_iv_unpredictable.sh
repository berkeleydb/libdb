#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_crypto_iv_unpredictable.sh --
#	Build and run crypto_iv_unpredictable.c: encryption IVs must not be
#	predictable from earlier IVs.  It runs the MT19937 state-recovery attack
#	against __db_generate_iv and requires it to predict nothing.  Needs a
#	build with cryptography (the default).
#
#	Usage (from the build directory):  sh ../test/db/run_crypto_iv_unpredictable.sh
#	Env: BUILD (default .), TIMEOUT (default 120).

set -e
BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/crypto_iv_unpredictable.c}
HOME_DIR=${HOME_DIR:-$BUILD/CRYPTO_IV_TESTDIR}
TIMEOUT=${TIMEOUT:-120}
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
if ! grep -q '^#define HAVE_CRYPTO 1' "$BUILD/db_config.h" 2>/dev/null; then
	echo "run_crypto_iv_unpredictable.sh: SKIP not a cryptography build"
	exit 0
fi
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling crypto_iv_unpredictable against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$_HERE/../../src" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BUILD/crypto_iv_unpredictable"
mkdir -p "$HOME_DIR"

run_with_timeout() {
	_t=$1; shift
	if command -v timeout >/dev/null 2>&1; then
		timeout "$_t" "$@"
	else
		"$@"
	fi
}
echo "Running crypto_iv_unpredictable (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/crypto_iv_unpredictable" "$HOME_DIR"; then
	echo "run_crypto_iv_unpredictable.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_crypto_iv_unpredictable.sh: FAIL (rc=$rc)"
	exit $rc
fi

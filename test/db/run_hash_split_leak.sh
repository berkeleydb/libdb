#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_hash_split_leak.sh --
#	Build and run hash_split_leak.c: a split or a bucket merge of a legacy
#	P_HASH_UNSORTED page must release its page pins and bucket lock when
#	sorting the page fails.  In an --enable-faultinject build it sweeps an
#	allocation failure across the whole insert workload; otherwise it runs
#	the workload once as a sanity check.
#
#	Usage (from the build directory):  sh ../test/db/run_hash_split_leak.sh
#	Env: BUILD (default .), TIMEOUT (default 1800), HSL_MAXK.

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/hash_split_leak.c}
HOME_DIR=${HOME_DIR:-$BUILD/HASH_SPLIT_LEAK_TESTDIR}
TIMEOUT=${TIMEOUT:-1800}
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
FI=""
if grep -q '^#define HAVE_FAULT_INJECT 1' "$BUILD/db_config.h" 2>/dev/null; then
	FI="-DHAVE_FI -I$_HERE/../faultinject"
fi
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling hash_split_leak against $LIB${FI:+ (fault injection)}"
"${CC:-cc}" -g -O1 ${CFLAGS:-} $FI -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BUILD/hash_split_leak"
mkdir -p "$HOME_DIR"

run_with_timeout() {
	_t=$1; shift
	if command -v timeout >/dev/null 2>&1; then
		timeout "$_t" "$@"
	else
		"$@"
	fi
}
echo "Running hash_split_leak (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/hash_split_leak" "$HOME_DIR"; then
	echo "run_hash_split_leak.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_hash_split_leak.sh: FAIL (rc=$rc)"
	exit $rc
fi

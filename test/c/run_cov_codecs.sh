#!/bin/sh -
#
# $Id$
#
# run_cov_codecs.sh --
#	Build and run cov_codecs.c: exhaustive boundary coverage of libdb's
#	self-contained codecs -- the compressed-integer (varint) codec in
#	src/common/db_compint.c and the string-to-number parsers in
#	src/common/db_getlong.c.
#
#	db_compint sits at 21% line / 25% branch in report #3 with
#	__db_decompress_int NEVER CALLED, because btree compression only
#	marshals 32-bit lengths (so the 64-bit decoder and the 4..9-byte size
#	classes are unreachable from any Tcl workload) and the property-based
#	tier that does cover them (test/pbt/pbt_compint.c) needs the `hegel`
#	server binary and compiles in STUB mode -- it links, prints SKIP and
#	executes nothing -- on a machine without hegel.  This driver walks the
#	size-class boundaries directly, with no external dependency.
#
#	Same shape as test/xa/run_xa_direct.sh and test/os/run_os_aio.sh:
#	compile against the just-built libdb in ./.libs, run in a clean home
#	under a hard timeout, report PASS/FAIL, exit non-zero on failure.
#
# Usage (from build_unix):
#	sh ../test/c/run_cov_codecs.sh

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/c/cov_codecs.c}
TIMEOUT=${TIMEOUT:-120}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

EXTRA_LIBS="-lpthread"
# liburing is linked into the .so on Linux builds that found it.
if echo 'int main(){return 0;}' > /tmp/_covcodecs_probe.c && \
    gcc /tmp/_covcodecs_probe.c -luring -o /tmp/_covcodecs_probe 2>/dev/null; then
	EXTRA_LIBS="$EXTRA_LIBS -luring"
fi
rm -f /tmp/_covcodecs_probe.c /tmp/_covcodecs_probe 2>/dev/null || true

echo "Compiling cov_codecs against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" -I../src -I../src/dbinc "$SRC" "$LIB" \
    $EXTRA_LIBS -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/cov_codecs"

# No environment and no files: these are pure functions.

echo "Running cov_codecs (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/cov_codecs"; then
	echo "run_cov_codecs.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_cov_codecs.sh: FAIL (rc=$rc)"
	exit $rc
fi

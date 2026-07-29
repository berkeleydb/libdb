#!/bin/sh -
#
# $Id$
#
# run_os_aio.sh --
#	Build and run os_aio_direct.c, which exercises the os_aio async-I/O
#	abstraction (src/os/os_aio.c) and its backends
#	(src/os/os_aio_pool.c, os_aio_posix.c, os_aio_uring.c) plus the
#	DB_ENV os-method setters (src/common/os_method.c).
#
#	The buffer pool reaches os_aio ONLY via DB_ENV->set_flags(DB_MPOOL_AIO)
#	and then picks a SINGLE backend at runtime (io_uring is probed first on
#	Linux), so a normal Tcl workload can never light up the pool + posix
#	backends on a box that also has io_uring.  This driver links the
#	internal libdb symbols and drives EACH configured backend directly.
#
# Usage (from build_unix):
#	sh ../test/os/run_os_aio.sh
#
# It compiles os_aio_direct against the just-built libdb in ./.libs, runs it
# in a guaranteed-clean home under a timeout, and reports PASS/FAIL.  Exits
# non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}			# build_unix dir (cwd by default)
SRC=${SRC:-../test/os/os_aio_direct.c}
HOME_DIR=${HOME_DIR:-OSAIO_TESTDIR}
TIMEOUT=${TIMEOUT:-90}

LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

# liburing/librt are pulled in transitively by the .so, but link them
# explicitly so the direct __os_aio_uring_init / posix aio symbols resolve
# regardless of how the .so was linked.
EXTRA_LIBS="-lpthread"
echo "int main(){return 0;}" > /tmp/_osaio_probe.c 2>/dev/null || true
if gcc /tmp/_osaio_probe.c -luring -o /tmp/_osaio_probe 2>/dev/null; then
	EXTRA_LIBS="$EXTRA_LIBS -luring"
fi
if gcc /tmp/_osaio_probe.c -lrt -o /tmp/_osaio_probe 2>/dev/null; then
	EXTRA_LIBS="$EXTRA_LIBS -lrt"
fi
rm -f /tmp/_osaio_probe.c /tmp/_osaio_probe 2>/dev/null || true

echo "Compiling os_aio_direct against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" -I../src -I../src/dbinc "$SRC" "$LIB" \
    $EXTRA_LIBS -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/os_aio_direct"

# Guaranteed-clean home (not rm -rf).
rm -f "$HOME_DIR"/__db.* "$HOME_DIR"/log.* "$HOME_DIR"/*.dat \
    "$HOME_DIR"/DB_CONFIG 2>/dev/null || true
mkdir -p "$HOME_DIR"

echo "Running os_aio_direct (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/os_aio_direct"; then
	echo "run_os_aio.sh: PASS"
	rm -f "$HOME_DIR"/__db.* "$HOME_DIR"/log.* "$HOME_DIR"/*.dat "$HOME_DIR"/*.db 2>/dev/null || true
	exit 0
else
	rc=$?
	echo "run_os_aio.sh: FAIL (rc=$rc)"
	exit $rc
fi

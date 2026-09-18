#!/bin/sh -
#
# $Id$
#
# run_cov_oom_paths.sh --
#	Build and run cov_oom_paths.c: a COVERAGE-oriented OOM error-path
#	sweep.  test/faultinject/fi_sweep.c is the bug-finding sweep; its
#	children _exit() (correct for a watchdogged classifier, but it skips
#	gcov's atexit flush, so its 947 failure points contribute ZERO
#	measured coverage).  This driver sweeps the same seam with a breadth-
#	first workload and calls __gcov_dump() in each child before _exit, so
#	the OOM error-return branches -- 6,016 of report #3's 38,998 missing
#	branches sit on `if ((ret = f()) != 0)` lines -- are measured.
#
#	Requires --enable-faultinject; without it the driver prints SKIP and
#	exits 0, so it is safe to run unconditionally.
#
#	Same shape as test/xa/run_xa_direct.sh and test/os/run_os_aio.sh:
#	compile against the just-built libdb in ./.libs, run in a clean home
#	under a hard timeout, report PASS/FAIL, exit non-zero on failure.
#
# Usage (from build_unix):
#	sh ../test/c/run_cov_oom_paths.sh

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/c/cov_oom_paths.c}
TIMEOUT=${TIMEOUT:-2400}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

EXTRA_LIBS="-lpthread"
# liburing is linked into the .so on Linux builds that found it.
if echo 'int main(){return 0;}' > /tmp/_covoom_probe.c && \
    gcc /tmp/_covoom_probe.c -luring -o /tmp/_covoom_probe 2>/dev/null; then
	EXTRA_LIBS="$EXTRA_LIBS -luring"
fi
rm -f /tmp/_covoom_probe.c /tmp/_covoom_probe 2>/dev/null || true

echo "Compiling cov_oom_paths against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" -I../src -I../src/dbinc -I../test/faultinject "$SRC" "$LIB" \
    $EXTRA_LIBS -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/cov_oom_paths"

# Guaranteed-clean homes (no rm -rf).
for d in COVOOM_TESTDIR; do
	rm -f "$d"/__db.* "$d"/__dbq.* "$d"/log.* "$d"/*.db "$d"/*.pag "$d"/DB_CONFIG 2>/dev/null || true
	mkdir -p "$d"
done

echo "Running cov_oom_paths (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/cov_oom_paths"; then
	echo "run_cov_oom_paths.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_cov_oom_paths.sh: FAIL (rc=$rc)"
	exit $rc
fi

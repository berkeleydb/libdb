#!/bin/sh -
#
# $Id$
#
# run_cov_api_surface.sh --
#	Build and run cov_api_surface.c, the direct driver for the DB_ENV / DB /
#	DBC / DB_TXN / DB_MPOOLFILE *getter* + callback-setter surface and the
#	argument-validation branches of the matching setters (the ~60
#	never-called getters in db_method.c / env_method.c / mp_fmethod.c /
#	db_cds.c that the Tcl bindings never reach).
#
#	Same shape as test/xa/run_xa_direct.sh and test/os/run_os_aio.sh:
#	compile against the just-built libdb in ./.libs, run in a clean home
#	under a hard timeout, report PASS/FAIL, exit non-zero on failure.
#
# Usage (from build_unix):
#	sh ../test/c/run_cov_api_surface.sh

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/c/cov_api_surface.c}
TIMEOUT=${TIMEOUT:-180}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

EXTRA_LIBS="-lpthread"
# liburing is linked into the .so on Linux builds that found it.
if echo 'int main(){return 0;}' > /tmp/_covapi_surface_probe.c && \
    gcc /tmp/_covapi_surface_probe.c -luring -o /tmp/_covapi_surface_probe 2>/dev/null; then
	EXTRA_LIBS="$EXTRA_LIBS -luring"
fi
rm -f /tmp/_covapi_surface_probe.c /tmp/_covapi_surface_probe 2>/dev/null || true

echo "Compiling cov_api_surface against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" -I../src -I../src/dbinc "$SRC" "$LIB" \
    $EXTRA_LIBS -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/cov_api_surface"

# Guaranteed-clean homes (no rm -rf).
for d in COVAPI_TESTDIR COVAPI_TESTDIR_notxn COVAPI_TESTDIR_cds; do
	rm -f "$d"/__db.* "$d"/__dbq.* "$d"/log.* "$d"/*.db "$d"/*.dat "$d"/DB_CONFIG 2>/dev/null || true
	mkdir -p "$d"
done

echo "Running cov_api_surface (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/cov_api_surface"; then
	echo "run_cov_api_surface.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_cov_api_surface.sh: FAIL (rc=$rc)"
	exit $rc
fi

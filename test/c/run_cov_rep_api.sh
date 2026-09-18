#!/bin/sh -
#
# $Id$
#
# run_cov_rep_api.sh --
#	Build and run cov_rep_api.c, the direct driver for the replication +
#	replication-manager CONFIGURATION and QUERY surface (the rep_* /
#	repmgr_* DB_ENV methods, the DB_SITE handle methods, txn_applied).
#
#	Same shape as test/xa/run_xa_direct.sh and test/os/run_os_aio.sh:
#	compile against the just-built libdb in ./.libs, run in a clean home
#	under a hard timeout, report PASS/FAIL, exit non-zero on failure.
#
# Usage (from build_unix):
#	sh ../test/c/run_cov_rep_api.sh

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/c/cov_rep_api.c}
TIMEOUT=${TIMEOUT:-180}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

EXTRA_LIBS="-lpthread"
# liburing is linked into the .so on Linux builds that found it.
if echo 'int main(){return 0;}' > /tmp/_covrep_probe.c && \
    gcc /tmp/_covrep_probe.c -luring -o /tmp/_covrep_probe 2>/dev/null; then
	EXTRA_LIBS="$EXTRA_LIBS -luring"
fi
rm -f /tmp/_covrep_probe.c /tmp/_covrep_probe 2>/dev/null || true

echo "Compiling cov_rep_api against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" -I../src -I../src/dbinc "$SRC" "$LIB" \
    $EXTRA_LIBS -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/cov_rep_api"

# Guaranteed-clean homes (no rm -rf).
for d in COVREP_TESTDIR_base COVREP_TESTDIR_mgr COVREP_TESTDIR_norep; do
	rm -f "$d"/__db.* "$d"/__dbq.* "$d"/log.* "$d"/*.db "$d"/DB_CONFIG 2>/dev/null || true
	mkdir -p "$d"
done

echo "Running cov_rep_api (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/cov_rep_api"; then
	echo "run_cov_rep_api.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_cov_rep_api.sh: FAIL (rc=$rc)"
	exit $rc
fi

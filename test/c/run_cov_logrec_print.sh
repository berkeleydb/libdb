#!/bin/sh -
#
# $Id$
#
# run_cov_logrec_print.sh --
#	Build and run cov_logrec_print.c: generate a log containing as many
#	DISTINCT log record types as one process can produce, then walk the
#	whole log through db_printlog so the generated per-record printers
#	(src/<sub>/<sub>_autop.c, ~39 never-called functions) all execute.
#
#	Same shape as test/xa/run_xa_direct.sh and test/os/run_os_aio.sh:
#	compile against the just-built libdb in ./.libs, run in a clean home
#	under a hard timeout, report PASS/FAIL, exit non-zero on failure.
#
# Usage (from build_unix):
#	sh ../test/c/run_cov_logrec_print.sh

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/c/cov_logrec_print.c}
TIMEOUT=${TIMEOUT:-300}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

EXTRA_LIBS="-lpthread"
# liburing is linked into the .so on Linux builds that found it.
if echo 'int main(){return 0;}' > /tmp/_covlogrec_print_probe.c && \
    gcc /tmp/_covlogrec_print_probe.c -luring -o /tmp/_covlogrec_print_probe 2>/dev/null; then
	EXTRA_LIBS="$EXTRA_LIBS -luring"
fi
rm -f /tmp/_covlogrec_print_probe.c /tmp/_covlogrec_print_probe 2>/dev/null || true

echo "Compiling cov_logrec_print against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" -I../src -I../src/dbinc "$SRC" "$LIB" \
    $EXTRA_LIBS -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/cov_logrec_print"

# Guaranteed-clean homes (no rm -rf).
for d in COVLOGREC_TESTDIR; do
	rm -f "$d"/__db.* "$d"/__dbq.* "$d"/log.* "$d"/*.db "$d"/*.dat "$d"/DB_CONFIG 2>/dev/null || true
	mkdir -p "$d"
done

echo "Running cov_logrec_print (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/cov_logrec_print"; then
	echo "run_cov_logrec_print.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_cov_logrec_print.sh: FAIL (rc=$rc)"
	exit $rc
fi

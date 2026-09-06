#!/bin/sh
# test/soak/run.sh -- build and run the Tier B2 resource-accounting soak.
#
# Runs thousands of SEQUENTIAL transactions in ONE long-lived environment and
# asserts that region resources (mutex slots, lockers, locks, objects, txn
# details, dirty pages) return to a steady state instead of growing with the
# transaction count.  Counts come from the public stat APIs only
# (DB_ENV->mutex_stat / lock_stat / txn_stat / memp_stat).
#
# A non-zero exit means a workload disagreed with its recorded expectation:
# either a control workload leaked, or a known-leaking workload stayed flat
# (the issue got fixed -- clear expect_leak in the table).
#
# The growth curve is printed for every workload, so a CI log is enough to
# diagnose a regression without re-running locally.
#
# Usage:
#   ./run.sh                     # build + run every workload, default N
#   ./run.sh build               # build only
#   ./run.sh WORKLOAD ...        # build + run the named workloads
#   ./run.sh --list              # list workload names
#
# Env:
#   CC             compiler (default: cc)
#   LIBDB_BUILD    path to a built build_unix (default: ../../build_unix)
#   SOAK_N         transactions per workload (default 2000)
#   SOAK_TIMEOUT   seconds for the whole run (default 900)
#   SOAK_SAN       1 => build with ASan (default 0)
#
# Run from test/soak/ inside a `nix develop` shell.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$HERE"

CC=${CC:-cc}
LIBDB_BUILD=${LIBDB_BUILD:-"$HERE/../../build_unix"}
SOAK_N=${SOAK_N:-2000}
SOAK_TIMEOUT=${SOAK_TIMEOUT:-900}
SOAK_SAN=${SOAK_SAN:-0}
OUT="$HERE/build"
LIBDBA="$LIBDB_BUILD/libdb.a"

if [ -f "$LIBDB_BUILD/Makefile" ]; then
	LDLIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$LIBDB_BUILD/Makefile" | head -1)
fi
LDLIBS="${LDLIBS:--lpthread} -ldl -lpthread"

CFLAGS="-g -O1 -Wall -Wextra -Wno-unused-parameter -I$LIBDB_BUILD -I$HERE"
[ "$SOAK_SAN" = "1" ] && CFLAGS="$CFLAGS -fsanitize=address"

[ -f "$LIBDBA" ] || {
	echo "error: libdb.a not found at $LIBDBA -- build libdb first:" >&2
	echo "    (cd $LIBDB_BUILD && ../dist/configure --enable-debug && make -j4)" >&2
	exit 2
}

mkdir -p "$OUT"
# shellcheck disable=SC2086
$CC $CFLAGS "$HERE/test_soak_resources.c" "$LIBDBA" $LDLIBS \
	-o "$OUT/test_soak_resources"
echo "built $OUT/test_soak_resources"

[ "${1:-}" = "build" ] && exit 0

cd "$OUT"
if [ "${1:-}" = "--list" ]; then
	exec ./test_soak_resources --list
fi
exec timeout "$SOAK_TIMEOUT" ./test_soak_resources -n "$SOAK_N" "$@"

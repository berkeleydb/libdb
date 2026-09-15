#!/bin/sh
# test/isolation/run.sh -- build and run the Tier B1 isolation/anomaly checker.
#
# Builds test_iso_anomaly against an existing libdb build and runs it under a
# timeout.  The checker's verdict is computed (enumerate serial orders), so a
# non-zero exit means an outcome disagreed with the recorded expectation --
# either a new serializability violation, or a known-broken scenario that
# started passing (i.e. the referenced issue got fixed and the table needs
# updating).
#
# Usage:
#   ./run.sh                 # build + run every scenario
#   ./run.sh build           # build only
#   ./run.sh SCENARIO ...    # build + run the named scenarios
#   ./run.sh --list          # list scenario names
#
# Env:
#   CC            compiler (default: cc)
#   LIBDB_BUILD   path to a built build_unix (default: ../../build_unix)
#   ISO_TIMEOUT   seconds for the whole run (default: 300)
#   ISO_SAN       1 => also build with ASan/UBSan (default 0)
#   ISO_LEVEL     snapshot | serializable | both (default: both)
#                 both runs every scenario under plain SI then SSI, proving
#                 the anomalies are VISIBLE under DB_TXN_SNAPSHOT and PREVENTED
#                 under DB_TXN_SERIALIZABLE.
#   ISO_SSI_GATES 1 => also run the two SSI mechanism gates (default 1):
#                 test_ssi_gc_pressure  -- marker GC must never drop a SIREAD
#                     marker early (a MISSED conflict, not a leak).  Run at
#                     both levels; under plain SI the skew MUST appear, which
#                     is what proves the serializable run is not vacuous.
#                 test_ssi_crash_pivot  -- a pivot must not survive a crash
#                     inside its commit window.  Needs a DIAGNOSTIC build
#                     (the crash hook is #ifdef DIAGNOSTIC); skipped with a
#                     notice otherwise.
#
# Run from test/isolation/ inside a `nix develop` shell.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$HERE"

CC=${CC:-cc}
LIBDB_BUILD=${LIBDB_BUILD:-"$HERE/../../build_unix"}
ISO_TIMEOUT=${ISO_TIMEOUT:-300}
ISO_SAN=${ISO_SAN:-0}
OUT="$HERE/build"
LIBDBA="$LIBDB_BUILD/libdb.a"

if [ -f "$LIBDB_BUILD/Makefile" ]; then
	LDLIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$LIBDB_BUILD/Makefile" | head -1)
fi
LDLIBS="${LDLIBS:--lpthread} -ldl -lpthread"

CFLAGS="-g -O1 -Wall -Wextra -Wno-unused-parameter -I$LIBDB_BUILD -I$HERE"
[ "$ISO_SAN" = "1" ] && CFLAGS="$CFLAGS -fsanitize=address"

[ -f "$LIBDBA" ] || {
	echo "error: libdb.a not found at $LIBDBA -- build libdb first:" >&2
	echo "    (cd $LIBDB_BUILD && ../dist/configure --enable-debug && make -j4)" >&2
	exit 2
}

ISO_SSI_GATES=${ISO_SSI_GATES:-1}

mkdir -p "$OUT"
for t in test_iso_anomaly test_ssi_gc_pressure test_ssi_crash_pivot; do
	# shellcheck disable=SC2086
	$CC $CFLAGS "$HERE/$t.c" "$LIBDBA" $LDLIBS -o "$OUT/$t"
	echo "built $OUT/$t"
done

[ "${1:-}" = "build" ] && exit 0

cd "$OUT"

# Does the library have the DIAGNOSTIC-only crash hook?  test_ssi_crash_pivot
# needs it to reach the in-commit kill points; without it the sweep would run
# but never actually crash, which the driver itself reports as a failure.  So
# check up front and skip with a notice instead.
have_diagnostic=0
if grep -q '^#define[[:space:]]*DIAGNOSTIC' "$LIBDB_BUILD/db_config.h" \
    2>/dev/null; then
	have_diagnostic=1
fi

# run_ssi_gates LEVEL -- the two SSI mechanism gates.  Returns non-zero if
# either failed.  The GC gate runs at the caller's level; the crash gate is
# level-independent (it hard-codes DB_TXN_SERIALIZABLE, since a pivot only
# exists under SSI) so it runs once, from the serializable pass.
run_ssi_gates() {
	_lvl=$1
	_rc=0
	[ "$ISO_SSI_GATES" = "1" ] || return 0

	echo "--- SSI gate: marker GC must not drop a marker early"\
	    "(ISO_LEVEL=$_lvl)"
	mkdir -p "gc-$_lvl"
	( cd "gc-$_lvl" && ISO_LEVEL="$_lvl" \
	    timeout "$ISO_TIMEOUT" ../test_ssi_gc_pressure ) || _rc=$?

	if [ "$_lvl" = "serializable" ]; then
		echo "--- SSI gate: a pivot must not survive a crash in its"\
		    "commit window"
		if [ "$have_diagnostic" = "1" ]; then
			mkdir -p crash
			( cd crash &&
			    timeout "$ISO_TIMEOUT" ../test_ssi_crash_pivot ) ||
			    _rc=$?
		else
			echo "    SKIP: library was not built with"\
			    "--enable-diagnostic, so the in-commit crash"\
			    "points are unreachable"
		fi
	fi
	return $_rc
}

# Choose the isolation level(s) to exercise.  Default: both, so a single run
# demonstrates the SI-anomaly-visible vs SSI-prevented contract.
LEVELS=${ISO_LEVEL:-both}
if [ "$LEVELS" = "both" ]; then
	rc=0
	echo "=== ISO_LEVEL=snapshot (plain SI: anomalies expected) ==="
	ISO_LEVEL=snapshot timeout "$ISO_TIMEOUT" ./test_iso_anomaly "$@" || rc=$?
	run_ssi_gates snapshot || rc=$?
	echo "=== ISO_LEVEL=serializable (SSI: anomalies prevented) ==="
	ISO_LEVEL=serializable timeout "$ISO_TIMEOUT" ./test_iso_anomaly "$@" || rc=$?
	run_ssi_gates serializable || rc=$?
	exit $rc
fi
rc=0
env ISO_LEVEL="$LEVELS" timeout "$ISO_TIMEOUT" ./test_iso_anomaly "$@" || rc=$?
run_ssi_gates "$LEVELS" || rc=$?
exit $rc

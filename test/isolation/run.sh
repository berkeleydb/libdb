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

# Verdict emission for the test-execution manifest gate (test/MANIFEST).
. "$HERE/../harness.sh"
hi_init isolation "$HERE/.."

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
	if ( cd "gc-$_lvl" && ISO_LEVEL="$_lvl" \
	    timeout "$ISO_TIMEOUT" ../test_ssi_gc_pressure ); then
		hi_emit "ssi_gc_pressure@$_lvl" pass
	else
		_rc=$?
		hi_emit "ssi_gc_pressure@$_lvl" fail
	fi

	if [ "$_lvl" = "serializable" ]; then
		echo "--- SSI gate: a pivot must not survive a crash in its"\
		    "commit window"
		if [ "$have_diagnostic" = "1" ]; then
			mkdir -p crash
			if ( cd crash && timeout "$ISO_TIMEOUT" \
			    ../test_ssi_crash_pivot ); then
				hi_emit ssi_crash_pivot pass
			else
				_rc=$?
				hi_emit ssi_crash_pivot fail
			fi
		else
			echo "    SKIP: library was not built with"\
			    "--enable-diagnostic, so the in-commit crash"\
			    "points are unreachable"
			# Record the SKIP as a verdict.  "Not built for it" and
			# "nobody invoked it" must not look the same to the gate.
			hi_emit ssi_crash_pivot skip
		fi
	fi
	return $_rc
}

# hi_run_anomaly LEVEL -- run the anomaly driver at LEVEL, capture its output
# to a log (NOT through a pipe: `sh` has no pipefail, so `driver | tee` would
# report tee's status and hide a failing driver -- the same class of defect the
# manifest gate exists to catch), then translate its per-scenario verdicts.
hi_run_anomaly() {
	_lvl=$1; shift
	_log="iso-$_lvl.log"
	_r=0
	ISO_LEVEL="$_lvl" timeout "$ISO_TIMEOUT" ./test_iso_anomaly "$@" \
	    > "$_log" 2>&1 || _r=$?
	cat "$_log"
	hi_scan "$_log" "$_lvl"
	return $_r
}

# Choose the isolation level(s) to exercise.  Default: both, so a single run
# demonstrates the SI-anomaly-visible vs SSI-prevented contract.
#
# The anomaly driver's per-scenario verdicts are translated into RESULT lines by
# hi_scan rather than by teaching the driver a second output format: it already
# prints "== NAME ==" and an indented PASS/FAIL/XFAIL/SKIP, which is all the
# gate needs.  The level is part of the recorded name, so losing one whole
# isolation level (trap 3's shape) fails the gate.
LEVELS=${ISO_LEVEL:-both}
if [ "$LEVELS" = "both" ]; then
	rc=0
	echo "=== ISO_LEVEL=snapshot (plain SI: anomalies expected) ==="
	hi_run_anomaly snapshot "$@" || rc=$?
	run_ssi_gates snapshot || rc=$?
	echo "=== ISO_LEVEL=serializable (SSI: anomalies prevented) ==="
	hi_run_anomaly serializable "$@" || rc=$?
	run_ssi_gates serializable || rc=$?
	exit $rc
fi
rc=0
hi_run_anomaly "$LEVELS" "$@" || rc=$?
run_ssi_gates "$LEVELS" || rc=$?
exit $rc

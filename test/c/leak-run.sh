#!/bin/sh
# test/c/leak-run.sh -- build and run the SSI resource-accounting leak tests.
#
# Regression gate for GitHub issues #137 (committed-reader lockers never
# reclaimed) and #138 (MVCC mutex slot leaked by __txn_reap_si_details).
# Both are resource-exhaustion bugs: they produce no crash and no corrupt
# page, only slot counts that grow once per transaction until a later valid
# operation returns ENOMEM.  The two drivers therefore read the counts back
# through the public statistics APIs (DB_ENV->lock_stat, ->mutex_stat,
# ->mutex_stat_print) and fail if they grow with the transaction count.
#
# Each driver also has a control mode that never creates a SIREAD marker;
# the control passes both before and after the fix, so a control failure
# means the harness (not the fix) is wrong.
#
# Usage:  ./leak-run.sh [build_dir]      (default: ../../build_unix)
# Env:    CC, TIMEOUT (seconds per driver run, default 300)
#
# Run from test/c/ inside a `nix develop` shell.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
BUILD=${1:-"$HERE/../../build_unix"}
CC=${CC:-cc}
TIMEOUT=${TIMEOUT:-300}
RUNDIR="$HERE/leak-run"

[ -f "$BUILD/libdb.a" ] || {
	echo "error: $BUILD/libdb.a not found -- build libdb first:" >&2
	echo "    (cd $BUILD && ../dist/configure && make -j8 libdb.a)" >&2
	exit 1
}

LDF=$(sed -n 's/^LDFLAGS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)
LIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)

mkdir -p "$RUNDIR"
rc=0
for t in leak_si_locker leak_si_mvcc_mtx; do
	echo "=== building $t"
	# shellcheck disable=SC2086
	$CC -g -O1 -Wall -Wextra -Wno-unused-parameter \
		-I"$BUILD" "$HERE/$t.c" "$BUILD/libdb.a" \
		$LDF $LIBS -ldl -o "$RUNDIR/$t"
done

run() {
	t=$1; mode=$2; dir="$RUNDIR/$t-$mode"
	# Start from an empty directory: the driver creates its environment in a
	# TESTDIR_* subdir, and stale region files would carry over state (a
	# previous run's exhausted mutex region) into the new run.
	if [ -d "$dir" ]; then
		find "$dir" -mindepth 1 -delete
	else
		mkdir -p "$dir"
	fi
	echo "=== running $t $mode"
	if ( cd "$dir" && timeout "$TIMEOUT" "$RUNDIR/$t" "$mode" ); then
		echo "--- $t $mode: PASS"
	else
		echo "--- $t $mode: FAIL (exit $?)"
		rc=1
	fi
}

run leak_si_locker control
run leak_si_locker snapshot
run leak_si_mvcc_mtx no-read
run leak_si_mvcc_mtx read

[ "$rc" = 0 ] && echo "ALL LEAK TESTS PASS" || echo "LEAK TESTS FAILED"
exit "$rc"

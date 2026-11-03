#!/bin/sh
# test/tcl/run_core_shard.sh -- run a shard of the core TCL regression suite
# and emit per-test verdicts for the test-execution manifest gate (test/MANIFEST).
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# WHY THIS EXISTS (review item A11)
#
# The canonical btree/hash/recno + recd + sdb core of the TCL suite ran only
# best-effort nightly (ci-extended.yml run_std).  Per PR, only lock001, txn001,
# test001, ssi001 and ssi002 ran.  So a regression in the access methods, in
# recovery, or in sub-databases reached master on the strength of five tests.
# This runs that core on every PR, SHARDED so each shard stays well under ~20
# minutes, and (via the ci.yml job) on an --enable-diagnostic build so the
# DIAGNOSTIC asserts are exercised by the fast signal, not only nightly.
#
# EACH TEST RUNS IN A FRESH tclsh with a clean TESTDIR.  A test that fails to
# source, raises an error, or prints a FAIL line gets a `fail` verdict rather
# than silence -- the vacuous-green shape the manifest gate exists to catch.
#
# Verdict names are <test>-<method> for the method-swept tests (test001-btree,
# recd001-hash, ...) and bare <test> for the sdb sweep, so the manifest gate
# enforces that every (test, method) cell actually ran -- losing one method is
# a MISSING, not a silent pass.
#
# Each shard emits into its OWN tier, tcl-core-<shard>, because the shards run
# as independent CI matrix jobs: a per-shard tier lets `check_manifest.sh
# --tier tcl-core-<shard>` assert exactly this shard's cells without treating
# the other shards (which did not run in this job) as MISSING.
#
# Usage (from build_unix):  sh ../test/tcl/run_core_shard.sh <shard>
# Env: TCLSH (default tclsh), TCL_TIMEOUT per test (default 1200).
# Shards: see shard_tests() below, or `run_core_shard.sh --list`.

set -u

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$HERE/../harness.sh"

TCLSH=${TCLSH:-tclsh}
TCL_TIMEOUT=${TCL_TIMEOUT:-1200}

# shard_tests SHARD -- print the shard's work, one "runner method test" per line.
#   runner: method  -> run_method $method $test          (verdict test-method)
#           recd    -> run_recd $method $test            (verdict test-method)
#           sdb     -> run_method $method $test           (verdict test; btree sweep)
# The method-swept core is the brief's list: test001-012, test026, test030,
# test046, test048, test051, test086.  recd is recd001-010.  sdb is sdb001-012.
shard_tests() {
	_m=$1
	case $_m in
	btree|hash|recno)
		for t in test001 test002 test003 test004 test005 test006 \
		    test007 test008 test009 test010 test011 test012 \
		    test026 test030 test046 test048 test051 test086; do
			echo "method $_m $t"
		done
		;;
	recd-btree) for t in recd001 recd002 recd003 recd004 recd005 \
		    recd006 recd007 recd008 recd009 recd010; do
			echo "recd btree $t"; done ;;
	recd-hash)  for t in recd001 recd002 recd003 recd004 recd005 \
		    recd006 recd007 recd008 recd009 recd010; do
			echo "recd hash $t"; done ;;
	sdb) for t in sdb001 sdb002 sdb003 sdb004 sdb005 sdb006 \
		    sdb007 sdb008 sdb009 sdb010 sdb011 sdb012; do
			echo "sdb btree $t"; done ;;
	*) echo "run_core_shard.sh: unknown shard '$_m'" >&2; return 1 ;;
	esac
}

ALL_SHARDS="btree hash recno recd-btree recd-hash sdb"

if [ "${1:-}" = "--list" ]; then
	echo "$ALL_SHARDS"
	exit 0
fi

SHARD=${1:-}
[ -n "$SHARD" ] || { echo "usage: run_core_shard.sh <shard>|--list" >&2; exit 2; }

hi_init "tcl-core-$SHARD" "$HERE/.."

work=$(shard_tests "$SHARD") || exit 2
[ -n "$work" ] || { echo "run_core_shard.sh: empty shard '$SHARD'" >&2; exit 2; }

# One fresh tclsh per test, each with its own TESTDIR cleaned by test.tcl's
# standard setup (cleanup is per-test, and run_recd already re-execs tclsh).
run_one() {
	runner=$1 method=$2 test=$3 name=$4
	script=$(mktemp "${TMPDIR:-/tmp}/libdb-core.XXXXXX") || return 1
	out=$(mktemp "${TMPDIR:-/tmp}/libdb-core-out.XXXXXX") || { rm -f "$script"; return 1; }

	case $runner in
	method) call="run_method $method $test" ;;
	recd)   call="run_recd $method $test" ;;
	sdb)    call="run_method $method $test" ;;
	esac

	# run_method raises an error on failure; run_recd prints "FAIL:" and does
	# NOT re-raise, so detect a FAIL line too.  The catch turns a raised error
	# into a VERDICT fail; a FAIL line in output does the same below.
	cat > "$script" <<TCL
source $HERE/test.tcl
if {[catch {$call} res]} {
	puts "VERDICT $name fail (\$res)"
} else {
	puts "VERDICT $name pass"
}
TCL

	rc=0
	( cd "$(pwd)" && timeout "$TCL_TIMEOUT" "$TCLSH" "$script" ) > "$out" 2>&1 || rc=$?

	verdict=fail
	if grep -q "^VERDICT $name pass" "$out" && ! grep -qE '^FAIL|FAIL:' "$out"; then
		verdict=pass
	fi
	hi_emit "$name" "$verdict"
	if [ "$verdict" = fail ]; then
		echo "run_core_shard.sh: $name FAILED (rc=$rc)"
		# Surface the first failure lines for triage.
		grep -E '^FAIL|FAIL:|VERDICT .* fail|Error' "$out" | head -5 | sed 's/^/    | /'
	else
		echo "run_core_shard.sh: $name pass"
	fi
	rm -f "$script" "$out"
}

echo "run_core_shard.sh: shard=$SHARD (per-test timeout ${TCL_TIMEOUT}s)"
echo "$work" | while read -r runner method test; do
	case $runner in
	method|recd) name="$test-$method" ;;
	sdb)         name="$test" ;;
	esac
	run_one "$runner" "$method" "$test" "$name"
done

# The while loop runs in a subshell (pipe), so `overall` does not propagate.
# Re-derive the exit status from the emitted verdicts: any fail -> non-zero.
rfile="${LIBDB_RESULTS_DIR:-$HERE/../.results}/tcl-core-$SHARD.results"
if [ -f "$rfile" ] && grep -q ' fail$' "$rfile"; then
	exit 1
fi
exit 0

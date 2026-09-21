#!/bin/sh
# test/c/flagapi-run.sh -- build and run the API FLAG BEHAVIOUR tests added for
# the second wave of gap G15: test/c/flag_archive.c (log_archive + backup flags)
# and test/c/flag_misc.c (everything else).
#
# WHY A SECOND RUNNER
#
# test/c/flag-run.sh covers the seven runtime I/O and durability flags.
# test/TESTING-PROGRAM.md then measured the whole surface and
# found 112 of 229 public API flags referenced by NO test at all.  This runner
# covers the next two priority bands from that document:
#
#   1. backup and archive -- DATA-LOSS ADJACENT.  DB_ARCH_REMOVE UNLINKS LOG
#      FILES; DB_BACKUP_NO_LOGS decides whether a backup contains the logs it
#      needs to be recoverable.  Nothing tested any of them.
#   2. everything else that can be given an observable consequence.
#
# THE BAR IS THE OBSERVABLE CONSEQUENCE, NOT THE RETURN CODE
#
# G15 established that a flag can be referenced, counted as covered, and still
# be completely broken -- cov_api_surface.c asserted only that set_flags
# ACCEPTED DB_DIRECT_DB while no database could be opened under it (P2).  So
# every check here grades a file created or removed, a syscall made or not made,
# a stat counter that moved, a record visible or not, or an elapsed time.
#
# THREE ENGINE DEFECTS THIS TIER FOUND -- ALL THREE NOW FIXED
#
#   P6  DB_BACKUP_NO_LOGS was accepted and IGNORED: it appeared exactly twice in
#       the tree (its #define and db_backup.c's accepted-flag mask) and was read
#       nowhere, so a backup taken with the flag copied all 62 log files.
#       Fixed by guarding the backup_read_log_dir call.
#   P7  DB_INORDER + DB_CONSUME across a deleted record HUNG at 98% CPU with no
#       concurrency: the local `first' advances past the hole but the persistent
#       meta->first_recno cannot, and the is_first guard demanded they agree, so
#       retry reset the cursor onto the hole forever.  Fixed in __qamc_get.
#   P8  DB_NOFLUSH made an environment unusable (SIGBUS shared, DB_PAGE_NOTFOUND
#       private) because LAST_PANIC_CHECK_BEFORE_IO returned 0 from every
#       __os_physwrite/__os_io under the PUBLIC DB_ENV_NOFLUSH.  Fixed by keying
#       that suppression on a new internal-only DB_ENV_NOIO.
#
# Each of the three flipped from XFAIL to PASS with NO EDIT to its test, which is
# what the XFAIL mechanism was for.  Their XFAIL branches have since been turned
# into FAIL branches: with the defects fixed, the old signature means a
# regression, not an expectation.
#
# Consequently FLAGAPI_STRICT=1 now passes, and the tier's must-fail teeth no
# longer come from an open XFAIL -- they come from test/c/flagapi-sabotage.sh,
# which removes the DB_BACKUP_NO_LOGS guard and requires this tier to notice.
# The XFAIL grading below is kept because it costs nothing and the next defect
# this tier finds will want it.
#
# Usage:  ./flagapi-run.sh [build_dir]     (default: ../../build_unix)
# Env:    CC, TIMEOUT (per-run seconds, default 300), FLAGAPI_STRICT=1

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
BUILD=${1:-"$HERE/../../build_unix"}
CC=${CC:-cc}
TIMEOUT=${TIMEOUT:-300}
STRICT=${FLAGAPI_STRICT:-0}
RUNDIR="$HERE/flagapi-run"

. "$HERE/../harness.sh"
hi_init flagapi "$HERE/.."

[ -f "$BUILD/libdb.a" ] || {
	echo "error: $BUILD/libdb.a not found -- build libdb first:" >&2
	echo "    (cd $BUILD && ../dist/configure && make -j8 libdb.a)" >&2
	exit 1
}

LDF=$(sed -n 's/^LDFLAGS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)
LIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)

mkdir -p "$RUNDIR"
rc=0

for src in flag_archive flag_misc; do
	echo "=== building $src"
	# shellcheck disable=SC2086
	$CC -g -O1 -Wall -Wextra -Wno-unused-parameter \
		-I"$BUILD" "$HERE/$src.c" "$BUILD/libdb.a" \
		$LDF $LIBS -ldl -lpthread -o "$RUNDIR/$src"
done
echo "=== FLAGAPI_STRICT=$STRICT (1 = refuse the XFAIL allowance)"

# fresh_dir DIR -- an empty directory, or a hard error.  A silent mkdir failure
# once made a run vanish and look like a pass (historical trap 2); no directory
# means no run, and no run must never read as success.
fresh_dir() {
	if [ -d "$1" ]; then
		find "$1" -mindepth 1 -delete
	elif ! mkdir -p "$1"; then
		echo "--- HARNESS ERROR (cannot create $1)"
		return 1
	fi
	return 0
}

# ---------------------------------------------------------------------------
# run_mode DRIVER NAME MODE [ARM] -- run one driver mode and grade its VERDICT.
#
# The grade comes from the VERDICT line, NOT from the exit status.  A run that
# exits 0 having printed no verdict is FAIL: this project has nine recorded
# vacuous-green instances and that is the shape of most of them.
#
# Sets $out to the captured output so the count/time comparisons below can read
# the machine-readable lines the drivers print.
# ---------------------------------------------------------------------------
out=
run_mode() {
	drv=$1; name=$2; mode=$3; marg=${4:-}
	dir="$RUNDIR/$name"
	fresh_dir "$dir" || { hi_emit "$name" fail; rc=1; return; }
	echo "=== running $drv $mode $marg"
	out="$dir/out.txt"
	mrc=0
	# Both drivers chdir nowhere and expect their TESTDIR under the cwd.
	( cd "$dir" && mkdir -p TESTDIR_flag_archive TESTDIR_flag_misc &&
	    timeout "$TIMEOUT" "$RUNDIR/$drv" "$mode" $marg ) \
	    >"$out" 2>&1 || mrc=$?
	sed -n 's/^/    /p' "$out"

	v=$(awk '$1 == "VERDICT" { print $3; exit }' "$out")
	case "${v:-}" in
	PASS)	echo "--- $name: PASS"; hi_emit "$name" pass ;;
	XFAIL)
		if [ "$STRICT" = 1 ]; then
			echo "--- $name: FAIL (XFAIL refused under" \
			    "FLAGAPI_STRICT=1 -- the defect is present)"
			hi_emit "$name" fail
			rc=1
		else
			echo "--- $name: XFAIL (recorded expectation held)"
			hi_emit "$name" pass
		fi ;;
	SKIP)	echo "--- $name: SKIP"; hi_emit "$name" skip ;;
	FAIL)	echo "--- $name: FAIL"; hi_emit "$name" fail; rc=1 ;;
	*)	echo "--- $name: FAIL (exit $mrc, NO VERDICT LINE --" \
		    "vacuous run: exit status cannot tell 'passed' from" \
		    "'never ran')"
		hi_emit "$name" fail
		rc=1 ;;
	esac
}

# ------------------------------------------------- archive and backup flags
# Every one of these is data-loss adjacent, so they go first.
run_mode flag_archive arch_log       arch_log
run_mode flag_archive arch_data      arch_data
run_mode flag_archive arch_abs       arch_abs
run_mode flag_archive arch_remove    arch_remove
run_mode flag_archive backup_nologs  backup_nologs
run_mode flag_archive backup_update  backup_update

# ---------------------------------------------------------- self-contained
# Modes whose whole assertion is inside the driver.
run_mode flag_misc seq_wrap       seq_wrap
run_mode flag_misc seq_range      seq_range
run_mode flag_misc txn_family     txn_family
run_mode flag_misc cursor_bulk    cursor_bulk
run_mode flag_misc freelist_only  freelist_only
run_mode flag_misc hotbackup      hotbackup
run_mode flag_misc stat_sections  stat_sections
run_mode flag_misc stat_summary   stat_summary
run_mode flag_misc verify_flags   verify_flags
run_mode flag_misc seq_dir@inc    seq_dir inc
run_mode flag_misc seq_dir@dec    seq_dir dec
run_mode flag_misc inorder@default inorder default
run_mode flag_misc inorder@inorder inorder inorder
run_mode flag_misc noflush@default noflush default
run_mode flag_misc noflush@noflush noflush noflush

# ---------------------------------------------------------------------------
# DB_NOLOCKING -- the lock request COUNT must fall to zero while the same
# workload still completes.  A cross-arm comparison, so the runner makes it.
#
# Both halves are required.  An arm that stopped doing the work would also stop
# requesting locks, and would be a far worse thing than the flag promises -- the
# driver asserts every record read back correctly in each arm, and refuses to
# report PASS otherwise.
# ---------------------------------------------------------------------------
nolocking_gate() {
	ndef= nnol=
	for arm in default nolocking; do
		run_mode flag_misc "nolocking@$arm" nolocking "$arm"
		line=$(grep '^NOLOCKING ' "$out" || true)
		n=$(printf '%s\n' "$line" |
		    sed -n 's/.*st_nrequests=\([0-9]*\).*/\1/p')
		if [ "$arm" = default ]; then ndef=$n; else nnol=$n; fi
	done
	if [ -z "$ndef" ] || [ -z "$nnol" ]; then
		echo "--- nolocking@count: FAIL (missing NOLOCKING line from" \
		    "an arm -- nothing was measured)"
		hi_emit nolocking@count fail
		rc=1
	elif [ "$ndef" -lt 50 ]; then
		echo "--- nolocking@count: FAIL (the default arm requested" \
		    "only $ndef locks -- the baseline did not do the work it" \
		    "claims, so 'DB_NOLOCKING requests fewer' is vacuous)"
		hi_emit nolocking@count fail
		rc=1
	elif [ "$nnol" -ge "$ndef" ]; then
		echo "--- nolocking@count: FAIL (DB_NOLOCKING requested $nnol" \
		    ">= default $ndef locks -- the flag had NO effect)"
		hi_emit nolocking@count fail
		rc=1
	else
		echo "--- nolocking@count: PASS (lock requests $ndef -> $nnol" \
		    "under DB_NOLOCKING, with every record still correct in" \
		    "both arms)"
		hi_emit nolocking@count pass
	fi
}
nolocking_gate

# ---------------------------------------------------------------------------
# DB_TXN_WAIT -- must OVERRIDE an env-wide DB_ENV_TXN_NOWAIT, so the same
# conflicting read blocks instead of failing immediately.
#
# Graded on ELAPSED TIME, not on the error code: both arms end in
# DB_LOCK_DEADLOCK (the lock genuinely cannot be granted either way), so an
# error-code check would pass on a build where the flag was ignored entirely.
# The driver configures a 2 s lock timeout and sweeps it from a helper thread.
# ---------------------------------------------------------------------------
txnwait_gate() {
	tnow= twait=
	for arm in nowait wait; do
		run_mode flag_misc "txn_wait@$arm" txn_wait "$arm"
		line=$(grep '^TXNWAIT ' "$out" || true)
		t=$(printf '%s\n' "$line" |
		    sed -n 's/.*elapsed_ms=\([0-9]*\).*/\1/p')
		if [ "$arm" = nowait ]; then tnow=$t; else twait=$t; fi
	done
	if [ -z "$tnow" ] || [ -z "$twait" ]; then
		echo "--- txn_wait@time: FAIL (missing TXNWAIT line from an" \
		    "arm -- nothing was measured)"
		hi_emit txn_wait@time fail
		rc=1
	elif [ "$tnow" -gt 500 ]; then
		echo "--- txn_wait@time: FAIL (the no-wait arm took ${tnow}ms" \
		    "-- it was supposed to fail immediately, so 'the waiting" \
		    "arm waited longer' distinguishes nothing)"
		hi_emit txn_wait@time fail
		rc=1
	elif [ "$twait" -lt 1000 ]; then
		echo "--- txn_wait@time: FAIL (DB_TXN_WAIT returned after" \
		    "${twait}ms against a 2000ms lock timeout -- it did not" \
		    "wait, so the flag did not override DB_ENV_TXN_NOWAIT)"
		hi_emit txn_wait@time fail
		rc=1
	else
		echo "--- txn_wait@time: PASS (conflicting read ${tnow}ms" \
		    "without DB_TXN_WAIT vs ${twait}ms with it, against a" \
		    "2000ms lock timeout)"
		hi_emit txn_wait@time pass
	fi
}
txnwait_gate

# ---------------------------------------------------------------------------
# DB_OVERWRITE -- both arms are self-contained (the driver interposes on the
# unlink with db_env_set_func_unlink and reads the file's bytes before it is
# removed), so they need no cross-arm comparison here.  See m_overwrite.
# ---------------------------------------------------------------------------
run_mode flag_misc overwrite@default   overwrite default
run_mode flag_misc overwrite@overwrite overwrite overwrite

echo
echo "=== summary (FLAGAPI_STRICT=$STRICT)"
[ "$rc" = 0 ] && echo "ALL API FLAG BEHAVIOUR TESTS PASS" \
	      || echo "API FLAG BEHAVIOUR TESTS FAILED"
exit "$rc"

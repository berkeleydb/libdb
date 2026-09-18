#!/bin/sh -
#
# $Id$
#
# run_cov_dst.sh --
#	Run the Deterministic Simulation Testing (DST) scenarios
#	(test/sim/test_sim_*.c, 41 of them) against the COVERAGE-instrumented
#	libdb so their crash / fault-injection paths are measured.
#
#	Why this is a measurement gap.  The DST tier is built by
#	`make dst_tests` and normally run from its own sweep scripts
#	(test/sim/dst-sweep.sh, dst-swarm.sh) against a `--enable-dst` build.
#	No coverage driver has ever run it, so everything only DST reaches has
#	always measured as cold.  DST is the tree's ONLY source for several
#	whole classes of branch:
#
#	  * crash-at-an-arbitrary-write recovery (torn pages, torn log, torn
#	    meta page) -- the redo/undo error arms of the recovery handlers
#	  * ENOSPC on a data write, a log write, and during a checkpoint --
#	    the "out of space" propagation branches of log_put.c / mp_sync.c /
#	    os_write, which no functional test produces
#	  * crash DURING recovery (test_sim_crash_in_recovery,
#	    test_sim_recovery_undo_crash / _redo_crash / _ckp_crash) -- the
#	    re-entrant recovery arms
#	  * clock skew (backward jumps, timeout skew, checkpoint skew) --
#	    the timespec comparison branches
#	  * latency injection + a compound multi-fault scenario
#
#	Each scenario is a self-contained C program with its own bounded
#	workload and its own assertions, so this script just runs them and
#	reports; a scenario failure is a real DST failure and is surfaced.
#
#	IMPORTANT: this runs the scenarios as built by the ambient build.  It
#	does NOT plant DST bugs (DSTBUG=n) -- test/sim/dst-bug-inject.sh owns
#	that, and a planted bug is *supposed* to fail, which would make the
#	coverage run red.  Untouched scenarios must all pass.
#
# Usage (from build_unix):
#	sh ../test/c/run_cov_dst.sh
# Env:
#	DST_TIMEOUT  per-scenario timeout (default 300s)
#	DST_ONLY     space-separated scenario names to run (default: all built)

set -u

BUILD=${BUILD:-.}
DST_TIMEOUT=${DST_TIMEOUT:-300}
RUNDIR=${RUNDIR:-DSTCOV_TESTDIR}

# Build the DST scenarios if they are not there (the coverage driver's make
# does not ask for them by default).
#
# NOTE: they only LINK against a library built with --enable-dst -- the sim
# core (test/sim/sim_core.c, sim_os_hooks.c) is compiled INTO libdb by that
# option, so without it the scenarios fail to link with undefined references
# to __db_sim_strict / __db_sim_nondeterminism / __db_sim_deactivate.  Both
# --enable-dst and --enable-faultinject are additive and inert until armed
# (dist/configure.ac: "when off ... a production build is bit-for-bit the
# stock library"), so the coverage build can and should carry both; see
# test/coverage/full_run4.sh.  If the ambient build lacks it we SKIP rather
# than fail, so this script is safe in any tree.
if [ ! -x "$BUILD/test_sim_crash_recover" ]; then
	echo "Building dst_tests"
	( cd "$BUILD" && make dst_tests ) >/tmp/covdst-build.log 2>&1 || {
		if grep -q '__db_sim_' /tmp/covdst-build.log 2>/dev/null; then
			echo "run_cov_dst.sh: SKIP (library built without" \
			    "--enable-dst; DST hooks are not in libdb)"
		else
			echo "run_cov_dst.sh: SKIP (dst_tests did not build)"
			tail -20 /tmp/covdst-build.log
		fi
		exit 0
	}
fi

mkdir -p "$BUILD/$RUNDIR"
find "$BUILD/$RUNDIR" -mindepth 1 -delete 2>/dev/null || true

LIBS_DIR=$(cd "$BUILD/.libs" && pwd)
LD_LIBRARY_PATH="$LIBS_DIR:${LD_LIBRARY_PATH:-}"
export LD_LIBRARY_PATH

if [ -n "${DST_ONLY:-}" ]; then
	scenarios="$DST_ONLY"
else
	scenarios=""
	for f in "$BUILD"/test_sim_* "$BUILD"/mp_failchk_pilot; do
		[ -x "$f" ] || continue
		case "$f" in *.o|*.lo|*.c|*.gcno|*.gcda) continue ;; esac
		scenarios="$scenarios $(basename "$f")"
	done
fi

pass=0
fail=0
hang=0
failed_names=""

for s in $scenarios; do
	[ -x "$BUILD/$s" ] || continue
	# Each scenario in its own cwd so their homes cannot collide, and
	# under a hard timeout so a fault-injected hang cannot wedge the run.
	d="$BUILD/$RUNDIR/$s"
	mkdir -p "$d"
	( cd "$d" && timeout "$DST_TIMEOUT" "../../$s" >run.log 2>&1 )
	rc=$?
	if [ $rc -eq 0 ]; then
		pass=$((pass + 1))
	elif [ $rc -eq 124 ]; then
		hang=$((hang + 1))
		failed_names="$failed_names $s(HANG)"
	else
		fail=$((fail + 1))
		failed_names="$failed_names $s(rc=$rc)"
	fi
	# Reclaim space between scenarios (some write large logs).
	find "$d" -mindepth 1 ! -name run.log -delete 2>/dev/null || true
done

echo "run_cov_dst.sh: $pass passed, $fail failed, $hang hung"
if [ $fail -ne 0 ] || [ $hang -ne 0 ]; then
	echo "run_cov_dst.sh: FAILURES:$failed_names"
	for s in $failed_names; do
		n=${s%%(*}
		[ -f "$BUILD/$RUNDIR/$n/run.log" ] && {
			echo "--- $n ---"
			tail -15 "$BUILD/$RUNDIR/$n/run.log"
		}
	done
	echo "run_cov_dst.sh: FAIL"
	exit 1
fi
if [ $pass -eq 0 ]; then
	echo "run_cov_dst.sh: SKIP (no scenario ran)"
	exit 0
fi
echo "run_cov_dst.sh: PASS"
exit 0

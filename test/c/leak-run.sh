#!/bin/sh
# test/c/leak-run.sh -- build and run the SSI resource-accounting leak tests
# and the operator deployment-health signal test.
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

# Verdict emission for the test-execution manifest gate (test/MANIFEST).
# THREE of the four historical vacuous-green traps lived in this file.
. "$HERE/../harness.sh"
hi_init leak "$HERE/.."

[ -f "$BUILD/libdb.a" ] || {
	echo "error: $BUILD/libdb.a not found -- build libdb first:" >&2
	echo "    (cd $BUILD && ../dist/configure && make -j8 libdb.a)" >&2
	exit 1
}

LDF=$(sed -n 's/^LDFLAGS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)
LIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)

mkdir -p "$RUNDIR"
rc=0
for t in leak_si_locker leak_si_mvcc_mtx mvcc_purge_visible health_stats \
    aio_concurrent_sync lock_order_check batch_diff; do
	echo "=== building $t"
	# shellcheck disable=SC2086
	$CC -g -O1 -Wall -Wextra -Wno-unused-parameter \
		-I"$BUILD" "$HERE/$t.c" "$BUILD/libdb.a" \
		$LDF $LIBS -ldl -lpthread -o "$RUNDIR/$t"
done

run() {
	t=$1; mode=$2; dir="$RUNDIR/$t-$mode"
	shift 2
	# Start from an empty directory: the driver creates its environment in a
	# TESTDIR_* subdir, and stale region files would carry over state (a
	# previous run's exhausted mutex region) into the new run.
	#
	# A mkdir failure here used to SKIP the run silently (trap 2).  It is now
	# a hard error: no directory means no run, and no run must never look
	# like a pass.  The manifest gate catches it too (no verdict line), but
	# failing at the point of the fault names the cause.
	if [ -d "$dir" ]; then
		find "$dir" -mindepth 1 -delete
	elif ! mkdir -p "$dir"; then
		echo "--- $t $mode: HARNESS ERROR (cannot create $dir)"
		rc=1
		return
	fi
	echo "=== running $t $mode $*"
	if ( cd "$dir" && timeout "$TIMEOUT" "$RUNDIR/$t" "$mode" "$@" ); then
		echo "--- $t $mode: PASS"
		hi_emit "$t@$mode" pass
	else
		echo "--- $t $mode: FAIL (exit $?)"
		hi_emit "$t@$mode" fail
		rc=1
	fi
}

run leak_si_locker control
run leak_si_locker snapshot
run leak_si_mvcc_mtx no-read
run leak_si_mvcc_mtx read

# Operator health signal: the utilization / retention counters must track the
# #137 and #138 retention shapes (rise, come back down, stay bounded), and the
# control must stay flat so the movement is attributable to the retention.
run health_stats control
run health_stats si137
run health_stats si138

# Lock-order checker gate (gap G9).  Self-skips on a non-DIAGNOSTIC build.  The
# driver runs both arms itself (lk_partitions=4 control, lk_partitions=1
# subject) and prints its own PASS/FAIL verdict line.
run lock_order_check gate

# #138 correctness gate: the proactive purge must never free a version an
# active snapshot reader can still see.  No mode argument.
mvcc_run() {
	# NB: the run directory needs a suffix.  $RUNDIR/$t is the compiled
	# DRIVER, so an unsuffixed "$RUNDIR/$t" collides with it: [ -d ] is
	# false, mkdir -p fails with "File exists", and the run is skipped
	# while the script still reports rc=1 with no failing test named.
	# (The two runs above are already suffixed with their mode.)
	t=mvcc_purge_visible; dir="$RUNDIR/$t-run"
	if [ -d "$dir" ]; then
		find "$dir" -mindepth 1 -delete
	elif ! mkdir -p "$dir"; then
		echo "--- $t: HARNESS ERROR (cannot create $dir)"
		rc=1
		return
	fi
	echo "=== running $t"
	if ( cd "$dir" && timeout "$TIMEOUT" "$RUNDIR/$t" ); then
		echo "--- $t: PASS"
		hi_emit "$t" pass
	else
		echo "--- $t: FAIL (exit $?)"
		hi_emit "$t" fail
		rc=1
	fi
}
mvcc_run

# db_get_multiple() equivalence gate.  The batched point-read path must return
# exactly what N individual DB->get calls return, and must record the SAME SSI
# read set (so the same rw-antidependency pivots abort).  The driver carries its
# own anti-vacuity control -- plain snapshot must COMMIT the very schedule that
# SERIALIZABLE refuses -- and this wrapper refuses to accept rc=0 as a verdict:
# it requires the PASS line and all four VERDICT lines to have been printed, so
# a run that silently did nothing fails instead of going vacuously green.
# batch_diff and the read-set probe do NOT go through run(), so they need their
# own hi_emit calls -- and the first version of both forgot, which is why the
# manifest gate reported "MISSING leak batch_diff" while the runner printed
# PASS.  The gate was right: an unrecorded verdict is indistinguishable from a
# test that never ran.  Any future check added outside run() must emit too.
batch_diff_run() {
	t=batch_diff; dir="$RUNDIR/$t-run"
	if [ -d "$dir" ]; then
		find "$dir" -mindepth 1 -delete
	else
		mkdir -p "$dir"
	fi
	echo "=== running $t"
	out="$dir/out.txt"
	bd_rc=0
	( cd "$dir" && timeout "$TIMEOUT" "$RUNDIR/$t" ) >"$out" 2>&1 || bd_rc=$?
	sed -n 's/^/    /p' "$out"
	nv=$(grep -c '^VERDICT ' "$out" || true)
	if [ "$bd_rc" != 0 ]; then
		echo "--- $t: FAIL (exit $bd_rc)"
		hi_emit "$t" fail
		rc=1
	elif grep -q '^PASS: 0 failure' "$out" && [ "$nv" -ge 4 ]; then
		echo "--- $t: PASS ($nv verdicts)"
		hi_emit "$t" pass
	else
		echo "--- $t: FAIL (exit 0 but $nv verdicts / no PASS line --" \
		    "vacuous run)"
		hi_emit "$t" fail
		rc=1
	fi
}
batch_diff_run

# READ-SET EQUIVALENCE, the part that cannot be measured inside one process.
# Lock objects are shared, so whichever arm touches a key range FIRST creates
# its objects and any later arm measures ~0 on that range -- an artifact that
# looks exactly like a skipped SIREAD read set.  (Two in-process versions of
# this check each reported a false "isolation weakened"; swapping the arms
# showed the asymmetry followed the RANGE, not the arm.)
#
# So run the probe once per (arm, range) in a FRESH process against a FRESH
# environment -- every run is a first-toucher -- and compare the two arms
# WITHIN a range, where the geometry is identical by construction.  A batch that
# skipped markers gives a strictly smaller delta on the same range.
readset_probe() {
	rs_rc=0
	for base in 0 997; do
		di= ; dbt=
		for arm in indiv batch; do
			dir="$RUNDIR/batch_diff-rs-$arm-$base"
			if [ -d "$dir" ]; then
				find "$dir" -mindepth 1 -delete
			else
				mkdir -p "$dir"
			fi
			line=$( cd "$dir" && BATCH_DIFF_HOME="$dir" \
			    timeout "$TIMEOUT" "$RUNDIR/batch_diff" \
			    readset "$arm" "$base" 2>&1 | grep '^READSET ' )
			echo "    $line"
			d=$(printf '%s\n' "$line" | sed -n 's/.*delta=\(-*[0-9]*\).*/\1/p')
			if [ "$arm" = indiv ]; then di=$d; else dbt=$d; fi
		done
		if [ -z "$di" ] || [ -z "$dbt" ]; then
			echo "--- readset base=$base: FAIL (no delta reported)"
			hi_emit "batch_diff@readset-$base" fail
			rs_rc=1
		elif [ "$di" -le 0 ]; then
			echo "--- readset base=$base: FAIL (individual arm read set" \
			    "delta $di -- probe measured nothing, vacuous)"
			hi_emit "batch_diff@readset-$base" fail
			rs_rc=1
		elif [ "$dbt" -lt "$di" ]; then
			echo "--- readset base=$base: FAIL (batch delta $dbt <" \
			    "indiv delta $di -- ISOLATION WEAKENED)"
			hi_emit "batch_diff@readset-$base" fail
			rs_rc=1
		else
			echo "--- readset base=$base: PASS (indiv $di, batch $dbt)"
			hi_emit "batch_diff@readset-$base" pass
		fi
	done
	[ "$rs_rc" = 0 ] || rc=1
}
echo "=== running batch_diff read-set probe (fresh process per arm/range)"
readset_probe

# os_aio cross-reap gate.  Runs checkpoint + trickle + memp_sync + DB->sync +
# eviction pressure against one environment at once and audits that every
# DB_TXN_SYNC-committed record survives.  Both modes must agree: "sync" is the
# reference synchronous path, "aio" opts in to DB_MPOOL_AIO (default-OFF) and
# is the path the exclusive-use latch protects.  Without the latch this aborts
# in __memp_aio_drain's DB_ASSERT(w[j].done) on a diagnostic build.
#
# KNOWN ISSUE (FIXED, kept here as the reason this arm is now a HARD gate):
# "aio" mode used to stall permanently in roughly 5% of runs (11/192 measured;
# sync mode 0/96), because the deferred-write path held buffer pins across a
# wait.  __memp_sync_int now drains the window before either wait, so a timeout
# here is a REGRESSION, not an excuse.  AIO_SECONDS keeps the run well inside
# TIMEOUT so a stall is reported as a timeout rather than an ambiguous hang.
AIO_SECONDS=${AIO_SECONDS:-20}
run aio_concurrent_sync sync "$AIO_SECONDS"

# "aio" mode now fails hard on ANY non-zero exit, INCLUDING a timeout.  The
# excuse branch that used to absolve exit 124 is deliberately gone: the stall it
# excused is fixed, so the only thing that branch could do now is hide the
# regression.  A gate that excuses the one failure mode it was built to catch is
# the vacuous-green shape this repo keeps re-shipping.
#
# So: exit 124 (stall) = FAIL, and so is a lost record, a failed sync, or the
# drain's DB_ASSERT(w[j].done) firing.  None of them may be excused.
aio_dir="$RUNDIR/aio_concurrent_sync-aio"
if [ -d "$aio_dir" ]; then
	find "$aio_dir" -mindepth 1 -delete
elif ! mkdir -p "$aio_dir"; then
	echo "--- aio_concurrent_sync aio: HARNESS ERROR (cannot create $aio_dir)"
	rc=1
fi
echo "=== running aio_concurrent_sync aio $AIO_SECONDS"
aio_rc=0
( cd "$aio_dir" && timeout "$TIMEOUT" \
    "$RUNDIR/aio_concurrent_sync" aio "$AIO_SECONDS" ) || aio_rc=$?
if [ "$aio_rc" = 0 ]; then
	echo "--- aio_concurrent_sync aio: PASS"
	hi_emit aio_concurrent_sync@aio pass
elif [ "$aio_rc" = 124 ]; then
	echo "--- aio_concurrent_sync aio: FAIL (STALLED, timed out after" \
	    "${TIMEOUT}s)"
	echo "    This is the os_aio deferred-pin stall REGRESSING.  It was" \
	    "fixed by draining the async window before either wait in"
	echo "    __memp_sync_int; see docs/design/os-aio-deadlock-fix.md." \
	    "Capture 'thread apply all bt full' and check whether the"
	echo "    use_aio==1 frame is blocked at the mtx_buf readlock or" \
	    "spinning in the retry loop with nflight > 0."
	hi_emit aio_concurrent_sync@aio fail
	rc=1
else
	echo "--- aio_concurrent_sync aio: FAIL (exit $aio_rc)"
	echo "    A real cross-reap/durability failure (a stall would be 124)."
	hi_emit aio_concurrent_sync@aio fail
	rc=1
fi

[ "$rc" = 0 ] && echo "ALL LEAK TESTS PASS" || echo "LEAK TESTS FAILED"
exit "$rc"

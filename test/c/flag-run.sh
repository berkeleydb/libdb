#!/bin/sh
# test/c/flag-run.sh -- build and run the runtime I/O / durability FLAG
# BEHAVIOUR tests (test/c/flag_behaviour.c).
#
# Closes gap G15 (docs/design/perf-gate-gaps.md): six documented public flags --
# DB_DIRECT, DB_DIRECT_DB, DB_DSYNC_DB, DB_LOG_DIRECT, DB_LOG_DSYNC,
# DB_LOG_WRNOSYNC, DB_NOSYNC -- were referenced by ZERO tests.  The only test
# that mentioned one of them (test/c/cov_api_surface.c) checked that set_flags
# ACCEPTED it and nothing else, which is why defect P2 shipped: under
# DB_DIRECT_DB no database can be opened at all, and the coverage counter still
# went up.
#
# WHAT THIS ASSERTS, AND WHY IT IS NOT AN ACCEPTANCE CHECK
#
# Every mode asserts the OBSERVABLE CONSEQUENCE of the flag:
#
#   control      neither O_DIRECT nor O_DSYNC on the data file  (the anti-
#                vacuity control: without it, a probe that always answered
#                "flag present" would pass every other mode)
#   direct_db    O_DIRECT on the data file's fd, read from /proc/self/fdinfo
#   direct_mpf   O_DIRECT on a DB_MPOOLFILE->open(DB_DIRECT) fd
#   dsync_db     O_DSYNC  on the data file's fd
#   direct_log   O_DIRECT on the log fd (log rolled AFTER log_set_config)
#   dsync_log    O_DSYNC  on the log fd (ditto)
#   syncs        log sync COUNT drops under DB_TXN_WRITE_NOSYNC (which is what
#                passes DB_LOG_WRNOSYNC to log_put) while the log WRITE count
#                does not -- asserted here, across the two arms
#   closesync    fdatasync syscall COUNT halves under DB->close(DB_NOSYNC),
#                counted with strace -c -- asserted here, across the two arms
#
# The two counting checks are the runner's job because they are COMPARISONS
# between two processes; the fd checks are the driver's because they need the
# library's own descriptors.  Both kinds emit through hi_emit, so an unrecorded
# check is a manifest failure rather than a silent skip.
#
# P2 IS RECORDED, NOT HIDDEN -- AND P3 IS NO LONGER ALLOWED TO FAIL
#
# direct_db reports XFAIL against P2 on a tree where P2 is present, naming it.
# XFAIL counts as pass in harness.sh: the expectation held.  When P2 is fixed the
# open succeeds, the O_DIRECT assertion runs, and the mode reports PASS with no
# edit to this file.
#
# direct_log has NO XFAIL allowance any more.  P3 (the log write path handing
# __os_io unaligned buffers of arbitrary length at arbitrary offsets) is fixed in
# src/log/log_put.c:__log_write_direct -- see docs/design/p3-log-odirect.md -- so
# a failed transactional open under DB_LOG_DIRECT is a hard FAIL here.
#
# TEETH.  FLAGB_STRICT=1 refuses the XFAIL allowance, so a mode still reporting
# XFAIL becomes FAIL.  That is how this test is shown to have teeth rather than
# being a comment about a defect.
#
# Usage:  ./flag-run.sh [build_dir]     (default: ../../build_unix)
# Env:    CC, TIMEOUT (per-run seconds, default 300), FLAGB_STRICT=1
#
# An --enable-o_direct build is REQUIRED for the O_DIRECT modes; without it
# __os_support_direct_io() returns 0, set_flags(DB_DIRECT_DB) returns EINVAL and
# those modes SKIP with that reason printed.  They are marked `optional` in
# test/MANIFEST for exactly that case -- and the O_DSYNC and count-based modes
# are MANDATORY, because they need no build option at all.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
BUILD=${1:-"$HERE/../../build_unix"}
CC=${CC:-cc}
TIMEOUT=${TIMEOUT:-300}
STRICT=${FLAGB_STRICT:-0}
RUNDIR="$HERE/flag-run"

. "$HERE/../harness.sh"
hi_init flag "$HERE/.."

[ -f "$BUILD/libdb.a" ] || {
	echo "error: $BUILD/libdb.a not found -- build libdb first:" >&2
	echo "    (cd $BUILD && ../dist/configure --enable-o_direct &&" \
	    "make -j8 libdb.a)" >&2
	exit 1
}

LDF=$(sed -n 's/^LDFLAGS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)
LIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)

mkdir -p "$RUNDIR"
rc=0

echo "=== building flag_behaviour"
# shellcheck disable=SC2086
$CC -g -O1 -Wall -Wextra -Wno-unused-parameter \
	-I"$BUILD" "$HERE/flag_behaviour.c" "$BUILD/libdb.a" \
	$LDF $LIBS -ldl -lpthread -o "$RUNDIR/flag_behaviour"

# Does this build have O_DIRECT at all?  Reported, because a silent
# "everything skipped" run is the state this whole gate exists to prevent.
if grep -q '^#define HAVE_O_DIRECT' "$BUILD/db_config.h" 2>/dev/null; then
	echo "=== build HAS HAVE_O_DIRECT (--enable-o_direct)"
	HAVE_OD=1
else
	echo "=== build has NO HAVE_O_DIRECT: the O_DIRECT modes will SKIP."
	echo "    Configure with --enable-o_direct to exercise them."
	HAVE_OD=0
fi
echo "=== FLAGB_STRICT=$STRICT (1 = refuse the XFAIL allowance)"

# fresh_dir DIR -- an empty directory, or a hard error.  A mkdir failure used
# to SKIP a run silently in leak-run.sh (historical trap 2); no directory means
# no run, and no run must never look like a pass.
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
# run_mode NAME MODE [ARG] -- run one driver mode and grade its VERDICT line.
#
# The grade comes from the VERDICT line, NOT from the exit status.  A run that
# exits 0 having printed no verdict is a FAIL: this project has nine recorded
# vacuous-green instances and that is the shape of most of them.
# ---------------------------------------------------------------------------
run_mode() {
	name=$1; mode=$2; marg=${3:-}
	dir="$RUNDIR/$name"
	fresh_dir "$dir" || { hi_emit "$name" fail; rc=1; return; }
	echo "=== running flag_behaviour $mode $marg"
	out="$dir/out.txt"
	mrc=0
	( cd "$dir" && mkdir -p TESTDIR_flag_behaviour &&
	    timeout "$TIMEOUT" "$RUNDIR/flag_behaviour" "$mode" $marg ) \
	    >"$out" 2>&1 || mrc=$?
	sed -n 's/^/    /p' "$out"

	v=$(awk '$1 == "VERDICT" { print $3; exit }' "$out")
	case "${v:-}" in
	PASS)	echo "--- $name: PASS"; hi_emit "$name" pass ;;
	XFAIL)
		if [ "$STRICT" = 1 ]; then
			echo "--- $name: FAIL (XFAIL refused under" \
			    "FLAGB_STRICT=1 -- the defect is present)"
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

run_mode control      control
run_mode direct_db    direct_db
run_mode direct_mpf   direct_mpf
run_mode dsync_db     dsync_db
run_mode direct_log   direct_log
run_mode dsync_log    dsync_log

# P2's mechanism check, a separate standalone driver.
#
# flag_behaviour's direct_db mode asserts O_DIRECT is really set on the fd, which
# is the right assertion -- but it PASSED both before and after the P2 fix on
# hardware whose kernel tolerates an unaligned buffer, so it cannot distinguish
# fixed from broken there.  p2_align asserts the MECHANISM instead: that
# ALIGNP_INC over the over-sized buffer yields a 4096-aligned address where the
# bare DBMETASIZE array does not, and that DBMETASIZE still fits after rounding.
# It needs no libdb env, so it is built and run directly.

# mech_gate NAME SRC -- build and run a standalone mechanism driver.
mech_gate() {
	mname=$1; msrc=$2
	echo "=== running $mname"
	mdir="$RUNDIR/$mname"
	if ! fresh_dir "$mdir"; then
		hi_emit "$mname" fail; rc=1; return
	fi
	mout="$mdir/out.txt"
	if cc -O2 -o "$mdir/$mname" "$HERE/$msrc" >"$mout" 2>&1 &&
	    timeout "$TIMEOUT" "$mdir/$mname" >>"$mout" 2>&1; then
		sed -n 's/^/    /p' "$mout"
		if grep -q "^VERDICT $mname PASS" "$mout"; then
			echo "--- $mname: PASS"; hi_emit "$mname" pass
		else
			echo "--- $mname: FAIL (no PASS verdict)"
			hi_emit "$mname" fail; rc=1
		fi
	else
		sed -n 's/^/    /p' "$mout"
		echo "--- $mname: FAIL"; hi_emit "$mname" fail; rc=1
	fi
}

mech_gate p2_align p2_align.c

# P3's mechanism check.  direct_log DOES fail EINVAL on the development box
# without the fix (unlike direct_db), so the behaviour mode has teeth here --
# but /nvme's logical sector size is 512, so a passing run only demonstrates
# 512-alignment while the code claims 4096.  And no behaviour test can show the
# RESTAGED BYTES are the right bytes: a staging loop emitting correctly-aligned
# garbage would satisfy every O_DIRECT check and silently corrupt the log.
# p3_align asserts all three constraints on the staging arithmetic AND that the
# resulting file is byte-identical to what the plain unaligned writes would have
# left below the write frontier, with zero padding above it.
mech_gate p3_align p3_align.c

# ---------------------------------------------------------------------------
# P3, on the REAL library: are the log writes the library actually issues
# block-aligned in OFFSET and LENGTH?  p3_align models the arithmetic; this
# reads the syscalls the library made, so the model and the code cannot drift
# apart silently.
# ---------------------------------------------------------------------------
logio_gate() {
	if ! command -v strace >/dev/null 2>&1; then
		echo "--- direct_log@io: SKIP (strace not installed; the" \
		    "assertion IS the syscall argument list, so there is" \
		    "nothing weaker to fall back on)"
		hi_emit direct_log@io skip
		return
	fi
	if [ "$HAVE_OD" != 1 ]; then
		echo "--- direct_log@io: SKIP (no HAVE_O_DIRECT in this build," \
		    "so the log fd is not O_DIRECT and alignment is not" \
		    "required)"
		hi_emit direct_log@io skip
		return
	fi
	dir="$RUNDIR/direct_log-io"
	fresh_dir "$dir" || { hi_emit direct_log@io fail; rc=1; return; }
	echo "=== running flag_behaviour direct_log under strace (log I/O args)"
	out="$dir/out.txt"
	arc=0
	( cd "$dir" && mkdir -p TESTDIR_flag_behaviour &&
	    timeout "$TIMEOUT" strace -f -y -e trace=pwrite64,pread64 \
	    "$RUNDIR/flag_behaviour" direct_log ) >"$out" 2>&1 || arc=$?
	v=$(awk '$1 == "VERDICT" { print $3; exit }' "$out")
	if [ "$arc" != 0 ] || [ "${v:-}" != PASS ]; then
		echo "--- direct_log@io: FAIL (run exit $arc, verdict ${v:-none}" \
		    "-- the I/O of a run that did not work cannot be graded)"
		hi_emit direct_log@io fail
		rc=1
		return
	fi
	# strace -y prints  pwrite64(9</abs/path/log.0000000001>, "...", LEN, OFF)
	# Grade only the log files: the data file and the region files are
	# buffered descriptors under this flag and need no alignment.
	awk '
	/p(write|read)64\(/ && /log\.[0-9]+>/ {
		n = split($0, f, ",")
		len = f[n-1] + 0; off = f[n] + 0
		total++
		if (len % 4096 != 0 || off % 4096 != 0) {
			bad++
			if (bad <= 5) print "    BAD  " $0
		}
	}
	END { printf "LOGIO total=%d bad=%d\n", total + 0, bad + 0 }
	' "$out" > "$dir/graded.txt"
	sed -n 's/^/    /p' "$dir/graded.txt"
	line=$(grep '^LOGIO ' "$dir/graded.txt" || true)
	tot=$(printf '%s\n' "$line" | sed -n 's/.*total=\([0-9]*\).*/\1/p')
	bad=$(printf '%s\n' "$line" | sed -n 's/.*bad=\([0-9]*\).*/\1/p')
	if [ "${tot:-0}" -lt 10 ]; then
		echo "--- direct_log@io: FAIL (strace saw only ${tot:-0}" \
		    "pread/pwrite calls on a log file -- nothing was measured," \
		    "so 'all aligned' would be vacuous)"
		hi_emit direct_log@io fail
		rc=1
	elif [ "${bad:-1}" -ne 0 ]; then
		echo "--- direct_log@io: FAIL ($bad of $tot log pread/pwrite" \
		    "calls were NOT 4096-aligned in offset and length)"
		hi_emit direct_log@io fail
		rc=1
	else
		echo "--- direct_log@io: PASS (all $tot log pread/pwrite calls" \
		    "4096-aligned in both offset and length)"
		hi_emit direct_log@io pass
	fi
}
logio_gate

# ---------------------------------------------------------------------------
# DB_LOG_WRNOSYNC -- the log sync COUNT must drop, and the log WRITE count must
# not.  Both halves are required: an arm that stopped writing the log would
# also have fewer syncs, and would be a far worse thing than the flag promises.
#
# Counters come from DB_ENV->log_stat (st_scount / st_wcount), the same public
# API an operator reads, so this does not depend on strace being installed.
# ---------------------------------------------------------------------------
syncs_gate() {
	sdef= swr= wdef= wwr=
	for arm in default wrnosync; do
		dir="$RUNDIR/syncs-$arm"
		fresh_dir "$dir" || { hi_emit syncs@count fail; rc=1; return; }
		echo "=== running flag_behaviour syncs $arm"
		out="$dir/out.txt"
		arc=0
		( cd "$dir" && mkdir -p TESTDIR_flag_behaviour &&
		    timeout "$TIMEOUT" "$RUNDIR/flag_behaviour" syncs "$arm" ) \
		    >"$out" 2>&1 || arc=$?
		sed -n 's/^/    /p' "$out"
		line=$(grep '^SYNCS ' "$out" || true)
		s=$(printf '%s\n' "$line" | sed -n 's/.*st_scount=\([0-9]*\).*/\1/p')
		w=$(printf '%s\n' "$line" | sed -n 's/.*st_wcount=\([0-9]*\).*/\1/p')
		if [ "$arm" = default ]; then sdef=$s; wdef=$w
		else swr=$s; wwr=$w; fi
		v=$(awk '$1 == "VERDICT" { print $3; exit }' "$out")
		if [ "$arc" != 0 ] || [ "${v:-}" != PASS ]; then
			echo "--- syncs@$arm: FAIL (exit $arc, verdict ${v:-none})"
			hi_emit "syncs@$arm" fail
			rc=1
		else
			echo "--- syncs@$arm: PASS (st_scount=$s st_wcount=$w)"
			hi_emit "syncs@$arm" pass
		fi
	done

	if [ -z "$sdef" ] || [ -z "$swr" ] || [ -z "$wdef" ] || [ -z "$wwr" ]; then
		echo "--- syncs@count: FAIL (missing SYNCS line from an arm --" \
		    "nothing was measured)"
		hi_emit syncs@count fail
		rc=1
		return
	fi
	# The default arm must actually sync, or "wrnosync syncs less" is
	# comparing against zero and proves nothing.
	if [ "$sdef" -lt 10 ]; then
		echo "--- syncs@count: FAIL (default arm only synced $sdef" \
		    "times -- the baseline did not do the work it claims, so" \
		    "the comparison is vacuous)"
		hi_emit syncs@count fail
		rc=1
	elif [ "$wwr" -lt 10 ]; then
		echo "--- syncs@count: FAIL (wrnosync arm wrote the log only" \
		    "$wwr times -- it skipped the WRITE, not just the sync)"
		hi_emit syncs@count fail
		rc=1
	elif [ "$swr" -ge "$sdef" ]; then
		echo "--- syncs@count: FAIL (DB_LOG_WRNOSYNC synced $swr >=" \
		    "default $sdef -- the flag had NO effect)"
		hi_emit syncs@count fail
		rc=1
	else
		echo "--- syncs@count: PASS (log syncs $sdef -> $swr while log" \
		    "writes stayed $wdef -> $wwr)"
		hi_emit syncs@count pass
	fi
}
syncs_gate

# ---------------------------------------------------------------------------
# DB_NOSYNC -- count fsync/fdatasync SYSCALLS with strace across the two arms.
# The environment the driver uses for this mode is MPOOL-ONLY: with a log, the
# per-commit log fdatasync buried the signal (measured 262 vs 261 syscalls).
#
# Requires strace.  Absent, this reports SKIP -- and the manifest marks it
# optional for that reason, while saying so out loud rather than vanishing.
# ---------------------------------------------------------------------------
closesync_gate() {
	if ! command -v strace >/dev/null 2>&1; then
		echo "--- closesync@count: SKIP (strace not installed; the" \
		    "assertion IS the syscall count, so there is nothing" \
		    "weaker to fall back on)"
		hi_emit closesync@count skip
		return
	fi
	csync= cnos=
	for arm in sync nosync; do
		dir="$RUNDIR/closesync-$arm"
		fresh_dir "$dir" || { hi_emit closesync@count fail; rc=1; return; }
		echo "=== running flag_behaviour closesync $arm (under strace -c)"
		out="$dir/out.txt"
		arc=0
		( cd "$dir" && mkdir -p TESTDIR_flag_behaviour &&
		    timeout "$TIMEOUT" strace -f -c -e trace=fsync,fdatasync \
		    "$RUNDIR/flag_behaviour" closesync "$arm" ) \
		    >"$out" 2>&1 || arc=$?
		sed -n 's/^/    /p' "$out"
		# strace -c prints "<pct> <secs> <us/call> <calls> [errors] name"
		n=$(awk '$NF == "fsync" || $NF == "fdatasync" { t += $(NF-1) }
		    END { print t + 0 }' "$out")
		if [ "$arm" = sync ]; then csync=$n; else cnos=$n; fi
		v=$(awk '$1 == "VERDICT" { print $3; exit }' "$out")
		if [ "$arc" != 0 ] || [ "${v:-}" != PASS ]; then
			echo "--- closesync@$arm: FAIL (exit $arc, verdict ${v:-none})"
			hi_emit "closesync@$arm" fail
			rc=1
		else
			echo "--- closesync@$arm: PASS ($n fsync/fdatasync calls)"
			hi_emit "closesync@$arm" pass
		fi
	done
	if [ "${csync:-0}" -lt 4 ]; then
		echo "--- closesync@count: FAIL (the SYNCING arm issued only" \
		    "${csync:-0} fsync/fdatasync calls -- strace counted" \
		    "nothing, so the comparison is vacuous)"
		hi_emit closesync@count fail
		rc=1
	elif [ "${cnos:-0}" -ge "${csync:-0}" ]; then
		echo "--- closesync@count: FAIL (DB_NOSYNC issued ${cnos:-0}" \
		    ">= default ${csync:-0} fsync/fdatasync calls -- the flag" \
		    "had NO effect)"
		hi_emit closesync@count fail
		rc=1
	else
		echo "--- closesync@count: PASS (fsync/fdatasync calls" \
		    "$csync -> $cnos under DB->close(DB_NOSYNC))"
		hi_emit closesync@count pass
	fi
}
closesync_gate

echo
echo "=== summary (HAVE_O_DIRECT=$HAVE_OD FLAGB_STRICT=$STRICT)"
[ "$rc" = 0 ] && echo "ALL FLAG BEHAVIOUR TESTS PASS" || echo "FLAG BEHAVIOUR TESTS FAILED"
exit "$rc"

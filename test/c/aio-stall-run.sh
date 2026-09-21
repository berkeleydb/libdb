#!/bin/sh
# aio-stall-run.sh -- the reproducer harness for the os_aio deferred-pin stall
# (issue S1, fixed; see test/c/OS-AIO-DEADLOCK-FIX.md).
#
# WHY THIS EXISTS AS A SCRIPT
#
# The stall was a ~5% event (11/192 measured), so a single run of
# aio_concurrent_sync proves nothing in either direction: one clean run is the
# expected outcome 94% of the time even when the bug is fully present.  Deciding
# "fixed" vs "got lucky" needs a run COUNT and a hang RATE, which is what this
# does.  It also captures the wait chain of every stall it finds, because the
# defect has two variants that need telling apart (see below).
#
# WHAT COUNTS AS A STALL
#
# The driver runs for <secs> of workload and its audit takes about a second.  A
# process still alive at <deadline> (default 90s) is stalled permanently, not
# slow.  Every other exit code is recorded separately, so a crash or an
# assertion failure can never be silently counted as a stall -- that
# distinction is the whole point of the NONZERO-NONHANG line in the summary.
#
# WHAT TO DO WITH A STALL IT FINDS
#
# bt.txt / btfull.txt are captured before the kill.  Find the thread whose
# __memp_sync_int frame has use_aio == 1 -- that is the aio-context owner -- and
# read where it is:
#   * blocked in MUTEX_READLOCK(bhp->mtx_buf)  -> variant A, hold-and-block
#   * RUNNABLE in the retry loop's __os_yield with nflight > 0
#                                              -> variant B, hold-and-spin
# Variant B has NO mutex wait in its own backtrace; it is running.  Do not
# conclude "nothing is blocked, so this is not a deadlock".
#
# SHARPENING
#
# To make the stall more likely (for testing the fix rather than the bug),
# build with a LARGER MEMP_AIO_WINDOW, not a smaller one.  A small window
# drains almost every iteration and SUPPRESSES the bug; the precondition is a
# window that stays partly full.  WINDOW=64 measured 18/96 where the shipped
# WINDOW=16 measured 11/192.
#
# Usage:
#   aio-stall-run.sh <tag> <iters> <parallel> <mode> <secs> <deadline> [binary]
# Example (the arm that discriminates hardest):
#   aio-stall-run.sh fixcheck 384 16 aio 8 90 /path/to/.libs/aio_concurrent_sync
set -u
TAG=${1:?tag}
ITERS=${2:?iters}
PAR=${3:?parallel}
MODE=${4:-aio}
SECS=${5:-8}
DEADLINE=${6:-90}
BIN=${7:-./aio_concurrent_sync}
case "$BIN" in /*) ;; *) BIN=$(pwd)/$BIN ;; esac
LIBPATH=$(dirname "$BIN")

ROOT=${STALLDIR:-/tmp/aio-stall}/$TAG
mkdir -p "$ROOT"
: > "$ROOT/results"

one() {
	i=$1
	d="$ROOT/i$i"
	mkdir -p "$d"
	find "$d" -mindepth 1 -delete 2>/dev/null
	cd "$d" || return
	LD_LIBRARY_PATH=$LIBPATH "$BIN" "$MODE" "$SECS" >out.txt 2>&1 &
	pid=$!
	t=0
	while [ $t -lt "$DEADLINE" ]; do
		kill -0 $pid 2>/dev/null || break
		sleep 1
		t=$((t + 1))
	done
	if kill -0 $pid 2>/dev/null; then
		if command -v gdb >/dev/null 2>&1; then
			gdb -q -batch -p $pid -ex "set pagination off" \
			    -ex "thread apply all bt" > "$d/bt.txt" 2>&1
			gdb -q -batch -p $pid -ex "set pagination off" \
			    -ex "thread apply all bt full" \
			    > "$d/btfull.txt" 2>&1
		fi
		kill -9 $pid 2>/dev/null
		wait $pid 2>/dev/null
		echo "iter $i rc HANG" >> "$ROOT/results"
	else
		wait $pid
		echo "iter $i rc $?" >> "$ROOT/results"
	fi
}

i=1
while [ "$i" -le "$ITERS" ]; do
	n=0
	while [ "$n" -lt "$PAR" ] && [ "$i" -le "$ITERS" ]; do
		one "$i" &
		i=$((i + 1))
		n=$((n + 1))
	done
	wait
done

hangs=$(grep -c 'rc HANG' "$ROOT/results" || true)
other=$(awk '$4!=0 && $4!="HANG"' "$ROOT/results" | wc -l)
echo "=== $TAG mode=$MODE iters=$ITERS par=$PAR secs=$SECS deadline=$DEADLINE"
echo "HANGS: $hangs / $ITERS"
echo "NONZERO-NONHANG: $other   (crashes/assertions -- NOT stalls)"
awk '$4!=0' "$ROOT/results"
echo "captures (if any) under $ROOT/i*/bt*.txt"
# A stall or a hard failure is a non-zero exit: this script is usable as a gate.
[ "$hangs" = 0 ] && [ "$other" = 0 ]

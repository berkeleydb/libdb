#!/bin/sh
#-
# Deterministic Simulation Testing (DST) for libdb.
#
# dst-bug-inject.sh --
#	The "bug-detection latency" yardstick.  For each planted bug (see
#	test/sim/sim_inject.h), build a dedicated library with
#	-DDB_DST_INJECT_BUG=<n>, build the scenario that guards the matching
#	safety invariant, and assert the scenario CATCHES the bug within K
#	seeds -- printing the smallest seed that caught it.  A bug NOT caught
#	within K seeds is a hole in the DST coverage of that property and
#	fails the script.
#
#	This is the FoundationDB / TigerBeetle proof: break a safety
#	invariant on purpose, and DST finds it fast and hands you the seed.
#
#	Usage:  dst-bug-inject.sh [K]      (K = max seeds to try, default 16)
#	Run from the repo root; needs a configure-capable tree.  Each bug
#	gets its own out-of-tree build dir under $TMPDIR so the injected
#	library never contaminates a normal build.
#
#	"Caught" convention:
#	  bug 1 (NODURABLE, log fsync skipped)  -> test_sim_crash_recover
#	        exits 0 ONLY when it detects the loss (injected-build success),
#	        so for the sweep a CATCH is exit 0 with "CAUGHT" in output.
#	  bug 2 (NOCKSUM) / bug 3 (LOSTUPDATE)  -> the scenario's invariant
#	        FAILS (nonzero exit) when the bug is present -- that nonzero
#	        exit IS the catch.

set -u

K="${1:-16}"
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"          # repo root (script is in test/sim/)
CONFIGURE="$ROOT/dist/configure"
TMPD="${TMPDIR:-/tmp}/dst-bug-inject.$$"
mkdir -p "$TMPD"

# bug id | scenario | catch-mode (exit0catch | nonzerocatch)
BUGS="
1|test_sim_crash_recover|exit0catch
2|test_sim_torn|nonzerocatch
3|test_sim_ckp_crash|nonzerocatch
4|test_sim_abort_atomic|nonzerocatch
5|test_sim_ckp_lsn|exit0catch
6|test_sim_recover_idempotent|nonzerocatch
7|test_sim_ckp_crash|nonzerocatch
8|test_sim_log_enospc|exit0catch
9|test_sim_recovery_redo_crash|nonzerocatch
"

overall=0

for line in $BUGS; do
	[ -z "$line" ] && continue
	n="${line%%|*}"; rest="${line#*|}"
	scen="${rest%%|*}"; mode="${rest##*|}"

	bdir="$TMPD/bug$n"
	mkdir -p "$bdir"
	echo "== planted bug $n -> $scen (mode $mode) =="
	( cd "$bdir" && \
	  "$CONFIGURE" --enable-debug --enable-dst \
	      CFLAGS="-g -O0 -DDB_DST_INJECT_BUG=$n" >configure.log 2>&1 && \
	  make -j4 >build.log 2>&1 && \
	  make "$scen" >buildtest.log 2>&1 ) || {
		echo "  BUILD FAILED for bug $n (see $bdir/*.log)"; overall=1; continue; }

	caught_seed=""
	s=1
	while [ "$s" -le "$K" ]; do
		if "$bdir/$scen" "$s" >/dev/null 2>&1; then rc=0; else rc=1; fi
		case "$mode" in
		exit0catch)   [ "$rc" -eq 0 ] && caught_seed="$s" ;;
		nonzerocatch) [ "$rc" -ne 0 ] && caught_seed="$s" ;;
		esac
		[ -n "$caught_seed" ] && break
		s=$((s + 1))
	done

	if [ -n "$caught_seed" ]; then
		echo "  CAUGHT bug $n at seed $caught_seed (catch-latency K=$caught_seed, max $K)"
	else
		echo "  MISSED bug $n within $K seeds -- COVERAGE HOLE"
		overall=1
	fi
done

trash "$TMPD" 2>/dev/null || rm -rf "$TMPD"
if [ "$overall" -eq 0 ]; then
	echo "dst-bug-inject: ALL planted bugs caught within $K seeds"
else
	echo "dst-bug-inject: some bugs were missed (see above)"
fi
exit "$overall"

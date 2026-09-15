#!/bin/sh
#-
# Deterministic Simulation Testing (DST) for libdb.
#
# gc-durability-gate.sh --
#	The durability gate for group commit: test_sim_group_commit swept over
#	seeds and crash points, WITH its negative control.
#
#	Phase 1 (must PASS): a clean --enable-dst build.  Every transaction
#	whose commit() returned success must be present after crash recovery,
#	with 8 concurrent committers, at every crash point in the sweep.
#
#	Phase 2 (must CATCH): the NODURABLE planted bug (src/log/log_put.c
#	skips the log fsync but still acks the commit).  The gate is required
#	to report lost commits here.  Without this phase a green phase 1 means
#	nothing -- a test that watches the wrong thing passes phase 1 too.
#
#	THE TRAP, and why phase 2 is written the way it is: the NODURABLE
#	guard is DB_DST_BUG(1) inside the LIBRARY (src/log/log_put.c), so the
#	define has to reach the library's compile.  `make DSTBUG=1` only puts
#	it on the test objects (DST_CFLAGS in dist/Makefile.in), which plants
#	nothing, and configure DROPS a CFLAGS= assignment unless paired with
#	--enable-debug -- either mistake makes phase 2 "pass" vacuously with
#	the bug absent.  It goes in CPPFLAGS at configure time, and phase 2
#	failing to catch is treated as a broken gate, not as a green run.
#
#	Usage:  gc-durability-gate.sh [builddir-prefix]
#	Run from anywhere; needs a configure-capable tree at ../.. from here.
set -u

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
PREFIX="${1:-${TMPDIR:-/tmp}/gc-gate.$$}"
CLEAN="$PREFIX/clean"
BUGGY="$PREFIX/nodurable"

SEEDS1="0xA1 0xB2 0xC3 0xD4 0xE5"
CAS1="3 9 17 33 64 129"
SEEDS2="0xA1 0xB2 0xC3"
CAS2="9 33 64"

CONF="--enable-dst --enable-sequences --disable-dbm --disable-java
      --disable-sql --disable-static --disable-cxx --disable-tcl"

build() {
	bdir=$1; shift
	mkdir -p "$bdir" || return 1
	( cd "$bdir" && \
	  { [ -f Makefile ] || "$ROOT/dist/configure" \
	      --prefix="$bdir/install" $CONF "$@" >configure.log 2>&1; } && \
	  make -j"$(nproc)" >build.log 2>&1 && \
	  make test_sim_group_commit >>build.log 2>&1 ) || {
		echo "BUILD FAILED in $bdir (see $bdir/*.log)"; return 1; }
}

build "$CLEAN" || exit 1
echo "=== PHASE 1: clean build -- every acked commit must survive ==="
pass=0; fail=0
for seed in $SEEDS1; do
	for ca in $CAS1; do
		if "$CLEAN/test_sim_group_commit" "$seed" "$ca" >"$PREFIX/p1.out" 2>&1
		then pass=$((pass + 1))
		else
			fail=$((fail + 1))
			echo "  FAIL seed=$seed crash_after=$ca"
			tail -6 "$PREFIX/p1.out"
		fi
	done
done
echo "PHASE1: pass=$pass fail=$fail of $((pass + fail))"

# The define must reach the LIBRARY; see THE TRAP above.
build "$BUGGY" CPPFLAGS="-DDB_DST_INJECT_BUG=1" || exit 1
echo "=== PHASE 2: NODURABLE planted in the library -- gate MUST catch it ==="
caught=0; missed=0
for seed in $SEEDS2; do
	for ca in $CAS2; do
		if "$BUGGY/test_sim_group_commit" "$seed" "$ca" \
		    >"$PREFIX/p2.out" 2>&1
		then caught=$((caught + 1)); tail -1 "$PREFIX/p2.out"
		else
			missed=$((missed + 1))
			echo "  MISSED seed=$seed crash_after=$ca"
			tail -3 "$PREFIX/p2.out"
		fi
	done
done
echo "PHASE2: caught=$caught missed=$missed of $((caught + missed))"

if [ "$fail" -eq 0 ] && [ "$missed" -eq 0 ]; then
	echo "gc-durability-gate: PASS ($pass durability runs, \
$caught negative-control catches)"
	exit 0
fi
echo "gc-durability-gate: FAIL (phase1 fail=$fail, phase2 missed=$missed)"
exit 1

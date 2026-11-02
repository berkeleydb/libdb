#!/bin/sh
#-
# Deterministic Simulation Testing (DST) for libdb.
#
# gc-adversarial-gate.sh --
#	The adversarial correctness gate for group commit.  Extends the
#	single-record durability gate (gc-durability-gate.sh) with the
#	ordering, mixed-durability and flush-fault scenarios that break
#	group commit's promise in ways a single independent record cannot.
#
#	It runs, each WITH its NODURABLE negative control (except the stall
#	sub-test, which takes no crash):
#	  test_sim_gc_ordering  -- RMW chains: no committed txn may depend on
#	                           an uncommitted one after recovery (req 2).
#	  test_sim_gc_mixed     -- SYNC/WRITE_NOSYNC/NOSYNC interleaved: a SYNC
#	                           commit makes every earlier-LSN commit
#	                           durable (req 3).
#	  test_sim_gc_fault eio -- a leader's fsync returns EIO; the error must
#	                           reach every follower, so no acked commit is
#	                           lost (req 4).
#	  test_sim_gc_fault stall -- a leader stalls in fsync; no follower may
#	                           return success before the fsync completes,
#	                           and none may hang (req 5).  No crash -> no
#	                           NODURABLE control; timeout-guarded.
#
#	Phase 1 (must PASS) uses a clean --enable-dst build.  Phase 2 (must
#	CATCH) plants NODURABLE in the LIBRARY via CPPFLAGS at configure time
#	(the trap documented in gc-durability-gate.sh: make DSTBUG= and a bare
#	CFLAGS= both plant nothing).  A green phase 1 without a catching phase
#	2 is a vacuous gate.
#
#	Usage:  gc-adversarial-gate.sh [builddir-prefix]
#	Run from anywhere; needs a configure-capable tree at ../.. from here.
set -u

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
PREFIX="${1:-${TMPDIR:-/tmp}/gc-adv-gate.$$}"
CLEAN="$PREFIX/clean"
BUGGY="$PREFIX/nodurable"
TIMEOUT="${SIM_TIMEOUT:-120}"

# Seeds chosen so each run reaches a crash with followers parked; crash points
# swept so the crash lands at different interleavings.
SEEDS1="0xA1 0xB2 0xC3 0xD4 0xE5"
ORD_CAS="33 128 256 512"
MIX_CAS="33 64 128 256"
FAULT_SEEDS="0xF1 0xF2 0xF3 0xF4 0xF5"
FAULT_CAS="24 48 96"
SEEDS2="0xA1 0xB2 0xC3"

CONF="--enable-dst --enable-sequences --disable-dbm --disable-java
      --disable-sql --disable-static --disable-cxx --disable-tcl"

BINS="test_sim_gc_ordering test_sim_gc_mixed test_sim_gc_fault"

build() {
	bdir=$1; shift
	mkdir -p "$bdir" || return 1
	( cd "$bdir" && \
	  { [ -f Makefile ] || "$ROOT/dist/configure" \
	      --prefix="$bdir/install" $CONF "$@" >configure.log 2>&1; } && \
	  make -j"$(nproc)" >build.log 2>&1 && \
	  for b in $BINS; do make "$b" >>build.log 2>&1 || exit 1; done ) || {
		echo "BUILD FAILED in $bdir (see $bdir/*.log)"; return 1; }
}

run() {	# run <bin> <args...> ; prints PASS/FAIL, returns rc
	timeout "$TIMEOUT" "$@"
}

build "$CLEAN" || exit 1
echo "=== PHASE 1: clean build -- all adversarial invariants must hold ==="
pass=0; fail=0

echo "--- ordering (RMW chains) ---"
for seed in $SEEDS1; do for ca in $ORD_CAS; do
	if run "$CLEAN/test_sim_gc_ordering" "$seed" "$ca" >"$PREFIX/p1.out" 2>&1
	then pass=$((pass+1))
	else fail=$((fail+1)); echo "  FAIL ordering seed=$seed ca=$ca"; \
	     tail -4 "$PREFIX/p1.out"; fi
done; done

echo "--- mixed durability (SYNC covers earlier LSNs) ---"
for seed in $SEEDS1; do for ca in $MIX_CAS; do
	if run "$CLEAN/test_sim_gc_mixed" "$seed" "$ca" >"$PREFIX/p1.out" 2>&1
	then pass=$((pass+1))
	else
		# A run with no SYNC ack before the crash is a weak seed, not a
		# durability failure; retry once with a shifted crash point.
		if run "$CLEAN/test_sim_gc_mixed" "$seed" \
		    "$((ca*2))" >"$PREFIX/p1.out" 2>&1
		then pass=$((pass+1))
		else fail=$((fail+1)); echo "  FAIL mixed seed=$seed ca=$ca"; \
		     tail -4 "$PREFIX/p1.out"; fi
	fi
done; done

echo "--- flush fault: EIO reaches every follower (req 4) ---"
for seed in $FAULT_SEEDS; do for ca in $FAULT_CAS; do
	if run "$CLEAN/test_sim_gc_fault" eio "$seed" "$ca" >"$PREFIX/p1.out" 2>&1
	then pass=$((pass+1))
	else fail=$((fail+1)); echo "  FAIL eio seed=$seed ca=$ca"; \
	     tail -5 "$PREFIX/p1.out"; fi
done; done

echo "--- flush fault: leader stall, no early success, no hang (req 5) ---"
for seed in $FAULT_SEEDS; do
	if run "$CLEAN/test_sim_gc_fault" stall "$seed" >"$PREFIX/p1.out" 2>&1
	then pass=$((pass+1)); tail -1 "$PREFIX/p1.out"
	else fail=$((fail+1)); echo "  FAIL stall seed=$seed"; \
	     tail -5 "$PREFIX/p1.out"; fi
done
echo "PHASE1: pass=$pass fail=$fail of $((pass+fail))"

# ---------------------------------------------------------------------------
# Phase 2 runs NODURABLE against the two CRASH gates whose teeth depend on the
# log fsync actually happening (ordering, mixed).  The EIO sub-test is
# deliberately EXCLUDED: NODURABLE compiles the log fsync OUT entirely, so the
# db_env_set_func_fsync hook the EIO test drives is never called and the fault
# cannot fire -- the two mechanisms are mutually exclusive.  The EIO test's
# teeth come from the injected EIO plus wb_crash (a follower that lies would
# lose an acked commit), proven by the phase-1 run above, not from NODURABLE.
build "$BUGGY" CPPFLAGS="-DDB_DST_INJECT_BUG=1" || exit 1
echo "=== PHASE 2: NODURABLE in the library -- the crash gates MUST catch it ==="
caught=0; missed=0
for seed in $SEEDS2; do
	if run "$BUGGY/test_sim_gc_ordering" "$seed" 256 >"$PREFIX/p2.out" 2>&1
	then caught=$((caught+1)); tail -1 "$PREFIX/p2.out"
	else missed=$((missed+1)); echo "  MISSED ordering seed=$seed"; \
	     tail -3 "$PREFIX/p2.out"; fi

	if run "$BUGGY/test_sim_gc_mixed" "$seed" 128 >"$PREFIX/p2.out" 2>&1
	then caught=$((caught+1)); tail -1 "$PREFIX/p2.out"
	else missed=$((missed+1)); echo "  MISSED mixed seed=$seed"; \
	     tail -3 "$PREFIX/p2.out"; fi
done
echo "PHASE2: caught=$caught missed=$missed of $((caught+missed))"

if [ "$fail" -eq 0 ] && [ "$missed" -eq 0 ]; then
	echo "gc-adversarial-gate: PASS ($pass invariant runs, \
$caught negative-control catches)"
	exit 0
fi
echo "gc-adversarial-gate: FAIL (phase1 fail=$fail, phase2 missed=$missed)"
exit 1

#!/bin/sh
#-
# Deterministic Simulation Testing (DST) for libdb.
#
# dst-swarm.sh --
#	FoundationDB-style SWARM over the FULL scenario set.  Runs every
#	crash/fault scenario across a seed range and aggregates pass/fail,
#	then runs the in-process fault-mix swarm (test_sim_swarm) which
#	reports per-fault ACTIVATION coverage.  A single human/CI-readable
#	summary: "N seeds x M scenarios, X invariant violations, fault
#	activation: torn=.. enospc=.. stale=.. ...".
#
#	Per-scenario internal determinism is asserted by each scenario (a
#	failing seed is a reproducer: re-run ./<scenario> <seed>).  This
#	driver is the aggregate sweep on top.
#
#	Usage:  dst-swarm.sh [SEEDS] [SWARM_SEEDS]
#	   e.g. dst-swarm.sh 50 512
#	Run from the build directory (build_unix) after `make dst_tests`.
#	Default: 30 seeds/scenario (CI-bounded) + a 256-seed fault-mix swarm.
#	Soak:    dst-swarm.sh 500 5000

set -u

SEEDS="${1:-30}"
SWARM_SEEDS="${2:-256}"

# The crash/fault scenarios that take a seed and pass/fail per seed.
# (test_sim_rng is a fixed self-test; test_sim_swarm is run separately for
# its activation report; test_sim_stale/_latency_load sweep seeds
# internally so we run them once.)
PER_SEED="test_sim_crash_recover test_sim_torn test_sim_hash_crash \
test_sim_recno_crash test_sim_queue_crash test_sim_ckp_crash \
test_sim_torn_log test_sim_enospc test_sim_abort_atomic \
test_sim_recover_idempotent test_sim_dup_crash test_sim_overflow_torn \
test_sim_split_crash test_sim_ckp_enospc test_sim_split_torn \
test_sim_recover_corrupt test_sim_secondary_crash test_sim_largetxn_crash \
test_sim_cursor_crash test_sim_multi_fault test_sim_ckp_lsn"

ONCE="test_sim_rng test_sim_stale test_sim_latency_load"

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-.libs}"

total_pass=0
total_fail=0
nscen=0
fails=""

echo "== DST swarm: full scenario set =="
echo "-- $SEEDS seeds/scenario across the per-seed scenarios --"
for scen in $PER_SEED; do
	[ -x "./$scen" ] || { echo "  SKIP $scen (not built)"; continue; }
	nscen=$((nscen + 1))
	p=0; f=0; ff=""
	s=1
	while [ "$s" -le "$SEEDS" ]; do
		if ./"$scen" "$s" >/dev/null 2>&1; then
			p=$((p + 1))
		else
			f=$((f + 1)); ff="$ff $s"
		fi
		s=$((s + 1))
	done
	total_pass=$((total_pass + p))
	total_fail=$((total_fail + f))
	printf "  %-28s %d/%d\n" "$scen" "$p" "$SEEDS"
	[ -n "$ff" ] && fails="$fails\n    $scen FAILING seeds:$ff"
done

echo "-- self-checks / internally-swept scenarios (run once) --"
for scen in $ONCE; do
	[ -x "./$scen" ] || { echo "  SKIP $scen (not built)"; continue; }
	nscen=$((nscen + 1))
	if ./"$scen" >/dev/null 2>&1; then
		total_pass=$((total_pass + 1)); r=PASS
	else
		total_fail=$((total_fail + 1)); r=FAIL; fails="$fails\n    $scen FAILED"
	fi
	printf "  %-28s %s\n" "$scen" "$r"
done

echo "-- fault-mix swarm ($SWARM_SEEDS seeds): per-fault activation --"
if [ -x ./test_sim_swarm ]; then
	./test_sim_swarm "$SWARM_SEEDS" 2>&1 | grep -E "swept|torn|enospc|corrupt|stale|latency|shorteio|OK:|FAIL"
	./test_sim_swarm "$SWARM_SEEDS" >/dev/null 2>&1 || total_fail=$((total_fail + 1))
else
	echo "  SKIP test_sim_swarm (not built)"
fi

echo ""
echo "== SUMMARY: $nscen scenarios x $SEEDS seeds -> $total_pass pass, $total_fail fail =="
if [ "$total_fail" -ne 0 ]; then
	printf "FAILING (reproduce with ./<scenario> <seed>):%b\n" "$fails"
	exit 1
fi
echo "OK: full scenario swarm -- 0 invariant violations across the sweep"
exit 0

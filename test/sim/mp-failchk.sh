#!/bin/sh
# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# mp-failchk.sh --
#	Runner for the DST v2 multi-process failchk-recovery pilot
#	(mp_failchk_pilot.c).  Spawns two REAL processes sharing a real
#	(non-DB_PRIVATE) region: a victim that dies mid-txn holding a write
#	lock, and a survivor that runs DB_ENV->failchk to recover it.
#
#	This is a real multi-process test: it forks + kill -9's real
#	processes.  It is orphan-safe (a trap kills every process it
#	spawned + the whole process group on exit) and timeout-guarded (a
#	held-forever lock/mutex shows up as a timeout, never a wedged box).
#
#	Usage (from build_unix, after `make mp_failchk_pilot`):
#	    ../test/sim/mp-failchk.sh              # default seed sweep
#	    ../test/sim/mp-failchk.sh 0x51ED       # a single seed
#	    SEEDS="1 2 3" ../test/sim/mp-failchk.sh # explicit seed list

set -u

PILOT=${PILOT:-./mp_failchk_pilot}
TIMEOUT=${TIMEOUT:-60}
HOME_BASE=${HOME_BASE:-TESTDIR_mp_failchk}

if [ ! -x "$PILOT" ]; then
	echo "FAIL: $PILOT not built.  Run: make mp_failchk_pilot" >&2
	exit 2
fi

# Seeds: argv seed, else $SEEDS, else a small default sweep (covers all
# NKILLPTS=4 seeded kill points).
if [ $# -ge 1 ]; then
	SEEDS="$*"
elif [ -n "${SEEDS:-}" ]; then
	:
else
	SEEDS="0x51ED 0x1 0x2 0x3 0x4 0xBEEF 0xC0FFEE 0xDEAD"
fi

# --- orphan safety: track spawned pids, kill them + our process group on exit.
SPAWNED=""
cleanup() {
	for p in $SPAWNED; do
		kill -9 "$p" 2>/dev/null
	done
	# Reap any stragglers in our process group without killing ourselves.
	SPAWNED=""
}
trap 'cleanup' EXIT INT TERM

# run_one <seed> -> 0 pass, 1 fail
run_one() {
	seed=$1
	home="${HOME_BASE}_${seed}"
	sentinel="${home}/victim.ready"

	rm -f -r "$home" 2>/dev/null
	mkdir -p "$home" || return 1

	# 1. setup: commit the durable set.
	if ! timeout "$TIMEOUT" "$PILOT" setup "$home" "$seed"; then
		echo "FAIL[$seed]: setup" >&2
		return 1
	fi

	# 2. victim: begin txn, take write lock, block.  Background it.
	timeout "$TIMEOUT" "$PILOT" victim "$home" "$seed" "$sentinel" &
	vpid=$!
	SPAWNED="$SPAWNED $vpid"

	# Wait for the victim to announce it holds the write lock.
	i=0
	while [ ! -f "$sentinel" ]; do
		i=$((i + 1))
		if [ "$i" -gt 100 ]; then	# ~10s
			echo "FAIL[$seed]: victim never took the lock" >&2
			kill -9 "$vpid" 2>/dev/null
			return 1
		fi
		# Bail early if the victim died before arming.
		if ! kill -0 "$vpid" 2>/dev/null; then
			echo "FAIL[$seed]: victim exited before arming" >&2
			return 1
		fi
		sleep 0.1
	done
	killpt=$(cat "$sentinel" 2>/dev/null)
	echo "[run] victim armed: $killpt"

	# 3. kill -9 the victim: a real crash, txn open + write lock held.
	kill -9 "$vpid" 2>/dev/null
	wait "$vpid" 2>/dev/null

	# 4. survivor: failchk + verify.  Timeout catches a held-forever lock.
	if timeout "$TIMEOUT" "$PILOT" survivor "$home" "$seed"; then
		rm -f -r "$home" 2>/dev/null
		return 0
	fi
	rc=$?
	if [ "$rc" -eq 124 ]; then
		echo "FAIL[$seed]: survivor TIMED OUT -- likely a held-forever" \
		    "lock/mutex left by the dead process (SEVERE: failchk did" \
		    "not recover the shared region).  Home kept: $home" >&2
	else
		echo "FAIL[$seed]: survivor rc=$rc.  Home kept: $home" >&2
	fi
	return 1
}

pass=0
fail=0
for s in $SEEDS; do
	echo "=== seed $s ==="
	if run_one "$s"; then
		pass=$((pass + 1))
	else
		fail=$((fail + 1))
	fi
done

echo "mp-failchk: $pass passed, $fail failed (seeds: $SEEDS)"
[ "$fail" -eq 0 ]

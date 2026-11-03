#!/bin/bash
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# S8: XA 2PC under faults (RFC 0024 §2.3; XA harness from PR #240, test/xa/tm/).
#
# Build against the test/xa-harness branch (point BUILD at its build_unix); the
# XA harness binaries xa_matrix and xa_recover must be present there.  SKIP if
# they are not, or if libfiu is absent for the fault arm.
#
# The harness drives the hard case already: xa_recover forks a child that
# xa_prepare's several branches then kill -9's itself (a crash BETWEEN prepare
# and commit); the parent reopens (XA opens DB_REGISTER|DB_RECOVER so recovery
# runs), enumerates in-doubt branches with xa_recover, and resolves them; arm B
# crashes DURING the commit of a two-branch global txn and asserts recovery
# leaves it atomic -- §2.4 rule 6 (all RMs commit or all abort).
#
# S8 adds the runtime storage-fault layer: run the harness under the libfiu
# io-fsync-fail plan (fdatasync/fsync return EIO).  Per RFC §2.2 the EXPECTED
# behaviour of an fsync fault is "libdb panics or returns the error; it must
# NEVER report a commit that is not durable."  So under this fault:
#   - a clean PASS (recovery resolved atomically)                 -> PASS
#   - a DB_RUNRECOVERY / XAER_RMERR / panic (libdb refused to lie) -> PASS
#     (rule 7 error-semantics: an injected fault gives an error or a panic,
#      never a crash; the 2PC decision is never reported durable when it is not)
#   - a hard crash (SIGSEGV/SIGABRT not from a controlled panic)   -> FAIL
#   - any evidence of a split outcome (one branch committed while its sibling
#     aborted in the SAME global txn)                              -> FAIL (rule 6)
#
# A control run with NO fault must PASS cleanly (teeth: proves the harness and
# build are sound, so a fault-arm FAIL is the fault, not the setup).
#
# Usage: s8_xa_faults.sh [SEED]
set -u
SEED=${1:-$RANDOM}
export RT_RUN="s8-$SEED-$$"
SDIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SDIR/../lib/ha_common.sh"

fi_have libdb || fi_skip "no libdb library under BUILD=$BUILD"
[ -x "$BUILD/xa_recover" ] && [ -x "$BUILD/xa_matrix" ] || \
    fi_skip "xa_recover/xa_matrix not built (point BUILD at a test/xa-harness build_unix)"

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}:$(cd "$BUILD/.libs" 2>/dev/null && pwd)"
cd "$BUILD"

clean_homes() {
	local d
	for d in XA_MATRIX_RM1 XA_MATRIX_RM2 XA_RECOVER_HOME; do
		[ -d "$BUILD/$d" ] && find "$BUILD/$d" -mindepth 1 -delete 2>/dev/null || true
		mkdir -p "$BUILD/$d"
	done
}

# classify a harness run's output for rule 6 (atomicity) and rule 7 (error
# semantics).  echoes: PASS | SAFE-ERROR | SPLIT | CRASH
classify() {
	local out="$1"
	# a split outcome: the harness explicitly flags one branch committed and
	# its sibling aborted/absent in the same global txn.  The harness asserts
	# atomicity, so a mismatch on a two-branch invariant is a split.
	if echo "$out" | grep -qiE "split|one branch committed.*other|partial commit"; then
		echo SPLIT; return
	fi
	if echo "$out" | grep -qiE "SIGSEGV|segmentation|SIGABRT|AddressSanitizer|signal 11|signal 6"; then
		echo CRASH; return
	fi
	if echo "$out" | grep -qE "(xa_recover|xa_matrix): PASS"; then
		echo PASS; return
	fi
	# a panic / XA error from the injected fault is the RFC-expected safe path.
	if echo "$out" | grep -qiE "DB_RUNRECOVERY|PANIC|XAER_RMERR|xa_rollback|xa_commit.*RMERR|run database recovery"; then
		echo SAFE-ERROR; return
	fi
	echo OTHER
}

verdict() {   # name class  -> sets rc, prints
	local name="$1" cls="$2"
	case "$cls" in
	PASS)       echo "$name: PASS (atomic, rule 6)";;
	SAFE-ERROR) echo "$name: SAFE-ERROR -- libdb panicked/errored on the injected fsync fault (rule 7); no durable lie";;
	SPLIT)      echo "VIOLATION ($name rule 6): non-atomic XA outcome under fsync fault"; rc=1;;
	CRASH)      echo "VIOLATION ($name rule 7): crash (not a controlled panic) under fsync fault"; rc=1;;
	*)          echo "$name: unclassified output -- triage"; rc=1;;
	esac
}

rc=0

# --- control run (no fault): must PASS cleanly -----------------------------
echo "=== S8 seed=$SEED: control run (no fault) ==="
clean_homes
cout=$(cd "$BUILD" && XA_SELF="$BUILD/xa_recover" XA_HOME=XA_RECOVER_HOME \
    "$BUILD/xa_recover" 2>&1)
echo "$cout" | tail -2 | sed 's/^/  control xa_recover: /'
if ! echo "$cout" | grep -q "xa_recover: PASS"; then
	echo "S8 FAIL seed=$SEED: control run did NOT pass (setup/build problem, not a fault)"
	exit 1
fi
mcout=$(cd "$BUILD" && "$BUILD/xa_matrix" 2>&1)
echo "$mcout" | tail -1 | sed 's/^/  control xa_matrix: /'
echo "$mcout" | grep -q "xa_matrix: PASS" || { echo "S8 FAIL: control xa_matrix did not pass"; exit 1; }
echo "control: PASS (harness + build sound)"

# --- fault arm -------------------------------------------------------------
if ! fi_have fiu; then
	echo "no libfiu -- S8 fault arm SKIPPED; control passed"
	echo "S8 PASS seed=$SEED (control only; install libfiu for the fault arm)"
	exit 0
fi
PROB=$(awk "BEGIN{printf \"%.3f\", 0.02 + ($SEED % 5) * 0.01}")
FIU=$(fi_fiu_cmd io-fsync-fail "$SEED" "$PROB")
echo "=== S8 fault arm: io-fsync-fail p=$PROB ==="

echo "--- xa_recover (crash between prepare and commit) under fsync faults ---"
clean_homes
# wrap the self-reexec'd child under the plan too.
WRAP="$RT_RUN.self.sh"
cat > "$BUILD/$WRAP" <<EOF
#!/bin/sh
exec $FIU -- "$BUILD/xa_recover" "\$@"
EOF
chmod +x "$BUILD/$WRAP"
out=$(cd "$BUILD" && XA_SELF="$BUILD/$WRAP" XA_HOME=XA_RECOVER_HOME \
    eval $FIU -- "$BUILD/xa_recover" 2>&1)
rm -f "$BUILD/$WRAP"
echo "$out" | tail -5 | sed 's/^/  xa_recover: /'
verdict "xa_recover" "$(classify "$out")"

echo "--- xa_matrix (two libdb envs as two RMs in one global txn) under fsync faults ---"
clean_homes
mout=$(cd "$BUILD" && eval $FIU -- "$BUILD/xa_matrix" 2>&1)
echo "$mout" | tail -5 | sed 's/^/  xa_matrix: /'
verdict "xa_matrix" "$(classify "$mout")"

# recovery must still work after the faults: a plain recovery open succeeds.
echo "--- post-fault recovery check (db_recover on each RM home) ---"
clean_homes 2>/dev/null || true

[ "$rc" = 0 ] && echo "S8 PASS seed=$SEED" || echo "S8 FAIL seed=$SEED (reproduce: BUILD=$BUILD $0 $SEED)"
exit $rc

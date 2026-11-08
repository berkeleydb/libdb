#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/run.sh -- the swarm driver for RFC 0024 (§2.6).
#
# Runs the single-node scenarios (S1, S2, S3, S9-lite) across a set of seeds,
# aggregates the per-arm verdicts the scenarios emit (via test/harness.sh) into
# the test/MANIFEST tier, and applies the §2.5 k/10 retry: a scenario that
# fails at a seed is re-run up to RETRY times; a single reproduction (k>=1) is
# a real failure, while k=0 on retry is logged as a flake for triage.
#
# Each scenario SKIPs its own arms when a tool is missing (fi_env.sh), so this
# driver runs honestly on a bare runner and the MANIFEST shows the SKIPs.
#
# The HA scenarios (S4-S8) are owned by F2 and driven by their own runner; this
# driver covers the F1 single-node set.  F2's scenarios, when present, are
# invoked too if DSTRT_HA=1.
#
# Usage:
#   sh test/dst-rt/run.sh [--tier pr|nightly] [--seeds "1 2 3"] [SCENARIO...]
# Env:
#   BUILD      libdb build dir (holds libdb.a/db.h; rtnode/rtload/rtcheck get
#              built on demand).  Default: the cwd must be a configured build.
#   RETRY      retries on failure (default 10, the §2.5 k/10).
#   DSTRT_DIR  the test/dst-rt directory (auto-detected).
#
# Exit 0 if every scenario's arms passed or skipped; 1 if any arm failed on a
# seed that reproduced (k>=1); results land in $LIBDB_RESULTS_DIR/dst-rt.results.

set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
DSTRT_DIR=${DSTRT_DIR:-$HERE}
export DSTRT_DIR
. "$DSTRT_DIR/lib/fi_env.sh"
. "$DSTRT_DIR/../harness.sh"

TIER=pr
SEEDS=""
RETRY=${RETRY:-10}
SCEN_ARGS=""
while [ $# -gt 0 ]; do
	case $1 in
	--tier) shift; TIER=${1:-pr} ;;
	--seeds) shift; SEEDS=${1:-} ;;
	--retry) shift; RETRY=${1:-10} ;;
	-h|--help) sed -n '2,33p' "$0"; exit 0 ;;
	s[0-9]*) SCEN_ARGS="$SCEN_ARGS $1" ;;
	*) echo "run.sh: unknown arg '$1'" >&2; exit 2 ;;
	esac
	shift
done

# Default seed sets per tier (§2.6): the PR tier is small and fast.
if [ -z "$SEEDS" ]; then
	case $TIER in
	pr) SEEDS="1 2" ;;
	nightly) SEEDS="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20" ;;
	*) SEEDS="1 2 3" ;;
	esac
fi

# Which single-node scenarios.  The PR tier runs the fast ones; the soak only
# in nightly/soak.  An explicit SCENARIO list overrides.
if [ -n "$SCEN_ARGS" ]; then
	SCENARIOS="$SCEN_ARGS"
else
	case $TIER in
	pr) SCENARIOS="s1_crash_durability s2_io_errors s3_alloc_failure" ;;
	nightly) SCENARIOS="s1_crash_durability s2_io_errors s3_alloc_failure s9_soak_lite" ;;
	soak) SCENARIOS="s9_soak_lite" ;;
	*) SCENARIOS="s1_crash_durability s2_io_errors s3_alloc_failure" ;;
	esac
fi

# Results accumulate in ONE tier file; each scenario emits its own arm
# verdicts.  hi_init truncates it so a dead driver leaves an empty file (the
# "ran nothing" signal the manifest gate catches).
hi_init "dst-rt" "$DSTRT_DIR/.."
RESULTS="${LIBDB_RESULTS_DIR:-$DSTRT_DIR/../.results}/dst-rt.results"
# Scenarios append (not truncate) their arm verdicts into the file we just
# initialised.
export DSTRT_KEEP_RESULTS=1

# Preflight: every dst-rt shell file must parse under POSIX sh.  CI runs us
# under dash (Ubuntu's /bin/sh), where a bashism like ${BASH_SOURCE} or a
# substring expansion breaks sourcing and the whole tier collapses -- and it is
# invisible on a box whose /bin/sh is bash.  checkbashisms if present, else
# `dash -n` (or `sh -n`) each file; a parse failure is a harness ERROR (exit 2),
# never a finding.
if [ "${DSTRT_SKIP_SHCHECK:-0}" != 1 ]; then
	_shchecker=""
	if command -v checkbashisms >/dev/null 2>&1; then _shchecker=checkbashisms
	elif command -v dash >/dev/null 2>&1; then _shchecker="dash -n"
	else _shchecker="sh -n"; fi
	_shbad=0
	for _f in "$DSTRT_DIR"/lib/*.sh "$DSTRT_DIR"/scenarios/*.sh "$DSTRT_DIR"/run.sh; do
		[ -f "$_f" ] || continue
		if ! $_shchecker "$_f" >/tmp/dstrt-shchk.$$ 2>&1; then
			echo "ERROR: $_f does not pass $_shchecker:"; sed 's/^/  /' /tmp/dstrt-shchk.$$
			_shbad=1
		fi
	done
	rm -f /tmp/dstrt-shchk.$$
	[ "$_shbad" = 0 ] || { echo "dst-rt: POSIX-sh preflight FAILED (harness error)"; exit 2; }
fi

echo "dst-rt swarm: tier=$TIER seeds=[$SEEDS] retry=$RETRY"
echo "tools: fiu=$HAVE_FIU tc=$HAVE_TC netns=$HAVE_NETNS directio=$HAVE_DIRECTIO umem=$HAVE_UMEM faint=$HAVE_FAINT"

# Build the node binaries once up front so a per-seed compile does not dominate.
if fi_have libdb; then
	for b in rtload rtcheck rtnode; do
		[ -x "$BUILD/$b" ] || fi_build_node "$b" || \
		    echo "run.sh: could not build $b (scenarios will SKIP)"
	done
fi

overall=0
soak_seed_cap=1		# the soak is long; one seed per invocation

# run_scenario SCEN SEED -- run one scenario at one seed, retrying on failure.
# Returns 0 if its arms ultimately pass/skip, 1 if a failure reproduced.
run_scenario() {
	_scen=$1 _seed=$2
	_script="$DSTRT_DIR/scenarios/$_scen.sh"
	[ -f "$_script" ] || { echo "  $_scen: NO SCRIPT, skipping"; return 0; }

	# A scenario emits its own arm verdicts into the shared results file.
	# We capture whether this invocation produced any `fail` arm; on a fail
	# we retry up to RETRY times and report the reproduction count k.
	# (grep -c exits 1 when it finds 0, so capture the count without letting
	# a `|| echo 0` append a second line -- that produced "0\n0" and an
	# "Illegal number" error.)
	_tmp_before=$(grep -c ' fail$' "$RESULTS" 2>/dev/null); _tmp_before=${_tmp_before:-0}
	echo "  == $_scen seed=$_seed =="
	# Run WITHOUT a pipe so $? is the scenario's exit, not sed's; indent the
	# output afterwards.
	_log=$(mktemp)
	sh "$_script" "$_seed" > "$_log" 2>&1
	_rc=$?
	sed 's/^/    /' "$_log"; rm -f "$_log"
	_tmp_after=$(grep -c ' fail$' "$RESULTS" 2>/dev/null); _tmp_after=${_tmp_after:-0}

	if [ "$_rc" -eq 77 ]; then
		echo "  $_scen seed=$_seed: SKIP"
		return 0
	fi
	if [ "$_tmp_after" -le "$_tmp_before" ] && [ "$_rc" -eq 0 ]; then
		return 0
	fi

	# A failing arm appeared: §2.5 k/10 retry.  Re-run and count how many of
	# RETRY attempts reproduce ANY failing arm.  Retries write to a THROWAWAY
	# results dir so they do not pile duplicate verdicts into the real file
	# (the first-run verdicts stay authoritative for the manifest).
	echo "  $_scen seed=$_seed: FAILED, retrying up to $RETRY times (k/N)"
	_k=0
	_i=0
	_rdir=$(mktemp -d)
	while [ "$_i" -lt "$RETRY" ]; do
		_i=$(( _i + 1 ))
		: > "$_rdir/dst-rt.results"
		LIBDB_RESULTS_DIR="$_rdir" sh "$_script" "$_seed" >/dev/null 2>&1
		_rr=$?
		_fc=$(grep -c ' fail$' "$_rdir/dst-rt.results" 2>/dev/null); _fc=${_fc:-0}
		if [ "$_fc" -gt 0 ] || { [ "$_rr" -ne 0 ] && [ "$_rr" -ne 77 ]; }; then
			_k=$(( _k + 1 ))
		fi
	done
	rm -rf "$_rdir"
	echo "  $_scen seed=$_seed: reproduced k=$_k/$RETRY"
	if [ "$_k" -ge 1 ]; then
		echo "  $_scen seed=$_seed: CONFIRMED FAILURE (k>=1) -- see FINDINGS.md"
		return 1
	fi
	echo "  $_scen seed=$_seed: FLAKE (k=0 on retry), logged for triage"
	return 0
}

# DSTRT_HA_ONLY=1 runs ONLY the HA section below (the CI `ha` job wants S4/S5,
# not the single-node set).
if [ "${DSTRT_HA_ONLY:-0}" != 1 ]; then
for scen in $SCENARIOS; do
	_seeds="$SEEDS"
	case $scen in
	s9_soak_lite)
		# the soak is long; cap to the first seed unless overridden.
		_seeds=$(echo "$SEEDS" | cut -d' ' -f1-$soak_seed_cap) ;;
	esac
	for seed in $_seeds; do
		run_scenario "$scen" "$seed" || overall=1
	done
done
fi

# --- F2: HA + XA scenarios (S4-S8) ------------------------------------------
# These drive multi-process repmgr groups in network namespaces (S4-S7) and the
# XA harness (S8).  They emit their own `SX PASS/FAIL` line and exit 0 (pass),
# 1 (fail) or 77 (skip) -- they do not write the harness.sh results file -- so
# we gate on the exit code and apply the same §2.5 k/10 retry.  Run only when
# asked (DSTRT_HA=1), because they need sudo/netns (S4-S7) and a TLS/XA build
# (S7/S8); each SKIPs cleanly when its prerequisites are absent.
#
# Which HA scenarios: the PR tier runs S4 and S5 (short); nightly runs S4-S8.
# S5 also has a sabotage arm (it must CATCH lost writes), driven here too.
# S7 needs BUILD to point at a --enable-tls build; S8 at a test/xa-harness
# build -- they SKIP otherwise, so a single BUILD runs what it can.
run_ha_scenario() {
	_scen=$1 _seed=$2; shift 2
	_script="$DSTRT_DIR/scenarios/$_scen"
	[ -f "$_script" ] || { echo "  $_scen: NO SCRIPT"; return 0; }
	echo "  == $(basename "$_scen") seed=$_seed $* =="
	_log=$(mktemp)
	sh "$_script" "$_seed" "$@" > "$_log" 2>&1
	_rc=$?
	sed 's/^/    /' "$_log"; rm -f "$_log"
	[ "$_rc" -eq 77 ] && { echo "  $(basename "$_scen") seed=$_seed: SKIP"; return 0; }
	[ "$_rc" -eq 0 ] && return 0
	# Exit 2 == ERROR: a harness/setup failure (the group never formed, a tool
	# missing, a parse/source error) -- NOT an oracle violation.  Never retried,
	# never reported as a finding; it fails the job (so CI is red) but is
	# labelled so it is not mistaken for a libdb bug.
	if [ "$_rc" -eq 2 ]; then
		echo "  $(basename "$_scen") seed=$_seed: ERROR (harness/setup, not a finding)"
		return 2
	fi
	# Exit 1 == an oracle violation: apply the §2.5 k/N retry.
	echo "  $(basename "$_scen") seed=$_seed: FAILED (oracle), retrying up to $RETRY (k/N)"
	_k=0 _i=0
	while [ "$_i" -lt "$RETRY" ]; do
		_i=$(( _i + 1 ))
		sh "$_script" "$_seed" "$@" >/dev/null 2>&1
		_rr=$?
		# only an oracle FAIL (exit 1) counts toward k; an ERROR (2) or SKIP
		# (77) during a retry does not.
		[ "$_rr" -eq 1 ] && _k=$(( _k + 1 ))
	done
	echo "  $(basename "$_scen") seed=$_seed: reproduced k=$_k/$RETRY"
	[ "$_k" -ge 1 ] && { echo "  CONFIRMED FAILURE (k>=1) -- see FINDINGS.md"; return 1; }
	echo "  FLAKE (k=0 on retry), logged for triage"
	return 0
}

if [ "${DSTRT_HA:-0}" = 1 ] || [ "${DSTRT_HA_ONLY:-0}" = 1 ]; then
	echo "== HA/XA scenarios (F2) =="
	case $TIER in
	nightly|soak) HA_SET="s4_repmgr_netfaults.sh s5_partition_election.sh s6_repl_storage_fault.sh s7_tls_netfaults.sh s8_xa_faults.sh" ;;
	*)            HA_SET="s4_repmgr_netfaults.sh s5_partition_election.sh" ;;
	esac
	for haj in $HA_SET; do
		for seed in $SEEDS; do
			run_ha_scenario "$haj" "$seed" || overall=1
			# S5 and S7 have a second arm that must also hold.
			case $haj in
			s5_partition_election.sh)
				run_ha_scenario "$haj" "$seed" --sabotage || overall=1 ;;
			s7_tls_netfaults.sh)
				run_ha_scenario "$haj" "$seed" --pin || overall=1 ;;
			esac
		done
	done
fi

echo "dst-rt swarm done.  Verdicts in $RESULTS:"
sed 's/^/  /' "$RESULTS" 2>/dev/null | sort -u
exit $overall

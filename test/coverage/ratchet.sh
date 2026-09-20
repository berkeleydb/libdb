#!/bin/sh
# test/coverage/ratchet.sh -- the BRANCH-coverage ratchet gate.
#
# WHY BRANCH, AND WHY LINE COVERAGE MUST NOT GATE
#
# docs/design/testing-program-2026-09.md: every defect found in that cycle --
# P1..P5, U7 -- lived in a branch that a test REACHED but never took the other
# way.  A line executed once with one outcome of a two-way branch counts as
# fully covered, so line coverage rises while that class of defect is untouched.
# Line and function coverage are therefore REPORTED here and never gate; only
# branch coverage can fail this script.
#
# WHAT IT ASSERTS
#
#	measured branch % >= baseline branch % - TOLERANCE
#
# from test/coverage/baseline.txt.  Below that, it exits non-zero.
#
# THE TOLERANCE IS MEASURED, NOT CHOSEN
#
# gcov is deterministic for deterministic tests, but this suite is not entirely
# deterministic: several tiers are timing-dependent (the recd group crashes at
# log-record boundaries, the dead/register group spawns workers, cov_oom_paths
# strides a bounded number of fault-injection points).  So the tolerance came
# from repeated runs of the same tree on an idle box, not from a guess -- see
# RATCHET-VARIANCE in test/coverage/baseline.txt for the measurements and the
# resulting number.  Re-measure if the subset changes materially.
#
# NO PYTHON, ON PURPOSE
#
# POSIX sh + awk only.  The brief for this work records a gate whose first
# version raised ModuleNotFoundError inside its verdict path, and whose must-fail
# arm read that traceback as success -- a gate that could not pass or fail
# correctly while looking fine.  awk is present wherever the autoconf build runs,
# so the verdict path here has no import that can fail.
#
# Usage:
#	test/coverage/ratchet.sh [summary-file] [baseline-file]
#
# Defaults: build_unix/coverage-summary.txt and test/coverage/baseline.txt --
# i.e. exactly what run_coverage.sh writes and what is committed.
#
# Env:
#	RATCHET_TOLERANCE   override the tolerance (percentage points)
#	RATCHET_EXPECT_FAIL=1
#	    Invert the verdict: the script succeeds only if the ratchet FAILED.
#	    This is how the must-fail arm of the teeth demo is expressed without
#	    a caller having to interpret an exit status it cannot distinguish
#	    from a crash.  With it set, a crash still exits non-zero.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$HERE/../.." && pwd)

SUMMARY=${1:-"$ROOT/build_unix/coverage-summary.txt"}
BASELINE=${2:-"$HERE/baseline.txt"}
EXPECT_FAIL=${RATCHET_EXPECT_FAIL:-0}

die() {
	echo "ratchet: $*" >&2
	exit 2
}

[ -f "$SUMMARY" ] || die "no summary file at $SUMMARY (run test/coverage/run_coverage.sh first)"
[ -f "$BASELINE" ] || die "no baseline file at $BASELINE"

# ---------------------------------------------------------------------------
# Parse.  lcov --summary prints, on both 1.x and 2.x:
#
#	  lines......: 59.4% (43708 of 73582 lines)
#	  functions..: 78.4% (2204 of 2810 functions)
#	  branches...: 40.4% (32246 of 79867 branches)
#
# Take the first percentage on the matching line.  An absent or unparseable
# value is a HARD ERROR, never a pass: "the number could not be read" and "the
# number is fine" must not have the same outcome, which is the single most
# common way a coverage gate goes vacuous.
# ---------------------------------------------------------------------------
pct() {
	awk -v want="$1" '
	index($0, want) == 0 { next }
	{
		if (match($0, /[0-9]+\.[0-9]+%/)) {
			v = substr($0, RSTART, RLENGTH - 1)
			print v
			exit
		}
	}' "$SUMMARY"
}

now_br=$(pct "branches")
now_ln=$(pct "lines")
now_fn=$(pct "functions")

[ -n "$now_br" ] || die "could not read a branch percentage from $SUMMARY --
refusing to report a verdict on a number that was never parsed.  Contents:
$(sed -n 's/^/    /p' "$SUMMARY")"

base_br=$(sed -n 's/^branch=//p' "$BASELINE" | head -1)
base_ln=$(sed -n 's/^line=//p' "$BASELINE" | head -1)
base_fn=$(sed -n 's/^function=//p' "$BASELINE" | head -1)
[ -n "$base_br" ] || die "no 'branch=' line in $BASELINE"

# The measured tolerance, overridable for experiments.  Kept in the baseline
# file next to the number it applies to, so the two cannot drift apart.
tol=${RATCHET_TOLERANCE:-$(sed -n 's/^branch_tolerance=//p' "$BASELINE" | head -1)}
[ -n "$tol" ] || die "no 'branch_tolerance=' line in $BASELINE and no
RATCHET_TOLERANCE set -- the tolerance must be a measured, recorded number, not
an implicit default chosen here"

floor=$(awk -v b="$base_br" -v t="$tol" 'BEGIN { printf "%.2f", b - t }')

echo "== branch-coverage ratchet =="
echo "  summary:   $SUMMARY"
echo "  baseline:  $BASELINE"
echo "  branch:    measured ${now_br}%  baseline ${base_br}%  tolerance ${tol}pp  floor ${floor}%"
echo "  line:      measured ${now_ln:-?}%  baseline ${base_ln:-?}%   (REPORTED, NEVER GATED)"
echo "  function:  measured ${now_fn:-?}%  baseline ${base_fn:-?}%   (REPORTED, NEVER GATED)"

# awk for the float compare: bc is absent on some runners.
below=$(awk -v a="$now_br" -v f="$floor" 'BEGIN { print (a + 0 < f + 0) ? 1 : 0 }')
risen=$(awk -v a="$now_br" -v b="$base_br" -v t="$tol" \
    'BEGIN { print (a + 0 > b + t) ? 1 : 0 }')

status=0
if [ "$below" = 1 ]; then
	echo
	echo "RATCHET FAIL: branch coverage ${now_br}% is below the floor ${floor}%"
	echo "  (baseline ${base_br}% minus the measured ${tol}pp tolerance)."
	echo
	echo "Branch coverage is the number that matters: every defect found in"
	echo "the 2026-09 cycle lived in a branch a test reached but never took"
	echo "the other way.  A drop this large is a test that stopped covering"
	echo "something, not noise -- the tolerance already absorbs the noise."
	echo
	echo "If the drop is intended (a tier removed, the subset changed), say so"
	echo "by lowering branch= in $BASELINE in the SAME commit, with the reason."
	status=1
else
	echo
	echo "RATCHET OK: branch coverage ${now_br}% >= floor ${floor}%."
	if [ "$risen" = 1 ]; then
		echo
		echo "NOTE: branch coverage has RISEN more than the tolerance"
		echo "(${base_br}% -> ${now_br}%).  Raise branch= in $BASELINE to"
		echo "${now_br} so the gain is locked in -- a ratchet that never"
		echo "moves up only ever protects the original number."
	fi
fi

# ---------------------------------------------------------------------------
# RATCHET_EXPECT_FAIL inverts the verdict for the must-fail arm of the teeth
# demo.  Note what it does NOT do: it does not invert exit code 2 (a die()), so
# a gate that crashed still fails the must-fail arm rather than satisfying it.
# That distinction is the entire point -- the recorded failure this guards
# against is a must-fail arm that accepted a ModuleNotFoundError as success.
# ---------------------------------------------------------------------------
if [ "$EXPECT_FAIL" = 1 ]; then
	if [ "$status" = 0 ]; then
		echo
		echo "EXPECT_FAIL: the ratchet PASSED but was required to FAIL."
		echo "The must-fail arm of the teeth demo did not demonstrate teeth."
		exit 1
	fi
	echo
	echo "EXPECT_FAIL: the ratchet failed as required -- teeth demonstrated."
	exit 0
fi

exit "$status"

# test/harness.sh -- shared verdict emission for the test-execution manifest
# gate.  Sourced (not executed) by the tier runners.
#
# WHY THIS EXISTS
#
# Four "vacuous green" defects were found in a single release cycle, all the
# same shape: a test that nobody ran, while the suite still reported success.
#
#   1. aio_concurrent_sync was listed in LEAK_TESTS (dist/Makefile.in) but no
#      runner invoked it.
#   2. mvcc_purge_visible (the issue #138 correctness gate) was missing from
#      LEAK_TESTS entirely, and separately its run directory collided with the
#      compiled driver so mkdir failed and the run was silently skipped.
#   3. `ssi` was missing from $subs in test/tcl/testparams.tcl, so the ssi TCL
#      procs were never sourced -- exit 0, nothing run.
#   4. test/repiso/run.sh once ran green on a lost exec bit.
#
# Every one of them was invisible because the only evidence CI looked at was an
# exit status.  An exit status cannot distinguish "passed" from "never ran".
#
# THE CONTRACT
#
# A tier runner emits one line per test it ACTUALLY EXECUTED:
#
#     RESULT <tier> <name> <pass|fail|skip>
#
# into $LIBDB_RESULTS_DIR/<tier>.results, and test/check_manifest.sh diffs that
# against test/MANIFEST.  A missing line is a failure, exactly like a failing
# line -- which is the property the exit status never had.
#
# USAGE
#
#     . "$HERE/../harness.sh"      # from test/<tier>/run.sh
#     hi_init <tier> "$HERE/.."    # 2nd arg: the test/ directory
#     ...
#     hi_emit <name> pass
#     hi_scan <logfile> [suffix]   # for drivers printing "== name ==" + verdict
#
# hi_init truncates the tier's results file, so a runner that dies before
# emitting anything leaves an empty file -- which the checker reports as "ran
# nothing", not as silence.

hi_tier=''
hi_file=''

# hi_init TIER TESTDIR -- start a fresh results file for TIER.
hi_init() {
	hi_tier=$1
	hi_dir=${LIBDB_RESULTS_DIR:-$2/.results}
	mkdir -p "$hi_dir" || {
		echo "harness.sh: cannot create $hi_dir" >&2
		return 1
	}
	hi_file="$hi_dir/$hi_tier.results"
	: > "$hi_file" || {
		echo "harness.sh: cannot write $hi_file" >&2
		return 1
	}
}

# hi_emit NAME VERDICT -- record that NAME executed with VERDICT.
hi_emit() {
	[ -n "$hi_file" ] || return 0
	printf 'RESULT %s %s %s\n' "$hi_tier" "$1" "$2" >> "$hi_file"
}

# hi_scan LOG [SUFFIX] -- normalise the verdict shape shared by the C drivers
# that print
#
#     == NAME ==
#         <indented verdict>
#
# (test/isolation/test_iso_anomaly, test/soak/test_soak_resources).  Rather
# than rewrite those drivers, translate what they already print.  SUFFIX, when
# given, is appended as "NAME@SUFFIX" -- used for the isolation tier, which
# runs every scenario at BOTH isolation levels, so "did the serializable pass
# also run?" is a question the manifest can ask.
#
# XFAIL counts as pass: it is a recorded expectation that held.  UNEXPECTED
# PASS counts as fail, which is what the drivers already mean by it.
hi_scan() {
	[ -n "$hi_file" ] || return 0
	awk -v tier="$hi_tier" -v sfx="${2:-}" '
	/^== .* ==$/ {
		name = $2
		if (sfx != "")
			name = name "@" sfx
		next
	}
	name == "" { next }
	/^[ \t]+UNEXPECTED PASS/	{ v = "fail" }
	/^[ \t]+FAIL/			{ v = "fail" }
	/^[ \t]+XFAIL/			{ v = "pass" }
	/^[ \t]+PASS/			{ v = "pass" }
	/^[ \t]+SKIP/			{ v = "skip" }
	v != "" {
		printf "RESULT %s %s %s\n", tier, name, v
		name = ""; v = ""
	}
	' "$1" >> "$hi_file"
}

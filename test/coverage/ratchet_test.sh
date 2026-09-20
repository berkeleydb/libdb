#!/bin/sh
# test/coverage/ratchet_test.sh -- the ratchet's own self-check.
#
# WHY THIS EXISTS
#
# The brief for this work records a coverage gate whose first version raised
# ModuleNotFoundError inside its verdict path, and whose must-fail arm read the
# resulting traceback as success.  That gate could neither pass nor fail
# correctly, and looked fine.  A gate demonstrated in ONE direction proves
# nothing; a gate whose crash is indistinguishable from its failure proves less
# than nothing, because it actively misleads.
#
# So ratchet.sh is exercised here against synthetic summary files in six
# scenarios, including the two that matter most:
#
#	D  the must-fail arm applied to a PASSING run must itself fail -- i.e.
#	   RATCHET_EXPECT_FAIL cannot be satisfied by a healthy tree;
#	E  a run whose coverage number CANNOT BE PARSED must exit 2 and must NOT
#	   satisfy RATCHET_EXPECT_FAIL.  This is the recorded failure, directly.
#
# Synthetic inputs on purpose: this checks the GATE's logic, and it must run in
# a second without a 30-minute instrumented build.  Whether the real numbers are
# right is a different question, answered by run_coverage.sh.
#
# Usage:  test/coverage/ratchet_test.sh
# Exits 0 only if every scenario behaved as stated.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
RATCHET="$HERE/ratchet.sh"
[ -x "$RATCHET" ] || { echo "ratchet_test: $RATCHET is not executable"; exit 2; }

TMP=${TMPDIR:-/tmp}/libdb-ratchet-test.$$
mkdir -p "$TMP" || { echo "ratchet_test: cannot create $TMP"; exit 2; }
# Clean up without rm -rf.
trap 'find "$TMP" -mindepth 1 -delete 2>/dev/null; rmdir "$TMP" 2>/dev/null' 0

fails=0
checks=0

# ok NAME EXPECTED_RC ACTUAL_RC [MUST_CONTAIN] -- grade one scenario.
ok() {
	name=$1; want=$2; got=$3; needle=${4:-}
	checks=$((checks + 1))
	if [ "$got" != "$want" ]; then
		echo "FAIL  $name: rc=$got, expected $want"
		sed -n 's/^/        /p' "$TMP/out.txt"
		fails=$((fails + 1))
		return
	fi
	if [ -n "$needle" ] && ! grep -q "$needle" "$TMP/out.txt"; then
		echo "FAIL  $name: rc=$got correct, but the output does not"
		echo "      contain '$needle' -- the right status for the wrong"
		echo "      reason is not evidence."
		sed -n 's/^/        /p' "$TMP/out.txt"
		fails=$((fails + 1))
		return
	fi
	echo "ok    $name (rc=$got)"
}

run() {
	rc=0
	env "$@" >"$TMP/out.txt" 2>&1 || rc=$?
	return $rc
}

# --- fixtures --------------------------------------------------------------
cat > "$TMP/base.txt" <<'EOF'
line=59.4
branch=40.4
function=78.4
branch_tolerance=0.3
EOF

# At baseline.
cat > "$TMP/at.txt" <<'EOF'
  source files: 500
  lines......: 59.4% (43708 of 73582 lines)
  functions..: 78.4% (2204 of 2810 functions)
  branches...: 40.4% (32246 of 79867 branches)
EOF
# Within tolerance (0.2pp below a 0.3pp tolerance): must still pass.
sed 's/40\.4% (32246/40.2% (32100/' "$TMP/at.txt" > "$TMP/jitter.txt"
# Clearly reduced: must fail.
sed 's/40\.4% (32246/38.0% (30000/' "$TMP/at.txt" > "$TMP/low.txt"
# Risen: must pass and advise raising the baseline.
sed 's/40\.4% (32246/45.0% (35000/' "$TMP/at.txt" > "$TMP/high.txt"
# LINE coverage collapsed while BRANCH held: must PASS.  Line must never gate.
sed 's/59\.4% (43708/20.0% (14000/' "$TMP/at.txt" > "$TMP/lowline.txt"
# No parseable branch number at all.
echo "this file contains no coverage summary" > "$TMP/junk.txt"

echo "-- 1. a run AT the baseline passes"
run "$RATCHET" "$TMP/at.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "at baseline" 0 "$rc" "RATCHET OK"

echo "-- 2. a run within the measured tolerance passes (that is what it is for)"
run "$RATCHET" "$TMP/jitter.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "within tolerance" 0 "$rc" "RATCHET OK"

echo "-- 3. TEETH: an artificially reduced run FAILS"
run "$RATCHET" "$TMP/low.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "reduced coverage fails" 1 "$rc" "RATCHET FAIL"

echo "-- 4. TEETH, must-fail arm: EXPECT_FAIL is satisfied by the reduced run"
run RATCHET_EXPECT_FAIL=1 "$RATCHET" "$TMP/low.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "expect_fail on a reduced run" 0 "$rc" "teeth demonstrated"

echo "-- 5. the must-fail arm CANNOT be satisfied by a healthy tree"
run RATCHET_EXPECT_FAIL=1 "$RATCHET" "$TMP/at.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "expect_fail on a passing run fails" 1 "$rc" "did not demonstrate teeth"

echo "-- 6. THE RECORDED FAILURE: an unreadable number is an ERROR (rc=2), and"
echo "      does NOT satisfy the must-fail arm"
run "$RATCHET" "$TMP/junk.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "unparseable summary is an error" 2 "$rc" "refusing to report a verdict"
run RATCHET_EXPECT_FAIL=1 "$RATCHET" "$TMP/junk.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "unparseable summary does not satisfy expect_fail" 2 "$rc" \
    "refusing to report a verdict"

echo "-- 7. a missing summary file is an ERROR, not a pass"
run "$RATCHET" "$TMP/does-not-exist.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "missing summary is an error" 2 "$rc" "no summary file"

echo "-- 8. a baseline with no tolerance is an ERROR: the tolerance must be a"
echo "      recorded measurement, never an implicit default"
sed '/branch_tolerance/d' "$TMP/base.txt" > "$TMP/notol.txt"
run "$RATCHET" "$TMP/at.txt" "$TMP/notol.txt" && rc=0 || rc=$?
ok "missing tolerance is an error" 2 "$rc" "measured, recorded number"

echo "-- 9. LINE coverage collapsing does NOT fail the gate (branch is the metric)"
run "$RATCHET" "$TMP/lowline.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "line coverage never gates" 0 "$rc" "REPORTED, NEVER GATED"

echo "-- 10. a RISEN number passes and says to raise the baseline"
run "$RATCHET" "$TMP/high.txt" "$TMP/base.txt" && rc=0 || rc=$?
ok "risen coverage advises a raise" 0 "$rc" "RISEN"

# ---------------------------------------------------------------------------
# 11. The coverage SCRIPTS must be free of undefined-function and syntax errors.
#
# `bash -n` cannot see a function used before it is defined -- it is a runtime
# NameError equivalent, and this exact break shipped into a 20-minute run: three
# concurrent coverage runs all died with
#
#	run_coverage.sh: line 506: cov_pkill: command not found
#
# twenty minutes in, at the first cleanup call, because the helper had been
# appended AFTER its first use.  Twenty minutes to learn a one-line fact.
#
# So: parse each script, find every shell function it defines, and require every
# call of a cov_*/phase* helper to appear textually after its definition.  Cheap,
# and it fails in milliseconds instead of after a build.
# ---------------------------------------------------------------------------
echo "-- 11. coverage scripts: helpers defined before first use, syntax clean"
for f in "$HERE/run_coverage.sh" "$HERE/run_coverage_parallel.sh" \
         "$HERE/ratchet.sh"; do
	b=$(basename "$f")
	checks=$((checks + 1))
	if ! sh -n "$f" 2>"$TMP/syn.txt" && ! bash -n "$f" 2>"$TMP/syn.txt"; then
		echo "FAIL  $b: syntax error"
		sed -n 's/^/        /p' "$TMP/syn.txt"
		fails=$((fails + 1))
		continue
	fi
	# For each `name() {` definition, the line it is defined on; for each call
	# of that name, the first line it is called on.  A call before the
	# definition is the failure this check exists for.
	bad=$(awk '
	/^[a-z_][a-z0-9_]*\(\)[ \t]*\{/ {
		name = $0
		sub(/\(\).*/, "", name)
		def[name] = NR
		next
	}
	{
		line = $0
		sub(/#.*/, "", line)
		for (n in def) continue
	}
	{ all[NR] = $0 }
	END {
		for (n in def)
			for (i = 1; i < def[n]; i++) {
				l = all[i]
				sub(/#.*/, "", l)
				if (l ~ ("(^|[ \t;&|(])" n "([ \t;&|)]|$)"))
					printf "%s called at line %d, defined at %d\n", n, i, def[n]
			}
	}' "$f")
	if [ -n "$bad" ]; then
		echo "FAIL  $b: helper called before it is defined"
		printf '%s\n' "$bad" | sed -n 's/^/        /p'
		fails=$((fails + 1))
	else
		echo "ok    $b (syntax clean, helpers defined before use)"
	fi
done

echo
if [ "$fails" = 0 ]; then
	echo "ratchet self-check: ALL OK ($checks checks)"
	exit 0
fi
echo "ratchet self-check: $fails of $checks checks FAILED"
exit 1

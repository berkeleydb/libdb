#!/bin/sh
# test/check_manifest.sh -- the test-execution manifest gate.
#
# Diffs test/MANIFEST (what is EXPECTED to run) against the RESULT lines the
# tier runners emitted (what ACTUALLY ran) and fails on any of:
#
#   MISSING       a mandatory manifest entry produced no verdict line.
#                 THE VACUOUS-GREEN CASE.  This is the whole reason the file
#                 exists: four times in one release cycle a test was not run
#                 and the suite still said success.
#   FAILED        a verdict line said `fail`.
#   EMPTY         a tier produced a results file with zero verdicts (a runner
#                 that exited 0 having run nothing -- the lost-exec-bit and
#                 mkdir-failed shapes).
#   UNDECLARED    a verdict for a test that is not in the manifest.  Reported,
#                 and fatal in --strict mode only: a new test should be added
#                 to the manifest, but forgetting to is not a correctness risk
#                 the way a missing run is.
#
# Usage:
#   test/check_manifest.sh [RESULTS_DIR]      # default: test/.results
#   test/check_manifest.sh --strict [DIR]     # UNDECLARED is fatal too
#   test/check_manifest.sh --tier T [DIR]     # only check tier T
#
# A tier with NO results file at all is reported as NOT RUN and is fatal only
# if it has mandatory entries -- unless --tier named it, in which case the
# absence is always fatal (that is how CI asserts a tier it just invoked
# actually produced evidence).
#
# Exit: 0 = every expectation met, 1 = a gate condition fired, 2 = usage.

set -u

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
MANIFEST=${LIBDB_MANIFEST:-"$HERE/MANIFEST"}

strict=0
only_tier=''
results=''
while [ $# -gt 0 ]; do
	case $1 in
	--strict) strict=1 ;;
	--tier) shift; [ $# -gt 0 ] || { echo "--tier needs a value" >&2; exit 2; }
		only_tier=$1 ;;
	-h|--help) sed -n '2,32p' "$0"; exit 0 ;;
	-*) echo "unknown option: $1" >&2; exit 2 ;;
	*) results=$1 ;;
	esac
	shift
done
results=${results:-${LIBDB_RESULTS_DIR:-"$HERE/.results"}}

[ -f "$MANIFEST" ] || { echo "check_manifest: no manifest at $MANIFEST" >&2; exit 2; }

# Collect the verdicts.  Every tier's file is concatenated; a RESULT line is
# self-describing (it names its own tier) so this is order-independent, and a
# stray file cannot silently satisfy another tier's expectation.
verdicts=$(mktemp) || exit 2
present=$(mktemp) || exit 2
trap 'rm -f "$verdicts" "$present"' 0 1 2 3 13 15

if [ -d "$results" ]; then
	for f in "$results"/*.results; do
		[ -f "$f" ] || continue
		t=$(basename "$f" .results)
		printf '%s\n' "$t" >> "$present"
		# Keep only well-formed RESULT lines; anything else is noise a
		# runner leaked into the file and must not become evidence.
		awk '$1 == "RESULT" && NF == 4 && $4 ~ /^(pass|fail|skip)$/ \
		    { print $2, $3, $4 }' "$f" >> "$verdicts"
	done
fi

echo "== test-execution manifest gate =="
echo "manifest: $MANIFEST"
echo "results:  $results"
echo "tiers with a results file: $(sort -u "$present" 2>/dev/null | tr '\n' ' ')"
echo "verdict lines: $(wc -l < "$verdicts" | tr -d ' ')"
echo

rc=0

# ---------------------------------------------------------------------------
# EMPTY: a results file exists but holds no verdict.  hi_init() truncates the
# file when a runner starts, so this is precisely "the runner started and
# produced no evidence" -- exit status 0 included.
# ---------------------------------------------------------------------------
if [ -s "$present" ]; then
	while read -r t; do
		[ -n "${only_tier:-}" ] && [ "$t" != "$only_tier" ] && continue
		n=$(awk -v t="$t" '$1 == t' "$verdicts" | wc -l | tr -d ' ')
		if [ "$n" -eq 0 ]; then
			echo "EMPTY      $t: the runner produced a results file with ZERO verdicts."
			echo "           A runner that exits 0 having run nothing is not a pass."
			rc=1
		fi
	done < "$present"
fi

# ---------------------------------------------------------------------------
# FAILED: an executed test reported failure.
# ---------------------------------------------------------------------------
while read -r t n v; do
	[ "$v" = fail ] || continue
	[ -n "${only_tier:-}" ] && [ "$t" != "$only_tier" ] && continue
	echo "FAILED     $t $n"
	rc=1
done < "$verdicts"

# ---------------------------------------------------------------------------
# MISSING: the vacuous-green gate.
#
# An entry marked `optional` is excused ONLY when its whole tier is absent.
# Once a tier has produced any verdict it is running, and a partial set then
# means something inside it was skipped -- which is trap 1 and trap 2 exactly.
# ---------------------------------------------------------------------------
notrun=''
while read -r t n opt; do
	case $t in ''|'#'*) continue ;; esac
	[ -n "${only_tier:-}" ] && [ "$t" != "$only_tier" ] && continue

	if ! grep -qx "$t" "$present" 2>/dev/null; then
		# Whole tier absent.  Fatal unless every entry is optional, or
		# --tier was NOT used to assert it.
		case " $notrun " in *" $t "*) ;; *) notrun="$notrun $t" ;; esac
		continue
	fi
	if ! awk -v t="$t" -v n="$n" '$1 == t && $2 == n { found = 1 }
	    END { exit !found }' "$verdicts"; then
		if [ "${opt:-}" = optional ]; then
			echo "missing    $t $n (optional; tier ran but this did not)"
			echo "           -- not fatal, but nothing proved it ran either."
		else
			echo "MISSING    $t $n: NO VERDICT LINE."
			echo "           The manifest says this test runs.  Nothing"
			echo "           reported running it.  That is a vacuous green:"
			echo "           the tier's exit status cannot tell 'passed'"
			echo "           from 'never executed'."
			rc=1
		fi
	fi
done < "$MANIFEST"

for t in $notrun; do
	# --tier T means "CI just ran T, assert it produced evidence".  An absent
	# results file then means the runner produced NOTHING, which is fatal
	# regardless of whether the entries are optional: `optional` excuses a
	# tier that was not SCHEDULED, not one that was scheduled and silent.
	if [ -n "${only_tier:-}" ]; then
		echo "NOT RUN    $t: asserted with --tier but produced no results file."
		echo "           The runner did not even start (lost exec bit, build"
		echo "           failure, wrong path?).  That is not a pass."
		rc=1
	elif awk -v t="$t" '$1 == t && $3 != "optional" && $1 !~ /^#/ { found = 1 }
	    END { exit !found }' "$MANIFEST"; then
		echo "not run    $t: no results file (tier not scheduled on this trigger)."
	else
		echo "not run    $t: no results file (all entries optional)."
	fi
done

# ---------------------------------------------------------------------------
# UNDECLARED: ran but not in the manifest.
# ---------------------------------------------------------------------------
while read -r t n v; do
	[ -n "${only_tier:-}" ] && [ "$t" != "$only_tier" ] && continue
	if ! awk -v t="$t" -v n="$n" '$1 == t && $2 == n { found = 1 }
	    END { exit !found }' "$MANIFEST"; then
		echo "UNDECLARED $t $n ($v): ran but is not in the manifest -- add it."
		[ "$strict" = 1 ] && rc=1
	fi
done < "$verdicts"

echo
if [ "$rc" = 0 ]; then
	echo "manifest gate: OK"
else
	echo "manifest gate: FAILED -- see the lines above."
	echo "A MISSING/EMPTY line means a test the manifest promises did not run."
	echo "Do NOT delete the manifest entry to make this pass; make the test run."
fi
exit "$rc"

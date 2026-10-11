#!/bin/sh
# test/db/run_all.sh -- run the test/db single-bug regression runners and emit
# verdicts for the test-execution manifest gate (test/MANIFEST).
#
# Replaces the inline `for r in ...` loop that used to live in ci.yml.  The list
# is READ FROM THE MANIFEST rather than repeated here, because a duplicated list
# is exactly what produced traps 1 and 3 (LEAK_TESTS vs leak-run.sh, and $subs
# vs the ssi test files): two copies of one list drift, and the drift is silent.
#
# Reading the list from the manifest means this tier cannot be "short" of the
# manifest -- instead a manifest entry with no matching script becomes a FAIL
# verdict, which is a louder and more accurate signal than a missing line.
#
# Usage (from build_unix):  sh ../test/db/run_all.sh
# Env: BUILD (default .), TIMEOUT, TIER (default db; "typed" runs the RFC 0014
#      runners, which need an --enable-typed build and FAIL rather than skip
#      without one)

set -u

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$HERE/../harness.sh"
TIER=${TIER:-db}
export TIER	# runners registered in two tiers read it
hi_init "$TIER" "$HERE/.."

MANIFEST=${LIBDB_MANIFEST:-"$HERE/../MANIFEST"}
rc=0

# awk, not grep|cut: the manifest is whitespace-separated with comments.
runners=$(awk -v tier="$TIER" '$1 == tier && $1 !~ /^#/ { print $2 }' "$MANIFEST")
[ -n "$runners" ] || {
	echo "run_all.sh: no $TIER entries in $MANIFEST" >&2
	exit 1
}

for r in $runners; do
	echo "::group::$r"
	if [ ! -f "$HERE/$r.sh" ]; then
		# A manifest entry naming a script that does not exist.  Fail
		# loudly: this is the "expected to run, cannot run" case.
		echo "run_all.sh: $HERE/$r.sh does not exist"
		hi_emit "$r" fail
		rc=1
		echo "::endgroup::"
		continue
	fi
	if sh "$HERE/$r.sh"; then
		hi_emit "$r" pass
	else
		echo "run_all.sh: $r FAILED (exit $?)"
		hi_emit "$r" fail
		rc=1
	fi
	echo "::endgroup::"
done
exit "$rc"

#!/bin/sh
# test/sim/run_capstones.sh -- run the DST fault-class capstones and emit
# verdicts for the test-execution manifest gate (test/MANIFEST).
#
# Replaces the inline `for t in ...` loop that used to live in ci.yml's dst job.
# The list comes from the manifest, so the workflow and the expectation cannot
# drift; and each capstone gets its own verdict, so one that stopped being built
# or stopped being listed shows up as a MISSING line instead of vanishing.
#
# The old loop used `set -e`, so the FIRST failure ended the run and the
# remaining capstones produced no signal at all -- indistinguishable from never
# having been listed. This runs all of them and reports each.
#
# Usage (from build_unix):  sh ../test/sim/run_capstones.sh
# Env: SIM_TIMEOUT (per capstone, default 240)

set -u

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$HERE/../harness.sh"
hi_init sim "$HERE/.."

MANIFEST=${LIBDB_MANIFEST:-"$HERE/../MANIFEST"}
SIM_TIMEOUT=${SIM_TIMEOUT:-240}

tests=$(awk '$1 == "sim" && $1 !~ /^#/ { print $2 }' "$MANIFEST")
[ -n "$tests" ] || { echo "run_capstones.sh: no sim entries in $MANIFEST" >&2; exit 1; }

rc=0
for t in $tests; do
	echo "--- $t ---"
	# A build failure is a FAIL verdict, not a silent skip: a capstone that
	# no longer compiles is exactly a test that stopped running.
	if ! make "$t"; then
		echo "run_capstones.sh: $t did not BUILD"
		hi_emit "$t" fail
		rc=1
		continue
	fi
	if timeout "$SIM_TIMEOUT" "./$t"; then
		hi_emit "$t" pass
	else
		echo "run_capstones.sh: $t FAILED (exit $?)"
		hi_emit "$t" fail
		rc=1
	fi
done
exit "$rc"

#!/bin/sh
#-
# Deterministic Simulation Testing (DST) for libdb.
#
# dst-sweep.sh --
#	Swarm runner: run one DST scenario over a seed range and report the
#	pass count plus per-fault activation stats parsed from the scenario
#	output.  A seed that FAILS is printed (it is a reproducer: re-run
#	`./<scenario> <seed>` to replay the exact failing run).
#
#	Usage:  dst-sweep.sh <scenario> [SEED_LO] [SEED_HI]
#	   e.g.  scripts/dst-sweep.sh test_sim_crash_recover 1 200
#
#	Run from the build directory (build_unix) after `make dst_tests`.

set -u

SCEN="${1:?usage: dst-sweep.sh <scenario> [lo] [hi]}"
LO="${2:-1}"
HI="${3:-100}"

if [ ! -x "./$SCEN" ]; then
	echo "dst-sweep: ./$SCEN not built (run: make $SCEN)" >&2
	exit 2
fi

pass=0
fail=0
fails=""
s="$LO"
while [ "$s" -le "$HI" ]; do
	if ./"$SCEN" "$s" >/dev/null 2>&1; then
		pass=$((pass + 1))
	else
		fail=$((fail + 1))
		fails="$fails $s"
	fi
	s=$((s + 1))
done

total=$((HI - LO + 1))
echo "dst-sweep: $SCEN  seeds [$LO..$HI]  ->  $pass/$total pass, $fail fail"
if [ -n "$fails" ]; then
	echo "  FAILING seeds (reproduce with ./$SCEN <seed>):$fails"
	exit 1
fi
exit 0

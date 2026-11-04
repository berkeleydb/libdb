#!/bin/sh -
#
# $Id$
#
# run_rep_cmpvote_order.sh --
#	Regression gate for the __rep_cmp_vote election comparator
#	(src/rep/rep_elect.c).  The comparator must be a TOTAL ORDER so every
#	site that tallies the same VOTE1 set agrees on the winner, and so the
#	winner has the most recent data (maximum data_gen).  The old comparator
#	was neither: it special-cased the first vote and let its last arm (equal
#	LSN => priority/tiebreaker) bypass data_gen, so a stale (lower-data_gen)
#	site with an equal LSN and a higher priority/tiebreaker could be elected
#	over a newer site -- rolling back a durable commit on sync.
#
#	The precondition (equal last-commit LSN, different data_gen) was shown
#	reachable against the live engine (test/rep/E1-FINDINGS.md); this gate
#	drives the REAL comparator (a verbatim copy, drift-checked) over those
#	captured vote vectors in every arrival order.
#
#	Two directions, so the gate has teeth:
#	  - the fixed comparator PASSES (order-independent, data_gen-maximal);
#	  - the pre-fix comparator (-DCMPVOTE_OLD) MUST FAIL.
#
# Exits non-zero on failure.

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/rep_cmpvote_order.c}
CC=${CC:-cc}
TMP=${TMP:-rep_cmpvote_order_tmp}

mkdir -p "$TMP"

echo "Compiling rep_cmpvote_order (fixed comparator)"
"$CC" -g -O1 ${CFLAGS:-} "$SRC" -o "$TMP/cmpvote_fixed"

echo "Compiling rep_cmpvote_order (pre-fix comparator, sabotage)"
"$CC" -g -O1 ${CFLAGS:-} -DCMPVOTE_OLD "$SRC" -o "$TMP/cmpvote_old"

rc=0

echo "--- fixed comparator (must PASS) ---"
if "$TMP/cmpvote_fixed"; then
	echo "fixed: PASS"
else
	echo "FAIL: the fixed comparator did not pass"
	rc=1
fi

echo "--- pre-fix comparator (must FAIL = teeth) ---"
if "$TMP/cmpvote_old"; then
	echo "FAIL: the pre-fix comparator PASSED -- the gate has no teeth"
	rc=1
else
	echo "pre-fix: FAILED as required (bug reproduced)"
fi

# Verify the embedded comparator is still a verbatim copy of src/.
CHK="$_HERE/check_rep_cmpvote_copy.sh"
if [ -f "$CHK" ]; then
	echo "--- verbatim-copy drift check ---"
	if sh "$CHK"; then
		echo "copy-check: ok"
	else
		echo "FAIL: embedded __rep_cmp_vote drifted from src/rep/rep_elect.c"
		rc=1
	fi
fi

rm -rf "$TMP"

if [ "$rc" -eq 0 ]; then
	echo "run_rep_cmpvote_order.sh: PASS"
else
	echo "run_rep_cmpvote_order.sh: FAIL"
fi
exit "$rc"

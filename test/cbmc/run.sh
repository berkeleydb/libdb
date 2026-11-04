#!/usr/bin/env bash
#
# test/cbmc/run.sh -- run every CBMC harness and report PASS/FAIL.
#
# Each harness formally verifies a self-contained algorithmic core of libdb
# against ALL inputs within a stated bound.  Run inside the nix dev shell:
#
#   nix develop /path/to/libdb --command bash test/cbmc/run.sh
#
# or, from a checkout:
#
#   cd test/cbmc && nix develop .. --command bash run.sh
#
# Exit non-zero if any harness that is expected to VERIFY does not.
set -u

cd "$(dirname "$0")"
CBMC=${CBMC:-cbmc}
# --bounds-check + --pointer-check are the memory-safety core.  We do NOT add
# --conversion-check globally: the harnesses deliberately narrow nondet ints
# to bytes (e.g. (u_int8_t)nondet_u32()) to fill buffers, which is not a
# property under test.  The varint harness enables it explicitly (see below).
COMMON="--bounds-check --pointer-check"
FAIL=0

# run <name> <file> <function> <extra cbmc args...>
run() {
	local name=$1 file=$2 func=$3; shift 3
	printf '=== %-14s ' "$name"
	local rc secs tmp
	tmp=$(mktemp)
	SECONDS=0
	"$CBMC" "$file" -Istubs --function "$func" $COMMON "$@" >"$tmp" 2>&1
	rc=$?
	secs=$SECONDS
	if grep -q "VERIFICATION SUCCESSFUL" "$tmp"; then
		printf 'PASS  (%ss)\n' "$secs"
	else
		printf 'FAIL  (%ss)\n' "$secs"
		grep -E ": FAILURE" "$tmp" | head -8 | sed 's/^/        /'
		FAIL=1
	fi
	rm -f "$tmp"
}

# --- harnesses expected to VERIFY on the current (correct) code ---
run varint    harness_varint.c   harness  --conversion-check                # loop-free, full uint64
run swap      harness_swap.c     harness                                   # loop-free
run hash4     harness_hash4.c    harness  --unwind 9  --unwinding-assertions
run getlong   harness_getlong.c  harness_long   --unwind 8
run getulong  harness_getlong.c  harness_ulong  --unwind 8
run dd_find   harness_dd_find.c  harness  --unwind 6  --unwinding-assertions
run cmpvote   harness_cmpvote.c  harness                                     # H1 (b,c): loop-free comparator
run cmpvote-d harness_cmpvote.c  harness_likepri                            # H1 (d): same-data_gen lex order
run tally     harness_tally.c    harness  --unwind 8  --unwinding-assertions  # H2 tally array
run maj-arith harness_majority.c harness_arith     --conversion-check          # H3 quorum arithmetic
run maj-inter harness_majority.c harness_intersect --unwind 9 --unwinding-assertions  # H3 intersection

# okitem proves the safety guarantee on the real (parenthesised) HPAGE_PTYPE.
# We drop --pointer-check here (keeping --bounds-check + __CPROVER_r_ok, the
# read-region safety predicate) because okitem deliberately forms a past-the-end
# pointer for its bounds math, which --pointer-check flags as a formation
# artifact.
printf '=== %-14s ' "okitem"
SECONDS=0
tmp=$(mktemp)
"$CBMC" harness_okitem.c -Istubs --function harness \
	--bounds-check --no-pointer-check --unwind 65 >"$tmp" 2>&1
secs=$SECONDS
if grep -q "VERIFICATION SUCCESSFUL" "$tmp"; then
	printf 'PASS  (%ss)\n' "$secs"
else
	printf 'FAIL  (%ss)\n' "$secs"; grep ": FAILURE" "$tmp" | head; FAIL=1
fi
rm -f "$tmp"

# Sabotage arms: each plants a real bug shape and MUST fail.  A sabotage arm
# that verifies means its harness has no teeth.
# sabotage <name> <file> <function> <cbmc args...>
sabotage() {
	local name=$1 file=$2 func=$3; shift 3
	printf '=== %-14s ' "$name"
	local tmp; tmp=$(mktemp); SECONDS=0
	"$CBMC" "$file" -Istubs --function "$func" "$@" >"$tmp" 2>&1
	if grep -q "VERIFICATION FAILED" "$tmp"; then
		printf 'FAIL as required (%ss)\n' "$SECONDS"
	else
		printf 'NOT CAUGHT (%ss)  [the harness has no teeth]\n' "$SECONDS"
		FAIL=1
	fi
	rm -f "$tmp"
}
# The pre-2ca81268a HPAGE_PTYPE (no parentheses): the real bug okitem found.
sabotage okitem-sab harness_okitem.c harness -DHPAGE_PTYPE_SABOTAGE \
	--bounds-check --no-pointer-check --unwind 65
# H1: swapping LOG_COMPARE's order arms breaks winner-maximises-LSN (d).
sabotage cmpvote-sab harness_cmpvote.c harness_likepri -DCMPVOTE_SABOTAGE \
	--bounds-check --pointer-check
# H2: == -> != on the "seen this eid" test double-counts a site (OOB + count).
sabotage tally-sab harness_tally.c harness -DTALLY_SABOTAGE \
	--bounds-check --pointer-check --unwind 8 --unwinding-assertions
# H3: majority = n/2 (floor) instead of n/2+1 breaks intersection + warning.
sabotage maj-arith-sab harness_majority.c harness_arith -DMAJORITY_SABOTAGE \
	--bounds-check --pointer-check --conversion-check
sabotage maj-inter-sab harness_majority.c harness_intersect -DMAJORITY_SABOTAGE \
	--bounds-check --pointer-check --unwind 9 --unwinding-assertions

# Every verbatim copy must still match src/.
printf '=== %-14s ' "copies"
if sh ./check_copies.sh > /tmp/cbmc-copies.$$ 2>&1; then
	printf 'PASS  (%s copies match src/)\n' "$(grep -c '^ok' /tmp/cbmc-copies.$$)"
else
	printf 'FAIL\n'; grep -v '^ok' /tmp/cbmc-copies.$$ | sed 's/^/        /' | head -20; FAIL=1
fi
rm -f /tmp/cbmc-copies.$$

echo
if [ $FAIL -eq 0 ]; then
	echo "ALL HARNESSES OK (every harness verified, every sabotage arm failed, every copy matches src/)"
else
	echo "SOME HARNESSES FAILED"
fi
exit $FAIL

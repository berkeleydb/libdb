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

# okitem: the FIXED-macro build proves the safety guarantee holds.  We drop
# --pointer-check here (keeping --bounds-check + __CPROVER_r_ok, the read-region
# safety predicate) because okitem deliberately forms a past-the-end pointer
# for its bounds math, which --pointer-check flags as a formation artifact.
printf '=== %-14s ' "okitem(FIXED)"
SECONDS=0
tmp=$(mktemp)
"$CBMC" harness_okitem.c -Istubs -DHPAGE_PTYPE_FIXED --function harness \
	--bounds-check --no-pointer-check --unwind 65 >"$tmp" 2>&1; rc=$?
secs=$SECONDS
if grep -q "VERIFICATION SUCCESSFUL" "$tmp"; then
	printf 'PASS  (%ss)  [proves the safety guarantee + the 1-line fix]\n' "$secs"
else
	printf 'FAIL  (%ss)\n' "$secs"; grep ": FAILURE" "$tmp" | head; FAIL=1
fi
rm -f "$tmp"

# okitem on the UNMODIFIED engine code: expected to FAIL -- this IS the real
# bug (see README.md "BUG FOUND").  A PASS here would mean the bug was fixed
# upstream; flip the expectation then.
printf '=== %-14s ' "okitem(REAL)"
SECONDS=0
tmp=$(mktemp)
"$CBMC" harness_okitem.c -Istubs --function harness \
	--bounds-check --no-pointer-check --unwind 65 >"$tmp" 2>&1
secs=$SECONDS
if grep -q "VERIFICATION FAILED" "$tmp"; then
	printf 'FAIL as expected (%ss)  [reproduces the real okitem OOB bug]\n' "$secs"
else
	printf 'UNEXPECTED PASS (%ss)  [okitem bug appears fixed -- update run.sh]\n' "$secs"
	FAIL=1
fi
rm -f "$tmp"

echo
if [ $FAIL -eq 0 ]; then
	echo "ALL HARNESSES OK (verifying harnesses passed; okitem reproduces its bug)"
else
	echo "SOME HARNESSES FAILED"
fi
exit $FAIL

#!/bin/sh
# test/check_manifest_test.sh -- self-check for test/check_manifest.sh.
#
# The checker is the thing that decides whether the whole suite is trustworthy,
# so it needs its own test: a checker that passes everything is itself the
# vacuous green.  Runs against a synthetic manifest + synthetic results, so it
# needs no libdb build and takes under a second.
#
# Usage: sh test/check_manifest_test.sh

set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
CHECK="$HERE/check_manifest.sh"
TD=$(mktemp -d) || exit 1
trap 'find "$TD" -mindepth 1 -delete 2>/dev/null; rmdir "$TD" 2>/dev/null' \
    0 1 2 3 13 15

fails=0
# case NAME WANT_RC -- run the checker over $TD/results with $TD/MANIFEST.
case_is() {
	_name=$1; _want=$2
	LIBDB_MANIFEST="$TD/MANIFEST" sh "$CHECK" "$TD/results" > "$TD/out" 2>&1
	_got=$?
	if [ "$_got" = "$_want" ]; then
		printf 'ok    %-42s (rc=%s)\n' "$_name" "$_got"
	else
		printf 'FAIL  %-42s (rc=%s, want %s)\n' "$_name" "$_got" "$_want"
		sed 's/^/        /' "$TD/out"
		fails=$((fails + 1))
	fi
}
# grep the last run's output.
said() {
	if grep -q "$1" "$TD/out"; then
		printf 'ok    output contains %s\n' "$1"
	else
		printf 'FAIL  output lacks %s\n' "$1"
		sed 's/^/        /' "$TD/out"
		fails=$((fails + 1))
	fi
}

mkdir -p "$TD/results"
cat > "$TD/MANIFEST" <<'EOF'
# comment line must be ignored
alpha	a_one
alpha	a_two
beta	b_one	optional
EOF

reset() { find "$TD/results" -mindepth 1 -delete 2>/dev/null; }

echo "-- 1. everything expected ran and passed => 0"
reset
printf 'RESULT alpha a_one pass\nRESULT alpha a_two pass\n' > "$TD/results/alpha.results"
case_is "all present" 0

echo "-- 2. a mandatory entry with NO verdict => 1 (THE VACUOUS-GREEN CASE)"
reset
printf 'RESULT alpha a_one pass\n' > "$TD/results/alpha.results"
case_is "missing mandatory verdict" 1
said "MISSING"

echo "-- 3. an executed test that FAILED => 1"
reset
printf 'RESULT alpha a_one pass\nRESULT alpha a_two fail\n' > "$TD/results/alpha.results"
case_is "explicit fail verdict" 1
said "FAILED"

echo "-- 4. results file exists but is EMPTY => 1 (runner exited 0, ran nothing)"
reset
: > "$TD/results/alpha.results"
case_is "empty results file" 1
said "EMPTY"

echo "-- 5. tier absent entirely, all-optional tier => 0 for that tier alone"
reset
printf 'RESULT alpha a_one pass\nRESULT alpha a_two pass\n' > "$TD/results/alpha.results"
case_is "optional tier absent is excused" 0

echo "-- 6. optional entry missing WHILE ITS TIER RAN => 0, but reported"
reset
printf 'RESULT alpha a_one pass\nRESULT alpha a_two pass\n' > "$TD/results/alpha.results"
: > "$TD/results/beta.results"
# beta's file is empty, so EMPTY fires; that is correct and separately tested.
# Give beta a DIFFERENT verdict so the tier is non-empty but b_one is absent.
printf 'RESULT beta b_other pass\n' > "$TD/results/beta.results"
case_is "optional missing while tier ran" 0
said "optional; tier ran"

echo "-- 7. UNDECLARED is reported, non-fatal by default, fatal with --strict"
reset
printf 'RESULT alpha a_one pass\nRESULT alpha a_two pass\nRESULT alpha a_extra pass\n' \
    > "$TD/results/alpha.results"
case_is "undeclared non-fatal by default" 0
said "UNDECLARED"
LIBDB_MANIFEST="$TD/MANIFEST" sh "$CHECK" --strict "$TD/results" > "$TD/out" 2>&1
if [ $? = 1 ]; then echo "ok    --strict makes UNDECLARED fatal"
else echo "FAIL  --strict did not fail on UNDECLARED"; fails=$((fails + 1)); fi

echo "-- 8. --tier makes an ABSENT tier fatal (CI just ran it: no file = no evidence)"
reset
printf 'RESULT alpha a_one pass\nRESULT alpha a_two pass\n' > "$TD/results/alpha.results"
LIBDB_MANIFEST="$TD/MANIFEST" sh "$CHECK" --tier beta "$TD/results" > "$TD/out" 2>&1
if [ $? = 1 ]; then echo "ok    --tier beta fails when beta produced nothing"
else echo "FAIL  --tier did not fail on an absent tier"; fails=$((fails + 1)); fi
said "NOT RUN"

echo "-- 9. a malformed line is NOT accepted as evidence"
reset
printf 'RESULT alpha a_one pass\nRESULT alpha a_two bogusverdict\n' \
    > "$TD/results/alpha.results"
case_is "malformed verdict rejected" 1
said "MISSING    alpha a_two"

echo "-- 10. no results directory at all => mandatory tiers reported"
reset
rmdir "$TD/results" 2>/dev/null
case_is "no results dir" 0
mkdir -p "$TD/results"

echo
if [ "$fails" = 0 ]; then
	echo "check_manifest self-check: ALL OK"
	exit 0
fi
echo "check_manifest self-check: $fails FAILURE(S)"
exit 1

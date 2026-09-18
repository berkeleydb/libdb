#!/bin/sh -
#
# $Id$
#
# run_db_verify_multifile.sh --
#	Regression gate for issue #165 -- db_verify given several files verified
#	the FIRST file repeatedly.
#
#	util/db_verify.c set fname/dname once from argv[0] before the per-file
#	loop, and the loop did ++argv without re-reading them, so every
#	iteration re-verified the first file while the message at the bottom of
#	the loop printed the current argv[0].  The output looked correct while
#	the work was wrong: `db_verify good.db corrupt.db` reported BOTH as
#	succeeded and exited 0.  Present identically in upstream 5.3.28.
#
#	Anyone running `db_verify *.db` therefore got a false clean bill of
#	health for every file after the first -- a silent failure of the one
#	tool whose entire purpose is detecting corruption.
#
#	This asserts both directions, because a checker that reports success on
#	everything also "passes" a good-files-only test:
#	  1. all-good files      -> every line succeeded, exit 0
#	  2. one corrupt in the middle -> that file (and only it) reported
#	     failed, exit non-zero
#	  3. the corrupt file first -> still detected (order independence)
#
# Exits non-zero on failure or hang.

set -eu

BUILD=${BUILD:-.}
HOME_DIR=${HOME_DIR:-DB_VERIFY_MULTI_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

VERIFY="$BUILD/db_verify"
LOAD="$BUILD/db_load"
[ -x "$VERIFY" ] || { echo "FAIL: $VERIFY missing"; exit 1; }
[ -x "$LOAD" ] || { echo "FAIL: $LOAD missing"; exit 1; }
VERIFY=$(cd "$(dirname "$VERIFY")" && pwd)/$(basename "$VERIFY")
LOAD=$(cd "$(dirname "$LOAD")" && pwd)/$(basename "$LOAD")

# `timeout` is GNU coreutils: absent on stock macOS (gtimeout with coreutils).
# If neither exists, run without one rather than failing with rc=127.
if command -v timeout >/dev/null 2>&1; then
	TIMEOUT_CMD="timeout"
elif command -v gtimeout >/dev/null 2>&1; then
	TIMEOUT_CMD="gtimeout"
else
	TIMEOUT_CMD=""
fi
run_with_timeout() {
	if [ -n "$TIMEOUT_CMD" ]; then
		"$TIMEOUT_CMD" "$@"
	else
		shift
		"$@"
	fi
}

PYTHON=${PYTHON:-python3}
command -v "$PYTHON" >/dev/null 2>&1 || \
    { echo "FAIL: $PYTHON needed to corrupt a page"; exit 1; }

rm -f "$HOME_DIR"/*.db 2>/dev/null || true
mkdir -p "$HOME_DIR"
cd "$HOME_DIR"

fail=0
note() { echo "  $*"; }

# Three distinct, valid btrees.
for n in a b c; do
	printf 'k%s\nv%s\n' "$n" "$n" | "$LOAD" -T -t btree "$n.db" >/dev/null
done
note "built a.db b.db c.db"

# --- Case 1: all good.  Every file must be reported, all succeeded.
out=$(run_with_timeout "$TIMEOUT" "$VERIFY" a.db b.db c.db 2>&1) && rc=0 || rc=$?
ok=$(printf '%s\n' "$out" | grep -c 'succeeded' || true)
note "all-good:      exit=$rc succeeded_lines=$ok"
[ "$rc" -eq 0 ] || { echo "  FAIL: all-good run exited $rc, expected 0"; fail=1; }
[ "$ok" -eq 3 ] || { echo "  FAIL: expected 3 'succeeded' lines, got $ok"; fail=1; }

# --- Case 2: corrupt the MIDDLE file.  Only it may be reported failed, and the
# exit status must be non-zero.  Before the fix this printed three 'succeeded'
# lines and exited 0.
"$PYTHON" - <<'PY'
d = bytearray(open("b.db", "rb").read())
d[25] = 255                     # page type byte -> invalid
open("b.db", "wb").write(bytes(d))
PY
note "corrupted b.db page type -> 255"

out=$(run_with_timeout "$TIMEOUT" "$VERIFY" a.db b.db c.db 2>&1) && rc=0 || rc=$?
bad=$(printf '%s\n' "$out" | grep -c 'failed' || true)
note "one-corrupt:   exit=$rc failed_lines=$bad"
printf '%s\n' "$out" | grep -E 'succeeded|failed' | sed 's/^/    /'

[ "$rc" -ne 0 ] || { echo "  FAIL: a corrupt file was present but db_verify exited 0 -- files after the first are not being verified"; fail=1; }
printf '%s\n' "$out" | grep -q 'b.db.*failed' || \
    { echo "  FAIL: b.db is corrupt but was not reported as failed"; fail=1; }
printf '%s\n' "$out" | grep -q 'a.db.*succeeded' || \
    { echo "  FAIL: a.db is valid but was not reported as succeeded"; fail=1; }
printf '%s\n' "$out" | grep -q 'c.db.*succeeded' || \
    { echo "  FAIL: c.db is valid but was not reported as succeeded -- the last file is being mis-verified"; fail=1; }

# --- Case 3: order independence.  The corrupt file first must still be caught,
# and the good files after it must still be verified as themselves.
out=$(run_with_timeout "$TIMEOUT" "$VERIFY" b.db a.db c.db 2>&1) && rc=0 || rc=$?
note "corrupt-first: exit=$rc"
[ "$rc" -ne 0 ] || { echo "  FAIL: corrupt-first run exited 0"; fail=1; }
printf '%s\n' "$out" | grep -q 'a.db.*succeeded' || \
    { echo "  FAIL: a.db not verified correctly when it follows a corrupt file"; fail=1; }
printf '%s\n' "$out" | grep -q 'c.db.*succeeded' || \
    { echo "  FAIL: c.db not verified correctly when it follows a corrupt file"; fail=1; }

if [ "$fail" -eq 0 ]; then
	echo "run_db_verify_multifile.sh: PASS"
	exit 0
fi
echo "run_db_verify_multifile.sh: FAIL"
exit 1

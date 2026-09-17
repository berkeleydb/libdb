#!/bin/sh
#
# opt_teeth.sh -- the teeth gate for RFC 0007 phase 1 (optimistic read
# validation).  Runs test/bench/opt_fires three ways and asserts the VERDICT
# lines.  Exit 0 only if all three verdicts appear; "rc=0" from the binary is
# never trusted on its own.
#
#   1. optimistic ON   -> the path must RUN (pages > 0) and validation must
#                         FIRE (invalid > 0), with zero wrong answers.
#   2. DB_NO_OPTREAD=1 -> the path must be entirely INERT (tries == 0).
#   3. sabotaged build -> a library whose BH_GEN_EXCL_ENTER never bumps the
#                         generation must FAIL arm 1.  Without this arm, arm 1
#                         is a test that would pass on a build with the
#                         property destroyed -- this project has that on record.
#
# Usage: ./opt_teeth.sh [-b BUILD_DIR] [-s SECS] [-k NKEYS]
#
# The sabotaged library is built in its own directory from a copy of the tree
# with one macro edited, so the good build is never disturbed.

set -e

BUILD=${BUILD:-../../build_unix}
SECS=10
NKEYS=50000
READERS=16
WRITERS=4
CACHE_MB=8

while [ $# -gt 0 ]; do
	case "$1" in
	-b) BUILD=$2; shift 2;;
	-s) SECS=$2; shift 2;;
	-k) NKEYS=$2; shift 2;;
	*) echo "usage: $0 [-b BUILD_DIR] [-s SECS] [-k NKEYS]" >&2; exit 2;;
	esac
done

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
SRC=$(CDPATH= cd -- "$HERE/../.." && pwd)
BUILD=$(CDPATH= cd -- "$BUILD" && pwd)
WORK=$(mktemp -d)
# No rm -rf anywhere in this repo's scripts: delete contents, then the dirs.
cleanup() {
	find "$WORK" -mindepth 1 -delete 2>/dev/null || true
	rmdir "$WORK" 2>/dev/null || true
}
trap cleanup 0 1 2 3 13 15

LIBS="-lpthread"
# liburing is linked into libdb.a on Linux builds that detected it.
if grep -q "HAVE_IO_URING" "$BUILD/db_config.h" 2>/dev/null; then
	LIBS="$LIBS -luring"
fi

fail() { echo "TEETH FAIL: $*"; exit 1; }

# One env home, reused by every arm: a DB_PRIVATE/layout artifact selected by
# the path is exactly how a false result gets manufactured here (see the warning
# atop run_bench.sh).  These arms are shared-env, but the rule is cheap to keep.
HOME_DIR=$WORK/env
mkdir -p "$HOME_DIR"

build_probe() {
	# $1 = library, $2 = output
	rm -f "$2"
	cc -O2 -w -I "$BUILD" -o "$2" "$HERE/opt_fires.c" "$1" $LIBS
	test -x "$2" || fail "probe $2 did not build"
}

echo "=== arm 1: optimistic ON (must run AND fire)"
build_probe "$BUILD/libdb.a" "$WORK/opt_fires"
find "$HOME_DIR" -mindepth 1 -delete
OPT_HOME=$HOME_DIR OPT_CACHE_MB=$CACHE_MB \
    "$WORK/opt_fires" "$NKEYS" "$READERS" "$WRITERS" "$SECS" \
    > "$WORK/on.txt" 2>&1 || true
cat "$WORK/on.txt"
grep -q "^VERDICT opt-fires-teeth " "$WORK/on.txt" ||
    fail "no opt-fires-teeth verdict (path never ran, or validation never fired)"
if grep -q "^FAIL" "$WORK/on.txt"; then
	fail "arm 1 reported FAIL"
fi

echo "=== arm 2: DB_NO_OPTREAD=1 (must be inert)"
find "$HOME_DIR" -mindepth 1 -delete
DB_NO_OPTREAD=1 OPT_HOME=$HOME_DIR OPT_CACHE_MB=$CACHE_MB \
    "$WORK/opt_fires" "$NKEYS" "$READERS" "$WRITERS" "$SECS" \
    > "$WORK/off.txt" 2>&1 || true
cat "$WORK/off.txt"
grep -q "^VERDICT opt-fires-control inert" "$WORK/off.txt" ||
    fail "control arm not inert with DB_NO_OPTREAD set"

echo "=== arm 3: sabotaged library (BH.gen never bumped) -- MUST FAIL arm 1"
SAB=$WORK/sab
mkdir -p "$SAB"
# Copy the source tree (not the build) and break exactly one macro.
tar -C "$SRC" -cf - src dist 2>/dev/null | tar -C "$SAB" -xf -
python3 - "$SAB/src/dbinc/mp.h" <<'PYEOF'
import sys
p = sys.argv[1]
s = open(p).read()
old = "\t(bhp)->gen = (u_int8_t)(((bhp)->gen + BH_GEN_STEP) | BH_GEN_INFLUX);\\\n"
if old not in s:
	sys.exit("sabotage: BH_GEN_EXCL_ENTER body not found -- update opt_teeth.sh")
open(p, "w").write(s.replace(old, "\t/* SABOTAGED: no generation bump. */\t\t\t\t\\\n"))
PYEOF
mkdir -p "$SAB/build_unix"
( cd "$SAB/build_unix" && "$SAB/dist/configure" --enable-diagnostic \
    > conf.log 2>&1 && make -j"$(nproc)" > build.log 2>&1 ) ||
    fail "sabotaged build failed (see $SAB/build_unix/build.log)"
test -f "$SAB/build_unix/libdb.a" || fail "sabotaged libdb.a missing"
build_probe "$SAB/build_unix/libdb.a" "$WORK/opt_fires_sab"
find "$HOME_DIR" -mindepth 1 -delete
OPT_HOME=$HOME_DIR OPT_CACHE_MB=$CACHE_MB \
    "$WORK/opt_fires_sab" "$NKEYS" "$READERS" "$WRITERS" "$SECS" \
    > "$WORK/sab.txt" 2>&1 || true
cat "$WORK/sab.txt"
grep -q "^FAIL" "$WORK/sab.txt" ||
    fail "SABOTAGED build PASSED -- arm 1 is vacuous; the generation bump is\
 not what makes it pass"
if grep -q "^VERDICT opt-fires-teeth " "$WORK/sab.txt"; then
	fail "sabotaged build produced the teeth verdict"
fi

echo "VERDICT opt-teeth all three arms behaved: on=fires off=inert sabotage=fails"
exit 0

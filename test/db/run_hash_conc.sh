#!/bin/sh -
#
# $Id$
#
# run_hash_conc.sh --
#	RFC 0017 change 1 correctness runner.  Builds and runs
#	test/db/hash_conc.c: concurrent HASH inserts and lookups across forced
#	bucket splits, from T threads in each of P processes, then verifies
#	every committed key is present exactly once, no aborted key is visible,
#	and DB->verify (db_verify) passes.
#
#	Teeth: set HASH_CONC_SABOTAGE=1 to rebuild the library with
#	-DRFC17_SABOTAGE_NO_VALIDATE (which makes __ham_get_meta_fast trust a stale
#	hint) and REQUIRE the test to FAIL.  Without the sabotage the
#	test must PASS.  Because the sabotage needs a library rebuild it is
#	opt-in; the normal arm runs everywhere.
#
# Usage (from build_unix):
#	sh ../test/db/run_hash_conc.sh
#	HASH_CONC_SABOTAGE=1 sh ../test/db/run_hash_conc.sh   # teeth: must FAIL
#
# Exits non-zero on failure or hang.

set -u

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/hash_conc.c}
HOME_DIR=${HOME_DIR:-HASH_CONC_TESTDIR}
TIMEOUT=${TIMEOUT:-300}
NPROC=${NPROC:-2}
NTHREAD=${NTHREAD:-4}
NKEYS=${NKEYS:-2000}

# Static library first (same rationale as run_hash_unsorted_cmp.sh).
LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
if [ -z "$LIB" ]; then
	for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib; do
		if [ -f "$cand" ]; then LIB="$cand"; break; fi
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "run_hash_conc.sh: FAIL (no libdb library under $BUILD)"; exit 1; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

# Optional teeth: rebuild the library so the LSN validation is defeated, then
# the concurrency verification must FAIL.  Done against a COPY of the build
# tree's objects is heavy; instead we recompile hash_meta.c with the sabotage define
# into a thin archive layered ahead of the real library at link time.  The
# sabotaged hash.o provides __ham_lookup, so the linker takes ours.
if [ "${HASH_CONC_SABOTAGE:-0}" = "1" ]; then
	echo "run_hash_conc.sh: SABOTAGE arm (LSN validation defeated; must FAIL)"
	SABO="$BUILD/hash_sabotage.o"
	if ! "${CC:-cc}" -c -g -O1 ${CFLAGS:-} -DRFC17_SABOTAGE_NO_VALIDATE \
	    -I"$BUILD" -I"$_HERE/../../src" -I"$_HERE/../../src/dbinc" \
	    "$_HERE/../../src/hash/hash_meta.c" -o "$SABO" 2> "$BUILD/sabotage-cc.log"; then
		echo "run_hash_conc.sh: SKIP (sabotage recompile of hash_meta.c failed;"
		echo "    this needs the full generated build tree)"
		sed -n '1,20p' "$BUILD/sabotage-cc.log"
		exit 0
	fi
	SABOTAGE_OBJ="$SABO"
else
	SABOTAGE_OBJ=""
fi

echo "Compiling hash_conc against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" $SABOTAGE_OBJ "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BUILD/hash_conc" || {
	echo "run_hash_conc.sh: FAIL (compile)"; exit 1; }

[ -d "$HOME_DIR" ] && find "$HOME_DIR" -mindepth 1 -delete 2>/dev/null
mkdir -p "$HOME_DIR"

if command -v timeout >/dev/null 2>&1; then TO="timeout"
elif command -v gtimeout >/dev/null 2>&1; then TO="gtimeout"
else TO=""; fi
run() { if [ -n "$TO" ]; then "$TO" "$@"; else shift; "$@"; fi; }

echo "Running hash_conc (nproc=$NPROC nthread=$NTHREAD nkeys=$NKEYS, timeout ${TIMEOUT}s)"
run "$TIMEOUT" "$BUILD/hash_conc" "$HOME_DIR" build "$NPROC" "$NTHREAD" "$NKEYS"
rc=$?

if [ "${HASH_CONC_SABOTAGE:-0}" = "1" ]; then
	if [ "$rc" -eq 0 ]; then
		echo "run_hash_conc.sh: SABOTAGE PASSED unexpectedly -- the test"
		echo "    has no teeth (validation off but verification still"
		echo "    passed).  FAIL"
		exit 1
	fi
	echo "run_hash_conc.sh: sabotage correctly caught (rc=$rc) -- PASS"
	exit 0
fi

if [ "$rc" -ne 0 ]; then
	echo "run_hash_conc.sh: FAIL (rc=$rc)"
	exit "$rc"
fi
echo "run_hash_conc.sh: PASS"
exit 0

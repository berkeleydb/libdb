#!/bin/sh -
#
# $Id$
#
# run_hash_notdurable.sh --
#	RFC 0017 change 1, lead-review item #1: a DB_TXN_NOT_DURABLE HASH under
#	forced bucket splits.  A non-logging cursor takes the LSN_NOT_LOGGED
#	branch in the split paths, so the metapage LSN stamp does NOT advance;
#	the cache fast path must therefore stay disabled (gated on
#	DBC_LOGGING) and master's locked metapath must keep the result correct.
#	Asserts every committed key present once, no aborted key visible, and
#	DB->verify passes -- the same oracle as run_hash_conc.sh.
#
#	Shares test/db/hash_conc.c (the `notdurable` mode).
#
# Usage (from build_unix):
#	sh ../test/db/run_hash_notdurable.sh
#
# Exits non-zero on failure or hang.

set -u

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/hash_conc.c}
HOME_DIR=${HOME_DIR:-HASH_NOTDUR_TESTDIR}
TIMEOUT=${TIMEOUT:-240}
NPROC=${NPROC:-2}
NTHREAD=${NTHREAD:-4}
NKEYS=${NKEYS:-2000}

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
[ -n "$LIB" ] || { echo "run_hash_notdurable.sh: FAIL (no libdb under $BUILD)"; exit 1; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling hash_conc (notdurable) against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BUILD/hash_conc" || {
	echo "run_hash_notdurable.sh: FAIL (compile)"; exit 1; }

[ -d "$HOME_DIR" ] && find "$HOME_DIR" -mindepth 1 -delete 2>/dev/null
mkdir -p "$HOME_DIR"

if command -v timeout >/dev/null 2>&1; then TO="timeout"
elif command -v gtimeout >/dev/null 2>&1; then TO="gtimeout"
else TO=""; fi
run() { if [ -n "$TO" ]; then "$TO" "$@"; else shift; "$@"; fi; }

echo "Running hash_conc notdurable (nproc=$NPROC nthread=$NTHREAD nkeys=$NKEYS)"
if run "$TIMEOUT" "$BUILD/hash_conc" "$HOME_DIR" notdurable "$NPROC" \
    "$NTHREAD" "$NKEYS"; then
	echo "run_hash_notdurable.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_hash_notdurable.sh: FAIL (rc=$rc)"
	exit "$rc"
fi

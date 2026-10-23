#!/bin/sh -
#
# $Id$
#
# run_hash_abort_split.sh --
#	RFC 0017 change 1: abort a transaction that forced several bucket
#	splits, with concurrent readers.  Asserts no reader ever saw an
#	aborted key, every aborted key is absent afterward, and DB->verify
#	passes.  This is the hazard-H2 (aborted-split LSN ABA) regression:
#	the cache fill takes a metapage READ lock that conflicts with the
#	splitter's uncommitted WRITE lock, so no reader can map through a
#	torn mid-split mapping.
#
#	Shares test/db/hash_conc.c with run_hash_conc.sh (the `abort` mode).
#
# Usage (from build_unix):
#	sh ../test/db/run_hash_abort_split.sh
#
# Exits non-zero on failure or hang.

set -u

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/hash_conc.c}
HOME_DIR=${HOME_DIR:-HASH_ABORT_TESTDIR}
TIMEOUT=${TIMEOUT:-240}
NREADER=${NREADER:-4}
NKEYS=${NKEYS:-4000}

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
[ -n "$LIB" ] || { echo "run_hash_abort_split.sh: FAIL (no libdb under $BUILD)"; exit 1; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling hash_conc (abort) against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BUILD/hash_conc" || {
	echo "run_hash_abort_split.sh: FAIL (compile)"; exit 1; }

[ -d "$HOME_DIR" ] && find "$HOME_DIR" -mindepth 1 -delete 2>/dev/null
mkdir -p "$HOME_DIR"

if command -v timeout >/dev/null 2>&1; then TO="timeout"
elif command -v gtimeout >/dev/null 2>&1; then TO="gtimeout"
else TO=""; fi
run() { if [ -n "$TO" ]; then "$TO" "$@"; else shift; "$@"; fi; }

echo "Running hash_conc abort (nreader=$NREADER nkeys=$NKEYS, timeout ${TIMEOUT}s)"
if run "$TIMEOUT" "$BUILD/hash_conc" "$HOME_DIR" abort "$NREADER" "$NKEYS"; then
	echo "run_hash_abort_split.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_hash_abort_split.sh: FAIL (rc=$rc)"
	exit "$rc"
fi

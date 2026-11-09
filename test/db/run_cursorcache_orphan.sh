#!/bin/sh -
#
# $Id$
#
# run_cursorcache_orphan.sh --
#	Regression test for the transient-cursor-cache ORPHAN defect (RFC 0021,
#	review #238 BLOCKER): a close of any handle must not strand a live
#	handle's cached cursor on its active queue.  See cursorcache_orphan.c.
#
#	This FAILs on the pre-fix head (inserts >100x slower, RSS +tens of MB)
#	and PASSes with the fix (inserts flat, RSS flat).
#
# Exits non-zero on failure.

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/cursorcache_orphan.c}
HOME_DIR=${HOME_DIR:-CURSORCACHE_ORPHAN_DIR}
TIMEOUT=${TIMEOUT:-300}
CYCLES=${CYCLES:-5000}

LIB=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
[ -n "$LIB" ] || { echo "FAIL: no static libdb found under $BUILD"; exit 1; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

if command -v timeout >/dev/null 2>&1; then TIMEOUT_CMD="timeout";
elif command -v gtimeout >/dev/null 2>&1; then TIMEOUT_CMD="gtimeout";
else TIMEOUT_CMD=""; fi
run_with_timeout() {
	if [ -n "$TIMEOUT_CMD" ]; then "$TIMEOUT_CMD" "$@"; else shift; "$@"; fi
}

[ -d "$HOME_DIR" ] && find "$HOME_DIR" -mindepth 1 -delete 2>/dev/null
mkdir -p "$HOME_DIR"

echo "Compiling cursorcache_orphan against $LIB"
"${CC:-cc}" -g -O2 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $EXTRALIBS -o "$BUILD/cursorcache_orphan"

echo "Running cursorcache_orphan (CYCLES=$CYCLES, timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/cursorcache_orphan" "$HOME_DIR" "$CYCLES"; then
	echo "run_cursorcache_orphan.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_cursorcache_orphan.sh: FAIL (rc=$rc)"
	exit 1
fi

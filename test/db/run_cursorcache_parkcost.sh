#!/bin/sh -
#
# $Id$
#
# run_cursorcache_parkcost.sh --
#	Regression test for the transient-cursor-cache THREAD-EXIT defect
#	(RFC 0021, review #238 concern 2): cursors cached by a thread that then
#	exits must not stay parked on the handle's active queue, slowing every
#	later insert.  See cursorcache_parkcost.c.
#
#	This FAILs on the pre-fix head (inserts up to 2.3x slower after a pool
#	of threads exits) and PASSes with the thread-exit flush.
#
# Exits non-zero on failure.

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/cursorcache_parkcost.c}
HOME_DIR=${HOME_DIR:-CURSORCACHE_PARK_DIR}
TIMEOUT=${TIMEOUT:-300}
POOL=${POOL:-256}

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

echo "Compiling cursorcache_parkcost against $LIB"
"${CC:-cc}" -g -O2 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $EXTRALIBS -o "$BUILD/cursorcache_parkcost"

echo "Running cursorcache_parkcost (POOL=$POOL, timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/cursorcache_parkcost" "$HOME_DIR" "$POOL"; then
	echo "run_cursorcache_parkcost.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_cursorcache_parkcost.sh: FAIL (rc=$rc)"
	exit 1
fi

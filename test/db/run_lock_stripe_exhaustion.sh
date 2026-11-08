#!/bin/sh -
#
# $Id$
#
# run_lock_stripe_exhaustion.sh --
#	Build and run lock_stripe_exhaustion.c, the regression test for the P12
#	per-stripe locker-allocation exhaustion that made a TCL recd015 backward
#	recovery pass fail with "BDB2055 Lock table is out of available locker
#	entries" (first broken by 2d8397dfe, shipped in v2026.10.5).
#
#	P12 gave each of the 64 locker stripes its own free list and refills an
#	empty stripe with a batch capped by st_maxlockers - st_lockers.  Once
#	st_lockers reaches st_maxlockers (= lk_max_lockers) the cap is zero, so a
#	create landing on a stripe whose free list is empty failed even though
#	other stripes still held idle free lockers.  __lock_getlocker_int now
#	steals a free locker from another stripe before failing.
#
# Exits non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/lock_stripe_exhaustion.c}
HOME_DIR=${HOME_DIR:-LOCK_STRIPE_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

# Prefer the STATIC library (see run_lock_priority_nullderef.sh for why).
LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-5.3.a "$BUILD"/.libs/libdb-*.a; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
if [ -z "$LIB" ]; then
	for cand in "$BUILD"/.libs/libdb-5.3.so "$BUILD"/.libs/libdb-5.3.dylib \
	    "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib; do
		if [ -f "$cand" ]; then LIB="$cand"; break; fi
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "FAIL: no libdb library (static or shared) found under $BUILD"; exit 1; }

# A static libdb needs its transitive deps named explicitly.
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling lock_stripe_exhaustion against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/lock_stripe_exhaustion"

# Start from an EMPTY home, regions included (see the priority runner note).
[ -d "$HOME_DIR" ] && find "$HOME_DIR" -mindepth 1 -delete 2>/dev/null
mkdir -p "$HOME_DIR"

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
		shift	# drop the seconds argument
		"$@"
	fi
}
echo "Running lock_stripe_exhaustion (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/lock_stripe_exhaustion"; then
	echo "run_lock_stripe_exhaustion.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_lock_stripe_exhaustion.sh: FAIL (rc=$rc)"
	exit $rc
fi

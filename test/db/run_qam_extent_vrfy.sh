#!/bin/sh -
#
# $Id$
#
# run_qam_extent_vrfy.sh --
#	Build and run qam_extent_vrfy.c, the regression gate for queue extent
#	verification.  PR #160 bounded __qam_vrfy_walkqueue()'s scan by
#	vdp->last_pgno, which is the last page of the MAIN .db file -- but a
#	queue with extents keeps only its meta page there, so the bound
#	collapsed to 0 and the extent walk was skipped: db_verify reported
#	success on a queue with a corrupted extent page (shipped in 5.3.35).
#
#	The test asserts BOTH directions, because a verifier that silently does
#	nothing also passes "legitimate queues still verify OK":
#	  1. a legitimate many-extent queue verifies clean, and
#	  2. a corrupted extent page is still DETECTED.
#
# Exits non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/db/qam_extent_vrfy.c}
HOME_DIR=${HOME_DIR:-QAM_EXTENT_VRFY_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

# Prefer the STATIC library: on macOS the .dylib carries a baked-in install name
# (/usr/local/BerkeleyDB.5.3/lib/...) which takes precedence over -rpath, so a
# shared link runs against an uninstalled path and dyld aborts.  Static linking
# avoids the dynamic loader entirely, which is what the other CI-wired suites
# (e.g. test/fuzz) already do.  Fall back to the shared library if no static one
# was built.
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

echo "Compiling qam_extent_vrfy against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/qam_extent_vrfy"

rm -f "$HOME_DIR"/*.db 2>/dev/null || true
mkdir -p "$HOME_DIR"

# `timeout` is GNU coreutils: present on Linux, absent on stock macOS (where it
# is `gtimeout` if coreutils is installed).  Resolve it once; if neither exists,
# run without a timeout rather than failing with rc=127.
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
echo "Running qam_extent_vrfy (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/qam_extent_vrfy"; then
	echo "run_qam_extent_vrfy.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_qam_extent_vrfy.sh: FAIL (rc=$rc)"
	exit $rc
fi

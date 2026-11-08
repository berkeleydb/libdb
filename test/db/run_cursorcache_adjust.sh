#!/bin/sh -
#
# $Id$
#
# run_cursorcache_adjust.sh --
#	Build and run cursorcache_adjust.c, the correctness test for the
#	per-thread transient-cursor cache (RFC 0021).
#
#	The cache lets DB->get/DB->put reuse a transient cursor without the
#	cursor-queue mutex.  A cached cursor stays on the handle's ACTIVE queue
#	so __db_walk_cursors still adjusts it while it is positioned on a page
#	another thread is changing.  This test hammers one shared DB_THREAD
#	btree handle with concurrent transient puts and gets on crowded leaves
#	and verifies every value round-trips.
#
#	SABOTAGE ARM: a second library is built with -DDBC_CACHE_SABOTAGE, which
#	positions a reused cached cursor while OFF the active queue, breaking
#	the invariant.  The test MUST FAIL against that library and PASS against
#	the normal one -- otherwise the test is not actually exercising the
#	invariant it claims to.
#
# Exits non-zero on failure.

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/cursorcache_adjust.c}
HOME_DIR=${HOME_DIR:-CURSORCACHE_TESTDIR}
TIMEOUT=${TIMEOUT:-240}

# Locate the normal static library built in $BUILD.
LIB=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
[ -n "$LIB" ] || { echo "FAIL: no static libdb found under $BUILD"; exit 1; }

# Static libdb needs its transitive deps named explicitly.
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

# `timeout` is GNU coreutils; resolve it once, tolerate its absence.
if command -v timeout >/dev/null 2>&1; then
	TIMEOUT_CMD="timeout"
elif command -v gtimeout >/dev/null 2>&1; then
	TIMEOUT_CMD="gtimeout"
else
	TIMEOUT_CMD=""
fi
run_with_timeout() {
	if [ -n "$TIMEOUT_CMD" ]; then "$TIMEOUT_CMD" "$@";
	else shift; "$@"; fi
}

clean_home() {
	[ -d "$HOME_DIR" ] && find "$HOME_DIR" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$HOME_DIR"
}

# ---- Arm 1: the real build.  Must PASS. ----
echo "Compiling cursorcache_adjust against $LIB (correct build)"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $EXTRALIBS -o "$BUILD/cursorcache_adjust"

clean_home
echo "Running cursorcache_adjust (correct, timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/cursorcache_adjust" "$HOME_DIR"; then
	echo "  correct arm PASSED (as expected)"
else
	rc=$?
	echo "run_cursorcache_adjust.sh: FAIL (correct build returned rc=$rc)"
	exit 1
fi

# ---- Arm 2: the sabotage build.  Must FAIL. ----
# Build a sabotaged copy of just db_cam.o (which carries the stale-handle epoch
# guard the sabotage disables) and splice it into a private archive, so we do
# not disturb $BUILD's library.  If the source/object is not available (e.g. an
# installed-only tree), skip the sabotage arm with a notice rather than failing:
# the correct arm already ran.
SRCROOT=""
for cand in "$_HERE/../.." "$BUILD/.." "$BUILD/../src"; do
	if [ -f "$cand/src/db/db_cam.c" ]; then
		SRCROOT=$(cd "$cand" && pwd); break
	fi
done

if [ -z "$SRCROOT" ] || [ ! -f "$BUILD/db_cam.o" ]; then
	echo "run_cursorcache_adjust.sh: NOTE sabotage arm skipped"\
	     "(no db_cam source/object to rebuild under $BUILD)"
	echo "run_cursorcache_adjust.sh: PASS"
	exit 0
fi

SABDIR="$BUILD/cursorcache_sabotage"
rm -rf "$SABDIR"
mkdir -p "$SABDIR"
cp "$LIB" "$SABDIR/libdb_sab.a"

echo "Compiling sabotaged db_cam.o (-DDBC_CACHE_SABOTAGE)"
"${CC:-cc}" -c -g -O1 -DDBC_CACHE_SABOTAGE \
    -I"$BUILD" -I"$SRCROOT/src" -I"$SRCROOT/src/dbinc" \
    -D_GNU_SOURCE -DHAVE_CONFIG_H \
    "$SRCROOT/src/db/db_cam.c" -o "$SABDIR/db_cam_sab.o" 2>"$SABDIR/cc.log" || {
	echo "run_cursorcache_adjust.sh: NOTE sabotage arm skipped"\
	     "(sabotaged db_cam.c did not compile standalone; see $SABDIR/cc.log)"
	echo "run_cursorcache_adjust.sh: PASS"
	exit 0
}

# Replace db_cam.o inside the private archive with the sabotaged object.
( cd "$SABDIR" && ar d libdb_sab.a db_cam.o 2>/dev/null; \
  cp db_cam_sab.o db_cam.o && ar r libdb_sab.a db_cam.o && ranlib libdb_sab.a )

echo "Compiling cursorcache_adjust against the sabotaged library"
# Link the sabotage executable with AddressSanitizer so the use-after-free the
# disabled epoch guard permits (a reopened handle reusing a freed cached
# cursor) is caught deterministically rather than depending on whether freed
# memory happens to still look valid.  ASan intercepts malloc/free
# process-wide, so it covers libdb's allocations too even though libdb.a itself
# is not instrumented.  If the toolchain has no ASan, fall back to
# MALLOC_PERTURB_, which fills freed memory and usually turns the fault into a
# crash.
ASAN=""
if printf 'int main(void){return 0;}\n' | \
    "${CC:-cc}" -fsanitize=address -xc - -o "$SABDIR/.asan_probe" \
    -lpthread >/dev/null 2>&1; then
	ASAN="-fsanitize=address"
	rm -f "$SABDIR/.asan_probe"
fi
"${CC:-cc}" -g -O1 $ASAN ${CFLAGS:-} -I"$BUILD" "$SRC" "$SABDIR/libdb_sab.a" \
    -lpthread $EXTRALIBS -o "$SABDIR/cursorcache_adjust_sab"

clean_home
echo "Running cursorcache_adjust (sabotage, timeout ${TIMEOUT}s) -- MUST FAIL"
# MALLOC_PERTURB_ fills freed memory even when ASan is unavailable, so a
# use-after-free on the freed cached cursor misbehaves rather than silently
# reading stale-but-plausible bytes.
if MALLOC_PERTURB_=165 \
    run_with_timeout "$TIMEOUT" "$SABDIR/cursorcache_adjust_sab" "$HOME_DIR"; then
	echo "run_cursorcache_adjust.sh: FAIL"\
	     "(sabotage build PASSED -- the test does not catch the broken invariant)"
	exit 1
else
	echo "  sabotage arm FAILED (as expected)"
fi

echo "run_cursorcache_adjust.sh: PASS"
exit 0

#!/bin/sh -
#
# $Id$
#
# run_cursorcache_envuaf.sh --
#	Regression test for the transient-cursor-cache ENV use-after-free
#	(RFC 0021, review #238): a stale cache slot must never dereference its
#	handle or env, because the closing thread need not be the caching
#	thread.  See cursorcache_envuaf.c.
#
#	Built with AddressSanitizer when the toolchain has it (the fault is a
#	read of freed heap).  Runs the "exit" and "reuse" arms.  Under ASan the
#	pre-fix head FAILs (heap-use-after-free in __dbc_cache_handle_live) and
#	the fix PASSes; without ASan both arms run for exit-code sanity.
#
# Exits non-zero on failure.

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/cursorcache_envuaf.c}
TIMEOUT=${TIMEOUT:-120}

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

# Link with ASan when available: it intercepts malloc/free process-wide, so it
# catches the use-after-free even though libdb.a itself is not instrumented.
ASAN=""
if printf 'int main(void){return 0;}\n' | \
    "${CC:-cc}" -fsanitize=address -xc - -o "$BUILD/.cc_asan_probe" \
    -lpthread >/dev/null 2>&1; then
	ASAN="-fsanitize=address"
	rm -f "$BUILD/.cc_asan_probe"
fi

echo "Compiling cursorcache_envuaf against $LIB${ASAN:+ (ASan)}"
"${CC:-cc}" -g -O1 $ASAN ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $EXTRALIBS -o "$BUILD/cursorcache_envuaf"

rc=0
for arm in exit reuse; do
	echo "Running cursorcache_envuaf $arm (timeout ${TIMEOUT}s)"
	if ASAN_OPTIONS=detect_leaks=0 \
	    run_with_timeout "$TIMEOUT" "$BUILD/cursorcache_envuaf" "$arm"; then
		echo "  $arm arm PASSED"
	else
		echo "run_cursorcache_envuaf.sh: FAIL ($arm arm rc=$?)"
		rc=1
	fi
done
[ "$rc" = 0 ] && echo "run_cursorcache_envuaf.sh: PASS"
exit $rc

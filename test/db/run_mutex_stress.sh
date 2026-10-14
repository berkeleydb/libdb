#!/bin/sh
# Mutual exclusion + lost-wakeup gate for the TAS / hybrid mutex.
#
# test/c/test_mutex_stress.c, two modes:
#   counter  -- N threads, plain RMW inside one mutex.  Catches a MUTEX_SET that
#               does not acquire atomically, and (on weakly ordered CPUs) a
#               MUTEX_SET/UNSET pair without acquire/release ordering.
#   handoff  -- two threads, strict alternation, so a missed wakeup is never
#               healed by a third thread.  Catches a hybrid sleep/wake
#               handshake without a store->load fence (MUTEX_MEMBAR).
# Teeth, measured on FreeBSD/arm64 (Neoverse-V1) with sabotaged mutex_int.h:
#   non-atomic MUTEX_SET            counter FAIL 6/6
#   RELAXED set + RELAXED unset     counter FAIL 6/6
#   MUTEX_MEMBAR = compiler barrier handoff FAIL 2/2 (hang); counter PASS 6/6
# so neither mode alone is enough.  Judged on the VERDICT line, not rc.
#
# On a build without a TAS mutex (pure pthreads) it still runs, and then tests
# the pthread mutex instead -- still a valid mutual-exclusion check.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/../c/test_mutex_stress.c

# Prefer the static library.  On macOS the .dylib carries a baked-in install
# name (/usr/local/BerkeleyDB.<version>/lib/...) that takes precedence over
# -rpath, so a binary linked against an uninstalled one is aborted by dyld
# before main() runs.
LIB="" ; LIBRPATH=""
[ -f "$BUILD/libdb.a" ] && LIB="$BUILD/libdb.a"
if [ -z "$LIB" ] ; then
	for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib ; do
		[ -f "$cand" ] && { LIB="$cand" ; break ; }
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "run_mutex_stress.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

# timeout(1) is GNU coreutils: present on Linux, absent on stock macOS (where it
# is gtimeout if coreutils is installed).  Without either, run unbounded rather
# than fail with rc=127; the driver's watchdog still catches a stall that starts
# inside its measurement window.
if command -v timeout >/dev/null 2>&1 ; then
	TIMEOUT_CMD=timeout
elif command -v gtimeout >/dev/null 2>&1 ; then
	TIMEOUT_CMD=gtimeout
else
	TIMEOUT_CMD=""
fi
run_with_timeout() {
	if [ -n "$TIMEOUT_CMD" ] ; then
		"$TIMEOUT_CMD" "$@"
	else
		shift	# drop the seconds argument
		"$@"
	fi
}

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/test_mutex_stress
rm -f "$BIN"
"${CC:-cc}" -O2 ${CFLAGS:-} -pthread -I"$BUILD" "$SRC" "$LIB" $LIBRPATH \
    $EXTRALIBS -o "$BIN" || { echo "run_mutex_stress.sh: FAIL cannot build" ; exit 1 ; }

H=${TMPDIR:-/tmp}/mutex_stress_$$
clean() { [ -d "$H" ] && find "$H" -mindepth 1 -delete ; }
rc=0
# args: threads secs spins [handoff].  spins=1 forces the suspend path;
# spins=0 keeps the library default (50 x ncpu).
for a in "16 3 1" "16 3 0" "2 15 1 handoff" "2 15 0 handoff" ; do
	clean ; mkdir -p "$H"
	# shellcheck disable=SC2086
	out=$(run_with_timeout 120 "$BIN" "$H" $a 2>&1) ; brc=$?
	v=$(printf '%s\n' "$out" | grep '^VERDICT mutex_stress')
	echo "  [$a] ${v:-NO VERDICT (rc=$brc)}"
	# Without a verdict the driver's own output is the only clue (a loader
	# abort, a missing command), so show it rather than discard it.
	[ -n "$v" ] || printf '%s\n' "$out" | tail -5 | sed 's/^/	| /'
	case "$v" in "VERDICT mutex_stress PASS"*) ;; *) rc=1 ;; esac
done
clean ; rmdir "$H" 2>/dev/null
if [ $rc -eq 0 ] ; then
	echo "run_mutex_stress.sh: PASS"
else
	echo "run_mutex_stress.sh: FAIL"
fi
exit $rc

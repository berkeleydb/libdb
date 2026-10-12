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

LIB="" ; LIBRPATH=""
for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib ; do
	[ -f "$cand" ] && { LIB="$cand" ; break ; }
done
[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
[ -n "$LIB" ] || [ ! -f "$BUILD/libdb.a" ] || LIB="$BUILD/libdb.a"
[ -n "$LIB" ] || { echo "run_mutex_stress.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

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
	v=$(timeout 120 "$BIN" "$H" $a 2>&1 | grep '^VERDICT mutex_stress')
	echo "  [$a] ${v:-NO VERDICT}"
	case "$v" in "VERDICT mutex_stress PASS"*) ;; *) rc=1 ;; esac
done
clean ; rmdir "$H" 2>/dev/null
if [ $rc -eq 0 ] ; then
	echo "run_mutex_stress.sh: PASS"
else
	echo "run_mutex_stress.sh: FAIL"
fi
exit $rc

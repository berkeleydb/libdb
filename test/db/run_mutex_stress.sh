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
. "$HERE/runner_lib.sh"
rl_default_build
SRC=$HERE/../c/test_mutex_stress.c

# Locate libdb (static first, for macOS dyld) and its transitive deps.
rl_find_lib

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

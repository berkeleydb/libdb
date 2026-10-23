#!/bin/sh
# Multi-PROCESS hybrid-mutex contention gate.
#
# test/c/test_mutex_mpstress.c forks NPROC independent processes (fork + exec,
# each a fresh libpthread with its own mapping of the shared mutex region) that
# all contend on ONE db_mutex_t in a shared (not DB_PRIVATE) environment, with
# low tas_spins so contended acquires go through __db_hybrid_mutex_suspend.
#
# This is the arm that catches the macOS bug the thread-only test_mutex_stress
# cannot: on Darwin a PTHREAD_PROCESS_SHARED condition variable binds to the
# virtual ADDRESS of the mutex it is first waited with, libdb maps the region
# at a different address in each process, and the second process's
# pthread_cond_wait() therefore returns EINVAL -> "pthread suspend failed" ->
# PANIC -> DB_RUNRECOVERY -> a wedged environment.  The test's mutex_lock then
# returns non-zero (hard FAIL) or the environment makes no progress (the 10s
# watchdog fires).
#
# Teeth: this FAILs on unmodified master on macOS (see test/KNOWN-ISSUES.md and
# the CI run linked from the PR) and PASSes with the Darwin suspend-path fix.
# On Linux/FreeBSD it is a genuine multi-process mutual-exclusion check and
# passes on master and on the fix alike.
#
# Judged on the VERDICT line, not rc, because the defect manifests as a hang.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/../c/test_mutex_mpstress.c

# Prefer the static library.  On macOS the .dylib carries a baked-in install
# name that takes precedence over -rpath, so a binary linked against an
# uninstalled one is aborted by dyld before main().  (Same rationale as
# run_mutex_stress.sh.)
LIB="" ; LIBRPATH=""
[ -f "$BUILD/libdb.a" ] && LIB="$BUILD/libdb.a"
if [ -z "$LIB" ] ; then
	for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib ; do
		[ -f "$cand" ] && { LIB="$cand" ; break ; }
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "run_mutex_mpstress.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

# timeout(1): GNU coreutils on Linux, gtimeout on macOS if coreutils installed.
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

BIN=$BUILD/test_mutex_mpstress
rm -f "$BIN"
"${CC:-cc}" -O2 ${CFLAGS:-} -pthread -I"$BUILD" "$SRC" "$LIB" $LIBRPATH \
    $EXTRALIBS -o "$BIN" || { echo "run_mutex_mpstress.sh: FAIL cannot build" ; exit 1 ; }

H=${TMPDIR:-/tmp}/mutex_mpstress_$$
clean() { [ -d "$H" ] && find "$H" -mindepth 1 -delete ; }
rc=0
# args: NPROC SECONDS SPINS.  spins=1 forces the suspend path; the second arm
# uses a few spins and more processes to widen the contention window.
for a in "2 5 1" "4 5 1" "4 5 8" ; do
	clean ; mkdir -p "$H"
	# shellcheck disable=SC2086
	out=$(run_with_timeout 90 "$BIN" "$H" $a 2>&1) ; brc=$?
	v=$(printf '%s\n' "$out" | grep '^VERDICT mutex_mpstress')
	echo "  [$a] ${v:-NO VERDICT (rc=$brc)}"
	[ -n "$v" ] || printf '%s\n' "$out" | tail -6 | sed 's/^/	| /'
	case "$v" in "VERDICT mutex_mpstress PASS"*) ;; *) rc=1 ;; esac
done
clean ; rmdir "$H" 2>/dev/null
if [ $rc -eq 0 ] ; then
	echo "run_mutex_mpstress.sh: PASS"
else
	echo "run_mutex_mpstress.sh: FAIL"
fi
exit $rc

#!/bin/sh -
#
# $Id$
#
# run_txn_remevent_leak.sh --
#	Build and run txn_remevent_leak.c, the regression test for the leak of
#	a child transaction's TXN_REMOVE event (DB_ENV->dbremove inside a
#	child txn) when the child or its parent aborts: __txn_doevents
#	(src/txn/txn_util.c) kept the event "for the parent" on abort as well
#	as on commit, but only a commit hands it over, so it was in no list.
#	The test counts libdb's allocations through db_env_set_func_*.
#
# Exits non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}
# Anchor to THIS script's directory, not the caller's cwd.  ../test/db/X.c
# only resolves when the build dir sits INSIDE the source tree; with a build
# dir that is a sibling of the source tree -- the layout run_all.sh itself
# documents, and the one the workflows use -- it resolved to nothing and the
# compile failed with "no such file or directory".
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/txn_remevent_leak.c}
HOME_DIR=${HOME_DIR:-TXN_REMEVENT_TESTDIR}
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

echo "Compiling txn_remevent_leak against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/txn_remevent_leak"

# Start from an EMPTY home, regions included. Removing only *.db left the
# previous run's __db.00N region files behind, so a second run (another arm,
# another build) attached a region it did not create: on FreeBSD that is a
# PANIC (a pshared mutex whose initialiser has exited, tracker row B3), and
# between builds it is a DB_VERSION_MISMATCH. Either way a false red.
[ -d "$HOME_DIR" ] && find "$HOME_DIR" -mindepth 1 -delete 2>/dev/null
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
echo "Running txn_remevent_leak (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/txn_remevent_leak"; then
	echo "run_txn_remevent_leak.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_txn_remevent_leak.sh: FAIL (rc=$rc)"
	exit $rc
fi

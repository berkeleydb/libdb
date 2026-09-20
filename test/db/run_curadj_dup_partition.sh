#!/bin/sh -
#
# $Id$
#
# run_curadj_dup_partition.sh --
#	Build and run curadj_dup_partition.c, the regression test for the
#	cursor-adjustment retry protocol on a DB_THREAD handle.
#
#	__db_walk_cursors() holds a cq_parts[] partition mutex across its
#	callback, but __bam_ca_dup_func / __bam_ca_undodup_func /
#	__ham_chgpg_recover_func kept dropping dbp->mutex -- the mutex the walk
#	held before the cursor queues were sharded.  On a DB_THREAD handle that
#	unlocks a mutex the thread does not hold and then re-enters the held
#	partition mutex through __db_cursor_int / __dbc_close, which deadlocks.
#
#	THIS IS PRIMARILY A HANG TEST, hence the timeout below: against the
#	defect the test process blocks forever in __db_cursor_int rather than
#	failing.  Non-threaded handles leave these mutexes MUTEX_INVALID, which
#	is why the TCL cursor suites cannot see this and this test uses
#	DB_THREAD deliberately.
#
# Exits non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/db/curadj_dup_partition.c}
HOME_DIR=${HOME_DIR:-CURADJ_DUP_TESTDIR}
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

echo "Compiling curadj_dup_partition against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/curadj_dup_partition"

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
echo "Running curadj_dup_partition (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/curadj_dup_partition" "$HOME_DIR"; then
	echo "run_curadj_dup_partition.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_curadj_dup_partition.sh: FAIL (rc=$rc)"
	exit $rc
fi

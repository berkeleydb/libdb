#!/bin/sh -
#
# $Id$
#
# run_compact_subdb_remove.sh --
#	Build and run compact_subdb_remove.c, the regression test for the
#	compaction hang when a sub-database is removed and a sibling's
#	metadata page is moved into the freed page in the same transaction.
#	__db_move_metadata (src/db/db_compact.c) asked for the handle lock on
#	the moved page under the dbp's own handle locker rather than the
#	transaction locker, self-deadlocking against the WRITE handle lock the
#	transaction already held from the pending remove.  The test itself is
#	SIGALRM-guarded: a hang is reported as a failure, never a hung suite.
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
SRC=${SRC:-$_HERE/compact_subdb_remove.c}
HOME_DIR=${HOME_DIR:-COMPACT_SUBDB_REMOVE}
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

echo "Compiling compact_subdb_remove against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/compact_subdb_remove"

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
echo "Running compact_subdb_remove (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/compact_subdb_remove"; then
	echo "run_compact_subdb_remove.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_compact_subdb_remove.sh: FAIL (rc=$rc)"
	exit $rc
fi

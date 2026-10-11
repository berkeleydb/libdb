#!/bin/sh -
#
# $Id$
#
# run_log_emfile_nopanic.sh --
#	Build and run log_emfile_nopanic.c, the regression test for the
#	environment panic when a log-file open fails with EMFILE/ENFILE.
#	__log_name (src/log/log_put.c) treated any non-ENOENT open error as
#	fatal and panicked the environment (abort, in diagnostic builds); a
#	transient descriptor shortage must instead be returned to the caller.
#	The condition is injected through db_env_set_func_open, so no real
#	descriptor exhaustion is needed.
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
SRC=${SRC:-$_HERE/log_emfile_nopanic.c}
HOME_DIR=${HOME_DIR:-LOG_EMFILE_NOPANIC}
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

echo "Compiling log_emfile_nopanic against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/log_emfile_nopanic"

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
echo "Running log_emfile_nopanic (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/log_emfile_nopanic"; then
	echo "run_log_emfile_nopanic.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_log_emfile_nopanic.sh: FAIL (rc=$rc)"
	exit $rc
fi

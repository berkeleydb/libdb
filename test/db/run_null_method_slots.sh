#!/bin/sh -
#
# $Id$
#
# run_null_method_slots.sh --
#	Build and run null_method_slots.c, the regression test for
#	https://github.com/berkeleydb/libdb/issues/149 -- two NULL
#	dereferences reachable from documented public APIs:
#
#	1. DB_ENV->rep_get_nsites() on a repmgr-configured but unopened
#	   environment dereferenced db_rep->region (attached only at open).
#	2. __cdsgroup_begin() left 4 of DB_TXN's 12 method slots NULL, so
#	   calling one was an indirect call through a NULL pointer.
#
# Exits non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/db/null_method_slots.c}
HOME_DIR=${HOME_DIR:-NULL_METHOD_TESTDIR}
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

echo "Compiling null_method_slots against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/null_method_slots"

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
echo "Running null_method_slots (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/null_method_slots"; then
	echo "run_null_method_slots.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_null_method_slots.sh: FAIL (rc=$rc)"
	exit $rc
fi

#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_bamc_prev_alias.sh --
#	Build and run bamc_prev_alias.c, the regression test for the
#	strict-aliasing miscompile in __bamc_prev: IS_CUR_DELETED read the
#	cursor index through DBC_INTERNAL * while the walk wrote it through
#	BTREE_CURSOR *, and clang -O2 on aarch64 reordered the two, so DB_PREV
#	skipped live records next to deleted ones.
#
#	The test is compiled -O1 but the LIBRARY is the build's own -O2 one,
#	which is where the reorder happens, so this checks the shipped code.
#
# Exits non-zero on failure.

set -e

BUILD=${BUILD:-.}
# Anchor to THIS script's directory, not the caller's cwd.  ../test/db/X.c
# only resolves when the build dir sits INSIDE the source tree; with a build
# dir that is a sibling of the source tree -- the layout run_all.sh itself
# documents, and the one the workflows use -- it resolved to nothing and the
# compile failed with "no such file or directory".
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/bamc_prev_alias.c}
HOME_DIR=${HOME_DIR:-BAMC_PREV_ALIAS_TESTDIR}
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

echo "Compiling bamc_prev_alias against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/bamc_prev_alias"

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
echo "Running bamc_prev_alias (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/bamc_prev_alias" "$HOME_DIR"; then
	echo "run_bamc_prev_alias.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_bamc_prev_alias.sh: FAIL (rc=$rc)"
	exit $rc
fi

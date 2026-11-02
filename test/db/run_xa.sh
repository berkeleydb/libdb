#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_xa.sh --
#	Build and run the Tuxedo-free XA test harness (test/xa/tm/): a minimal
#	test transaction manager (xatm.c) that drives libdb's X/Open XA
#	resource-manager switch (db_xa_switch, src/xa/xa.c + xa_map.c) exactly
#	as a real TM does.  No Oracle Tuxedo install is required.
#
#	Two programs:
#	  xa_matrix    single-process state-machine matrix (2PC, rollback,
#	               one-phase, prepare/commit, prepare/rollback, read-only,
#	               two RMs in one global txn, suspend/resume + JOIN, and the
#	               protocol/argument error codes);
#	  xa_recover   multi-process crash recovery (child prepares, kill -9,
#	               parent recovers with xa_recover and resolves in-doubt
#	               branches; and a crash-during-commit arm).
#
#	Usage (from build_unix):  sh ../test/db/run_xa.sh
#	Env: BUILD (default .), TIMEOUT (default 300).

set -e

BUILD=${BUILD:-.}
# Anchor source paths to THIS script's directory (see run_dd_stripes.sh): a
# build dir that is a sibling of the source tree -- the layout run_all.sh
# documents -- makes ../test/xa/... resolve to nothing.
_HERE=$(cd "$(dirname "$0")" && pwd)
XADIR="$_HERE/../xa/tm"
TIMEOUT=${TIMEOUT:-300}

# ---------------------------------------------------------------------------
# Library discovery (copied from run_dd_stripes.sh): prefer the STATIC libdb,
# fall back to the versioned shared object, and NEVER hardcode the soname or
# -luring.  On macOS the .dylib carries a baked-in install name that takes
# precedence over -rpath, so static linking is preferred there too.
# ---------------------------------------------------------------------------
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

# A static libdb needs its transitive deps named explicitly (liburing on Linux
# when AIO is built in; discovered, not hardcoded).
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

# -I"$BUILD" for db_config.h / the generated headers; -I the XA source dir for
# xatm.h; -I ../src/dbinc for xa.h (db_xa_switch's XID/flag definitions).
INCS="-I$BUILD -I$XADIR -I$_HERE/../../src/dbinc"

compile() {
	# $1 = program name, remaining args = extra .c sources
	prog=$1; shift
	echo "Compiling $prog against $LIB"
	"${CC:-cc}" -g -O1 ${CFLAGS:-} $INCS \
	    "$XADIR/$prog.c" "$XADIR/xatm.c" "$@" "$LIB" \
	    -lpthread $LIBRPATH $EXTRALIBS \
	    -o "$BUILD/$prog"
}

compile xa_matrix
compile xa_recover

# `timeout` is GNU coreutils (Linux); macOS may have `gtimeout`.  Resolve once.
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
		shift
		"$@"
	fi
}

# Clean homes (regions included): a stale region from a prior run attached by a
# process that did not create it is a PANIC on some platforms (see
# run_dd_stripes.sh).  Remove without the recursive-force rm form.
for d in XA_MATRIX_RM1 XA_MATRIX_RM2 XA_RECOVER_HOME; do
	[ -d "$BUILD/$d" ] && find "$BUILD/$d" -mindepth 1 -delete 2>/dev/null || true
	mkdir -p "$BUILD/$d"
done

rc=0

echo "== Running xa_matrix (timeout ${TIMEOUT}s) =="
if ! ( cd "$BUILD" && run_with_timeout "$TIMEOUT" ./xa_matrix ); then
	echo "run_xa.sh: xa_matrix FAILED"
	rc=1
fi

echo "== Running xa_recover (timeout ${TIMEOUT}s) =="
# xa_recover re-executes ITSELF as the crashing child, so it needs its own
# absolute path and the home directory via the environment.
XA_SELF="$BUILD/xa_recover"
case "$XA_SELF" in
	/*) ;;			# already absolute
	*) XA_SELF="$(cd "$BUILD" && pwd)/xa_recover" ;;
esac
if ! ( cd "$BUILD" && XA_SELF="$XA_SELF" XA_HOME=XA_RECOVER_HOME \
    run_with_timeout "$TIMEOUT" ./xa_recover ); then
	echo "run_xa.sh: xa_recover FAILED"
	rc=1
fi

if [ "$rc" -eq 0 ]; then
	echo "run_xa.sh: PASS"
else
	echo "run_xa.sh: FAIL (rc=$rc)"
fi
exit "$rc"

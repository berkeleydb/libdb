# test/db/runner_lib.sh -- shared boilerplate for the test/db/run_*.sh runners.
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# WHY THIS EXISTS
#
# The test/db runners each repeated the same four blocks: BUILD defaulting,
# libdb discovery (static-first so macOS does not load a .dylib by its baked-in
# install name), a `timeout`/`gtimeout` wrapper, and a from-empty home setup.
# 19 of 61 carried their own copy of run_with_timeout; many repeated the
# discovery loop and the BUILD default verbatim.  That duplication was a proven
# bug source (D-7 / P11 / P-2): a stale `../../build_unix` BUILD default caused
# repeated "no libdb library under build_unix" false failures.
#
# This file is SOURCED (not executed) by a runner, after it has set its own
# HERE/SRC/etc.  Each function does one job and leaves the runner's compile
# line, arguments and verdict logic untouched, so behaviour is identical.
#
# Usage from a runner:
#     _HERE=$(cd "$(dirname "$0")" && pwd)
#     . "$_HERE/runner_lib.sh"
#     rl_default_build              # sets BUILD
#     rl_find_lib                   # sets LIB, LIBRPATH, EXTRALIBS
#     rl_compile <src> <out>        # the common `cc -g -O1 ... -lpthread` link
#     rl_fresh_home "$HOME_DIR"     # empty the home dir, regions included
#     run_with_timeout "$TIMEOUT" <cmd...>

# ---------------------------------------------------------------------------
# rl_default_build -- set BUILD to the directory that holds a built libdb.
#
# The default is the CURRENT directory when it contains a libdb library (the
# layout `sh ../test/db/run_X.sh` uses from build_unix, and what run_all.sh
# documents).  Only when the cwd has no library do we fall back to the sibling
# ../../build_unix -- the old unconditional default, which went stale and
# produced "no libdb library under build_unix" when the real build was the cwd.
# An explicit $1 or $BUILD still wins.
# ---------------------------------------------------------------------------
rl_default_build() {
	if [ -n "${BUILD:-}" ]; then
		:
	elif [ -f libdb.a ] || [ -d .libs ]; then
		BUILD=.
	else
		BUILD=${_HERE:-$(dirname "$0")}/../../build_unix
	fi
	# Absolutise if it exists, so a later `cd` in the runner does not break it.
	[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
}

# ---------------------------------------------------------------------------
# rl_find_lib -- locate the libdb to link against and its transitive deps.
#
# Sets:
#   LIB       path to libdb (static preferred, shared fallback)
#   LIBRPATH  -Wl,-rpath,... when a shared library was chosen (else empty)
#   EXTRALIBS transitive deps a static libdb needs named explicitly (liburing)
#
# Static first: on macOS a .dylib carries a baked-in install name
# (/usr/local/BerkeleyDB.5.3/lib/...) that takes precedence over -rpath, so a
# shared link runs against an uninstalled path and dyld aborts.  Static linking
# avoids the dynamic loader entirely.
# ---------------------------------------------------------------------------
rl_find_lib() {
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

	EXTRALIBS=""
	for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done
}

# ---------------------------------------------------------------------------
# rl_compile <src> <out> -- the common runner compile+link line.
#
# `cc -g -O1 ${CFLAGS} -I$BUILD <src> $LIB -lpthread $LIBRPATH $EXTRALIBS -o <out>`
# Runners whose link line differs (a utility build, -O2 -pthread, extra -lssl)
# keep their own invocation and do not call this.
# ---------------------------------------------------------------------------
rl_compile() {
	_src=$1
	_out=$2
	echo "Compiling $(basename "$_src") against $LIB"
	"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$_src" "$LIB" \
	    -lpthread $LIBRPATH $EXTRALIBS \
	    -o "$_out"
}

# ---------------------------------------------------------------------------
# rl_fresh_home <dir> -- start from an EMPTY home, regions included.
#
# Removing only *.db left the previous run's __db.00N region files behind, so a
# second run (another arm, another build) attached a region it did not create:
# on FreeBSD that is a PANIC (a pshared mutex whose initialiser has exited), and
# between builds it is a DB_VERSION_MISMATCH.  Either way a false red.
# ---------------------------------------------------------------------------
rl_fresh_home() {
	_dir=$1
	[ -d "$_dir" ] && find "$_dir" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$_dir"
}

# ---------------------------------------------------------------------------
# run_with_timeout <seconds> <cmd...> -- run under a timeout if one exists.
#
# `timeout` is GNU coreutils: present on Linux, absent on stock macOS (where it
# is `gtimeout` if coreutils is installed).  Resolve it once; if neither exists,
# run without a timeout rather than failing with rc=127.
# ---------------------------------------------------------------------------
if command -v timeout >/dev/null 2>&1; then
	_RL_TIMEOUT_CMD="timeout"
elif command -v gtimeout >/dev/null 2>&1; then
	_RL_TIMEOUT_CMD="gtimeout"
else
	_RL_TIMEOUT_CMD=""
fi
run_with_timeout() {
	if [ -n "$_RL_TIMEOUT_CMD" ]; then
		"$_RL_TIMEOUT_CMD" "$@"
	else
		shift	# drop the seconds argument
		"$@"
	fi
}

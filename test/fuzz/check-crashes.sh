#!/bin/sh
# test/fuzz/check-crashes.sh -- replay every committed crash/regression seed
# through its standalone harness and assert a CLEAN (non-crash) outcome.
#
# This is the regression gate for the hardening fixes: each seed once
# crashed the engine (OOB read, SIGFPE, ...) and must now return an error
# instead.  A crash/ASan fault here is a regression.
#
# Leak detection is ON: the DB_PRIVATE region-teardown leak that used to
# fire on these verify / recovery cleanup paths is now fixed, so every
# committed seed replays leak-clean.
# We assert both the memory-safety property (no crash/OOB/FPE) and no leak.
#
# Build target: the standard (--enable-debug) build that run.sh uses.  Do NOT
# run this against an --enable-diagnostic build: there DB_ASSERT / __env_panic
# intentionally abort() on unrecoverable corrupt input (e.g. the pagesize==0
# DB_ASSERT in __memp_fopen, or a recovery-failure panic), which is by-design
# diagnostic behavior, not the OOB/FPE crash class this gate guards against.
#
# libdb ASan instrumentation:
#   Some crash classes (a heap-buffer-overflow / use-after-free / double-free
#   *inside* libdb's own allocations -- e.g. the __part_verify type-confusion
#   OOB write) are only observable when libdb itself is compiled with
#   AddressSanitizer; a harness-only ASan build (libdb.a plain) cannot see
#   them.  If a build_unix built with `CFLAGS=-fsanitize=address` (ASan only,
#   NOT undefined -- UBSan flags libdb's pervasive base+offset pointer idioms)
#   is available, point LIBDB_BUILD at it to catch those.  This gate
#   auto-builds one under build_asan_gate/ when LIBDB_ASAN=1 (default on).
#
# Usage:  ./check-crashes.sh
# Env:    CC, LIBDB_BUILD (see run.sh), LIBDB_ASAN (1=build+use an ASan libdb)
#
# Run from test/fuzz/ inside a `nix develop` shell.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$HERE"

CC=${CC:-clang}
LIBDB_ASAN=${LIBDB_ASAN:-1}

# Build (once) an ASan-instrumented libdb so a memory fault *inside* libdb is
# caught, then link the standalone harnesses against it.  ASan only -- UBSan
# would fire on libdb's legitimate base+offset pointer arithmetic.  The
# harness's own SAN flags in run.sh still add UBSan to the harness .c, so we
# neutralise it for the lib by exporting an ASan-only LIBDB build here.
if [ "$LIBDB_ASAN" = "1" ] && [ -z "${LIBDB_BUILD:-}" ]; then
	GATE_BUILD="$HERE/../../build_asan_gate"
	if [ ! -f "$GATE_BUILD/libdb.a" ]; then
		mkdir -p "$GATE_BUILD"
		( cd "$GATE_BUILD" &&
		  ../dist/configure --enable-debug \
		      CC="$CC" CFLAGS="-fsanitize=address -g -O1" >configure.log 2>&1 &&
		  make -j4 >build.log 2>&1 ) ||
		{ echo "warning: ASan libdb build failed; falling back to plain lib" >&2; }
	fi
	[ -f "$GATE_BUILD/libdb.a" ] && export LIBDB_BUILD="$GATE_BUILD"
fi

# Build the standalone (no-libFuzzer) drivers for every harness once.
FUZZ_STANDALONE=1 ./run.sh build

# The seed's harness is its filename prefix before the first underscore.
seed_harness() {
	case $1 in
	dbfile_*) echo dbfile ;;
	recover_*) echo recover ;;
	api_*)     echo api ;;
	*)         echo "" ;;
	esac
}

rc=0
found=0
for seed in crashes/*.seed; do
	[ -f "$seed" ] || continue
	found=$((found + 1))
	h=$(seed_harness "$(basename "$seed")")
	if [ -z "$h" ]; then
		echo "SKIP (unknown harness): $seed"
		continue
	fi
	if ASAN_OPTIONS=detect_leaks=1 "build/fuzz_${h}_standalone" \
	    "$seed" >/dev/null 2>&1; then
		echo "PASS: $seed ($h)"
	else
		echo "FAIL (crash/fault): $seed ($h)"
		rc=1
	fi
done

[ "$found" -eq 0 ] && echo "no crash seeds found"
exit $rc

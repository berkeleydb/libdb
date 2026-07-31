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
# Usage:  ./check-crashes.sh
# Env:    CC, LIBDB_BUILD (see run.sh)
#
# Run from test/fuzz/ inside a `nix develop` shell.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$HERE"

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

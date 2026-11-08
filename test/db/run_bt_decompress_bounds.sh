#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_bt_decompress_bounds.sh --
#	Build and run bt_decompress_bounds.c, the regression test for the
#	out-of-bounds read in __bam_defdecompress (src/btree/bt_compress.c):
#	the default compressed-btree decoder read each varint's leading byte,
#	and the magic byte at offset 0, BEFORE checking the compressed buffer
#	was long enough, so a corrupt/truncated compressed item (a page from
#	disk) over-reads past its end.
#
#	The test places a tightly-sized compressed item against a PROT_NONE
#	guard page, so the over-read faults on ANY build (not only ASan).
#	Against the defect the child dies on SIGSEGV; with the fix the decoder
#	returns EINVAL and the test prints VERDICT ... PASS.
#
#	__bam_defdecompress only exists when the library was built with
#	compression (HAVE_COMPRESSION).  If this build has no compression the
#	test SKIPs, since the function it guards is not in the library.
#
# Exits non-zero on failure.

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/bt_decompress_bounds.c}
TIMEOUT=${TIMEOUT:-120}

# The decoder under test is compiled only with --enable-compression.  Without
# it the symbol is absent and the test proves nothing, so skip cleanly.
if ! grep -q '^#define[[:space:]]\+HAVE_COMPRESSION[[:space:]]\+1' \
    "$BUILD/db_config.h" 2>/dev/null; then
	echo "run_bt_decompress_bounds.sh: SKIP (library built without --enable-compression)"
	exit 0
fi

# Prefer the STATIC library (same rationale as the other db-tier runners: a
# shared link can resolve to an uninstalled dylib on macOS).
LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
if [ -z "$LIB" ]; then
	for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib; do
		if [ -f "$cand" ]; then LIB="$cand"; break; fi
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "FAIL: no libdb library found under $BUILD"; exit 1; }

# A static libdb needs its transitive deps named explicitly.
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling bt_decompress_bounds against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/bt_decompress_bounds"

# `timeout` is GNU coreutils: present on Linux, absent on stock macOS.
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

echo "Running bt_decompress_bounds (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/bt_decompress_bounds"; then
	echo "run_bt_decompress_bounds.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_bt_decompress_bounds.sh: FAIL (rc=$rc)"
	exit $rc
fi

#!/bin/sh
# A3 (REVIEW-2026-11 Sec 1.5): pin the lock-manager region layout.
#
# Fails if the stripe count, the one-cache-line stripe padding, the key region
# struct sizes, or the region signature move -- so that a layout change is a
# loud, intentional REGION-SIG-CHANGE-INTENDED act, not release-over-release
# drift.  See test/audit/LOCK-REGION-LAYOUT-2026-11.md.
#
# Library discovery follows test/db/run_handle_sizes.sh; the struct-size check
# compiles against the internal headers like test/db/run_s7_stat_truth.sh.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/lock_region_layout.c

# The recorded canonical region signature (LP64 x86_64).  On a deliberate
# region change, update this and the sizes in lock_region_layout.c together, in
# the REGION-SIG-CHANGE-INTENDED commit.
EXPECT_SIG=0xe67a9de8

LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a ; do
	[ -f "$cand" ] && { LIB="$cand" ; break ; }
done
if [ -z "$LIB" ] ; then
	for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib ; do
		[ -f "$cand" ] && { LIB="$cand" ; break ; }
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "run_lock_region_layout.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/lock_region_layout
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$HERE/../../src" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>"$BIN.cc.err" || {
	echo "run_lock_region_layout.sh: FAIL compile"
	sed -n '1,5p' "$BIN.cc.err" | sed 's/^/    /'
	exit 1
}

rc=0
out=$("$BIN" 2>&1)
echo "$out" | sed 's/^/  /'
echo "$out" | grep -q '^VERDICT lock_region_layout PASS' || rc=1

# Region signature check -- platform specific, so this is informative on
# non-x86_64/LP64 hosts (recorded value is the canonical LP64 x86_64 one).
arch=$(uname -m 2>/dev/null)
if [ "$arch" = x86_64 ] || [ "$arch" = amd64 ] ; then
	sig=$(BUILD_DIR="$BUILD" sh "$HERE/../../dist/env_sig_print.sh" 2>/dev/null \
	    | grep -oE '0x[0-9a-fA-F]+' | head -1)
	if [ -z "$sig" ] ; then
		echo "  region signature: could not read it (env_sig_print.sh) -- SKIP"
	elif [ "$sig" = "$EXPECT_SIG" ] ; then
		echo "  region signature: $sig == recorded $EXPECT_SIG"
	else
		echo "  region signature: $sig != recorded $EXPECT_SIG -- the region"
		echo "  layout changed.  If intentional, update EXPECT_SIG here and the"
		echo "  sizes in lock_region_layout.c in the REGION-SIG-CHANGE-INTENDED"
		echo "  commit; otherwise a field moved in DB_LOCKREGION by accident."
		rc=1
	fi
else
	echo "  region signature: $arch is not x86_64/LP64 -- SKIP (recorded value"
	echo "  is the canonical LP64 x86_64 signature; structural invariants above"
	echo "  are checked on every platform)"
fi

if [ "$rc" -eq 0 ] ; then
	echo "run_lock_region_layout.sh: PASS"
else
	echo "run_lock_region_layout.sh: FAIL"
fi
exit $rc

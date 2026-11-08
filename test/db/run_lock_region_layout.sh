#!/bin/sh
# A3 (REVIEW-2026-11 Sec 1.5): pin the lock-manager region layout.
#
# Fails if the stripe count, the one-cache-line stripe padding or the key
# region struct sizes move -- so that a layout change is a
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

# No region-signature check here.  The signature depends on the platform AND
# the configure options (a plain Debian build prints 0x841f4ea8, an
# --enable-diagnostic --enable-test one 0xe67a9de8), so a single recorded
# constant fails every configuration but one; it did, on CI's plain jobs.
# The signature is gated where it can be compared like with like: the
# "region signature" CI job (PR vs base, same configure) and the LTS L1 gate
# (QUALIFICATION-MATRIX.md).  This runner pins the layout invariants above,
# which hold in every configuration.

if [ "$rc" -eq 0 ] ; then
	echo "run_lock_region_layout.sh: PASS"
else
	echo "run_lock_region_layout.sh: FAIL"
fi
exit $rc

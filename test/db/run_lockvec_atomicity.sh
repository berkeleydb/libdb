#!/bin/sh
# A4 (REVIEW-2026-11 Sec 1.5): pin the DB_ENV->lock_vec atomicity contract.
#
# Berkeley DB's lock-manager design note (sources/historical/lock-Design.txt,
# bug #553) says that once the region is no longer a single mutex, lock_vec is
# no longer atomic across its elements -- only element order is kept, which is
# what lock coupling needs.  libdb's default is fine-grained (lk_partitions =
# 10 * ncpu), so this is the live configuration.
#
# This runner builds lockvec_atomicity.c two ways and asserts the contract:
#   * NORMAL: arms a CONFIG_TEST-only inter-element yield hook, so the vector's
#     non-atomicity across elements is observable (scenario 1 passes) and
#     coupling order holds (scenario 2 passes) -> PASS.
#   * SABOTAGE (-DLOCKVEC_NO_HOOK): same lk_partitions, but the hook is NOT
#     armed, so the real inter-element window is microseconds and the probe
#     cannot catch it -- scenario 1's mid-vector observation fails -> the
#     binary exits FAIL.  The runner asserts the sabotage build FAILS, which is
#     the test's teeth: it proves the normal PASS depends on catching the
#     real inter-element gap, not on luck.
#
# Library discovery follows test/db/run_handle_sizes.sh.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/lockvec_atomicity.c

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
[ -n "$LIB" ] || { echo "run_lockvec_atomicity.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

# The inter-element hook is CONFIG_TEST-only (--enable-test).  Without it the
# library has no hook, the normal arm cannot open the window it is meant to
# observe, and its FAIL would say nothing about lock_vec.  SKIP instead.
if ! grep -q '^#define[[:space:]]\{1,\}CONFIG_TEST[[:space:]]\{1,\}1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "run_lockvec_atomicity.sh: SKIP library built without --enable-test (no inter-element hook)"
	exit 0
fi

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

d=${TMPDIR:-/tmp}/lockvec_$$
rm -f "$d"/* 2>/dev/null
mkdir -p "$d"
cd "$d" || { echo "run_lockvec_atomicity.sh: FAIL cannot cd $d" ; exit 1 ; }

build_one() {	# $1 = extra defines, $2 = output binary
	"${CC:-cc}" -g -O1 ${CFLAGS:-} $1 -I"$BUILD" -I"$HERE/../../src" \
	    "$SRC" "$LIB" \
	    -lpthread $LIBRPATH $EXTRALIBS -o "$2" 2>"$2.cc.err"
}

rc=0

# ---- NORMAL arm: hook armed.  Must PASS, and scenario 1 must observe A FREE
#      mid-vector (the documented non-atomicity).
if ! build_one "" "$d/normal" ; then
	echo "run_lockvec_atomicity.sh: FAIL compile (normal)"
	sed -n '1,5p' "$d/normal.cc.err" | sed 's/^/    /'
	exit 1
fi
echo "-- normal (hook armed) --"
nout=$("$d/normal" 2>&1) ; nrc=$?
echo "$nout" | sed 's/^/  /'
if [ "$nrc" -ne 0 ] ; then
	echo "  normal arm FAIL (vector should be non-atomic + coupling ordered)"
	rc=1
fi
echo "$nout" | grep -q 'scenario 1 (non-atomic.*A FREE during vector' || {
	echo "  normal arm did NOT observe A free mid-vector -- contract not shown"
	rc=1
}

# ---- SABOTAGE arm: hook NOT armed.  The real inter-element window is
#      microseconds, so scenario 1 must observe A HELD (cannot catch the gap).
#      This is the teeth: the SAME scenario flips from FREE to HELD purely
#      because the hook stopped firing, proving the normal arm's "A FREE"
#      is a genuine detection of the inter-element window, not timing luck.
#      A change that made __lock_vec hold a lock across all elements would make
#      even the hooked (normal) arm observe A HELD -> normal arm FAIL -> caught.
if ! build_one "-DLOCKVEC_NO_HOOK" "$d/sabotage" ; then
	echo "run_lockvec_atomicity.sh: FAIL compile (sabotage)"
	sed -n '1,5p' "$d/sabotage.cc.err" | sed 's/^/    /'
	exit 1
fi
echo "-- sabotage (-DLOCKVEC_NO_HOOK, hook not armed) --"
sout=$("$d/sabotage" 2>&1) ; src=$?
echo "$sout" | sed 's/^/  /'
if [ "$src" -ne 0 ] ; then
	echo "  sabotage arm binary FAILED unexpectedly (expected A HELD/PASS)"
	rc=1
fi
if echo "$sout" | grep -q 'scenario 1 (NO_HOOK): A HELD' ; then
	echo "  teeth confirmed: without the hook the gap is NOT observable"
	echo "  (scenario 1 flips FREE->HELD), so the normal arm's observation"
	echo "  is a real detection of the inter-element window."
else
	echo "  sabotage arm OBSERVED the gap without the hook -- the normal PASS"
	echo "  would then be timing luck, not a real observation."
	rc=1
fi

rm -f "$d"/normal "$d"/sabotage "$d"/*.cc.err
rmdir "$d" 2>/dev/null

if [ "$rc" -eq 0 ] ; then
	echo "run_lockvec_atomicity.sh: PASS"
else
	echo "run_lockvec_atomicity.sh: FAIL"
fi
exit $rc

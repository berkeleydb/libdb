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
#   * NO-HOOK (-DLOCKVEC_NO_HOOK): same lk_partitions, hook not armed, run
#     NOHOOK_RUNS times (default 10).  The bare window is microseconds wide,
#     so the probe should catch it in a minority of runs; a majority would
#     mean the normal arm's observation does not depend on the hook.
#     Coupling order must hold in every run.
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

# Link what the library was configured with (liburing, ...), as leak-run.sh
# does; pkg-config alone misses liburing where it has no .pc file.
EXTRALIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$BUILD/Makefile" 2>/dev/null | head -1)
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

# ---- NO-HOOK arm: same scenarios, hook not armed.  The inter-element
#      window is real (no region mutex is held between elements), but only
#      microseconds wide, so the probe should seldom catch it.  The arm must
#      not demand "never": on a fast many-core host the probe does catch the
#      bare window now and then (4 of 40 runs on a 32-vCPU c6i.8xlarge), and
#      that is the very non-atomicity this test documents, not a defect.  It
#      demands the hooked arm see the gap far more often than the bare
#      window does, which shows the normal arm's FREE comes from the hook and
#      not from luck.  The engine-side teeth are separate: a change that made
#      __lock_vec atomic would make the hooked arm see A HELD -> FAIL.
if ! build_one "-DLOCKVEC_NO_HOOK" "$d/sabotage" ; then
	echo "run_lockvec_atomicity.sh: FAIL compile (sabotage)"
	sed -n '1,5p' "$d/sabotage.cc.err" | sed 's/^/    /'
	exit 1
fi
NOHOOK_RUNS=${NOHOOK_RUNS:-10}
caught=0
i=0
while [ "$i" -lt "$NOHOOK_RUNS" ] ; do
	i=$((i + 1))
	sout=$("$d/sabotage" 2>&1)
	echo "$sout" | grep -q 'scenario 1 (NO_HOOK): A FREE' && caught=$((caught + 1))
	echo "$sout" | grep -q 'scenario 2 (coupling.*A HELD' || {
		echo "  no-hook run $i: coupling order broken (A free mid-vector)"
		rc=1
	}
done
echo "-- no-hook (-DLOCKVEC_NO_HOOK): bare window caught in $caught of $NOHOOK_RUNS runs --"
# The hooked arm caught it in its one run (checked above).  Without the hook,
# a majority of catches would mean the hook adds nothing.
if [ $((caught * 2)) -gt "$NOHOOK_RUNS" ] ; then
	echo "  the bare window was caught in most runs: the hook is not what the"
	echo "  normal arm observes, so its FREE proves nothing."
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

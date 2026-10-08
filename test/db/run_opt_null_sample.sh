#!/bin/sh
# M1 regression: __memp_fget_opt_valid() must answer 0 for a sample whose bhp
# is NULL, rather than dereferencing it.
#
# Deliberately not a concurrency test. The defect is a missing branch, so it is
# checkable with a direct call -- which also means this runs in milliseconds on
# any machine, instead of depending on an eviction race that did not reproduce
# below 64 vCPUs.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-"$HERE/../../build_unix"}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/opt_null_sample.c

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
[ -n "$LIB" ] || { echo "run_opt_null_sample.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/opt_null_sample
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$HERE/../../src" \
    "$SRC" "$LIB" -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_opt_null_sample.sh: FAIL compile" ; exit 1 ; }
# A stale binary would be a false pass.
test -x "$BIN" || { echo "run_opt_null_sample.sh: FAIL no binary produced" ; exit 1 ; }

out=$("$BIN" 2>&1)
rc=$?
echo "$out" | sed 's/^/  /'

# rc is checked as well as the verdict: without the guard this SIGSEGVs, and a
# signal death produces no verdict line at all.
if [ "$rc" -ge 128 ] ; then
	echo "run_opt_null_sample.sh: FAIL died on signal $((rc - 128)) -- the NULL guard in BH_SAMPLE_VALID is missing (M1)"
	exit 1
fi

v=$(echo "$out" | grep '^VERDICT opt_null_sample' | head -1)
[ -n "$v" ] || { echo "run_opt_null_sample.sh: FAIL no VERDICT line (exit $rc)" ; exit 1 ; }
case "$v" in
*PASS*)	echo "run_opt_null_sample.sh: PASS" ; exit 0 ;;
esac
echo "run_opt_null_sample.sh: FAIL"
exit 1

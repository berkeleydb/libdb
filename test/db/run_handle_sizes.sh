#!/bin/sh
# A1 regression: pin the size of every public handle struct.
#
# sizeof(DB) grew 1456 -> 1744 bytes under an unchanged soname and nothing
# noticed, because the abidiff job only ever compares a PR head against the
# previous release tag.  This asserts the sizes directly, so the next change
# fails in the commit that causes it.
#
# Library discovery follows test/db/run_qam_readpath_bound.sh rather than being
# reinvented.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-"$HERE/../../build_unix"}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/handle_sizes.c

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
[ -n "$LIB" ] || { echo "run_handle_sizes.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/handle_sizes
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_handle_sizes.sh: FAIL compile" ; exit 1 ; }
# A stale binary would give a false pass.
test -x "$BIN" || { echo "run_handle_sizes.sh: FAIL no binary produced" ; exit 1 ; }

out=$("$BIN" 2>&1)
echo "$out" | sed 's/^/  /'
v=$(echo "$out" | grep '^VERDICT handle_sizes' | head -1)
[ -n "$v" ] || { echo "run_handle_sizes.sh: FAIL no VERDICT line" ; exit 1 ; }

case "$v" in
*PASS*)	echo "run_handle_sizes.sh: PASS" ; exit 0 ;;
*SKIP*)	echo "run_handle_sizes.sh: SKIP" ; exit 0 ;;
esac
echo "run_handle_sizes.sh: FAIL"
exit 1

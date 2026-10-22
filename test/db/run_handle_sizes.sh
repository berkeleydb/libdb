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
# $1, else $BUILD (what run_all.sh sets), else the in-tree build_unix.
# Honouring $BUILD matters: with only $1, run_all.sh -- which passes no
# argument -- silently tested a stale ../../build_unix instead of the build
# under test, or failed outright with a sibling build dir.
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/handle_sizes.c

# Registered twice in test/MANIFEST: in the typed tier (TIER=typed, set by
# run_all.sh) it must be measuring an --enable-typed build, or its PASS would
# say nothing about D2 -- so a default build there is a FAIL, not a skip.
if [ "${TIER:-db}" = typed ] &&
    ! grep -q '^#define HAVE_TYPED 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "VERDICT handle_sizes FAIL not-a-typed-build ($BUILD)"
	echo "run_handle_sizes.sh: FAIL"
	exit 1
fi

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

# Measure the struct the way the LIBRARY was compiled: struct __db is
# library-allocated and opaque (A1), so the size that matters is the one the
# library's own .c files see.  Any HAVE_* that the build set and that could
# reach a public struct's layout must be defined for this consumer too, or a
# field added under such a guard (e.g. #ifdef HAVE_TYPED, D2's forbidden case)
# would grow the library's struct while this test, compiling db.h without the
# macro, still read the old size -- a vacuous green.  HAVE_TYPED is the only
# such guard today; it legitimately leaves sizeof(DB) unchanged (D2), so this
# flag changes nothing on a correct typed build and catches a wrong one.
GUARDS=""
grep -q '^#define HAVE_TYPED 1' "$BUILD/db_config.h" 2>/dev/null &&
    GUARDS="$GUARDS -DHAVE_TYPED"

BIN=$BUILD/handle_sizes
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} $GUARDS -I"$BUILD" "$SRC" "$LIB" \
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

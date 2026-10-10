#!/bin/sh
# F6 regression: a mutex slot reclaimed by __mutex_failchk must not be freed a
# second time by its owner.
#
# __mutex_failchk frees by SLOT INDEX and cannot clear the owner's db_mutex_t,
# so a TXN_DETAIL keeps a stale mvcc_mtx id; __txn_env_refresh then frees it
# again on env close. In a diagnostic build that trips the DB_MUTEX_ALLOCATED
# assert; in a production build it silently links the slot into the mutex free
# list twice.
#
# This runner checks the EXIT SIGNAL as well as the verdict, because the
# unguarded failure is an abort inside DB_ENV->close -- which produces no
# verdict line at all.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
# $1, else $BUILD (what run_all.sh sets), else the in-tree build_unix.
# Honouring $BUILD matters: with only $1, run_all.sh -- which passes no
# argument -- silently tested a stale ../../build_unix instead of the build
# under test, or failed outright with a sibling build dir.
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/mvcc_mtx_doublefree.c

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
[ -n "$LIB" ] || { echo "run_mvcc_mtx_doublefree.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/mvcc_mtx_doublefree
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$HERE/../../src" \
    "$SRC" "$LIB" -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_mvcc_mtx_doublefree.sh: FAIL compile" ; exit 1 ; }
# A stale binary would be a false pass.
test -x "$BIN" || { echo "run_mvcc_mtx_doublefree.sh: FAIL no binary produced" ; exit 1 ; }

# The driver needs its env home to exist, and a FRESH one: a leftover region
# from a previous run makes DB_RECOVER panic with "No such file or directory".
work=$BUILD/TESTDIR_f6
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"
out=$(cd "$BUILD" && "$BIN" 2>&1)
rc=$?
echo "$out" | sed 's/^/  /'

# rc is checked as well as the verdict: without the guard this SIGSEGVs, and a
# signal death produces no verdict line at all.
if [ "$rc" -ge 128 ] ; then
	echo "run_mvcc_mtx_doublefree.sh: FAIL died on signal $((rc - 128)) -- the NULL guard in BH_SAMPLE_VALID is missing (M1)"
	exit 1
fi

if [ "$rc" -ge 128 ] ; then
	echo "run_mvcc_mtx_doublefree.sh: FAIL died on signal $((rc - 128)) -- the double free is present (F6); expect the DB_MUTEX_ALLOCATED assert in __mutex_free_int"
	exit 1
fi

# A run in which failchk reclaimed NOTHING cannot have exercised the double
# free, so it must not report PASS. An earlier version of this test did exactly
# that: it passed with the guard REMOVED, because the child did a read-only get
# and mvcc_mtx is allocated lazily only on a dirty/create fetch
# (mp_fget.c:267), so no mutex existed for failchk to reclaim. BDB2017 is
# printed once per reclaimed slot and is the only positive evidence that the
# scenario was built.
n2017=$(echo "$out" | grep -c 'BDB2017' || true)
if [ "${n2017:-0}" -lt 1 ] ; then
	echo "run_mvcc_mtx_doublefree.sh: SKIP failchk reclaimed no mutexes (0 BDB2017 lines), so the F6 scenario was never constructed -- this run proves nothing either way"
	exit 0
fi

v=$(echo "$out" | grep '^VERDICT f6_mvcc_doublefree' | head -1)
[ -n "$v" ] || { echo "run_mvcc_mtx_doublefree.sh: FAIL no VERDICT line (exit $rc)" ; exit 1 ; }
case "$v" in
*PASS*)	echo "run_mvcc_mtx_doublefree.sh: PASS" ; exit 0 ;;
esac
echo "run_mvcc_mtx_doublefree.sh: FAIL"
exit 1

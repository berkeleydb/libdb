#!/bin/sh
# RFC 0015 S4 regression: td->mvcc_mtx retained across MVCC writer txns.
#
# The binary runs N serial MVCC writers and reports the mutex-region latch
# (MUTEX_SYSTEM_LOCK) acquisitions over the window plus the region cache depth
# (free_mvcc_n).  Run it twice:
#   REUSE   (default)             : cache populated, few acquisitions.
#   NOREUSE (DB_NO_MVCC_MTX_REUSE): cache empty, ~2 acquisitions per writer.
# PASS requires REUSE acquisitions strictly fewer than NOREUSE's, and the two
# per-run invariants (cache populated iff reuse on).
#
# Single-threaded and deterministic.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/s4_mvcc_mtx_reuse.c

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
[ -n "$LIB" ] || { echo "run_s4_mvcc_mtx_reuse.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/s4_mvcc_mtx_reuse
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$HERE/../../src" \
    "$SRC" "$LIB" -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_s4_mvcc_mtx_reuse.sh: FAIL compile" ; exit 1 ; }
test -x "$BIN" || { echo "run_s4_mvcc_mtx_reuse.sh: FAIL no binary produced" ; exit 1 ; }

run_one() {
	# $1 = extra env setting ("" or "DB_NO_MVCC_MTX_REUSE=1")
	work=$BUILD/TESTDIR_s4
	[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$work"
	(cd "$BUILD" && env $1 "$BIN" 2>&1)
}

out_r=$(run_one "")
rc_r=$?
out_n=$(run_one "DB_NO_MVCC_MTX_REUSE=1")
rc_n=$?
echo "$out_r" | sed 's/^/  /'
echo "$out_n" | sed 's/^/  /'

if [ "$rc_r" -ge 128 ] || [ "$rc_n" -ge 128 ] ; then
	echo "run_s4_mvcc_mtx_reuse.sh: FAIL died on signal"
	exit 1
fi

vr=$(echo "$out_r" | grep '^VERDICT s4_mvcc_mtx_reuse' | head -1)
vn=$(echo "$out_n" | grep '^VERDICT s4_mvcc_mtx_reuse' | head -1)
case "$vr$vn" in
*SKIP*)	echo "run_s4_mvcc_mtx_reuse.sh: SKIP $vr" ; exit 0 ;;
esac
case "$vr" in *PASS*) ;; *) echo "run_s4_mvcc_mtx_reuse.sh: FAIL (reuse run)"; exit 1 ;; esac
case "$vn" in *PASS*) ;; *) echo "run_s4_mvcc_mtx_reuse.sh: FAIL (noreuse run)"; exit 1 ;; esac

# Cross-run: reuse must take strictly fewer mutex-region acquisitions.
acq_r=$(echo "$out_r" | sed -n 's/.*acquisitions over [0-9]* MVCC writers = \([0-9]*\) .*/\1/p' | head -1)
acq_n=$(echo "$out_n" | sed -n 's/.*acquisitions over [0-9]* MVCC writers = \([0-9]*\) .*/\1/p' | head -1)
if [ -z "$acq_r" ] || [ -z "$acq_n" ] ; then
	echo "run_s4_mvcc_mtx_reuse.sh: FAIL could not parse acquisition counts"
	exit 1
fi
echo "  cross-run: REUSE acq=$acq_r  NOREUSE acq=$acq_n"
if [ "$acq_r" -ge "$acq_n" ] ; then
	echo "run_s4_mvcc_mtx_reuse.sh: FAIL reuse ($acq_r) did not reduce "\
"mutex-region acquisitions below noreuse ($acq_n)"
	exit 1
fi
echo "run_s4_mvcc_mtx_reuse.sh: PASS"
exit 0

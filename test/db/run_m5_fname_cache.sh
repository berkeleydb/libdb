#!/bin/sh
# RFC 0015 M5 regression: dbp->mutex is off the per-cursor path.
#
# One transaction opens NCURSOR cursors on one logged handle.  The binary
# reports dbp->mutex acquisitions over that window.  Run it twice:
#   CACHE   (default)            : only the first cursor locks (dedup lock-free).
#   NOCACHE (DB_NO_FNAME_CACHE)  : every cursor locks (pre-M5).
# PASS requires CACHE acquisitions strictly fewer than NOCACHE's, plus the two
# per-run invariants (NOCACHE ~one lock/cursor, CACHE far fewer).
#
# Single-threaded and deterministic.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/m5_fname_cache.c

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
[ -n "$LIB" ] || { echo "run_m5_fname_cache.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/m5_fname_cache
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$HERE/../../src" \
    "$SRC" "$LIB" -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_m5_fname_cache.sh: FAIL compile" ; exit 1 ; }
test -x "$BIN" || { echo "run_m5_fname_cache.sh: FAIL no binary produced" ; exit 1 ; }

run_one() {
	work=$BUILD/TESTDIR_m5
	[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$work"
	(cd "$BUILD" && env $1 "$BIN" 2>&1)
}

out_c=$(run_one "")
rc_c=$?
out_n=$(run_one "DB_NO_FNAME_CACHE=1")
rc_n=$?
echo "$out_c" | sed 's/^/  /'
echo "$out_n" | sed 's/^/  /'

if [ "$rc_c" -ge 128 ] || [ "$rc_n" -ge 128 ] ; then
	echo "run_m5_fname_cache.sh: FAIL died on signal"
	exit 1
fi

vc=$(echo "$out_c" | grep '^VERDICT m5_fname_cache' | head -1)
vn=$(echo "$out_n" | grep '^VERDICT m5_fname_cache' | head -1)
case "$vc$vn" in *SKIP*) echo "run_m5_fname_cache.sh: SKIP $vc$vn" ; exit 0 ;; esac
case "$vc" in *PASS*) ;; *) echo "run_m5_fname_cache.sh: FAIL (cache run)"; exit 1 ;; esac
case "$vn" in *PASS*) ;; *) echo "run_m5_fname_cache.sh: FAIL (nocache run)"; exit 1 ;; esac

acq_c=$(echo "$out_c" | sed -n 's/.*acquisitions over [0-9]* cursors = \([0-9]*\).*/\1/p' | head -1)
acq_n=$(echo "$out_n" | sed -n 's/.*acquisitions over [0-9]* cursors = \([0-9]*\).*/\1/p' | head -1)
if [ -z "$acq_c" ] || [ -z "$acq_n" ] ; then
	echo "run_m5_fname_cache.sh: FAIL could not parse acquisition counts"
	exit 1
fi
echo "  cross-run: CACHE acq=$acq_c  NOCACHE acq=$acq_n"
if [ "$acq_c" -ge "$acq_n" ] ; then
	echo "run_m5_fname_cache.sh: FAIL cache ($acq_c) did not reduce "\
"dbp->mutex acquisitions below nocache ($acq_n)"
	exit 1
fi
echo "run_m5_fname_cache.sh: PASS"
exit 0

#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_r2_fuzz.sh -- randomised R2 duplicate-set revive stress.
#
# A case-insensitive btree (DB_DUP / DB_DUPSORT) driven with random pins,
# puts of equal-comparing spellings, and deletes.  After every op the driver
# requires DB->verify to pass, the live record count per case-folded key to
# match a model, and all live records of a folded key to share one spelling
# (one duplicate set).  This catches the first R2 fix, which made two
# equal-comparing keys into two non-dup key items (DB_VERIFY_BAD).
#
# ~SEEDS seeds x OPS ops, ~30 s.  Any seed FAIL fails the runner.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=r2_fuzz
SRC=$HERE/$NAME.c
SEEDS=${R2_FUZZ_SEEDS:-60}
OPS=${R2_FUZZ_OPS:-300}

# Library discovery follows run_r2_dup_revive.sh.
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
[ -n "$LIB" ] || { echo "VERDICT $NAME FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

CC=${CC:-cc}
bin=$BUILD/${NAME}
rm -f "$bin"
"$CC" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$bin" 2>&1 || {
	echo "VERDICT $NAME FAIL compile" ; exit 1 ; }
test -x "$bin" || { echo "VERDICT $NAME FAIL no binary" ; exit 1 ; }

work=$BUILD/TESTDIR_${NAME}
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"

fails=0
seed=1
while [ "$seed" -le "$SEEDS" ] ; do
	out=$("$bin" "$seed" "$OPS" "$work" 2>&1)
	rc=$?
	case "$out" in
	*" FAIL"*) echo "  $out" ; fails=$((fails + 1)) ;;
	esac
	[ "$rc" -ne 0 ] && [ $((fails)) -eq 0 ] && \
	    { echo "  seed $seed exited $rc: $out" ; fails=$((fails + 1)) ; }
	seed=$((seed + 1))
done

echo "  ran $SEEDS seeds x $OPS ops, $fails FAIL"
if [ "$fails" -eq 0 ] ; then
	echo "VERDICT $NAME PASS"
	exit 0
fi
echo "VERDICT $NAME FAIL $fails seed(s)"
exit 1

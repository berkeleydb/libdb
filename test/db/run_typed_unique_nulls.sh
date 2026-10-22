#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_typed_unique_nulls.sh -- RFC 0014 commit 7 test (spec §7.2 typed_unique_nulls).
#
# Exercises db_set_schema validation (§3.4), the fingerprint self-checks
# (§5.1), the db_get_schema deep copy (§1.5), and the after-open refusal.
# Typed tier: it needs an --enable-typed library (the symbols live only
# there), so a default build is a FAIL, not a skip -- a skip would be a
# vacuous green.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=typed_unique_nulls
SRC=$HERE/$NAME.c

if ! grep -q '^#define HAVE_TYPED 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "VERDICT $NAME FAIL not-a-typed-build ($BUILD)"
	exit 1
fi

# Library discovery follows test/db/run_qam_readpath_bound.sh.
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

BIN=$BUILD/$NAME
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "VERDICT $NAME FAIL compile" ; exit 1 ; }
test -x "$BIN" || { echo "VERDICT $NAME FAIL no binary produced" ; exit 1 ; }

work=$BUILD/TESTDIR_$NAME
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"

TO=""
command -v timeout >/dev/null 2>&1 && TO="timeout 120"
out=$(cd "$work" && $TO "$BIN" . 2>&1)
rc=$?
echo "$out" | sed 's/^/  /'
[ "$rc" -eq 124 ] && { echo "VERDICT $NAME FAIL hung (killed at 120s)" ; exit 1 ; }
[ "$rc" -ge 128 ] && { echo "VERDICT $NAME FAIL died on signal $((rc - 128))" ; exit 1 ; }

v=$(echo "$out" | grep "^VERDICT $NAME " | head -1)
[ -n "$v" ] || { echo "VERDICT $NAME FAIL no VERDICT line (rc=$rc)" ; exit 1 ; }
case "$v" in
*" PASS"*) [ "$rc" -eq 0 ] && exit 0 ;;
esac
echo "run_$NAME.sh: FAIL (rc=$rc)"
exit 1

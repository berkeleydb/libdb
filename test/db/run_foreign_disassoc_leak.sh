#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_foreign_disassoc_leak.sh -- RFC 0014 §8.2 regression.
#
# __db_disassociate_foreign leaked one DB_FOREIGN_INFO per DB->close of a
# secondary that has a foreign database.  The driver counts allocations
# through db_env_set_func_malloc/free and prints exactly one VERDICT line;
# absence of that line is failure, so rc alone never judges.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=foreign_disassoc_leak
SRC=$HERE/$NAME.c

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
# A stale binary would be a false pass.
test -x "$BIN" || { echo "VERDICT $NAME FAIL no binary produced" ; exit 1 ; }

# A fresh home: a leftover region from another build is a false red.
work=$BUILD/TESTDIR_$NAME
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"

TO=""
command -v timeout >/dev/null 2>&1 && TO="timeout 120"
out=$($TO "$BIN" "$work" 2>&1)
rc=$?
echo "$out" | sed 's/^/  /'
[ "$rc" -eq 124 ] && { echo "VERDICT $NAME FAIL hung (killed at 120s)" ; exit 1 ; }

v=$(echo "$out" | grep "^VERDICT $NAME " | head -1)
[ -n "$v" ] || { echo "VERDICT $NAME FAIL no VERDICT line (rc=$rc)" ; exit 1 ; }
case "$v" in
*" PASS"*) [ "$rc" -eq 0 ] && exit 0 ;;
esac
echo "run_$NAME.sh: FAIL (rc=$rc)"
exit 1

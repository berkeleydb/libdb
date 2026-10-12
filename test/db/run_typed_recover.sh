#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_typed_recover.sh -- RFC 0014 commit 9 test (spec §7.2 typed_recover,
# D5/C14).
#
# The typed build's db_recover recovers a typed environment with NO codecs
# registered: recovery opens the typed subdatabases but calls no comparator
# or constraint (the §3.5 open hook is skipped for DB_AM_RECOVER handles).
# The test kill -9's a writer mid-txn, runs the REAL db_recover utility, then
# reopens with the schema and checks the surviving rows against a model.
# Typed tier: needs an --enable-typed library, so a default build is a FAIL,
# not a skip.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=typed_recover
SRC=$HERE/$NAME.c

if ! grep -q '^#define HAVE_TYPED 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "VERDICT $NAME FAIL not-a-typed-build ($BUILD)"
	exit 1
fi

# The db_recover utility built in this BUILD dir (libtool wrapper, or .libs).
RECOVER=""
for cand in "$BUILD"/db_recover "$BUILD"/.libs/db_recover ; do
	[ -x "$cand" ] && { RECOVER="$cand" ; break ; }
done
[ -n "$RECOVER" ] || { echo "VERDICT $NAME FAIL no db_recover under $BUILD" ; exit 1 ; }

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
out=$(cd "$work" && $TO "$BIN" . "$RECOVER" 2>&1)
rc=$?
echo "$out" | sed 's/^/  /'
[ "$rc" -eq 124 ] && { echo "VERDICT $NAME FAIL hung (killed at 120s)" ; exit 1 ; }
# The test itself forks a child that SIGKILLs; that is the child, not $BIN, so
# $BIN's own exit code is what matters here.
[ "$rc" -ge 128 ] && { echo "VERDICT $NAME FAIL died on signal $((rc - 128))" ; exit 1 ; }

v=$(echo "$out" | grep "^VERDICT $NAME " | head -1)
[ -n "$v" ] || { echo "VERDICT $NAME FAIL no VERDICT line (rc=$rc)" ; exit 1 ; }
case "$v" in
*" PASS"*) [ "$rc" -eq 0 ] && exit 0 ;;
esac
echo "run_$NAME.sh: FAIL (rc=$rc)"
exit 1

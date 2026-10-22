#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_foreign_reassoc.sh -- RFC 0014 §8.1 regression.
#
# A second DB->associate_foreign on an already-associated secondary returned
# EINVAL but left its DB_FOREIGN_INFO linked into the second foreign
# database's f_primaries.  The driver runs one arm per hazard, each in its own
# process so a crash in one cannot mask another:
#   i    wrong cascade         ii   FK check silently disabled
#   iii  use-after-free (a crash: ASan report, or a fault on the poisoned
#        handle in a non-ASan build -- either way no PASS line)
# Each arm must print its own "VERDICT foreign_reassoc PASS arm X".

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=foreign_reassoc
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

# A sanitizer-built libdb (the lockmatrix / CI typed-job configuration) needs
# the driver built with the same compiler and -fsanitize, or it does not link.
# Take both from the build's Makefile so arm iii is judged by ASan there.
TCC=${CC:-cc}
SAN=""
if [ -f "$BUILD/Makefile" ] ; then
	SAN=$(sed -n 's/^CFLAGS[	 ]*=.*\(-fsanitize=[^ 	]*\).*/\1/p' "$BUILD/Makefile" | head -1)
	[ -n "$SAN" ] && [ -f "$BUILD/config.log" ] &&
	    TCC=$(sed -n "s/^CC='\\(.*\\)'\$/\\1/p" "$BUILD/config.log" | head -1)
	[ -n "$TCC" ] || TCC=${CC:-cc}
fi

BIN=$BUILD/$NAME
rm -f "$BIN"
"$TCC" -g -O1 ${CFLAGS:-} $SAN -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "VERDICT $NAME FAIL compile" ; exit 1 ; }
test -x "$BIN" || { echo "VERDICT $NAME FAIL no binary produced" ; exit 1 ; }
echo "driver: $TCC ${SAN:-(no sanitizer)}"

# Arm iii's expected failure is a crash; do not litter the build dir with cores.
ulimit -c 0 2>/dev/null
TO=""
command -v timeout >/dev/null 2>&1 && TO="timeout 120"
rc=0
for a in i ii iii ; do
	work=$BUILD/TESTDIR_${NAME}_$a
	[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$work"
	out=$(ASAN_OPTIONS=${ASAN_OPTIONS:-detect_leaks=0} $TO "$BIN" "$work" "$a" 2>&1)
	arc=$?
	echo "$out" | sed 's/^/  /'
	if echo "$out" | grep -q "^VERDICT $NAME PASS arm $a\$" && [ "$arc" -eq 0 ] ; then
		echo "  arm $a: PASS"
	elif [ "$arc" -eq 124 ] ; then
		echo "VERDICT $NAME FAIL arm $a hung (killed at 120s)" ; rc=1
	elif echo "$out" | grep -q 'ERROR: AddressSanitizer' ; then
		echo "VERDICT $NAME FAIL arm $a: AddressSanitizer report (rc=$arc)" ; rc=1
	elif [ "$arc" -ge 128 ] ; then
		echo "VERDICT $NAME FAIL arm $a died on signal $((arc - 128))" ; rc=1
	else
		echo "VERDICT $NAME FAIL arm $a (rc=$arc)" ; rc=1
	fi
done
[ "$rc" -eq 0 ] && echo "VERDICT $NAME PASS all three arms"
exit $rc

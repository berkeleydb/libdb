#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_r2_dup_revive.sh -- R2 regression (RFC 0014 §8.3, KNOWN-ISSUES R2).
#
# An equal-key put over a DUPLICATE set whose members are all deleted and
# pinned revived the set under the DELETED key's bytes (finishes R1).  The
# driver judges every case on the stored key bytes and the live record count
# and prints one VERDICT line.  Two arms:
#   normal   -- the build's library: must PASS.
#   sabotage -- bt_cursor.o rebuilt with -DR2_REVIVE_SABOTAGE (the redirect
#               disabled): must FAIL, proving the test has teeth.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=r2_dup_revive
SRC=$HERE/$NAME.c

# The source root holds src/btree/bt_cursor.c (BUILD is .../b under it, or a
# sibling); find it from the build dir's configured layout.
SRCROOT=""
for cand in "$BUILD/.." "$BUILD/../.." "$HERE/../.." ; do
	[ -f "$cand/src/btree/bt_cursor.c" ] && \
	    { SRCROOT=$(cd "$cand" && pwd) ; break ; }
done

# Library discovery follows run_r1_revive.sh.
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
rc_all=0

run_arm() {		# $1 = arm name, $2 = extra object (or empty)
	arm=$1
	extra=$2
	bin=$BUILD/${NAME}_$arm
	rm -f "$bin"
	"$CC" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" $extra "$LIB" \
	    -lpthread $LIBRPATH $EXTRALIBS -o "$bin" 2>&1 || {
		echo "VERDICT $NAME FAIL compile ($arm)" ; exit 1 ; }
	test -x "$bin" || { echo "VERDICT $NAME FAIL no binary ($arm)" ; exit 1 ; }

	work=$BUILD/TESTDIR_${NAME}_$arm
	[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$work"

	TO=""
	command -v timeout >/dev/null 2>&1 && TO="timeout 240"
	out=$($TO "$bin" "$work" 2>&1)
	armrc=$?
	echo "  --- $arm arm ---"
	echo "$out" | sed 's/^/  /'
	[ "$armrc" -eq 124 ] && { echo "  $arm: hung (killed at 240s)" ; }
	echo "$out" | grep "^VERDICT $NAME " | head -1
	ARM_RC=$armrc
}

# Normal arm: must PASS.
run_arm normal ""
v=$(echo "$out" | grep "^VERDICT $NAME " | head -1)
case "$v" in
*" PASS"*) [ "$ARM_RC" -eq 0 ] || rc_all=1 ;;
*) rc_all=1 ;;
esac

# Sabotage arm: rebuild bt_cursor.o with the redirect off; must FAIL.
if [ -n "$SRCROOT" ] ; then
	sabo=$BUILD/r2_sabotage_bt_cursor.o
	rm -f "$sabo"
	"$CC" -c -g -O1 ${CFLAGS:-} -DR2_REVIVE_SABOTAGE \
	    -I"$BUILD" -I"$SRCROOT/src" "$SRCROOT/src/btree/bt_cursor.c" \
	    -o "$sabo" 2>/tmp/r2_sabo_cc.log || {
		echo "  sabotage: could not rebuild bt_cursor.o (skipping teeth)"
		head -5 /tmp/r2_sabo_cc.log | sed 's/^/    /' ; }
	if [ -f "$sabo" ] ; then
		run_arm sabotage "$sabo"
		v=$(echo "$out" | grep "^VERDICT $NAME " | head -1)
		case "$v" in
		*" PASS"*) echo "  TEETH: sabotage PASSED -- test has no teeth" ;
		    rc_all=1 ;;
		*) echo "  TEETH: sabotage FAILED as expected" ;;
		esac
	fi
else
	echo "  (source root not found; sabotage teeth-check skipped)"
fi

if [ "$rc_all" -eq 0 ] ; then
	echo "VERDICT $NAME PASS"
	exit 0
fi
echo "VERDICT $NAME FAIL"
exit 1

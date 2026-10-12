#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_typed_util.sh -- RFC 0014 commit 12 test (spec §9 commit 12, D8).
#
# The utilities say "typed database" where they cannot enforce it:
#   - a TYPED-build db_verify of a typed fixture prints the typed note
#     (BDB1198 Btree / BDB1199 Hash) and still succeeds structurally;
#   - a TYPED-build db_dump of a typed fixture prints the typed note on
#     stderr (triggered by the __db_typed catalog subdatabase);
#   - a DEFAULT-build db_verify succeeds WITHOUT the note (the note is
#     HAVE_TYPED-guarded so the default library's .text is unchanged, D2);
#   - a DEFAULT-build db_dump of the typed subdatabase fails closed (BDB0055).
# The committed fixtures test/db/fixtures/typed_{bt,hash}.db are the inputs.
#
# This runner is registered in BOTH tiers: in the typed tier it asserts the
# notes appear; in the db tier (default build) it asserts they do NOT, and the
# subdatabase dump fails closed.  Neither branch is a skip.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=typed_util
FIX=$HERE/fixtures

typed=0
grep -q '^#define HAVE_TYPED 1' "$BUILD/db_config.h" 2>/dev/null && typed=1

# Utility + library discovery (libtool wrapper or .libs binary + rpath).
VERIFY=""
DUMP=""
for cand in "$BUILD"/db_verify "$BUILD"/.libs/db_verify ; do
	[ -x "$cand" ] && { VERIFY="$cand" ; break ; }
done
for cand in "$BUILD"/db_dump "$BUILD"/.libs/db_dump ; do
	[ -x "$cand" ] && { DUMP="$cand" ; break ; }
done
[ -n "$VERIFY" ] && [ -n "$DUMP" ] || {
	echo "VERDICT $NAME FAIL no db_verify/db_dump under $BUILD" ; exit 1 ; }

LLP=""
[ -d "$BUILD/.libs" ] && LLP="$BUILD/.libs"

work=$BUILD/TESTDIR_$NAME
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"
cp "$FIX/typed_bt.db" "$work/bt.db" || { echo "VERDICT $NAME FAIL no bt fixture" ; exit 1 ; }
cp "$FIX/typed_hash.db" "$work/hash.db" || { echo "VERDICT $NAME FAIL no hash fixture" ; exit 1 ; }

run() {  # run <util> <args...> ; captures combined output in $OUT, rc in $RC
	OUT=$(cd "$work" && LD_LIBRARY_PATH="$LLP" "$@" 2>&1)
	RC=$?
}

nfail=0
ck() {  # ck <cond-rc 0/1> <msg>
	if [ "$1" -eq 0 ] ; then echo "  ok  : $2" ; else echo "  FAIL: $2" ; nfail=$((nfail+1)) ; fi
}

if [ "$typed" -eq 1 ] ; then
	# db_verify prints the typed note and still succeeds.
	run "$VERIFY" bt.db
	echo "$OUT" | grep -q "BDB1198" && r=0 || r=1
	ck "$r" "typed db_verify(bt) prints BDB1198 typed note"
	{ [ "$RC" -eq 0 ] && echo "$OUT" | grep -q "succeeded" ; } && r=0 || r=1
	ck "$r" "typed db_verify(bt) still succeeds structurally"

	run "$VERIFY" hash.db
	echo "$OUT" | grep -q "BDB1199" && r=0 || r=1
	ck "$r" "typed db_verify(hash) prints BDB1199 typed note"

	# db_dump prints the typed note (stderr) triggered by __db_typed.
	run "$DUMP" bt.db
	echo "$OUT" | grep -q "contains a typed database" && r=0 || r=1
	ck "$r" "typed db_dump(bt) prints the typed-database note"
else
	# Default build: no note, verify still succeeds.
	run "$VERIFY" bt.db
	echo "$OUT" | grep -q "BDB1198" && r=1 || r=0
	ck "$r" "default db_verify(bt) does NOT print the typed note"
	{ [ "$RC" -eq 0 ] && echo "$OUT" | grep -q "succeeded" ; } && r=0 || r=1
	ck "$r" "default db_verify(bt) succeeds structurally"

	# Default build: dumping the typed SUBDATABASE fails closed (BDB0055).
	run "$DUMP" -s typedsub bt.db
	{ [ "$RC" -ne 0 ] && echo "$OUT" | grep -q "BDB0055"; } && r=0 || r=1
	ck "$r" "default db_dump(-s typedsub) fails closed with BDB0055"
fi

if [ "$nfail" -eq 0 ] ; then
	echo "VERDICT $NAME PASS (typed=$typed)"
	exit 0
fi
echo "VERDICT $NAME FAIL $nfail checks (typed=$typed)"
exit 1

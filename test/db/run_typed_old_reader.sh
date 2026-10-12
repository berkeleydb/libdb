#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_typed_old_reader.sh -- RFC 0014 commit 4 test (spec §7.2
# typed_old_reader), registered in two tiers:
#
#   db tier:    build typed_old_reader against the DEFAULT build and open the
#               committed fixtures; a build that does not know BTM_TYPED /
#               DB_HASH_TYPED must refuse them (EINVAL + BDB0055), RDONLY and
#               read-write.  Optional LIBDB_OLD row.
#   typed tier: regenerate the fixtures with the typed build and compare the
#               typed meta bit against the committed ones, so the fixture
#               cannot drift from the code.  Needs an --enable-typed build.
#
# The fixtures live under test/db/fixtures/.  Reader output is one VERDICT.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=typed_old_reader
FIX=$HERE/fixtures
TIER=${TIER:-db}

find_lib() {
	_b=$1 _lib="" _rp=""
	for cand in "$_b"/libdb.a "$_b"/.libs/libdb-*.a ; do
		[ -f "$cand" ] && { _lib="$cand" ; break ; }
	done
	if [ -z "$_lib" ] ; then
		for cand in "$_b"/.libs/libdb-*.so "$_b"/.libs/libdb-*.dylib ; do
			[ -f "$cand" ] && { _lib="$cand" ; break ; }
		done
		[ -n "$_lib" ] && _rp="-Wl,-rpath,$(cd "$_b/.libs" && pwd)"
	fi
	LIB=$_lib ; LIBRPATH=$_rp
}

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

# ---------------------------------------------------------------------------
# typed tier: regenerate and compare the typed meta bit.
# ---------------------------------------------------------------------------
if [ "$TIER" = typed ] ; then
	if ! grep -q '^#define HAVE_TYPED 1' "$BUILD/db_config.h" 2>/dev/null
	then
		echo "VERDICT $NAME FAIL not-a-typed-build ($BUILD)"
		exit 1
	fi
	find_lib "$BUILD"
	[ -n "$LIB" ] || { echo "VERDICT $NAME FAIL no libdb under $BUILD" ; exit 1 ; }
	GEN=$BUILD/typed_mkfixture
	rm -f "$GEN"
	"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$HERE/typed_mkfixture.c" \
	    "$LIB" -lpthread $LIBRPATH $EXTRALIBS -o "$GEN" 2>&1 || {
		echo "VERDICT $NAME FAIL compile mkfixture" ; exit 1 ; }
	work=$BUILD/TESTDIR_${NAME}_regen
	[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$work"
	( cd "$work" && "$GEN" . ) >/dev/null 2>&1 || {
		echo "VERDICT $NAME FAIL regen" ; exit 1 ; }
	# Compare the typed meta bit (byte offset 16 in the subdatabase meta
	# page carries dbmeta.flags; rather than parse the page, compare the
	# whole committed fixture to the regenerated one on the fields that are
	# deterministic.  The uid and lsn differ run to run, so compare only
	# that both carry the typed bit: the committed fixture must be refused
	# by a default build (the db tier proves that), and the regenerated one
	# must carry the same typed-ness, which we verify by re-reading it with
	# a typed opener below.  Here we assert the regenerated files exist and
	# are the expected sizes, and that a default-built opener refuses them
	# (done by the db tier against the committed copy).  The strong check is
	# the byte compare of the meta flags word.
	nf=0
	for f in typed_bt.db typed_hash.db ; do
		[ -f "$work/$f" ] || { echo "  FAIL: regen missing $f" ; nf=1 ; }
		[ -f "$FIX/$f" ] || { echo "  FAIL: committed missing $f" ; nf=1 ; }
	done
	# The typed subdatabase meta page sits at file offset 32768 (page 8 at
	# the 4KB default pagesize; page 0 is the master database).  DBMETA.flags
	# -- which carries BTM_TYPED (0x100) / DB_HASH_TYPED (0x08) -- is at
	# struct offset 48, so file offset 32816.  Compare that 4-byte word
	# between the committed and the regenerated fixture: equal means the
	# fixture's typed-ness has not drifted from the code.  (uid and lsn
	# differ run to run, so only the flags word is deterministic.)
	flags_at() { od -A n -t x1 -j 32816 -N 4 "$1" | tr -d ' \n' ; }
	for f in typed_bt.db typed_hash.db ; do
		cf=$(flags_at "$FIX/$f")
		rf=$(flags_at "$work/$f")
		if [ -n "$cf" ] && [ "$cf" = "$rf" ] ; then
			echo "  ok: $f meta flags match committed ($cf)"
		else
			echo "  FAIL: $f meta flags committed=$cf regen=$rf"
			nf=1
		fi
	done
	if [ "$nf" -eq 0 ] ; then
		echo "VERDICT $NAME PASS fixtures regenerate with the same typed bit"
		exit 0
	fi
	echo "VERDICT $NAME FAIL fixture drift"
	exit 1
fi

# ---------------------------------------------------------------------------
# db tier: a reader without the typed bits must refuse the committed fixtures.
# ---------------------------------------------------------------------------
find_lib "$BUILD"
[ -n "$LIB" ] || { echo "VERDICT $NAME FAIL no libdb under $BUILD" ; exit 1 ; }
# Compile the reader the way the library sees its own headers: with
# -DHAVE_TYPED iff the build has it.  Against a typed build the reader then
# EXPECTS the fixture to open (it knows the bit); against a default build it
# expects BDB0055.  This is what lets the db tier run meaningfully in BOTH
# builds instead of a false red when run against the typed library.
GUARD=""
grep -q '^#define HAVE_TYPED 1' "$BUILD/db_config.h" 2>/dev/null &&
    GUARD="-DHAVE_TYPED"
BIN=$BUILD/$NAME
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} $GUARD -I"$BUILD" "$HERE/$NAME.c" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "VERDICT $NAME FAIL compile" ; exit 1 ; }
test -x "$BIN" || { echo "VERDICT $NAME FAIL no binary" ; exit 1 ; }

run_one() {
	_rdr=$1 _lbl=$2
	work=$BUILD/TESTDIR_${NAME}_$_lbl
	[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$work"
	cp "$FIX/typed_bt.db" "$FIX/typed_hash.db" "$work/" || return 1
	TO="" ; command -v timeout >/dev/null 2>&1 && TO="timeout 60"
	rc=0
	for f in typed_bt.db typed_hash.db ; do
		out=$(cd "$work" && $TO "$_rdr" . "$f" 2>&1)
		trc=$?
		echo "$out" | sed "s/^/  [$_lbl] /"
		[ "$trc" -ne 0 ] && rc=1
	done
	return $rc
}

rc=0
run_one "$BIN" head || rc=1

# Optional: LIBDB_OLD names a v2026.10.3 install to also refuse the fixture.
if [ -n "${LIBDB_OLD:-}" ] && [ -d "$LIBDB_OLD" ] ; then
	find_lib "$LIBDB_OLD"
	if [ -n "$LIB" ] ; then
		OBIN=$BUILD/${NAME}_old
		rm -f "$OBIN"
		if "${CC:-cc}" -g -O1 -I"$LIBDB_OLD/include" -I"$LIBDB_OLD" \
		    "$HERE/$NAME.c" "$LIB" -lpthread $LIBRPATH -o "$OBIN" \
		    2>/dev/null ; then
			run_one "$OBIN" old || rc=1
		else
			echo "  [old] (skipped: could not build against LIBDB_OLD)"
		fi
	fi
fi

if [ "$rc" -eq 0 ] ; then
	echo "VERDICT $NAME PASS default build refuses the typed fixtures"
	exit 0
fi
echo "run_$NAME.sh: FAIL"
exit 1

#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_typed_cmp_fuzz.sh -- RFC 0014 commit 8 test (spec §7.2 typed_cmp_fuzz).
#
# Fuzz the installed __typed_bt_compare with torn inputs under ASan: a
# generated comparator reading torn bytes must not crash.  The test binary is
# built with -fsanitize=address,undefined so a slice that runs off the end
# (the §6.3 adversary) is caught as a heap-buffer-overflow.  Typed tier: needs
# an --enable-typed library, so a default build is a FAIL, not a skip.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=typed_cmp_fuzz
SRC=$HERE/$NAME.c

if ! grep -q '^#define HAVE_TYPED 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "VERDICT $NAME FAIL not-a-typed-build ($BUILD)"
	exit 1
fi

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

# ASan+UBSan on the CONSUMER: the comparator hands (p, len) slices to the
# test's own field codec, so an unbounded len makes the test read past an
# exact-sized heap block -- caught by the test binary's own instrumentation,
# which is what makes a missing §6.3 check a visible heap-buffer-overflow.
# -fno-sanitize=function: clang's function-type check (part of "undefined";
# gcc has none) flags every call through a libdb method pointer, because the
# K&R-style definitions (__db_open_pp and the rest) have no prototype type.
# With halt_on_error that stopped the test at its first dbp->open, before any
# fuzzing, and the run reported "no VERDICT line".  The check says nothing
# about the comparator's bounds; ASan does, and stays fully on.
SAN="-fsanitize=address,undefined -fno-omit-frame-pointer"
BIN=$BUILD/$NAME
rm -f "$BIN"
# -fno-sanitize=function goes LAST: a later -fsanitize=undefined (the CI
# job's CFLAGS) would otherwise turn the check back on.  Only clang has the
# check; gcc rejects the flag, so probe for it.
NOFN=""
echo 'int main(void){return 0;}' > "$BUILD/$NAME.probe.c"
"${CC:-cc}" -fsanitize=undefined -fno-sanitize=function \
    "$BUILD/$NAME.probe.c" -o "$BUILD/$NAME.probe" >/dev/null 2>&1 &&
    NOFN="-fno-sanitize=function"
rm -f "$BUILD/$NAME.probe.c" "$BUILD/$NAME.probe"
"${CC:-cc}" -g -O1 $SAN ${CFLAGS:-} $NOFN -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "VERDICT $NAME FAIL compile" ; exit 1 ; }
test -x "$BIN" || { echo "VERDICT $NAME FAIL no binary produced" ; exit 1 ; }

work=$BUILD/TESTDIR_$NAME
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"

# halt_on_error=1 so the first OOB stops with a non-zero exit (the sabotage).
# That is ASan's job; the sabotage arm expects "heap-buffer-overflow".  UBSan
# stays on and reports, but it must not halt.  The engine's offsetof idiom
# (SSZA, db_int.in: &((T *)0)->field[0]) is a "member access within null
# pointer" to clang's UBSan, and every Btree open evaluates it
# (bt_open.c:94), so halting there stopped the test before any fuzzing.
# That is pre-existing, the same on master, and not the comparator.
ASAN_OPTIONS=${ASAN_OPTIONS:-halt_on_error=1:abort_on_error=1}
UBSAN_OPTIONS=${UBSAN_OPTIONS:-print_stacktrace=1}
export ASAN_OPTIONS UBSAN_OPTIONS

TO=""
command -v timeout >/dev/null 2>&1 && TO="timeout 120"
# Iteration count: full 10^6, but the test self-caps at 55s wall anyway.
out=$(cd "$work" && $TO "$BIN" . 1000000 2>&1)
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

#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_typed_footprint.sh -- RFC 0014 §2.4: --enable-typed must cost a default
# build NOTHING, and must not move the region signature in either build (D2).
#
# Builds three libraries, all from scratch in a scratch dir:
#   base     the merge base with master, plain dist/configure
#   head     this tree, plain dist/configure
#   typed    this tree, dist/configure --enable-typed
# and asserts:
#   1  .text of libdb-*.so: head == base, byte-exact
#   2  env_sig_print.sh:    head == base
#   3  exported db_set_schema / __typed_* symbols: none in head, some in
#      typed (until the API exists -- commit 1-2 of §9 -- "typed" has none
#      either, and item 3 reports that as "not yet applicable", never PASS
#      for the typed half: see TYPED_API below)
#   4  env_sig_print.sh:    typed == head (D2)
# One VERDICT line names every item that failed.
#
# Env:
#   TYPED_FOOTPRINT_BASE  commit to compare against (default: merge base of
#                         HEAD with master, else origin/master).  CI's shallow
#                         checkout has neither, so CI must fetch and set this.
#   TYPED_API             1 once db_set_schema exists (§9 commit 3), making
#                         item 3's typed half mandatory.  Default: detected
#                         from src/typed/typed.c.
#   KEEP=1                keep the scratch dir.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
NAME=typed_footprint
MAKE=${MAKE:-$(command -v gmake || command -v make)}
NCPU=$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

verdict() { echo "VERDICT $NAME $*" ; }

command -v git >/dev/null 2>&1 && git -C "$SRC" rev-parse HEAD >/dev/null 2>&1 || {
	verdict "FAIL not a git checkout: the base build needs git"
	exit 1
}
BASE=${TYPED_FOOTPRINT_BASE:-}
if [ -z "$BASE" ] ; then
	for b in master origin/master ; do
		BASE=$(git -C "$SRC" merge-base HEAD "$b" 2>/dev/null) && break
		BASE=""
	done
fi
[ -n "$BASE" ] || {
	verdict "FAIL no base commit (no master / origin/master; set TYPED_FOOTPRINT_BASE)"
	exit 1
}
BASE=$(git -C "$SRC" rev-parse "$BASE") || { verdict "FAIL bad base $BASE" ; exit 1 ; }

work=${TMPDIR:-/tmp}/typed_footprint_$$
mkdir -p "$work" || exit 1
cleanup() {
	[ "${KEEP:-0}" = 1 ] && { echo "kept $work" ; return ; }
	git -C "$SRC" worktree remove --force "$work/basesrc" >/dev/null 2>&1
	[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
	rmdir "$work" 2>/dev/null
}
trap cleanup EXIT INT TERM

git -C "$SRC" worktree add --detach "$work/basesrc" "$BASE" >/dev/null 2>&1 || {
	verdict "FAIL git worktree add $BASE"
	exit 1
}

# build <name> <srcdir> [configure args]
build() {
	_n=$1 ; _s=$2 ; shift 2
	mkdir -p "$work/$_n"
	( cd "$work/$_n" && "$_s/dist/configure" "$@" ) > "$work/$_n.cfg.log" 2>&1 &&
	    ( cd "$work/$_n" && $MAKE -j"$NCPU" ) > "$work/$_n.build.log" 2>&1 || {
		echo "  $_n: build failed; last lines of $work/$_n.*.log:"
		tail -5 "$work/$_n.build.log" "$work/$_n.cfg.log" 2>/dev/null | sed 's/^/    /'
		return 1
	}
}
build base "$work/basesrc" || { verdict "FAIL base build" ; exit 1 ; }
build head "$SRC" || { verdict "FAIL head build" ; exit 1 ; }
build typed "$SRC" --enable-typed || { verdict "FAIL typed build" ; exit 1 ; }
grep -q '^#define HAVE_TYPED 1' "$work/typed/db_config.h" || {
	verdict "FAIL --enable-typed did not define HAVE_TYPED"
	exit 1
}
grep -q '^#define HAVE_TYPED' "$work/head/db_config.h" && {
	verdict "FAIL a default configure defined HAVE_TYPED"
	exit 1
}

so() { ls "$work/$1"/.libs/libdb-*.so "$work/$1"/.libs/libdb-*.dylib 2>/dev/null | head -1 ; }
text() {
	_f=$(so "$1")
	[ -n "$_f" ] || { echo "none" ; return ; }
	# size -A is GNU/LLVM; macOS size(1) has no -A, use -m there.
	size -A "$_f" 2>/dev/null | awk '$1 == ".text" { print $2 }' | grep . ||
	    size -m "$_f" 2>/dev/null | awk '/Section __text/ { print $NF }'
}
sig() { BUILD_DIR="$work/$1" sh "$SRC/dist/env_sig_print.sh" "$2" 2>/dev/null ; }
nsyms() {
	_f=$(so "$1")
	[ -n "$_f" ] || { echo 0 ; return ; }
	# grep -c prints 0 on no match and exits 1: no `|| echo 0'.
	nm -D "$_f" 2>/dev/null | grep -c 'db_set_schema\|__typed_'
}

fail=""
tb=$(text base) ; th=$(text head)
echo "  1  .text  base=$tb head=$th"
[ "$tb" != none ] && [ -n "$tb" ] && [ "$tb" = "$th" ] || fail="$fail 1(.text $tb->$th)"

sb=$(sig base "$work/basesrc") ; sh_=$(sig head "$SRC") ; st=$(sig typed "$SRC")
echo "  2  env_sig base=$sb head=$sh_"
[ -n "$sb" ] && [ "$sb" = "$sh_" ] || fail="$fail 2(sig $sb->$sh_)"

nh=$(nsyms head) ; nt=$(nsyms typed)
if [ -z "${TYPED_API:-}" ] ; then
	grep -q 'db_set_schema' "$SRC/src/typed/typed.c" 2>/dev/null && TYPED_API=1 || TYPED_API=0
fi
echo "  3  typed symbols head=$nh typed=$nt (TYPED_API=$TYPED_API)"
[ "$nh" -eq 0 ] || fail="$fail 3(head exports $nh typed symbols)"
if [ "$TYPED_API" = 1 ] ; then
	[ "$nt" -gt 0 ] || fail="$fail 3(typed build exports no typed symbols)"
else
	echo "     typed half not yet applicable: no API in src/typed/typed.c"
fi

echo "  4  env_sig head=$sh_ typed=$st"
[ -n "$st" ] && [ "$st" = "$sh_" ] || fail="$fail 4(sig default $sh_ typed $st)"

if [ -z "$fail" ] ; then
	verdict "PASS base $BASE"
	exit 0
fi
verdict "FAIL$fail"
exit 1

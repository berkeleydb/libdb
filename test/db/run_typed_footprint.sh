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
# Builds, from scratch in a scratch dir:
#   head     this tree, plain dist/configure
#   typed    this tree, dist/configure --enable-typed
#   base     a typed-free reference, plain dist/configure (when one exists)
# and asserts:
#   0  every typed reference in src/ and util/ is inside #ifdef HAVE_TYPED
#      (typed_guard_lint.sh; needs no base, so it is the item that survives
#      the merge to master)
#   1  .text of libdb-*.so: head == base, byte-exact            (needs base)
#   2  env_sig_print.sh:    head == base                        (needs base)
#   3  exported db_set_schema / __typed_* symbols: none in head; some in
#      typed once the API exists (TYPED_API, below)
#   4  env_sig_print.sh:    typed == head (D2)
# One VERDICT line names every item that failed, and which items ran.
#
# THE BASE (spec §11 V4).  Items 1-2 compare against "HEAD minus the typed
# layer": the parent of the commit that created src/typed/.  Not the merge
# base with master (§2.4's wording): that also subtracts every NON-typed
# change still on the branch -- Phase 0's P1-P3 move .text legitimately -- and
# once typed is on master the merge base is HEAD itself, which measures
# nothing.  The typed layer's own later commits all sit above that parent, so
# any .text they add to a default build shows up here.
# ponytail: valid while every commit above that parent is typed-layer work.
# A merge commit above the base (the typed PR landing on master, or master
# merged in) breaks that, so items 1-2 then need an explicit
# TYPED_FOOTPRINT_BASE and are otherwise skipped, as they are in a shallow
# clone or a tarball.  Item 0 runs everywhere and carries the guarantee.
#
# Env:
#   TYPED_FOOTPRINT_BASE  typed-free reference commit (default: the parent of
#                         the first commit adding src/typed/, if no merge
#                         sits above it)
#   TYPED_API             1 once db_set_schema is DEFINED (§9 commit 3; a
#                         prototype or EXTERN: comment is not enough).
#   KEEP=1                keep the scratch dir.
# CFLAGS/LDFLAGS/CPPFLAGS from the environment are ignored: all three builds
# use configure's defaults, so a sanitizer job measures the same thing.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
NAME=typed_footprint
MAKE=${MAKE:-$(command -v gmake || command -v make)}
NCPU=$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

verdict() { echo "VERDICT $NAME $*" ; }

command -v git >/dev/null 2>&1 && git -C "$SRC" rev-parse HEAD >/dev/null 2>&1 ||
    NOGIT=1
BASE=${TYPED_FOOTPRINT_BASE:-}
if [ -n "$BASE" ] && [ -n "${NOGIT:-}" ] ; then
	verdict "FAIL TYPED_FOOTPRINT_BASE set but $SRC is not a git checkout"
	exit 1
fi
if [ -z "$BASE" ] && [ -z "${NOGIT:-}" ] ; then
	# The parent of the oldest commit that created src/typed/: the newest
	# tree that has every non-typed change below the typed layer and none
	# of the layer itself.  Absent in a shallow clone.
	first=$(git -C "$SRC" log --format=%H --diff-filter=A -- src/typed \
	    2>/dev/null | tail -1)
	[ -n "$first" ] && BASE=$(git -C "$SRC" rev-parse -q --verify "$first^" 2>/dev/null)
	if [ -n "$BASE" ] &&
	    [ -n "$(git -C "$SRC" rev-list --merges "$BASE..HEAD" | head -1)" ] ; then
		echo "  a merge sits above base $BASE: other work is mixed in, so"
		echo "  items 1-2 need an explicit TYPED_FOOTPRINT_BASE"
		BASE=""
	fi
fi
if [ -n "$BASE" ] ; then
	BASE=$(git -C "$SRC" rev-parse --verify -q "$BASE^{commit}") || {
		verdict "FAIL TYPED_FOOTPRINT_BASE is not a commit: $TYPED_FOOTPRINT_BASE"
		exit 1
	}
	# A base whose tree is HEAD's is not a typed-free reference.
	if [ "$(git -C "$SRC" rev-parse "$BASE^{tree}")" = \
	    "$(git -C "$SRC" rev-parse "HEAD^{tree}")" ] ; then
		echo "  base $BASE has HEAD's tree: no typed-free reference"
		BASE=""
	fi
fi

work=${TMPDIR:-/tmp}/typed_footprint_$$
mkdir -p "$work" || exit 1
cleanup() {
	[ "${KEEP:-0}" = 1 ] && { echo "kept $work" ; return ; }
	[ -d "$work/basesrc" ] &&
	    git -C "$SRC" worktree remove --force "$work/basesrc" >/dev/null 2>&1
	[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
	rmdir "$work" 2>/dev/null
}
trap cleanup EXIT INT TERM

if [ -n "$BASE" ] ; then
	git -C "$SRC" worktree add --detach "$work/basesrc" "$BASE" >/dev/null 2>&1 || {
		verdict "FAIL git worktree add $BASE"
		exit 1
	}
fi

# build <name> <srcdir> [configure args]
build() {
	_n=$1 ; _s=$2 ; shift 2
	mkdir -p "$work/$_n"
	( unset CFLAGS LDFLAGS CPPFLAGS ; cd "$work/$_n" &&
	    "$_s/dist/configure" "$@" ) > "$work/$_n.cfg.log" 2>&1 &&
	    ( cd "$work/$_n" && $MAKE -j"$NCPU" ) > "$work/$_n.build.log" 2>&1 || {
		echo "  $_n: build failed; last lines of $work/$_n.*.log:"
		tail -5 "$work/$_n.build.log" "$work/$_n.cfg.log" 2>/dev/null | sed 's/^/    /'
		return 1
	}
}
if [ -n "$BASE" ] ; then
	build base "$work/basesrc" || { verdict "FAIL base build" ; exit 1 ; }
fi
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
	size -A "$_f" 2>/dev/null | awk '$1 == ".text" { print $2 }'
}
sig() { BUILD_DIR="$work/$1" sh "$SRC/dist/env_sig_print.sh" "$2" 2>/dev/null ; }
nsyms() {
	_f=$(so "$1")
	[ -n "$_f" ] || { echo 0 ; return ; }
	# Exported (defined, external) symbols.  ELF: nm -D lists the dynamic
	# table.  Mach-O has no -D; nm -gU lists the defined externals, and
	# each name carries a leading underscore, which the pattern allows.
	# grep -c prints 0 on no match and exits 1: no `|| echo 0'.
	case $_f in
	*.dylib) nm -gU "$_f" 2>/dev/null ;;
	*)	nm -D --defined-only "$_f" 2>/dev/null || nm -D "$_f" 2>/dev/null ;;
	esac | grep -c 'db_set_schema\|__typed_'
}

fail=""
ran="0 3 4"
# Item 0 needs no base and no history: every typed reference in the engine is
# inside #ifdef HAVE_TYPED.  It is the guarantee items 1-2 measure, checked at
# the source, so it keeps holding once the typed layer is on master and the
# base below it is old history (or absent, in a shallow clone).
if ! sh "$HERE/typed_guard_lint.sh" "$SRC" > "$work/lint.out" 2>&1 ; then
	echo "  0  unguarded typed references:" ; sed 's/^/     /' "$work/lint.out"
	fail="$fail 0(unguarded typed code)"
else
	echo "  0  every typed reference is inside #ifdef HAVE_TYPED"
fi
sh_=$(sig head "$SRC") ; st=$(sig typed "$SRC")
if [ -n "$BASE" ] ; then
	ran="0 1 2 3 4"
	tb=$(text base) ; th=$(text head)
	echo "  1  .text  base=$tb head=$th"
	[ -n "$tb" ] && [ "$tb" != none ] && [ "$tb" = "$th" ] ||
	    fail="$fail 1(.text $tb->$th)"
	sb=$(sig base "$work/basesrc")
	echo "  2  env_sig base=$sb head=$sh_"
	[ -n "$sb" ] && [ "$sb" = "$sh_" ] || fail="$fail 2(sig $sb->$sh_)"
else
	echo "  1,2 not measured: no typed-free base"
fi

nh=$(nsyms head) ; nt=$(nsyms typed)
if [ -z "${TYPED_API:-}" ] ; then
	# A function DEFINITION, not a prototype or EXTERN: comment: the symbol
	# db_set_schema arrives in §9 commit 3 as a definition (`^db_set_schema(`
	# at column 0, K&R style), while commit 2 only adds an `EXTERN:' comment
	# and a declaration.  Matching the definition keeps TYPED_API=0 until the
	# symbol can actually be exported, so item 3's typed half is not demanded
	# of a build that has no code yet.
	grep -Eq '^db_set_schema[ (]' "$SRC/src/typed/typed.c" 2>/dev/null &&
	    TYPED_API=1 || TYPED_API=0
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
	verdict "PASS items $ran${BASE:+ base $BASE}"
	exit 0
fi
verdict "FAIL$fail"
exit 1

#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# typed_guard_lint.sh -- RFC 0014 §2.4: every typed-layer reference in the
# engine sources must sit inside an #ifdef HAVE_TYPED region, so a default
# build compiles none of it.  This is the history-free half of the footprint
# gate: it needs no base build and no git, so it holds on master forever.
#
# Usage: typed_guard_lint.sh <srcdir>
# Prints each unguarded reference as file:line:text and exits 1 if any.

SRC=${1:-.}
# Identifiers that only the typed layer defines or uses.  db.in is the public
# header and declares the API unconditionally (like the compression setters),
# so it is not scanned; src/typed/ is the layer itself and is guarded whole.
PAT='__typed_|DB2_AM_TYPED|BTM_TYPED|DB_HASH_TYPED|DB_CONSTRAINT_|DB_CODEC_|db_set_schema|db_get_schema|db_record_fixed|__os_atomic_read_ptr'

find "$SRC/src" "$SRC/util" -name '*.[ch]' ! -path '*/src/typed/*' \
    ! -path '*/dbinc_auto/*' 2>/dev/null | sort | while read -r f ; do
	awk -v pat="$PAT" -v file="$f" '
	# depth: nesting of #if*; tdepth: depth at which a HAVE_TYPED region opened
	# (0 = not inside one).  "#else" of a HAVE_TYPED if leaves the region; an
	# "#ifndef HAVE_TYPED" region is the default build, so it is scanned.
	/^[ \t]*#[ \t]*if/ {
		depth++
		if (!tdepth && $0 ~ /HAVE_TYPED/ && $0 !~ /ifndef|!/) tdepth = depth
		next
	}
	/^[ \t]*#[ \t]*else/ { if (tdepth == depth) { tdepth = 0; elsed[depth] = 1 } ; next }
	/^[ \t]*#[ \t]*elif/ { if (tdepth == depth) tdepth = 0 ; next }
	/^[ \t]*#[ \t]*endif/ { if (tdepth == depth) tdepth = 0 ; delete elsed[depth]; depth-- ; next }
	# A #define emits no code (BTM_TYPED is an on-disk format constant and
	# must be visible everywhere); only a USE outside the guard costs bytes.
	/^[ \t]*#[ \t]*define/ { next }
	!tdepth && $0 ~ pat && $0 !~ /^[ \t]*(\/\*|\*)/ { print file ":" NR ":" $0 }
	' "$f"
done > "${TMPDIR:-/tmp}/typed_guard_lint.$$"
n=$(wc -l < "${TMPDIR:-/tmp}/typed_guard_lint.$$")
cat "${TMPDIR:-/tmp}/typed_guard_lint.$$"
rm -f "${TMPDIR:-/tmp}/typed_guard_lint.$$"
[ "$n" -eq 0 ]

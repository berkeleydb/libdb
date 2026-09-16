#!/bin/sh
# dist/env_sig_print.sh -- print __env_struct_sig() for the tree it is run in.
#
# WHY THIS EXISTS
#
# src/env/env_sig.c hashes the sizeof of ~137 structs into a build signature.
# src/env/env_region.c refuses to ATTACH an existing environment when
# renv->signature != __env_struct_sig():
#
#     "BDB1539 Build signature doesn't match environment"  -> DB_VERSION_MISMATCH
#
# So any size or layout change to ANY struct env_sig.c hashes -- including
# process-private ones like struct __db_mpool -- breaks upgrade-in-place for
# every existing deployment.  abidiff CANNOT SEE THIS: the public ABI (sizeof
# DB / DBC / DB_ENV / DB_TXN) is unchanged, so the abi-drift gate goes green
# while real customer upgrades fail.  This nearly shipped in v2026.09.5 and was
# caught only by a human.
#
# Adding a struct field is a THREE-gate question: public ABI (abidiff catches
# it), shared-region layout (majver/minver catches it), and this signature
# (nothing caught it until now).
#
# HOW
#
# Compiles a 12-line main() against src/env/env_sig.c and src/hash/hash_func.c
# (the only symbol env_sig.c needs is __ham_func5) and prints the value in hex.
# Deliberately NOT a full library build: it must be cheap enough to run twice
# in one CI job (PR head and merge base) and must work in a worktree that has
# never been configured -- so it needs a db_config.h, which it takes from an
# existing build dir or generates with a bare configure.
#
# Usage:
#   dist/env_sig_print.sh [SRCDIR]     # default: the tree this script is in
# Env:
#   CC          compiler (default: cc)
#   BUILD_DIR   a directory containing db_config.h (default: <src>/build_unix)

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
# Absolutize both paths BEFORE anything cd's.  `dist/env_sig_print.sh .` is the
# documented usage (and what the CI gate uses), and with a relative SRC the
# configure below ran "./dist/configure" from INSIDE ./build_unix, where no such
# file exists -- the script then reported "configure failed" for a tree that is
# perfectly fine.  That is what made the region-signature gate red on a branch
# whose signature had not changed at all.
SRC=${1:-"$HERE/.."}
SRC=$(CDPATH= cd -- "$SRC" && pwd) || {
	echo "env_sig_print.sh: no such source directory: ${1:-$HERE/..}" >&2
	exit 1
}
CC=${CC:-cc}
BUILD_DIR=${BUILD_DIR:-"$SRC/build_unix"}

if [ ! -f "$BUILD_DIR/db_config.h" ]; then
	# A bare configure is enough: db_config.h plus the generated db.h is all
	# env_sig.c's include chain needs.  Quiet, because the caller wants one
	# hex value on stdout and nothing else.  On failure show the tail of the
	# log: "configure failed" with no reason cost one CI cycle to diagnose.
	mkdir -p "$BUILD_DIR"
	BUILD_DIR=$(CDPATH= cd -- "$BUILD_DIR" && pwd)
	( cd "$BUILD_DIR" && "$SRC/dist/configure" \
	    >"$BUILD_DIR/env_sig_configure.log" 2>&1 ) || {
		echo "env_sig_print.sh: configure failed in $BUILD_DIR" >&2
		echo "--- last 20 lines of $BUILD_DIR/env_sig_configure.log:" >&2
		tail -20 "$BUILD_DIR/env_sig_configure.log" >&2 || true
		exit 1
	}
fi
BUILD_DIR=$(CDPATH= cd -- "$BUILD_DIR" && pwd)

tmpd=$(mktemp -d) || exit 1
trap 'rm -f "$tmpd"/sig_main.c "$tmpd"/sigprint; rmdir "$tmpd" 2>/dev/null' \
    0 1 2 3 13 15

cat > "$tmpd/sig_main.c" <<'EOF'
#include "db_config.h"
#include "db_int.h"
#include <stdio.h>
int
main(void)
{
	printf("0x%08x\n", (unsigned)__env_struct_sig());
	return (0);
}
EOF

# -w: this is pre-2000 K&R-era C and the warnings are not the point here.
$CC -w -I"$BUILD_DIR" -I"$SRC/src" -I"$SRC" \
    -o "$tmpd/sigprint" "$tmpd/sig_main.c" \
    "$SRC/src/env/env_sig.c" "$SRC/src/hash/hash_func.c" >&2 || {
	echo "env_sig_print.sh: compile failed" >&2
	exit 1
}
"$tmpd/sigprint"

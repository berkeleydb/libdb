#!/bin/sh
# test/lockmatrix/run.sh -- build and run the Tier B3 lock-mode matrix.
#
# Exercises every db_lockmode_t through lock_get / lock_put / lock_vec, the
# whole conflict matrix, and the lock-LIST operations (DB_LOCK_PUT_READ,
# DB_LOCK_UPGRADE_WRITE) with a locker holding a MIX of write and SIREAD
# locks -- the shape that overflows the objlist allocation in issue #140.
#
# The interesting failure is an out-of-bounds WRITE inside libdb's own
# allocation, which is only observable when LIBDB ITSELF is ASan-instrumented.
# A harness-only ASan build cannot see it.  So by default this builds (once,
# reusing the fuzz tier's mechanism and directory) an ASan libdb under
# build_asan_gate/ and links against that.
#
# On current master (#140 unfixed) an ASan run is EXPECTED to abort with a
# heap-buffer-overflow inside __lock_vec.  The harness prints each shape
# before attempting it, so the last line of output names the shape that
# faulted.  When #140 lands, the whole matrix must complete and exit 0.
#
# Usage:
#   ./run.sh                     # build (ASan libdb) + run every section
#   ./run.sh build               # build only
#   ./run.sh modes|conflicts|list ...
#
# Env:
#   CC             compiler (default: clang; needed for the ASan libdb)
#   LIBDB_ASAN     1 => build/use an ASan-instrumented libdb (default 1)
#   LIBDB_BUILD    explicit libdb build dir (skips the ASan auto-build)
#   LOCK_TIMEOUT   seconds for the run (default 600)
#
# Run from test/lockmatrix/ inside a `nix develop` shell.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$HERE"

CC=${CC:-clang}
LIBDB_ASAN=${LIBDB_ASAN:-1}
LOCK_TIMEOUT=${LOCK_TIMEOUT:-600}
OUT="$HERE/build"

# Same mechanism as test/fuzz/check-crashes.sh: one shared ASan libdb under
# build_asan_gate/, built on demand and reused by both tiers.
if [ "$LIBDB_ASAN" = "1" ] && [ -z "${LIBDB_BUILD:-}" ]; then
	GATE_BUILD="$HERE/../../build_asan_gate"
	if [ ! -f "$GATE_BUILD/libdb.a" ]; then
		echo "building ASan libdb in $GATE_BUILD (once) ..."
		mkdir -p "$GATE_BUILD"
		( cd "$GATE_BUILD" &&
		  ../dist/configure --enable-debug \
		      CC="$CC" CFLAGS="-fsanitize=address -g -O1" \
		      >configure.log 2>&1 &&
		  make -j"$(nproc 2>/dev/null || echo 4)" >build.log 2>&1 ) ||
		echo "warning: ASan libdb build failed; falling back" >&2
	fi
	[ -f "$GATE_BUILD/libdb.a" ] && LIBDB_BUILD="$GATE_BUILD"
fi
LIBDB_BUILD=${LIBDB_BUILD:-"$HERE/../../build_unix"}
LIBDBA="$LIBDB_BUILD/libdb.a"

if [ -f "$LIBDB_BUILD/Makefile" ]; then
	LDLIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$LIBDB_BUILD/Makefile" | head -1)
fi
LDLIBS="${LDLIBS:--lpthread} -ldl -lpthread"

# ASan on the harness too, so the harness's own DBTs are red-zoned.
CFLAGS="-g -O1 -Wall -Wextra -Wno-unused-parameter -fsanitize=address"
CFLAGS="$CFLAGS -I$LIBDB_BUILD -I$HERE"

[ -f "$LIBDBA" ] || {
	echo "error: libdb.a not found at $LIBDBA -- build libdb first:" >&2
	echo "    (cd $LIBDB_BUILD && ../dist/configure --enable-debug && make -j4)" >&2
	exit 2
}
echo "linking against $LIBDBA"

mkdir -p "$OUT"
# shellcheck disable=SC2086
$CC $CFLAGS "$HERE/test_lock_matrix.c" "$LIBDBA" $LDLIBS \
	-o "$OUT/test_lock_matrix"
echo "built $OUT/test_lock_matrix"

[ "${1:-}" = "build" ] && exit 0

cd "$OUT"
exec timeout "$LOCK_TIMEOUT" ./test_lock_matrix "$@"

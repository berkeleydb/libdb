#!/bin/sh
# test/isolation/run.sh -- build and run the Tier B1 isolation/anomaly checker.
#
# Builds test_iso_anomaly against an existing libdb build and runs it under a
# timeout.  The checker's verdict is computed (enumerate serial orders), so a
# non-zero exit means an outcome disagreed with the recorded expectation --
# either a new serializability violation, or a known-broken scenario that
# started passing (i.e. the referenced issue got fixed and the table needs
# updating).
#
# Usage:
#   ./run.sh                 # build + run every scenario
#   ./run.sh build           # build only
#   ./run.sh SCENARIO ...    # build + run the named scenarios
#   ./run.sh --list          # list scenario names
#
# Env:
#   CC            compiler (default: cc)
#   LIBDB_BUILD   path to a built build_unix (default: ../../build_unix)
#   ISO_TIMEOUT   seconds for the whole run (default: 300)
#   ISO_SAN       1 => also build with ASan/UBSan (default 0)
#
# Run from test/isolation/ inside a `nix develop` shell.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$HERE"

CC=${CC:-cc}
LIBDB_BUILD=${LIBDB_BUILD:-"$HERE/../../build_unix"}
ISO_TIMEOUT=${ISO_TIMEOUT:-300}
ISO_SAN=${ISO_SAN:-0}
OUT="$HERE/build"
LIBDBA="$LIBDB_BUILD/libdb.a"

if [ -f "$LIBDB_BUILD/Makefile" ]; then
	LDLIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$LIBDB_BUILD/Makefile" | head -1)
fi
LDLIBS="${LDLIBS:--lpthread} -ldl -lpthread"

CFLAGS="-g -O1 -Wall -Wextra -Wno-unused-parameter -I$LIBDB_BUILD -I$HERE"
[ "$ISO_SAN" = "1" ] && CFLAGS="$CFLAGS -fsanitize=address"

[ -f "$LIBDBA" ] || {
	echo "error: libdb.a not found at $LIBDBA -- build libdb first:" >&2
	echo "    (cd $LIBDB_BUILD && ../dist/configure --enable-debug && make -j4)" >&2
	exit 2
}

mkdir -p "$OUT"
# shellcheck disable=SC2086
$CC $CFLAGS "$HERE/test_iso_anomaly.c" "$LIBDBA" $LDLIBS \
	-o "$OUT/test_iso_anomaly"
echo "built $OUT/test_iso_anomaly"

[ "${1:-}" = "build" ] && exit 0

cd "$OUT"
exec timeout "$ISO_TIMEOUT" ./test_iso_anomaly "$@"

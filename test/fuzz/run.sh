#!/bin/sh
# test/fuzz/run.sh -- build and (optionally) run the libdb fuzz harnesses.
#
# Builds each LLVMFuzzerTestOneInput harness against a freshly built
# libdb.a with clang -fsanitize=fuzzer,address,undefined, then runs each
# for a bounded time as a smoke fuzz.  Set FUZZ_STANDALONE=1 to build the
# no-libFuzzer driver instead (a main() that replays file arguments),
# useful for reproducing a saved crash under ASan/UBSan only.
#
# Usage:
#   ./run.sh build            # just build the harnesses
#   ./run.sh smoke [SECONDS]  # build + run each for SECONDS (default 60)
#   ./run.sh repro H FILE     # build standalone + replay FILE through H
#                             #   (H = dbfile|recover|api)
#
# Env:
#   CC              compiler (default: clang)
#   LIBDB_BUILD     path to a built build_unix (default: ../../build_unix)
#   FUZZ_STANDALONE 1 => build the standalone driver (no libFuzzer runtime)
#
# Run from test/fuzz/ inside a `nix develop` shell (clang + libFuzzer).

set -eu

CC=${CC:-clang}
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
LIBDB_BUILD=${LIBDB_BUILD:-"$HERE/../../build_unix"}
OUT="$HERE/build"
CORPUS="$HERE/corpus"

SAN="-fsanitize=address,undefined -fno-sanitize-recover=undefined"
FUZZ="-fsanitize=fuzzer,address,undefined"
CFLAGS="-g -O1 -Wall -Wextra -Wno-unused-parameter -I$LIBDB_BUILD -I$HERE"
LIBDBA="$LIBDB_BUILD/libdb.a"
# libdb's own link deps live in the generated Makefile's LIBS (e.g.
# -luring -lpthread on Linux with io_uring).  Reuse them so we stay in
# sync with the build; fall back to a sane default if not found.  -ldl is
# added for dlopen used by some os backends.
if [ -f "$LIBDB_BUILD/Makefile" ]; then
	LDLIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$LIBDB_BUILD/Makefile" | head -1)
fi
LDLIBS="${LDLIBS:--lpthread} -ldl"

HARNESSES="dbfile recover api"

die() { echo "error: $*" >&2; exit 1; }

ensure_lib() {
	[ -f "$LIBDBA" ] || die "libdb.a not found at $LIBDBA -- build libdb first:
    (cd $LIBDB_BUILD && ../dist/configure --enable-debug && make -j4)"
}

build_one() {
	h=$1
	mkdir -p "$OUT"
	if [ "${FUZZ_STANDALONE:-0}" = "1" ]; then
		echo "  [standalone] fuzz_$h"
		# shellcheck disable=SC2086
		$CC $CFLAGS $SAN -DFUZZ_STANDALONE \
			"$HERE/fuzz_$h.c" "$LIBDBA" $LDLIBS \
			-o "$OUT/fuzz_${h}_standalone"
	else
		echo "  [libfuzzer]  fuzz_$h"
		# shellcheck disable=SC2086
		$CC $CFLAGS $FUZZ \
			"$HERE/fuzz_$h.c" "$LIBDBA" $LDLIBS \
			-o "$OUT/fuzz_$h"
	fi
}

build_all() {
	ensure_lib
	for h in $HARNESSES; do build_one "$h"; done
	echo "built harnesses in $OUT"
}

smoke() {
	secs=${1:-60}
	ensure_lib
	FUZZ_STANDALONE=0
	for h in $HARNESSES; do build_one "$h"; done
	rc=0
	for h in $HARNESSES; do
		echo "=== smoke fuzz: fuzz_$h (${secs}s) ==="
		mkdir -p "$OUT/artifacts_$h" "$OUT/work_$h"
		# Seed the working corpus from the committed seeds; libFuzzer
		# writes newly found inputs into work_$h, keeping the tracked
		# corpus/ dir pristine.
		cp "$CORPUS/$h/"* "$OUT/work_$h/" 2>/dev/null || true
		# The recover harness trips a KNOWN engine leak on the
		# corrupt-log recovery-cleanup path (reported, not a harness
		# bug) -- run it with leak detection off so ASan still catches
		# crashes/OOB without halting on that documented leak.  See
		# crashes/README.md and test/fuzz/README.md.
		leakopt=1
		[ "$h" = recover ] && leakopt=0
		# -artifact_prefix so any crashing input lands in a known dir.
		if ASAN_OPTIONS="detect_leaks=$leakopt" \
			"$OUT/fuzz_$h" "$OUT/work_$h" \
			-max_total_time="$secs" -max_len=65536 -rss_limit_mb=4096 \
			-artifact_prefix="$OUT/artifacts_$h/" \
			-print_final_stats=1; then
			echo "fuzz_$h: no crash"
		else
			echo "fuzz_$h: CRASH or nonzero exit -- see $OUT/artifacts_$h/"
			rc=1
		fi
	done
	return $rc
}

repro() {
	h=$1; file=$2
	case " $HARNESSES " in *" $h "*) ;; *) die "unknown harness: $h";; esac
	[ -f "$file" ] || die "no such file: $file"
	FUZZ_STANDALONE=1 build_one "$h"
	echo "=== replay $file through fuzz_$h ==="
	"$OUT/fuzz_${h}_standalone" "$file"
}

cmd=${1:-smoke}
case "$cmd" in
	build) build_all ;;
	smoke) shift; smoke "$@" ;;
	repro) shift; [ $# -eq 2 ] || die "usage: run.sh repro H FILE"; repro "$@" ;;
	*) die "usage: run.sh {build|smoke [SECONDS]|repro H FILE}" ;;
esac

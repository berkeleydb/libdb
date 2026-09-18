#!/bin/sh -
#
# $Id$
#
# run_cov_fuzz_corpus.sh --
#	Replay the committed fuzz corpus + crash seeds through the fuzz
#	harnesses built in FUZZ_STANDALONE mode against the COVERAGE-
#	instrumented libdb, so the branches those inputs reach are measured.
#
#	Why this is a measurement gap, not a new test.  test/fuzz/run.sh and
#	check-crashes.sh both build their own libdb (clang + ASan/libFuzzer,
#	in build_asan_gate/ or a separate build_unix), because their job is
#	memory-safety detection.  That means the corpus never runs against
#	the gcov-instrumented tree, so the code it covers -- the corrupt-input
#	rejection branches of db_vrfy.c / bt_verify.c / hash_verify.c /
#	qam_verify.c / heap_verify.c / db_conv.c / __memp_fopen's header
#	validation / the recovery log-header parser -- measures as cold even
#	though the seeds exercise it hard.  Those are the "reject malformed
#	input" halves of conditionals: a big, genuinely-covered branch slice
#	that simply was not being counted.
#
#	This script builds the SAME harness sources with FUZZ_STANDALONE (a
#	main() that replays file arguments, no libFuzzer runtime needed) and
#	links them against the instrumented .so, then feeds every file in
#	test/fuzz/corpus/<h>/ and every test/fuzz/crashes/<h>_*.seed to the
#	matching harness.
#
#	It does NOT assert memory safety -- check-crashes.sh owns that gate
#	and needs an ASan build to do it properly.  Here a seed that makes the
#	engine abort is expected for some inputs (an --enable-diagnostic build
#	panics by design on unrecoverable corruption), so each replay runs in
#	its own timeout-guarded child and a crash is counted, not fatal.  The
#	assertion is that every seed gets replayed.
#
# Usage (from build_unix):
#	sh ../test/c/run_cov_fuzz_corpus.sh

set -u

BUILD=${BUILD:-.}
FUZZDIR=${FUZZDIR:-../test/fuzz}
TIMEOUT=${TIMEOUT:-30}		# per-seed
RUNDIR=${RUNDIR:-FUZZCOV_TESTDIR}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }
LIBS_DIR=$(cd "$BUILD/.libs" && pwd)

EXTRA_LIBS="-lpthread"
if echo 'int main(){return 0;}' > /tmp/_covfuzz_probe.c && \
    gcc /tmp/_covfuzz_probe.c -luring -o /tmp/_covfuzz_probe 2>/dev/null; then
	EXTRA_LIBS="$EXTRA_LIBS -luring"
fi
rm -f /tmp/_covfuzz_probe.c /tmp/_covfuzz_probe 2>/dev/null || true

mkdir -p "$BUILD/$RUNDIR"
find "$BUILD/$RUNDIR" -mindepth 1 -delete 2>/dev/null || true

built=0
replayed=0
crashed=0

for h in dbfile recover api; do
	src="$FUZZDIR/fuzz_$h.c"
	[ -f "$src" ] || continue
	bin="$BUILD/cov_fuzz_$h"
	echo "Compiling fuzz_$h (standalone) against $LIB"
	if ! gcc -g -O1 ${CFLAGS:-} -DFUZZ_STANDALONE \
	    -I"$BUILD" -I../src -I../src/dbinc -I"$FUZZDIR" \
	    "$src" "$LIB" $EXTRA_LIBS -Wl,-rpath,"$LIBS_DIR" -o "$bin" \
	    2>/tmp/covfuzz-$h-build.log; then
		echo "  SKIP fuzz_$h (did not compile)"
		head -20 /tmp/covfuzz-$h-build.log
		continue
	fi
	built=$((built + 1))

	# Collect this harness's inputs: its corpus dir + its crash seeds.
	set -- 
	if [ -d "$FUZZDIR/corpus/$h" ]; then
		for f in "$FUZZDIR/corpus/$h"/*; do
			[ -f "$f" ] && set -- "$@" "$f"
		done
	fi
	for f in "$FUZZDIR/crashes/${h}_"*.seed; do
		[ -f "$f" ] && set -- "$@" "$f"
	done
	[ $# -eq 0 ] && { echo "  no inputs for fuzz_$h"; continue; }

	echo "  replaying $# inputs through fuzz_$h"
	for f in "$@"; do
		# Each seed in its own child + cwd so a panic cannot wedge the
		# loop and artifacts do not accumulate.
		( cd "$BUILD/$RUNDIR" && \
		  timeout "$TIMEOUT" "../$(basename "$bin")" "../../${f#../}" \
		    >/dev/null 2>&1 ) || crashed=$((crashed + 1))
		replayed=$((replayed + 1))
		# Clean between seeds: each harness writes a scratch db file.
		find "$BUILD/$RUNDIR" -mindepth 1 -delete 2>/dev/null || true
	done
done

echo "run_cov_fuzz_corpus.sh: built=$built harnesses, replayed=$replayed inputs, $crashed non-zero exits"
if [ "$built" -eq 0 ]; then
	echo "run_cov_fuzz_corpus.sh: SKIP (no harness compiled)"
	exit 0
fi
if [ "$replayed" -eq 0 ]; then
	echo "run_cov_fuzz_corpus.sh: FAIL (nothing replayed)"
	exit 1
fi
# Non-zero exits are expected for corrupt-input seeds on a diagnostic build;
# check-crashes.sh is the memory-safety gate, not this script.
echo "run_cov_fuzz_corpus.sh: PASS"
exit 0

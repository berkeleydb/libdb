#!/bin/sh
# S5 regression: DB_ENV->failchk must TERMINATE when a dead transactional
# locker holds only a DB_LOCK_SIREAD marker.
#
# Drives BOTH lk_partitions=1 and lk_partitions=10.  That second arm is the
# point: the tracker originally filed S5 as an "lk_partitions=1 failure", and
# the identical non-progress shape reproduces with the default partitioning, so
# the v2026.09.6 latch-alias fix was never going to address it and neither would
# any further one-partition latch work.
#
# The driver reports a hang AS A HANG from its own SIGALRM handler and emits a
# VERDICT line.  An outer `timeout` would turn the spin into rc=124, and rc is
# not a verdict -- a missing VERDICT line is treated as failure here.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
# $1, else $BUILD (what run_all.sh sets), else the in-tree build_unix.
# Honouring $BUILD matters: with only $1, run_all.sh -- which passes no
# argument -- silently tested a stale ../../build_unix instead of the build
# under test, or failed outright with a sibling build dir.
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/../c/s5_proof.c

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
[ -n "$LIB" ] || { echo "run_s5_failchk_spin.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/s5_proof
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" -I"$HERE/../../src" \
    "$SRC" "$LIB" -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_s5_failchk_spin.sh: FAIL compile" ; exit 1 ; }
# A stale binary from a previous run would be a false pass.
test -x "$BIN" || { echo "run_s5_failchk_spin.sh: FAIL no binary produced" ; exit 1 ; }

rc=0
for parts in 1 10 ; do
	dir=$BUILD/TESTDIR_s5_$parts
	# find, not `rm -f "$dir"/*': the glob overflows ARG_MAX once a spinning
	# (unfixed) run has left hundreds of thousands of files behind, and the
	# resulting failure then reports the wrong cause.
	[ -d "$dir" ] && find "$dir" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$dir" 2>/dev/null
	# Spool to a file rather than a shell variable: the UNFIXED engine emits
	# ~3.1 GB of BDB2053 here, which overflows the variable and makes the
	# NEXT execve() fail with E2BIG ("Argument list too long") -- so the
	# must-fail arm would fail for a harness reason and never reach its
	# verdict. head -c also bounds disk use on the spinning arm.
	log=$BUILD/s5_out_$parts.log
	(cd "$dir" && "$BIN" "$parts" 2>&1) | head -c 2000000 > "$log"
	# Only the tail matters; the spin repeats one line indefinitely.
	tail -40 "$log" | sed "s/^/  [parts=$parts] /"
	out=$(tail -200 "$log")

	# The driver indents its verdict, so anchoring at ^ finds nothing and a
	# PASSING run is reported as "no VERDICT line".  Match the token with
	# leading whitespace allowed, but still require the exact test name so a
	# stray line cannot satisfy it.
	v=$(echo "$out" | grep -E '^[[:space:]]*VERDICT s5_failchk_spin' | head -1)
	if [ -z "$v" ] ; then
		# Distinguish the two ways a VERDICT can be missing, because
		# "no VERDICT line" alone describes the spin as a harness
		# problem.  A flood of BDB2053 is the S5 signature itself: the
		# driver is still inside DB_ENV->failchk and never reached its
		# own alarm handler's print.
		n=$(grep -c 'BDB2053' "$log")
		if [ "$n" -gt 1000 ] ; then
			echo "run_s5_failchk_spin.sh: FAIL (parts=$parts) __lock_failchk is SPINNING -- $n BDB2053 lines in the first 2MB, no verdict reached (S5)"
		else
			echo "run_s5_failchk_spin.sh: FAIL no VERDICT line (parts=$parts); see $log"
		fi
		rc=1
		continue
	fi
	case "$v" in
	*PASS*)	: ;;
	*SKIP*)	echo "  [parts=$parts] skipped" ;;
	*)	rc=1 ;;
	esac
done

[ "$rc" -eq 0 ] && { echo "run_s5_failchk_spin.sh: PASS" ; exit 0 ; }
echo "run_s5_failchk_spin.sh: FAIL"
exit 1

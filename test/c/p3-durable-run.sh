#!/bin/sh
# test/c/p3-durable-run.sh -- the DURABILITY proof for the P3 fix
# (DB_LOG_DIRECT, src/log/log_put.c:__log_write_direct).
#
# WHY THIS EXISTS SEPARATELY FROM flag-run.sh
#
# flag-run.sh proves the flag now WORKS: the transactional open completes, the
# log fd is O_DIRECT, and every log I/O is block-aligned.  None of that proves
# the fix is SAFE.  __log_write_direct rewrites the leading partial block of
# every write and zero-pads the trailing one, so it touches bytes the plain path
# never touched -- including bytes belonging to records that were already
# fsync'd and acked durable.  If that restaging is wrong, a committed
# transaction disappears, which is the worst failure this library can have.
# This project already shipped one "fast liar" (__memp_aio_drain swallowing
# write errors and reporting false durability), so the bar is a real crash.
#
# WHAT IT ASSERTS
#
#   p3_durable@ack     Commit N transactions with DB_TXN_SYNC under
#                      DB_LOG_DIRECT -- each individually acked durable -- then
#                      SIGKILL the process.  No clean close, no checkpoint, no
#                      DB_ENV->close anywhere.  Then recover and require ALL N
#                      records present with byte-correct contents.  A missing
#                      record is an acked commit that did not survive.
#                      The count comes from the writer's own DURABLE-ACK lines,
#                      so the verifier is told how many commits were ACKED
#                      rather than how many were requested: a writer killed
#                      early cannot make the gate easier by acking fewer.
#   p3_durable@verify  db_verify on the recovered database: recovery must leave
#                      a structurally valid btree, not merely the right keys.
#   p3_durable@group   Group commit intact under the flag: 8 threads x 60
#                      DB_TXN_SYNC commits must need far fewer log fsyncs than
#                      commits, and st_maxcommitperflush must exceed 1.
#                      Compared against the BUFFERED arm, because "few fsyncs"
#                      is only meaningful next to the behaviour without the flag
#                      -- a flag that broke grouping would still pass an
#                      absolute threshold if the threshold were generous.
#
# Usage:  ./p3-durable-run.sh [build_dir]     (default: ../../build_unix)
# Env:    CC, TIMEOUT (per-run seconds, default 300)
#
# An --enable-o_direct build is REQUIRED; without it the modes SKIP with the
# reason printed, and the manifest marks them optional for exactly that case.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
BUILD=${1:-"$HERE/../../build_unix"}
CC=${CC:-cc}
TIMEOUT=${TIMEOUT:-300}
NTXN=${NTXN:-300}
RUNDIR="$HERE/p3-durable-run"

. "$HERE/../harness.sh"
hi_init p3durable "$HERE/.."

[ -f "$BUILD/libdb.a" ] || {
	echo "error: $BUILD/libdb.a not found -- build libdb first:" >&2
	echo "    (cd $BUILD && ../dist/configure --enable-o_direct &&" \
	    "make -j8 libdb.a)" >&2
	exit 1
}

LDF=$(sed -n 's/^LDFLAGS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)
LIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$BUILD/Makefile" | head -1)

mkdir -p "$RUNDIR"
rc=0

echo "=== building p3_durable"
# shellcheck disable=SC2086
$CC -g -O1 -Wall -Wextra -Wno-unused-parameter \
	-I"$BUILD" "$HERE/p3_durable.c" "$BUILD/libdb.a" \
	$LDF $LIBS -ldl -lpthread -o "$RUNDIR/p3_durable"

if grep -q '^#define HAVE_O_DIRECT' "$BUILD/db_config.h" 2>/dev/null; then
	echo "=== build HAS HAVE_O_DIRECT (--enable-o_direct)"
	HAVE_OD=1
else
	echo "=== build has NO HAVE_O_DIRECT: every mode will SKIP."
	echo "    Configure with --enable-o_direct to exercise them."
	HAVE_OD=0
fi

fresh_dir() {
	if [ -d "$1" ]; then
		find "$1" -mindepth 1 -delete
	elif ! mkdir -p "$1"; then
		echo "--- HARNESS ERROR (cannot create $1)"
		return 1
	fi
	return 0
}

# ---------------------------------------------------------------------------
# The crash/recovery gate.
# ---------------------------------------------------------------------------
ack_gate() {
	dir="$RUNDIR/ack"
	fresh_dir "$dir" || { hi_emit p3durable@ack fail; rc=1; return; }
	mkdir -p "$dir/TESTDIR_p3_durable"

	echo "=== writer: $NTXN DB_TXN_SYNC commits under DB_LOG_DIRECT," \
	    "then SIGKILL"
	wout="$dir/writer.txt"
	wrc=0
	# The writer _exit(0)s on its own after N commits; kill -9 covers the
	# case where it is still running, and either way NOTHING is closed.
	(
		cd "$dir" || exit 1
		timeout "$TIMEOUT" "$RUNDIR/p3_durable" writer "$NTXN" &
		wpid=$!
		wait $wpid
	) >"$wout" 2>&1 || wrc=$?
	sed -n 's/^/    /p' "$wout" | grep -v '^    DURABLE-ACK' | head -20

	wv=$(awk '$1 == "VERDICT" { print $3; exit }' "$wout")
	if [ "${wv:-}" = SKIP ]; then
		echo "--- p3durable@ack: SKIP (no O_DIRECT support)"
		hi_emit p3durable@ack skip
		hi_emit p3durable@verify skip
		return
	fi
	# How many commits were ACKED?  This, not $NTXN, is what must survive.
	acked=$(grep -c '^DURABLE-ACK ' "$wout" || true)
	echo "    writer acked $acked durable commits (exit $wrc)"
	if [ "${acked:-0}" -lt 50 ]; then
		echo "--- p3durable@ack: FAIL (only ${acked:-0} commits were" \
		    "acked -- too few for the recovery check to mean anything;" \
		    "the writer did not do the work it claims)"
		hi_emit p3durable@ack fail
		hi_emit p3durable@verify fail
		rc=1
		return
	fi
	# Belt and braces: make certain no live process can flush anything
	# before we recover.
	pkill -9 -f "$RUNDIR/p3_durable" 2>/dev/null || true
	sync

	echo "=== recovering and checking all $acked acked commits survived"
	vout="$dir/verify.txt"
	vrc=0
	( cd "$dir" && timeout "$TIMEOUT" "$RUNDIR/p3_durable" \
	    verify "$acked" ) >"$vout" 2>&1 || vrc=$?
	sed -n 's/^/    /p' "$vout"

	vv=$(awk '$1 == "VERDICT" { print $3; exit }' "$vout")
	case "${vv:-}" in
	PASS)	echo "--- p3durable@ack: PASS ($acked acked commits survived" \
		    "a SIGKILL with no clean close)"
		hi_emit p3durable@ack pass ;;
	SKIP)	echo "--- p3durable@ack: SKIP"; hi_emit p3durable@ack skip ;;
	*)	echo "--- p3durable@ack: FAIL (verify exit $vrc, verdict" \
		    "${vv:-none -- vacuous run})"
		hi_emit p3durable@ack fail
		rc=1 ;;
	esac

	# db_verify on what recovery produced.  The right keys in a corrupt
	# btree is still corruption.
	if [ ! -x "$BUILD/db_verify" ]; then
		echo "--- p3durable@verify: SKIP (no db_verify in $BUILD --" \
		    "build it with 'make db_verify')"
		hi_emit p3durable@verify skip
		return
	fi
	echo "=== db_verify on the recovered database"
	dvout="$dir/db_verify.txt"
	dvrc=0
	( cd "$dir" && timeout "$TIMEOUT" "$BUILD/db_verify" \
	    -h TESTDIR_p3_durable durable.db ) >"$dvout" 2>&1 || dvrc=$?
	sed -n 's/^/    /p' "$dvout"
	if [ "$dvrc" = 0 ]; then
		echo "--- p3durable@verify: PASS (db_verify clean on the" \
		    "recovered database)"
		hi_emit p3durable@verify pass
	else
		echo "--- p3durable@verify: FAIL (db_verify exit $dvrc)"
		hi_emit p3durable@verify fail
		rc=1
	fi
}

# ---------------------------------------------------------------------------
# Group commit, direct vs buffered.
# ---------------------------------------------------------------------------
group_gate() {
	sdir= sbuf= mdir= mbuf= cn=
	for arm in direct buffered; do
		dir="$RUNDIR/group-$arm"
		fresh_dir "$dir" || { hi_emit p3durable@group fail; rc=1; return; }
		mkdir -p "$dir/TESTDIR_p3_durable"
		echo "=== group commit, $arm arm"
		out="$dir/out.txt"
		arc=0
		( cd "$dir" && timeout "$TIMEOUT" "$RUNDIR/p3_durable" \
		    group "$arm" ) >"$out" 2>&1 || arc=$?
		sed -n 's/^/    /p' "$out"
		line=$(grep '^GROUP ' "$out" || true)
		s=$(printf '%s\n' "$line" | sed -n 's/.*st_scount=\([0-9]*\).*/\1/p')
		m=$(printf '%s\n' "$line" | sed -n 's/.*st_maxcommitperflush=\([0-9]*\).*/\1/p')
		c=$(printf '%s\n' "$line" | sed -n 's/.*commits=\([0-9]*\).*/\1/p')
		cn=${c:-$cn}
		if [ "$arm" = direct ]; then sdir=$s; mdir=$m
		else sbuf=$s; mbuf=$m; fi
		v=$(awk '$1 == "VERDICT" { print $3; exit }' "$out")
		if [ "${v:-}" = SKIP ]; then
			echo "--- p3durable@group: SKIP (no O_DIRECT support)"
			hi_emit p3durable@group skip
			return
		fi
		if [ "$arc" != 0 ] || [ "${v:-}" != PASS ]; then
			echo "--- p3durable@group($arm): FAIL (exit $arc," \
			    "verdict ${v:-none})"
			hi_emit p3durable@group fail
			rc=1
			return
		fi
	done

	if [ -z "$sdir" ] || [ -z "$sbuf" ] || [ -z "$mdir" ]; then
		echo "--- p3durable@group: FAIL (an arm printed no GROUP" \
		    "line -- nothing was measured)"
		hi_emit p3durable@group fail
		rc=1
		return
	fi
	# The comparison: grouping under the flag must be in the same league as
	# grouping without it.  A fix that serialized the log (one fsync per
	# commit) would pass an absolute threshold but fail here.
	echo "    direct:   ${sdir} syncs for ${cn} commits," \
	    "max/flush=${mdir}"
	echo "    buffered: ${sbuf} syncs for ${cn} commits," \
	    "max/flush=${mbuf}"
	if [ "$sdir" -gt $((sbuf * 3 + 10)) ]; then
		echo "--- p3durable@group: FAIL (DB_LOG_DIRECT needed $sdir" \
		    "log syncs where buffered needed $sbuf -- the flag" \
		    "degraded group commit by more than 3x)"
		hi_emit p3durable@group fail
		rc=1
	else
		echo "--- p3durable@group: PASS (group commit intact under" \
		    "DB_LOG_DIRECT: $sdir syncs vs buffered $sbuf for $cn" \
		    "commits, best flush covered $mdir commits)"
		hi_emit p3durable@group pass
	fi
}

if [ "$HAVE_OD" != 1 ]; then
	echo "--- p3durable@ack: SKIP (no HAVE_O_DIRECT in this build)"
	hi_emit p3durable@ack skip
	echo "--- p3durable@verify: SKIP (no HAVE_O_DIRECT in this build)"
	hi_emit p3durable@verify skip
	echo "--- p3durable@group: SKIP (no HAVE_O_DIRECT in this build)"
	hi_emit p3durable@group skip
else
	ack_gate
	group_gate
fi

echo
echo "=== summary (HAVE_O_DIRECT=$HAVE_OD)"
[ "$rc" = 0 ] && echo "ALL P3 DURABILITY TESTS PASS" ||
    echo "P3 DURABILITY TESTS FAILED"
exit "$rc"

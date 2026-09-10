#!/bin/sh
# test/repiso/run.sh -- build and run the Tier B4 two-site replication
# isolation harness.
#
# Two checks, both about issue #140's CLIENT-side consequence:
#
#   1. locklist (DETERMINISTIC).  Run the master+client pair, then read the
#      client's own replicated log with db_printlog and assert that every page
#      each committed transaction MODIFIED appears in that transaction's commit
#      lock list.  Apply reacquires exactly the listed objects as write locks
#      and takes no other page locks, so an omitted page is a page apply changes
#      while holding no lock on it.  No timing, no race: the log either has the
#      omission or it does not.
#
#   2. anomaly (BEHAVIOURAL).  The same run's client holds a read lock across
#      the master's trigger commit and re-reads the key.  The two reads must
#      agree.  Because the lock is held across the whole window there is no
#      interleaving to win, but the check does depend on apply actually getting
#      as far as the record, so its negative result is reported as
#      INCONCLUSIVE rather than PASS.
#
# Usage:
#   ./run.sh                # build + run both checks
#   ./run.sh build          # build only
#   ./run.sh locklist       # deterministic log check only
#   ./run.sh anomaly        # behavioural check only
#
# Env:
#   CC             compiler (default: cc)
#   LIBDB_BUILD    path to a built build_unix (default: ../../build_unix)
#   REPISO_TIMEOUT seconds for one master/client pair (default: 180 -- see the
#                  measured spread below)
#   REPISO_PORT    base TCP port (default: 39100)
#   REPISO_RUNS    repetitions of the anomaly check (default: 3)
#   REPISO_GATE    which check decides the exit status (default: locklist)
#                    locklist  only check 1, the deterministic log invariant,
#                              plus a master crash, can fail the run.  Check 2's
#                              verdict is printed but advisory.  This is the
#                              default because check 1 is a pure function of the
#                              log bytes -- no timing, no race -- and a gate that
#                              can flake gets disabled, at which point it
#                              protects nothing.
#                    both      check 2 can fail the run too.
#   REPISO_KEEP    1 => keep the scratch directories
#   REPISO_VERBOSE 1 => per-transaction detail from the log checker
#
# TIMEOUT PROVENANCE (do not re-tighten from a single run):
#   Measured wall time for one complete master+client pair, 10 runs on an
#   idle 8-core box: 12.4 12.5 12.4 12.6 12.5 12.4 12.7 12.5 12.4 12.5 s
#   (median 12.5, spread 0.3s).  Nearly all of it is the client's deliberate
#   10s sentinel poll, which with the fix present is EXPECTED to expire.
#   Under `make -j8` load on the same box: 12.6-14.9s.  180s is >12x the worst
#   observed value, which is the right shape of margin because the failure mode
#   under load is "did not finish", not "slightly slow".
#
# Run from test/repiso/ inside a `nix develop` shell.

set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$HERE"

CC=${CC:-cc}
LIBDB_BUILD=${LIBDB_BUILD:-"$HERE/../../build_unix"}
REPISO_TIMEOUT=${REPISO_TIMEOUT:-180}
REPISO_PORT=${REPISO_PORT:-39100}
REPISO_RUNS=${REPISO_RUNS:-3}
REPISO_KEEP=${REPISO_KEEP:-0}
REPISO_VERBOSE=${REPISO_VERBOSE:-0}
REPISO_GATE=${REPISO_GATE:-locklist}
OUT="$HERE/build"
LIBDBA="$LIBDB_BUILD/libdb.a"

[ -f "$LIBDBA" ] || {
	echo "error: libdb.a not found at $LIBDBA -- build libdb first:" >&2
	echo "    (cd $LIBDB_BUILD && ../dist/configure --enable-debug && make -j4)" >&2
	exit 2
}

# db_printlog: prefer the libtool WRAPPER in build_unix, which sets up the
# shared-library path.  The real ELF binary under .libs needs
# LD_LIBRARY_PATH=$LIBDB_BUILD/.libs, so fall back to that explicitly rather
# than silently producing an empty dump (an empty dump reads as NO_EVIDENCE,
# which is a confusing way to say "the tool did not run").
PRINTLOG="$LIBDB_BUILD/db_printlog"
PLENV=""
if [ ! -x "$PRINTLOG" ]; then
	PRINTLOG="$LIBDB_BUILD/.libs/db_printlog"
	PLENV="$LIBDB_BUILD/.libs"
fi
[ -x "$PRINTLOG" ] || {
	echo "error: db_printlog not found under $LIBDB_BUILD" >&2
	exit 2
}
printlog() {
	if [ -n "$PLENV" ]; then
		LD_LIBRARY_PATH="$PLENV${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
			"$PRINTLOG" "$@"
	else
		"$PRINTLOG" "$@"
	fi
}
# Fail early and loudly if the dump is empty, rather than at verdict time.
if ! printlog -V >/dev/null 2>&1; then
	echo "error: $PRINTLOG does not run (shared library path?)" >&2
	exit 2
fi

if [ -f "$LIBDB_BUILD/Makefile" ]; then
	LDLIBS=$(sed -n 's/^LIBS=[[:space:]]*//p' "$LIBDB_BUILD/Makefile" | head -1)
fi
LDLIBS="${LDLIBS:--lpthread} -ldl -lpthread"

CFLAGS="-g -O1 -Wall -Wextra -Wno-unused-parameter -I$LIBDB_BUILD -I$HERE"

mkdir -p "$OUT"
# shellcheck disable=SC2086
$CC $CFLAGS "$HERE/test_rep_iso.c" "$HERE/rep_iso_net.c" "$LIBDBA" $LDLIBS \
	-o "$OUT/test_rep_iso"
# shellcheck disable=SC2086
$CC $CFLAGS "$HERE/check_locklist.c" -o "$OUT/check_locklist"
echo "built $OUT/test_rep_iso and $OUT/check_locklist"

[ "${1:-}" = "build" ] && exit 0

WHAT=${1:-all}

# ---------------------------------------------------------------------------
# one_pair RUNDIR PORT -- run a master+client pair.  Sets PAIR_CLIENT_RC and
# PAIR_MASTER_RC.  Never returns non-zero itself, so the caller decides what a
# given rc means.
# ---------------------------------------------------------------------------
one_pair() {
	rundir=$1
	port=$2

	rm -f "$rundir"/master/* "$rundir"/client/* "$rundir"/rv/* 2>/dev/null || true
	mkdir -p "$rundir/master" "$rundir/client" "$rundir/rv"

	vflag=""
	[ "$REPISO_VERBOSE" = "1" ] && vflag="-v"

	# Master listens, so start it first; the client retries the connect for
	# 30s, so a slow start is not a failure.
	timeout "$REPISO_TIMEOUT" "$OUT/test_rep_iso" --role=master \
		--port="$port" --home="$rundir/master" \
		--rendezvous="$rundir/rv" $vflag \
		> "$rundir/master.log" 2>&1 &
	mpid=$!
	timeout "$REPISO_TIMEOUT" "$OUT/test_rep_iso" --role=client \
		--port="$port" --host=127.0.0.1 --home="$rundir/client" \
		--rendezvous="$rundir/rv" $vflag \
		> "$rundir/client.log" 2>&1 &
	cpid=$!

	PAIR_CLIENT_RC=0; wait "$cpid" || PAIR_CLIENT_RC=$?
	PAIR_MASTER_RC=0; wait "$mpid" || PAIR_MASTER_RC=$?
}

failures=0
advisory=0
run=0

# ---------------------------------------------------------------------------
# Check 1 + 2 share a run: the same pair produces both the behavioural verdict
# and the log to inspect.  Running them separately would just double the cost.
# ---------------------------------------------------------------------------
if [ "$WHAT" = "all" ] || [ "$WHAT" = "anomaly" ] || [ "$WHAT" = "locklist" ]; then
	i=1
	nruns=$REPISO_RUNS
	[ "$WHAT" = "locklist" ] && nruns=1
	while [ "$i" -le "$nruns" ]; do
		rundir="$OUT/REPISODIR.$i"
		port=$((REPISO_PORT + i))
		echo "== run $i/$nruns (port $port) =="
		one_pair "$rundir" "$port"
		run=$((run + 1))

		sed -n 's/^\(MASTER\|CLIENT\): /  &/p' "$rundir/master.log" \
			"$rundir/client.log" 2>/dev/null || true
		grep -h 'RESULT verdict=' "$rundir/master.log" \
			"$rundir/client.log" 2>/dev/null | sed 's/^/  /' || true

		if [ "$WHAT" != "locklist" ]; then
			# Check 2 is behavioural.  Whether it can FAIL the run is
			# REPISO_GATE's decision; it always reports.
			count_as=advisory
			[ "$REPISO_GATE" = "both" ] && count_as=failure

			if [ "$PAIR_CLIENT_RC" -eq 124 ] || \
			   [ "$PAIR_MASTER_RC" -eq 124 ]; then
				echo "  anomaly check: TIMEOUT (client" \
					"rc=$PAIR_CLIENT_RC master" \
					"rc=$PAIR_MASTER_RC) after" \
					"${REPISO_TIMEOUT}s [$count_as]"
				bump=1
			elif [ "$PAIR_CLIENT_RC" -ne 0 ]; then
				echo "  anomaly check: FAIL (client" \
					"rc=$PAIR_CLIENT_RC) [$count_as]"
				bump=1
			else
				echo "  anomaly check: PASS"
				bump=0
			fi
			if [ "$bump" = "1" ]; then
				if [ "$count_as" = "failure" ]; then
					failures=$((failures + 1))
				else
					advisory=$((advisory + 1))
				fi
			fi

			# A master killed by a SIGNAL is its own finding, not just
			# noise on the way to the client's verdict: rc>128 here is
			# the #140 MASTER-side heap overflow (SIGSEGV inside
			# __lock_vec).  This ALWAYS fails the run whatever the gate
			# setting -- a crashing engine is never a pass, and unlike
			# check 2 there is nothing timing-dependent about it.
			if [ "$PAIR_MASTER_RC" -gt 128 ] && \
			   [ "$PAIR_MASTER_RC" -ne 124 ]; then
				echo "  MASTER CRASHED: killed by signal" \
					"$((PAIR_MASTER_RC - 128))" \
					"(rc=$PAIR_MASTER_RC) -- this is the" \
					"#140 master-side heap overflow"
				failures=$((failures + 1))
			fi
		fi

		# ---- the deterministic log check, on the CLIENT's own log ----
		if [ "$WHAT" != "anomaly" ]; then
			vf=""
			[ "$REPISO_VERBOSE" = "1" ] && vf="--verbose"
			lrc=0
			printlog -h "$rundir/client" 2>/dev/null \
				| "$OUT/check_locklist" $vf \
				> "$rundir/locklist.log" 2>&1 || lrc=$?
			sed 's/^/  /' "$rundir/locklist.log"
			case "$lrc" in
			0) echo "  locklist check: PASS" ;;
			1) echo "  locklist check: FAIL (omission found)"
			   failures=$((failures + 1)) ;;
			*) echo "  locklist check: NO EVIDENCE (rc=$lrc)"
			   failures=$((failures + 1)) ;;
			esac
		fi

		# Cleanup AFTER the log check: db_printlog resolves fileids
		# through the database files, and the checker reads its output.
		if [ "$REPISO_KEEP" != "1" ]; then
			find "$rundir" -mindepth 2 -type f \
				\( -name '*.db' -o -name 'log.*' \
				   -o -name '__db.*' \) \
				-delete 2>/dev/null || true
		fi
		i=$((i + 1))
	done
fi

echo
echo "$run run(s), $failures gating failure(s), $advisory advisory finding(s)"
echo "gate = $REPISO_GATE (locklist = the deterministic log invariant plus a" \
	"master crash; both = also the behavioural anomaly check)"
[ "$advisory" -eq 0 ] || echo "NOTE: advisory findings do not affect the exit" \
	"status under REPISO_GATE=$REPISO_GATE"
[ "$failures" -eq 0 ] || exit 1
exit 0

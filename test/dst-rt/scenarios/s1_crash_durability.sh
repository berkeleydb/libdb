#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/scenarios/s1_crash_durability.sh <seed> -- RFC 0024 S1.
#
# Local crash-durability (one node):  run a seeded workload, SIGKILL the loader
# at a seeded op count, recover, and check that every ACKed commit survived and
# nothing FAILed is visible (§2.4 rules 1-3, 7).  The directio arm removes the
# page cache so a crash loses everything not on disk; see finding F1-001 for
# why that arm SKIPs against an unmodified engine today.
#
# Two arms:
#   NORMAL    an honest (sync) loader killed mid-run -- every ACKed commit must
#             be present after recovery (durability holds).
#   SABOTAGE  a --nosync loader (DB_TXN_NOSYNC) that still logs ACK -- a
#             durability LIE.  Under directio + kill, acked-but-unsynced writes
#             are lost and rtcheck MUST catch it.  The arm FAILs the oracle on
#             purpose; the scenario PASSES only if the oracle fires (teeth).
#
# Emits (for the manifest gate): RESULT dst-rt s1-normal pass|fail and
# RESULT dst-rt s1-sabotage pass|fail.  Prints a reproduce line.
#
# Usage:  sh scenarios/s1_crash_durability.sh <seed>
# Env:    BUILD (libdb build dir, holds rtload/rtcheck), DSTRT_DIR.

set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
DSTRT_DIR=${DSTRT_DIR:-$(CDPATH= cd -- "$HERE/.." && pwd)}
. "$DSTRT_DIR/lib/fi_env.sh"
. "$DSTRT_DIR/lib/fi_check.sh"
. "$DSTRT_DIR/../harness.sh"

SEED=${1:-1}
OPS=${OPS:-2000}
KILL_AT=${KILL_AT:-$(( (SEED * 37) % 1500 + 200 ))}	# seeded, deterministic
KEYS=${KEYS:-300}
VALSZ=${VALSZ:-200}

echo "# reproduce: BUILD=$BUILD DSTRT_DIR=$DSTRT_DIR sh $0 $SEED"
echo "# seed=$SEED ops=$OPS kill-at=$KILL_AT keys=$KEYS valsz=$VALSZ"

# When driven by run.sh (DSTRT_KEEP_RESULTS=1) do NOT truncate the shared
# results file -- run.sh did the single hi_init so every scenario appends.
if [ "${DSTRT_KEEP_RESULTS:-0}" = 1 ]; then
	hi_tier="dst-rt"
	hi_file="${LIBDB_RESULTS_DIR:-$DSTRT_DIR/../.results}/dst-rt.results"
else
	hi_init "dst-rt" "$DSTRT_DIR/.."
fi

fi_have libdb || { echo "SKIP: no libdb library under $BUILD"; exit 77; }
[ -x "$BUILD/rtload" ] || fi_build_node rtload || { echo "SKIP: rtload build"; exit 77; }
[ -x "$BUILD/rtcheck" ] || fi_build_node rtcheck || { echo "SKIP: rtcheck build"; exit 77; }

RUN=$(fi_mkrun s1)
trap 'rm -rf "$RUN" 2>/dev/null' 0

# directio: two arms (lead direction on F1-001).
#   native  -- the SUPPORTED path: a build with --enable-o_direct, DB_DIRECT_DB
#              + DB_LOG_DIRECT (rtload --direct).  Verified alignment-clean.
#   preload -- libdirectio, a blanket O_DIRECT on every regular file, which
#              trips F1-001 on the buffered-by-design region files at env open.
# We detect native direct support by probing rtload --direct, and detect the
# preload gap by probing libdirectio.
HAVE_NATIVE_DIRECT=0
probe="$RUN/probe.nd"; mkdir -p "$probe"
if "$BUILD/rtload" --home "$probe" --ack "$RUN/probe.nd.ack" --node p \
    --seed 1 --ops 2 --keys 2 --direct >/dev/null 2>&1; then
	HAVE_NATIVE_DIRECT=1
else
	echo "# native direct I/O unavailable (build without --enable-o_direct)"
fi

PRELOAD_DIRECTIO_OK=0
PRELOAD_DIRECTIO_ENV=""
# Prefer the path-filtered shim (O_DIRECT on *.db and log.* only -- finding
# F1-001): it exercises the uncached crash path for the data/log files and the
# utilities, without tripping libdb's buffered region writes.  Fall back to raw
# libdirectio (which trips F1-001 and so stays a SKIP).
if [ "${HAVE_DIRECTIO_MATCH:-0}" = 1 ]; then
	probe="$RUN/probe.dm"; mkdir -p "$probe"
	if LD_PRELOAD="$DIRECTIO_MATCH_SO" DIRECTIO_MATCH='*.db' \
	    "$BUILD/rtload" --home "$probe" --ack "$RUN/probe.dm.ack" \
	    --node p --seed 1 --ops 2 --keys 2 >/dev/null 2>&1; then
		PRELOAD_DIRECTIO_OK=1
		PRELOAD_DIRECTIO_ENV="LD_PRELOAD=$DIRECTIO_MATCH_SO DIRECTIO_MATCH=*.db"
		# When the build supports native direct I/O, add --direct so the
		# LOG is also O_DIRECT (aligned by DB_LOG_DIRECT); the shim covers
		# the data file *.db, native covers the log -> fullest uncached run.
		[ "$HAVE_NATIVE_DIRECT" = 1 ] && PRELOAD_DIRECT_FLAG="--direct"
	fi
fi
if [ "$PRELOAD_DIRECTIO_OK" = 0 ] && fi_have directio; then
	probe="$RUN/probe.pd"; mkdir -p "$probe"
	if LD_PRELOAD="$DIRECTIO_SO" "$BUILD/rtload" --home "$probe" \
	    --ack "$RUN/probe.pd.ack" --node p --seed 1 --ops 2 --keys 2 \
	    >/dev/null 2>&1; then
		PRELOAD_DIRECTIO_OK=1
		PRELOAD_DIRECTIO_ENV="LD_PRELOAD=$DIRECTIO_SO"
	fi
fi

# run_arm NAME MODE [PRELOAD] [DIRECT] -- kill a loader at KILL_AT, recover,
# check.  MODE: "" honest, "--nosync" sabotage.  PRELOAD: env prefix (e.g.
# "LD_PRELOAD=...").  DIRECT: "--direct" for the native path.  Echoes rtcheck.
run_arm() {
	_name=$1 _mode=$2 _pre=${3:-} _direct=${4:-}
	_home="$RUN/$_name/home"; mkdir -p "$_home"
	_ack="$RUN/$_name/ack.log"; : > "$_ack"

	# First loader: killed at KILL_AT (writes some ACKs, dies mid-stream).
	# shellcheck disable=SC2086
	eval $_pre "$BUILD/rtload" --home "$_home" --ack "$_ack" \
	    --node "$_name" --seed "$SEED" --ops "$OPS" --keys "$KEYS" \
	    --valsz "$VALSZ" --kill-at "$KILL_AT" $_direct $_mode >/dev/null 2>&1
	# rc 137 (SIGKILL) is expected; any other non-zero is a crash, which is
	# itself a rule-7 violation unless it is the kill we asked for.

	# rtcheck opens the home with DB_RECOVER and applies rules 1-3,7.
	"$BUILD/rtcheck" --home "$_home" --ack "$_ack" 2>&1
}

rc=0

# NORMAL arm: honest sync loader.  The oracle must PASS (durability holds).
echo "== s1-normal =="
out=$(run_arm normal "")
echo "$out" | sed 's/^/    /'		# prints acked/failed/unknown counts
if echo "$out" | grep -q '^PASS'; then
	hi_emit s1-normal pass
else
	# includes VACUOUS (0 acked checked) -- a non-vacuity failure.
	hi_emit s1-normal fail; rc=1
	echo "    UNEXPECTED: honest sync loader lost an acked commit (or vacuous)"
fi

# NATIVE DIRECTIO arm: the SUPPORTED direct-I/O path (lead direction on
# F1-001).  A --enable-o_direct build with DB_DIRECT_DB + DB_LOG_DIRECT, killed
# and recovered -- data and log go straight to disk, so recovery is tested with
# the page cache out of the picture.  Verified alignment-clean (FINDINGS F1-001).
echo "== s1-native-directio =="
if [ "$HAVE_NATIVE_DIRECT" = 1 ]; then
	out=$(run_arm native "" "" "--direct")
	echo "$out" | sed 's/^/    /'
	if echo "$out" | grep -q '^PASS'; then
		echo "    native O_DIRECT durable across kill+recovery"
		hi_emit s1-native-directio pass
	else
		hi_emit s1-native-directio fail; rc=1
	fi
else
	echo "    SKIP: libdb not built with --enable-o_direct"
	hi_emit s1-native-directio skip
fi

# SABOTAGE arm: --nosync but still ACKed.  A clean SIGKILL of the loader loses
# libdb's IN-PROCESS log buffer (DB_TXN_NOSYNC never flushed the commit records
# to the fd), so acked-but-unsynced commits are gone after recovery -- WITHOUT
# needing directio.  (Measured: honest sync arm PASSes the same kill; --nosync
# arm loses ~dozens of acked writes.)  Run it under the native direct path when
# available (widens the loss window), else buffered.  PASSES iff the oracle
# FAILED (caught the lie) -- a VACUOUS result here means no acked op existed to
# lose, which is NOT a caught lie, so it is a fail.
echo "== s1-sabotage =="
if [ "$HAVE_NATIVE_DIRECT" = 1 ]; then
	out=$(run_arm sabotage "--nosync" "" "--direct")
else
	out=$(run_arm sabotage "--nosync")
fi
echo "$out" | sed 's/^/    /'
if echo "$out" | grep -q '^FAIL'; then
	echo "    oracle caught the NOSYNC durability lie (teeth)"
	hi_emit s1-sabotage pass
else
	echo "    SABOTAGE NOT CAUGHT: NOSYNC+kill kept every acked commit (or vacuous)"
	hi_emit s1-sabotage fail; rc=1
fi

# PRELOAD DIRECTIO arm: libdirectio (blanket O_DIRECT).  Finding F1-001: this
# trips libdb's buffered-by-design region writes at env open, so it SKIPs with
# the reference until the optional engine fix lands.  SKIP-with-reason, never a
# silent pass.
echo "== s1-preload-directio =="
if [ "${PRELOAD_DIRECTIO_OK:-0}" = 1 ]; then
	out=$(run_arm preload "" "$PRELOAD_DIRECTIO_ENV" "${PRELOAD_DIRECT_FLAG:-}")
	echo "$out" | sed 's/^/    /'
	echo "    (preload: $PRELOAD_DIRECTIO_ENV ${PRELOAD_DIRECT_FLAG:-})"
	if echo "$out" | grep -q '^PASS'; then
		echo "    preload O_DIRECT durable across kill+recovery"
		hi_emit s1-preload-directio pass
	else
		hi_emit s1-preload-directio fail; rc=1
	fi
else
	echo "    SKIP: finding F1-001 -- libdb cannot open an env under a BLANKET"
	echo "          O_DIRECT (libdirectio), and no path-filtered shim is built."
	echo "          The NATIVE direct arm above is the supported path and is clean."
	hi_emit s1-preload-directio skip
fi

exit $rc

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

hi_init "dst-rt" "$DSTRT_DIR/.."

fi_have libdb || { echo "SKIP: no libdb library under $BUILD"; exit 77; }
[ -x "$BUILD/rtload" ] || fi_build_node rtload || { echo "SKIP: rtload build"; exit 77; }
[ -x "$BUILD/rtcheck" ] || fi_build_node rtcheck || { echo "SKIP: rtcheck build"; exit 77; }

RUN=$(fi_mkrun s1)
trap 'rm -rf "$RUN" 2>/dev/null' 0

# directio availability drives whether we exercise the uncached path.  Finding
# F1-001: an unmodified engine cannot open an env under libdirectio, so we run
# the kill+recovery WITHOUT directio and record the directio gap, rather than
# emit a false PASS.
USE_DIRECTIO=0
PRELOAD=""
if fi_have directio; then
	# Probe: can THIS libdb open an env under directio?  If not, F1-001.
	probe="$RUN/probe"; mkdir -p "$probe"
	if LD_PRELOAD="$DIRECTIO_SO" "$BUILD/rtload" --home "$probe" \
	    --ack "$RUN/probe.ack" --node p --seed 1 --ops 2 --keys 2 \
	    >/dev/null 2>&1; then
		USE_DIRECTIO=1; PRELOAD="LD_PRELOAD=$DIRECTIO_SO"
	else
		echo "# directio present but libdb cannot open under it: finding F1-001"
		echo "#   -> running kill+recovery WITHOUT directio (buffered)"
	fi
fi

# ---- arm: run a loader that kills itself at KILL_AT, then recover+check -----
# mode: "" honest, "--nosync" sabotage.  echo PASS/FAIL to stdout.
run_arm() {
	_name=$1 _mode=$2
	_home="$RUN/$_name/home"; mkdir -p "$_home"
	_ack="$RUN/$_name/ack.log"; : > "$_ack"

	# First loader: killed at KILL_AT (writes some ACKs, dies mid-stream).
	# shellcheck disable=SC2086
	eval $PRELOAD "$BUILD/rtload" --home "$_home" --ack "$_ack" \
	    --node "$_name" --seed "$SEED" --ops "$OPS" --keys "$KEYS" \
	    --valsz "$VALSZ" --kill-at "$KILL_AT" $_mode >/dev/null 2>&1
	# rc 137 (SIGKILL) is expected; any other non-zero is a crash, which is
	# itself a rule-7 violation unless it is the kill we asked for.

	# rtcheck opens the home with DB_RECOVER and applies rules 1-3,7.
	"$BUILD/rtcheck" --home "$_home" --ack "$_ack" 2>&1
}

rc=0

# NORMAL arm: honest sync loader.  The oracle must PASS (durability holds).
echo "== s1-normal =="
out=$(run_arm normal "")
echo "$out" | sed 's/^/    /'
if echo "$out" | grep -q '^PASS'; then
	hi_emit s1-normal pass
else
	hi_emit s1-normal fail; rc=1
	echo "    UNEXPECTED: honest sync loader lost an acked commit"
fi

# SABOTAGE arm: --nosync but still ACKed.  A clean SIGKILL of the loader loses
# libdb's IN-PROCESS log buffer (DB_TXN_NOSYNC never flushed the commit records
# to the fd), so acked-but-unsynced commits are gone after recovery -- WITHOUT
# needing directio.  (Measured: honest sync arm PASSes the same kill; --nosync
# arm loses ~dozens of acked writes.)  directio only widens the loss window to
# the OS page cache too; when present we run the sabotage under directio as a
# stronger arm, otherwise the in-process-buffer loss alone gives the oracle
# teeth.  The scenario PASSES iff the oracle FAILED (caught the lie).
echo "== s1-sabotage =="
if [ "$USE_DIRECTIO" = 1 ]; then
	out=$(run_arm sabotage "--nosync")
else
	PRELOAD="" out=$(run_arm sabotage "--nosync")
fi
echo "$out" | sed 's/^/    /'
if echo "$out" | grep -q '^FAIL'; then
	echo "    oracle caught the NOSYNC durability lie (teeth)"
	hi_emit s1-sabotage pass
else
	echo "    SABOTAGE NOT CAUGHT: NOSYNC+kill kept every acked commit"
	hi_emit s1-sabotage fail; rc=1
fi

exit $rc

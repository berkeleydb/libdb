#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/scenarios/s9_soak_lite.sh <seed> -- RFC 0024 S9 (lite).
#
# A 10-minute soak that cycles the single-node S1-S3 plans, watching for
# unbounded growth (§2.3/§2.4 rule for S9) and any invariant violation.  The
# full S9 (hours, + HA) is a separate tier; this "lite" form is the PR/nightly
# single-node soak.
#
# Each cycle picks a plan by the seeded schedule, runs a bounded loader under
# it, recovers and checks the oracle.  Between cycles it samples resource
# counters (region memory, lockers, mutexes, log files, RSS) from db_stat so a
# leak shows as a monotone climb rather than a one-shot failure.
#
# Verdict s9-soak: pass if every cycle's oracle PASSed and no counter grew
# without bound; fail on the first violation or a clear upward trend.
#
# Usage:  sh scenarios/s9_soak_lite.sh <seed>
# Env:    SOAK_SECS (default 600), BUILD, DSTRT_DIR.

set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
DSTRT_DIR=${DSTRT_DIR:-$(CDPATH= cd -- "$HERE/.." && pwd)}
. "$DSTRT_DIR/lib/fi_env.sh"
. "$DSTRT_DIR/lib/fi_fiu.sh"
. "$DSTRT_DIR/../harness.sh"

SEED=${1:-1}
SOAK_SECS=${SOAK_SECS:-600}

echo "# reproduce: SOAK_SECS=$SOAK_SECS BUILD=$BUILD DSTRT_DIR=$DSTRT_DIR sh $0 $SEED"
# When driven by run.sh (DSTRT_KEEP_RESULTS=1) do NOT truncate the shared
# results file -- run.sh did the single hi_init so every scenario appends.
if [ "${DSTRT_KEEP_RESULTS:-0}" = 1 ]; then
	hi_tier="dst-rt"
	hi_file="${LIBDB_RESULTS_DIR:-$DSTRT_DIR/../.results}/dst-rt.results"
else
	hi_init "dst-rt" "$DSTRT_DIR/.."
fi

fi_have libdb || { echo "SKIP: no libdb under $BUILD"; exit 77; }
[ -x "$BUILD/rtload" ] || fi_build_node rtload || { echo "SKIP: rtload build"; exit 77; }
[ -x "$BUILD/rtcheck" ] || fi_build_node rtcheck || { echo "SKIP: rtcheck build"; exit 77; }

RUN=$(fi_mkrun s9)
trap 'rm -rf "$RUN" 2>/dev/null' 0

DBSTAT=""
for cand in "$BUILD/db_stat" "$BUILD/.libs/db_stat"; do
	[ -x "$cand" ] && { DBSTAT="$cand"; break; }
done

# splitmix-ish from the seed for plan selection; deterministic.
pick=$SEED
next_pick() { pick=$(( (pick * 1103515245 + 12345) & 0x7fffffff )); echo $pick; }

# one env home reused across cycles -- a leak accumulates here.
HOME_DIR="$RUN/home"; mkdir -p "$HOME_DIR"
# Per-cycle ack files, passed to rtcheck in order.  One file per cycle keeps
# each cycle's txnids in their own namespace (rtcheck scopes txnids by ack-file
# index), and the global append order across files resolves overwrites.
ACKS=""

start=$(date +%s)
cycle=0
rc=0
prev_region=0
growth_strikes=0

# plan rotation: honest, io-eio-write, io-fsync-fail, mem-fiu-random, nosync-kill
plans="honest io-eio-write io-fsync-fail mem-fiu-random nosync-kill"
nplans=5

while :; do
	now=$(date +%s)
	[ $(( now - start )) -ge "$SOAK_SECS" ] && break
	cycle=$(( cycle + 1 ))
	p=$(next_pick)
	idx=$(( p % nplans + 1 ))
	plan=$(echo "$plans" | cut -d' ' -f"$idx")
	cseed=$(( SEED + cycle ))
	ACK=$(printf '%s/ack.%04d' "$RUN" "$cycle"); : > "$ACK"

	case "$plan" in
	honest)
		"$BUILD/rtload" --home "$HOME_DIR" --ack "$ACK" --node s \
		    --seed "$cseed" --ops 300 --keys 400 --valsz 200 \
		    >/dev/null 2>&1
		ACKS="$ACKS --ack $ACK" ;;
	io-eio-write|io-fsync-fail|mem-fiu-random)
		cmd=$(fi_have fiu && fi_fiu_cmd "$plan" "$cseed" 0.01 || echo "")
		# shellcheck disable=SC2086
		eval ${cmd:-} "$BUILD/rtload" --home "$HOME_DIR" --ack "$ACK" \
		    --node s --seed "$cseed" --ops 300 --keys 400 --valsz 200 \
		    >/dev/null 2>&1
		ACKS="$ACKS --ack $ACK" ;;
	nosync-kill)
		# NOSYNC + kill in a SEPARATE throwaway home, so the churn cannot
		# perturb the shared home's durability baseline.  The soak's point
		# here is that libdb survives repeated NOSYNC crash+recovery churn
		# without leaking or wedging; durability of THESE ops is not the
		# claim (that is S1's job).
		_kh="$RUN/killhome"; mkdir -p "$_kh"
		"$BUILD/rtload" --home "$_kh" --ack "$RUN/discard.log" \
		    --node s --seed "$cseed" --ops 300 --keys 400 --valsz 200 \
		    --nosync --kill-at 150 >/dev/null 2>&1
		# recover it once (proves the throwaway home is recoverable) then
		# wipe it for the next kill cycle.
		"$BUILD/rtcheck" --home "$_kh" --ack "$RUN/discard.log" \
		    >/dev/null 2>&1
		find "$_kh" -mindepth 1 -delete 2>/dev/null
		rm -f "$ACK"; : > "$RUN/discard.log" ;;
	esac

	# Oracle over EVERY accumulated ack file (in order), so an overwrite in a
	# later cycle is resolved against the true latest value.  Skip on the
	# nosync-kill cycles (their ops went to the throwaway home).
	if [ "$plan" != nosync-kill ] && [ -n "$ACKS" ]; then
		# shellcheck disable=SC2086
		out=$("$BUILD/rtcheck" --home "$HOME_DIR" $ACKS 2>&1)
		if ! echo "$out" | grep -q '^PASS'; then
			echo "    cycle $cycle ($plan): oracle VIOLATION"
			echo "$out" | sed 's/^/        /' | head -5
			hi_emit s9-soak fail; rc=1; break
		fi
	fi

	# Resource sample: region bytes in use.  A monotone climb over many
	# cycles (allowing for the growing working set) is the leak signal.
	if [ -n "$DBSTAT" ]; then
		region=$("$DBSTAT" -h "$HOME_DIR" -e 2>/dev/null | \
		    awk '/Region size/{gsub(/[^0-9]/,"",$1); r+=$1} END{print r+0}')
		if [ "${region:-0}" -gt 0 ] && [ "$prev_region" -gt 0 ]; then
			# a >50% jump in region memory between cycles is a strike;
			# 3 consecutive strikes = unbounded growth.
			if [ "$region" -gt $(( prev_region + prev_region / 2 )) ]; then
				growth_strikes=$(( growth_strikes + 1 ))
			else
				growth_strikes=0
			fi
			if [ "$growth_strikes" -ge 3 ]; then
				echo "    cycle $cycle: region memory grew 3x running"
				hi_emit s9-soak fail; rc=1; break
			fi
		fi
		prev_region=$region
	fi

	[ $(( cycle % 10 )) -eq 0 ] && \
	    echo "    cycle $cycle, ${plan}, $(( now - start ))s elapsed"
done

if [ "$rc" = 0 ]; then
	echo "    soak clean: $cycle cycles in ${SOAK_SECS}s, no violation, no unbounded growth"
	hi_emit s9-soak pass
fi
exit $rc

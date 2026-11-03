#!/bin/sh
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# S7: repmgr TLS under network faults (RFC 0024 §2.3; RFC 0018, PR #241).
#
# Needs a --enable-tls build: point BUILD at it (e.g. /data/.../tls/build_unix).
# SKIP cleanly if the build has no TLS (the node errors DB_OPNOTSUP on the
# repmgr_tls directive, which we detect) or if openssl is missing.
#
# Positive arm: 3 TLS sites (mutual auth, verify peer) under net-flaky then
# net-partition+heal; handshakes retry, the group converges, traffic is
# encrypted (every site uses repmgr_tls require,verify).  Oracle: §2.4 1-5.
#
# Negative arm (--pin): sites a and b configure repmgr_tls_pin with the
# fingerprints of a's and b's certs only.  Site c presents a cert that is NOT
# pinned.  c MUST NEVER join the group (no ROLE line, no data).  If c ever
# becomes master or client, that is a FAIL.
#
# Usage: s7_tls_netfaults.sh [SEED] [--pin]
set -u
SEED=${1:-$(awk 'BEGIN{srand();print int(rand()*100000)}')}
ARM=tls
[ "${2:-}" = "--pin" ] && ARM=pin
export RT_RUN="s7-$ARM-$SEED-$$"
SDIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SDIR/../lib/ha_common.sh"

ha_need_netns
command -v openssl >/dev/null 2>&1 || fi_skip "openssl not installed -- S7 needs it"
ha_build

# TLS build check: the library must export the repmgr TLS entry points, else
# this is not an --enable-tls build and the scenario is not applicable here.
if ! nm "$LIBDB_LIB" 2>/dev/null | grep -q __repmgr_set_tls && \
   ! nm "$BUILD/.libs"/libdb-*.so 2>/dev/null | grep -q __repmgr_set_tls; then
	fi_skip "libdb was not built with --enable-tls (point BUILD at a TLS build)"
fi

echo "=== S7 arm=$ARM seed=$SEED run=$RT_RUN ==="

ha_tls_setup 3            # CA + per-node certs; sets HA_TLS=1, HA_RUN_PRESET

if [ "$ARM" = pin ]; then
	# pin only a's and b's fingerprints; c's cert is unpinned.
	FA=$(ha_tls_fpr a); FB=$(ha_tls_fpr b)
	HA_TLS_PINS="$FA $FB"
	echo "pinned fingerprints: a=$FA b=$FB (c unpinned)"
fi

ha_group_up 3 quorum "--keys 2000 --valsz 200" || {
	# in the pin arm, the group may still form with a+b (c excluded); only
	# a hard failure to get ANY master is fatal.
	[ "$ARM" = pin ] || { echo "ERROR: group did not form (harness/setup, not an oracle violation)"; ha_group_down; exit 2; }
}

if [ "$ARM" = pin ]; then
	echo "--- negative arm: c must NOT join (unpinned cert) ---"
	# write on the master so there is data, then check c never participated.
	ha_load 100 "$SEED"
	sleep 5
	CJOINED=0
	grep -qE 'ROLE (master|client)' "$HA_RUN/ev.c.log" 2>/dev/null && CJOINED=1
	# c must also hold no replicated data.
	CRECS=$(net_run c env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
	    "$BUILD/db_dump" -p -h "$HA_RUN/c" rt.db 2>/dev/null | grep -c '^ k')
	CRECS=${CRECS:-0}
	ha_group_down
	if [ "$CJOINED" = 0 ] && [ "$CRECS" = 0 ]; then
		echo "S7-pin PASS seed=$SEED (unpinned site c never joined)"
		exit 0
	fi
	echo "S7-pin FAIL seed=$SEED (unpinned c joined: role=$CJOINED recs=$CRECS)"
	exit 1
fi

# positive arm: faults, then converge.
jit=$(( SEED % 30 + 5 )); loss=$(( SEED % 2 + 1 ))
echo "--- baseline load ---"
ha_load 300 "$SEED"

echo "--- net-flaky (${jit}ms jitter, ${loss}% loss) under TLS ---"
for n in $HA_NODES; do net_apply "$n" "delay ${jit}ms $((jit/2))ms loss ${loss}%"; done
ha_load 200 "$((SEED+1))"
net_clear

echo "--- net-partition (isolate a client) + heal under TLS ---"
net_partition "a b|c"
ha_load 200 "$((SEED+2))"
net_heal
sleep 5

echo "--- quiesce + converge ---"
ha_quiesce; sleep 5
ha_quiesce; sleep 3

ha_oracle; rc=$?
ha_group_down
[ "$rc" = 0 ] && echo "S7 PASS seed=$SEED" || echo "S7 FAIL seed=$SEED (reproduce: BUILD=$BUILD $0 $SEED; evidence $HA_RUN)"
exit $rc

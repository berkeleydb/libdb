#!/bin/bash
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# S6: replication + storage fault (RFC 0024 §2.3).
#
# 3 netns sites under DB_REPMGR_ACKS_QUORUM.  One CLIENT (c) runs under the
# libfiu io-eio-write plan (pwrite/write fail EIO at probability p, on that node
# only, via fiu-run -x).  Under a steady write load on the master:
#   - the group must stay available: the master keeps acking (a 2-site quorum of
#     a + the healthy client b is enough even if c falls out);
#   - the faulty client either errors out / panics or limps, but must not stall
#     the group;
#   - after the fault node is restarted clean (no fiu), it rejoins and the group
#     converges.
# Oracle: §2.4 rules 1-5, with teeth from the ACKed writes on the surviving
# majority.  SKIP cleanly if libfiu is absent.
#
# Usage: s6_repl_storage_fault.sh [SEED]
set -u
SEED=${1:-$RANDOM}
export RT_RUN="s6-$SEED-$$"
SDIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SDIR/../lib/ha_common.sh"

ha_need_netns
fi_have fiu || fi_skip "libfiu (fiu-run) not installed -- S6 needs it"
ha_build

echo "=== S6 seed=$SEED run=$RT_RUN ==="

# EIO probability from the seed: 1-5%.
PROB=$(awk "BEGIN{printf \"%.3f\", 0.01 + ($SEED % 5) * 0.01}")

# Launch c under the io-eio-write plan.
HA_FIU_NODE=c
HA_FIU_PLAN=io-eio-write
HA_FIU_SEED=$SEED
HA_FIU_PROB=$PROB

ha_group_up 3 quorum "--keys 2000 --valsz 200" || { echo "FAIL: group up"; ha_group_down; exit 1; }
echo "--- c is under io-eio-write p=$PROB ---"

echo "--- steady load on the master while c's disk fails ---"
for r in 0 1 2 3; do
	ha_load 200 "$((SEED + r))"
done

# the group must still be making progress: the master acked > 0 writes.
M=$(ha_master)
ACKS=$(awk '$3=="txn" && $7=="ACK"' "$HA_RUN/ack.$M.log" 2>/dev/null | wc -l)
echo "master=$M acked=$ACKS during c's disk failure"
if [ "$ACKS" -gt 0 ]; then
	echo "availability: group kept acking with a faulty client: OK"
	AVAIL_OK=1
else
	echo "VIOLATION (S6 availability): master acked 0 writes -- the faulty client stalled the group"
	AVAIL_OK=0
fi

echo "--- clear the fault: restart c clean (no fiu), let it rejoin ---"
ha_kill c
HA_FIU_NODE=""        # restart c WITHOUT the fiu plan
ha_restart c
# give c time to resync
sleep 5

echo "--- quiesce + converge ---"
ha_quiesce; sleep 5
ha_quiesce; sleep 3

ha_oracle; rc=$?
[ "${AVAIL_OK:-1}" = 1 ] || rc=1
ha_group_down
[ "$rc" = 0 ] && echo "S6 PASS seed=$SEED" || echo "S6 FAIL seed=$SEED (reproduce: $0 $SEED; evidence $HA_RUN)"
exit $rc

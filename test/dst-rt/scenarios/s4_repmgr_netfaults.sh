#!/bin/sh
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# S4: repmgr 3-site under network faults (RFC 0024 §2.3).
#
# Topology: 3 netns sites, DB_REPMGR_ACKS_QUORUM, elections on, heartbeats set
# (rtnode defaults).  A seed drives the workload and the fault schedule.  Each
# fault class is exercised in turn against a running, loaded group:
#   net-flaky     jitter + loss + reorder on all links
#   net-asym      a->b drops, b->a clean
#   net-slow-rep  rate-limit one client so it falls behind
#   kill          SIGKILL a client, restart it with DB_RECOVER
# then heal + quiesce and run the oracle (§2.4 rules 1-5).
#
# Usage: s4_repmgr_netfaults.sh [SEED]
set -u
SEED=${1:-$(awk 'BEGIN{srand();print int(rand()*100000)}')}
export RT_RUN="s4-$SEED-$$"
SDIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SDIR/../lib/ha_common.sh"

ha_need_netns
ha_build

echo "=== S4 seed=$SEED run=$RT_RUN ==="
ha_group_up 3 quorum "--keys 2000 --valsz 200" || { echo "ERROR: group did not form (harness/setup, not an oracle violation)"; ha_group_down; exit 2; }

# seeded choice of parameters from the seed
jit=$(( SEED % 40 + 5 ))      # 5-44 ms
loss=$(( SEED % 3 + 1 ))      # 1-3 %
rate="$(( SEED % 3 + 1 ))mbit"

echo "--- baseline load ---"
ha_load 300 "$SEED"

echo "--- net-flaky (${jit}ms jitter, ${loss}% loss, 0.5% reorder, all links) ---"
for n in $HA_NODES; do net_apply "$n" "delay ${jit}ms $((jit/2))ms loss ${loss}% reorder 99.5% 0.5%"; done
ha_load 300 "$((SEED+1))"
net_clear

echo "--- net-asym (a->b drops, b->a clean) ---"
net_partition_asym a b
ha_load 200 "$((SEED+2))"
net_heal

echo "--- net-slow-rep (rate-limit client c to $rate) ---"
net_rate c "$rate"
ha_load 200 "$((SEED+3))"
net_clear

echo "--- kill client c, reload, restart c ---"
# pick a non-master to kill
M=$(ha_master); VICTIM=c; [ "$M" = c ] && VICTIM=b
ha_kill "$VICTIM"
ha_load 200 "$((SEED+4))"
ha_restart "$VICTIM"

echo "--- heal + quiesce + settle ---"
net_heal
ha_quiesce
sleep 5
ha_quiesce         # second drain so the restarted node catches up
sleep 3

ha_oracle; rc=$?
ha_group_down
[ "$rc" = 0 ] && echo "S4 PASS seed=$SEED" || echo "S4 FAIL seed=$SEED (reproduce: $0 $SEED; evidence $HA_RUN)"
exit $rc

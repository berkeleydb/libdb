#!/bin/sh
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# S5: partition the master into the minority, then heal (RFC 0024 §2.3).
#
# 3 netns sites, elections on.  Two arms:
#
#   normal  (DB_REPMGR_ACKS_QUORUM): load on the master, partition the master
#     alone into the minority, keep writing to the isolated old master (its
#     acked writes MUST stop because it cannot reach a quorum), let the majority
#     (2 sites) elect a new master, write there, then heal.  The old master
#     rejoins as a client.  Oracle: no acked write is lost, one master per gen,
#     all sites converge.
#
#   sabotage (DB_REPMGR_ACKS_NONE + partition + kill of the old master): the old
#     master reports commits ACKed without replicating them, so when it is
#     partitioned away and then killed (its home is NOT brought back into the
#     group), those acked writes are lost.  The oracle MUST catch the lost acked
#     writes -- that is the proof the durability check has teeth.
#
# Usage: s5_partition_election.sh [SEED] [--sabotage]
set -u
SEED=${1:-$(awk 'BEGIN{srand();print int(rand()*100000)}')}
ARM=normal
[ "${2:-}" = "--sabotage" ] && ARM=sabotage
export RT_RUN="s5-$ARM-$SEED-$$"
SDIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SDIR/../lib/ha_common.sh"

ha_need_netns
ha_build

echo "=== S5 arm=$ARM seed=$SEED run=$RT_RUN ==="

if [ "$ARM" = sabotage ]; then
	POL=none
else
	POL=quorum
fi
ha_group_up 3 "$POL" "--keys 2000 --valsz 200" || { echo "ERROR: group did not form (harness/setup, not an oracle violation)"; ha_group_down; exit 2; }

OLD=$(ha_master)
echo "--- initial master=$OLD, baseline load ---"
ha_load 300 "$SEED"

echo "--- partition $OLD alone into the minority ---"
# majority = the other two sites; cut the old master off.
MAJ=""
for n in $HA_NODES; do [ "$n" != "$OLD" ] && MAJ="$MAJ $n"; done
MAJ=$(echo $MAJ)
# remember how many ops the old master had acked BEFORE the partition; writes
# made while isolated must NOT count as durable (the minority has no quorum).
ACK_BEFORE=$(wc -l < "$HA_RUN/ack.$OLD.log" 2>/dev/null || echo 0)
PF_BEFORE=$(grep -c perm_failed "$HA_RUN/ev.$OLD.log" 2>/dev/null); PF_BEFORE=${PF_BEFORE:-0}
net_partition "$MAJ|$OLD"

# keep writing to the (now isolated) old master.  Under QUORUM these should
# fail to ack (PERM_FAILED); under ACKS_NONE they "ack" locally (the lie).
echo "--- write to the isolated old master $OLD (expect no quorum acks under QUORUM) ---"
ha_send "$OLD" "load 200 $((SEED+1))"
sleep 6

echo "--- wait for the majority to elect a new master ---"
NEWM=""
deadline=$(( $(date +%s) + 40 ))
while [ "$(date +%s)" -lt "$deadline" ]; do
	for n in $MAJ; do
		# the newest ROLE master line among the majority after the cut
		grep -q 'ROLE master' "$HA_RUN/ev.$n.log" 2>/dev/null && NEWM=$n
	done
	[ -n "$NEWM" ] && [ "$NEWM" != "$OLD" ] && break
	sleep 2
done
echo "new master among majority: ${NEWM:-NONE}"

if [ "$ARM" = sabotage ]; then
	echo "--- SABOTAGE: write more to old master, then KILL it (home not restarted) ---"
	ha_send "$OLD" "load 200 $((SEED+2))"
	sleep 4
	ha_kill "$OLD"
	# write on the new master so the group is live
	[ -n "$NEWM" ] && ha_send "$NEWM" "load 100 $((SEED+3))"
	sleep 4
	net_heal
	# do NOT restart OLD: its home is gone from the group.  The oracle must
	# find OLD's ACKS_NONE "acked" writes missing on the surviving master.
	ha_quiesce; sleep 5
	ha_oracle; rc=$?
	# For the sabotage arm, a PASS is the FAILURE: the oracle should have
	# caught lost acked writes.  Invert the verdict.
	ha_group_down
	if [ "$rc" != 0 ]; then
		echo "S5-sabotage PASS seed=$SEED (oracle correctly caught lost acked writes)"
		exit 0
	else
		echo "S5-sabotage FAIL seed=$SEED (oracle did NOT catch lost acked writes -- no teeth!)"
		exit 1
	fi
fi

# normal arm: write on the new master, heal, old master rejoins as client.
echo "--- write on the new master $NEWM ---"
[ -n "$NEWM" ] && ha_load_on "$NEWM" 200 "$((SEED+2))"

# RFC S5: the minority master must stop acking.  repmgr reports this as
# PERM_FAILED on the isolated old master; rtnode then labels those commits
# UNKNOWN (not ACK), so no isolated-window write is counted durable.  Assert
# BOTH: PERM_FAILED fired, AND none of the isolated-window writes is ACK.
PF_AFTER=$(grep -c perm_failed "$HA_RUN/ev.$OLD.log" 2>/dev/null); PF_AFTER=${PF_AFTER:-0}
# count ACK txn lines the old master added AFTER the pre-partition point.
ACK_AFTER_TOTAL=$(awk '$3=="txn" && $7=="ACK"' "$HA_RUN/ack.$OLD.log" 2>/dev/null | wc -l)
ACK_BASELINE=$(head -n "$ACK_BEFORE" "$HA_RUN/ack.$OLD.log" 2>/dev/null | awk '$3=="txn" && $7=="ACK"' | wc -l)
ISO_ACKS=$(( ACK_AFTER_TOTAL - ACK_BASELINE ))
MINORITY_OK=1
if [ "$PF_AFTER" -gt "$PF_BEFORE" ]; then
	echo "minority check: $OLD reported PERM_FAILED $PF_BEFORE->$PF_AFTER (lost quorum): OK"
else
	echo "VIOLATION (S5 minority): $OLD kept acking while isolated (no PERM_FAILED)"
	MINORITY_OK=0
fi
if [ "$ISO_ACKS" -le 0 ]; then
	echo "minority check: $OLD labelled 0 isolated-window writes ACK (all UNKNOWN): OK"
else
	echo "VIOLATION (S5 minority): $OLD labelled $ISO_ACKS isolated-window writes ACK despite no quorum"
	MINORITY_OK=0
fi

echo "--- heal; old master must step down and rejoin as a client ---"
net_heal
sleep 8
# after heal the old master should no longer think it is master
FINAL=$(ha_master)
echo "final master=$FINAL (was $OLD)"
ha_quiesce; sleep 5
ha_quiesce; sleep 3

ha_oracle; rc=$?
[ "${MINORITY_OK:-1}" = 1 ] || rc=1
ha_group_down
[ "$rc" = 0 ] && echo "S5 PASS seed=$SEED" || echo "S5 FAIL seed=$SEED (reproduce: $0 $SEED; evidence $HA_RUN)"
exit $rc

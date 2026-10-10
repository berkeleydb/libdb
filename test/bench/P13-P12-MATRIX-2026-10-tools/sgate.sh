#!/bin/sh
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
# sgate.sh -- scale_shape_gate.sh for one arm at one scale.
#
# TRAP 1 AGAIN: scale_shape_gate.sh loads its dataset ONCE and reuses it, and
# P12's `locker_shard` is read at region CREATE. So each arm needs its OWN DATA
# directory, created under that arm's environment -- otherwise the P12 arms
# silently measure whatever the first arm created.
#
# Directory names are held at EQUAL LENGTH across arms: run_bench.sh records a
# DB_PRIVATE throughput mode selected by the LENGTH of the environment-home
# path. These are shared environments, where that effect was NOT observed, but
# equal-length names cost nothing and remove the question.
set -u
arm=$1; scale=$2; reps=${3:-9}

case $arm in
base)	ev="DB_NO_LOCKER_SHARD=1 DB_NO_LOCKER_MUTEX_REUSE=1" ;;
p13x)	ev="DB_NO_LOCKER_SHARD=1" ;;
p12x)	ev="DB_NO_LOCKER_MUTEX_REUSE=1" ;;
both)	ev="SG_ARM=both" ;;
*)	echo "unknown arm $arm" >&2; exit 2 ;;
esac

D=/nvme/sg_${scale}_${arm}
mkdir -p "$D"
find "$D" -mindepth 1 -delete 2>/dev/null

cd /nvme/t_P12P13/test/bench || exit 2
# BIN points at the ONE binary built from the P12+P13 tree, so every arm runs
# identical object code and differs only by the runtime switch.
mkdir -p /nvme/sgbin && cp -f /nvme/xe_tproc_c /nvme/sgbin/xe_tproc_c

# shellcheck disable=SC2086
env $ev BUILD=/nvme/t_P12P13/build_unix BIN=/nvme/sgbin DATA="$D" \
    timeout -s KILL 14400 sh ./scale_shape_gate.sh -r "$reps" -S "$scale" \
    -o "$D/shape.tsv" > "/nvme/sgate_${scale}_${arm}.out" 2>&1
rc=$?
echo "arm=$arm scale=$scale rc=$rc"
grep -E '^VERDICT scale-shape|^GATE ERROR|^t=' "/nvme/sgate_${scale}_${arm}.out" | tail -8
exit $rc

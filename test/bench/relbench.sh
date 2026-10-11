#!/bin/sh
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
# See the file LICENSE for redistribution information.
#
# relbench.sh -- v2026.10.5 release benchmark on FreeBSD metal (x86_64 + aarch64).
#
# TWO arms, ONE binary, runtime switches only, so no build skew:
#   new  = the release as shipped (F5 + P13 + P12 all on)
#   old  = all three switched off (DB_LOCK_REFRESH_LOCK_MUTEX, DB_NO_LOCKER_MUTEX_REUSE,
#          DB_NO_LOCKER_SHARD) -- the pre-2026.10.4 code paths on the same binary
# The region LAYOUT is P12's in both arms (it is compile-time), so this measures
# the algorithms, which is what the switches control.
#
# Traps already paid for, all honoured here:
#  * DB_NO_LOCKER_SHARD is read at region CREATE: every arm RECREATES its env.
#  * B3: the loader exits before the measured run attaches, which on FreeBSD
#    panics with a pthread-backed mutex unless kern.ipc.umtx_vnode_persistent=1.
#    Set for the duration; recorded in the TSV header.
#  * db_stat -c is LOCK, -x is MUTEX. Both recorded per rep.
#  * Arms alternate WITHIN each rep; the starting arm rotates per rep.
#  * Rows stream to the TSV as they complete.
set -u
BIN=${BIN:-/nvme/xe_tproc_c}
BUILD=${BUILD:-/nvme/bb}
DATA=${DATA:-/nvme/rbdata}
SCALE=${SCALE:-96}; PAD=${PAD:-256}; CACHE=${CACHE:-4294967296}
SECS=${SECS:-20}; WARM=${WARM:-8}; REPS=${REPS:-7}
THREADS=${THREADS:-"1 2 4 8 16 32 64"}
OUT=${OUT:-/nvme/relbench.tsv}
DBSTAT=$BUILD/db_stat

arm_env() { case $1 in old) echo "DB_LOCK_REFRESH_LOCK_MUTEX=1 DB_NO_LOCKER_MUTEX_REUSE=1 DB_NO_LOCKER_SHARD=1";; *) echo "RB_ARM=new";; esac; }
stat_of() { awk -v pat="$2" '$0 ~ pat { if (match($0, /\(([0-9]+)\)/)) { print substr($0, RSTART+1, RLENGTH-2); exit } print $1; exit }' "$1"; }

sudo sysctl -q kern.ipc.umtx_vnode_persistent=1 >/dev/null
if [ ! -f "$OUT" ]; then
	{
	printf '# libdb-relbench\tv1\n# generated\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
	printf '# host\t%s %s ncpu=%s\n' "$(uname -sr)" "$(uname -m)" "$(sysctl -n hw.ncpu)"
	printf '# cpu\t%s\n' "$(sysctl -n hw.model)"
	printf '# release\t%s\n' "$($DBSTAT -V)"
	printf '# umtx_vnode_persistent\t%s\n' "$(sysctl -n kern.ipc.umtx_vnode_persistent)"
	printf '# scale\t%s\n# pad\t%s\n# cache\t%s\n# secs\t%s\n# warmup\t%s\n# reps\t%s\n' "$SCALE" "$PAD" "$CACHE" "$SECS" "$WARM" "$REPS"
	printf 'arm\tthreads\trep\ttpm\tcommits\tc_region\tc_locker\tc_part\tc_objq\tc_deadlock\tx_region\n'
	} > "$OUT"
fi

one_run() { # arm threads rep record
	arm=$1; t=$2; rep=$3; rec=$4; ev=$(arm_env "$arm")
	[ -d "$DATA" ] && find "$DATA" -mindepth 1 -delete 2>/dev/null; mkdir -p "$DATA"
	env $ev timeout 900 "$BIN" -i -e libdb -a btree -h "$DATA" -S "$SCALE" -P "$PAD" -c "$CACHE" >/tmp/.rbload 2>&1 || {
		[ "$rec" = 1 ] && printf '%s\t%s\t%s\tLOAD_FAILED\t0\t0\t0\t0\t0\t0\t0\n' "$arm" "$t" "$rep" >> "$OUT"; return 0; }
	env $ev "$DBSTAT" -c -Z -h "$DATA" >/dev/null 2>&1; env $ev "$DBSTAT" -x -Z -h "$DATA" >/dev/null 2>&1
	rc=0
	env $ev timeout $((SECS + WARM + 600)) "$BIN" -e libdb -a btree -h "$DATA" -S "$SCALE" -P "$PAD" -c "$CACHE" \
	    -t "$t" -s "$SECS" -W "$WARM" >/tmp/.rbrun 2>&1 || rc=$?
	line=$(grep -m1 '^VERDICT tproc-c' /tmp/.rbrun)
	[ "$rec" = 1 ] || return 0
	[ "$rc" = 124 ] && { printf '%s\t%s\t%s\tTIMEOUT_STALL\t0\t0\t0\t0\t0\t0\t0\n' "$arm" "$t" "$rep" >> "$OUT"; return 0; }
	[ -z "$line" ] && { printf '%s\t%s\t%s\tNO_VERDICT_rc%s\t0\t0\t0\t0\t0\t0\t0\n' "$arm" "$t" "$rep" "$rc" >> "$OUT"; return 0; }
	tpm=$(printf '%s\n' "$line" | sed -n 's/.*tpmC_like=\([0-9.]*\).*/\1/p')
	com=$(printf '%s\n' "$line" | sed -n 's/.*committed=\([0-9]*\).*/\1/p')
	env $ev "$DBSTAT" -c -h "$DATA" >/tmp/.rbc 2>&1; env $ev "$DBSTAT" -x -h "$DATA" >/tmp/.rbx 2>&1
	printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$arm" "$t" "$rep" "$tpm" "${com:-0}" \
	    "$(stat_of /tmp/.rbc 'number of region locks that required waiting')" \
	    "$(stat_of /tmp/.rbc 'number of locker allocations that required waiting')" \
	    "$(stat_of /tmp/.rbc 'number of partition locks that required waiting')" \
	    "$(stat_of /tmp/.rbc 'number of object queue operations that required waiting')" \
	    "$(stat_of /tmp/.rbc 'Number of deadlocks')" \
	    "$(stat_of /tmp/.rbx 'number of region locks that required waiting')" >> "$OUT"
	echo "  $arm t=$t rep=$rep tpm=$tpm"
}

for t in $THREADS; do one_run new $t 0 0; one_run old $t 0 0; done   # prewarm, discarded
r=1
while [ $r -le $REPS ]; do
	for t in $THREADS; do
		if [ $((r % 2)) -eq 1 ]; then one_run new $t $r 1; one_run old $t $r 1
		else one_run old $t $r 1; one_run new $t $r 1; fi
	done
	r=$((r + 1))
done
sudo sysctl -q kern.ipc.umtx_vnode_persistent=0 >/dev/null
echo "RELBENCH DONE"

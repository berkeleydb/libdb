#!/bin/sh
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
# See the file LICENSE for redistribution information.
#
# spinbench.sh -- the mutex-spin / mtx_regenv benchmark for FreeBSD metal
# (x86_64 c6id.metal, aarch64 c7gd.metal).  Derived from relbench.sh; every
# trap it records is still honoured here.
#
# ONE binary per architecture; every arm is a RUNTIME switch -- an env var or
# a DB_CONFIG line -- so the comparison is the algorithm, never build skew:
#
#   base      the branch as built, defaults (DB_MUTEX_BACKOFF is off by
#             default, so base IS the historical spin loop)
#   bo16      DB_MUTEX_BACKOFF=16            bounded exponential backoff
#   bo64      DB_MUTEX_BACKOFF=64
#   align64   DB_CONFIG mutex_set_align 64   one mutex per cache line pair
#                                           (RFC 0015 E6/S6a; no signature
#                                           change, attach-safe)
#   spin200   DB_CONFIG mutex_set_tas_spins 200   (default is 50 x ncpu)
#   spin800   DB_CONFIG mutex_set_tas_spins 800
#   s5        DB_TXN_FREELIST=1             TXN_DETAIL region free list (RFC 0015 S5)
#   m1        DB_TXN_LATCH_SPLIT=1          split the txn role off mtx_regenv (M1)
#   s5m1      DB_TXN_FREELIST=1 DB_TXN_LATCH_SPLIT=1   S5+M1 together (RFC order)
# plus any arms named in EXTRA_ARMS as  name=ENV1=v,ENV2=v  (comma-separated
# env assignments), e.g.:
#   EXTRA_ARMS="m1spin=DB_TXN_LATCH_SPLIT=1,DB_MUTEX_BACKOFF=16"
#
# NOTE on reading the M1/s5m1 arms: mtx_regenv is the lock/txn/log latch, so
# -c/-t/-l "region lock" are ONE counter in every non-split arm.  With m1 or
# s5m1 the TXN role moves to its own mutex, so t_region DIVERGES from c_region
# and l_region -- that divergence IS the proof the split engaged.  Per RFC 0015
# M1, judge M1 by THROUGHPUT and shape, not by c_region (which barely falls:
# ~80% of its arrivals are log appends that stay on mtx_regenv).  The telling
# arms are {base, s5, m1, s5m1}; a win needs s5m1 to beat base at the t where
# the metal shape peaks.
#
# Choose with ARMS="base bo16 align64 ..." (default: all of the above).
#
# Traps honoured (each was paid for once):
#  * Every arm RECREATES its env (P12's switch is read at region create; a
#    DB_CONFIG knob like mutex_set_align is too).
#  * B3: the loader exits before the measured run attaches; on FreeBSD a
#    pthread-backed mutex needs kern.ipc.umtx_vnode_persistent=1 unless the
#    build is hybrid on this architecture.  Set for the run, recorded, and
#    restored to its previous value at exit (not forced to 0).
#  * db_stat -c is LOCK, -x is MUTEX, -t TXN, -l LOG.  -c/-t/-l "region lock"
#    lines are ONE mutex (mtx_regenv) unless a split arm is on; all four are
#    recorded per rep so a split is visible as -t and -l diverging.
#  * Arms alternate WITHIN each rep; the starting arm rotates per rep.
#  * A discarded prewarm precedes the measured reps.
#  * Rows stream to the TSV as they complete; raw db_stat output is kept per
#    rep under $RAW so nothing has to be re-run to answer a later question.
#
# Usage (as the lead, on the metal box, after building):
#   BUILD=/nvme/sb/build_unix BIN=/nvme/sb/xe_tproc_c sh spinbench.sh
# The script refuses to run if the build is not hybrid (it would measure
# pthread mutexes and say nothing about the spin loop).
set -u
BIN=${BIN:-/nvme/xe_tproc_c}
BUILD=${BUILD:-/nvme/bb}
DATA=${DATA:-/nvme/sbdata}
SCALE=${SCALE:-96}; PAD=${PAD:-256}; CACHE=${CACHE:-4294967296}
SECS=${SECS:-20}; WARM=${WARM:-8}; REPS=${REPS:-7}
THREADS=${THREADS:-"1 2 4 8 16 32 64 128"}
ARMS=${ARMS:-"base bo16 bo64 align64 spin200 spin800 s5 m1 s5m1"}
EXTRA_ARMS=${EXTRA_ARMS:-}
OUT=${OUT:-/nvme/spinbench.tsv}
RAW=${RAW:-/nvme/spinbench.raw}
DBSTAT=$BUILD/db_stat

[ -x "$DBSTAT" ] || { echo "no db_stat under $BUILD" >&2; exit 2; }
[ -x "$BIN" ] || { echo "no driver $BIN" >&2; exit 2; }
grep -q '^#define HAVE_MUTEX_HYBRID 1' "$BUILD/db_config.h" || {
	echo "REFUSING: $BUILD is not a hybrid-mutex build; this benchmark" \
	    "measures the TAS spin path." >&2
	exit 2
}
mkdir -p "$RAW" || exit 2

for a in $EXTRA_ARMS; do ARMS="$ARMS ${a%%=*}"; done

# arm -> env assignments (space-separated) for `env'
arm_env() {
	case $1 in
	base|align64|spin200|spin800) echo "SB_ARM=$1" ;;
	bo16)	echo "DB_MUTEX_BACKOFF=16" ;;
	bo64)	echo "DB_MUTEX_BACKOFF=64" ;;
	s5)	echo "DB_TXN_FREELIST=1" ;;
	m1)	echo "DB_TXN_LATCH_SPLIT=1" ;;
	s5m1)	echo "DB_TXN_FREELIST=1 DB_TXN_LATCH_SPLIT=1" ;;
	*)	for a in $EXTRA_ARMS; do
			[ "${a%%=*}" = "$1" ] && { echo "${a#*=}" | tr ',' ' '; return; }
		done
		echo "UNKNOWN_ARM=$1" ;;
	esac
}
# arm -> DB_CONFIG body
arm_dbconfig() {
	case $1 in
	align64) echo "mutex_set_align 64" ;;
	spin200) echo "mutex_set_tas_spins 200" ;;
	spin800) echo "mutex_set_tas_spins 800" ;;
	esac
}
stat_of() { awk -v pat="$2" '$0 ~ pat { if (match($0, /\(([0-9]+)\)/)) { print substr($0, RSTART+1, RLENGTH-2); exit } print $1; exit }' "$1"; }

OLD_PERSIST=$(sysctl -n kern.ipc.umtx_vnode_persistent)
sudo sysctl -q kern.ipc.umtx_vnode_persistent=1 >/dev/null
trap 'sudo sysctl -q kern.ipc.umtx_vnode_persistent=$OLD_PERSIST >/dev/null' 0 1 2 15

if [ ! -f "$OUT" ]; then
	{
	printf '# libdb-spinbench\tv1\n# generated\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
	printf '# host\t%s %s ncpu=%s\n' "$(uname -sr)" "$(uname -m)" "$(sysctl -n hw.ncpu)"
	printf '# cpu\t%s\n' "$(sysctl -n hw.model)"
	printf '# release\t%s\n' "$($DBSTAT -V)"
	printf '# mutex\t%s\n' "$(grep -E '^db_cv_mutex=' "$BUILD/config.log" 2>/dev/null | cut -d= -f2)"
	printf '# umtx_vnode_persistent\t1 (was %s)\n' "$OLD_PERSIST"
	printf '# arms\t%s\n' "$ARMS"
	for a in $ARMS; do printf '# arm %s\tenv=%s\tdbconfig=%s\n' "$a" "$(arm_env $a)" "$(arm_dbconfig $a)"; done
	printf '# scale\t%s\n# pad\t%s\n# cache\t%s\n# secs\t%s\n# warmup\t%s\n# reps\t%s\n' "$SCALE" "$PAD" "$CACHE" "$SECS" "$WARM" "$REPS"
	printf 'arm\tthreads\trep\ttpm\tcommits\tc_region\tt_region\tl_region\tc_locker\tc_part\tc_objq\tc_deadlock\tx_region\ttas_spins\tmutex_align\n'
	} > "$OUT"
fi

one_run() { # arm threads rep record
	arm=$1; t=$2; rep=$3; rec=$4; ev=$(arm_env "$arm")
	[ -d "$DATA" ] && find "$DATA" -mindepth 1 -delete 2>/dev/null; mkdir -p "$DATA"
	dbc=$(arm_dbconfig "$arm"); [ -n "$dbc" ] && echo "$dbc" > "$DATA/DB_CONFIG"
	env $ev timeout 900 "$BIN" -i -e libdb -a btree -h "$DATA" -S "$SCALE" -P "$PAD" -c "$CACHE" >/tmp/.sbload 2>&1 || {
		[ "$rec" = 1 ] && printf '%s\t%s\t%s\tLOAD_FAILED\n' "$arm" "$t" "$rep" >> "$OUT"; return 0; }
	for f in c t l x; do env $ev "$DBSTAT" -$f -Z -h "$DATA" >/dev/null 2>&1; done
	rc=0
	env $ev timeout $((SECS + WARM + 600)) "$BIN" -e libdb -a btree -h "$DATA" -S "$SCALE" -P "$PAD" -c "$CACHE" \
	    -t "$t" -s "$SECS" -W "$WARM" >/tmp/.sbrun 2>&1 || rc=$?
	line=$(grep -m1 '^VERDICT tproc-c' /tmp/.sbrun)
	[ "$rec" = 1 ] || return 0
	[ "$rc" = 124 ] && { printf '%s\t%s\t%s\tTIMEOUT_STALL\n' "$arm" "$t" "$rep" >> "$OUT"; return 0; }
	[ -z "$line" ] && { printf '%s\t%s\t%s\tNO_VERDICT_rc%s\n' "$arm" "$t" "$rep" "$rc" >> "$OUT"; return 0; }
	tpm=$(printf '%s\n' "$line" | sed -n 's/.*tpmC_like=\([0-9.]*\).*/\1/p')
	com=$(printf '%s\n' "$line" | sed -n 's/.*committed=\([0-9]*\).*/\1/p')
	# NOT `r': that is the caller's rep counter (sh has no locals).
	rp=$RAW/${arm}_t${t}_r${rep}
	for f in c t l x; do env $ev "$DBSTAT" -$f -h "$DATA" > $rp.$f 2>&1; done
	cp /tmp/.sbrun $rp.run
	printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$arm" "$t" "$rep" "$tpm" "${com:-0}" \
	    "$(stat_of $rp.c 'number of region locks that required waiting')" \
	    "$(stat_of $rp.t 'number of region locks that required waiting')" \
	    "$(stat_of $rp.l 'number of region locks that required waiting')" \
	    "$(stat_of $rp.c 'number of locker allocations that required waiting')" \
	    "$(stat_of $rp.c 'number of partition locks that required waiting')" \
	    "$(stat_of $rp.c 'number of object queue operations that required waiting')" \
	    "$(stat_of $rp.c 'Number of deadlocks')" \
	    "$(stat_of $rp.x 'number of region locks that required waiting')" \
	    "$(stat_of $rp.x 'Mutex test-and-set spins')" \
	    "$(stat_of $rp.x 'Mutex alignment')" >> "$OUT"
	echo "  $arm t=$t rep=$rep tpm=$tpm"
}

for t in $THREADS; do for a in $ARMS; do one_run $a $t 0 0; done; done   # prewarm, discarded
r=1
while [ $r -le $REPS ]; do
	n=$(echo $ARMS | wc -w | tr -d ' ')
	k=$(( (r - 1) % n ))
	# rotate the arm list by k for this rep
	i=0; rot=""; tail=""
	for a in $ARMS; do
		if [ $i -lt $k ]; then tail="$tail $a"; else rot="$rot $a"; fi
		i=$((i + 1))
	done
	for t in $THREADS; do
		for a in $rot $tail; do one_run $a $t $r 1; done
	done
	r=$((r + 1))
done
echo "SPINBENCH DONE"

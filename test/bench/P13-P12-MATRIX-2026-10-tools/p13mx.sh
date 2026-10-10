#!/bin/sh
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# p13mx.sh -- the P13/P12 FOUR-ARM matrix, 64 vCPU Linux.
#
# ONE binary (the P13+P12 build, which carries both runtime switches), so no
# build skew can enter the comparison. Arms selected by env var only.
#
# TRAP 1, ALREADY PAID FOR: P12's `locker_shard` is read by __lock_region_init
# when the REGION IS CREATED and stored in the region, so attachers inherit the
# creator's choice (dbinc/lock.h). Setting DB_NO_LOCKER_SHARD on a run that
# ATTACHES to an existing region is SILENTLY IGNORED. A previous pass reused one
# dataset across arms and fabricated a 13x regression from exactly this. So
# EVERY ARM RECREATES ITS ENVIRONMENT, and the load runs under the same env as
# the measured run.
#
# TRAP 2: a loop-ordering bug once compiled all four arms as baseline. Not
# applicable here (one binary), but the four per-arm trees' lock_id.o sizes and
# md5s were verified distinct before this ran.
#
# Arms alternate WITHIN each rep, and the STARTING ARM ROTATES per rep so no arm
# permanently owns the position right after a dataset load.
#
# Per-commit latch waits for EVERY latch in BOTH subsystems: `db_stat -c` is
# LOCK, `-x` is MUTEX. P12's regression was INVISIBLE in -c (every -c latch
# improved while throughput fell 18%); the cost was in -x. Not optional.
#
# Rows STREAM to the TSV: a sibling agent buffered reps remotely and lost them
# with the instance.
set -u

BIN=${BIN:-/nvme/xe_tproc_c}
BUILD=${BUILD:-/nvme/t_P12P13/build_unix}
DATA=${DATA:-/nvme/mxdata/d}
SCALE=${SCALE:-96}
PAD=${PAD:-256}
CACHE=${CACHE:-4294967296}
SECS=${SECS:-20}
WARM=${WARM:-8}
REPS=${REPS:-9}
THREADS=${THREADS:-"1 2 8 16 32 64"}
OUT=${OUT:-/nvme/p13mx.tsv}

DBSTAT=$BUILD/db_stat
[ -x "$DBSTAT" ] || { echo "no db_stat under $BUILD" >&2; exit 2; }
[ -x "$BIN" ] || { echo "no driver $BIN" >&2; exit 2; }
mkdir -p "$DATA" || exit 2

# arm : the environment that SELECTS it. Sense is "off switch" throughout, so
# baseline is both switches off and P13P12 is neither set.
arm_env() {
	case $1 in
	base)	echo "DB_NO_LOCKER_SHARD=1 DB_NO_LOCKER_MUTEX_REUSE=1" ;;
	P13)	echo "DB_NO_LOCKER_SHARD=1" ;;
	P12)	echo "DB_NO_LOCKER_MUTEX_REUSE=1" ;;
	P13P12)	echo "MX_ARM=P13P12" ;;
	esac
}
ARMS="base P13 P12 P13P12"

# Pull one named counter out of a db_stat report. db_stat prints humanised
# values ("70M") in the FIRST column for big counters, with the exact value in
# parentheses -- prefer the parenthesised exact value when present.
stat_of() {
	awk -v pat="$2" '
	  $0 ~ pat {
		if (match($0, /\(([0-9]+)\)/)) {
			print substr($0, RSTART+1, RLENGTH-2); exit
		}
		print $1; exit
	  }' "$1"
}

if [ ! -f "$OUT" ]; then
	{
	printf '# libdb-p13-p12-matrix\tv1\n'
	printf '# generated\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
	printf '# vcpus\t%s\n' "$(nproc)"
	printf '# cpu_model\t%s\n' "$(sed -n 's/^model name[[:space:]]*:[[:space:]]*//p' /proc/cpuinfo | head -1)"
	printf '# kernel\t%s\n' "$(uname -sr)"
	printf '# scale\t%s\n# pad\t%s\n# cache\t%s\n' "$SCALE" "$PAD" "$CACHE"
	printf '# secs\t%s\n# warmup\t%s\n# reps\t%s\n' "$SECS" "$WARM" "$REPS"
	printf 'arm\tthreads\trep\ttpm\tcommits\t'
	printf 'c_region\tc_locker\tc_part\tc_partmax\tc_objq\tc_confl_w\tc_confl_nw\tc_deadlock\t'
	printf 'x_region\tx_inuse\tx_maxinuse\n'
	} > "$OUT"
fi

# one_run ARM THREADS REP RECORD
one_run() {
	arm=$1; t=$2; rep=$3; record=$4
	ev=$(arm_env "$arm")

	# EVERY ARM RECREATES THE ENVIRONMENT -- trap 1.
	find "$DATA" -mindepth 1 -delete 2>/dev/null
	# shellcheck disable=SC2086
	if ! env $ev timeout 600 "$BIN" -i -e libdb -a btree -h "$DATA" \
	    -S "$SCALE" -P "$PAD" -c "$CACHE" >/tmp/.mxload 2>&1; then
		echo "### LOAD FAILED arm=$arm t=$t rep=$rep"
		[ "$record" = 1 ] && printf '%s\t%s\t%s\tLOAD_FAILED\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\n' \
		    "$arm" "$t" "$rep" >> "$OUT"
		return 0
	fi

	# Clear BOTH subsystems immediately before the run. -c is LOCK, -x is
	# MUTEX, -l is LOG (reading -l for lock statistics already produced one
	# wrong published conclusion, so it is not touched here).
	# shellcheck disable=SC2086
	env $ev "$DBSTAT" -c -Z -h "$DATA" >/dev/null 2>&1
	# shellcheck disable=SC2086
	env $ev "$DBSTAT" -x -Z -h "$DATA" >/dev/null 2>&1

	rc=0
	# shellcheck disable=SC2086
	env $ev timeout -s KILL $((SECS + WARM + 600)) "$BIN" -e libdb -a btree \
	    -h "$DATA" -S "$SCALE" -P "$PAD" -c "$CACHE" \
	    -t "$t" -s "$SECS" -W "$WARM" >/tmp/.mxrun 2>&1 || rc=$?
	line=$(grep -m1 '^VERDICT tproc-c' /tmp/.mxrun)

	if [ "$record" != 1 ]; then
		if [ -z "$line" ]; then
			echo "### PREWARM arm=$arm t=$t produced NO VERDICT (rc=$rc)"
			prewarm_bad=1
		else
			echo "    prewarm $arm t=$t: $(printf '%s\n' "$line" | sed -n 's/.*tpmC_like=\([0-9.]*\).*/\1/p') tpm (DISCARDED)"
		fi
		return 0
	fi

	if [ "$rc" = 124 ] || [ "$rc" = 137 ]; then
		printf '%s\t%s\t%s\tTIMEOUT_STALL\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\n' \
		    "$arm" "$t" "$rep" >> "$OUT"
		echo "### STALL arm=$arm t=$t rep=$rep"
		return 0
	fi
	if [ -z "$line" ]; then
		printf '%s\t%s\t%s\tNO_VERDICT\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\n' \
		    "$arm" "$t" "$rep" >> "$OUT"
		echo "### NO VERDICT arm=$arm t=$t rep=$rep rc=$rc"
		grep -iE '^FAIL|error' /tmp/.mxrun | head -3 | sed 's/^/###   /'
		return 0
	fi

	tpm=$(printf '%s\n' "$line" | sed -n 's/.*tpmC_like=\([0-9.]*\).*/\1/p')
	cm=$(printf '%s\n' "$line" | sed -n 's/.*committed=\([0-9]*\).*/\1/p')

	# shellcheck disable=SC2086
	env $ev "$DBSTAT" -c -h "$DATA" >/tmp/.mxsc 2>/dev/null
	# shellcheck disable=SC2086
	env $ev "$DBSTAT" -x -h "$DATA" >/tmp/.mxsx 2>/dev/null

	cr=$(stat_of /tmp/.mxsc 'number of region locks that required waiting')
	cl=$(stat_of /tmp/.mxsc 'number of locker allocations that required waiting')
	cp=$(stat_of /tmp/.mxsc 'number of partition locks that required waiting')
	cpm=$(stat_of /tmp/.mxsc 'maximum number of times any partition lock was waited for')
	co=$(stat_of /tmp/.mxsc 'number of object queue operations that required waiting')
	cw=$(stat_of /tmp/.mxsc 'conflicts, for which we waited')
	cn=$(stat_of /tmp/.mxsc 'conflicts, for which we did not wait')
	cd=$(stat_of /tmp/.mxsc 'Number of deadlocks')
	xr=$(stat_of /tmp/.mxsx 'number of region locks that required waiting')
	xi=$(stat_of /tmp/.mxsx 'Mutex in-use count')
	xm=$(stat_of /tmp/.mxsx 'Mutex maximum in-use count')

	printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
	    "$arm" "$t" "$rep" "$tpm" "${cm:-0}" \
	    "${cr:-0}" "${cl:-0}" "${cp:-0}" "${cpm:-0}" "${co:-0}" \
	    "${cw:-0}" "${cn:-0}" "${cd:-0}" \
	    "${xr:-0}" "${xi:-0}" "${xm:-0}" >> "$OUT"
	echo "rep=$rep t=$t $arm: $tpm tpm (commits=$cm c_lk=$cl x_rg=$xr)"
}

# ---- the DISCARDED prewarm pass -------------------------------------------
# TPROC-C MUTATES its dataset, and the first pass after a load runs against a
# smaller, tidier tree. Without this the scale gate measured a monotone 34%
# decline across reps (cv 16.6%) and correctly refused a verdict.
prewarm_bad=0
echo "=== prewarm pass (DISCARDED)"
for t in $THREADS; do
	for a in $ARMS; do
		one_run "$a" "$t" pw 0
	done
done
if [ "$prewarm_bad" = 1 ]; then
	echo "### prewarm did not complete; measured reps would carry fresh-dataset drift. REFUSING."
	exit 2
fi
echo

# ---- measured reps: arms alternate within each rep, start arm rotates ------
rep=1
while [ "$rep" -le "$REPS" ]; do
	for t in $THREADS; do
		# rotate the arm order by (rep-1) so no arm permanently owns the
		# slot immediately after a dataset load.
		k=$(( (rep - 1) % 4 ))
		order=$(echo $ARMS | tr ' ' '\n' | awk -v k="$k" '
		    {a[NR]=$0} END{for(i=0;i<NR;i++) print a[((i+k)%NR)+1]}')
		for a in $order; do
			one_run "$a" "$t" "$rep" 1
		done
	done
	echo "--- rep $rep done $(date -u +%H:%M:%S)"
	rep=$((rep + 1))
done
echo "wrote $OUT"

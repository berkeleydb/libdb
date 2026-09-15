#!/bin/sh
# false_abort_sweep.sh -- drive ssi_abort_bench to quantify libdb's SSI
# FALSE-ABORT rate: what fraction of DB_TXN_SERIALIZABLE aborts are artifacts of
# tracking rw-antidependencies at PAGE rather than key granularity.
#
#   BENCH=/path/to/ssi_abort_bench OUT=out.csv ./false_abort_sweep.sh [reps]
#
# DESIGN: hold the LOGICAL conflict structure constant, vary only the PHYSICAL
# co-location of keys.
#
#   ring        SSI_READ_OFF=0 -- the shipped write-skew ring.  Its
#               rw-antidependency cycle is REAL, so its aborts are
#               genuine+artifact mixed and it cannot attribute them by itself.
#               Reported as the reference.
#   ring-decoy  SSI_READ_OFF>0 -- each worker reads a key NOBODY writes.  The
#               logical conflict graph is EMPTY: a key-granularity SSI would
#               abort ZERO transactions, so every abort here is 100% artifact.
#               THIS is the measurement.
#   rmw         realistic read-modify-write, uniform and Zipfian, swept over
#               page size: what SSI costs on a workload that is not a ring.
#
# TWO TRAPS THIS SCRIPT AVOIDS, both confirmed empirically:
#  1. OVERFLOW.  A value above ~pagesize/4 goes to an overflow page; the leaf
#     then holds only pointers, records-per-leaf JUMPS, and page-level ww
#     conflicts appear as deadlocks (measured: pagesize 512 + valsz 200 ->
#     rpl 13.98, deadlock 2414; valsz 64 -> rpl 4.00, deadlock 0).  So valsz is
#     PINNED at 64, which is inline at every page size >= 512, and the page-size
#     sweep is then monotonic in records-per-leaf with deadlock == 0 throughout.
#  2. STALE ENVIRONMENT.  A reused/recovered env has previously produced a
#     phantom ssi_abort > 0.  Every single run gets a FRESH unique directory,
#     removed afterwards even on failure.
# Every invocation is under `timeout` so a hang costs one point, not the sweep.
set -u

BENCH=${BENCH:-./ssi_abort_bench}
OUT=${OUT:-false-abort.csv}
SECS=${SECS:-5}
THREADS=${THREADS:-8}
REPS=${1:-5}
RUNDIR=${RUNDIR:-/tmp/fa_sweep_$$}
TMO=${TMO:-120}

# valsz PINNED inline (see trap 1).  spread 1024 > 2x the largest measured
# records-per-leaf (341 at pagesize 32768), so no two workers' WRITE keys ever
# share a leaf at any point in the sweep and the only thing varying is the READ
# key's distance / the page's capacity.
VALSZ=${VALSZ:-64}
SPREAD=${SPREAD:-1024}
HOT=${HOT:-8192}		# THREADS(8) * SPREAD(1024)
PAGESIZES=${PAGESIZES:-"512 1024 4096 8192 16384 32768"}
OFFSETS=${OFFSETS:-"1 2 4 8 16 32 64 128 256 512"}
RMW_HOT=${RMW_HOT:-20000}

mkdir -p "$RUNDIR" || exit 1

# one <tag> <rep> <hotkeys> <VAR=val ...> -- run one point, append its CSV row.
one() {
	tag=$1; rep=$2; hot=$3; shift 3
	d="$RUNDIR/e_${tag}_${rep}"
	mkdir -p "$d" || return 1
	out=$(env SSI_CSV=1 SSI_ENV="$d" SSI_VALSZ="$VALSZ" "$@" \
	    timeout "$TMO" "$BENCH" "$hot" "$SECS" "$THREADS" 2>"$d/err")
	rc=$?
	err=$(tail -2 "$d/err" 2>/dev/null | tr '\n' ' ')
	find "$d" -mindepth 1 -delete 2>/dev/null
	rmdir "$d" 2>/dev/null
	if [ $rc -ne 0 ]; then
		echo "WARN $tag rep=$rep rc=$rc $err" >&2
		echo "$tag,$rep,FAILED,rc=$rc" >>"$OUT"
		return 1
	fi
	echo "$out" | sed -n "s/^CSV,r/$tag,$rep,r/p" >>"$OUT"
	return 0
}

# Header once (a 1-second throwaway point).
d="$RUNDIR/e_hdr"; mkdir -p "$d"
env SSI_CSV=1 SSI_ENV="$d" timeout "$TMO" "$BENCH" 64 1 2 2>/dev/null |
    sed -n 's/^CSV,workload,/tag,rep,workload,/p' | head -1 >"$OUT"
find "$d" -mindepth 1 -delete 2>/dev/null; rmdir "$d" 2>/dev/null

rep=1
while [ "$rep" -le "$REPS" ]; do
	# A/B ALTERNATE: genuine ring and decoy ring at the SAME page size are
	# run back to back, so any machine drift hits both arms of the
	# comparison equally.
	for ps in $PAGESIZES; do
		one "ring-ps$ps" "$rep" "$HOT" SSI_PAGESIZE="$ps" \
		    SSI_SPREAD="$SPREAD" ISO_LEVEL=serializable
		one "decoy-ps$ps" "$rep" "$HOT" SSI_PAGESIZE="$ps" \
		    SSI_SPREAD="$SPREAD" SSI_READ_OFF=1 ISO_LEVEL=serializable
	done
	# The shipped default configuration (pagesize 1024, valsz 200 ->
	# records-per-leaf 3.00 measured), for continuity with the existing
	# bench numbers.  Uses SSI_VALSZ=200 deliberately: still inline at 1024.
	one "ring-default" "$rep" "$HOT" SSI_PAGESIZE=1024 SSI_VALSZ=200 \
	    SSI_SPREAD="$SPREAD" ISO_LEVEL=serializable
	one "decoy-default" "$rep" "$HOT" SSI_PAGESIZE=1024 SSI_VALSZ=200 \
	    SSI_SPREAD="$SPREAD" SSI_READ_OFF=1 ISO_LEVEL=serializable
	# Read-offset sweep: at what KEY distance does false sharing die?
	# Prediction: aborts persist while off < records-per-leaf, then
	# collapse.  Run at two page sizes with very different capacities.
	for ps in 4096 32768; do
		for off in $OFFSETS; do
			one "decoy-ps${ps}-off$off" "$rep" "$HOT" \
			    SSI_PAGESIZE="$ps" SSI_SPREAD="$SPREAD" \
			    SSI_READ_OFF="$off" ISO_LEVEL=serializable
		done
	done
	# SI controls.  Snapshot must show ssi_abort == 0 everywhere; a nonzero
	# value would mean the counter, not the mechanism, is being measured.
	one "decoy-si-ps32768" "$rep" "$HOT" SSI_PAGESIZE=32768 \
	    SSI_SPREAD="$SPREAD" SSI_READ_OFF=1 ISO_LEVEL=snapshot
	one "ring-si-ps32768" "$rep" "$HOT" SSI_PAGESIZE=32768 \
	    SSI_SPREAD="$SPREAD" ISO_LEVEL=snapshot
	one "decoy-si-ps512" "$rep" "$HOT" SSI_PAGESIZE=512 \
	    SSI_SPREAD="$SPREAD" SSI_READ_OFF=1 ISO_LEVEL=snapshot
	# Realistic read-modify-write over page size, uniform + Zipfian, with
	# SI controls so the SSI premium is separable from ordinary write-write
	# loss (first-updater-wins / page ww) that SI pays too.
	for ps in 512 1024 4096 16384 32768; do
		one "rmw-uni-ps$ps" "$rep" "$RMW_HOT" SSI_PAGESIZE="$ps" \
		    SSI_WORKLOAD=rmw SSI_READS=4 ISO_LEVEL=serializable
		one "rmw-uni-si-ps$ps" "$rep" "$RMW_HOT" SSI_PAGESIZE="$ps" \
		    SSI_WORKLOAD=rmw SSI_READS=4 ISO_LEVEL=snapshot
		one "rmw-zipf-ps$ps" "$rep" "$RMW_HOT" SSI_PAGESIZE="$ps" \
		    SSI_WORKLOAD=rmw SSI_READS=4 SSI_ZIPF=1 \
		    ISO_LEVEL=serializable
		one "rmw-zipf-si-ps$ps" "$rep" "$RMW_HOT" SSI_PAGESIZE="$ps" \
		    SSI_WORKLOAD=rmw SSI_READS=4 SSI_ZIPF=1 \
		    ISO_LEVEL=snapshot
	done
	echo "rep $rep done: $(wc -l <"$OUT") rows" >&2
	rep=$((rep + 1))
done

rmdir "$RUNDIR" 2>/dev/null
echo "wrote $OUT"

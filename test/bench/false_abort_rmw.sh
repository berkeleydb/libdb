#!/bin/sh
# false_abort_rmw.sh -- the REALISTIC arm of the SSI false-abort measurement.
#
#   BENCH=/path/to/ssi_abort_bench OUT=rmw.csv ./false_abort_rmw.sh [reps]
#
# The ring in false_abort_sweep.sh is a worst case built to maximise SSI edges.
# A user asks "what does SSI cost MY workload", so this arm runs a plain
# read-modify-write (SSI_READS reads then one write) over a uniform or Zipfian
# key space, and sweeps the page size -- the lever that sets records-per-leaf.
#
# THREE ARMS per (distribution, page size), run BACK TO BACK so machine drift
# hits all three equally:
#
#   plain   writes and reads both drawn from the whole key space.  Has a GENUINE
#           conflict graph, so its aborts are genuine+artifact mixed.
#   decoy   SSI_WSTRIDE=8: writes only to multiples of 8, reads only to
#           non-multiples.  No transaction reads a key any transaction writes ->
#           the logical conflict graph is EMPTY, so a key-granularity SSI would
#           abort ZERO.  The two key classes still interleave in key order, so
#           they share leaves.  Every abort here is a FALSE abort.
#   split   SSI_WSPLIT=1: writes from the lower half of the key space, reads
#           from the upper half.  Logical graph EMPTY *and* the classes are
#           separated in key order, so only the boundary leaf can be shared.
#           This is the row-granularity ideal -- the baseline the decoy's excess
#           is measured against, and it is what makes the claim falsifiable
#           rather than assumed.
#
# Plus an SI control per point (ssi_abort must be 0) and the ordinary-deadlock
# counter, so page ww loss is never counted as an SSI abort.  valsz is pinned at
# 64 (inline at every page size >= 512; see false_abort_sweep.sh trap 1), fresh
# env per run, every invocation under `timeout`.
set -u

BENCH=${BENCH:-./ssi_abort_bench}
OUT=${OUT:-false-abort-rmw.csv}
SECS=${SECS:-5}
THREADS=${THREADS:-8}
REPS=${1:-5}
RUNDIR=${RUNDIR:-/tmp/fa_rmw_$$}
TMO=${TMO:-120}
HOT=${HOT:-20000}
VALSZ=${VALSZ:-64}
READS=${READS:-4}
STRIDE=${STRIDE:-8}
PAGESIZES=${PAGESIZES:-"512 1024 4096 8192 16384 32768"}

mkdir -p "$RUNDIR" || exit 1

one() {
	tag=$1; rep=$2; shift 2
	d="$RUNDIR/e_${tag}_${rep}"
	mkdir -p "$d" || return 1
	out=$(env SSI_CSV=1 SSI_ENV="$d" SSI_VALSZ="$VALSZ" SSI_WORKLOAD=rmw \
	    SSI_READS="$READS" "$@" \
	    timeout "$TMO" "$BENCH" "$HOT" "$SECS" "$THREADS" 2>"$d/err")
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

d="$RUNDIR/e_hdr"; mkdir -p "$d"
env SSI_CSV=1 SSI_ENV="$d" SSI_WORKLOAD=rmw timeout "$TMO" "$BENCH" 2000 1 2 \
    2>/dev/null | sed -n 's/^CSV,workload,/tag,rep,workload,/p' | head -1 >"$OUT"
find "$d" -mindepth 1 -delete 2>/dev/null; rmdir "$d" 2>/dev/null

rep=1
while [ "$rep" -le "$REPS" ]; do
	for dist in uni zipf; do
		[ "$dist" = zipf ] && z="SSI_ZIPF=1" || z="SSI_ZIPF=0"
		for ps in $PAGESIZES; do
			one "$dist-plain-ps$ps" "$rep" SSI_PAGESIZE="$ps" \
			    $z ISO_LEVEL=serializable
			one "$dist-decoy-ps$ps" "$rep" SSI_PAGESIZE="$ps" \
			    $z SSI_WSTRIDE="$STRIDE" ISO_LEVEL=serializable
			one "$dist-split-ps$ps" "$rep" SSI_PAGESIZE="$ps" \
			    $z SSI_WSPLIT=1 ISO_LEVEL=serializable
			one "$dist-plain-si-ps$ps" "$rep" SSI_PAGESIZE="$ps" \
			    $z ISO_LEVEL=snapshot
			one "$dist-decoy-si-ps$ps" "$rep" SSI_PAGESIZE="$ps" \
			    $z SSI_WSTRIDE="$STRIDE" ISO_LEVEL=snapshot
		done
	done
	echo "rep $rep done: $(wc -l <"$OUT") rows" >&2
	rep=$((rep + 1))
done

rmdir "$RUNDIR" 2>/dev/null
echo "wrote $OUT"

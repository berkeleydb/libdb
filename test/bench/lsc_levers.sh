#!/bin/sh
# lsc_levers.sh -- the metadata allocation ceiling, and the two levers that
# move it WITHOUT a library change.
#
# The characterisation says allocating transactions serialise on PGNO_BASE_MD,
# each holding it across its own durable commit, so the engine can retire at
# most 1/commit_latency ALLOCATING transactions per second no matter how many
# threads run.  That predicts two things an application can do, and one it
# cannot:
#
#   BATCH=N     more inserts under ONE allocating transaction.  The txn rate
#               stays capped; inserts/s should rise ~linearly in N until the
#               cap is no longer the binding constraint.
#   PAGESIZE=P  fewer inserts need an allocation at all (P/4096 x fewer pages),
#               so the allocating fraction falls and the cap stops binding.
#   nosync      removes the fsync from inside the hold.  NOT a candidate fix --
#               it is the control that proves the hold-across-fsync is the
#               mechanism rather than the lock itself.
#
# ONE binary, ONE directory reused by every arm, only runtime switches varying
# (run_bench.sh's DB_PRIVATE layout warning).  Streams one line per arm.
set -e
WT=${WT:-$HOME/wt-lockscope}
B=$WT/test/bench
S=$WT/build_unix
T=${T:-96}
SECS=${SECS:-15}
REPS=${REPS:-3}
OUT=${OUT:-/tmp/lsc-lev}
mkdir -p $OUT
D=/tmp/lscmx
mkdir -p $D

cd $B
for rep in $(seq 1 $REPS); do
for arm in base batch10 batch100 pg16k pg64k bulk nosync; do
	find $D -mindepth 1 -delete 2>/dev/null || true
	BATCH=1; PAGESIZE=0; MODE=sync; BULK=0
	case $arm in
	batch10)  BATCH=10 ;;
	batch100) BATCH=100 ;;
	pg16k)    PAGESIZE=16384 ;;
	pg64k)    PAGESIZE=65536 ;;
	bulk)     BULK=1 ;;
	nosync)   MODE=nosync ;;
	esac
	L=$OUT/$arm.r$rep
	( PREPOP=0 KEYRANGE=1000000 BATCH=$BATCH PAGESIZE=$PAGESIZE \
	  LSC_BULK=$BULK ./commit_bench $D $T $SECS $MODE \
	  > $L.bench.txt 2>&1 ) &
	BPID=$!
	sleep 6
	for i in 1 2 3; do
		$S/db_stat -h $D -Co > $L.objects.$i.txt 2>&1 || true
		sleep 1
	done
	wait $BPID || true
	AGG=$(cat $L.objects.*.txt | awk '
	    /page +[0-9]+$/ { st=$4; pg=$NF
		if (st=="WAIT") { w[pg]++; tw++ } else if (st=="HELD") th++ }
	    END { printf "wait_total=%d wait_pg0=%d wait_other=%d held_total=%d",
		tw, w[0]+0, tw-(w[0]+0), th }')
	SPL=$($S/db_printlog -h $D 2>/dev/null \
	    | grep -oE '__(bam_split|db_pg_alloc|txn_regop)' \
	    | sort | uniq -c | awk '{printf "%s=%s ", $2, $1}')
	echo "LEVER rep=$rep arm=$arm $(grep '^mode=' $L.bench.txt)"
	echo "LEVER rep=$rep arm=$arm $(grep '^BATCH' $L.bench.txt)"
	echo "LEVER rep=$rep arm=$arm $(grep '^PHASE put_us' $L.bench.txt)"
	echo "LEVER rep=$rep arm=$arm $(grep '^PHASE commit_us' $L.bench.txt)"
	echo "LEVER rep=$rep arm=$arm LOCKS $AGG"
	echo "LEVER rep=$rep arm=$arm LOG $SPL"
	echo "LEVER rep=$rep arm=$arm ---"
done
done
find $D -mindepth 1 -delete 2>/dev/null || true
echo LEVERS_DONE

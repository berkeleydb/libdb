#!/bin/sh
# lsc_matrix.sh -- the Deliverable-1 characterisation matrix.
#
# For each thread count and each arm (unwarmed = growing tree = the workload
# under study; warmed = steady state, the control that proves we did not just
# re-measure around the problem), run commit_bench and, WHILE it runs, sample
# "locks grouped by object" so waiters can be attributed per lock object.
#
# Prints one RESULT line per run as it completes -- nothing is buffered, so a
# lost box costs at most the run in flight.
#
# Usage: lsc_matrix.sh <tag> [threads...]      env: SECS, REPS, ARMS, SAMPLES
set -e
WT=${WT:-$HOME/wt-lockscope}
B=$WT/test/bench
S=$WT/build_unix
TAG=${1:-run}
shift 2>/dev/null || true
THREADS=${*:-1 8 32 96}
SECS=${SECS:-20}
REPS=${REPS:-1}
ARMS=${ARMS:-"unwarmed warmed"}
SAMPLES=${SAMPLES:-5}
OUT=${OUT:-/tmp/lsc-$TAG}
mkdir -p $OUT

# ONE env directory, reused by every arm, at a FIXED path length: the only
# unconfounded form on this box (run_bench.sh's DB_PRIVATE layout warning).
D=/tmp/lscmx
mkdir -p $D

cd $B
echo "# lsc_matrix tag=$TAG secs=$SECS reps=$REPS threads='$THREADS' arms='$ARMS'"
$S/../test/bench/commit_bench 2>/dev/null | head -0 || true

for rep in $(seq 1 $REPS); do
for t in $THREADS; do
for arm in $ARMS; do
	find $D -mindepth 1 -delete 2>/dev/null || true
	if [ "$arm" = unwarmed ]; then P=0; else P=1; fi
	L=$OUT/$arm.t$t.r$rep
	( PREPOP=$P KEYRANGE=1000000 ./commit_bench $D $t $SECS sync \
	    > $L.bench.txt 2>&1 ) &
	BPID=$!
	# Let PREPOP finish and the convoy establish before sampling.
	if [ "$arm" = warmed ]; then sleep 12; else sleep 6; fi
	i=1
	while [ $i -le $SAMPLES ]; do
		$S/db_stat -h $D -Co > $L.objects.$i.txt 2>&1 || true
		i=$((i+1))
		sleep 1
	done
	$S/db_stat -h $D -cl > $L.lockstat.txt 2>&1 || true
	wait $BPID || true

	# ---- attribute the waiters per lock object, per sample ----
	AGG=$(cat $L.objects.*.txt | awk '
	    /page +[0-9]+$/ {
		st=$4; pg=$NF
		if (st=="WAIT") { w[pg]++; tw++ }
		else if (st=="HELD") { h[pg]++; th++ }
	    }
	    END {
		printf "wait_total=%d wait_pg0=%d wait_other=%d held_total=%d held_pg0=%d held_pages=%d",
		    tw, w[0]+0, tw-(w[0]+0), th, h[0]+0, length(h)
		mx=0; mp=-1
		for (p in w) if (p != 0 && w[p] > mx) { mx=w[p]; mp=p }
		printf " max_nonmeta_wait_pg=%s n=%d\n", mp, mx
	    }')
	# ---- split rate + allocation rate from the log ----
	SPL=$($S/db_printlog -h $D 2>/dev/null \
	    | grep -oE '__(bam_split|bam_rsplit|db_pg_alloc|txn_regop|db_pg_free)' \
	    | sort | uniq -c | awk '{printf "%s=%s ", $2, $1}')
	OPS=$(grep -oE 'ops_sec=[0-9]+' $L.bench.txt | head -1)
	PUT=$(grep '^PHASE put_us' $L.bench.txt)
	CMT=$(grep '^PHASE commit_us' $L.bench.txt)
	BEG=$(grep '^PHASE begin_us' $L.bench.txt)
	SUM=$(grep '^mode=' $L.bench.txt)
	echo "RESULT rep=$rep arm=$arm t=$t $SUM"
	echo "RESULT rep=$rep arm=$arm t=$t $BEG"
	echo "RESULT rep=$rep arm=$arm t=$t $PUT"
	echo "RESULT rep=$rep arm=$arm t=$t $CMT"
	echo "RESULT rep=$rep arm=$arm t=$t LOCKS $AGG"
	echo "RESULT rep=$rep arm=$arm t=$t LOG $SPL"
	echo "RESULT rep=$rep arm=$arm t=$t ---"
done
done
done
find $D -mindepth 1 -delete 2>/dev/null || true
echo MATRIX_DONE

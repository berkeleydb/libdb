#!/bin/sh
# lsc_site.sh -- WHICH CALL SITE are the waiters queued at, and for how long is
# the meta lock held?
#
# The waiter census (lsc_matrix.sh) says every waiter is on page 0.  It cannot
# say whether they are blocked at __bam_split's own meta get (bt_split.c:83,
# taken before the descent for lock ordering, released with __LPUT) or at
# __db_new's (db_meta.c:135, released with __TLPUT = held to commit inside a
# txn).  That distinction decides what a fix would have to move.
#
# Deep backtraces (bt 25) let us count, per parked thread, whether __bam_split
# or __db_new is on the stack above __lock_get_internal.
set -e
WT=${WT:-$HOME/wt-lockscope}
B=$WT/test/bench
S=$WT/build_unix
OUT=${OUT:-/tmp/lsc-site}
T=${T:-96}
SECS=${SECS:-25}
mkdir -p $OUT
D=/tmp/lscmx
mkdir -p $D
find $D -mindepth 1 -delete 2>/dev/null || true

cd $B
( PREPOP=0 KEYRANGE=1000000 ./commit_bench $D $T $SECS sync \
    > $OUT/bench.txt 2>&1 ) &
BPID=$!
sleep 8
for i in 1 2 3; do
	timeout 120 gdb -p $BPID -batch -ex 'thread apply all bt 25' \
	    > $OUT/deep.$i.txt 2>&1 || true
	sleep 2
done
wait $BPID || true

echo "=== bench"
grep -E '^mode=|^PHASE|^BATCH' $OUT/bench.txt

for i in 1 2 3; do
	echo "=== sample $i: per-thread call-site attribution"
	awk '
	    /^Thread /            { nthr++; inlk=0; split_f=0; new_f=0; fsy=0 }
	    /__lock_get_internal/ { inlk=1 }
	    /__bam_split/         { split_f=1 }
	    /__db_new/            { new_f=1 }
	    /__os_fsync/          { fsy=1 }
	    /^$/ {
		if (inlk) {
		    nwait++
		    if (new_f) both++
		    else if (split_f) split_only++
		    else other++
		}
		if (fsy) nfsync++
		inlk=0; split_f=0; new_f=0; fsy=0
	    }
	    END {
		printf "threads=%d in_lock_get=%d  at_db_new(held-to-commit site)=%d  at_bam_split_only(pre-descent site)=%d  elsewhere=%d  in_fsync=%d\n",
		    nthr, nwait, both, split_only, other, nfsync
	    }' $OUT/deep.$i.txt
done

echo "=== distinct frames directly above __lock_get_internal, all samples"
cat $OUT/deep.*.txt | grep -B0 -A2 '__lock_get_internal' \
    | grep -oE '__(db_lget|lock_get|bam_split|db_new|bam_page|bam_root|bam_iitem)' \
    | sort | uniq -c | sort -rn | head
echo SITE_DONE

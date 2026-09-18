#!/bin/sh
# rdl_regime.sh -- the two-regime measurement for the read-descent-lock question.
#
# Regime U (uniform):  scale_bench rrand -- every read lands on a different leaf,
#                      so lock objects spread across partitions.
# Regime H (hot key):  scale_bench rhot  -- every read lands on ONE leaf, so
#                      LOCK_PART = ndx % part_t_size selects ONE partition mutex
#                      no matter how many partitions exist.
#
# Reports, per regime and thread count: ops/sec and the perf self-time share of
# the lock manager (__lock_get_internal + __lock_put_nolock + the tas mutex).
# Both arms use a SHARED environment (not DB_PRIVATE), so the DB_PRIVATE
# env-path-length layout artifact documented at the top of run_bench.sh does not
# apply here.
set -e
W=/home/admin/rdl-wt
B=${B:-$W/bp}
S=/tmp/rdl-regime
SECS=${SECS:-10}
NKEYS=${NKEYS:-100000}
THREADS=${THREADS:-"1 8 32 96"}
BIN=/tmp/rdl_scale.bin

rm -f $BIN
gcc -O2 -g -fno-omit-frame-pointer -o $BIN $W/test/bench/scale_bench.c \
    -I$B -L$B -ldb -lpthread -luring
test -x $BIN || { echo "COMPILE FAILED"; exit 1; }

mkdir -p $S
find $S -mindepth 1 -delete
cd $S

for wk in rrand rhot; do
	case $wk in
	rrand) label=U-uniform ;;
	rhot)  label=H-hotkey ;;
	esac
	echo "=== REGIME $label workload=$wk secs=$SECS nkeys=$NKEYS ==="
	timeout 900 $BIN $wk $NKEYS $SECS $THREADS 2>&1 | grep -v '^#' || true
done

# Perf profile at the top thread count only: that is where the partition-mutex
# question is decided.
top=$(echo $THREADS | awk '{print $NF}')
for wk in rrand rhot; do
	case $wk in
	rrand) label=U-uniform ;;
	rhot)  label=H-hotkey ;;
	esac
	echo "=== PERF $label t=$top ==="
	find $S -mindepth 1 -delete
	timeout 900 perf record -q -F 997 -g --call-graph fp \
	    -o $S/perf-$wk.data -- $BIN $wk $NKEYS $SECS $top >/dev/null 2>&1 || true
	timeout 300 perf report -i $S/perf-$wk.data --no-children --percent-limit 0.4 \
	    --stdio 2>/dev/null | grep -E '^ +[0-9]' | head -18 || true
	echo "--- lock-manager self-time share, $label ---"
	timeout 300 perf report -i $S/perf-$wk.data --no-children --stdio \
	    2>/dev/null | grep -E '^ +[0-9]' |
	    awk '{ pct=$1+0; tot+=pct;
		if ($0 ~ /__lock_get_internal|__lock_put_nolock|__lock_get|__lock_put|__lock_vec|__db_lget/) lk+=pct;
		if ($0 ~ /__db_tas_mutex_lock|MUTEX_LOCK/) mtx+=pct;
		if ($0 ~ /__memp_fget|__memp_fput|__memp_bhpin/) pin+=pct }
	      END { printf "LOCKMGR=%.2f%%  TASMUTEX=%.2f%%  PIN=%.2f%%  (of %.1f%% accounted)\n", lk, mtx, pin, tot }'
done
echo RDL_REGIME_DONE

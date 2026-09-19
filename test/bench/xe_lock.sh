#!/bin/sh
# xe_lock.sh -- refuse to start a measurement campaign while another is running.
#
# WHY.  I contaminated 25 runs by starting the HASH campaign while the MIXED
# campaign was still going.  Two benchmarks sharing 96 CPUs and one NVMe device
# produced an 8x outlier (134 txn/s against ~1100 for the same configuration) and
# inflated the MIXED arm's CV from 1.4% to 41.6% at t=1 and 4.9% to 47.8% at t=8.
# The arm looked intrinsically unstable; it was not, the overlap was.  Excluding
# the overlapping runs restored the real figures, which is how I know the noise
# was the contention and not the engine.
#
# The cgroup memory cap does NOT prevent this: it bounds memory, not CPU time or
# device queue depth.  Only serialization does.
#
# usage: xe_lock.sh <command...>
LOCK=/nvme/results/.campaign.lock

mkdir -p "$(dirname "$LOCK")"
if [ -e "$LOCK" ]; then
	pid=$(cat "$LOCK" 2>/dev/null)
	if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
		echo "REFUSING TO START: a campaign is already running (pid $pid)."
		echo "Two concurrent campaigns contend for CPU and device and corrupt"
		echo "both sets of numbers.  Wait for it, or kill it deliberately."
		exit 1
	fi
	echo "# stale lock from dead pid $pid, reclaiming"
fi
echo $$ > "$LOCK"
trap 'rm -f $LOCK' EXIT INT TERM
exec "$@"

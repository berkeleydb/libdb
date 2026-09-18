#!/bin/bash
#
# xe_run.sh -- driver for the cross-engine TPROC campaign.
#
# Runs the arm matrix REPS times with the arms ALTERNATING WITHIN EACH REP, and
# streams one TSV row per completed run to stdout AND to the results file as it
# happens.
#
# ---------------------------------------------------------------------------
# WHY THE ARMS ALTERNATE, AND WHY RESULTS STREAM
# ---------------------------------------------------------------------------
# Alternating (A B C D, A B C D, ...) rather than blocking (A A A A A, B B ...)
# is what makes the comparison robust to drift: if the machine warms up, a
# neighbour starts stealing cache, or the SSD's write amplification climbs over
# the hour, a blocked design attributes that drift entirely to whichever arm ran
# during it.  Alternating spreads it across all arms.
#
# Streaming is for a different failure: a sibling agent buffered 5 reps to a
# remote file, lost the instance, and lost 4.2 of them.  Every run's result is
# printed and appended the moment it completes, so losing the box costs at most
# the run in flight.
#
# ---------------------------------------------------------------------------
# THE PAGE-CACHE PROBLEM (read this before changing anything here)
# ---------------------------------------------------------------------------
# The experiment requires that neither engine get a second, unaccounted cache
# from the OS page cache.  Neither engine can be made to use O_DIRECT here:
#
#   WiredTiger  `direct_io` is a documented no-op in 12.0.0 ("no longer
#               supported, retained for backward compatibility").  It parses,
#               so a run can set it and claim O_DIRECT; an strace of a real
#               write through an undersized cache shows ZERO O_DIRECT opens on
#               the data file.  Verified, not assumed.
#   libdb       DB_DIRECT_DB genuinely passes O_DIRECT to open(), but the first
#               meta-page read then fails EINVAL ("BDB0134 read: ..., 8192:
#               Invalid argument") because __fop_read_meta reads through an
#               unaligned buffer, and DB->open fails outright.  So it cannot be
#               used either -- that is a real libdb bug, reported as such.
#
# So the page cache is bounded from OUTSIDE, identically for both engines, with
# a cgroup v2 memory.max on the benchmark process.  The cap is
# engine_cache + XE_CGROUP_HEADROOM, so the kernel has almost no room to hold a
# second copy of the data: page-cache pages are charged to the cgroup and
# reclaimed under pressure.  This is symmetric, requires no cooperation from
# either engine, and is verifiable (memory.stat's file field is reported).
#
# usage: xe_run.sh [-r REPS] [-s SECS] [-W WARMUP] [-n "1 8 32 96"] [-o OUT]
#                  [-C CACHE_GIB] [-S SCALE] [-P PAD] [-w WORKLOAD] [-A ARMS]
set -u

REPS=5
SECS=60
WARMUP=60
THREADS="1 8 32 96"
OUT=/nvme/results/xe.tsv
CACHE_GIB=16
SCALE=0			# 0 = derive from CACHE_GIB for the target ratio
PAD=1024
WORKLOAD="c h"
ARMS="libdb-sync-btree libdb-uring-btree libdb-sync-hash libdb-uring-hash libdb-sync-mixed libdb-uring-mixed wt-btree"
BIN=/nvme
DATA=/nvme/xedata
TIMEOUT_PAD=900
CGROUP=/sys/fs/cgroup/xebench
HEADROOM_GIB=6
TARGET_RATIO=13.6	# data:cache, matching the briefed 1.85TiB:139GiB

while getopts "r:s:W:n:o:C:S:P:w:A:h" ch; do
	case $ch in
	r) REPS=$OPTARG ;;
	s) SECS=$OPTARG ;;
	W) WARMUP=$OPTARG ;;
	n) THREADS=$OPTARG ;;
	o) OUT=$OPTARG ;;
	C) CACHE_GIB=$OPTARG ;;
	S) SCALE=$OPTARG ;;
	P) PAD=$OPTARG ;;
	w) WORKLOAD=$OPTARG ;;
	A) ARMS=$OPTARG ;;
	h) sed -n '3,50p' "$0"; exit 0 ;;
	*) exit 1 ;;
	esac
done

mkdir -p "$(dirname "$OUT")" "$DATA"
CACHE_BYTES=$((CACHE_GIB * 1024 * 1024 * 1024))

# ---- cgroup page-cache cap -------------------------------------------
cg_setup() {
	if [ ! -d "$CGROUP" ]; then
		sudo mkdir -p "$CGROUP" 2>/dev/null || return 1
	fi
	echo "+memory" | sudo tee /sys/fs/cgroup/cgroup.subtree_control >/dev/null 2>&1
	local cap=$(( (CACHE_GIB + HEADROOM_GIB) * 1024 * 1024 * 1024 ))
	echo "$cap" | sudo tee "$CGROUP/memory.max" >/dev/null 2>&1 || return 1
	# No swap: swapping the engine cache out would be a third storage tier.
	echo 0 | sudo tee "$CGROUP/memory.swap.max" >/dev/null 2>&1
	echo "# cgroup $CGROUP memory.max=$(cat "$CGROUP/memory.max") (cache=${CACHE_GIB}G + headroom=${HEADROOM_GIB}G)"
	return 0
}

cg_run() {
	# Run "$@" inside the cgroup.  sudo cgexec is not installed; move the
	# shell into the cgroup and exec, which is equivalent and dependency-free.
	sudo sh -c "echo \$\$ > $CGROUP/cgroup.procs; exec $*"
}

cg_filecache() {
	awk '/^file /{print $2}' "$CGROUP/memory.stat" 2>/dev/null || echo 0
}

# ---- arm decoding ----------------------------------------------------
arm_engine() { case $1 in wt-*) echo wt ;; *) echo libdb ;; esac; }
arm_am()     { echo "${1##*-}"; }
arm_aio()    { case $1 in *-uring-*) echo 1 ;; *) echo 0 ;; esac; }

# The DATASET an arm uses.  libdb-sync and libdb-uring differ only by a runtime
# flag, so they MUST share a dataset -- loading two identical copies would waste
# hours and, worse, give the two arms different physical layouts, which is
# exactly the confound this campaign is trying to avoid.
arm_dataset() {
	local a=$1 e am
	e=$(arm_engine "$a"); am=$(arm_am "$a")
	echo "${e}_${am}"
}

# ---- path-length equalisation ---------------------------------------
# DB_PRIVATE throughput on at least one c7i.24xlarge is BIMODAL IN THE LENGTH of
# the environment-home path (a 1.65x step at 38 vs 42 chars, ASLR off, spread
# within a mode ~0%).  These runs are shared-env, not DB_PRIVATE, and
# shared-env numbers were NOT affected -- but the cheap insurance is to make
# every arm's home path the SAME LENGTH, so path length cannot differ across
# arms even in principle.  Dataset names are padded to a fixed width.
DATASET_WIDTH=14
dataset_dir() {
	local n=$1
	while [ ${#n} -lt $DATASET_WIDTH ]; do n="${n}_"; done
	echo "$DATA/$n"
}

# ---- output ----------------------------------------------------------
if [ ! -s "$OUT" ]; then
	{
		echo "# cross-engine TPROC campaign"
		echo "# started $(date -u +%FT%TZ)"
		echo "# host $(uname -n) kernel $(uname -r) nproc $(nproc)"
		echo "# ram_gib $(awk '/MemTotal/{printf "%.1f", $2/1048576}' /proc/meminfo)"
		echo "# cache_gib $CACHE_GIB target_data_cache_ratio $TARGET_RATIO"
		echo "# reps $REPS secs $SECS warmup $WARMUP threads '$THREADS'"
		echo "# arms '$ARMS'"
		printf "workload\tarm\tengine\tam\taio\tthreads\trep\tmetric\tvalue\n"
	} >> "$OUT"
fi

emit() { printf '%s\n' "$1" | tee -a "$OUT"; }

# ---- load a dataset once, reused by every rep ------------------------
loaded_marker() { echo "$(dataset_dir "$1")/.xe_loaded_$2"; }

load_dataset() {
	local wl=$1 ds=$2 e am d marker bin
	e="${ds%%_*}"; am="${ds##*_}"
	d=$(dataset_dir "$ds")
	marker=$(loaded_marker "$ds" "$wl")
	bin=$BIN/xe_tproc_$wl
	[ -f "$marker" ] && { echo "# dataset $ds/$wl already loaded"; return 0; }
	mkdir -p "$d"
	find "$d" -mindepth 1 -delete 2>/dev/null
	echo "# LOADING $wl $ds scale=$SCALE pad=$PAD -> $d"
	local t0=$(date +%s)
	if ! cg_run "$bin -i -e $e -a $am -h $d -S $SCALE -P $PAD \
	    -c $CACHE_BYTES" 2>&1 | grep -E 'VERDICT load|FAIL|load '; then
		echo "# LOAD FAILED $wl $ds"
		return 1
	fi
	echo "# loaded $wl $ds in $(( $(date +%s) - t0 ))s"
	touch "$marker"
	return 0
}

# ---- one measured run ------------------------------------------------
run_one() {
	local wl=$1 arm=$2 t=$3 rep=$4
	local e am aio ds d bin cap log rc line
	e=$(arm_engine "$arm"); am=$(arm_am "$arm"); aio=$(arm_aio "$arm")
	ds=$(arm_dataset "$arm"); d=$(dataset_dir "$ds")
	bin=$BIN/xe_tproc_$wl
	cap=$((SECS + WARMUP + TIMEOUT_PAD))
	log=/nvme/results/${wl}_${arm}_t${t}_r${rep}.log

	local aflag=""
	[ "$aio" = 1 ] && aflag="-A"
	local extra=""
	[ "$wl" = h ] && extra="-w 4 -n 200000"

	local fc0 fc1
	fc0=$(cg_filecache)
	# EVERY run is timeout-wrapped.  Exit 124 is DATA, not a failure to
	# hide: DB_MPOOL_AIO is being measured precisely because it has a known
	# stall class (S1, fixed this cycle but still default-off), and a stall
	# under the feature under test is a finding.
	cg_run "timeout -s KILL $cap $bin -e $e -a $am -h $d -S $SCALE -P $PAD \
	    -c $CACHE_BYTES -t $t -s $SECS -W $WARMUP $aflag $extra" \
	    > "$log" 2>&1
	rc=$?
	fc1=$(cg_filecache)

	if [ $rc -eq 124 ] || [ $rc -eq 137 ]; then
		emit "$(printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\tTIMEOUT_STALL\t%d' \
		    "$wl" "$arm" "$e" "$am" "$aio" "$t" "$rep" "$cap")"
		echo "### STALL rc=$rc arm=$arm t=$t rep=$rep -- see $log"
		# Capture evidence rather than discarding the run.
		tail -5 "$log" | sed 's/^/###   /'
		return 0
	fi

	line=$(grep -m1 '^VERDICT' "$log")
	if [ -z "$line" ]; then
		emit "$(printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\tNO_VERDICT\t0' \
		    "$wl" "$arm" "$e" "$am" "$aio" "$t" "$rep")"
		echo "### FAIL no VERDICT line arm=$arm t=$t rep=$rep rc=$rc"
		grep -E '^FAIL|error|Error' "$log" | head -3 | sed 's/^/###   /'
		return 0
	fi

	# Pull every key=value off the verdict line into TSV rows.
	echo "$line" | tr ' ' '\n' | grep '=' | while IFS='=' read -r k v; do
		case $k in
		txn_per_sec|tpmC_like|committed|queries_per_sec|rows_per_sec|\
		updates_per_sec|ops|data_gib|elapsed)
			emit "$(printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s' \
			    "$wl" "$arm" "$e" "$am" "$aio" "$t" "$rep" "$k" "$v")"
			;;
		esac
	done

	# Latency percentiles per transaction type.
	grep -E '^TXN |^OP ' "$log" | while read -r tag name comm retr err rest; do
		case "$comm" in
		-) continue ;;		# N/A row
		esac
		local p50 p99 p999
		if [ "$tag" = TXN ]; then
			p50=$(echo "$rest" | awk '{print $1}')
			p99=$(echo "$rest" | awk '{print $2}')
			p999=$(echo "$rest" | awk '{print $3}')
		else
			p50=$(echo "$rest" | awk '{print $2}')
			p99=$(echo "$rest" | awk '{print $3}')
			p999=$(echo "$rest" | awk '{print $4}')
		fi
		for m in p50:$p50 p99:$p99 p999:$p999; do
			emit "$(printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s_%s\t%s' \
			    "$wl" "$arm" "$e" "$am" "$aio" "$t" "$rep" \
			    "${m%%:*}" "$name" "${m##*:}")"
		done
	done

	# Cache-hit / read-amplification evidence: proof this was out-of-cache.
	local io
	io=$(grep -m1 '^IOSTAT' "$log")
	if [ -n "$io" ]; then
		echo "$io" | tr ' ' '\n' | grep '=' | while IFS='=' read -r k v; do
			case $k in
			hit_rate_pct|cache_miss|pages_in|read_amp_pages_per_txn|bytes_read)
				emit "$(printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\tio_%s\t%s' \
				    "$wl" "$arm" "$e" "$am" "$aio" "$t" "$rep" "$k" "$v")"
				;;
			esac
		done
	fi

	# Page-cache growth inside the cgroup, so the cap is shown to work.
	emit "$(printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\tcgroup_file_bytes\t%s' \
	    "$wl" "$arm" "$e" "$am" "$aio" "$t" "$rep" "$fc1")"

	# Steady state and optread engagement, as reported by the run itself.
	local ss
	ss=$(grep -m1 '^# steady_state=' "$log" | sed 's/.*steady_state=\([^ ]*\).*/\1/')
	emit "$(printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\tsteady_state\t%s' \
	    "$wl" "$arm" "$e" "$am" "$aio" "$t" "$rep" "${ss:-unknown}")"

	grep -m1 '^OPTREAD' "$log" | sed 's/^/# /'
	grep -m1 '^HASHDELIV' "$log" | sed 's/^/# /'
	grep -m1 '^NA q4' "$log" | sed 's/^/# /'
	echo "# done $wl $arm t=$t rep=$rep : $(echo "$line" | cut -c1-140)"
	return 0
}

# ---- main ------------------------------------------------------------
cg_setup || { echo "FAIL cannot set up cgroup -- page cache would be unbounded"; exit 1; }

# Derive scale for the target data:cache ratio if not given.
# Measured: ~10.63 MiB of tproc-c data per warehouse at pad=1024, and
# ~0.30 GiB of tproc-h data per million lineitems at pad=200 (pad-dependent).
if [ "$SCALE" = 0 ]; then
	TARGET_GIB=$(awk "BEGIN{printf \"%d\", $CACHE_GIB * $TARGET_RATIO}")
	SCALE=$(awk "BEGIN{printf \"%d\", $TARGET_GIB * 1024 / 10.63}")
	echo "# derived SCALE=$SCALE for target ${TARGET_GIB}GiB data (${TARGET_RATIO}x cache)"
fi

echo "# ARMS: $ARMS"
echo "# threads: $THREADS   reps: $REPS   secs: $SECS   warmup: $WARMUP"

for wl in $WORKLOAD; do
	# Load every distinct dataset ONCE, before any measurement.
	declare -A seen=()
	for arm in $ARMS; do
		ds=$(arm_dataset "$arm")
		[ -n "${seen[$ds]:-}" ] && continue
		seen[$ds]=1
		load_dataset "$wl" "$ds" || echo "# continuing without $ds"
	done

	for rep in $(seq 1 "$REPS"); do
		for t in $THREADS; do
			# ARMS ALTERNATE HERE: the inner loop is the arm, so one
			# rep touches every arm before the next rep starts.
			for arm in $ARMS; do
				run_one "$wl" "$arm" "$t" "$rep"
			done
		done
		echo "# ===== rep $rep of $REPS complete for workload $wl ====="
	done
done

echo "# campaign complete $(date -u +%FT%TZ)"

#!/bin/sh
#
# run_bench.sh -- reproducible performance-regression harness for test/bench.
#
# Builds the microbenchmark drivers, runs a FIXED case matrix (fixed seed,
# fixed record counts, fixed duration, pinned thread counts) REPS times each,
# and writes one TSV row per (benchmark, config, threads, rep) to stdout or
# to -o FILE.  Aggregation, noise-floor statistics and the pass/fail gate all
# live in bench_cmp.py -- this script only measures.
#
# Usage:
#   ./run_bench.sh [-o results.tsv] [-r REPS] [-s SECS] [-b BDB_BUILD_DIR]
#                  [-q]            # quick smoke matrix (1 rep, 2s, few cases)
#                  [-n "1 8 32"]   # override thread sweep
#
# Environment:
#   BDB          libdb autoconf build tree (default ../../build_unix).  If it
#                has no db.h the tree is configured+built with the flags
#                recorded in the output provenance header.
#   BENCH_HW     free-form hardware label recorded in the header (e.g. an EC2
#                instance type).  Auto-detected from EC2 IMDS when possible.
#   BENCH_SCRATCH working directory for benchmark environments
#                (default $TMPDIR/libdb-bench).
#
# See README.md for how to interpret the output and run the gate.

set -e

REPS=5
SECS=10
OUT=
QUICK=0
BDB=${BDB:-../../build_unix}
THREADS_DEF="1 8 32 96"
THREADS_TPROC="1 8 32"
SEED=42			# every driver that accepts -R gets this
CFLAGS_BENCH="-O2 -pthread"

# Fixed dataset sizes.  Chosen to fit the drivers' own in-process cache
# settings so the gate measures CPU/synchronisation, not disk (see README).
NKEYS=100000		# scale_bench / scale_iso record count
LOCK_NOBJ_DISTINCT=1024
LOCK_NOBJ_SHARED=64
SSI_HOTKEYS=64
TPROC_SCALE=1

while getopts "o:r:s:b:n:qh" ch; do
	case $ch in
	o) OUT=$OPTARG ;;
	r) REPS=$OPTARG ;;
	s) SECS=$OPTARG ;;
	b) BDB=$OPTARG ;;
	n) THREADS_DEF=$OPTARG; THREADS_TPROC=$OPTARG ;;
	q) QUICK=1 ;;
	h|*) sed -n '3,28p' "$0"; exit 0 ;;
	esac
done

if [ "$QUICK" = 1 ]; then
	REPS=1; SECS=2; NKEYS=20000; TPROC_SCALE=1
	THREADS_DEF="1 4"; THREADS_TPROC="1 4"
fi

here=$(cd "$(dirname "$0")" && pwd)
cd "$here"
case $BDB in /*) ;; *) BDB=$(cd "$BDB" 2>/dev/null && pwd || echo "$BDB") ;; esac

SCRATCH=${BENCH_SCRATCH:-${TMPDIR:-/tmp}/libdb-bench}

# Never `rm -rf` (spelled `rm -fr` here deliberately: some shells' safety
# wrappers reject the other spelling).
scratch_reset() {
	rm -fr "$1" 2>/dev/null || true
	mkdir -p "$1"
}

log() { echo "run_bench: $*" >&2; }

# ---------------------------------------------------------------- build ----
if [ ! -f "$BDB/db.h" ]; then
	log "no $BDB/db.h -- configuring libdb in $BDB"
	mkdir -p "$BDB"
	( cd "$BDB" && ../dist/configure --enable-o_direct >/dev/null &&
	  make -j"$(getconf _NPROCESSORS_ONLN)" >/dev/null ) ||
		{ log "libdb build failed"; exit 2; }
fi
LIBDB_CONFIGURE="--enable-o_direct"
log "building drivers against $BDB"
make -s BDB="$BDB" CFLAGS="$CFLAGS_BENCH" >&2

# Guard against measuring the WRONG library.  The Makefile sets -rpath, but a
# stale binary, a LD_LIBRARY_PATH, or a system libdb-5.3 in the default search
# path would otherwise silently redirect every driver at another build -- which
# produced a reproducible SIGSEGV in ssi_abort_bench against a distro
# libdb-5.3.28 during development, i.e. this failure mode is not theoretical.
for _b in lock_bench scale_bench scale_iso ssi_abort_bench tproc_b tproc_c tproc_h; do
	_resolved=$(ldd "$here/$_b" 2>/dev/null | awk '/libdb-5\.3/{print $3}')
	case $_resolved in
	"$BDB"/.libs/*) ;;
	*) log "FATAL: $_b resolves libdb-5.3 to '${_resolved:-<none>}',"
	   log "       expected $BDB/.libs/ -- refusing to publish numbers for"
	   log "       a library that is not the one under test."
	   exit 2 ;;
	esac
done

# ----------------------------------------------------------- provenance ----
emit() { if [ -n "$OUT" ]; then cat >> "$OUT"; else cat; fi; }

imds() {
	tok=$(curl -s -m 1 -X PUT http://169.254.169.254/latest/api/token \
	    -H 'X-aws-ec2-metadata-token-ttl-seconds: 60' 2>/dev/null) || return 1
	[ -n "$tok" ] || return 1
	curl -s -m 1 -H "X-aws-ec2-metadata-token: $tok" \
	    "http://169.254.169.254/latest/meta-data/$1" 2>/dev/null
}

HW=${BENCH_HW:-$(imds instance-type 2>/dev/null || echo unknown)}
[ -n "$HW" ] || HW=unknown
CPU_MODEL=$(awk -F': *' '/model name/{print $2; exit}' /proc/cpuinfo 2>/dev/null || echo unknown)
VCPUS=$(getconf _NPROCESSORS_ONLN)
NUMA=$(ls -d /sys/devices/system/node/node* 2>/dev/null | wc -l | tr -d ' ')
GITREV=$(git -C "$here" rev-parse --short HEAD 2>/dev/null || echo unknown)
GITDIRTY=$(git -C "$here" status --porcelain -- "$here/.." 2>/dev/null | head -1)
LOADAVG=$(cut -d' ' -f1-3 /proc/loadavg 2>/dev/null || echo unknown)
DBVER=$(awk -F'"' '/DB_VERSION_STRING/{print $2; exit}' "$BDB/db.h" 2>/dev/null || echo unknown)

[ -n "$OUT" ] && : > "$OUT"
{
	echo "# libdb-bench-results	v1"
	echo "# generated	$(date -u +%Y-%m-%dT%H:%M:%SZ)"
	echo "# git_rev	$GITREV${GITDIRTY:+ (worktree dirty)}"
	echo "# libdb_version	$DBVER"
	echo "# hardware	$HW"
	echo "# cpu_model	$CPU_MODEL"
	echo "# vcpus	$VCPUS"
	echo "# numa_nodes	$NUMA"
	echo "# kernel	$(uname -sr)"
	echo "# compiler	$(${CC:-cc} --version 2>/dev/null | head -1)"
	echo "# bench_cflags	$CFLAGS_BENCH"
	echo "# libdb_configure	$LIBDB_CONFIGURE"
	echo "# reps	$REPS"
	echo "# secs	$SECS"
	echo "# seed	$SEED"
	echo "# nkeys	$NKEYS"
	echo "# tproc_scale	$TPROC_SCALE"
	echo "# loadavg_at_start	$LOADAVG"
	printf 'benchmark\tconfig\tthreads\tmetric\tunit\trep\tvalue\n'
} | emit

row() { printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$1" "$2" "$3" "$4" "$5" "$6" "$7" | emit; }

# Wall-clock ceiling per driver invocation: the measured window plus a
# generous allowance for load/populate.  Measured spreads for these drivers
# are in README.md; the ceiling exists only to stop a wedged run, so it is
# deliberately far above any observed time (a tight timeout is how timing
# gates start flaking).
cap_for() { echo $(( ($1 * $2) + 900 )); }

run_capped() {
	_cap=$1; shift
	timeout -k 10 "$_cap" "$@"
}

# Drivers report errors on stderr and several of them RETIRE THE WORKER that
# hit one (`return NULL`), so a run that logs errors reports the throughput of
# a shrinking thread pool -- a plausible-looking number that measures nothing.
# Capture stderr per invocation and record it in the results file as a
# `# driver_stderr` line; bench_cmp.py warns when a file contains any.
ERRDIR=$SCRATCH/stderr
note_stderr() {
	_tag=$1; _rep=$2; _f=$3
	[ -s "$_f" ] || return 0
	_n=$(wc -l < "$_f" | tr -d ' ')
	_first=$(head -1 "$_f" | tr '\t' ' ' | cut -c1-100)
	echo "# driver_stderr	$_tag rep $_rep: $_n line(s), first: $_first" | emit
}

# ------------------------------------------------------------- drivers ----
# Each bench_* function runs ONE rep of its case and emits rows.

# scale_bench <workload>: prints "<wkld> <thr> <ops/sec> ..." per thread count
bench_scale() {
	wkld=$1; rep=$2
	d=$SCRATCH/scale_$wkld; scratch_reset "$d"; mkdir -p "$ERRDIR"
	( cd "$d" && run_capped "$(cap_for "$SECS" "$(set -- $THREADS_DEF; echo $#)")" \
	    "$here/scale_bench" "$wkld" "$NKEYS" "$SECS" $THREADS_DEF \
	    2>"$ERRDIR/scale_$wkld" ) |
	awk -v b=scale_bench -v c="$wkld" -v r="$rep" '
	    /^#/ || NF < 3 { next }
	    { printf "%s\t%s\t%s\tops_per_sec\tops/s\t%s\t%.0f\n", b, c, $2, r, $3 }' | emit
	note_stderr "scale_bench/$wkld" "$rep" "$ERRDIR/scale_$wkld"
}

# scale_iso <iso>: prints "<iso> <thr> <ops/sec> ops/sec"
bench_iso() {
	iso=$1; rep=$2
	d=$SCRATCH/iso_$iso; scratch_reset "$d"; mkdir -p "$ERRDIR"
	( cd "$d" && run_capped "$(cap_for "$SECS" "$(set -- $THREADS_DEF; echo $#)")" \
	    "$here/scale_iso" "$iso" "$NKEYS" "$SECS" $THREADS_DEF \
	    2>"$ERRDIR/iso_$iso" ) |
	awk -v b=scale_iso -v c="$iso" -v r="$rep" '
	    /^#/ || NF < 3 { next }
	    { printf "%s\t%s\t%s\tops_per_sec\tops/s\t%s\t%.0f\n", b, c, $2, r, $3 }' | emit
	note_stderr "scale_iso/$iso" "$rep" "$ERRDIR/iso_$iso"
}

# lock_bench <mode> <nobj>: prints "<threads> <ops/sec>"
bench_lock() {
	mode=$1; nobj=$2; rep=$3
	d=$SCRATCH/lock_$mode; scratch_reset "$d"; mkdir -p "$ERRDIR"
	( cd "$d" && mkdir -p LOCKBENCHDIR &&
	  run_capped "$(cap_for "$SECS" "$(set -- $THREADS_DEF; echo $#)")" \
	    "$here/lock_bench" "$SECS" "$nobj" "$mode" $THREADS_DEF \
	    2>"$ERRDIR/lock_$mode" ) |
	awk -v b=lock_bench -v c="$mode" -v r="$rep" '
	    /^#/ || NF != 2 { next }
	    { printf "%s\t%s\t%s\tops_per_sec\tops/s\t%s\t%.0f\n", b, c, $1, r, $2 }' | emit
	note_stderr "lock_bench/$mode" "$rep" "$ERRDIR/lock_$mode"
}

# ssi_abort_bench: prints "threads=N hot=H ... (NNN txn/s)"
bench_ssi() {
	rep=$1
	# The driver hardcodes /tmp/ssi_abort_env and opens with DB_RECOVER.
	scratch_reset /tmp/ssi_abort_env; mkdir -p "$ERRDIR"
	run_capped "$(cap_for "$SECS" "$(set -- $THREADS_DEF; echo $#)")" \
	    "$here/ssi_abort_bench" "$SSI_HOTKEYS" "$SECS" $THREADS_DEF \
	    2>"$ERRDIR/ssi" |
	awk -v b=ssi_abort_bench -v c="hot$SSI_HOTKEYS" -v r="$rep" '
	    /txn\/s/ {
		t = $1; sub(/^threads=/, "", t)
		for (i = 1; i <= NF; i++)
			if ($i ~ /txn\/s\)/) { v = $(i-1); sub(/^\(/, "", v) }
		printf "%s\t%s\t%s\ttxn_per_sec\ttxn/s\t%s\t%.0f\n", b, c, t, r, v
	    }' | emit
}

# tproc_b/-c/-h take -i to populate, then one measured run per thread count.
# The dataset is repopulated for every measured run so each sample starts from
# an identical database state (these workloads mutate rows).
bench_tproc() {
	drv=$1; rep=$2
	for t in $THREADS_TPROC; do
		d=$SCRATCH/${drv}_t$t; scratch_reset "$d"
		run_capped 1800 "$here/$drv" -i -h "$d" -S "$TPROC_SCALE" \
		    -R "$SEED" >/dev/null 2>&1 ||
			{ log "$drv populate failed"; continue; }
		run_capped "$(cap_for "$SECS" 1)" "$here/$drv" -h "$d" \
		    -S "$TPROC_SCALE" -t "$t" -s "$SECS" -R "$SEED" |
		awk -v b="$drv" -v c="S$TPROC_SCALE" -v th="$t" -v r="$rep" '
		    /^tproc-b/ { for (i=1;i<=NF;i++) if ($i=="txn/s")
			  printf "%s\t%s\t%s\ttxn_per_sec\ttxn/s\t%s\t%.0f\n", b, c, th, r, $(i-1) }
		    /^tproc-c/ { for (i=1;i<=NF;i++) if ($i=="tpmC-like")
			  printf "%s\t%s\t%s\ttpmC_like\ttxn/min\t%s\t%.0f\n", b, c, th, r, $(i-1) }
		    /^tproc-h/ { for (i=1;i<=NF;i++) {
			  if ($i ~ /^queries\/s=/) { v=$i; sub(/^queries\/s=/,"",v)
			    printf "%s\t%s\t%s\tqueries_per_sec\tqueries/s\t%s\t%.2f\n", b, c, th, r, v }
			  if ($i ~ /^rows-scanned\/s=/) { v=$i; sub(/^rows-scanned\/s=/,"",v)
			    printf "%s\t%s\t%s\trows_per_sec\trows/s\t%s\t%.0f\n", b, c, th, r, v } } }' | emit
	done
}

# ---------------------------------------------------------------- matrix ---
mkdir -p "$SCRATCH"
rep=1
while [ "$rep" -le "$REPS" ]; do
	log "rep $rep/$REPS"
	bench_lock distinct "$LOCK_NOBJ_DISTINCT" "$rep"
	bench_lock shared "$LOCK_NOBJ_SHARED" "$rep"
	bench_scale rrand "$rep"
	bench_scale rhot "$rep"
	bench_scale wrand "$rep"
	bench_iso none "$rep"
	# scale_iso's `snap` workload is NOT in the matrix.  Under this fork
	# DB_TXN_SNAPSHOT is serializable (SSI), so its one long-lived read-only
	# snapshot transaction per thread accumulates a SIREAD marker per page
	# read for the whole window and exhausts the lock region (BDB2055 /
	# ENOMEM) partway through -- at 500k lock entries as well as at the
	# default ~1000.  A worker that hits the error retires, so the driver
	# still prints a throughput number, but it is the throughput of a
	# shrinking thread pool.  Run it by hand with a short -s for
	# exploration; it cannot be a gate case.  See README.md.
	bench_ssi "$rep"
	bench_tproc tproc_b "$rep"
	bench_tproc tproc_c "$rep"
	bench_tproc tproc_h "$rep"
	rep=$((rep + 1))
done

{
	echo "# loadavg_at_end	$(cut -d' ' -f1-3 /proc/loadavg 2>/dev/null)"
	echo "# complete	yes"
} | emit
rm -fr "$SCRATCH" 2>/dev/null || true
log "done${OUT:+ -> $OUT}"

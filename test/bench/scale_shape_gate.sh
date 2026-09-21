#!/bin/sh
# test/bench/scale_shape_gate.sh -- the SCALING-SHAPE gate (gaps G12 + G13).
#
# THE DEFECT THIS EXISTS TO CATCH
#
# libdb's TPROC-C throughput does not merely lose to WiredTiger; it gets SLOWER
# AS CORES ARE ADDED.  From test/bench/TPROC-XENGINE-2026-09.md:
#
#	threads   libdb tpm   WiredTiger tpm
#	      1         251              258   (indistinguishable)
#	      8       1,041            1,455   (libdb PEAK)
#	     32         522            2,805   (libdb 50% of its own peak)
#	     96         337            3,308   (libdb 32% of its own peak)
#
# WiredTiger rises monotonically.  At t=1 the engines are equal, which is what
# makes this a CONCURRENCY defect rather than a throughput deficit.
#
# WHY NO EXISTING GATE COULD CATCH IT
#
#   * All 34 CI jobs run on ubuntu-latest, a 2-4 vCPU shared runner.  The defect
#     first appears ABOVE 8 threads.  A 2-core runner cannot produce the
#     contention, so the defect is STRUCTURALLY invisible there.
#   * The one perf job (bench.yml) is `continue-on-error: true` and labelled
#     "informational only", so a fall from 1,041 to 337 tpm is reported by
#     nothing.
#
# WHAT THIS GATE ASSERTS: SHAPE, NOT ABSOLUTE THROUGHPUT
#
#	tpm(t=HI) >= tpm(t=LO) - tolerance          [default LO=8, HI=32]
#
# A monotonicity property.  That choice is deliberate and is the reason this
# gate can exist at all: an absolute threshold has to be re-tuned for every
# machine class, compiler and kernel, and gets widened until it means nothing.
# A SHAPE comparison is between two measurements taken minutes apart on the SAME
# box in the SAME build, so machine class cancels out.
#
# THE TOLERANCE IS MEASURED, NOT TYPED IN.  It comes from the reps' own
# coefficient of variation via test/bench/bench_cmp.py (TOL_SIGMA * CV, floored
# at TOL_FLOOR_PCT) -- the same statistics module the rest of test/bench uses,
# which has its own self-test in test_bench_cmp.py.  Tightening the gate
# therefore requires producing a QUIETER baseline, not editing a number.
#
# ARMS ALTERNATE ACROSS REPS (t=8, t=32, t=96, t=8, t=32, t=96, ...) rather than
# blocking, so machine drift -- a warming box, a neighbour, climbing SSD write
# amplification -- is spread across the thread counts instead of being
# attributed entirely to whichever ran during it.  Results STREAM to the TSV as
# they complete: a sibling agent buffered five reps remotely, lost the instance,
# and lost four of them.
#
# TEETH, BOTH DIRECTIONS (--self-test).  A gate that only ever fails is
# indistinguishable from a broken gate, so --self-test feeds the SAME verdict
# logic two synthetic series and requires opposite answers:
#     descending (the measured libdb shape)   MUST FAIL
#     ascending  (the measured WiredTiger shape) MUST PASS
# The workflow runs --self-test before the real measurement, so a gate that has
# stopped discriminating is caught before its verdict is believed.
#
# Usage:
#   ./scale_shape_gate.sh --self-test              # teeth, no benchmark, seconds
#   ./scale_shape_gate.sh --check RESULTS.tsv      # verdict on an existing TSV
#   ./scale_shape_gate.sh [-r REPS] [-s SECS] [-n "8 32 96"] [-o OUT] [-S SCALE]
#   ./scale_shape_gate.sh --noise RESULTS.tsv      # the measured noise floor
#
# Env: BUILD (libdb build tree, default ../../build_unix), BIN, DATA
#
# THIS NEEDS 32+ CORES.  On a 2-4 vCPU runner t=32 and t=8 both measure
# oversubscription and the comparison is meaningless, so the script REFUSES to
# produce a verdict below MIN_CORES rather than emitting a number nobody should
# trust.

set -u

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
TOP=$(CDPATH= cd -- "$HERE/../.." && pwd)
BUILD=${BUILD:-"$TOP/build_unix"}
BIN=${BIN:-"$HERE"}
DATA=${DATA:-"${SHAPE_DATA:-/tmp/shape_data}"}
CMP="$HERE/bench_cmp.py"

REPS=5
SECS=30
WARMUP=10
# PREWARM: one DISCARDED pass over every thread count before the measured reps.
#
# This is not decoration, it was MEASURED into existence.  TPROC-C MUTATES its
# dataset (new-order inserts rows, delivery deletes them), so a freshly loaded
# database is not in steady state and the first passes run against a smaller,
# tidier tree.  Without a prewarm pass the t=8 arm measured
#
#     rep1 282211, rep2 240373, rep3 212493, rep4 193171, rep5 185604 tpm
#
# -- a monotone 34% decline across reps, i.e. cv 16.6%, which is ABOVE
# bench_cmp.py's 10% usability ceiling.  The gate correctly REFUSED to emit a
# verdict on it ("a gate that cannot distinguish signal from its own noise is
# worse than no gate") rather than reporting the -11.6% delta it had computed.
#
# Widening the tolerance to cover that spread would have been the wrong repair:
# it would have hidden the very defect the gate exists for, since the defect
# itself is a throughput decline.  The right repair is a QUIETER BASELINE, which
# is also the only way bench_cmp.py allows a tolerance to be tightened.
PREWARM=1
THREADS="8 32 96"
SCALE=96
PAD=256
CACHE_BYTES=$((4 * 1024 * 1024 * 1024))
OUT=""
MODE=run
CHECKFILE=""
LO=8
HI=32
MIN_CORES=32
TIMEOUT_PAD=600

while [ $# -gt 0 ]; do
	case $1 in
	--self-test)	MODE=selftest ;;
	--check)	MODE=check; shift; CHECKFILE=$1 ;;
	--noise)	MODE=noise; shift; CHECKFILE=$1 ;;
	-r) shift; REPS=$1 ;;
	-s) shift; SECS=$1 ;;
	-W) shift; WARMUP=$1 ;;
	-n) shift; THREADS=$1 ;;
	-o) shift; OUT=$1 ;;
	-S) shift; SCALE=$1 ;;
	-P) shift; PAD=$1 ;;
	--lo) shift; LO=$1 ;;
	--hi) shift; HI=$1 ;;
	--no-prewarm) PREWARM=0 ;;
	--min-cores) shift; MIN_CORES=$1 ;;
	-h|--help) sed -n '2,70p' "$0"; exit 0 ;;
	*) echo "unknown option: $1" >&2; exit 2 ;;
	esac
	shift
done

[ -f "$CMP" ] || { echo "no $CMP -- the statistics live there, not here" >&2
	exit 2; }

# Verdict emission for the test-execution manifest gate (test/MANIFEST, tier
# `shape`).  Sourced, not executed: see test/harness.sh.  Without this a run
# that produced no verdict would be indistinguishable from one that passed.
. "$TOP/test/harness.sh"
hi_init shape "$TOP/test"

# ---------------------------------------------------------------------------
# verdict_from_tsv FILE -- THE GATE.
#
# Reads the TSV, takes the MEDIAN tpmC_like per thread count, derives the
# tolerance from the reps' own spread through bench_cmp.py --noise, and asserts
# monotonicity between LO and HI.
#
# Every refusal below is deliberate.  A gate that emits a verdict from one rep,
# or from a thread count that produced no rows, is worse than no gate: it
# launders an unmeasured claim into a green check.  This project has NINE
# recorded vacuous-green instances, one of them in a perf measurement.
# ---------------------------------------------------------------------------
verdict_from_tsv() {
	f=$1
	echo "== scaling-shape gate =="
	echo "results: $f"
	echo "assertion: median tpm(t) >= median tpm(t=$LO) - tolerance," \
	    "for every t > $LO (reference $LO, headline $HI)"
	echo

	# The noise report is the tolerance's provenance -- printed, so the
	# number in the verdict can be traced to the spread it came from.
	echo "-- measured noise floor (bench_cmp.py --noise) --"
	python3 "$CMP" --noise "$f" 2>&1 | sed 's/^/   /'
	echo

	python3 - "$HERE" "$f" "$LO" "$HI" <<'PYEOF'
import sys

# bench_cmp.py is the project's statistics module: same TOL_SIGMA, same
# TOL_FLOOR_PCT, same CV_EXCLUDE_PCT, and it has its own self-test
# (test/bench/test_bench_cmp.py).  Re-deriving a tolerance here would be a
# second, untested copy of the one thing that must not be guessed.
#
# argv[1] is test/bench/, passed in because this script is fed to python on
# STDIN, which makes __file__ and argv[0] useless for locating a sibling module.
# The FIRST version of this used argv[0] and the import raised
# ModuleNotFoundError -- which exited non-zero, which the caller read as "the
# gate failed the descending series".  A crash impersonating a verdict.  The
# --self-test ascending arm is what caught it, which is the entire argument for
# demonstrating teeth in BOTH directions.
sys.path.insert(0, sys.argv[1])
from bench_cmp import TOL_SIGMA, TOL_FLOOR_PCT, CV_EXCLUDE_PCT, stats

path, lo, hi = sys.argv[2], int(sys.argv[3]), int(sys.argv[4])

# Collect tpmC_like per thread count.  Anything that is not a complete data row
# is ignored here and COUNTED, so a run full of TIMEOUT_STALL / NO_VERDICT rows
# cannot quietly become a small clean sample.
by_t, bad = {}, 0
hdr = None
with open(path) as fh:
    for line in fh:
        line = line.rstrip("\n")
        if not line or line.startswith("#"):
            continue
        cols = line.split("\t")
        if hdr is None:
            hdr = cols
            continue
        if len(cols) != len(hdr):
            continue
        r = dict(zip(hdr, cols))
        m = r.get("metric", "")
        if m in ("TIMEOUT_STALL", "NO_VERDICT", "FAILED_RUN"):
            bad += 1
            continue
        if m != "tpmC_like":
            continue
        try:
            by_t.setdefault(int(r["threads"]), []).append(float(r["value"]))
        except (ValueError, KeyError):
            continue

if bad:
    print("WARNING: %d run(s) recorded a stall or produced no verdict; those "
          "are NOT counted as data." % bad)

for t in sorted(by_t):
    lo_v, med, hi_v, cv = stats(by_t[t])
    print("t=%-3d n=%d  min=%.0f  median=%.0f  max=%.0f  cv=%.2f%%"
          % (t, len(by_t[t]), lo_v, med, hi_v, cv))
print()

# --- refusals, before any verdict -----------------------------------------
if lo not in by_t or hi not in by_t:
    print("GATE ERROR: no data for t=%d and/or t=%d -- the gate cannot be "
          "evaluated.  A missing arm is NOT a pass." % (lo, hi))
    sys.exit(2)
for t in (lo, hi):
    if len(by_t[t]) < 2:
        print("GATE ERROR: t=%d has %d rep(s).  A tolerance derived from a "
              "single sample is not a measurement; use -r 5 or more."
              % (t, len(by_t[t])))
        sys.exit(2)

_, med_lo, _, cv_lo = stats(by_t[lo])
_, med_hi, _, cv_hi = stats(by_t[hi])

if med_lo <= 0:
    print("GATE ERROR: median tpm at t=%d is %.1f -- the baseline arm "
          "measured nothing, so the ratio is meaningless." % (lo, med_lo))
    sys.exit(2)

# The tolerance comes from the LO (reference) arm's own spread, exactly as
# bench_cmp.cmd_compare derives it from the baseline file's spread.
tol_pct = max(TOL_FLOOR_PCT, TOL_SIGMA * cv_lo)
delta_pct = 100.0 * (med_hi - med_lo) / med_lo


# ---------------------------------------------------------------------------
# THE VERDICT.  Monotonicity is asserted from LO against EVERY higher thread
# count present, not only against HI.
#
# This is a MEASURED correction, not extra thoroughness.  On the c6id.24xlarge
# the t=8 -> t=32 step is a mild -8% (inside a tolerance derived from a quiet
# baseline) while t=8 -> t=96 is -68%.  A gate that looked only at t=32 would
# have reported PASS on a machine where throughput collapses to a third of peak
# by 96 threads -- the exact defect it was built for.  The worst step is what
# decides the verdict; every step is printed either way.
# ---------------------------------------------------------------------------
steps, worst = [], None
for t in sorted(by_t):
    if t <= lo:
        continue
    if len(by_t[t]) < 2:
        print("note: t=%d has %d rep(s), not enough to judge; reported, not "
              "gated." % (t, len(by_t[t])))
        continue
    _, med_t, _, _ = stats(by_t[t])
    d = 100.0 * (med_t - med_lo) / med_lo
    steps.append((t, med_t, d))
    if worst is None or d < worst[2]:
        worst = (t, med_t, d)

if not steps:
    print("GATE ERROR: no thread count above t=%d has enough reps to judge. "
          "A gate with one point is not a shape gate." % lo)
    sys.exit(2)

print("tolerance: max(%.1f%% floor, %.1f x cv(t=%d)=%.2f%%) = %.1f%%"
      % (TOL_FLOOR_PCT, TOL_SIGMA, lo, cv_lo, tol_pct))
for t, med_t, d in steps:
    print("step:      tpm(t=%-3d)=%-9.0f vs tpm(t=%d)=%-9.0f  =  %+7.1f%%   %s"
          % (t, med_t, lo, med_lo, d,
             "BELOW TOLERANCE" if d < -tol_pct else "ok"))

# A reference arm noisier than CV_EXCLUDE_PCT cannot support ANY verdict; say so
# rather than gating at a tolerance so wide it would never fire.
if cv_lo > CV_EXCLUDE_PCT:
    print()
    print("GATE ERROR: t=%d cv is %.1f%%, above the %.1f%% usability ceiling. "
          "A gate that cannot distinguish signal from its own noise is worse "
          "than no gate.  Quiet the machine (--prewarm is on by default) or "
          "raise the rep count.  Do NOT widen the tolerance: the defect this "
          "gate looks for IS a throughput decline, so a tolerance wide enough "
          "to cover this noise would hide it."
          % (lo, cv_lo, CV_EXCLUDE_PCT))
    sys.exit(2)

fails = [s for s in steps if s[2] < -tol_pct]
print()
if fails:
    t_w, med_w, d_w = worst
    print("VERDICT scale-shape FAIL threads_lo=%d threads_hi=%d "
          "tpm_lo=%.0f tpm_hi=%.0f delta_pct=%+.1f tol_pct=%.1f "
          "failing_steps=%s"
          % (lo, t_w, med_lo, med_w, d_w, tol_pct,
             ",".join("t%d:%+.1f%%" % (t, d) for t, _, d in fails)))
    print()
    print("NEGATIVE SCALING: throughput FELL by %.1f%% going from %d to %d "
          "threads, beyond the %.1f%% tolerance this machine's own rep-to-rep "
          "spread supports." % (-d_w, lo, t_w, tol_pct))
    print("This is the G12 defect: libdb peaks at 8 threads and declines, "
          "while WiredTiger rises monotonically to 96.  The mechanism is P1, "
          "the PGNO_BASE_MD allocation convoy (__db_new holds the metadata "
          "page write-locked until commit, across its own fsync).")
    print("See rfc/0011-test-coverage-gaps.md and "
          "test/bench/BTREE-LOCK-SCOPE-2026-09.md.")
    sys.exit(1)

t_w, med_w, d_w = worst
print("VERDICT scale-shape PASS threads_lo=%d threads_hi=%d "
      "tpm_lo=%.0f tpm_hi=%.0f delta_pct=%+.1f tol_pct=%.1f"
      % (lo, t_w, med_lo, med_w, d_w, tol_pct))
sys.exit(0)
PYEOF
	return $?
}

# ---------------------------------------------------------------------------
# --self-test: THE TEETH, both directions, with no benchmark run.
#
# Two synthetic series through the SAME verdict_from_tsv above.  Numbers are the
# MEASURED ones from TPROC-XENGINE-2026-09.md, so this is not an abstract shape
# test -- it is "would this gate have caught the real defect, and would it have
# passed the real competitor".
# ---------------------------------------------------------------------------
self_test() {
	d=$(mktemp -d) || exit 2
	trap 'rm -f "$d"/*.tsv; rmdir "$d" 2>/dev/null' 0 1 2 3 13 15
	strc=0

	# Small deterministic jitter so cv > 0 and the tolerance is exercised
	# rather than collapsing onto the floor.
	mk() {
		file=$1; shift
		{
		  printf '# synthetic\tscale_shape_gate self-test\n'
		  printf 'benchmark\tconfig\tthreads\trep\tmetric\tunit\tvalue\n'
		  for spec in "$@"; do
			t=${spec%%:*}; base=${spec##*:}
			r=1
			while [ "$r" -le 5 ]; do
				# +/- ~2% deterministic wobble
				v=$(awk -v b="$base" -v r="$r" \
				    'BEGIN{printf "%.0f", b * (1 + ((r%3)-1)*0.02)}')
				printf 'tproc-c\tlibdb\t%s\t%d\ttpmC_like\ttpm\t%s\n' \
				    "$t" "$r" "$v"
				r=$((r + 1))
			done
		  done
		} > "$file"
	}

	echo "###########################################################"
	echo "# TEETH 1/2 -- the MEASURED libdb shape MUST FAIL the gate."
	echo "#   t=8 1041 tpm, t=32 522 tpm, t=96 337 tpm"
	echo "#   (TPROC-XENGINE-2026-09.md, libdb-sync-btree)"
	echo "###########################################################"
	mk "$d/descending.tsv" 8:1041 32:522 96:337
	if verdict_from_tsv "$d/descending.tsv"; then
		echo
		echo "TEETH FAILURE: the gate PASSED the measured negative-scaling"
		echo "shape.  It cannot catch the defect it exists for."
		strc=1
	else
		echo
		echo "OK: descending shape FAILED the gate, as required."
	fi

	echo
	echo "###########################################################"
	echo "# TEETH 2/2 -- the MEASURED WiredTiger shape MUST PASS."
	echo "#   t=8 1455 tpm, t=32 2805 tpm, t=96 3308 tpm"
	echo "#   (same run, same box, same workload -- wt-btree)"
	echo "###########################################################"
	mk "$d/ascending.tsv" 8:1455 32:2805 96:3308
	if verdict_from_tsv "$d/ascending.tsv"; then
		echo
		echo "OK: ascending shape PASSED the gate, as required."
	else
		echo
		echo "TEETH FAILURE: the gate FAILED a monotonically RISING shape."
		echo "It would fail every engine including a correct one, which"
		echo "makes it indistinguishable from a broken gate."
		strc=1
	fi

	echo
	if [ "$strc" = 0 ]; then
		echo "SELFTEST PASS -- the gate discriminates in BOTH directions:"
		echo "  fails the measured libdb shape, passes the measured WT shape."
	else
		echo "SELFTEST FAIL -- see above."
	fi
	return "$strc"
}

case $MODE in
selftest)
	self_test
	strc=$?
	[ "$strc" = 0 ] && hi_emit selftest pass || hi_emit selftest fail
	exit "$strc" ;;
noise)		python3 "$CMP" --noise "$CHECKFILE"; exit $? ;;
check)
	verdict_from_tsv "$CHECKFILE"
	crc=$?
	# Exit 2 is a GATE ERROR (missing arm, single rep, unusable noise): it is
	# recorded as `fail`, not `skip`.  An unevaluable gate is not a pass.
	case $crc in
	0) hi_emit gate pass ;;
	*) hi_emit gate fail ;;
	esac
	exit "$crc" ;;
esac

# ===========================================================================
# MEASURE.
# ===========================================================================
NCORES=$( (nproc 2>/dev/null || echo 1) )
echo "== scaling-shape measurement =="
echo "cores: $NCORES   threads: $THREADS   reps: $REPS   secs: $SECS"

# Refuse rather than mislead.  On a GitHub-hosted runner (2-4 vCPU) t=8 and
# t=32 both measure oversubscription; the difference between them says nothing
# about libdb's concurrency and everything about the scheduler.
if [ "$NCORES" -lt "$MIN_CORES" ]; then
	echo
	echo "REFUSING TO MEASURE: this machine has $NCORES cores, the gate needs" \
	    "$MIN_CORES+."
	echo "Below that, t=$LO and t=$HI both measure oversubscription, so the"
	echo "comparison would describe the SCHEDULER rather than libdb.  Emitting"
	echo "a number here would be worse than emitting none: it would look like"
	echo "evidence.  Run this on a self-hosted or on-demand 32+ core box."
	hi_emit gate skip
	exit 3
fi

DRIVER="$BIN/xe_tproc_c"
if [ ! -x "$DRIVER" ]; then
	echo "building xe_tproc_c from $BUILD"
	lib=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
	# A wildcard cannot name the WRONG library.  A hardcoded -ldb-5.3 once
	# resolved to Debian's 2013 Berkeley DB behind a benchmark's back and
	# every published number described Oracle's code, not this fork.
	[ -n "$lib" ] || {
		echo "no $BUILD/.libs/libdb-*.so -- build libdb first." >&2
		echo "Refusing to fall back to a system libdb." >&2
		exit 2
	}
	${CC:-cc} -O2 -pthread -I"$BUILD" -I"$HERE" "$HERE/xe_tproc_c.c" \
	    "$lib" -Wl,-rpath,"$(cd "$(dirname "$lib")" && pwd)" -lm \
	    -o "$DRIVER" || exit 2
fi

OUT=${OUT:-"$HERE/results/scale_shape.tsv"}
mkdir -p "$(dirname "$OUT")" "$DATA" || exit 2

{
	printf '# libdb-scale-shape\tv1\n'
	printf '# generated\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
	printf '# git_rev\t%s\n' "$(cd "$TOP" && git rev-parse --short HEAD \
	    2>/dev/null || echo unknown)"
	printf '# hardware\t%s\n' "$(cat /sys/devices/virtual/dmi/id/product_name \
	    2>/dev/null || uname -m)"
	printf '# vcpus\t%s\n' "$NCORES"
	printf '# cpu_model\t%s\n' "$(sed -n 's/^model name[[:space:]]*:[[:space:]]*//p' \
	    /proc/cpuinfo | head -1)"
	printf '# kernel\t%s\n' "$(uname -sr)"
	printf '# secs\t%s\n' "$SECS"
	printf '# warmup\t%s\n' "$WARMUP"
	printf '# tproc_scale\t%s\n' "$SCALE"
	printf '# pad\t%s\n' "$PAD"
	printf '# cache_bytes\t%s\n' "$CACHE_BYTES"
	printf '# reps\t%s\n' "$REPS"
	printf '# prewarm\t%s\n' "$PREWARM"
	printf 'benchmark\tconfig\tthreads\trep\tmetric\tunit\tvalue\n'
} > "$OUT"

emit() { printf '%s\n' "$1" | tee -a "$OUT"; }

# ---- load the dataset once ------------------------------------------------
if [ ! -f "$DATA/.loaded_S$SCALE" ]; then
	echo "=== loading dataset scale=$SCALE pad=$PAD into $DATA"
	mkdir -p "$DATA/d"
	find "$DATA/d" -mindepth 1 -delete 2>/dev/null
	if ! timeout 3600 "$DRIVER" -i -e libdb -a btree -h "$DATA/d" \
	    -S "$SCALE" -P "$PAD" -c "$CACHE_BYTES" 2>&1 |
	    grep -E 'VERDICT load|FAIL'; then
		echo "LOAD FAILED -- refusing to report throughput on a dataset"
		echo "that did not load."
		exit 2
	fi
	touch "$DATA/.loaded_S$SCALE"
else
	echo "=== dataset already loaded in $DATA"
fi

# ---- one run; $1 = threads, $2 = rep label, $3 = 1 to RECORD, 0 to discard --
cap=$((SECS + WARMUP + TIMEOUT_PAD))
one_run() {
	t=$1; rep=$2; record=$3
	log="$(dirname "$OUT")/shape_t${t}_r${rep}.log"
	rc=0
	timeout -s KILL "$cap" "$DRIVER" -e libdb -a btree \
	    -h "$DATA/d" -S "$SCALE" -P "$PAD" -c "$CACHE_BYTES" \
	    -t "$t" -s "$SECS" -W "$WARMUP" >"$log" 2>&1 || rc=$?

	line=$(grep -m1 '^VERDICT tproc-c' "$log")
	if [ "$record" != 1 ]; then
		# The discarded pass still has to be REPORTED.  A prewarm that
		# silently died would leave the dataset un-warmed and the
		# measured reps would carry the drift this exists to remove.
		if [ -z "$line" ]; then
			echo "### PREWARM t=$t produced no verdict (rc=$rc) -- see $log"
			prewarm_bad=1
		else
			echo "    prewarm t=$t: $(printf '%s\n' "$line" |
			    sed -n 's/.*tpmC_like=\([0-9.]*\).*/\1/p') tpm (DISCARDED)"
		fi
		return 0
	fi

	if [ "$rc" = 124 ] || [ "$rc" = 137 ]; then
		# A stall is DATA, recorded as such -- never discarded, and never
		# counted as a throughput sample.
		emit "$(printf 'tproc-c\tlibdb\t%s\t%s\tTIMEOUT_STALL\ts\t%d' \
		    "$t" "$rep" "$cap")"
		echo "### STALL t=$t rep=$rep -- see $log"
		return 0
	fi
	if [ -z "$line" ]; then
		emit "$(printf 'tproc-c\tlibdb\t%s\t%s\tNO_VERDICT\tn\t0' \
		    "$t" "$rep")"
		echo "### NO VERDICT t=$t rep=$rep rc=$rc"
		grep -E '^FAIL|error' "$log" | head -3 | sed 's/^/###   /'
		return 0
	fi
	for k in tpmC_like txn_per_sec committed; do
		v=$(printf '%s\n' "$line" |
		    sed -n "s/.*[[:space:]]$k=\([0-9.]*\).*/\1/p")
		[ -n "$v" ] || continue
		u=tpm
		[ "$k" = txn_per_sec ] && u=txn/s
		[ "$k" = committed ] && u=txns
		emit "$(printf 'tproc-c\tlibdb\t%s\t%s\t%s\t%s\t%s' \
		    "$t" "$rep" "$k" "$u" "$v")"
	done
}

# ---- the DISCARDED prewarm pass -------------------------------------------
prewarm_bad=0
if [ "$PREWARM" = 1 ]; then
	echo "=== prewarm pass (DISCARDED -- brings the mutated dataset to" \
	    "steady state; see the PREWARM comment at the top)"
	for t in $THREADS; do
		one_run "$t" pw 0
	done
	if [ "$prewarm_bad" = 1 ]; then
		echo "### the prewarm pass did not complete; the measured reps" \
		    "would carry fresh-dataset drift.  Refusing to continue."
		hi_emit gate fail
		exit 2
	fi
	echo
fi

# ---- alternate the thread counts within each rep -------------------------
rep=1
while [ "$rep" -le "$REPS" ]; do
	for t in $THREADS; do
		echo "=== rep $rep t=$t"
		one_run "$t" "$rep" 1
	done
	rep=$((rep + 1))
done

echo
verdict_from_tsv "$OUT"
grc=$?
case $grc in
0) hi_emit gate pass ;;
*) hi_emit gate fail ;;
esac
exit "$grc"

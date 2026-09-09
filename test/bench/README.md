# libdb microbenchmark suite and performance-regression harness

Two things live here:

* **the drivers** — `scale_bench`, `scale_iso`, `lock_bench`,
  `ssi_abort_bench`, `tproc_b`, `tproc_c`, `tproc_h`: standalone C programs
  that each drive one aspect of libdb from N threads and print throughput.
  They are exploratory tools; run them by hand with whatever arguments the
  question needs.
* **the harness** — `run_bench.sh` (measure) and `bench_cmp.py` (compare and
  gate). The harness runs a *fixed* subset of what the drivers can do, so that
  two runs of it are comparable. This is the measurement gate that has to
  exist before any scalability claim is believable.

Nothing here is a TPC benchmark. `tproc_*` are HammerDB-*style* workloads,
independently implemented, producing no TPC-comparable numbers.

## Quick start

```sh
# build libdb first (the harness will do it if the tree is unconfigured)
cd ../../build_unix && ../dist/configure --enable-o_direct && make -j"$(nproc)"

cd ../test/bench
./run_bench.sh -q -o /tmp/smoke.tsv          # ~1 min sanity check
./run_bench.sh -r 5 -o /tmp/mine.tsv         # ~40 min real measurement
./bench_cmp.py --noise /tmp/mine.tsv         # this machine's noise floor
./bench_cmp.py baseline-c7i.24xlarge.tsv /tmp/mine.tsv   # the gate
echo $?                                       # 0 = no regression, 1 = regression
```

The drivers link the build tree's libdb via `-rpath`. That is not cosmetic:
without it the loader silently picks up any system-installed `libdb-5.3`, and
every number then describes *that* library. `run_bench.sh` re-checks each
binary's resolved `libdb-5.3` with `ldd` and refuses to publish results if it
is not the tree under test.

Options: `-r REPS` (default 5), `-s SECS` per measured window (default 10),
`-n "1 8 32"` thread sweep, `-b DIR` libdb build tree, `-q` quick smoke
matrix, `-o FILE` output. `BENCH_HW` overrides the recorded hardware label
(auto-detected from EC2 IMDS); `BENCH_SCRATCH` moves the scratch databases.

`python3 test_bench_cmp.py` self-checks the comparison tool, including that it
fails on a synthetic 25% slowdown. No dependencies beyond python3 stdlib.

## Output format

TSV. `#`-comment lines carry provenance (git rev, libdb version, hardware, CPU
model, vCPUs, kernel, compiler, build flags, seed, record counts, load average
at start and end). Data rows are:

```
benchmark  config  threads  metric  unit  rep  value
lock_bench distinct 32      ops_per_sec ops/s 3  4812004
```

One row per rep — the file holds raw samples, not summaries. All aggregation
happens in `bench_cmp.py`, so the same file serves as both a baseline and a
noise-floor input, and a later change to how spread is computed does not
invalidate stored measurements.

## The fixed configuration

Reproducibility comes from pinning everything the drivers accept: seed 42
(every driver taking `-R`), 100 000 records for `scale_bench`/`scale_iso`,
1024 / 64 lock objects for `lock_bench` distinct / shared, 64 hot keys for
`ssi_abort_bench`, scale 1 for the `tproc_*` drivers, a 10 s measured window,
and thread counts `1 8 32 96` (`1 8 32` for `tproc_*`, whose populate step
dominates). Datasets are sized to fit each driver's own cache setting: the
gate is a CPU-and-synchronisation measurement, not a storage one — see "not
covered".

`tproc_*` databases are repopulated before *every* measured run, because those
workloads mutate rows and an inherited database is a different starting state.

Each driver invocation is `timeout`-wrapped at the measured window plus 900 s.
That ceiling is deliberately far above any observed runtime: it exists only to
stop a wedged run, and a tight timeout is precisely how timing checks start
flaking (this project has shipped two thresholds set from a single measurement
and had both fail release qualification).

## How to read a delta

```
case                    metric      base_median base_spread     new_median new_spread      delta_pct tol_pct verdict
lock_bench/distinct/t32 ops_per_sec 4812004     4735001..4890210 4801119   4700855..4855002 -0.2     5.0     pass
scale_bench/wrand/t96   ops_per_sec 103864      99120..108440    88112     86400..90100     -15.2    8.7     FAIL
tproc_h/S1/t32          queries_per_sec 0.5     0.5..0.5         0.5       0.5..0.5         +0.0     -       SKIP
```

* `delta_pct` is the change in **medians**, candidate vs baseline. Higher is
  better for every metric here, so a regression is always a negative delta.
* `base_spread` / `new_spread` are min..max over the reps. **Always read the
  spread.** A -6% delta between two runs whose spreads overlap heavily is not
  a finding; the same delta between two tight non-overlapping spreads is.
* `tol_pct` is derived per case from the *baseline's own* rep-to-rep spread
  (see below) — it is not a global constant.
* `SKIP` means the case is measured and printed but excluded from the verdict.
* `FAIL` on any gated case exits 1. `MISSING` / `NEW` cases are reported and
  do not affect the verdict.

Two runs of *identical code* should produce all `pass`. If they do not, the
machine is too noisy for the gate — check the `loadavg` provenance lines
before believing any verdict.

## The tolerance, and why it is this number

The tolerance is computed, not chosen:

```
tolerance_pct = max(5.0, 3.0 * CV_baseline_case)
```

where `CV_baseline_case` is the coefficient of variation of that exact case's
reps *in the baseline file*. Consequences worth stating plainly:

* **It cannot be re-tightened by editing a constant.** A tighter gate requires
  a quieter baseline — which is the only honest way to tighten a threshold.
* `3.0 * CV` is the ordinary 3-sigma convention. With >= 5 reps the median's
  own sampling spread is well under one CV, so 3 CV absorbs a slower day
  without hiding a double-digit regression.
* The **5% floor** exists because a very quiet case (CV ~0.5%) would otherwise
  imply a 1.5% gate, and 1.5% is below the reproducibility of a *recompile* —
  code layout and linker order alone move these numbers by a few percent.
* Cases whose baseline CV exceeds **10%** are excluded from the verdict rather
  than gated at a 45%+ tolerance. A check that cannot resolve a real
  regression is worse than no check: it produces confident `pass` verdicts
  that mean nothing, and a gate that cries wolf gets switched off. Measured
  evidence for this specific threshold: across two runs of *identical* code the
  three cases above it moved -25.5%, -7.7% and -22.6%.
* Cases whose baseline median is below **100** are excluded whatever their CV,
  because resolution rather than stability limits them. `tproc_h`'s queries/s
  at 32 threads has median 0.5 and CV 0.00% — 5 completed queries in a 10 s
  window, where one query either way moves it 20%. The same workload's
  rows/s metric has the resolution to gate, and is gated.

Excluded cases still print, so their numbers remain visible for manual reading.

The measured noise floor these numbers were chosen against is in `NOISE.md`,
reproduced next to the tolerance so nobody re-tightens it by guess, and the
proof that the gate neither false-positives nor sleeps through a real slowdown
is in `GATE-VERIFICATION.md`. `--tolerance`, `--tolerance-floor`,
`--cv-exclude` and `--exclude` override the defaults; use them to reproduce a
specific verdict, not to make a red run green.

## Baseline provenance

`baseline-c7i.24xlarge.tsv` is committed with its full provenance header:
instance type, CPU model, vCPU count, NUMA node count, kernel, compiler,
libdb version and git rev, benchmark `CFLAGS`, libdb `configure` flags, seed
and dataset sizes, and the load average at start and end. **A baseline without
provenance is useless** — a delta measured against a different machine, build
type or dataset size is not a regression signal, and `bench_cmp.py` prints a
warning when the two files disagree on any of those.

The committed baseline is *not* a target for other hardware. On a different
machine, measure your own with `-r 5`, check `--noise`, and compare against
that.

## CI wiring: which mode is authoritative

`.github/workflows/bench.yml` has two modes.

* **Advisory (`push` / `pull_request`, GitHub-hosted runner).** Builds the
  drivers, runs `-q`, runs the `bench_cmp.py` self-check, and uploads results.
  `continue-on-error: true`; it never gates a merge. Shared runners have 2–4
  noisy vCPUs, cannot exercise a 96-thread sweep, and their neighbour load is
  invisible. **Numbers from this mode are not comparable to the committed
  baseline and must not be quoted as measurements.** Its only job is to prove
  the harness still builds and runs.
* **Authoritative (`workflow_dispatch` on a self-hosted `[self-hosted, bench]`
  runner).** Full matrix, `-r 5`, compared against the committed baseline with
  the real verdict. This is the mode whose PASS/FAIL means anything.

There is deliberately **no per-push performance gate**. A flaky gate gets
disabled, and a disabled gate is worth less than none.

## What is NOT covered

Read this before treating a green verdict as "no performance regression".

* **Storage/IO.** Every configuration is sized to fit in cache. Nothing here
  measures eviction, checkpoint cost, log-write throughput at `-d sync`, or
  any disk-bound path. The drivers support `-d sync|wnosync`; the harness runs
  only the `nosync` default.
* **Latency.** Throughput only — no p50/p99/p999. A change that halves tail
  latency, or doubles it at constant throughput, is invisible here.
* **Multi-process.** All drivers are multi-*threaded* in one process. Nothing
  exercises cross-process region contention.
* **Replication / HA.** No `DB_ENV->repmgr` path is touched.
* **Recovery and checkpoint.** No measurement of recovery time or checkpoint
  stalls.
* **Access methods other than BTREE/RECNO.** No hash, no queue.
* **Memory footprint / RSS.** Not recorded.
* **NUMA effects.** The committed baseline is a single-NUMA-node instance.
  Cross-socket behaviour is unmeasured.
* **YCSB and HammerDB proper.** Out of scope for this harness. `tproc_*` are
  independent HammerDB-*style* workloads and are not a substitute; wiring the
  real suites is separate future work.
* **`scale_iso snap` / long-lived snapshot readers.** Excluded — see below.
* **The CPU governor is not pinned.** `cpupower` reports no cpufreq driver on
  this instance family; frequency is host-managed, so turbo variance is part of
  the measured spread rather than eliminated from it.
* **Absolute performance vs other engines.** Nothing here compares libdb to
  InnoDB, WiredTiger or LMDB. The harness answers "did *this* tree get slower
  than *that* tree on the same machine", and only that.

## Excluded from the gate

`NOISE.md` has the measured CVs and the full exclusion list. Exclusions are
data-driven, not hand-maintained: `bench_cmp.py` drops any case whose baseline
CV exceeds 10% or whose median is below 100, so the list moves with the
hardware. On the committed baseline that is 6 of 40 cases —
`lock_bench/shared` at t32 and t96, `scale_bench/rhot/t32`, and all three
`tproc_h` `queries_per_sec` cases.

One workload is excluded from the matrix entirely rather than by CV:
**`scale_iso snap`**. Under this fork `DB_TXN_SNAPSHOT` is serializable (SSI),
so that driver's one long-lived read-only snapshot transaction per thread
accumulates a SIREAD marker per page read for the whole window and exhausts the
lock region (`BDB2055` / `ENOMEM`) partway through — at 500 000 lock entries as
well as at the default ~1000. A worker that hits the error retires, so the
driver still prints a throughput number, but it is the throughput of a
shrinking thread pool: measured 1075 ops/s at 8 threads against 2.25M ops/s for
a run that completes. Run it by hand with a short `-s` for exploration.

The harness records every driver's stderr as a `# driver_stderr` provenance
line and `bench_cmp.py` warns when a results file contains any, precisely
because a driver that logs errors may be reporting a degraded run as a number.

# Closing the gates the performance run exposed

`docs/design/perf-gate-gaps.md` identified four structural gaps that let three
defects through 34 CI jobs. This is what was built to close them, what each
gate asserts, and — the part that matters — how each gate was shown to have
teeth in **both** directions.

Everything here was measured on a **c6id.24xlarge**: 96 vCPU (Intel Xeon
Platinum 8375C @ 2.90 GHz), 185 GiB RAM, 5.2 TB striped local NVMe (xfs),
Debian 12, kernel 6.1.0-53, gcc 12.2.0, Python 3.11.2. Nothing else ran on the
box.

## Summary

| Gap | What was built | Teeth shown | Verdict on master |
|---|---|---|---|
| G15 | `test/c/flag_behaviour.c` + `flag-run.sh` (12 verdicts) | `FLAGB_STRICT=1` fails; default passes | **2 XFAIL** (P2 + a new sibling), 10 pass |
| G14 | `test/config/option_sweep.sh` + `config_smoke.c` | 33 legs run; removed features SKIP, not pass | **1 real build failure** |
| G12/G13 | `test/bench/scale_shape_gate.sh` + baseline | `--self-test`: measured libdb shape FAILS, measured WT shape PASSES | **FAILS: -68.2% at t=96** |

Three new tiers in `test/MANIFEST` (`flag`, `config`, `shape`), two workflows.
No `src/` changes. `sizeof(DB/DBC/DB_ENV/DB_TXN)` and `__env_struct_sig()`
untouched; `dist/s_execbits` and `test/check_manifest.sh` pass.

---

## G15 — behaviour tests for the six untested runtime flags

### The concealment that made this necessary

`test/c/cov_api_surface.c` contained:

```c
(void)dbenv->set_flags(dbenv, DB_DIRECT_DB, 1);
(void)dbenv->set_flags(dbenv, DB_DIRECT_DB, 0);
checks += 2;
```

Two coverage counts for a flag under which **no database can be opened at
all**. The test could not fail while the feature was entirely dead. That is the
vacuous-green pattern one layer out: coverage rose, behaviour was never
exercised.

**Fixed:** the setter branches are still driven (argument validation is in that
file's remit) but `checks` is no longer incremented, with the reason in place so
it does not get re-added. Behaviour now lives in a driver that asserts
consequences.

### What each new check asserts

`test/c/flag_behaviour.c` reads the flag back off the descriptor the library
actually opened, from `/proc/self/fdinfo/<fd>`. Both the AND and the OR of the
flags across all matching descriptors are computed: "flag present" is asserted
against the **AND** (so a second unflagged handle cannot hide behind a flagged
one) and "flag absent" against the **OR**.

| verdict | flag | assertion | result on master |
|---|---|---|---|
| `control` | — | neither `O_DIRECT` nor `O_DSYNC` on the data file by default | **PASS** (`fd7=02100002`) |
| `direct_db` | `DB_DIRECT_DB` | `O_DIRECT` (0o40000) on the data file | **XFAIL** — P2 |
| `direct_mpf` | `DB_DIRECT` | `O_DIRECT` on a `DB_MPOOLFILE->open(DB_DIRECT)` fd | **PASS** (`0140000` set) |
| `dsync_db` | `DB_DSYNC_DB` | `O_DSYNC` (0o10000) on the data file | **PASS** (`fd7=02110002`) |
| `direct_log` | `DB_LOG_DIRECT` | `O_DIRECT` on a log file opened after `log_set_config` | **XFAIL** — new finding |
| `dsync_log` | `DB_LOG_DSYNC` | `O_DSYNC` on a log file opened after `log_set_config` | **PASS** (`log.0000000007=02110002`) |
| `syncs@count` | `DB_LOG_WRNOSYNC` | log sync count drops, log **write** count does not | **PASS** — 203 → 2 syncs, writes 203 → 203 |
| `closesync@count` | `DB_NOSYNC` | `fsync`/`fdatasync` syscall count drops on `DB->close` | **PASS** — 16 → 8 calls |

`control` is the anti-vacuity arm. Without it, a probe that always answered
"flag present" would pass every other mode.

### Two findings beyond the brief

**1. `DB_LOG_DIRECT` has the same defect as `DB_DIRECT_DB`.** Not previously
recorded. Under `--enable-o_direct`, the log write path hands `__os_io`
unaligned buffers, and the library says so itself:

```
BDB0137 write: 0x7ffff129d287, 1: Invalid argument
BDB0137 write: 0x7f938ce00590, 131: Invalid argument
```

A 1-byte write and a 131-byte write to an `O_DIRECT` descriptor. The first
transactional open therefore fails `EINVAL`. P2 is a family, not a single site.

**2. P2 is localized to the fop and log layers, not to `os_open`.**
`direct_mpf` passes with `O_DIRECT` confirmed set on the descriptor, while
`direct_db` fails. So `DB_OSO_DIRECT → O_DIRECT` plumbing in `src/os/os_open.c`
works correctly; the defect is in the callers that read and write through
unaligned buffers. That is a narrower and more actionable statement than
"O_DIRECT is broken", and neither check alone could make it.

### Teeth, both directions

`direct_db` and `direct_log` report **XFAIL** naming P2, which `harness.sh`
counts as a verdict that held. When P2 is fixed the opens succeed, the
`O_DIRECT` assertions run, and both report PASS — with no edit to the test. If
an open ever succeeds while `O_DIRECT` is *not* set, that is a **FAIL**: a
silently ignored flag is exactly what the old acceptance check could not see.

`FLAGB_STRICT=1` refuses the XFAIL allowance. Measured, same build, same box:

```
default             FLAGB_STRICT=1
control      PASS   control      PASS
direct_db    XFAIL  direct_db    FAIL (XFAIL refused -- the defect is present)
direct_mpf   PASS   direct_mpf   PASS
dsync_db     PASS   dsync_db     PASS
direct_log   XFAIL  direct_log   FAIL (XFAIL refused -- the defect is present)
dsync_log    PASS   dsync_log    PASS
syncs@count  PASS   syncs@count  PASS
closesync@count PASS closesync@count PASS
ALL PASS            FLAG BEHAVIOUR TESTS FAILED
```

The workflow runs both passes and **fails if the strict pass succeeds** — which
means either P2 got fixed (delete the step) or the test stopped asserting
anything.

### One measurement that had to be fixed to be honest

The first `DB_NOSYNC` check used a transactional environment and measured **262
vs 261** `fdatasync` calls — a 0.4% difference, because ~260 of them were
per-commit log syncs and only one or two were the data file. A gate that cannot
resolve its own signal is not a gate. The mode now uses an **MPOOL-only**
environment and 8 database handles, giving **16 vs 8** — a clean 2x, recorded
in the driver's comments so it does not get "simplified" back.

---

## G14 — the configure-option sweep

`dist/aclocal/options.m4` declares **54** options. CI built 12.

### The plan, and why the completeness check is the point

`test/config/option_sweep.sh --check-complete` enumerates every
`AC_ARG_ENABLE`/`AC_ARG_WITH` and fails if an option is in **neither** a sweep
list **nor** the commented exclusion list. It checks both directions: a stale
entry naming an option that no longer exists is also a failure, because it looks
like coverage and is not.

```
declared in dist/aclocal/options.m4: 54
swept with a smoke run:              29
swept build-only:                     4
excluded with a reason:              21
completeness gate: OK
```

Adding a `configure` option now forces a testing decision — the same discipline
`test/MANIFEST` applies to tests.

### Why a build-only sweep would not have been enough

`--enable-o_direct` **builds perfectly and passes all 12 smoke checks**. P2 is a
runtime defect behind a *runtime* flag; the option sweep proves the option
compiles and works, and G15's flag tests catch the flag. That split is the
argument for having both halves — a build-only sweep would have reported
`o_direct` green and moved on.

`test/config/config_smoke.c` is the runtime half: 12 checks in one short
process — env open with flags read back, byte-compared round-trips with cursor
counts on btree/hash/recno/queue/heap, commit-visible **and**
abort-rolled-back, `DB->stat` reporting a nonzero key count on a populated
database, the log write counter rising, `DB->compact`, `memp_stat`, `DB->verify`.

### Full results: 33 legs on master

**29 smoke legs (build + run).** `pass=N skip=M` is the smoke driver's own count.

| option | configure arg | result |
|---|---|---|
| o_direct | `--enable-o_direct` | pass=12 skip=0 |
| atomicsupport | `--disable-atomicsupport` | pass=12 skip=0 |
| atomicfileread | `--enable-atomicfileread` | pass=12 skip=0 |
| **mutexsupport** | `--disable-mutexsupport` | **BUILD FAILURE** |
| mutex | `--with-mutex=POSIX/pthreads/library` | pass=12 skip=0 |
| posixmutexes | `--enable-posixmutexes` | pass=12 skip=0 |
| umrw | `--enable-umrw` | pass=12 skip=0 |
| stacksize | `--with-stacksize=262144` | pass=12 skip=0 |
| log_checksum | `--disable-log_checksum` | pass=12 skip=0 |
| partition | `--disable-partition` | pass=12 skip=0 |
| hash | `--disable-hash` | pass=11 skip=1 |
| heap | `--disable-heap` | pass=11 skip=1 |
| queue | `--disable-queue` | pass=11 skip=1 |
| replication | `--disable-replication` | pass=12 skip=0 |
| statistics | `--disable-statistics` | pass=9 skip=3 |
| verify | `--disable-verify` | pass=11 skip=1 |
| compression | `--disable-compression` | pass=12 skip=0 |
| smallbuild | `--enable-smallbuild` | pass=5 skip=7 |
| debug_rop | `--enable-debug_rop` | pass=12 skip=0 |
| debug_wop | `--enable-debug_wop` | pass=12 skip=0 |
| diagnostic | `--enable-diagnostic` | pass=12 skip=0 |
| debug | `--enable-debug` | pass=12 skip=0 |
| dst | `--enable-dst` | pass=12 skip=0 |
| faultinject | `--enable-faultinject` | pass=12 skip=0 |
| handoff-trace | `--enable-handoff-trace` | pass=12 skip=0 |
| cryptography | `--with-cryptography=no` | pass=12 skip=0 |
| localization | `--enable-localization` | pass=12 skip=0 |
| stripped_messages | `--enable-stripped_messages` | pass=12 skip=0 |
| uniquename | `--with-uniquename=_libdbsweep` | pass=12 skip=0 |

**4 build-only legs**: `cxx`, `stl`, `dbm`, `compat185` — all build ok.

The `skip` counts are the sweep working. `--disable-hash` correctly makes
`DB->open(DB_HASH)` return `EOPNOTSUPP` and the driver reports SKIP with the
errno printed, rather than a false pass. Same for heap, queue, verify, and the
three statistics counters. The sweep separately requires **at least 4 PASSES**
per leg, so an "everything skipped" run cannot go green.

### The real finding

**`--disable-mutexsupport` does not build on master.**

```
src/dbinc_auto/os_ext.h:27:28: error: unknown type name 'db_atomic_t';
    did you mean 'sig_atomic_t'?
```

The generated prototype header references `db_atomic_t` unconditionally, but the
type is only defined when mutex support is compiled in. The no-mutex
configuration has been broken long enough that nothing noticed — which is
precisely the G14 shape. **Not fixed here** (`src/` is out of scope for this
work); recorded as a finding.

### 21 exclusions, each with a reason

Five options are **RETIRED** — `configure` errors out on purpose, so reporting
them as coverage gaps would be a false finding. The exclusion list quotes
configure's own refusal for each:

| option | reason (measured, not assumed) |
|---|---|
| `mutexalign` | RETIRED: "`--with-mutexalign` no longer supported, use `DbEnv::mutex_set_align`" |
| `bigfile` | RETIRED: "`--enable-bigfile` no longer supported, use `--enable-largefile`" |
| `rpc` | RETIRED: "RPC support has been removed from Berkeley DB" |
| `pthread_self` | RETIRED no-op: configure only warns "is now always enabled" |
| `pthread_api` | RETIRED no-op: configure only warns "is now always enabled" |
| `uimutexes` | "unable to find UI mutex interfaces" — Solaris/UI only |
| `perfmon_statistics` | "Enabling perfmon statistics requires `--enable-dtrace`" |
| `compile-commands` | "requires bear to be installed" |
| `test` | "`--enable-test` requires `--enable-tcl`" |
| `dtrace` | needs a DTrace-capable kernel; Linux has no provider here |
| `systemtap` | needs `sys/sdt.h` and a stap-capable kernel |
| `mingw` | cross-compiles for Windows; needs a mingw toolchain |
| `java`, `jdbc` | need a JDK (+ an external SQLite JDBC tree for jdbc) |
| `tcl` | needs tclsh + headers; the tcl **bindings** are already covered by the tcl tier in `ci.yml` |
| `dump185` | needs the **system** DB 1.85/1.86 `db.h`, documented as an external prerequisite in `dist/Makefile.in` |
| `sql`, `sql_compat`, `sql_codegen`, `amalgamation`, `readline` | need the bundled SQLite source at `lang/sql/sqlite/`, absent from this tree |

Six of these were moved onto the list **after** a first sweep run reported them
as failures — the reason text is configure's actual message, not a guess.

### A bug this sweep found in its own harness

The `--enable-smallbuild` leg reported `exit 139, NO SMOKE LINE` — a segfault.
The cause was in the smoke driver: **`DB->verify` is a destructor.** Both
`db_vrfy.c` and `db_vrfy_stub.c` call `__db_close()` before returning, on the
failure path too, and the driver closed the handle again. Fixed, with the
reason in a comment.

Worth noting *how* it surfaced: the wrapper refused to grade the run from its
exit status and required a `SMOKE` verdict line, so a crashed driver was
reported as a crash instead of passing. That check earned its keep on its first
run.

---

## G12/G13 — the scaling-shape gate

### What it asserts

> median `tpm(t)` >= median `tpm(t=8)` − tolerance, **for every t > 8**

Shape, not absolute throughput. A shape comparison is between measurements
taken minutes apart on the same box in the same build, so machine class cancels
out; a stored absolute number has to be re-tuned for every machine, compiler and
kernel, and gets widened until it means nothing.

The tolerance is **derived, never typed in**: `test/bench/bench_cmp.py`'s
`TOL_SIGMA * CV` floored at `TOL_FLOOR_PCT`, imported rather than reimplemented
so there is one tested copy of the arithmetic (`test_bench_cmp.py` is its
self-test). Tightening the gate therefore requires producing a quieter
baseline, which is the only honest way to tighten a threshold.

### Measured noise floor

5 reps, thread counts alternating within each rep (`8 32 96, 8 32 96, …`) so
machine drift is spread across arms instead of attributed to whichever ran
during it. Results stream to the TSV as they complete.

| threads | min | **median** | max | **cv** | tolerance implied |
|---:|---:|---:|---:|---:|---:|
| 8 | 186,843 | **202,411** | 213,383 | **4.19%** | 12.6% |
| 32 | 178,034 | **185,143** | 186,867 | **1.98%** | 5.9% |
| 96 | 63,482 | **64,268** | 65,598 | **1.11%** | 5.0% |

**The noise floor is 4.19% CV at the reference point, giving a 12.6%
tolerance.** For comparison, the cross-engine campaign's noise floor was 40.8%;
a shorter run on a quiet box with a pre-warmed dataset is far tighter.

### Verdict on current master: FAIL

```
tolerance: max(5.0% floor, 3.0 x cv(t=8)=4.19%) = 12.6%
step:      tpm(t=32 )=185143    vs tpm(t=8)=202411     =     -8.5%   ok
step:      tpm(t=96 )=64268     vs tpm(t=8)=202411     =    -68.2%   BELOW TOLERANCE

VERDICT scale-shape FAIL threads_lo=8 threads_hi=96 tpm_lo=202411
        tpm_hi=64268 delta_pct=-68.2 tol_pct=12.6 failing_steps=t96:-68.2%
```

Exit status 1. The G12 defect is present and the gate catches it.

### The brief's t=32 formulation is necessary but not sufficient — measured

The brief specified `tpm(t=32) >= tpm(t=8)`. On this hardware **that assertion
passes on a defective tree**: the t=8 → t=32 step is −8.5%, inside the 12.6%
tolerance a quiet baseline supports. Only t=96 (−68.2%) is unmistakable.

A t=32-only gate would have reported PASS on the very tree whose throughput
collapses to a third of peak. So the gate asserts monotonicity from the
reference against **every** higher thread count and takes the worst step; all
steps are printed either way. This is recorded in the baseline file's header
because it changes how the gate should be read.

### Teeth, both directions

`--self-test` feeds the gate's own verdict code two synthetic series built from
the **measured** numbers in `test/bench/TPROC-XENGINE-2026-09.md`, and requires
opposite answers. No benchmark run; it takes seconds, so it runs on
`ubuntu-latest` on every nightly.

```
TEETH 1/2 -- the measured libdb shape (1041 -> 522 -> 337) MUST FAIL
  step: tpm(t=32)=522  vs tpm(t=8)=1041  =  -49.9%  BELOW TOLERANCE
  step: tpm(t=96)=337  vs tpm(t=8)=1041  =  -67.6%  BELOW TOLERANCE
  VERDICT scale-shape FAIL ... failing_steps=t32:-49.9%,t96:-67.6%
  OK: descending shape FAILED the gate, as required.

TEETH 2/2 -- the measured WiredTiger shape (1455 -> 2805 -> 3308) MUST PASS
  step: tpm(t=32)=2805 vs tpm(t=8)=1455  =  +92.8%  ok
  step: tpm(t=96)=3308 vs tpm(t=8)=1455  = +127.4%  ok
  VERDICT scale-shape PASS
  OK: ascending shape PASSED the gate, as required.

SELFTEST PASS -- the gate discriminates in BOTH directions.
```

**The self-test has already caught a real defect in the gate itself.** The first
version raised `ModuleNotFoundError` inside its verdict path (it located
`bench_cmp` via `argv[0]`, which is useless for a script fed to python on
stdin). That exited non-zero — which the descending arm read as "correctly
failed the negative-scaling series". A crash impersonating a verdict. Only the
ascending arm, which then also "failed", exposed that the gate was not running
at all. This is the whole argument for demonstrating teeth in both directions,
and it happened on the first attempt.

### Two refusals the gate makes instead of reporting

**Below 32 cores it refuses to measure** (exit 3). On a 2–4 vCPU runner t=8 and
t=32 both measure oversubscription; the difference describes the Linux scheduler,
not libdb. A number produced there would look exactly like evidence.

**Above 10% CV at the reference point it refuses to judge** (exit 2). This
fired for real: the first run, without a prewarm pass, measured t=8 at

```
rep1 282211, rep2 240373, rep3 212493, rep4 193171, rep5 185604 tpm
```

a monotone 34% decline across reps — cv 16.6%. The gate had already computed a
−11.6% delta and **discarded it**:

```
GATE ERROR: t=8 cv is 16.6%, above the 10.0% usability ceiling.
```

The cause is that TPROC-C **mutates its own dataset** (new-order inserts,
delivery deletes), so early reps run against a fresher, tidier tree. The repair
was a **discarded prewarm pass** over every thread count before the measured
reps — cv 16.58% → 4.19% — not a wider tolerance. Widening would have been
actively wrong here: the defect this gate looks for *is* a throughput decline, so
a tolerance wide enough to cover that noise would have hidden it. The refusal
message now says so explicitly.

### Baseline recorded

`test/bench/baseline-scale-shape.tsv` holds all 45 measured rows plus full
provenance (git rev, hardware, CPU model, kernel, secs, scale, cache, prewarm
flag), with tpm(t=8) / tpm(t=32) / tpm(t=96) called out in the header. The gate
does **not** compare against these numbers — it asserts shape within each run —
but they make a future change comparable, and they put the shape this tree
exhibits on the record.

---

## Wiring

**`test/MANIFEST`** — three new tiers, 49 entries. Mandatory entries are those
that need no build option and no big box: the `control`/`dsync_*`/`syncs`/
`closesync` flag verdicts, the `config complete` completeness gate, and
`shape selftest`. The O_DIRECT flag modes are optional (they need
`--enable-o_direct`, and the runner *prints* why it skipped). The per-option
sweep legs and the shape measurement are optional because they are nightly on a
big box — but `option_sweep.sh` separately fails if the number of recorded legs
does not match the number planned, so a leg that wrote no row is a failure, not
an absence.

**`.github/workflows/test-tiers.yml`** — new `flagbehaviour` job: builds
`--enable-o_direct` (the only CI job that does), runs the flag tests, asserts
the manifest, then runs the `FLAGB_STRICT=1` teeth pass and **fails if it
succeeds**.

**`.github/workflows/nightly-bigbox.yml`** — `schedule:` at 05:23 UTC (offset
from the other two nightlies) plus `workflow_dispatch` with inputs. Four jobs:

- `shape-selftest` (ubuntu-latest) — the teeth, plus `bench_cmp.py`'s own
  self-test. Runs everywhere, always.
- `option-completeness` (ubuntu-latest) — the completeness gate and the plan.
  Cheap, parses `options.m4` only.
- `scale-shape` and `option-sweep` — `[self-hosted, bigbox]`, gated on the repo
  variable `HAVE_BIGBOX_RUNNER` so the workflow is readable and dispatchable in
  a fork without queueing forever against a label nobody provides.

The header states plainly that **GitHub-hosted runners cannot run these jobs**
and why. `scale-shape` is deliberately **not** `continue-on-error` — that is
exactly how `bench.yml` became decorative — so it will show red on master until
P1 is fixed. That is the gate working.

---

## What remains open

**Defects found and not fixed** (`src/` changes are out of scope for this work):

1. **P2** — `DB_DIRECT_DB` cannot open a database. Recorded as XFAIL; the test
   starts passing when it is fixed.
2. **P2's log sibling** — `DB_LOG_DIRECT` fails the same way through the log
   write path. New finding, XFAIL.
3. **`--disable-mutexsupport` does not build** — `db_atomic_t` referenced
   unconditionally in `src/dbinc_auto/os_ext.h`.
4. **P1** — the `PGNO_BASE_MD` allocation convoy, the mechanism behind the
   shape failure. Already characterized in
   `test/bench/BTREE-LOCK-SCOPE-2026-09.md`.

**Gaps in what was built:**

- **No WiredTiger arm in the shape gate.** The teeth demonstration uses WT's
  *measured* numbers as a synthetic series, not a live WT run. A live
  cross-engine arm would need libwiredtiger on the runner; the gate is
  single-engine by design (it asserts libdb's shape against itself), so this is
  a limitation of the *teeth* demonstration, not of the gate. Noted rather than
  hidden: the ascending arm proves the verdict logic accepts a rising shape, not
  that WT still rises today.
- **The shape gate measures one arm only** — `libdb-sync-btree` at
  scale 96, 4 GiB cache, 30 s reps. It does not sweep access methods, durability
  modes, or `DB_MPOOL_AIO`. Those were measured in the cross-engine campaign;
  gating all of them nightly would multiply the runtime by ~7 for signal the
  campaign already priced.
- **The dataset is cache-resident** (0.34 GiB data, 4 GiB cache). The defect
  reproduces clearly anyway, and an in-cache workload isolates *concurrency*
  from I/O — but it is not the 13.6x out-of-cache ratio the campaign used, so
  the absolute tpm here (202k) is much higher than the campaign's (1,041) and
  the two are **not comparable**. Only the shapes are.
- **`--enable-largefile` is not swept.** It is the live replacement for the
  retired `--enable-bigfile`, but it is autoconf's own option (`AC_SYS_LARGEFILE`),
  not declared in `options.m4`, so the completeness check does not see it and it
  is neither swept nor excluded.
- **Known-interacting option pairs are not swept.** The brief allowed "a small
  set of known-interacting pairs"; only the three pairs required to reach an
  option at all are built (`--enable-cxx --enable-stl`, and the sql variants
  which are excluded anyway). One-at-a-time was the priority.
- **No option is swept under the Tcl suite.** The smoke driver is deliberately
  breadth-not-depth (12 checks, seconds per leg, ~33 builds). A `--disable-`
  option that breaks something subtle in an access method would pass it. Running
  even a subset of the Tcl suite per leg would turn a 40-minute sweep into hours.
- **The `flag` tier does not cover `DB_DIRECT` on the log's *write* path
  positively.** `direct_log` XFAILs before it can probe, so once the P2 family is
  fixed, that assertion runs for the first time and could find a third problem.

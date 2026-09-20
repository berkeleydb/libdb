# Testing-program improvements: API flag behaviour, a branch ratchet, and a
# faster coverage job

Work done 2026-09-20 against `docs/design/testing-program-2026-09.md`, which is
the measured baseline this builds on. Branch: `test/coverage-and-speed`.

Everything below was measured on a dedicated idle 32-vCPU EC2 box (`/nvme`, XFS
over NVMe), gcc/gcov 12.2.0, lcov 1.16. Where a number differs from the 2026-09
document, the toolchain difference is stated.

## Summary

| | before | after |
|---|---|---|
| public API flags that are the SUBJECT of a graded verdict | 7 | **34** |
| public API flags with no reference under `test/` at all | 112 | **82** (29 explicitly SKIPped with a reason, 53 genuinely open) |
| branch-coverage regression gate | none (advisory warning on a `continue-on-error` job) | **fails the build**, tolerance measured |
| coverage job wall time | **1970 s** serial | **1095 s** parallel (1.80x) |
| known-interacting configure-option pairs | 0 | **5**, each with a stated reason |
| engine defects found | — | **3** (P6, P7, P8), all XFAIL with references |

Three engine defects, one pre-existing test-design failure, and three
concurrency bugs in the test infrastructure were found. None of the engine
defects is fixed here: per the brief each gets its own reviewed change.

---

## 1. Behaviour tests for untested API flags

### What "covered" means here

`G15` established that a flag can be referenced, counted as covered, and still
be completely broken: `cov_api_surface.c` asserted only that `set_flags`
*accepted* `DB_DIRECT_DB`, while the flag could not open a database at all
(`P2`). So the bar for every check added here is an externally observable
consequence, and every claim of an *absence* has a control arm asserting the
corresponding *presence* — without one, a probe looking in the wrong place
satisfies every absence assertion.

Concretely, what is asserted:

| flag(s) | the observable |
|---|---|
| `DB_ARCH_LOG` | the returned list names `log.*` files, only those, and each exists on disk |
| `DB_ARCH_DATA` | names both databases, no log file, each exists |
| `DB_ARCH_ABS` | names are absolute **and** the same call without the flag returns relative ones |
| `DB_ARCH_REMOVE` | every removable log file is **gone**, the live log **survives**, and the environment still commits and reads back |
| `DB_BACKUP_NO_LOGS` | log files absent from the backup directory, against a control backup that contains them |
| `DB_BACKUP_UPDATE` | log files copied, deleted `.db` files **not** re-copied |
| `DB_SEQ_INC` / `DB_SEQ_DEC` | the sequence values returned go up / go down |
| `DB_SEQ_WRAP` | a sequence at its range end wraps, where the no-flag control returns `EINVAL` |
| `DB_SEQ_RANGE_SET` | `set_range`'s bound is enforced (5 values), against an unbounded control (50) |
| `DB_TXN_FAMILY` | sibling children read each other's uncommitted writes where independent transactions get `DB_LOCK_DEADLOCK` |
| `DB_TXN_WAIT` | **elapsed time**: 0 ms without it, 2001 ms with it, against a 2000 ms lock timeout |
| `DB_CURSOR_BULK` | all 400 records stored through the bulk cursor read back byte-correct |
| `DB_INORDER` | consumed record numbers strictly increasing across a deleted record |
| `DB_FREELIST_ONLY` | 5 pages truncated, file 5275648 → 5255168 bytes, all 10000 surviving records intact |
| `DB_NOLOCKING` | lock requests 911 → 0 while every record still reads back correctly |
| `DB_OVERWRITE` | the **bytes in the region file at unlink time**: `0x97` (real data) without the flag, uniform `0xff` with it |
| `DB_NOFLUSH` | a shared and a private environment can be created, filled and read back |
| `DB_HOTBACKUP_IN_PROGRESS` | reads back while set and is **gone** after the paired clear (it is reference-counted) |
| `DB_STAT_LOCK_CONF` / `_OBJECTS` / `_PARAMS` | each flag's own section present with it, absent without it |
| `DB_STAT_SUMMARY` | `rep_stat_print` output differs from the plain arm |
| `DB_SALVAGE` / `DB_PRINTABLE` / `DB_AGGRESSIVE` / `DB_ORDERCHKONLY` | the salvage **output**: non-empty, containing the stored value literally where raw salvage escapes it, and `DB_ORDERCHKONLY` writing no dump |

`DB_TXN_WAIT` is graded on time rather than on the error code deliberately: both
arms return `DB_LOCK_DEADLOCK`, because the lock genuinely cannot be granted
either way, so an error-code check would pass on a build that ignored the flag
entirely.

### Files

```
test/c/flag_archive.c     log_archive + DB_ENV->backup flags
test/c/flag_misc.c        everything else
test/c/flagapi-run.sh     the runner: 29 graded verdicts
test/c/flag_inventory.sh  the reproducible accounting behind the numbers above
test/MANIFEST             29 new mandatory entries, tier `flagapi`
.github/workflows/test-tiers.yml   the `flagapi` job
```

Result on the measurement box: **26 PASS, 3 XFAIL, rc=0**, and
`check_manifest.sh --tier flagapi` clean with 29 verdict lines.

### The count, honestly

`test/c/flag_inventory.sh` exists so these figures can be regenerated and
disputed rather than trusted:

```
public API flags declared                 229
SUBJECT of a graded verdict                33  (14.4%)
  ..plus asserted via a trigger flag        1
  ..appearing in driver code at all        48  (21.0%)
mentioned under test/ only                 99  (43.2%)
UNTESTED (no reference at all)             82  (35.8%)
  ..of those, SKIPPED with a reason        29
  ..of those, genuinely OPEN               53
```

Three distinctions matter, and all three cut the number *down*:

- **34, not 48.** The larger figure counts every flag appearing in driver code,
  which includes the scaffolding those drivers need to reach the flag under test
  (`DB_CREATE`, `DB_INIT_MPOOL`, `DB_AUTO_COMMIT`, `DB_MPOOL_DIRTY`, …).
  Counting scaffolding as coverage is a softer form of exactly the mistake G15
  named, so the subject list is explicit and cross-checked against the code.
- **34 = 33 + 1.** `DB_LOG_WRNOSYNC` is not settable directly — `__txn_commit`
  passes it to `log_put` when the environment has `DB_TXN_WRITE_NOSYNC` — so it
  is asserted through its trigger and reported separately rather than folded in.
- **"mentioned" is not coverage.** 99 flags are named somewhere under `test/`,
  often only in a comment or an acceptance check. That is the weak bar G15 warned
  about.

The script refuses to report at all if its comment-stripper yields too little
input: the first version's malformed `sed` produced an empty file and
confidently reported "0 flags asserted", which looks like a finding and is a
broken tool.

### The 29 honest SKIPs

Full list with reasons: `test/c/flag_inventory.sh --skipped`. Every skip is
structural, and the list is *checked* — a skip with no reason, a skip of a flag
that does not exist, or a skip of a flag that IS asserted each fail the script,
because all three make the inventory a lie. Categories:

- **INTERNAL-ONLY (20, plus `DB_UNREF` which is explained but counts as
  "mentioned")** — the flag shares the public bit-space (every flag in
  `api_flags.in` is generated from `dist/api_flags`) but is only ever passed
  between internal functions, and the public entry point's `__db_fchk` rejects
  it. The `DB_ST_*` / `DB_SA_*` verify-descent flags are the clearest case:
  `bt_verify.c` passes them down its own recursion, and `DB->verify`'s
  `VERIFY_FLAGS` mask (`db_vrfy.c:61`) does not include them.
- **FEEDBACK VALUE (2)** — `DB_UPGRADE` and `DB_VERIFY` are the `opcode`
  *argument* the library passes **to** an application's `set_feedback` callback.
  There is nothing to set; asserting them is a binding test and belongs with the
  callback coverage in `cov_api_surface.c`.
- **NEEDS MULTI-SITE REPLICATION (4)** — the `DB_REP_*` transport flags are
  arguments passed to an application's `send()` callback; asserting them needs
  two live sites and a controllable network, which is `test/repiso` and the
  `rep0NN` harness, not this tier.
- **NEEDS A CRASHED PROCESS (4)** — `DB_MUTEX_ALLOCATED`, `DB_MUTEX_LOCKED`,
  `DB_MUTEX_LOGICAL_LOCK`, `DB_FAILCHK_ISALIVE` are only observable across a
  process that died holding a resource.

The remaining **53 are genuinely open**, not skipped:
`test/c/flag_inventory.sh --open`. The largest coherent groups are eight of the nine
`DB_LOG_VERIFY_*` flags (they need a `DB_LOG_VERIFY_CONFIG` driver, which does
not exist yet), eight `DB_MPOOL_*` `memp_fget`/`fclose` modes, and six
`DB_LOCK_*` `lock_vec` operations.

---

## 2. Three engine defects

All three reproduce on stock master, are not filesystem- or
environment-dependent, and are recorded as `XFAIL` with a reference. Each test
flips to PASS with no edit once the engine is fixed.

### P6 — `DB_BACKUP_NO_LOGS` is accepted and completely ignored

Documented in `docs_src/api/c/envbackup.md` as "Back up only the `*.db` files.
Do not backup the log files." It appears **exactly once in all of `src/`** —
`db_backup.c:684`, the accepted-flag mask — and is read nowhere.

Measured: a plain `DB_ENV->backup` copied 62 log files; the same backup with
`DB_BACKUP_NO_LOGS` copied the same 62.

Severity: an operator asking for a logless backup silently gets logs. Wasted
space rather than data loss, but the flag is a documented no-op.

### P7 — `DB_INORDER` + `DB_CONSUME` across a deleted record hangs forever

Minimal reproduction: a queue with `set_flags(DB_INORDER)`, 20 records via
`DB_APPEND`, `DB->del` of record 10, then drain with `DB_CONSUME`. Without the
flag it drains all 19 survivors and returns `DB_NOTFOUND`. With it, records 1–9
are consumed, the hole is reached, and the call never returns — 98% CPU,
indefinitely.

A gdb hit count on the `retry:` label in `__qamc_get` (`src/qam/qam.c:691`):

```
default arm     22 hits, then terminates
DB_INORDER    >100,001 hits, still climbing
```

Mechanism: `qam.c:667` sets `inorder = F_ISSET(dbp, DB_AM_INORDER) &&
with_delete`, which then takes the record lock *without* `DB_LOCK_NOWAIT`
(`qam.c:838`) and selects the `first != cp->recno` test (`qam.c:866-867`). Over a
deleted record that condition never converges — the non-inorder path advances
`first` past the hole at `qam.c:956` (`QAM_INC_RECNO(first)`, reached via
`else if (first == cp->recno)`), which the inorder path does not reach.

Severity: highest of the three. An unbounded spin in a documented public flag,
reachable with no concurrency at all.

The test carries its own watchdog rather than relying on the runner's timeout,
for two reasons: the hang is inside a single `DB->get` that never returns, so the
drain loop's iteration cap cannot see it; and a killed process yields **no
verdict line**, which is graded FAIL — correct in general, but it would record
this as an unexplained crash rather than as the specific understood defect it is.

### P8 — `DB_NOFLUSH` makes an environment unusable

3/3 runs each, 100% deterministic:

```
set_flags(DB_NOFLUSH) + DB_ENV->open without DB_PRIVATE
    -> SIGBUS in __env_alloc_init (src/env/env_alloc.c:136), from
       __env_attach (env_region.c:425).  __db.001 is left ZERO BYTES.
set_flags(DB_NOFLUSH) + DB_ENV->open with DB_PRIVATE
    -> the env opens; the first DB->open fails DB_PAGE_NOTFOUND.
```

Root cause is not subtle. `LAST_PANIC_CHECK_BEFORE_IO` (`src/dbinc/os.h:105`)
expands to:

```c
PANIC_CHECK(env);
if (env != NULL && F_ISSET((env)->dbenv, DB_ENV_NOFLUSH))
        return (0)
```

— an unconditional `return (0)` sitting inside `__os_physwrite`'s write loop
(`os_rw.c:322`) and `__os_io`'s read and write arms
(`os_rw.c:71,93,208,221`). Under `DB_NOFLUSH` **every write in the library
reports success and writes nothing**. Environment creation depends on one:
`__db_file_extend` (`src/env/env_file.c:44`) extends the region file by writing
its last byte, that write silently does nothing, the file stays empty, and the
subsequent mmap faults on first touch.

The flag's intent is to skip the cache flush on *close*, which is the one use the
library makes of `DB_ENV_NOFLUSH` internally (`env_open.c:582`, during panicked
teardown, where nothing is written afterwards — so the bug is invisible there).
Applied by an application before open, as the public flag allows, it suppresses
all I/O for the environment's entire life.

`dist/api_flags:221` marks it `UNDOC`, and `docs_src/_migrate/flag-reconcile.md`
records it as an undocumented `set_flags` flag left for a later audit. This is
that audit's finding.

The shared-environment arm runs in a **forked child** so a fatal signal reports a
verdict instead of killing the driver.

### Teeth, both directions

`FLAGAPI_STRICT=1` refuses the XFAIL allowance:

```
FLAGAPI_STRICT=0   26 PASS, 3 XFAIL, rc=0
FLAGAPI_STRICT=1   backup_nologs, inorder@inorder, noflush@noflush all
                   FAIL ("XFAIL refused"), rc=1
```

The CI teeth step is a **hard gate** here, unlike the existing `flagbehaviour`
job's. That difference is deliberate: `P2`/`P3` are filesystem-dependent (they
reproduce on XFS and not on the GitHub runner's filesystem), so "must fail" is
not a property of the code there. `P6`/`P7`/`P8` reproduce anywhere the library
builds, so the step asserts the strict run failed **on those three modes by
name** — a build break also makes a run fail, and reading that as teeth is the
vacuous-green shape this project has nine recorded instances of. That logic was
exercised in all three outcomes (correct fail, wrong-reason fail, unexpected
pass) before being committed.

### Four checks that corrected their own premise

Worth recording, because each looked like a defect and was not, and the next
reader should not re-derive them. All four are commented in place.

1. **`DB_ARCH_REMOVE` must be graded against `log_archive(flags=0)`**, not
   `DB_ARCH_LOG`. `DB_ARCH_LOG` lists *every* log file including the live one, so
   grading against it reported a spurious "the flag did nothing" on the live log.
2. **`DB_STAT_*` section flags make the output SMALLER.** A section flag
   suppresses the default counter block (`__lock_stat_print` only calls
   `__lock_print_stats` when `flags == 0` or `DB_STAT_ALL`), so "the flag
   enlarges the output" is the wrong property. Graded on per-section headings
   present-with / absent-without instead.
3. **Raw `DB_SALVAGE` escapes bytes as `\NNN`** — which is exactly what
   `DB_PRINTABLE` changes. Requiring the marker literally in the raw output
   failed a correctly working flag; the assertion is now "printable contains it,
   raw does not", which attributes the behaviour to the flag.
4. **`DB_ORDERCHKONLY` requires a named subdatabase** (`db_vrfy.c:174`). Passing
   `NULL` tests the argument check, not the flag.

And one test-design failure of mine, likewise commented: `DB_NOLOCKING` with
`DB_AUTO_COMMIT` cannot release its handle lock, so every operation fails and the
arm reports zero lock requests *because it did no work* — indistinguishable from
the flag working. Measured 400 of 400 records missing. The mode now uses a
mutex-free-appropriate environment (`DB_INIT_LOCK`, no `DB_INIT_TXN`).

---

## 3. The branch-coverage ratchet

```
test/coverage/ratchet.sh        the gate
test/coverage/ratchet_test.sh   its self-check, 14 checks
test/coverage/baseline.txt      branch=40.4, branch_tolerance=0.3
.github/workflows/coverage.yml  wired in, no longer advisory
```

`docs/design/testing-program-2026-09.md`: "`baseline.txt` exists but nothing
fails when coverage *drops*." The old step emitted `::warning` and always
succeeded, on a job that was additionally `continue-on-error` — so a coverage
regression was reported by nothing that could stop it.

**Only branch coverage gates.** Line and function coverage are reported and can
never fail the build: a line executed once with one outcome of a two-way branch
counts as covered, which is precisely the class every defect in the 2026-09
cycle (`P1`–`P5`, `U7`) lived in. Self-check 9 asserts that line coverage
collapsing to 20% does not fail the gate.

### Before / after

| | branch | line | function |
|---|---|---|---|
| committed baseline (lcov 2.0, 2026-09-09) | 40.6 | 59.4 | 78.6 |
| re-measured here (lcov 1.16, gcov 12.2.0) | **40.4** | 59.4 | 78.4 |
| after this work | **40.4** | 59.4 | 78.4 |

The 40.6 → 40.4 difference is the toolchain, not a regression: the committed
number came from lcov 2.0-1 and gcov 11.5.0, and this box has lcov 1.16 and gcov
12.2.0. The baseline is updated to the number this toolchain produces, with the
provenance recorded in `baseline.txt`.

**The new tests do not yet move the subset's branch number.** They are a separate
tier (`flagapi`) and are not in `run_coverage.sh`'s curated subset, so they add
no branches to this measurement. What they add is 29 verdicts on flags that had
none, and three defects. Wiring them into the coverage subset is listed as open
work below.

### The tolerance is measured

Three full runs of the same tree, idle box, separate worktrees:

```
run   branch hits   of 79867   branch %
W1       32271                 40.4059
W2       32272                 40.4072
W3       32232                 40.3571

spread (max-min)  0.0501 pp
stdev             0.0286 pp
wall              1970 / 1970 / 1969 s
```

`branch_tolerance=0.3` is ~10x the observed stdev and 6x the spread: tight enough
that a real regression cannot hide in it (losing one tier moves far more — see
below), loose enough not to fire on noise.

### Teeth, both directions, on real measured data

```
all three real runs          RATCHET OK  (40.4% >= floor 40.10%), rc=0
real summary doctored to
  35.4% (>> tolerance)       RATCHET FAIL, rc=1
same, RATCHET_EXPECT_FAIL=1  rc=0, "teeth demonstrated"
```

`RATCHET_EXPECT_FAIL` deliberately does **not** invert the error exit (code 2), so
a gate that *crashed* still fails the must-fail arm. Self-check 6 asserts exactly
that, because the recorded failure this guards against is a must-fail arm that
accepted a `ModuleNotFoundError` as success. For the same reason the gate is
POSIX `sh` + `awk` with no import in its verdict path.

Self-check 11 exists because `bash -n` cannot see a function used before it is
defined. That shipped during this work: three concurrent runs all died twenty
minutes in with `run_coverage.sh: line 506: cov_pkill: command not found`,
because the helper had been appended *after* its first use. Check 11 parses each
coverage script and fails in milliseconds on a helper called before its
definition; it was verified against a reconstruction of that exact break.

---

## 4. Coverage wall time: 1970 s → 1095 s

```
test/coverage/run_coverage_parallel.sh   one build, seven concurrent groups,
                                         merged with lcov -a
```

| | wall | branch hits | branch % |
|---|---|---|---|
| serial (3 runs) | 1970 / 1970 / 1969 s | 32271 / 32272 / 32232 | 40.4 |
| parallel (2 runs) | **1095 / 1096 s** | 32260 / 32242 | 40.4 |

**1.80x, 875 s saved (44%).** Parallel is 0.0092 pp below the serial mean — 7
branches of 79867, 0.009% — against a measured serial run-to-run spread of
0.0501 pp. So the difference is inside the noise, not a systematic loss, and the
merged result passes the same ratchet the serial run does. Per file: 35 of 279
differ at all, bidirectionally, 149 branches total.

**No coverage was traded for speed, and that is checked rather than asserted:**
the parallel path's merged number goes through `ratchet.sh` exactly as the serial
one does, and the serial run is kept as a nightly reference so the fast path is
graded against something that is not itself parallel.

### Why groups in separate trees rather than backgrounded blocks

Every Tcl block uses `build_unix/TESTDIR`, and three of them empty it between
tests (`find TESTDIR -mindepth 1 -delete`). Backgrounding them in one tree makes
them delete each other's databases mid-test. Parameterising that path is not
cheap: `TESTDIR` is baked into `test/tcl/test.tcl`, the ~135 tests that source
it, and the `recdscript.tcl` subprocesses that recompute it independently.

So: one instrumented build, then each group in its own directory with its own
`GCOV_PREFIX`, merged with `lcov -a`. The merge equivalence was verified on a
two-branch C program **before** being trusted for the suite:

```
arm 1 alone     3 of 6 branches (50%)
arm 2 alone     3 of 6 branches (50%)
lcov -a both    6 of 6 branches (100%)
serial run      6 of 6 branches (100%)   <- identical
```

That experiment also caught a trap: capturing a bare `GCOV_PREFIX` directory
yields an **empty** `.info`, because lcov needs each `.gcno` beside its `.gcda`.
An empty capture would have merged as "this group covered nothing" while every
command exited 0, so the runner symlinks the graph files in, and a group that
produces no `.gcda` at all **aborts the run** rather than being merged as an
empty contribution. That guard fired for real twice during development.

### The group split is measured

The first arrangement (five groups) gave `deadreg` 1344 s against everything else
put together, so no schedule of five groups could beat ~22 minutes. `deadreg` and
`recd` are each split in two, giving seven groups and a 829 s critical path:

```
deadreg2 829s   deadreg1 515s   recd1 326s   recd2 113s
tcl 93s         cdrivers 67s    misc 7s
```

The critical path is now `deadreg2`, so further gains need that split finer or
those tests made faster — not more groups.

### Three cwd-relative assumptions, each found by a failing run

1. `test.tcl` does `source ./include.tcl`, and `include.tcl` is *generated* into
   `build_unix` — four of five groups produced zero `.gcda` with "couldn't read
   file ./include.tcl".
2. `include.tcl` then names `src_root`/`test_path` as `../dist/..`, relative to
   `build_unix`, which does not resolve one level deeper — next failure was
   "couldn't read file ../dist/../test/tcl/testutils.tcl". The group copy is
   rewritten with absolute roots.
3. The C drivers compile their sources as `../test/xa/xa_direct.c`, so the
   `cdrivers` and `misc` groups run **from** `build_unix`. Safe for them and not
   for the Tcl groups, because each C driver uses its own home directory
   (`XA_TESTDIR`, `OSAIO_TESTDIR`, `BACKUP_TESTDIR`, …) rather than the shared
   `TESTDIR`.

### Three shared fixed resources that made concurrency impossible

The brief asked whether any tiers are serialised only because they share a fixed
scratch path. They are — and the scratch *directory* was not the first thing to
bite.

**(a) Fixed TCP ports — worth 3.8 pp of measured coverage variance.**
The first variance measurement gave 40.4 / 36.6 / 39.4, and it was not noise.
`TestChannel` brings up three live repmgr sites and picks ports by probing upward
from a hard-coded base of 30100 (`test/c/suites/TestChannel.c:1305`),
bind-then-close-then-reuse, so concurrent runs raced. One run was clean, one took
`SIGABRT`, one reported a CuTest failure — and `TestChannel` is the **only** thing
in the curated subset that exercises live replication, so 60 files under `rep/`
and `repmgr/` swung ~3100 branches (`rep_record.c` alone 476 → 0) while every
other phase was second-for-second identical.

`TestChannel` already honoured a `BDBPORTRANGE` environment variable;
`run_cov_cutest.sh` simply never set it. It now derives a private 200-port window
per run from the PID. Verified: three concurrent `cov_cutest` runs took
`49700:49899` / `34300:34499` / `49900:50099` and all three reported "9 suites, 9
clean", where before one crashed and one failed. Post-fix variance: **0.0501 pp**.

Had this gone unfound, the tolerance would have had to be ~4 pp — wide enough to
hide any regression worth gating on.

**(b) Global `pkill` patterns.** `pkill -f 'wrap.tcl'` and four siblings match by
*command line* across the whole machine, so concurrent runs killed each other's
live workers: two of three runs died at `dead003` with rc=124. Separate worktrees
do not help — the pattern names no path. `cov_pkill` now anchors each pattern to
this run's build directory, with a process-group fallback. Verified directly:
with two runs each holding a `wrap.tcl` worker, `cov_pkill` left the other run's
alive (runA=0, runB=1) where the bare `pkill` killed both.

**(c) Fifteen fixed `/tmp` log paths**, now a per-run directory. Two concurrent
runs overwrote each other's diagnostics, so a failure in one was diagnosed from
the other's output.

`run_coverage.sh` also now writes `build_unix/coverage-phases.txt` — per-phase
wall time — so "which block costs the 30 minutes" stays a measured fact.

### CI shape

- `ratchet-selfcheck` — seconds, no build, gates everything below it.
- `coverage-parallel` — **per push and PR**, gates on the ratchet, plus a
  must-fail teeth step on the real measured numbers.
- `coverage-serial` — nightly/dispatch, the reference measurement, also gated.

---

## 5. Known-interacting configure-option pairs

One-at-a-time sweeping cannot see interactions, and 2^54 is not a target. Five
named pairs, each carrying a one-line reason for the interaction it probes
(`SWEEP_PAIRS` in `test/config/option_sweep.sh`); a pair whose reason is empty
**fails its leg**, because an unjustified pair is a random sample of a 2^54 space.

| pair | result |
|---|---|
| `diagnostic × o_direct` | PASS (smoke pass=12) |
| `smallbuild × statistics` | PASS (smoke pass=5, skip=7) |
| `replication × cryptography` | PASS (smoke pass=12) |
| `diagnostic × smallbuild` | PASS (smoke pass=5, skip=7) |
| `mutexsupport × atomicsupport` | **XFAIL** — see below |

The completeness gate is untouched: pairs are extra *legs*, not a new population
of options, so `--check-complete` still reports 54/29/4/21 and an unlisted option
still fails the gate.

### `mutexsupport × atomicsupport`, and an honest XFAIL

This leg fails, and the interesting part is that it is **not** an interaction.
`--disable-mutexsupport` fails the smoke driver *alone*, and identically, on
pristine master — verified by running the single-option leg on an unmodified
worktree:

```
--- mutexsupport: FAIL (SMOKE pass=0 fail=1 skip=0)
```

`U7` fixed `--disable-mutexsupport` so it *builds*; nobody then checked that the
smoke driver can run against it. `config_smoke.c` opens its environment with
`DB_INIT_LOCK|DB_INIT_TXN`, which a library built without mutex support cannot
provide — it correctly answers "library build did not include support for
locking" and panics. So this is a **pre-existing test-design gap**, not a defect,
and the pair adds no information until `config_smoke.c` can request a mutex-free
environment.

It is kept, XFAILed, and named so the fix has somewhere to land. An XFAIL leg
that starts **passing** fails the sweep: verified by pointing it at a passing
configuration, which produced `UNEXPECTED PASS` and sweep rc=1. That also caught
a real bug in the grading — `walk_pairs` runs in a subshell, so the verdict
reached the summary file but not the exit status, and `unexpected-pass` had to be
added to the bad-tag list the final grading reads.

---

## What remains open

1. **`P6`, `P7`, `P8` are unfixed**, by design — each needs its own reviewed
   change. `P7` is the one to take first: an unbounded spin in a documented
   public flag, reachable with no concurrency.
2. **53 flags are genuinely open** (`flag_inventory.sh --open`). The largest
   coherent groups, and what each needs: eight `DB_LOG_VERIFY_*` flags need a
   `DB_LOG_VERIFY_CONFIG` driver; eight `DB_MPOOL_*` `memp_fget`/`fclose`
   modes need a mpool-level driver in the shape of `flag_behaviour.c`'s
   `direct_mpf` mode; the six `DB_LOCK_*` `lock_vec` operations need a lock-vec
   driver.
3. **The new tier is not in the coverage subset.** `flagapi` runs as its own CI
   job, so its branches are not counted in `baseline.txt`. Adding it to
   `run_coverage.sh`'s `COV_TESTS` would raise the branch number and let the
   ratchet lock the gain in.
4. **`config_smoke.c` cannot smoke a mutex-free build.** Fixing that turns the
   `mutexsupport` single leg and the `mutexsupport × atomicsupport` pair from
   XFAIL into real assertions.
5. **The critical path is now `deadreg2` at 829 s.** Further wall-time gains need
   that group split finer or `dead*`/`env012` made faster; adding groups will not
   help.
6. **`TestQueue` crashes in an optimised build** (pre-existing, documented in
   `test/coverage/FULL-COVERAGE-REPORT-4.md`): a fixed `static char buf[1024]`
   overrun in the test harness, not in libdb.
7. **The ratchet never raises itself.** It prints the new number and asks for the
   baseline to be raised when coverage rises past the tolerance, but a human
   must edit `baseline.txt`. Automating that risks ratcheting in a fluke.
8. **Lcov-version skew is handled but not pinned.** `run_coverage.sh` now works
   on lcov 1.x and 2.x, but the two produce slightly different numbers
   (40.4 vs 40.6 on the same tree), so the baseline is only comparable within a
   toolchain. Pinning lcov in CI would make the ratchet's floor exact.

# Re-measuring the four parked read-path branches against v2026.09.6

**Branch:** `perf/pin-remeasure-2026096`
**Baseline:** `v2026.09.6` (`65a10621e`), the release that shipped `db_get_multiple()`
**Box:** dedicated EC2 `c7i.24xlarge`, 96 vCPU Xeon 8488C, 185 GB, Debian 12 / 6.1,
1 NUMA node, governor=performance, THP off, NUMA balancing off, **ASLR off**,
`perf_event_paranoid=-1`
**Total measurements:** 1 412 `RESULT` lines across 7 runs, **0 `FAIL` lines**

---

## The question, and the short answer

Every one of the four branches was measured while **85.3 % of cycles sat in the
cursor-lifecycle mutex**. `v2026.09.6` shipped `db_get_multiple()`, which removes
that mutex. Were the four negative verdicts an artifact of being measured behind
a bottleneck that is now gone?

| branch | old verdict | new verdict | the number that decides it |
|---|---|---|---|
| `perf/mpool-pin` | "no effect" | **NULL** | 0.957–1.033× across 12 cells; noise floor ±4.8 % |
| `perf/bhpin-r1` | "neutral" | **REGRESSION as it stands** (and its old verdict was *vacuous*) | 1.55× / 1.36× where it fires, **0.885× on the shared batched path**, and it **panics** once armed |
| `perf/rsnap-ml` | 7–13× regression | **REGRESSION, confirmed** | 0.063× at private/batch/32, 0.318–0.365× at 32 threads |
| `perf/lock-readpath` | "neutral" | **NULL** | 0.964–1.019× across 12 cells |

**Premise: confirmed.** On this box, at 96 threads, `__memp_fget` plus the pin
atomics are **71.9 %** of self time on the batched path and **2.4 %** on the
individual path. The four branches really were measured behind a wall.

**Recommendation: ship none of the four.** The premise was right and the old
measurements were indeed taken in the wrong regime, but re-measuring in the
right regime does not rescue any of the four. It does something more useful: it
shows the one branch that attacks the now-dominant cost (`perf/bhpin-r1`) is
**1.5× where it can fire and latently unsafe**, which converts it from "parked,
neutral" into "a live 1.5× lead with a specific, locatable bug to fix". Details
in §6 and §9.

Two findings below are more important than any of the four verdicts, and both
invalidate part of the prior art:

* **§5** — `perf/bhpin-r1`'s original "neutral" result was measured on a fast
  path that executed **zero times** (0 hits in 5.6 M attempts).
* **§7** — DB_PRIVATE throughput on this box is **bimodal in memory layout**, a
  1.65× step selected by something as incidental as the *length* of the
  environment's path string. Any DB_PRIVATE A/B that does not hold placement
  constant can report a 1.65× "win" that is pure layout.

---

## 1. Rebases

All four were off older masters. Three rebased cleanly; one was ported.

| branch | commits | onto `v2026.09.6` | conflicts |
|---|---:|---|---|
| `perf/bhpin-r1` | 4 | **clean** (`git rebase --onto v2026.09.6 86074b6b6`) | none |
| `perf/rsnap-ml` | 3 | **clean** (cherry-pick) | none |
| `perf/lock-readpath` | 3 | **one-line conflict** | two local declarations added at the same spot in `__lock_get_internal`; resolved by keeping both (`int si_txn_lock;` and `int fast_grant, m;`) |
| `perf/mpool-pin` | +39 | **ported, not rebased** | see below |

`perf/mpool-pin` is +39 commits off a much older master (`e66a5e66d`) and 36 of
those 39 are already in `v2026.09.6` (SSI, CalVer, CI, Meson/Nix, Windows
fixes); `git cherry` reports only **3** commits not upstream, and two of those
are old CI/autoconf work. Its actual perf content is **one commit**, `a95ac48d5`,
which touches exactly two files. So rather than replay 39 commits, I
cherry-picked that one commit:

* **Ported:** the `MPOOL_HOTFIELDS_ISOLATED` layout of `struct __bh` in
  `src/dbinc/mp.h` — the write-hot fields (`ref`, `priority`) moved onto their
  own cache line behind 64-byte pads, away from the read-mostly
  identity/traversal fields (`pgno`, `mf_offset`, `flags`, `hq`).
* **Conflict resolved:** `v2026.09.6` added a `u_int8_t wired` field to `__bh`
  that did not exist when the branch was written; I placed it with the
  read-mostly group (it is written once when a page is wired and read on every
  optimistic probe, so it belongs with the read-mostly fields, not in the
  write-hot line).
* **Dropped:** the commit's other file, `docs/design/scaling-findings.md`, which
  no longer exists on master.
* Built with `-DMPOOL_HOTFIELDS_ISOLATED=1`, which is verified live: the arm's
  `sizeof(BH)` is **224** vs the baseline's **96** (§2).

---

## 2. Every arm is ARMED, not silently identical to base

A build that failed to pick up its change would present as a perfectly
reproducible NULL result. Each branch leaves a distinct struct fingerprint, read
from each build's own headers (`test/bench/pin_armed.c`):

| arm | `__env_struct_sig()` | DB/DBC/DB_ENV/DB_TXN | `BH` | `DB_MPOOL_HASH` | `DB_LOCKOBJ` | `BTREE` |
|---|---|---|---:|---:|---:|---:|
| base | `0xb86f77f0` | 1744/552/2088/336 | 96 | 56 | 136 | 152 |
| bhpin | `0x412dc438` | 1744/552/2088/336 | 96 | **128** | 136 | 152 |
| rsnap | `0xbd2f21d0` | 1744/552/2088/336 | 96 | 56 | 136 | **184** |
| lockrp | `0x530fb3f8` | 1744/552/2088/336 | 96 | 56 | **176** | 152 |
| mpoolp | `0x702ac9f0` | 1744/552/2088/336 | **224** | 56 | 136 | 152 |

Each arm moves exactly the struct its branch claims: the per-bucket seqlock
(`DB_MPOOL_HASH` 56→128), `bt_isnap[3]` (`BTREE` 152→184), `nheld[10]`
(`DB_LOCKOBJ` 136→176), the cache-line pads (`BH` 96→224). Public sizes are
unchanged in all five builds. `__env_struct_sig()` differs in all four arms,
which is also why every arm needs its own environment directory — and, as §7
shows, that necessity is what let a layout artifact into the first tables.

`ldd` verifies each bench binary links the tree under test
(`CC-OK <arm> -> /home/admin/wt-<arm>/build_unix/.libs/libdb-2026.0.so`).

---

## 3. Method

`test/bench/pin_bench.c` is `batch_bench.c` plus four things measurement needs:

1. **`DB_TXN_NOSYNC`** on the load — the load is autocommit `db->put`, i.e.
   fsync-per-put without it.
2. **A checkpoint + full trickle after the load.** This turned out to be
   load-bearing, not hygiene: without it every page stays `BH_DIRTY` for the
   whole run and `perf/bhpin-r1` is silently disarmed (§5).
3. **A warmup window before every measured window**, so a cache-warming curve
   cannot be read as a scaling curve.
4. **`PIN_PRIVATE`** — `DB_PRIVATE` + `set_thread_count`, which is the only
   configuration in which R1's fast path can fire at all.

Plus a hard non-vacuity rule: zero throughput or any worker error prints `FAIL`
and exits non-zero. `rc=0` alone is never a pass. All 1 412 data points below
carry a throughput number; **0** `FAIL` lines were produced.

**Design:** 6 arms (the 4 branches, the baseline, and *the baseline binary a
second time*) × 2 API paths (`DB->get` loop vs `db_get_multiple`) × 2 env kinds
(shared, `DB_PRIVATE`) × 3 thread counts (1, 32, 96) × 5 reps, **arm order
rotated one position every rep**, fresh environment per (arm, env, mode, rep),
200 000-key in-cache B-tree, 512 MB cache, batch = 16 keys, 2 s warmup + 5 s
measured.

**The noise floor is measured, not assumed.** The `base2` arm is the same
baseline binary as `base`; the deviation between them is the floor:

```
sweep 2 : base2-vs-base |deviation| max = 4.8%   median = 0.7%   over 12 cells
confirm : base2-vs-base |deviation| max = 2.7%   median = 1.3%   over 12 cells
```

**So any |difference| ≲ 5 % is a NULL result on this box, and is reported as
such.** (Within one run. §7 shows cross-run comparisons need more care.)

---

## 4. The premise: where self time actually goes

`perf record -F 97 --call-graph fp`, 96 threads, 8 s, same workload, production
builds. Self time, `--no-children`:

### Individual `DB->get` path, baseline — the old regime

```
85.16%  __db_tas_mutex_lock_int
 7.05%  __db_tas_mutex_unlock
 1.05%  __os_atomic_read
 0.78%  __db_cursor_int
 0.78%  __bam_cmp
 0.65%  __db_tas_mutex_readlock_int
 0.62%  __dbc_close
 0.58%  __memp_fget
VERDICT profile arm=base mode=indiv env=shared  pin_share=2.40%  mutex_share=92.88%
VERDICT profile arm=base mode=indiv env=private pin_share=2.28%  mutex_share=92.90%
```

### Batched `db_get_multiple` path, baseline — the new regime

```
34.31%  __os_atomic_read
28.11%  __memp_fget
 8.21%  __os_atomic_dec
 7.96%  __bam_cmp
 7.96%  __memp_fput
 2.44%  __db_tas_mutex_readlock_int
 2.39%  __db_tas_mutex_unlock
 1.32%  __lock_getobj
VERDICT profile arm=base mode=batch env=shared  pin_share=71.89%  mutex_share=5.61%
VERDICT profile arm=base mode=batch env=private pin_share=72.43%  mutex_share=6.09%
```

**The premise holds, and slightly stronger than claimed.** `BATCHED-READS-RESULTS.md`
said the pin becomes ~63 % of self time; on this box it is **71.9 %** (shared)
and **72.4 %** (private), while the mutex collapses from 92.9 % to 5.6 %. The
two API paths are genuinely two different regimes, and the four branches' old
verdicts describe the left column.

This is also why the cross of API paths mattered: at 96 threads the individual
path is *mutex-bound in every arm*, so it cannot distinguish them — every arm
lands within noise of base at `indiv/96` (0.964×–1.016×) no matter what it does
to the pin. The batched path is where a pin change is visible at all.

---

## 5. `perf/bhpin-r1`'s original result was measured on code that never ran

R1 keeps `DIAGNOSTIC`-only counters (`__memp_bhpin_hits` /
`__memp_bhpin_attempts`) precisely so this can be checked. Reading them after
the load, so only read traffic counts (`test/bench/pin_fires.c`):

```
VERDICT bhpin-fires mode=indiv env=private reads=4496672 attempts=8993346 hits=0 hit_rate=0.0%
VERDICT bhpin-fires mode=batch env=private reads=3862928 attempts=7725860 hits=0 hit_rate=0.0%
VERDICT bhpin-fires mode=indiv env=shared  reads=6876400 attempts=0       hits=0
VERDICT bhpin-fires mode=batch env=shared  reads=3974576 attempts=0       hits=0
```

**Zero hits in 9 M attempts.** Two separate reasons, both read off the code
rather than guessed — the branch ships a `R1_BHPIN_BREAKDOWN` build with
per-reason bail counters (`test/bench/pin_why.c`):

```
VERDICT bhpin-why mode=indiv env=private reads=2805872 attempts=5611746 hits=0
        notwired=2805872 badflag=2805874 chain=0 flagbits=0x6 unaccounted=0
```

1. **`badflag`, `flagbits=0x6`** = `BH_DIRTY|BH_DIRTY_CREATE`. The load dirties
   every page it touches; with `DB_TXN_NOSYNC` and no checkpoint they stay dirty
   for the entire run, and R1 refuses any dirty buffer. **A read benchmark run
   on a dirty buffer pool is not a read benchmark** — this is a harness defect,
   and it disarmed the very code under test.
2. **`notwired`** = the other half of the traffic is leaf pages, which
   `bt_search.c` deliberately does not wire.
3. **`attempts=0` for every shared-env run** — the path is gated on
   `F_ISSET(env, ENV_PRIVATE)`, so a shared environment never even attempts it.

R1's own report concluded "R1 is statistically neutral… removing its RMW buys
nothing measurable". That conclusion is not supported by its measurement: the
RMW was never removed in the run that produced it. The correct prior verdict is
**vacuous, not neutral** — and the anti-vacuous-green rule this project keeps
relearning applies to perf results, not just to tests.

Once the harness checkpoints after the load, the path fires. Measured from the
**production** build (necessary — see §6 — because the `DIAGNOSTIC` build cannot
survive the path firing), same binary, same directory, kill switch only:

```
=== arm=on   RESULT keys_per_sec=10727051
VERDICT fires-prod arm=on  readlock_self=2.11% all_mutex_self=8.56% memp_fget_self=10.21%
=== arm=off  RESULT keys_per_sec= 7776872
VERDICT fires-prod arm=off readlock_self=3.51% all_mutex_self=7.41% memp_fget_self=35.93%
```

`__memp_fget` self time **35.93 % → 10.21 %** at *higher* read volume. The path
fires, and it removes the cost it was designed to remove.

---

## 6. `perf/bhpin-r1` panics once it is armed — with a control

Arming the path exposes a crash in the `DIAGNOSTIC` build:

```
BDB2031 shared unlock 140737331915936 already unlocked
BDB0061 PANIC: Permission denied
BDB0060 PANIC: fatal region error detected; run recovery
DB->get: BDB0087 DB_RUNRECOVERY: Fatal error, run database recovery
```

Decoded (`addr2line`, same binary):

```
__os_stack   os_stack.c:38
__os_abort   os_abort.c:25
__env_panic  db_err.c:247
__memp_fput  mp_fput.c:197        <-- MUTEX_UNLOCK(env, bhp->mtx_buf)
__bam_search bt_search.c:1099
__bamc_search bt_cursor.c:2804
__bamc_get   bt_cursor.c:1099
__dbc_iget   db_cam.c:976
```

**Attribution by control, not by inspection** — same driver, same flags, same
environment, four builds (`test/bench/pinrm_panic_control.sh`):

```
PANICTEST label=basediag  env=private exit=0   panic_lines=0  result_lines=1
PANICTEST label=bhpindiag env=private exit=134 panic_lines=11 result_lines=0
PANICTEST label=baseprod  env=private exit=0   panic_lines=0  result_lines=1
PANICTEST label=bhpinprod env=private exit=0   panic_lines=0  result_lines=1
PANICTEST label=basediag  env=shared  exit=0   panic_lines=0  result_lines=1
PANICTEST label=bhpindiag env=shared  exit=0   panic_lines=0  result_lines=1
PANICTEST label=baseprod  env=shared  exit=0   panic_lines=0  result_lines=1
PANICTEST label=bhpinprod env=shared  exit=0   panic_lines=0  result_lines=1
```

Only `bhpin` + `DIAGNOSTIC` + `private` fails — i.e. only where R1's path
actually fires. The baseline is clean in the same configuration, so it is R1 and
not the harness's checkpoint.

**The bug.** R1 option (c) returns the frame having taken `bhp->ref` but
**deliberately no latch** — that is the entire point of the optimization. But
`__memp_fput` is unchanged and unlocks `bhp->mtx_buf` unconditionally on both
its exits (`mp_fput.c:197` for the refcount>1 path, `mp_fput.c:261` for the
final path). R1's earlier refcount-free variant handled the asymmetry with a
borrow sentinel in the pinlist; commit `201fdb507` **removed that sentinel
handling from `__memp_fput`** when it moved to option (c), but option (c) still
skips the latch — so nothing tells `fput` that this pin came in latch-free.

**Why the production build "passes" is the dangerous part.** The check that
fires is `DIAGNOSTIC`-only (`mut_tas.c:517`). Without it, the code reaches
`mut_tas.c:542`: `atomic_dec(&mutexp->sharecount)` on a latch this thread never
share-locked. That silently drives another reader's share count toward zero, and
`MUTEX_SHARE_ISEXCLUSIVE` is granted by CAS against `sharecount == 0`
(`mutex_int.h:1018`) — so a writer can take the exclusive latch while a genuine
reader still holds a share. Silent data corruption under a race, not a crash.
The 59.9 M-read production stress passing (§8) is therefore **not** evidence of
safety; it is evidence that the corruption window is narrow.

This is fixable — teach `__memp_fput` to recognise the latch-free pin (the
sentinel R1 already invented, or a per-`BH` flag) — but it is not fixed on the
branch, and the branch cannot ship as it stands.

---

## 7. DB_PRIVATE throughput on this box is bimodal in memory layout

Chasing a disagreement between two runs produced the most consequential finding
here, and it invalidates part of my own first tables.

The same baseline binary, same arguments, at `private/batch/96`:

```
sweep 1  : 7.77M 7.80M 7.90M 7.90M 8.03M   (CV 1.3%)
sweep 2  : 7.73M 7.95M 7.95M 7.96M 7.98M   (CV 1.3%)
sweep 3  : 7.59M                            (single rep)
confirm  : 4.80M 4.81M 4.85M 4.86M 4.86M   (CV 0.6%)
```

Each run internally tight, the runs 1.65× apart. The box was idle
(load < 4, 100 % idle in `vmstat`, no steal, single NUMA node, fixed 2.4 GHz, no
`cpufreq`). Directory reuse vs fresh directories is **not** the cause (5.43M vs
5.50M, interleaved). What *is* the cause:

```
RESULT tag=len42/private/rep1 keys_per_sec=8005933      # /home/admin/runs/env-base-private-batch-r1
RESULT tag=len38/private/rep1 keys_per_sec=4818783      # /home/admin/runs/cf-base-private-batch
RESULT tag=len44/private/rep1 keys_per_sec=8030397
RESULT tag=len18/private/rep1 keys_per_sec=4779371
RESULT tag=len18/private/rep2 keys_per_sec=4791118
RESULT tag=len44/private/rep2 keys_per_sec=8016438
RESULT tag=len38/private/rep2 keys_per_sec=4882594
RESULT tag=len42/private/rep2 keys_per_sec=7892882
RESULT tag=len42/private/rep3 keys_per_sec=7918252
RESULT tag=len38/private/rep3 keys_per_sec=4791315
RESULT tag=len44/private/rep3 keys_per_sec=7807065
RESULT tag=len18/private/rep3 keys_per_sec=4917005
```

**The length of the `$PIN_HOME` path selects the mode**, reproducibly, with the
arms interleaved. Content is irrelevant, only length: two 38-char names of
different content give 4.73M / 4.79M, two 42-char names give 7.87M / 7.87M.
ASLR is off on this box (`randomize_va_space=0`), so this is deterministic
placement, not noise. Padding the environment block alone does *not* move it
(4.73M–4.92M across four padding sizes at a fixed 38-char home), so it is the
region layout the path length induces, not the env block size — consistent with
cache-set aliasing of a 512 MB `DB_PRIVATE` region reached through a
`DB_MPOOL_HASH` table whose buckets are indexed off `mf_offset`.

**The shared-env numbers are unaffected**: base `shared/batch/96` is
5.299M / 5.304M / 5.317M / 5.366M across four independent runs.

**Consequence for the method.** `pinrm_sweep.sh` gives each arm its own
directory name (they must not share an environment — the arms have different
`__env_struct_sig()`), so every **cross-arm DB_PRIVATE** ratio it reports is
confounded with a possible 1.65× layout step. The shared-env half of the sweep
is clean. This is exactly the trap that would manufacture a win, so the
DB_PRIVATE claim for R1 is settled instead by the unconfounded design in §8.4:
**one binary, one directory reused by both arms, only the kill switch varying,
repeated at a path length from each mode**.

`pin_bench` now prints `homelen` and the largest anonymous mapping with every
run, so placement is recorded rather than invisible.

---

## 8. Measured tables

### 8.1 Baseline shape (v2026.09.6, for orientation)

| env | api | t=1 | t=32 | t=96 |
|---|---|---:|---:|---:|
| shared | indiv | 745 237 | 3 539 366 | 2 064 478 |
| shared | batch | 868 702 | 3 893 291 | 5 299 121 |
| private | indiv | 755 920 | 4 009 447 | 2 116 369 |
| private | batch | 865 785 | 5 756 753 | 7 950 464 |

The documented shape reproduces: the individual path peaks at 32 threads and
declines to 96; the batched path keeps climbing.

### 8.2 Shared environment — unconfounded, 5 reps, rotating order

| api | thr | base | base2 | bhpin | rsnap | lockrp | mpoolp |
|---|---:|---:|---:|---:|---:|---:|---:|
| indiv | 1 | 745 237 (CV 1.1) | 1.002× | 0.994× | **0.362×** | 0.993× | 1.001× |
| indiv | 32 | 3 539 366 (2.0) | 0.992× | **0.893×** | **0.583×** | 1.016× | 1.011× |
| indiv | 96 | 2 064 478 (4.1) | 1.013× | 1.003× | 1.032× | 0.990× | 0.989× |
| batch | 1 | 868 702 (0.8) | 1.002× | 0.990× | **0.328×** | 0.997× | 0.985× |
| batch | 32 | 3 893 291 (3.3) | 0.952× | **0.880×** | **0.318×** | 1.008× | 1.019× |
| batch | 96 | 5 299 121 (1.0) | 0.995× | **0.885×** | 1.042× | 1.003× | 0.999× |

Arm CVs 0.4–4.7 %. Bold = outside the ±4.8 % floor.

### 8.3 DB_PRIVATE environment — **cross-arm ratios here are confounded by §7**

| api | thr | base | base2 | bhpin | rsnap | lockrp | mpoolp |
|---|---:|---:|---:|---:|---:|---:|---:|
| indiv | 1 | 755 920 (0.6) | 0.988× | 1.027× | **0.353×** | 0.982× | 0.982× |
| indiv | 32 | 4 009 447 (1.4) | 0.993× | 0.974× | **0.365×** | 0.995× | 1.018× |
| indiv | 96 | 2 116 369 (4.6) | 0.971× | 1.012× | 1.015× | 0.964× | 0.974× |
| batch | 1 | 865 785 (0.1) | 1.006× | 1.048× | **0.329×** | 1.003× | 0.995× |
| batch | 32 | 5 756 753 (2.1) | 0.990× | *1.211×* | **0.063×** | 0.987× | 1.001× |
| batch | 96 | 7 950 464 (1.3) | 1.006× | *1.353×* | 1.032× | 0.978× | 1.017× |

*Italic* = large but potentially layout-confounded; settled in §8.4. The rsnap
regressions are far too large to be layout (0.063× is a 16× loss) and the
lockrp/mpoolp cells are inside the floor either way.

### 8.4 The unconfounded R1 test: one binary, one directory, kill switch only

6 reps, arms alternating, `DB_NO_BHPIN` the only variable, run at both layout
modes (`test/bench/pinrm_final_ab.sh`, 288 points):

**home path length 38 (lower layout mode)**

| env | api | thr | R1 ON | CV% | R1 OFF | CV% | ON/OFF |
|---|---|---:|---:|---:|---:|---:|---:|
| private | indiv | 1 | 775 558 | 0.7 | 747 406 | 0.9 | 1.038× |
| private | indiv | 32 | 3 972 539 | 2.4 | 3 147 901 | 3.6 | **1.262×** |
| private | indiv | 96 | 2 095 081 | 2.1 | 2 165 944 | 2.7 | 0.967× |
| private | batch | 1 | 910 328 | 1.0 | 875 748 | 1.0 | 1.039× |
| private | batch | 32 | 5 286 884 | 2.1 | 3 339 790 | 3.1 | **1.583×** |
| private | batch | 96 | 7 385 678 | 1.1 | 4 773 202 | 1.9 | **1.547×** |
| shared | indiv | 1/32/96 | — | — | — | — | 1.000× / 1.010× / 0.993× |
| shared | batch | 1/32/96 | — | — | — | — | 1.004× / 1.039× / 0.995× |

**home path length 42 (upper layout mode)**

| env | api | thr | R1 ON | CV% | R1 OFF | CV% | ON/OFF |
|---|---|---:|---:|---:|---:|---:|---:|
| private | indiv | 1 | 781 536 | 0.8 | 750 206 | 0.6 | 1.042× |
| private | indiv | 32 | 3 895 740 | 2.0 | 3 920 421 | 2.2 | 0.994× |
| private | indiv | 96 | 2 039 861 | 5.6 | 2 070 094 | 3.4 | 0.985× |
| private | batch | 1 | 917 730 | 0.8 | 874 631 | 0.6 | 1.049× |
| private | batch | 32 | 6 905 010 | 4.0 | 5 795 507 | 1.8 | **1.191×** |
| private | batch | 96 | 10 771 022 | 2.2 | 7 943 765 | 1.1 | **1.356×** |
| shared | indiv | 1/32/96 | — | — | — | — | 0.993× / 1.005× / 1.005× |
| shared | batch | 1/32/96 | — | — | — | — | 0.993× / 1.010× / 1.001× |

**R1's win survives the unconfounded test, in both layout modes**: batched
DB_PRIVATE **1.55× / 1.36×** at 96 threads and **1.58× / 1.19×** at 32, CV
1.1–4.0 %, with the shared arm dead flat (0.993–1.039×) as its `ENV_PRIVATE`
gate predicts. Note the layout mode changes the *size* of the win (1.55× vs
1.36×) because it changes the baseline, which is itself a demonstration of why
§7 matters.

### 8.5 Profile of the winner vs the baseline (private, batched, t=96)

| symbol (self) | base | bhpin |
|---|---:|---:|
| `__os_atomic_read` | 33.97 % | 30.54 % |
| `__memp_fget` | 28.33 % | **11.27 %** |
| `__memp_fput` | 6.00 % | **27.67 %** |
| `__os_atomic_dec` | 8.93 % | 13.64 % |
| `__bam_cmp` | 8.95 % | 3.78 % |
| pin share | **72.43 %** | 56.16 % |
| mutex share | 6.09 % | 5.41 % |

R1 removes work from `__memp_fget` (28.3 % → 11.3 %) and the total pin share
drops 72.4 % → 56.2 %, so the gain is where the design said it would be. It does
**not** remove the pin: `__os_atomic_read` is still 30.5 % and `__memp_fput`
*grows* to 27.7 %, because option (c) still takes `bhp->ref` and `fput` still
pays the full latched release. That asymmetry is the same one that causes the
panic in §6 — and it also means the remaining ~56 % is still on the table for a
correct design.

---

## 9. Verdicts

### `perf/mpool-pin` — **NULL**

Ported (one commit, §1), armed (`BH` 96→224, §2). Cells: 0.982×–1.033× across
all 12; largest deviation 3.3 % against a 4.8 % floor. The branch's own
conclusion — that the read-path cost is *true* sharing of the atomic counters,
not false sharing with neighbouring fields — is now confirmed in the regime
where those counters are 72 % of self time rather than 2 %. **The premise was
right that its verdict was taken behind a wall; removing the wall does not change
its verdict.** This is the strongest possible form of a negative result: the
hypothesis was retested where it should have shone, and it did not.

### `perf/bhpin-r1` — **REGRESSION as it stands; the underlying lever is a real 1.5×**

Three separate findings, and the order matters:

1. Its original "neutral" verdict was **vacuous** — 0 hits in 9 M attempts (§5).
2. Armed, it is a genuine **1.55× / 1.36×** at private/batch/96 by the
   unconfounded same-binary test (§8.4), with the profile confirming the
   mechanism (§8.5).
3. It is **0.885× at shared/batch/96 and 0.880–0.893× at 32 threads** (§8.2) —
   an 11–12 % regression, well outside the floor, in the environment where its
   fast path provably never fires (`attempts=0`). It pays the cost of the
   change — `DB_MPOOL_HASH` 56→128 bytes, and a seqlock bump on every
   `mtx_hash`-exclusive mutator — for none of the benefit. Shared environments
   are the default deployment.
4. It **panics** once armed, at `mp_fput.c:197`, controls clean on base (§6),
   and the production build's silence is a missing check rather than safety.

**Verdict: REGRESSION / do not merge — but do not park it either.** This is the
only one of the four that attacks the now-dominant cost and shows a large
measured win where it runs. It needs: `__memp_fput` taught about the latch-free
pin (the bug), and the shared-env path either supported or made free
(the regression). That is follow-up work, not a re-park.

### `perf/rsnap-ml` — **REGRESSION, confirmed and worse than reported**

Armed (`BTREE` 152→184). The old report measured 7–13× down; against
`v2026.09.6` it is **0.328×–0.362× at 1 thread**, **0.318×/0.583× at
shared/32**, and **0.063× at private/batch/32** (a 16× loss). Note it is
*neutral to slightly positive* at 96 threads (1.015×–1.042×) — not because the
layer got better but because at 96 threads on the individual path everything is
mutex-bound, and on the batched path its per-descent `__os_malloc` + full-page
`memcpy` is hidden by contention. Its 45.1 % CV at private/batch/32 is the
3-slot cache thrashing, visible as variance. The branch's own root cause — there
is no small stable hot working set below the root, so a 3-slot per-handle cache
thrashes — is unaffected by the cursor mutex being gone. **Confirmed
regression; keep parked.**

### `perf/lock-readpath` — **NULL**

Rebased with one trivial declaration conflict; armed (`DB_LOCKOBJ` 136→176).
Cells: 0.964×–1.019× across all 12, largest deviation 3.6 % against a 4.8 %
floor. Its own diagnosis — the hot-key wall is the mutex *acquire* and the
two-lock-per-read structure, not the holder-walk hold time — is untouched by the
cursor mutex removal. Worth noting *why* it cannot win in this workload: at
`shared/batch/96` the lock manager is only ~2.8 % of self time
(`__lock_getobj` 1.32 % + `__lock_get_internal` 1.19 %), so even eliminating it
entirely could not produce a measurable gain. **NULL; keep parked.**

---

## 10. Correctness

Run on the arms that looked like a win (`test/bench/pinrm_correctness.sh`,
`pinrm_stress.sh`):

| gate | base | bhpin |
|---|---|---|
| `db_verify` on an env the arm's own library built | **CLEAN** (`BDB5105 Verification of bench.db succeeded.`) | **CLEAN** |
| `batch_diff` arm `indiv` | PASS, 4 verdicts | PASS, 4 verdicts |
| `batch_diff` arm `batch` | PASS, 4 verdicts | PASS, 4 verdicts |
| `batch_diff` arm `both` | PASS, 4 verdicts | PASS, 4 verdicts |
| — write skew prevented under `DB_TXN_SERIALIZABLE` | 10/10 indiv, 10/10 batch, lock-resolved 0 | 10/10 indiv, 10/10 batch, lock-resolved 0 |
| — snapshot control (skew must be *allowed*) | allowed 10/10 | allowed 10/10 |
| DB_PRIVATE stress, R1 ON, 48 readers + 16 writers, 20 s | 59.1 M reads, 0 mismatch, 0 error, `db_verify` CLEAN | 59.9 M reads, 0 mismatch, 0 error, `db_verify` CLEAN |
| DB_PRIVATE stress, R1 OFF | 60.3 M reads, 0 mismatch | 59.9 M reads, 0 mismatch |
| `DIAGNOSTIC` build, DB_PRIVATE, path armed | clean | **PANIC** (§6) |

**Two honest caveats about these gates, both of which cut against R1:**

* **`batch_diff` is vacuous for R1.** It opens a *shared* environment
  (`test/c/batch_diff.c:620`), and R1's own counters report `attempts=0` in a
  shared env. Its three PASSes on the bhpin build are real evidence about
  `db_get_multiple` and **no evidence at all** about R1. That is why
  `pin_stress.c` exists — a DB_PRIVATE reader/writer stress is the only
  correctness signal here that touches the code under test.
* **The 59.9 M-read production stress passing does not clear R1**, for the reason
  in §6: the accounting error it commits is only *detected* in a `DIAGNOSTIC`
  build. A clean production run means the race window is narrow, not absent.

---

## 11. Overall recommendation

**Ship none of the four as they stand.**

* `perf/mpool-pin` — **NULL**, retested in the regime that should have favoured
  it. Close it; the question is answered.
* `perf/lock-readpath` — **NULL**, and the lock manager is only ~2.8 % of self
  time in this workload, so it could not have won. Keep parked.
* `perf/rsnap-ml` — **REGRESSION** confirmed (0.063×–0.583× where it bites).
  Keep parked.
* `perf/bhpin-r1` — **do not merge, do not re-park.** It has an 11–12 %
  regression in the default (shared) environment where its path cannot fire, and
  a latent shared-latch accounting bug that a `DIAGNOSTIC` build turns into a
  panic and a production build turns into a silent race. But it is the only one
  of the four aimed at the cost that now dominates, and where it fires it is
  **1.36×–1.55×** by the strictest test available. It is a live lead with two
  named, bounded defects.

**What the re-measurement actually established.** The premise was correct:
`__memp_fget` + pin atomics are now 72 % of self time on the batched path (2.4 %
on the individual path), and the four verdicts were formed in a regime that no
longer exists. But re-measuring in the new regime rescued none of the four, for
three different reasons — one hypothesis was simply wrong (`mpool-pin`: false
sharing was never the problem), one attacks a cost that is now negligible
(`lock-readpath`: 2.8 %), one has a design flaw the cursor mutex was masking
(`rsnap-ml`). Only the fourth was mismeasured, and correcting *its* measurement
turned a "neutral" into both a 1.5× lead and a correctness bug.

**And a methodological result that outlives all four verdicts:** on this box,
DB_PRIVATE throughput is bimodal in memory layout with a 1.65× step selected by
the length of a path string (§7), and different arms need different environment
directories because their region signatures differ. Any future DB_PRIVATE A/B
here that does not hold placement constant — including R1's original one — can
report a 1.65× effect that is pure layout. Same binary, same directory, one
variable, or the number means nothing.

### Follow-ups this work identified (not done here)

1. **`__memp_fput` must recognise a latch-free pin** before R1 can be
   considered. `201fdb507` removed the borrow-sentinel handling when it moved to
   option (c), but option (c) still skips the latch.
2. **R1's shared-env regression** (0.885×) needs either a shared-env fast path or
   a cheaper disabled path; as gated, shared environments pay and never benefit.
3. **`mut_tas.c`'s share-count underflow check is `DIAGNOSTIC`-only.** A
   mismatched shared unlock silently corrupts the share count in production
   builds. Making it cheap-but-always-on (or asserting in the mpool layer) would
   have caught this class of bug at its first firing.
4. **~56 % of self time is still the pin** even with R1 on (§8.5), because
   option (c) keeps `bhp->ref` and `__memp_fput` grows to 27.7 %. A design that
   removes the refcount RMW on both ends is the remaining lever.
5. **`batch_diff` should gain a DB_PRIVATE mode**, so it is not vacuous for any
   future change gated on `ENV_PRIVATE`.

---

## Appendix — reproducing

```sh
# on the box: five worktrees + five builds (mpoolp gets -DMPOOL_HOTFIELDS_ISOLATED=1)
bash test/bench/pinrm_setup.sh
bash test/bench/pinrm_build_bench.sh     # ldd-verifies each arm links its own tree
bash test/bench/pinrm_armed.sh           # struct fingerprints: no arm is silently base

# the sweep (6 arms incl. base twice, 2 API paths, 2 env kinds, {1,32,96}, 5 reps)
bash test/bench/pinrm_sweep.sh 5 runs/sweep.txt 2 5
python3 test/bench/pinrm_report.py runs/sweep.txt --md

# R1: does it fire, why not, and the unconfounded A/B
bash test/bench/pinrm_bhpin_fires.sh     # DIAGNOSTIC counters
bash test/bench/pinrm_bhpin_why.sh       # per-reason bail counters
bash test/bench/pinrm_fires_prod.sh      # firing observed from a production build
bash test/bench/pinrm_final_ab.sh 6 runs/final_ab.txt 5
python3 test/bench/pinrm_ab_report.py runs/final_ab.txt r1on-len42 r1off-len42

# the panic, with its control
bash test/bench/pinrm_panic_control.sh

# the layout artifact
bash test/bench/pinrm_drift.sh 4 runs/drift.txt
bash test/bench/pinrm_pathlen.sh 3 runs/pathlen.txt

# profiles and correctness
bash test/bench/pinrm_profile.sh base  batch shared 8
bash test/bench/pinrm_profile.sh bhpin batch private 8
bash test/bench/pinrm_correctness.sh bhpin
bash test/bench/pinrm_stress.sh bhpin 20
```

Rebased branches (local, pushed nowhere): `rm/bhpin-r1`, `rm/rsnap-ml-r`,
`rm/lock-readpath` are `v2026.09.6` + the branch commits; `rm/mpool-pin-port` is
`v2026.09.6` + the single cherry-picked `a95ac48d5`.

No driver in this branch removes anything: every script pre-creates its
directory and empties it with `find <dir> -mindepth 1 -delete`.

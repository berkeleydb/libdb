<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# P13 + P12 re-evaluation — report

> **P13 is implemented and NOT yet merged.** It is a measured ~1500x fix on
> FreeBSD (F4) with a byte-identical region signature, so unlike P12 it ships
> without breaking existing environments. But the **four-arm Linux matrix was
> not measured** — the only available box gave 61–79% cv against a 10% ceiling —
> so the maintainer's hypothesis (does P12 stop being a regression once P13 is
> in?) is **unanswered**. See §7. Two further defects were found on the way:
> **F5** (a second site of the F4 mechanism that caps FreeBSD at one thread) and
> **F6** (a pre-existing double-free, proved pre-existing on a pristine tree).


Working tree left with **P13 only** applied, uncommitted and unpushed, as
instructed (`src/lock/lock_id.c`, `src/lock/lock_region.c`, plus an F6 row in
`test/KNOWN-ISSUES.md`). The P12 and P13+P12 arms exist as patch files
(`/tmp/P13.patch`, `/tmp/P12P13.patch`) and as built trees, not in the tree.

---

## 0. Summary — what is established and what is not

| Claim | Status |
|---|---|
| The blocker's cause | **Proved.** FreeBSD libthr runs an O(N) GC per process-shared mutex destroy. Filed as **F4**. |
| P13 fixes it | **Measured.** FreeBSD t=1: **121,772 commits vs 79**, one binary, runtime switch. |
| A second site of the same defect | **Found and characterised, not fixed.** Filed as **F5**. It caps FreeBSD at one thread. |
| P13 breaks no existing environment | **Measured.** `__env_struct_sig()` byte-identical to baseline. |
| P13 correctness (4 questions + 5 gates) | **All answered/passed**, including two sabotage arms. |
| A pre-existing double-free found while gating | **Proved pre-existing.** Filed as **F6**. |
| **The four-arm Linux matrix** | **NOT MEASURED.** cv 61–79% on the only Linux box available, against a 10% ceiling. No delta quoted. |
| **The maintainer's hypothesis (does P13 rescue P12?)** | **UNANSWERED.** See §7. This is the headline gap. |

The single most useful sentence for a user:

> **With P13 in place FreeBSD does 117,124 commits at t=1 and 1,644 at t=2, so
> libdb currently does not scale past one thread on FreeBSD. F4 and F5 together
> are the reason.**

---

## 1. The blocker: explained, and it is a shipped FreeBSD defect (F4)

TPROC-C ran ~1000× slower than Linux at t=1 with no I/O and no contention.

**Proved by `dtrace` on the live benchmark, not inferred.** The hot stack:

```
_umtx_op  <- pthread_mutex_destroy / pthread_cond_destroy
          <- __db_pthread_mutex_destroy
          <- __mutex_free_int
          <- __lock_freelocker_int
          <- __txn_end  <- __txn_commit
```

`__mutex_free_int` measured at **25 ms per call** (6.45 s over 257 calls) — which
*is* the whole 113 ms transaction. Every transaction type showed an identical
p50 of 113,664 µs, the signature of one common fixed cost rather than per-type
work.

### Mechanism, quotable

> FreeBSD libthr's `pshared_gc()` (`lib/libthr/thread/thr_pshared.c`) runs on
> **every** `pthread_mutex_destroy`/`pthread_cond_destroy` of a
> `PTHREAD_PROCESS_SHARED` object, and walks the **entire** process-wide pshared
> hash — issuing one `_umtx_op(UMTX_OP_SHM, UMTX_SHM_ALIVE)` **syscall per live
> entry**. The cost of destroying one shared mutex is therefore O(number of
> shared pthread objects the process holds), in syscalls. libdb has one
> `DB_MUTEX` per mpool buffer header, so a normal cache puts hundreds of
> thousands of entries in that hash.

libthr's own source comment concedes the design:

> *"Among all processes sharing a lock only one executes
> pthread_lock_destroy(). Other processes still have the hash and mapped
> off-page. Mitigate the problem by checking the liveness of all hashed keys
> periodically. **Right now this is executed on each pthread_lock_destroy(), but
> may be done less often if found to be too time-consuming.**"*

### Evidence

| Measurement | Value |
|---|---|
| `UMTX_SHM_ALIVE` per single destroy | **~110,000** |
| `UMTX_SHM_ALIVE` in 8 s vs destroys in 8 s | 14.5 M vs ~130 |
| `_umtx_op` by op (8 s) | `UMTX_SHM_ALIVE` 8,853,925; `CREAT` 43,179; `DESTROY` 43,213 |

**Standalone probe, no libdb involved** (`/tmp/gcprobe.c`): hold N live
`PROCESS_SHARED` mutexes, time one `pthread_mutex_destroy`.

| live shared objs | FreeBSD | Linux |
|---:|---:|---:|
| 0 | 3.8 µs | 1.1 µs |
| 500 | 76.3 µs | — |
| 2,000 | 286.7 µs | — |
| 8,000 | 1,177.6 µs | — |
| 32,000 | **5,245.4 µs** | — |
| 100,000 | (test timed out at 240 s) | **<1 µs (flat)** |

Strictly linear at ~0.15 µs/entry on FreeBSD; flat on Linux. **So this is
neither a host artifact nor a benchmark artifact.** It is libdb's per-locker
`__mutex_alloc`/`__mutex_free` — P13's exact subject — colliding with an O(N)
libthr GC.

A second probe (`/tmp/pshinit.c`) isolates init+destroy, which the pre-existing
`/nvme/pshared.c` probe did not measure (it timed lock/unlock of an
already-initialised mutex, and correctly concluded that could not explain 50 ms
— it measured the wrong operation):

| | FreeBSD | Linux |
|---|---:|---:|
| PRIVATE mutex init+lock+unlock+destroy | 73.6 ns | 375.2 ns |
| SHARED mutex init+lock+unlock+destroy | 3,325.8 ns | 1,025.2 ns |
| SHARED mutex+cond (the hybrid/self-block shape libdb uses) | **6,590.9 ns** | 457.6 ns |
| ratio shared+cond / private | **89.5×** | **1.2×** |

### Should this go upstream to FreeBSD?

**Yes, in my judgement, with a caveat.** The case for reporting is strong: the
cost is superlinear in a way no caller can see or avoid, the comment shows the
authors already anticipated it, and the fix is local (amortise the GC — run it
every Nth destroy, or on a timer, or only when the hash exceeds some size).
A 200-line reproducer with no libdb dependency already exists (`/tmp/gcprobe.c`)
and shows the linearity in one table, which is the hard part of such a report.

The caveat: libdb is also unusual in holding ~400,000 pshared objects, and a
reviewer may reasonably say the GC was designed for processes holding a handful.
That argues for reporting it as *"this design is O(N) per destroy and here is a
workload where N is 400,000"* rather than as a straightforward bug. **Not filed
by me** — that is a maintainer decision about representing the project upstream.

---

## 2. F5: a second site of the same mechanism, which caps FreeBSD at one thread

With P13 in place, FreeBSD t=1 is 117,124 commits but **t=2 is 1,644** and t=4
is 1,348. `dtrace` shows the entire t≥2 profile is:

```
_umtx_op  <- pthread_cond_destroy / pthread_mutex_destroy
          <- __db_pthread_mutex_destroy
          <- __mutex_refresh            (src/mutex/mut_alloc.c)
          <- __lock_freelock            (src/lock/lock.c:2104)
          <- __lock_get_internal
```

Same libthr `pshared_gc` mechanism, **different caller**. `__mutex_refresh` is
destroy+init, so it pays the identical per-destroy hash walk, and it runs **per
LOCK free** rather than per locker. It fires only on the contended branch
(`lockp->status != DB_LSTAT_HELD && != DB_LSTAT_EXPIRED`), which is exactly why
t=1 is clean and t≥2 is not.

**Deliberately not folded into P13**, and verified as out of scope:
`__mutex_refresh` does **not** take `MUTEX_SYSTEM_LOCK`, so it is neither the
admission-control latch nor the thing P12 exposed.

A fix must first answer the question the code itself poses at `lock.c:2097` —
*"If the lock is not held we cannot be sure of its mutex state so we refresh
it"* — i.e. **why a destroy+init is required rather than an unlock or a
re-lock.** Two directions, neither chosen:

1. establish that the mutex state *is* knowable here and replace the refresh
   with an unlock; or
2. keep the refresh but stop it destroying a pshared object — reset rather than
   recreate.

Characterised rather than patched: the `DB_MUTEX_SELF_BLOCK` state machine
should not be changed under time pressure. Filed as **F5** with the stack, the
contended-branch condition, and the per-lock-vs-per-locker distinction.

---

## 3. P13: what the change is

`src/lock/lock_id.c`, `src/lock/lock_region.c`. **+165 / −10 lines.**

`__lock_freelocker_int` **retains** `sh_locker->mtx_locker` on the free list
instead of calling `__mutex_free`; `__lock_getlocker_int` **reuses** it instead
of calling `__mutex_alloc`. The hot path therefore takes `MUTEX_SYSTEM_LOCK`
**zero** times per transaction instead of twice.

This is a smaller change than the `lock_alloc.incl` sharding shape the handoff
suggested, and strictly better than sharding would be: it *removes* the
acquisitions rather than spreading them, so there is no residual global latch
for a future de-serialisation to expose.

Supporting details that are easy to get wrong and were each checked:

- `__env_alloc` memory is **not** zeroed (it is `CLEAR_BYTE`-filled only under
  `DIAGNOSTIC`), and the reuse test reads `mtx_locker` on a never-used locker.
  Both the initial free list (`lock_region.c`) and the refill batch
  (`lock_id.c`) now set `MUTEX_INVALID` explicitly.
- The mutex is acquired **after** the locker is popped, so the
  allocation-failure path returns the locker to the free list rather than
  leaking it.
- Switch: **`DB_NO_LOCKER_MUTEX_REUSE`** restores pre-P13 behaviour, so the A/B
  runs on one binary. Process-local static cache, following the
  `DB_NO_OPTREAD` (`bt_search.c:106`) and `DB_NO_GROUP_COMMIT`
  (`log_put.c:988`) precedent.

### Measured effect on FreeBSD (the platform where F4 bites)

One binary, one directory, runtime switch, t=1, scale 2:

| arm | commits / 10 s |
|---|---:|
| P13 **on** | **121,772** |
| P13 **off** (`DB_NO_LOCKER_MUTEX_REUSE=1`) | **79** |

Bulk load, same binary: **7,625 → 88,307 rows/s** (11.6×).

Mechanism confirmed by counters, not just throughput: with P13 on,
`__mutex_free_int` calls inside the measurement window go to **zero**.

---

## 4. The four correctness questions

### 4.1 FAILCHK — real, and now guarded

**`lock_failchk.c:170` does reach the retain branch for a DEAD process's
locker.** Confirmed by instrumenting the branch itself:

```
P13TRACE free id=80000002 mtx=1769 reuse=1 failchk=0
P13TRACE free id=1        mtx=1768 reuse=1 failchk=0
P13TRACE free id=2        mtx=1767 reuse=1 failchk=1   <- dead child's cursor locker
P13TRACE free id=3        mtx=1768 reuse=1 failchk=1   <- dead child's cursor locker
P13TRACE free id=80000004 mtx=1780 reuse=1 failchk=0
```

`failchk=1` on exactly the two dead-child lockers and on nothing else.

Both hazards the maintainer named are real:

1. unlocking a mutex this thread does not own is undefined for pthreads; and
2. a retained-but-still-locked mutex would **self-deadlock** the next locker to
   recycle the slot, at the `MUTEX_LOCK` in `__lock_getlocker_int`.

**Fix: `__lock_freelocker_int` keeps `__mutex_free` when `DB_ENV_FAILCHK` is
set** — the narrow fix the maintainer proposed. `DB_ENV_FAILCHK` is set for the
whole failchk run (`env_failchk.c:85`), so it is a reliable discriminator, and
`__mutex_free` → `__mutex_destroy` already has explicit failchk handling
(`mut_pthread.c:723` skips the destroy for the failchk thread rather than
trusting the state). Costs nothing: failchk is not a hot path.

**A gate-quality finding: `run_s5_failchk_spin.sh` has NO teeth for this.** It
**passes with the guard removed** — verified by building the sabotage and
re-running it. S5 stops at *"did failchk terminate"* and never reuses the
reclaimed locker afterwards. So I wrote `/tmp/p13fc.c`, which churns 200 lockers
through the reclaimed slot after failchk. Judged on a VERDICT line and on
forward progress, because the failure mode is a hang.

Writing that test exposed a second trap worth recording: **my first version used
`txn_begin`, and passed vacuously.** `lock_failchk.c:170` only frees lockers
with `id < TXN_MINIMUM` — i.e. **cursor** lockers, not transaction lockers — so
a child dying inside a transaction never reaches the branch at all. The test had
to be rewritten to die holding an open **cursor**.

### 4.2 REGION CLEANUP / the mutex leak — not a leak, and measured not argued

The maintainer was right that nothing walked `free_lockers` at shutdown. Added
to `__lock_env_refresh`, **`ENV_PRIVATE` arm only**: a shared region outlives
the process, and its retained mutexes belong to the next attacher, so freeing
them there would corrupt a live region.

The decisive measurement is **inside one environment lifetime** (sampling once
per fresh env cannot see the difference — closing the env resets the count
either way, and my first version of this test made exactly that mistake):

| shape | arm | inuse after 500 txns | after 3,000 txns |
|---|---|---:|---:|
| SEQ (1 locker live) | P13 | 2,789 | **2,789** |
| SEQ | control | 2,788 | 2,788 |
| CONC (64 live) | P13 | 2,852 | **2,852** |
| CONC | control | 2,788 | 2,788 |

**The retained set is bounded by *concurrently-live* lockers, not by churn:**
+1 at concurrency 1, +64 at concurrency 64, then **flat across 3,000
transactions**. That is the same bound `free_lockers` itself already has.
Control flat at 0. Same result on Linux (+1 / +64, flat).

**The test has teeth:** a sabotage arm that grows concurrency per round reports
`VERDICT p13_mutex_leak LEAK  SEQ: RISING first=2789 last=2949 delta=160`.

### 4.3 MULTI-PROCESS validity — stated, as asked

`mtx_locker` is a `db_mutex_t`, which is an **index into the shared mutex
array** (`MUTEXP_SET`, `mutex_int.h`), not a pointer and not process-local
memory. On a shared environment the locker mutex is allocated with neither
`DB_MUTEX_PROCESS_ONLY` nor `ENV_PRIVATE` semantics. So a mutex retained by
process A and inherited by a locker that process B later recycles resolves, in
B, to the same shared `DB_MUTEX` — which is precisely the property that makes
`mtx_locker` usable for cross-process blocking in the first place
(`lock.c:1591`, `:1613` copy it into a `DB_LOCK`'s `mtx_lock` so a waiter in
*another* process can block on it).

**Retaining it adds no new cross-process assumption.** Written into the code
comment rather than left implicit.

**What I did not do: run two processes against one region under load.** The
reasoning above is a code argument, not a measurement. It is the same gap P12's
report declared (§6, "Multi-process region sharing"). The failchk test
(`/tmp/p13fc.c`) *is* genuinely multi-process — a real forked child, a real
death, a real cross-process reclaim — so the retain/reuse path has been
exercised across processes, but not under concurrent load.

### 4.4 `DB_MUTEX_SELF_BLOCK` carried state — answered

`mut_tas.c:203-205` and `mut_pthread.c:426`/`:446` write `mutexp->pid`/`tid` on
**acquire**, and the create path's `MUTEX_LOCK` re-writes both before the locker
is reachable. So the pid/tid always describe the current holder, not the
previous one.

The only readers that treat them as meaningful are the failchk paths
(`mut_pthread.c:247`, `mut_tas.c:151`, `mut_failchk.c:50`), which ask
*"is the recorded holder still alive?"* — and §4.1's guard keeps failchk on the
pre-P13 `__mutex_free` path, so no reused mutex is ever presented to them.

A pending waiter cannot be inherited either: the locker is unreachable at free
time (off its bucket chain, `heldby` empty, so no `DB_LOCK` references its
mutex), which is the same invariant `__mutex_refresh` already relies on for
`lock.c`'s `DB_LOCK` mutexes.

---

## 5. Gates

| Gate | Build | Result |
|---|---|---|
| `test/db/run_all.sh` | default | **22 PASS**, 2 pre-existing failures (below) |
| `test/db/run_all.sh` | `--enable-diagnostic` | **22 PASS**, same 2 |
| `run_s5_failchk_spin.sh`, reuse **ON** | default | **PASS, both `lk_partitions=1` and `=10`** |
| `test/c/chk.locksireads` (SSI, **ASan**) | ASan | **PASS** — trigger, control, and commit-lock-list completeness |
| `test/lockmatrix/run.sh` | default | **rc=0, 0 checks failed** |
| `dist/s_validate` | — | **rc=0** |
| `dist/s_execbits` | — | **rc=0** |
| `/tmp/p13leak.c` (new) | default + diagnostic + Linux | **ok**, both arms; sabotage arm says LEAK |
| `/tmp/p13fc.c` (new) | default + Linux | **PASS**, both arms |

The two `run_all.sh` failures are `run_s5_failchk_spin` and
`run_opt_null_sample`, and they are the **known build-dir-default artifact P12's
report already documented**: both runners default to
`$HERE/../../build_unix` rather than the build dir passed in. **Both PASS when
given the build dir explicitly, and both fail identically on a pristine
baseline tree** — verified in both directions. So 24/24 effective.

The ASan SSI gate is worth noting: P12's report listed ASan as *not verified*
and flagged it as *"worth doing before any merge"* given shared-region list
manipulation. It now passes with P13 applied.

**TCL lock tests (`lock001`–`006`, `dead001`–`007`) and `ssi001`–`009` were NOT
run: no `tclsh` on either box** (checked both the FreeBSD host and the Linux
box). The handoff permits saying so; this is the one gate class that is simply
absent. The C-level SSI gate above partly compensates, and `lockmatrix` covers
the conflict matrix, but the deadlock-detector TCL tests specifically are
unrun — note P13 does not touch `lock_deadlock.c`, unlike P12.

---

## 6. Region signature — P13's key structural advantage

Measured with the same `db_config.h` via `BUILD_DIR=... dist/env_sig_print.sh`:

| arm | `__env_struct_sig()` | `DB_LOCKREGION` change | breaks existing envs? |
|---|---|---|---|
| baseline | `0xb86f77f0` | — | — |
| **P13 only** | **`0xb86f77f0`** | **none** | **NO** |
| P12 only | `0x0b67cec8` | +4064 B, new SHARED struct | yes |
| P13 + P12 | `0x0b67cec8` | as P12 | yes |

**P13's signature is byte-identical to baseline.** It adds no shared struct and
changes no `sizeof`, so unlike P12 it can ship without the region-format break
that P12 cannot avoid (`env_region.c` would otherwise refuse every existing
environment with `BDB1539`). Baseline and P12 values reproduce P12's published
pair exactly, which cross-checks the measurement.

`DB_REGION_MAJOR`/`MINOR` left untouched, as instructed.

---

## 7. The four-arm matrix — NOT MEASURED, and why

**No Linux throughput delta is quoted, because none is defensible.**

All four arms were built and verified distinct (`lock_id.o` sizes 101,128 /
102,000 / 103,952 / 105,040 for base / P13 / P12 / P13+P12), and run 3 reps ×
t=1,2,4,8 × 4 arms, one binary, one directory, arms alternated within each rep.
The result is unusable:

| arm | t=1 cv | t=2 cv | t=4 cv | t=8 cv |
|---|---:|---:|---:|---:|
| base | 7.4% | 19.0% | **61.8%** | 2.5% |
| P13 | 2.2% | 11.6% | **59.6%** | 8.5% |
| P12 | **61.7%** | **71.3%** | **79.4%** | 10.5% |
| P13+P12 | **54.1%** | **62.2%** | **75.1%** | 1.1% |

Against the gate's **10% usability ceiling**. The same arm swings 7,386 →
130,442 commits between reps. The box is an 8-core dev machine also running my
own tooling: too small for t=16/32/64 and too noisy at any t.

A second, independent reason not to trust it: **P12 is not even engaging at this
scale.** Its own metric barely moves — locker-alloc waits 16,913 (shard on) vs
19,443 (shard off), against the 10–50× reduction P12 published on 64 vCPU. At
t=8, every `-c` latch is within noise across all four arms (`c_locker`
0.2110 / 0.2187 / 0.2217 / 0.2143 per commit). There is no contention here for
either change to act on.

**So the maintainer's hypothesis — does P12 stop being a regression once P13 is
in? — is UNANSWERED.** It needs a 64 vCPU box, which is the shape P12 was
originally measured on. The FreeBSD host cannot substitute: F5 caps it at one
thread, and at t=1 there is no contention to measure.

What I can say is narrower and worth stating precisely: **the prerequisite P12's
report identified is now removed.** P12 was rejected because `mtx_locker_stripe[0]`
was acting as admission control in front of `MUTEX_SYSTEM_LOCK`, taken twice per
transaction. P13 removes those two acquisitions entirely rather than spreading
them, so the latch P12 exposed is no longer on the hot path at all. That makes
the hypothesis **more likely**, and it makes the re-test *worth running* — but
it does not substitute for running it.

### The harness trap that would have faked a result

Recording this because anyone re-running this A/B will hit it. **P12's
`locker_shard` is read by `__lock_region_init` when the region is CREATED and
stored in the region**, so that attachers inherit the creator's choice
(deliberate, documented in `dbinc/lock.h`). Setting `DB_NO_LOCKER_SHARD` on a
run that **attaches** to an existing environment is **silently ignored**.

My first pass reused one dataset across arms and measured P12 at 5,354 vs
77,426 commits — an apparent **13× regression that was pure harness error**.
**Each arm must recreate the environment.** (P13's switch is process-local and
has no such requirement, but recreating uniformly avoids treating the two
differently.)

I also caught a build-ordering error in my own harness before it reached any
number: a first loop applied each patch and a second loop ran `make`, so all
four arms compiled the *same* clean baseline. Detected because all four
`lock_id.o` files were byte-identical in size. Per-arm source markers are now
verified *before* each compile.

### Scaling-shape gate at `-S 96` and `-S 32`

**Not run.** It requires the 32+ core box `scale_shape_gate.sh` itself refuses
to run without, for the same reason the matrix does.

---

## 8. F6 — a pre-existing double-free found while gating P13

Not mine, but found by this work and worth fixing.

```
BDB0059 assert failure: src/mutex/mut_alloc.c/245: "F_ISSET(mutexp, DB_MUTEX_ALLOCATED)"
  __os_abort <- __mutex_free_int <- __txn_env_refresh <- __env_refresh <- __env_close
```

`__txn_env_refresh`'s snapshot sweep (`src/txn/txn_region.c:517`, same shape at
`:597`) calls `__mutex_free(env, &td->mvcc_mtx)` on a mutex that
`__mutex_failchk` has **already** returned to the free list, and
`__mutex_free_int` asserts `DB_MUTEX_ALLOCATED`.

Reproducer: child opens the env with `DB_FAILCHK` + `set_isalive`, creates a
locker, dies; parent runs `failchk` (which prints `BDB2017 Freeing mutex for
process:` per reclaimed slot) then closes the env.

**Proved pre-existing, three ways:** it reproduces with P13 off
(`DB_NO_LOCKER_MUTEX_REUSE=1`), *and* on a **pristine baseline tree with P13 not
applied at all**, on the same diagnostic build.

Worth noting the production-build reading is the more serious one: with
`DB_ASSERT` compiled out, the double-free silently corrupts the mutex free list
instead of aborting. The deliberate comment at `:507-516` shows the free was
added to stop a mutex-region leak, so the fix is a guard, not removing the call.
**Not fixed here** — outside P13's scope.

---

## 9. What I did NOT verify

- **The four-arm matrix, the per-latch tables at t=8/16/32/64, and the
  scaling-shape gate at `-S 96`/`-S 32`.** §7. No 64 vCPU Linux box; the
  FreeBSD host is capped at one thread by F5. **This is the main gap.**
- **The maintainer's hypothesis.** Unanswered, not confirmed and not refuted.
- **Any Linux throughput claim for P13.** Expect single digits or nothing —
  P13 removes ~2 mutex ops per transaction with no libthr GC behind them — but
  I did not measure it to a reportable cv, so I assert nothing. **Do not read
  the FreeBSD 1500× as a general number; it is an amplification of a
  platform-specific O(N) GC.**
- **TCL `lock001`–`006`, `dead001`–`007`, `ssi001`–`009`.** No `tclsh` on either
  box. P13 does not touch `lock_deadlock.c`.
- **Multi-process under concurrent load.** §4.3 is a code argument plus a
  single-child failchk test, not a loaded two-process run.
- **F5's fix.** Characterised only; the `DB_MUTEX_SELF_BLOCK` question is open.
- **F6's fix.** Filed only.
- **Mutex-pool exhaustion at the bound.** §4.2 shows the retained set tracks
  concurrently-live lockers; I did not drive concurrency to `mutex_max` to
  confirm the ENOMEM path degrades gracefully.
- **Id wraparound** (needs 2^31 ids) and **non-x86-64**, both inherited gaps
  from P12's list.
- **Soak.** Longest run is 30 s.

---

## 10. Host teardown

| Resource | Id | State |
|---|---|---|
| Instance | `i-0a11c8e35b97dfc95` | **terminated** (confirmed by `describe-instances`) |
| Key pair | `libdb-p13-20261009-0709` | **deleted** (`describe-key-pairs` -> `InvalidKeyPair.NotFound`) |
| Security group | `sg-06191a975ac7bc6c7` | **deleted** (`describe-security-groups` -> `InvalidGroup.NotFound`) |
| Orphaned volumes | - | **none** (`describe-volumes --filters status=available` returned empty) |

The local private key was removed too. The key and SG leaked by the previous
run are the ones deleted above, so nothing is left behind from either session.

Artifacts copied back before teardown, in `/tmp/p13_artifacts/`:

| file | what |
|---|---|
| `gcprobe.c` | the standalone libthr `pshared_gc` linearity probe (no libdb) -- the F4 proof |
| `pshinit.c` | PROCESS_SHARED init+destroy cost, FreeBSD vs Linux |
| `p13leak.c` | the in-env-lifetime mutex-leak gate (section 4.2) |
| `p13fc.c` | the failchk-reuse gate (section 4.1) that S5 does not provide |
| `p13_ab.sh` | the four-arm harness, with the `locker_shard`-at-region-create trap documented in its header |
| `p13_ab.tsv` | the 48 raw runs behind section 7's cv table (retained precisely because they are NOT reportable) |
| `P13.patch`, `P12P13.patch` | the P13-only and P13+P12 arms as patches against `fa109c45a` |

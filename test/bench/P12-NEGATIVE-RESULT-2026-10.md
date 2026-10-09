<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# P12 implementation report — per-stripe locker allocation

> **Measured negative result — the code is NOT on master.** P12 was
> implemented as RFC 0012 specifies, removed the latch it targeted (locker-alloc
> waits down 10–50x), and cost **~18–21% throughput at t>=16**. The
> implementation is preserved as a unified diff at
> `test/bench/P12-IMPLEMENTATION.diff` so it is recoverable without a branch;
> the reasoning that matters is in this report. Do not re-attempt the per-stripe
> sharding without first addressing `MUTEX_SYSTEM_LOCK` — see the
> admission-control finding below, which is the reusable part.


**Verdict: MEASURED NEGATIVE RESULT. Do not merge as-is.**

The change does exactly what RFC 0012 specified, and the specified mechanism is
confirmed removed: locker-alloc latch waits fall **10–50×**. Throughput
nevertheless falls **~18–21% at t≥16**, reproducibly, and the scaling shape gets
*worse*, not better. The fix wins on its own metric and loses on the one that
matters.

Working tree left with the change applied, uncommitted, unpushed, as instructed.

---

## 1. What changed

`region->free_lockers`, `region->lockers` (ulinks) and `region->nlockers` were
one each, region-wide, mutated on **every** locker create and free under
`mtx_locker_stripe[0]`. All three are now per-stripe, in a new
`DB_LOCKERSTRIPE` indexed by `LOCK_LOCKER_STRIPE(bucket index)` — the same
stripe that already owns the bucket chain.

| File | Change |
|---|---|
| `src/dbinc/lock.h` | `DB_LOCKERSTRIPE` (free list, ulinks, nlockers, maxnlockers); `locker_stripe[64]`; `LOCK_LOCKER_WRITE`/`UNLOCK_LOCKER_WRITE`; `LOCK_LOCKER_ID_ALLOC` (stripe 0, id counters only); rewrote the lock-order comment |
| `src/lock/lock_id.c` | create/free take ONE stripe; `__lock_nlockers`/`__lock_maxnlockers`; all three rare walkers iterate every stripe |
| `src/lock/lock_region.c` | per-stripe init, round-robin seeding, refresh |
| `src/lock/lock_stat.c` | `st_nlockers`/`st_maxnlockers` summed on demand |
| `src/lock/lock_deadlock.c` | per-stripe ulinks walk |
| `src/env/env_sig.c` | `__ADD(__db_lockerstripe)`, count 41→42 |

**`__lock_freelocker` returns a locker to the stripe it came from.** Both
directions derive the stripe from `LOCKER_HASH(... sh_locker->id ...)` →
`LOCK_LOCKER_STRIPE(indx)`, identical to the create path, so the lists cannot
leak across stripes. This was checked explicitly, as instructed.

### Lock order, as the comment now states it

Stripe 0 is **no longer on the create/free path at all** — the three
region-wide fields it existed to guard are gone from it. The documented order
was updated accordingly rather than left describing an order the code no longer
takes:

- create/free take exactly one stripe; nothing nests inside it;
- multi-stripe paths take stripes **ascending from 0** (`LOCK_LOCKERS`,
  `LOCK_LOCKERS_REST`), which keeps the order total and deadlock-free;
- stripe 0 retains exactly one role: the `lock_id`/`cur_maxid` counters, whose
  stripe is unknowable until the id is chosen. Paid once per `__lock_id`, not
  on `txn_begin`.

The old "a reader on stripe 0 blocks behind any writer anywhere" penalty is
also gone, and that is noted where it was documented.

### A race I introduced and fixed

My first cut tested the per-stripe free list **unlatched**, then took the
stripe. Unsound, not merely racy: `__lock_getlocker_int`'s refill branch opens
with `UNLOCK_LOCKERS` (all 64), so a caller arriving there holding one stripe
would release 64 latches it never acquired. Fixed by taking the stripe first and
testing under it. **All throughput numbers below are from the post-fix binary**;
the pre-fix numbers were discarded.

---

## 2. Latch table — before/after, per commit

One binary, runtime switch `DB_NO_LOCKER_SHARD`, one dataset directory, arms
alternated within each rep, 3 reps, `db_stat -c -Z` immediately before each run.
`arm=off` reproduces master's behaviour. 64 vCPU Xeon 8375C, scale 96, 20 s
after 8 s warmup, discarded prewarm pass first. cv ≤ 6.8%.

| t | arm | tpm | locker | partition | region | obj-queue | conflict |
|---:|:---|---:|---:|---:|---:|---:|---:|
| 8 | off | 1,960,875 | 0.1120 | 0.1483 | 1.1646 | 0.0004 | 0.1656 |
| 8 | **on** | 1,876,749 | **0.0024** | 0.1373 | 1.1013 | 0.0004 | 0.1480 |
| 16 | off | 1,310,866 | 0.2352 | 0.2051 | 1.5709 | 0.0022 | 0.3308 |
| 16 | **on** | 1,058,666 | **0.0091** | 0.1601 | 1.2853 | 0.0019 | 0.2987 |
| 32 | off | 894,642 | 0.3425 | 0.2339 | 1.4113 | 0.0049 | 0.5581 |
| 32 | **on** | 727,639 | **0.0235** | 0.1845 | 1.0919 | 0.0044 | 0.5191 |
| 64 | off | 815,742 | 0.5940 | 0.2953 | 1.3206 | 0.0083 | 0.7580 |
| 64 | **on** | 672,139 | **0.0581** | 0.2446 | 1.0036 | 0.0066 | 0.7205 |

On/off ratio — **every latch improves, throughput falls**:

| t | tpm delta | locker | partition | region | obj-queue | conflict |
|---:|---:|---:|---:|---:|---:|---:|
| 8 | −4.3% | 0.02× | 0.93× | 0.95× | 0.91× | 0.89× |
| 16 | **−19.2%** | 0.04× | 0.78× | 0.82× | 0.86× | 0.90× |
| 32 | **−18.7%** | 0.07× | 0.79× | 0.77× | 0.90× | 0.93× |
| 64 | **−17.6%** | 0.10× | 0.83× | 0.76× | 0.79× | 0.95× |

The P12 metric itself is met — locker-alloc per commit at t=64 goes
0.5940 → 0.0581, and the 5.3× t=8→t=64 growth becomes numerically steeper
(24.3×) only because the baseline is 47× smaller in absolute terms.

**No latch that `db_stat -c` reports absorbed the loss.** Contention did not
relocate within the lock subsystem; it left it.

### Where it did go: the mutex-region latch (`db_stat -x`)

`db_stat -c` is the LOCK subsystem and does not report this latch. Every locker
create calls `__mutex_alloc` and every free `__mutex_free`, both of which take
`MUTEX_SYSTEM_LOCK` (`mutex_int.h:871`) — a **single global mutex**, twice per
transaction. Per commit, t=32, 3 alternating pairs:

| rep | arm=on | arm=off |
|---:|---:|---:|
| 1 | 0.01881 | 0.00180 |
| 2 | 0.01916 | 0.00176 |
| 3 | 0.02025 | 0.00176 |

**~11× worse with the fix on, reproducibly.** The acquisition *count* is
identical in both arms (one alloc + one free per locker), so this is pure
contention, not extra work. Mechanism: stripe 0 was incidentally acting as
**admission control** in front of that global latch — serialising creates meant
threads arrived at `MUTEX_SYSTEM_LOCK` one at a time. Removing stripe 0 lets 64
threads arrive simultaneously at a latch that was always global, converting
cheap waits on a lock-subsystem latch into expensive waits on the mutex region.

This is a *consistent* explanation, not a proven one — see §6.

---

## 3. Hypotheses tested and disproven

| Hypothesis | Test | Result |
|---|---|---|
| **Escalation rate** — 1/64-size lists empty often, escalating to the region latch | instrumented counters | **23 escalations / 371,300 creates = 0.01%.** Dead. |
| **False sharing** — 40 B stripes, 1.6 per cache line | padded to exactly 64 B, re-measured | t=32: −18.7% → −18.6%. **+0.3%, i.e. nothing.** Dead. |
| **Contention relocated to another lock latch** | all five latches, per commit | every one *improved*. Dead. |

Padding was kept anyway (it is correct, costs only region memory, and removes a
confound), with a compile-time assertion. Note `pad[23]` is **not** a valid
sabotage of that assertion — 8-byte struct alignment rounds it back to 64; only
a size-changing perturbation (`pad[32]`) fires it, verified.

---

## 4. Region layout — the three-gate question

**`__env_struct_sig()` before → after: `0xb86f77f0` → `0x0b67cec8`.**

Both measured with the same `db_config.h` via `BUILD_DIR=... dist/env_sig_print.sh`.
(An earlier `0xeae0caa0` I reported was taken against a *different* build config
and is superseded; it was not a valid before/after pair.)

This is a **region-format break**: `env_region.c` will refuse every existing
environment with `BDB1539` → `DB_VERSION_MISMATCH`, while abidiff stays green
because no public ABI changed. **Flagged, not bumped** — `DB_REGION_MAJOR`/
`MINOR` left untouched, as instructed.

### The break is unavoidable, not a consequence of my layout choice

`sizeof(DB_LOCKREGION)`:

| form | sizeof | delta |
|---|---:|---:|
| v2026.10.3 master | 1000 | — |
| embedded, 40 B stripe | 3528 | +2528 |
| embedded, 64 B padded stripe (current) | 5064 | +4064 |

A size-preserving form **is** achievable: removing the three fields vacates 36
bytes (`free_lockers`@576 + `lockers`@592 = 32 contiguous, `nlockers`@680 = 4),
and a single `roff_t stripe_off` needs 8, so a region-allocated array behind an
offset — exactly the `locker_off`/`obj_off`/`part_off` precedent — would hold
`sizeof` at **1000, unchanged**.

But that does **not** save the signature. `env_sig.c` hashes one `sizeof` per
listed struct, and any *new* shared struct must be listed — `__db_locker` and
`__db_lockobj` are both region-allocated and both hashed, because processes
sharing a region must agree on their layout regardless of who allocates them.
Measured directly: adding one extra `__ADD` line while leaving
`DB_LOCKREGION` at master's 1000 bytes still moves the signature
(`0xb86f77f0` → `0x0784fad0`).

So: **any form of this change breaks region compatibility.** The `roff_t` form
remains worth doing for consistency with the neighbouring arrays and to make the
stripe count a runtime value — not for compatibility. I did not implement it,
since the throughput result makes the layout question moot for now.

`test/db/run_handle_sizes.sh` (public handle sizes) **passes** — no public ABI
change.

---

## 5. Gates

### Scale-shape gate, `-r 9`, both scales, both arms

cv at t=8 is under the 10% usability ceiling in all four arms, so every verdict
is reportable.

| S | arm | t=8 | t=32 | t=96 | cv8% | tol% | Δt32 | Δt96 | verdict |
|---:|:---|---:|---:|---:|---:|---:|---:|---:|:---|
| 96 | off | 1,973,942 | 933,098 | 743,828 | 6.49 | 19.5 | −52.7% | −62.3% | FAIL |
| 96 | **on** | 1,889,362 | 732,560 | 598,140 | 4.71 | 14.1 | −61.2% | **−68.3%** | FAIL |
| 32 | off | 1,767,796 | 1,563,587 | 1,465,317 | 5.65 | 16.9 | −11.6% ok | −17.1% | FAIL |
| 32 | **on** | 1,666,860 | 1,425,047 | 1,364,425 | 3.92 | 11.8 | −14.5% | **−18.1%** | FAIL |

On vs off at equal thread count:

- **S=96:** t=8 −4.3%, t=32 −21.5%, t=96 −19.6%
- **S=32:** t=8 −5.7%, t=32 −8.9%, t=96 −6.9%

Both arms fail at both scales, so the *failure* is the pre-existing G12 defect,
not mine. But the fix is **strictly worse on every axis**: worse shape
(−68.3% vs −62.3% at S=96) and lower absolute throughput at every thread count
at both scales. The brief noted S=32 passes at −3.5% on master; this box
measures master at −17.1%, i.e. a different (noisier/slower) machine than the
one that figure came from — which is another reason I report only within-box
on/off comparisons.

### Correctness — all pass

| Gate | Result |
|---|---|
| `test/db/run_all.sh`, default build | **19/19 pass** |
| `test/db/run_all.sh`, `--enable-diagnostic` | **19/19 pass** |
| `dist/s_validate` | **rc=0, 22 checkers** |
| `dist/s_execbits` | **rc=0** |
| `test/tcl` `lock001`–`lock006` | **6/6 pass**, both arms |
| `test/tcl` `dead001`–`dead007` | **7/7 pass**, both arms |
| `test/tcl/run_targeted.sh` | **5/5 pass** (`lock001 txn001 test001 ssi001 ssi002`) |
| `test/lockmatrix/run.sh` | **rc=0, 0 checks failed** |
| `test/db/run_handle_sizes.sh` | **pass** |

Deadlock detection specifically works — all 7 `dead*` tests pass, exercising the
per-stripe ulinks walk in `lock_deadlock.c` that this change rewrote.

Two notes on how that 19/19 was reached, so it is not mistaken for a clean
single command:

- `run_all.sh` reports `run_s5_failchk_spin` and `run_opt_null_sample` as
  FAILED because those two runners default to `$HERE/../../build_unix` rather
  than `$BUILD`, and that stale tree fails to link against this box's liburing.
  **Reproduced identically on pristine master**, so pre-existing and unrelated.
  Both **pass** when given the build dir explicitly, on both builds.
- The first run also hit `BDB1539` in several runners from stale `*TESTDIR*`
  environments created by a pre-patch build — the signature break doing exactly
  what §4 describes. Cleared and re-run.

`test/tcl` additionally needs `--enable-test` (not just `--enable-tcl`), since
`berkdb getconfig` is `#ifdef CONFIG_TEST`; without it the suite dies before
running anything, which also reproduces on master.

---

## 6. What I did NOT verify

- **That the mutex-region latch is the *cause*.** I showed it is ~11× worse with
  the fix on, that acquisition counts are equal, and that the three competing
  hypotheses are dead. I did **not** prove causation — the decisive experiment
  is to remove the per-locker `__mutex_alloc`/`__mutex_free` from the create
  path (pre-allocate or pool `mtx_locker`) and re-measure; if throughput then
  exceeds the off arm, the admission-control story is confirmed. Not attempted.
- **The `roff_t` region-allocated form.** Sizing is arithmetic from measured
  offsets (§4); I did not build or measure it. It cannot change the signature
  outcome, but I have not demonstrated that empirically beyond the one-extra-
  `__ADD` probe.
- **Multi-process region sharing.** Every test here is single-process. The
  `locker_shard` switch is stored in the region specifically so attachers
  inherit the creator's choice, but I never ran two processes against one region
  to confirm it.
- **Non-x86-64.** `DB_CACHE_LINE_BYTES` is 64, correct for x86-64 and arm64,
  unverified elsewhere. It is a performance hint; a wrong value mis-sizes
  padding and nothing more.
- **Id wraparound.** `lock_id.c`'s 2^31 wraparound rebuild now walks all 64
  stripes. Code-reviewed, never executed — it needs 2^31 locker ids.
- **Failchk's per-stripe walk under real process death.** `run_s5_failchk_spin`
  passes, but it exercises the bucket-table walk; I did not construct a case
  that frees a locker from a dead process via the per-stripe ulinks path.
- **`--enable-diagnostic` lock-order checker under heavy contention.** The
  diagnostic build passes 19/19 and I ran `lock_bench` on it in both arms, but
  not the full scale-96 TPROC-C load, which is where an order violation would be
  most likely to surface.
- **ASan/UBSan.** `lockmatrix` was run with `LIBDB_ASAN=0`. Given this touches
  shared-region list manipulation, an ASan run is worth doing before any merge.
- **Longer-run stability.** Longest measured run is 30 s (gate) / 20 s (A/B).
  No soak test, so a slow leak in the per-stripe counters would not have shown.

---

## 7. Recommendation

The RFC's diagnosis was right about the latch and right that per-stripe lists
remove it. The amendment's reasoning holds: the free-list pop is the hot path and
it is now stripe-local. But the latch was not the throughput bottleneck — it was
sitting in front of one, and removing it exposed a global mutex-region latch that
is worse. **Measured negative result; this should join the others on master
rather than be merged.**

The next experiment is cheap and well-defined: eliminate the per-locker
`mtx_locker` alloc/free from the create path, then re-run this exact A/B. If
that lands, P12's sharding likely becomes a win and should be re-tested on top
of it — the two changes are complementary, and this one is a prerequisite that
cannot pay off alone.

### Host

Instance `i-0ee9838a86893d96d` (us-east-2, profile `hotdog`) — **terminated**,
confirmed by `describe-instances` returning state `terminated`. No orphaned
volumes (`describe-volumes --filters status=available` returned empty). Other
running instances in that account are not mine and were left alone. Raw data
copied back before teardown: `/tmp/p12_ab2_final.tsv` (the 24-run A/B) and
`/tmp/shape{96,32}_{on,off}.out` (the four gate arms).

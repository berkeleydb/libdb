# RFC 0015: Multi-core scalability audit: every serialisation point, and the order to remove them

- **Status:** Draft
- **Type:** Prospective (an audit with proposals; no code changes)
- **Author:** libdb maintainers (static review)
- **Date:** 2026-10
- **Tracking:** G12, G13 residual (scale 96, t=8→t=32 = −37% with P12+P13), P5 / RFC 0008 Risk 1, RFC 0012 follow-up
- **Prototype:** none. Static review of `master` @ `2d8397dfe` (P12 merged) plus the existing measurements under `test/bench/`.
- **Review note (2026-10-10):** the five load-bearing citations were checked against the source before the RFC was placed: the `mtx_regenv` aliasing (`lock_region.c:189`, `txn_region.c:118`, `log.c:224`, `env_open.c:1122`); `__txn_begin_int` and `__txn_end` each having 3 `TXN_SYSTEM_LOCK` sites; the global `++c_mp->put_counter` per page put (`mp_fput.c:147`); `c_locker` sampling stripe 0 only (`lock_stat.c:168`); and `mp_fget.c:492` folding `hash_examined` into `st_hash_searches`. All five hold.

---

## Summary

This audit reads every subsystem for state that every thread touches on every operation. It cites each site, ranks the sites against the per-latch data the project already has, and proposes fixes in three sizes.

**The central finding is [FOUND] (read in the code), and it changes how the existing [MEASURED] data reads.** The latch that `db_stat -c` reports as *"region locks that required waiting"* is the single largest per-commit wait in the P13/P12 matrix (1.43–1.64 per commit at t≥16 in the base, P13 and P13+P12 arms, and 1.21–1.34 in P12-alone). It is **not a lock-manager latch.** `LOCK_REGION_LOCK`, `TXN_SYSTEM_LOCK` and `LOG_SYSTEM_LOCK` are one physical mutex, `renv->mtx_regenv` (`lock_region.c:189`, `txn_region.c:118`, `log.c:224`). That mutex is also the `__env_alloc` latch for the shared primary region (`env_open.c:1122`). `lock_order.h:40-52` documents the aliasing, but no benchmark report draws the consequence. So `db_stat -c`, `db_stat -t` and `db_stat -l` all report the same counter under three names. On TPROC-C at t=16–64, the waits per commit on that one latch are **2.3–5.5× the locker-allocation waits that P12 fixed** before P12, and **19–83× after it** (matrix medians, `P13-P12-MATRIX-2026-10.md` §5). It is taken by `txn_begin`, by `txn_end`, by every log record, and by every first snapshot read. G13-DIAGNOSIS already measured it at **12.78 acquisitions per TPC transaction** (its "env region" row), without naming it as the txn/log latch, and it read a *different* mutex, the dbreg list latch, as "log region". A no-log control that already exists in the tree (`p5_notdur.tsv`) still shows **2.0 acquisitions per row and a wait fraction rising 0 → 0.19** from t=1 to t=64. That is the "second, non-log scaling defect" that RFC 0008's Risk 1 left unnamed: `TXN_SYSTEM_LOCK` *is* the log latch.

**This reconnects P5 to G13.** G13-DIAGNOSIS concluded *"P5/RFC 0008's log latch cannot be this defect"*. That conclusion was drawn in the pre-B2 regime (upgrade-deadlock bound, 1.16 CPUs busy at t=32) and partly from the wrong mutex. After B2, the largest latch wait in the four-arm matrix **is** the P5 latch, with the txn manager sharing it. RFC 0008's D0 and D2 are therefore candidates for the G13 residual again.

The recommended order (detailed in §4):

1. Small, certain fixes: per-acquire mutex statistics and owner stores, padding, the `put_counter` global write, and the `mvcc_mtx` alloc/free per MVCC writer (the same shape as F4).
2. Split `mtx_regenv` into its three roles: a txn-region latch, a log-region latch and an env allocator latch. Take the allocator out of the txn critical sections with a TXN_DETAIL free list. Measure this alone and together with RFC 0008 D0 (fewer log records per transaction), because ~80% of the latch's arrivals are log appends. This is the measured top cliff.
3. The `PGNO_BASE_MD` allocation convoy that is already measured.
4. Architecture: an Aether-style consolidation log buffer (RFC 0008 D2), a ProcArray-style active-transaction array, and lock-free buffer pins.

Every proposal that removes serialisation names what is downstream of it, because of P12.

---

## Prior work read and *not* re-reported as findings

| Item | What it established, which this RFC builds on |
|---|---|
| P1, P4 | Locker stripes keyed by bucket; `mtx_dblist` taken shared for the cursor walk. |
| P5 / RFC 0008 | The log latch is a *handoff* cost (mutex 90.45% vs memmove 1.46%). Reserve-then-copy (D1) is **disqualified** in a multi-process setting (a hole looks like virtual EOF at `log_get.c:1238`). D0 (fewer records per transaction) is first, and D2 (consolidation array) is next. |
| P9 | Retracted. `tas_spins` work is **not** blocked. The 50×ncpu spin count and the owner-id stores remain a lead. |
| P12 / RFC 0012 | Per-stripe locker free lists. Alone they were −18%, because stripe 0 was **admission control** in front of `MUTEX_SYSTEM_LOCK`. On top of P13 they gave +22%. Merged as a region-format break (`0xb86f77f0` → `0x0b67cec8`). |
| P13, F4, F5 | Retain the locker mutex instead of alloc/free (FreeBSD ~1500×). F5 re-arms the lock mutex without destroy/init. |
| B2, G13 | DB_RMW removed the driver's upgrade-deadlock artefact. The residual at scale 96 is not row conflicts and not I/O. |
| B3 | FreeBSD pshared pthread objects die when the last mapper exits. The workaround is TAS or `umtx_vnode_persistent=1`. |
| PIN-REMEASURE | `perf/mpool-pin` (BH hot-field padding) is **NULL**: the cost is *true* sharing of `bhp->ref`, not false sharing. `perf/lock-readpath` (O(1) `nheld[]`) is **NULL**. `perf/rsnap-ml` is a **0.063×** regression: there is no hot working set below the root. `perf/bhpin-r1` is 1.36–1.55× where it fires, 0.885× in shared envs, and unsafe (`mp_fput.c:197`). |
| RFC 0007 | Optimistic descent: 1.71× at t=32 per-key and 2.05× at t=96 batched. It is opt-in (`DB_OPTREAD`, after M2). At t=96 per-key the cursor-lifecycle mutex becomes the wall. |
| CURSOR-SHARD | 8-way `cq_parts[]`: 39× at t=96 once the thread-id hash was fixed. |
| BTREE-LOCK-SCOPE | 100% of bulk-insert waiters are on `PGNO_BASE_MD`, because `__db_new` holds the meta write lock to commit. |

---

## Motivation

- **G13 residual.** At scale 96, t=8 → t=32 retains −37.0% even with P12+P13 (`P13-P12-MATRIX-2026-10.md` §10). Scale 32 retains −12.2%. More warehouses scale *worse*, so the limit is not row conflict, and page-I/O waits are 0.
- **The WiredTiger gap widens with threads**: 6–8× at t=8 grows to 20–58× at t=128 (`CROSS-ENGINE-2026-09.md:149`).
- **RFC 0008 Risk 1.** With `DB_TXN_NOT_DURABLE`, throughput still falls 235k (t=8) → 131k (t=96). The RFC says *"a second, non-log scaling defect behind P5 which this RFC does not identify."* This audit identifies it (§1.1).

---

## 1. Executive summary: the top five cliffs, ranked by expected impact

| # | Cliff | One-line evidence | Class |
|---|---|---|---|
| **1** | **`mtx_regenv` is three latches in one: the txn region, the log region and the env allocator.** Every `txn_begin`, `txn_end`, `__log_put`, first snapshot fget, `__txn_oldest_reader`, and the leader's buffer `pwrite` in group commit serialise on it. | [MEASURED] `c_region` = **1.43–1.64 waits/commit at t≥16** in base, P13 and P13+P12 (1.21–1.34 in P12-alone), the largest latch in the matrix, and unmoved by P13+P12. [MEASURED] G13-DIAGNOSIS §2 "env region" (n=1, i.e. this mutex): **12.78 acquisitions per TPC transaction** at t=8, which matches the static count of 2 (begin/end) + 9.33 log records + current-LSN calls. [MEASURED] `p5_notdur.tsv`: **2.00 acq/row, wait fraction 0.000@t1 → 0.179@t32 → 0.191@t64**, with **zero** log records. [FOUND] `lock_region.c:189`, `txn_region.c:118`, `log.c:224`. | medium → large |
| **2** | **The meta page is held to commit by `__db_new`** (`db_meta.c:134`, released by a no-op `__TLPUT` at `:263`). Every page allocation in a transaction queues on one lock object. | [MEASURED] BTREE-LOCK-SCOPE: **400/400 waiters on pgno 0** at t=96. Little's law closes at 333 ms vs measured p99 282 ms. | medium |
| **3** | **Every latch acquire writes its own mutex line 2–3 times beyond the lock word**: the `mutex_set_[rd_]nowait` RMW (`mut_tas.c:104-110`, `:347-351`) and the `pid`/`tid` store (`mut_tas.c:204`, **and `:382` on every *shared* acquire**). With hybrid on, a shared latch's readers also write the line. | [MEASURED-adjacent] PIN-REMEASURE: `__os_atomic_read` 34% and `__memp_fget` 28% self-time batched. RFC 0008: mean hold time inflates **21 ns → ~300 ns (14×)** for constant work. The specific stores are [UNSURE] until measured. | small |
| **4** | **The deadlock detector runs inline on every blocking request and takes all 64 locker stripes** (`lock.c:1662` → `lock_deadlock.c:464`), walking every in-use locker under them. | [MEASURED pre-B2] `__dd_build` was 18.84% inclusive at t=32 (G13-DIAGNOSIS §1). Post-B2 `c_confl_w` is still **0.80–0.85/commit at t=64**, so it still blocks almost once per transaction. [UNMEASURED] post-B2 detector share. | medium |
| **5** | **The buffer pin is still a shared RMW on a hot line**: `atomic_inc(&bhp->ref)` (`mp_fget.c:387`) + `MUTEX_READLOCK(mtx_buf)` sharecount CAS (`mut_tas.c:369`), plus per-fget global counters `c_mp->stat.st_hash_searches` (`mp_fget.c:486`), `mfp->stat.st_cache_hit` (`:476`), and **`++c_mp->put_counter` on every fput** (`mp_fput.c:147`). | [MEASURED] PIN-REMEASURE: pin = **71.9%** of self time, batched at t=96. RFC 0007: removing it gives 1.71×/2.05×. The `put_counter` contribution is [UNSURE]. | small (counters) / large (pin) |

What is **not** in the top five, and why:
- **Locker allocation** is fixed by P12: 0.076/commit, 19× below `c_region`.
- **`MUTEX_SYSTEM_LOCK`** is ~0 after P13 (≤0.00004/commit). Two other per-transaction alloc/free sites remain (§2.1), but only for MVCC writers.
- **Lock partitions**: 0.37/commit at t=64, max any one 0.047. Already well spread (640 partitions on 64 vCPU).
- **mpool hash buckets**: 0 waits in G13-DIAGNOSIS §2.

---

## 2. Per-subsystem inventory

Notation: **freq** = how often per transaction (txn) or per page operation (op). Tags: **[FOUND]** = read in code; **[MEASURED]** = present in existing data, file cited; **[UNSURE]** = static reading only; or the cost cannot be ranked without data.

### 2.0 Which `db_stat` counter is which mutex (read this before any table)

| `db_stat` line | Reads mutex | Source | Also known as |
|---|---|---|---|
| `-c` "region locks that required waiting" (`c_region`) | `DB_LOCKREGION.mtx_region` | `lock_stat.c:164` | **= `renv->mtx_regenv`** (`lock_region.c:189`) |
| `-t` "region locks that required waiting" | `DB_TXNREGION.mtx_region` | `txn_stat.c:125` | **= `renv->mtx_regenv`** (`txn_region.c:118`) |
| `-l` "region locks that required waiting" | `LOG.mtx_region` | `log_stat.c:86` | **= `renv->mtx_regenv`** (`log.c:224`) |
| `-e` "Environment region mutex" | `renv->mtx_regenv` | `env_stat.c:183` | the same object |
| `-c` "locker allocations that required waiting" (`c_locker`) | `mtx_locker_stripe[0]` only | `lock_stat.c:168` | post-P12: one stripe of 64, a **1/64 sample** plus the `lock_id` counter |
| `-c` partition (`c_part`) | Σ `part_array[i].mtx_part` | `lock_stat.c:140` | |
| `-x` "region locks that required waiting" (`x_region`) | `MUTEX_SYSTEM_LOCK` | `mut_stat.c:84` | |

**[FOUND] Consequence 1.** G13's superseded "db_stat -l" table (`G13-RESOLVED-2026-10.tsv`) was declared wrong because `-l` means log, not lock. In fact `-l` and `-c` report the **same latch**. The superseded number (1.125 → 1.603/commit) and the corrected `c_region` number (1.18 → 1.50/commit, RFC 0012) agree because they are one counter. The correction was right about the flag and wrong about the conclusion. The "lock region" *was* the residual. It just is not a lock-manager latch.

**[MEASURED + FOUND] Consequence 1b — G13-DIAGNOSIS §2 contains the same aliasing error in the other direction.** Its per-`alloc_id` table has three rows that need relabelling:
- "lock region", n=705 = 640 partitions + `mtx_dd` + 64 locker stripes (`lock_region.c:270,274,290`; 640 = `10 × 64 vCPU`, `lock_method.c:42`). It does **not** include `mtx_regenv`.
- "env region", n=1 = **`mtx_regenv` = the lock, txn and log region latch**: 12.78 acq/txn and 0.53 waits/txn at t=8.
- "log region", n=1 = **`dblp->mtx_dbreg`** (`MTX_LOG_REGION`, `log.c:65`), the dbreg FNAME-list latch. **It is not the log latch.**

So that report's line *"The log region is not a participant"* reads the dbreg latch. Its parallel `db_stat -l` figure (316,278 → 5,298 waits) is the right mutex, and it did fall — but in the pre-B2 regime, where the box was idle on upgrade deadlocks (1.16 CPUs used at t=32). After B2, the same mutex carries 1.4–1.6 waits per commit (the matrix). **The G13-DIAGNOSIS verdict that D0 "targets a latch nothing is waiting for" should be re-read as "…in the pre-B2 deadlock-bound regime"; it is not true of the post-B2 engine.**

**[FOUND] Consequence 2.** After P12, `c_locker` samples **only stripe 0** (`lock_stat.c:168`). The 0.076/commit figure for P13+P12 therefore understates locker-stripe contention by up to 64×, unless stripe 0 is hotter because of `__lock_id` (§2.2). The headline P12 number is correct about stripe 0 and silent about stripes 1–63. **[UNSURE]** whether this matters; the proposal is S7.

### 2.1 Mutex region (`src/mutex/`)

| # | Serialisation point | Taken by / freq | Why global | Tag |
|---|---|---|---|---|
| M-1 | `MUTEX_SYSTEM_LOCK` (`mutex_int.h:871`) in `__mutex_alloc_int` (`mut_alloc.c:92`) and `__mutex_free_int` (`mut_alloc.c:284`) | after P13, per-txn callers are only (a) **MVCC writer txns**: `__mutex_alloc(MTX_TXN_MVCC)` on the first dirty fget (`mp_fget.c:268`) and `__mutex_free(&td->mvcc_mtx)` in `__txn_end` (`txn.c:1966`); (b) frozen BH (`mp_mvcc.c:282`); (c) fresh frame on a cold miss (`mp_alloc.c:123`, **nested inside `MPOOL_REGION_LOCK`**) and frame free (`mp_bh.c:931`); (d) a log commit struct, first use only (`log_put.c:1100`, recycled after). Per handle open/close: `db.c:497`, `:512` (×9), `mp_fopen.c:866-870`, `sequence.c:203` | one free list in the region | [FOUND] |
| M-2 | `__mutex_alloc_int` **holds `MUTEX_SYSTEM_LOCK` across `__mutex_init`** (`mut_alloc.c:182-185`), contrary to its own comment at `:87-89` ("Drop the lock before initializing the mutex"). On pthreads/hybrid that is `pthread_mutex_init` + `pthread_cond_init` with pshared attrs (`mut_pthread.c:129-170`) under the global latch. `__mutex_free_int` does the destroy *outside* (`:281` before `:284`), so the two sides are asymmetric. | every `__mutex_alloc` | | [FOUND] |
| M-3 | **The `td->mvcc_mtx` alloc/free pair per MVCC writer transaction**: `mp_fget.c:266-269` → `txn.c:1964-1967` | **2× `MUTEX_SYSTEM_LOCK` + pthread init/destroy per writer txn on any `DB_MULTIVERSION` database** | the same shape as F4/P13, and on FreeBSD it pays libthr's O(N) `pshared_gc` per destroy | [FOUND] — not previously reported as a per-txn site |
| M-4 | Per-acquire statistics RMW **inside the mutex itself**: `STAT_INC(... mutex_set_wait/nowait ...)` (`mut_tas.c:104-110`, `mut_pthread.c:397-399`); `set_rd_wait/nowait` on shared (`mut_tas.c:347-351`). `HAVE_STATISTICS` is on by default (`configure.ac:1178`). Plain non-atomic `++` (`db_int.in:297-309`). | **every acquire of every latch** | a counter in the latch's own cache line, so even an uncontended acquire writes one more word of the contended line | [FOUND] |
| M-5 | Owner identity stores: `dbenv->thread_id(dbenv, &mutexp->pid, &mutexp->tid)` on every exclusive acquire (`mut_tas.c:204`, `mut_pthread.c:427,447`) **and every shared acquire** (`mut_tas.c:382`, "the last requestor's id"). That is an indirect call through `dbenv->thread_id` → `__os_id` → `pthread_self()` (`os_pid.c:44-47`). | every acquire. For a shared latch with N readers, N cores write `pid`/`tid` into one line | needed by failchk only (`DB_ENV_FAILCHK`, `mut_tas.c:151-153`) | [FOUND] RFC 0008 Risk 3 named this; [UNSURE] about its share |
| M-6 | Spin = **50 × ncpu** (`mutex.h:30`, `mut_region.c:56-60`): 3,200 on 64 vCPU, 4,800 on 96. Each failed iteration first reads `MUTEXP_IS_BUSY`, then `MUTEX_PAUSE` (`mut_tas.c:150-166`). No exponential backoff in the hybrid build: after spinning it calls `__os_yield(0,0)` = `sched_yield` (`mut_tas.c:248`, `os_yield.c:53`), then the futex. | contended acquire | | [FOUND]. [MEASURED] RFC 0008 Risk 4: the default spin was *best* at t=8. Do not re-try "lower spins" blind. |
| M-7 | **`DB_MUTEX` is 152 bytes** (hybrid+pthreads, gdb on `build_unix` debug info; this is a DIAGNOSTIC build with the same field set) and is **aligned to 4 bytes** (`MUTEX_ALIGN` default `sizeof(unsigned)`, `mutex_int.h:826`, `mut_region.c:337`). So consecutive mutexes in the array **straddle cache lines**: mutex *k* starts at 152k mod 64 ∈ {0, 24, 48, 8, 32, 56, 16, 40}. The `tas`/`sharecount`/`wait` words (offsets 88–99) of one mutex share a line with the `flags`/`u.m.mutex` of its neighbour. `mutex_set_align` exists (`mut_method.c:158`, `DB_CONFIG mutex_set_align`) but defaults to 4. | every latch | | [FOUND] the layout. [UNSURE] the effect: PG measured "little on laptops, significant on >2 sockets" for the same fix |
| M-8 | Atomics are **out-of-line functions** (`os_atomic.c:162` `__os_atomic_read`, `:227` `__os_atomic_inc`), so each `atomic_read`/`inc`/`dec`/`cas` is a call. PIN-REMEASURE's profile shows `__os_atomic_read` itself at **34.31%** self time (batched, t=96). | every atomic | | [FOUND]. [MEASURED] symbol share; the call overhead is not the coherence miss, but it does attribute the miss to that symbol |

### 2.2 Lock manager (`src/lock/`)

| # | Point | Taken by / freq | Tag |
|---|---|---|---|
| L-1 | `LOCK_REGION_LOCK` = `mtx_regenv` (§2.0). On the lock path it is taken by `__lock_getobj` when the object exceeds `sizeof(objdata)` (`lock.c:2312`), by the object free (`lock.c:2064`), by locker refill (`lock_id.c:530`), and by lock/object steal exhaustion (`lock_alloc.incl:70`). `LOCK_SYSTEM_LOCK` is live only at `lk_partitions == 1` (`lock.h:442-449`). | rare on the lock path; **the counter is dominated by the txn and log paths** (§2.4, §2.5) | [FOUND] |
| L-2 | `LOCK_LOCKERS` (all 64 stripes, `lock.h:612-616`) per op: **`__lock_sicommit` on every `DB_TXN_SERIALIZABLE` commit/abort** (`lock.c:435-436`); **`__lock_id_free` for every cursor-owned locker** (`lock_id.c:290`, from `__dbc_destroy`, `db_cam.c:234`). A non-transactional cursor on a `DB_THREAD` handle allocates `dbc->lref` with `__lock_id` once (`db_am.c:326-331`), and closed cursors keep it on the free queue, so the free happens only at handle close or cursor destroy. Also `__lock_id_free_pp` (`lock_id.c:244`), family lockers (`lock_id.c:979-982`), and the detector (L-4). | SSI: 1/txn. Non-txn cursors: **per new DBC allocation only** (amortised by the free queue) | [FOUND] |
| L-3 | `__lock_id` takes stripe 0 for `lock_id`/`cur_maxid` (`lock_id.c:112`): every `__lock_id` call (a new cursor's `lref`, handle open). Not on the txn path, which uses the txnid. | per new DBC allocation | [FOUND] — low |
| L-4 | **Inline deadlock detection.** Every blocking `__lock_get_internal` sets `region->need_dd = 1` (`lock.c:1644`) and calls `__lock_detect` (`lock.c:1662-1663`) unless `no_dd` (the locker holds nothing, `:1496`). Every `__lock_vec` with `need_dd` set runs it too (`lock.c:806-808, 826-827`). `__dd_build` takes `LOCK_LOCKERS` (all 64 stripes, `lock_deadlock.c:464`), walks **every in-use locker on every stripe** (`:517-545`), and allocates `count × nentries` bitmaps and an id array with `__os_calloc` (`:487-502`). | **per blocking lock request** | [FOUND]. [MEASURED pre-B2] 18.84% inclusive. [MEASURED post-B2] `c_confl_w` 0.80–0.85/commit at t=64 |
| L-5 | `region->need_dd`, `region->next_timeout`: written by every blocker (`lock.c:1644`, `:1634-1638`), read by every `__lock_vec` (`:806`) and cleared by the detector (`lock_deadlock.c:159`). They sit in `DB_LOCKREGION` next to `mtx_dd` and `detect`, i.e. a region-wide hot line. | per block / per lock_vec | [FOUND] |
| L-6 | Per-lock statistics: `obj_stat[ndx]` (`DB_LOCK_HSTAT`, **88 bytes**, unpadded) RMW on every get (`lock.c:1117-1118`, `:1474-1477`) and put (`:1991`, `:2214`). `part_stat` in `DB_LOCKPART` (**88 bytes**, unpadded; gdb) on every get (`:1478-1481`) and put (`:2213`). The objects are protected by their partition mutex, but **adjacent `obj_stat` buckets belong to different partitions** (`LOCK_PART = ndx % part_t_size`, `lock.h:375`), so two partitions' holders write one line. The same goes for adjacent `DB_LOCKPART`s and their `mtx_part` words. | every lock get/put | [FOUND]. [UNSURE] cost; `c_partmax` is small |
| L-7 | Lock and object free-list steal: when a partition's free list is empty, it walks **every partition** taking each `mtx_part` (`lock_alloc.incl:41-49`), then `LOCK_REGION_LOCK` (`lock_alloc.incl:70`). | rare (steady state recycles) | [FOUND] |
| L-8 | SSI `DB_LOCK_SIREAD`: `atomic_inc(&region->nsireaders)` per marker (`lock.c:1546`), a region-wide line; `atomic_inc(&td->si_ref)` (`:1545`). The `__lock_sicleanup` trigger reads `nsireaders` in **every serializable `txn_begin`** (`txn.c:357-361`). | per SIREAD lock | [FOUND] |
| L-9 | `__lock_get_api` takes the locker stripe *shared* (`lock.c:892`) — good. The internal `__lock_get` (`lock.c:929`) takes no locker stripe at all — good. | | [FOUND] (no issue) |

### 2.3 Buffer pool (`src/mp/`)

| # | Point | Freq | Tag |
|---|---|---|---|
| B-1 | `MP_GET_BUCKET` → `MUTEX_READLOCK(hp->mtx_hash)` (`mp.h:373`); the retry path takes it **exclusive** (`mp_fget.c:333`). | per fget | [FOUND]. [MEASURED] 0 waits (G13-DIAGNOSIS §2) |
| B-2 | **`atomic_inc(&bhp->ref)`** (`mp_fget.c:387`) and `atomic_dec` (`mp_fput.c:188`), plus `MUTEX_READLOCK(bhp->mtx_buf)` (`mp_fget.c:408`) = sharecount CAS (`mut_tas.c:369`) + `pid`/`tid` store (`:382`) + `STAT_INC set_rd_nowait` (`:350`), and the unlock `atomic_dec` (`mut_tas.c:542`). **That is ≥5 writes per page per descent level to two lines that every thread reading the root writes.** | per page per level | [FOUND]. [MEASURED] 71.9% self time batched (PIN-REMEASURE §4) |
| B-3 | Per-fget **region-wide** stats: `c_mp->stat.st_hash_searches` (`mp_fget.c:486-487`), `st_hash_longest` (`:488-491`), `STAT_ADJUST` (`:492`; **bug**: it adds `st_hsearch` into `st_hash_searches` instead of `st_hash_examined`, inherited from Oracle). Per-**file**: `mfp->stat.st_cache_hit` (`:476-477`). | every fget | [FOUND]. RFC 0007 removed two of these from the optimistic path only (`mp_fget.c:1463-1479`) |
| B-4 | **`++c_mp->put_counter`** on every `__memp_fput` (`mp_fput.c:147`): non-atomic, one word per cache region (`mp.h:258`, offset 416 of `MPOOL`). It is read only by `__memp_alloc`'s give-up heuristic (`mp_alloc.c:258,271`). RFC 0007 itself calls this *"one word per region, so EVERY reader on every core writes the same line"* (`mp_fget.c:1467-1471`) but removed it only from the optimistic path. | **every fput** | [FOUND] |
| B-5 | `mfp->file_written = 1` unconditional on every dirty fput (`mp_fput.c:175`) and in `__memp_fset` (`mp_fset.c:163`), in the same `MPOOLFILE` as `stat` and `multiversion`, which every fget reads (`mp_fget.c:225`). | per dirty fput | [FOUND] |
| B-6 | The CLOCK hand `c_mp->last_checked++` under `MPOOL_REGION_LOCK` (`mp_alloc.c:103`, `:202`). `MPOOL_REGION_LOCK` is taken on **every** `__memp_alloc` (`:103`); with a fresh frame it nests `MUTEX_SYSTEM_LOCK` inside it (`:123`). | per cache miss / new page | [FOUND]. PG's equivalent fix is cited in §5 |
| B-7 | MVCC: `__log_current_lsn_int` (= `mtx_regenv`) on a snapshot txn's first fget (`mp_fget.c:262`); `__txn_oldest_reader` = `__log_current_lsn_int` + **`TXN_SYSTEM_LOCK` + O(active txns) walk** (`txn_region.c:433-438`), from eviction (`mp_alloc.c:414`) and fget (`mp_fget.c:625`) whenever an obsolete-looking version is met. `__txn_add_buffer`/`__txn_remove_buffer` take `td->mvcc_mtx` per versioned page (`txn_region.c:539`, `:569`). | MVCC only | [FOUND] |
| B-8 | SSI read conflict: `__memp_si_rwconflict` takes `TXN_SYSTEM_LOCK` (= `mtx_regenv`) **while holding `mtx_hash`** (`mp_fget.c:131`) for each serializable read that meets a newer version. | SSI + version chain | [FOUND] |
| B-9 | Checkpoint `__memp_sync_int` takes **each dirty bucket's `mtx_hash` exclusively** (`mp_sync.c:452`), mallocs `nreg × htab_buckets` `BH_TRACK` (`:421`), and sorts them. Concurrent fgets on those buckets block on the exclusive latch. | per checkpoint | [FOUND]. [UNSURE] no checkpoint-concurrent measurement exists |
| B-10 | Pin-list growth beyond `PINMAX` = 4 (`db_int.in:693`) takes **`mtx_regenv`** (`mp_fget.c:1229`). B-tree split stacks exceed 4 pins. | rare per thread | [FOUND] |
| B-11 | `__memp_bh_pinned` (RFC 0007 eviction check) walks **every thread slot** in the thread table (`mp_alloc.c:992-1003`). | per eviction candidate when optread is on | [FOUND] |

### 2.4 Transaction manager (`src/txn/`)

| # | Point | Freq | Tag |
|---|---|---|---|
| T-1 | **`__txn_begin_int`**: `TXN_SYSTEM_LOCK` (`txn.c:497`) held across `__env_alloc(TXN_DETAIL)` (`:515`, a best-fit search of the size queues, `env_alloc.c:275-300`), `++last_txnid` (`:521`), two stats (`:524-528`), `thread_id` (`:532`), ~20 field stores, `SH_TAILQ_INSERT_HEAD(active_txn)` (`:568`) and `curtxns++` (`:569`). Released at `:581`. | **1/txn** | [FOUND] |
| T-2 | **`__txn_end`**: `TXN_SYSTEM_LOCK` (`txn.c:1919`) across status, `SH_TAILQ_REMOVE` (`:1921`), up to three `__env_alloc_free` (best-fit with address-queue coalescing, `env_alloc.c:449-474`), MVCC list moves, `td->mvcc_mtx` lock/unlock (`:1945-1964`), stats, `STAT_DEC nactive`. Released at `:2024`. | **1/txn** | [FOUND] |
| T-3 | SSI pivot check: `TXN_SYSTEM_LOCK` (`txn.c:853-861`). | 1/serializable commit | [FOUND] |
| T-4 | `__txn_record_fname` under **`dbp->mutex`** (`db_am.c:138-140`) for every cursor created inside a real txn on a logged handle; grows its array under `TXN_SYSTEM_LOCK` past 4 DBs (`txn_util.c:425-437`). `__txn_dref_fname` takes **`fname->mutex` = `dbp->mutex`** (`dbreg.c:173`) once per DB per txn end (`txn_util.c:492-511`). So **the per-DB-handle mutex is taken by every transaction that touches the DB, at least twice**. | per cursor in txn + per DB per txn | [FOUND] |
| T-5 | `__txn_oldest_reader`, `__txn_getactive` (`txn_chkpt.c:417`): `TXN_SYSTEM_LOCK` + walk of every active txn. ProcArray in PG had exactly this shape (§5). | per eviction-of-old-version / per checkpoint | [FOUND] |
| T-6 | `time(NULL)` per commit record (`txn.c:988`) — vDSO on Linux. No issue. | 1/txn | [FOUND] (no issue) |

### 2.5 Log (`src/log/`)

| # | Point | Freq | Tag |
|---|---|---|---|
| G-1 | `LOG_SYSTEM_LOCK` (= `mtx_regenv`) around `__log_put_next` → `__log_putr` → `__log_fill` (`log_put.c:176-358`). | **≈3.1 records per single-row txn** (RFC 0008 Finding 3) | [FOUND]. [MEASURED] 90.45% mutex vs 1.46% memmove (RFC 0008) |
| G-2 | Group-commit leader **`pwrite`s the in-memory buffer while holding `mtx_regenv`** (`log_put.c:1208-1213`), then drops it for the fsync (`:1233`). Under `DB_TXN_SYNC` every leader round therefore holds the txn/lock/env latch across a syscall. | per flush round | [FOUND]. [UNSURE] NOSYNC workloads never hit it |
| G-3 | Buffer-full `__log_write` inside `__log_fill` under the latch (`log_put.c:1388-1392`); file switch fsync under the latch (`:705`). | rare with `lg_bsize` ≥ 8 MB | [FOUND]. [MEASURED] 170× fewer in-latch writes gave no change (RFC 0008) |
| G-4 | `__log_current_lsn_int` (`log_put.c:391`): on the snapshot first-fget path, in `__txn_oldest_reader`, in `__txn_end` for MVCC (`txn.c:1870`), and in checkpoint. | per snapshot txn | [FOUND] |
| G-5 | Per-record `__os_malloc`/`__os_free` of the marshalled record (`log_put.c:2120-2130`), plus `__os_calloc`+memcpy in `__log_put` unless `DB_LOG_NOCOPY` (`:158-165`). These go to the process heap, so glibc per-thread arenas handle it. | 3.1/txn | [FOUND]. [UNSURE] probably minor on Linux |
| G-6 | The libdb log is a single byte-wise back-chained stream (`hdr->prev`, RFC 0008 §anatomy), so reservation is inherently serial. | — | [FOUND] (RFC 0008) |

### 2.6 Access methods (`src/btree/`, `src/hash/`, `src/qam/`, `src/heap/`)

| # | Point | Freq | Tag |
|---|---|---|---|
| A-1 | **`__db_new` meta write lock held to commit** (`db_meta.c:134-136`, `__TLPUT` no-op at `:263`, `db_am.h:234`). | per page allocation in a txn | [FOUND]. [MEASURED] 100% of waiters (BTREE-LOCK-SCOPE) |
| A-2 | `__bam_ca_di` on **every insert and delete** (`bt_put.c:370,381,397,501`, `bt_delete.c:281`) → `__db_walk_cursors` (`db.c:1401-1448`): `mtx_dblist` shared, **plus all 8 `cq_parts[]` mutexes of every handle on that fileid, exclusive, one at a time**, walking every active cursor. With T threads sharing a handle, each insert takes 8 partition mutexes that the other T−1 threads' cursor open/close also take. | 2 per new key/data insert (`bt_put.c:370` after the key item, `:501` after the data item) | [FOUND]. [MEASURED] P4 removed the list latch only; `__bam_ca_di` was 2.89% after |
| A-3 | Root as hot page: every descent pins the root (B-2). `__bam_rsnap_*` removes the root pin on the read path (`bt_search.c:877-882`); `__bam_opt_descend` removes interior pins when `DB_OPTREAD` is set (`bt_search.c:890`). Writes still pin and latch every level. | per op | [FOUND]. [MEASURED] rsnap +22%, optread 1.71× (t=32) |
| A-4 | `DB_RECNUM`: `__bam_adjust` walks to the root, write-locking every level, on every insert and delete (`bt_put.c:511-512`). The root's write lock is held to commit. | per insert with RECNUM | [FOUND]. Inherent to the format (counts in internal pages) |
| A-5 | Queue append: exclusive `DB_MPOOL_DIRTY` latch on the meta page per append (`qam.c:378-381`), `cur_recno++` (`:385`). Latch only, not a lock, and it is released before the record lock wait (`:401-411`). | per append | [FOUND]. The latch is short; [UNSURE] about its cost |
| A-6 | Hash: meta READ lock + latch per op (`hash_meta.c:40-44`); `nelem++` dirties the meta only without `STD_LOCKING` (`hash_page.c:2669-2672`) — good. A split takes a meta WRITE lock (`hash.c:1326`). | per op / per split | [FOUND] |
| A-7 | Heap: `h->curregion` is a per-handle hint (`heap.c:403`); region pages take an exclusive latch on space change (`heap.c:1787`). It shares the A-1 meta allocation path (`heap.c:651`). | per insert | [FOUND] |
| A-8 | `__dbc_cleanup` takes `dbp->mutex` for non-transient cursor swaps (`db_cam.c:2408`). | per cursor op that duplicates | [FOUND] |

### 2.7 Handle level (`src/db/`, `src/dbreg/`, `src/env/`, `src/os/`)

| # | Point | Freq | Tag |
|---|---|---|---|
| H-1 | `ENV_ENTER` → `__env_set_state` (`db_int.in:618-635`, `env_failchk.c:398`): when `thr_max != 0`, it hashes `thread_id` and walks one bucket chain **without a latch** (`env_failchk.c:436-451`). Only the first sighting of a thread takes `mtx_regenv` (`:473`). It writes `ip->dbth_state` (thread-local line). | every API call | [FOUND] — fine. `thr_nbucket = thr_max/8` (`:248`) |
| H-2 | `ENV_LEAVE` writes `dbth_state = THREAD_OUT` (`db_int.in:654-657`) — thread-local. | every API call | [FOUND] — fine |
| H-3 | Cursor partition pick calls `dbenv->thread_id` (`db_am.c:43`) per cursor. | per cursor | [FOUND] — cheap |
| H-4 | Replication on: `__db_rep_enter` → `REP_SYSTEM_LOCK` + `handle_cnt++` per DB API call (`rep_util.c:1197`). | per API call when replicated | [FOUND] — out of scope for G13, a global for HA users |
| H-5 | `env->mtx_env` in `__os_openhandle` (`os_handle.c:43`) — open path only. | rare | [FOUND] |
| H-6 | `PANIC_ISSET` reads `renv->panic` on every `ENV_ENTER` (`region.h:313-316`); it shares the `REGENV` header line with `mtx_regenv`'s neighbours. Read-only, so it stays Shared in every cache. | every call | [FOUND] — fine (read-shared) |

### 2.8 False sharing: shared structs without cache-line treatment

From the debug info in `build_unix` (gdb `ptype /o`; this is a DIAGNOSTIC tree, the field set matches production minus the DIAGNOSTIC-only fields):

| Struct | sizeof | Hot written fields | Padded? | Neighbours written by other cores? |
|---|---:|---|---|---|
| `DB_MUTEX` | 152 | `tas`@88, `sharecount`@92, `wait`@96, `pid`/`tid`@100-111, stats@124-147, `flags`@148 | **no**, 4-byte align | **yes**: consecutive mutexes straddle lines (M-7) |
| `DB_LOCKPART` | 88 | `mtx_part` index, free lists, `part_stat` | no | yes (array, `lock_region.c:264`) |
| `DB_LOCK_HSTAT` | 88 | every counter | no | yes, and interleaved across partitions (L-6) |
| `DB_MPOOL_HASH` | 56 | `hash_bucket` (insert/remove), `hash_page_dirty` atomic, `old_reader` | no | yes, but [MEASURED] 0 bucket waits |
| `MPOOL` | 456 | `mtx_region`@0, `last_checked`@100, `pages`@108, **`wired_pages` atomic @112**, `stat`@120-415, **`put_counter`@416** | no | `put_counter` and `stat` are written by all cores |
| `DB_TXNREGION` | 208 | `mtx_region` index, `curtxns`, `last_txnid`, `stat`, `active_txn` head | no | all under one latch, so no *false* sharing; the true sharing is T-1/T-2 |
| `DB_LOCKREGION` | 5064 | `need_dd`, `next_timeout`, `nsireaders`, `stat`; `locker_stripe[64]` **padded** (`lock.h:146-169`) | stripes only | `need_dd` + `nsireaders` + `stat` |
| `BH` | 96 | `ref`@8, `priority`@16, `mtx_buf` index@0 | no (padding measured NULL) | true sharing, not false |
| `REGENV` | 160 | `mtx_regenv` (index only), `refcnt`, `panic` | no | read-mostly; fine |

**The only explicit cache-line treatment in the tree is `DB_LOCKERSTRIPE`** (`lock.h:104`, `:146-169`).

### 2.9 NUMA and per-CPU structures

- **[FOUND] No NUMA awareness at all.** There is no `mbind`, `set_mempolicy`, `MAP_POPULATE`, `madvise`, `sched_getcpu`, or `MAP_HUGETLB`/`SHM_HUGETLB` anywhere in `src/` (grep: no hits). Regions are file-backed `mmap(MAP_SHARED)` (`os_map.c:461`, `:523`). `DB_REGION_INIT` zero-writes the file through `write()` from the creating thread (`os_map.c:216-218`), so placement is wherever the page cache put the pages. Without it, placement is first-touch by whichever thread first writes each page (the creator's `memset`s in the `*_region_init` functions).
- **[FOUND] No per-CPU or per-thread counters anywhere.** The only thread-local storage in the tree is the DIAGNOSTIC lock-order checker (`mut_order.c:45`, `__thread`). The nearest thing to per-thread state is `DB_THREAD_INFO` (pin list, state) in the shared env region, which requires `set_thread_count`.
- **[MEASURED]** CROSS-ENGINE: spread/node0 is 0.18–0.46× for libdb vs 0.21–0.33× for WT, i.e. **at least as penalised**. It is not a clean multi-socket scaling test (its own caveat).

### 2.10 Platform

- **[FOUND] Every libdb mutex in a non-`DB_PRIVATE` env is `PTHREAD_PROCESS_SHARED`** (`mut_pthread.c:129-170`, unless `DB_MUTEX_PROCESS_ONLY`). On Linux a pshared futex hashes on the *physical* page (`FUTEX_PRIVATE_FLAG` cannot be used), which is a slightly slower path through `get_futex_key` than the private hash. [UNSURE] about the magnitude; it is pure kernel cost on contended waits only.
- **[FOUND] Remaining destroy/init sites reachable per transaction** after P13 and F5: **`td->mvcc_mtx`** (M-3) only. Per cold-miss/eviction: `bhp->mtx_buf` alloc/free (`mp_alloc.c:123`, `mp_bh.c:931`) — steady-state eviction **reuses** frames (`BH_FREE_REUSE`, `mp_bh.c:918-920`), so the free happens only on `BH_FREE_FREEMEM`. Per handle open/close: `db.c` ×9 mutexes (`dbp->mutex` + 8 `cq_parts`, `db.c:497,512`).
- **[FOUND] `__mutex_refresh` callers**: only `lock.c:2202`, behind `DB_LOCK_REFRESH_LOCK_MUTEX=1` since F5. Done.
- **FreeBSD (F4/F5/B3):** M-3 has the F4 shape exactly — a destroy of a pshared mutex+cond per MVCC writer txn. **[UNSURE, high prior]** that `DB_MULTIVERSION` TPROC-C on FreeBSD is still ~O(live mutexes) per commit.

---

## 3. Design: the proposals

Each proposal carries: change · expected gain and basis · **ABI** (`sizeof` DB/DBC/DB_ENV/DB_TXN, `test/db/run_handle_sizes.sh`) · **Region signature** (`env_sig.c` hashes `sizeof` of every shared struct) · **On-disk/log** · **Admission-control risk** · **Benchmark** · **Effort**.

Workload abbreviations: **TPC** = `xe_tproc_c -e libdb -a btree -S 96 -P 256 -c 4G`, 20 s + 8 s warmup, 9 reps, arms alternated within reps, using the `p13mx.sh` driver (it records `c_*`/`x_*`). **Shape** = `scale_shape_gate.sh` at `SCALE=96` and `SCALE=32`. **P5** = `p5_log_bench` (modes nosync / notdur). **Pin** = `pin_bench`/`batch_bench` (indiv and batch, shared env). All on one binary with a runtime switch, with the DB_PRIVATE layout trap (PIN-REMEASURE §7) avoided by reusing one directory.

### Small

**S1. Stop counting mutex statistics inside the mutex on the uncontended path.**
- *Change:* in `__db_tas_mutex_lock_int` / `readlock_int` / `__db_pthread_mutex_lock`, drop the `mutex_set_nowait` / `set_rd_nowait` increment (`mut_tas.c:108-110`, `:350-351`, `mut_pthread.c:399-400`). Keep `set_wait` (taken only when we already lost the race, so the line is already contended). Report `nowait` as derived where possible (`st_nrequests − waits` for lock objects), and otherwise as "not tracked" behind a new compile-time `HAVE_MUTEX_STATISTICS_FULL` for diagnostics. Alternative: make the counters thread-local per process and sum them in `db_stat` — but they are per-mutex, so that does not scale.
- *Gain:* removes one RMW per uncontended acquire on the contended line. The basis is PG's "Pad LWLocks" and "Pin/Unpin lockfree" history (§5): every extra write to a hot BufferDesc/LWLock line mattered at >2 sockets. Expected 0–5% on 1 socket, more on 2. [UNSURE]
- *ABI:* no. *Region sig:* **no**, if the fields stay (only the store goes). *Format:* no.
- *Admission risk:* none. Nothing waits on the counter.
- *Benchmark:* Pin batched t=32/96 + TPC t=16/32/64. Metric: throughput, and `perf stat -e l2_rqsts.rfo_miss` per op (the `opt_c2c.sh` method). Expect RFO/op down. `db_stat -x -A` `nowait` columns become 0, which proves the change engaged. The `pct` stats change meaning: **user-visible stat semantics**, call it out in the release notes.
- *Effort:* 0.5 day.

**S2. Write `pid`/`tid` only when someone can read them.**
- *Change:* `pid`/`tid` exist for failchk and for diagnostics. Failchk reads them in the exclusive path (`mut_tas.c:151-153`, `mut_pthread.c:253`) and in the shared path (`mut_tas.c:392`), through `is_alive`. Two candidate rules, of different safety:
  - (a) **Skip the store when the value is already ours.** Do a load-compare first and store only if it differs, so a thread that re-acquires a latch it held last writes nothing. For a shared latch, the readers' (pid, tid) differ, so this helps only exclusive latches that one thread re-takes often. It is safe everywhere.
  - (b) **Gate the store on an environment-wide "failchk is in use" bit in the region**, set the first time *any* attaching process calls `set_isalive`. A per-process `ALIVE_ON(env)` test is **not** safe: `is_alive` is a per-process function pointer (`env_method.c:1540`), so process A (no `is_alive`) could take a latch whose stale owner process B's failchk then reads. The region bit makes the rule environment-wide; once set, it stays set.
- *Gain:* (b) removes 2 stores (12 bytes) per acquire in environments that never use failchk, which is the default. On shared latches (`mtx_buf`, locker stripes, `mtx_dblist`, `mtx_hash`) it turns a reader from a writer of the line into a pure CAS. Basis: RFC 0008 Risk 3 calls this "the highest-value follow-up". [UNSURE]
- *ABI:* no. *Region sig:* (a) no. (b) **yes**: one `u_int32_t` in `REGENV` or `DB_MUTEXREGION`, unless it fits in existing padding (`REGENV.unused` is reserved for alignment; do not reuse it). *Format:* no.
- *Admission risk:* none. *Correctness:* `MUTEX_IS_OWNED` uses `DB_MUTEX_LOCKED`, not `pid` (`mutex_int.h:973-976`), so F5's reasoning is unaffected. **`DB_MUTEX_LOCKED` must still be maintained**; it is F5's load-bearing fact (`mut_pthread.c:426,446`). A process that turns failchk on *after* latches were taken sees stale owners for latches held at that moment, which is the same exposure that exists today for any latch taken before a crash.
- *Benchmark:* the same as S1, measured as a separate arm. Also run `run_s5_failchk_spin.sh` and the TCL `dead*` tests with and without `set_isalive`, plus a **two-process** arm in which only one process sets `is_alive`.
- *Effort:* (a) 0.5 day; (b) 1 day.

**S3. Remove the global per-fput and per-fget mpool writes.**
- *Change:* (a) `++c_mp->put_counter` (`mp_fput.c:147`): replace with a progress signal that `__memp_alloc` can derive from data it already has. For example, compare `c_mp->stat.st_page_clean`/`st_ro_evict`, or `pages`, at loop start vs after `__os_yield`. Or bump it only when the refcount reaches 0 *and* the buffer is not hot (rare). (b) `st_hash_searches`/`st_hash_longest`/`st_hash_examined` (`mp_fget.c:486-492`): sample them, e.g. only when `st_hsearch > 1`, or move them under `HAVE_STATISTICS_FULL`. Fix the `:492` mis-target in the same change. (c) `mfp->stat.st_cache_hit` (`:476`): same treatment, or make it per-cache-region `c_mp` (that still shares one line, so better to sample it 1/64 using a cheap thread-local counter). (d) `mfp->file_written = 1` (`mp_fput.c:175`, `mp_fset.c:163`): `if (!mfp->file_written) mfp->file_written = 1;`.
- *Gain:* removes 2–4 unconditional global-line writes per page op. Basis: RFC 0007 already showed that removing two of these was needed for the optimistic path's RFO reduction (0.78× RFO/read). The default (pessimistic) path still pays them. [UNSURE] about size; expected visible on Pin batched.
- *ABI:* no. *Region sig:* no (fields kept). *Format:* no. **`db_stat -m` semantics change** for (b)/(c) if sampled: document it.
- *Admission risk:* low. The pin RMW (B-2) remains and becomes a bigger share. That is the intended next target (L3), not a hidden cliff.
- *Benchmark:* Pin indiv/batch, t=1/8/32/96, shared env (the default deployment). RFO/op via `l2_rqsts.rfo_miss`. TPC t=32/64 to check no regression. Gate on `__memp_alloc` give-up behaviour: the existing `test/db` cache-pressure tests and S1's AIO stress (`test/c/OS-AIO-DEADLOCK-FIX.md` harness), because `put_counter` drives the "give up after no progress" decision.
- *Effort:* 1 day.

**S4. Retain `td->mvcc_mtx` instead of alloc/free per MVCC writer transaction (a P13 for MVCC).**
- *Change:* mirror P13. Keep a small per-region free list of initialised `MTX_TXN_MVCC` mutexes, or let the mutex live with the TXN_DETAIL once S5's free list recycles details. The detail is recycled, and its mutex is kept unless `DB_ENV_FAILCHK` is set (the T8 rule, `lock_id.c:722`). Respect F6: `__mutex_free`'s already-reclaimed guard stays.
- *Gain:* Linux: removes 2 `MUTEX_SYSTEM_LOCK` acquisitions + init/destroy per MVCC writer txn (`x_region` should not move much, because P13 already showed it at ~0). **FreeBSD: removes a per-txn pshared destroy → libthr O(N) GC**, the same mechanism as F4 (25 ms per destroy at ~110k live objects). [UNSURE] that it is reached in today's FreeBSD runs: TPC sets `DB_MULTIVERSION` only with `-m`.
- *ABI:* no. *Region sig:* **no** if the free list lives in an existing `roff_t` or in the TXN_DETAIL recycle (S5). *Format:* no.
- *Admission risk:* exposes `TXN_SYSTEM_LOCK` slightly more (`x_region` → ~0 is already the case). Low.
- *Benchmark:* TPC **with `-m`** (MVCC) on Linux and FreeBSD, t=1/2/8. FreeBSD dtrace: `__mutex_free_int` calls per commit should go → 0 (`P13-FREEBSD-MUTEX-GC-2026-10.md` method). Linux: `x_inuse`/commit flat.
- *Effort:* 2 days including the failchk interaction.

**S5. Take the allocator out of the two `TXN_SYSTEM_LOCK` critical sections.**
- *Change:* add **a TXN_DETAIL free list** in `DB_TXNREGION`, the same way `free_commits` works for the log (`log_put.c:1096-1108`). `__txn_begin_int` pops from it, and `__txn_end` pushes to it, both inside the critical section they already hold. That takes `__env_alloc` (a best-fit search of the size queues, `env_alloc.c:275-300`) and `__env_alloc_free` (address-queue coalescing, `env_alloc.c:449-474`) off the per-transaction path. The list refills from `__env_alloc` only when it is empty.
- *Rejected sub-options, and why.* Two other ideas look like they shorten the section but would make it worse. (1) Initialising the detail's fields before taking the latch needs the detail first, so it would take a **second** acquisition. (2) Moving the `name`/`log_dbs` frees out of `__txn_end` has the same problem, because those frees need `mtx_regenv` as the allocator latch. RFC 0008 shows that acquisitions, not hold length, are the cost of a handoff-bound latch, so neither is proposed.
- *Gain:* the allocator walk leaves both hot sections. **This is smaller than M1**, but it is safe, and it is a prerequisite for M1: after the split, the txn latch must not need the env allocator latch on the common path. [UNSURE] ±5–10%.
- *ABI:* no. *Region sig:* **yes** (a new `SH_TAILQ_HEAD` in `DB_TXNREGION`). *Format:* no.
- *Admission risk:* **medium — this is the P12 pattern.** `mtx_regenv` is also the *log* latch. A shorter txn-side hold lets more threads reach `__log_put` sooner. Expect `c_region` waits/commit **not** to fall much, with the log side absorbing the time. Measure throughput, not the latch.
- *Benchmark:* TPC t=8/16/32/64 + **P5 notdur** (the no-log control isolates the txn-side share). Metric: `p5_notdur` `region_wait`/row at t=32 (0.348 today) should fall. TPC `c_region`/commit is reported but is **not** the success criterion.
- *Effort:* 1–2 days.

**S6. Pad what the matrix touches.**
- *Change:* (a) default `mutex_align` to 64 when `HAVE_MUTEX_HYBRID || HAVE_MUTEX_PTHREADS` on x86-64/arm64 (`mut_region.c:54-55`). `sizeof(DB_MUTEX)` 152 → 192 per slot, about 26% more mutex-region memory: with ~2 mutexes per buffer header on a 4 GB cache of 8 KB pages, that is 0.5 M mutexes × 40 B = 20 MB. (b) Pad `DB_LOCKPART` to 128 B (2 lines: `mtx_part` index + free lists on one, `part_stat` on the other). (c) Move `put_counter`, `wired_pages` and `last_checked` in `MPOOL` onto their own line (moot if S3 removes `put_counter`). (d) Move `need_dd`/`next_timeout` and `nsireaders` in `DB_LOCKREGION` onto separate lines from `stat`.
- *Gain:* PG measured "little difference on laptop-class machines, significant on >2 sockets" for the same change (§5). On a 1-socket c6id expect noise, so measure on 2 sockets. **mpool-pin's NULL is a warning:** padding BH did nothing, because the cost was true sharing. [UNSURE]
- *ABI:* no. *Region sig:* **yes** for (b)/(c)/(d) (struct sizes). (a) changes `mutex_size` at runtime, not `sizeof(DB_MUTEX)`, so the **signature does not move**. It is **attach-safe across mixed configurations**: the creator records `mutex_size` and `st_mutex_align` in the region (`mut_region.c:160-162`), and every attacher indexes the array with the region's `mutex_size` (`MUTEXP_SET`, `mutex_int.h:962-966`), not its own `dbenv->mutex_align`. [FOUND]. *Format:* no.
- *Admission risk:* none (no serialisation removed).
- *Benchmark:* TPC and Pin on a **2-socket** host (`m6i.metal` / `r8i.metal`, aws-benchmark skill), t=32/64/96, node-spread. Metric: throughput + RFO/op.
- *Effort:* 1 day plus a 2-socket run.

**S7. Make `db_stat` tell the truth about the aliased latch.**
- *Change:* (a) Label `-c`/`-t`/`-l` "region locks" as "environment region latch (shared by lock, txn, log)". (b) Report the locker stripes as Σ over all 64 (`lock_stat.c:168` reads stripe 0 only). (c) After M1, each subsystem reports its own latch.
- *Gain:* no throughput. It prevents the next misattribution; this RFC needed one.
- *ABI:* no. *Sig:* no (`DB_LOCK_STAT` fields are unchanged if it is a sum). *Format:* no.
- *Effort:* 0.5 day.

### Medium

**M1. Split `mtx_regenv` into three latches: txn region, log region, env allocator.** — *the measured top cliff*
- *Change:* allocate `DB_TXNREGION.mtx_region` and `LOG.mtx_region` as their own mutexes (`MTX_TXN_REGION` and `MTX_LOG_REGION` already exist in `mutex.h` and are **unused** for this purpose today). Keep `renv->mtx_regenv` as the env allocator + refcount latch. `__env_alloc` from the shared primary region still needs **one** allocator latch (`infop->mtx_alloc`, `env_alloc.c:219`), so the txn and log subsystems must stop allocating on the hot path (S5 does this for txn; the log already recycles `__db_commit`). Rare allocations take `mtx_regenv` nested inside the subsystem latch (allocator innermost, as `lock_order.h` `DB_LO_RANK_ALLOC` already models). **The lock subsystem keeps `mtx_regenv` as `LOCK_SYSTEM_LOCK`**, because with `lk_partitions == 1` its one-latch semantics are load-bearing (`lock.c:1015-1075`, the SSI self-deadlock guard). With >1 partition it only uses it for `__env_alloc`, which is correct as is.
- **Lock-order and data-coupling audit required** (RFC 0010 A3). Every site that relies on "txn lock == log lock == lock lock" must be found, **including data that one subsystem writes under its alias and the other reads under its own.** One such coupling found statically: **`td->begin_lsn` is written inside `__log_put` under `LOG_SYSTEM_LOCK`** (`DB_SET_TXN_LSNP`, `log.h:447-457`; the assignment at `log_put.c:189-190` "before dropping the region lock") **and read by `__txn_getactive` under `TXN_SYSTEM_LOCK`** (`txn_chkpt.c:417-422`) to compute the checkpoint LSN. Today the two are one latch. After the split, the checkpoint can read a half-written `begin_lsn`. The comment at `txn_chkpt.c:397-400` already tolerates `file`/`offset` 0 "in transition", so a torn read of a 0 is safe, but a torn *non-zero* pair is not. Fix: make `begin_lsn` a 64-bit atomic store, or have `__txn_getactive` also take the log latch (rare, so this is cheap). Other known sites: `lock.c:1015-1075` (SSI branch skips `TXN_SYSTEM_LOCK` when `part_t_size == 1` *because* it is the same latch — after the split it must **take** `TXN_SYSTEM_LOCK`, nested inside `LOCK_SYSTEM_LOCK`); `__log_put` inside `TXN_SYSTEM_LOCK`? (No: `__txn_begin_int` releases before `__lock_getlocker` at `txn.c:581-588`, and `__txn_end` frees before taking it.) `__txn_recycle_id` drops `TXN_SYSTEM_LOCK` to log (`txn.c:462-468`) — fine. `dbreg.c:283 → log_put.c:174` (filelist before log), unchanged. `__memp_si_rwconflict` takes `TXN_SYSTEM_LOCK` under `mtx_hash` (`mp_fget.c:131`) — rank check needed. `__txn_oldest_reader` calls `__log_current_lsn_int` *before* `TXN_SYSTEM_LOCK` (`txn_region.c:433-436`) — fine. The DIAGNOSTIC checker's `DB_LO_RANK_UNRANKED` for `mtx_regenv` (`lock_order.h:109-127`) can then become real ranks — a correctness *gain*.
- *Gain:* at TPC t=16–64, `c_region` = 1.43–1.59 waits/commit on one latch. G13-DIAGNOSIS §2 measures **12.78 acquisitions/txn** on it ("env region", t=8), and 9.33 log records/txn, so roughly **2–3 txn-side + ~9–10 log-side** acquisitions. The split moves about 1/5 of the arrivals onto a separate latch. That is a small share of the *count*. The bigger effect is that a txn_begin no longer queues behind a burst of 9 log appends from other transactions. [MEASURED] `p5_notdur`: the txn-only share already reaches 0.35 waits/row at t=64 with **no** log traffic. **Expected: +5–20% at t≥16 on TPC, and possibly negative alone (see the admission risk).** The basis is the size of `c_region` relative to `c_locker`: P12 removed 0.56/commit of locker waits for +12–29%, and `c_region` is 1.4–1.6/commit. The discount comes from the fact that ~80% of the latch's arrivals are log appends, which stay on one latch. **M1's real value is that it makes L1 (log consolidation) and L2 (lock-free txn begin/end) separable and measurable.** Today any log fix also changes the txn latch, and the reverse. [UNSURE]
- *ABI:* no. *Region sig:* **no** `sizeof` change (the `mtx_region` fields exist; only their *value* changes) — **but processes from before and after the split must not share a region**: an old process would take `mtx_regenv` while a new one takes the new latch. **This is a semantic region-format break that `__env_struct_sig()` cannot see.** It must bump `DB_REGION_MAGIC` or add a field so the signature moves. Recommend shipping it in the same release as S5 (which moves the signature anyway). *Format:* no.
- *Admission-control risk — the P12 lesson, stated in advance:* today `mtx_regenv` serialises txn_begin/txn_end **against** `__log_put`. After the split, threads that finished `txn_begin` arrive at the **log latch** without having queued behind log writers. Downstream of the txn latch are: (1) **the log latch**, which RFC 0008 shows is handoff-bound at ~90k txn/s; (2) **`__lock_getlocker`**, the per-stripe locker create (`txn.c:588`), which P12 already de-serialised; (3) the lock partitions. **Prediction:** the log latch's waits/commit rise. If log waits rise by more than txn waits fall, throughput drops: the P12 shape. **Mitigation built into the plan:** measure M1 *with* RFC 0008 D0 or batching, and as a standalone arm. If M1 alone regresses, it is admission control again, and D2 (L1) is the prerequisite, just as P13 was for P12.
- *Benchmark:* TPC 4 arms {base, M1, D0, M1+D0}, t=1/8/16/32/64, 9 reps. **Record `-t` and `-l` region waits separately** (they become distinct mutexes, which is also the engagement proof: `-t` and `-l` counts diverge from each other and from `-c`). Shape at 96 and 32. P5 notdur (txn latch only) and P5 nosync (both). Success = shape at scale 96 improves from −37%; throughput t=32 +10% at p<0.05 (exact Mann-Whitney 9v9, `sig.py`).
- *Effort:* 1 week code + 1 week gating (lock-order checker, TCL `txn*`/`recd*`/`ssi*`, multi-process `run_s5_failchk_spin`).

**M2. Release the meta-page allocation lock before commit (A-1).**
- *Change:* this is BTREE-LOCK-SCOPE Deliverable 2, still pending. Options in increasing invasiveness: (a) `__db_new` takes the meta lock **only as a latch** plus a short-duration lock released at the end of `__db_new` (not `__TLPUT`). This is unsafe under 2PL if the transaction aborts after another transaction has allocated a page from the freed-list head. Abort undo of `__db_pg_alloc` re-links a page into the free list, which is a physical change to the meta page — recovery is redo/undo on the meta LSN, so the question is whether undo can be logical. It needs an argument. (b) **Per-transaction page reservation**: allocate pages to the transaction in batches of k under one meta hold. That reduces the frequency, not the hold. (c) **Nested top-level action for allocation** (ARIES NTA, which InnoDB uses for page allocation in a mini-transaction): log the allocation as its own committed mini-transaction, so no transaction holds the meta lock to commit. If the user txn aborts, the page is leaked to the free list by a compensating free. **This is the correct design** and is what `DB_TXN_BULK`'s watermark partially gestures at.
- *Gain:* [MEASURED] 100% of bulk-insert waiters, put p99 282 ms → device floor at t=96 unwarmed. Steady-state TPC allocates less (~6% of puts in BTREE-LOCK-SCOPE's growing tree; TPC's ORDERS/NEWORDER grow continuously). [UNSURE] about TPC's share.
- *ABI:* no. *Sig:* no. **Format: (c) adds a log record type** (`DB_LOGVERSION` bump, recovery function), a forward-compatible log-format change, the same class as RFC 0008 D0.
- *Admission risk:* allocation becomes concurrent; next downstream is `mfp->mutex` (`last_pgno`, `mp_fget.c:962`) and file extension (`__db_file_extend`). Expect contention there on a growing file.
- *Benchmark:* `BTREE-LOCK-SCOPE` matrix (`lsc_matrix.sh`), unwarmed t=96, waiter census via `db_stat -Co`: pg0 waiters 400 → ~0. TPC unchanged-or-better. Crash/recovery: `recd*` TCL + a kill-mid-allocation test + `db_verify` free-list check.
- *Effort:* (c) 2–3 weeks; this is recovery code.

**M3. Make the deadlock detector not run inline on every block (L-4).**
- *Change:* G13 proved that `DB_LOCK_EXPIRE` alone hangs (real cycles exist), so detection must stay. Options: (a) **Run only on a timer + on block with backoff**: the first blocker runs it; subsequent blockers within δ (e.g. 1 ms, or "while a detector run is in progress") skip, using `need_dd` as a generation counter instead of a flag. (b) **Make `__dd_build` read-mostly**: take locker stripes **shared** (they are shared latches, `lock_region.c:290`), since it only reads locker fields — except `dd_id`, which it writes (`lock_deadlock.c:526`). Move `dd_id` into the detector's private `id_array` map (a hash from locker offset to dd index), so the walk is a reader. (c) Long term: an incremental waits-for graph maintained at block time (per-object waiter lists already exist, `dd_objs`).
- *Gain:* [MEASURED pre-B2] 18.84% of cycles. Post-B2, deadlocks/commit are ~0.034 (matrix `c_deadlock` at t=64) but `c_confl_w` is 0.80/commit, so the detector still runs ~0.8×/commit and walks all lockers each time. [UNSURE] about the current share; measure first (E3).
- *ABI:* no. *Sig:* (a) no (`need_dd` is `u_int32_t` already); (b) no if `dd_id` stays and is simply unused. *Format:* no.
- *Admission risk:* the detector's `LOCK_LOCKERS` currently blocks **every locker create/free** (all 64 stripes exclusive) for the walk's duration. That is a stop-the-world event that throttles `txn_begin`/`txn_end`. Removing it lets them run, which pushes harder on `mtx_regenv` (M1) and the log. Ordering: **M1 before M3**.
- *Benchmark:* TPC t=32/64; `perf` inclusive share of `__lock_detect` (target: <2%); `c_deadlock`/commit unchanged (correctness: same cycles found); `c_locker` (now Σ stripes, S7) should fall. TCL `dead001-007`, `lock001-006`.
- *Effort:* (a) 2 days; (b) 1 week.

**M4. Cut the per-insert cursor-adjust walk (A-2).**
- *Change:* `__bam_ca_di` must find every cursor positioned on (pgno, indx). Today it locks all 8 partitions of every handle and walks every active cursor. Options: (a) **A per-fileid "active cursors on this file" count per partition**, kept with `active_queue` insert/remove; skip a partition whose count is 0 — a read of a counter under no latch, racing only with a cursor *open*, which cannot be positioned on our page yet (it has no position until its first `get`, which latches the page we hold exclusively). Needs a proof. (b) **Index cursors by page** (`(fileid,pgno) → cursor list`, a small hash per handle) so the adjust touches only cursors on that page. This is what WT/InnoDB do implicitly (cursor position is a page pointer + a page-level "modification count"). (c) **Lazy cursor repositioning**: cursors record the page LSN at position time and re-search if it changed — removes `__bam_ca_*` from the write path entirely, at the cost of a re-search on the (rare) stale cursor.
- *Gain:* P4 measured `__bam_ca_di` at 2.89% after the list latch went shared. Its remaining cost is the 8 exclusive partition mutexes per insert ×2 — the same mutexes every cursor open/close takes (`db_am.c:146,472`). [UNSURE] small on TPC, large on many-threads-one-handle inserts.
- *ABI:* **(b) changes `sizeof(DB)` or `DBC`** if the index lives in the handle → `run_handle_sizes.sh` fails; A1 is already open on this. (a) adds one `u_int32_t` per `cq_parts[]` → `sizeof(DB)` +32 B → same. (c) adds an LSN to `DBC` internal (`BTREE_CURSOR`, not public) → no public ABI. *Sig:* no (handles are process-local). *Format:* no.
- *Admission risk:* cursor open/close partitions (`CQ_LOCK`) become less contended; downstream is the locker stripe for `__lock_id` on non-txn cursors (L-3) and `dbp->mutex` for `__txn_record_fname` (T-4).
- *Benchmark:* `scale_bench wrand` shared handle, t=8/32/96 (`set_thread_count` set — the CURSOR-SHARD trap); TPC. Metric: `__bam_ca_di` + `__db_walk_cursors` inclusive share; throughput.
- *Effort:* (a) 3 days + proof; (c) 2 weeks.

**M5. Take `dbp->mutex` off the per-transaction path (T-4).**
- *Change:* `__txn_record_fname` (`db_am.c:138`) only needs to protect `fname->txn_ref` (the per-txn `log_dbs` array is private to the txn). Make `txn_ref` a `db_atomic_t` (it is in `FNAME`, a shared struct → **region sig moves**) and drop the lock; `__txn_dref_fname`'s `txn_ref == 1` close decision then needs a CAS loop (`txn_util.c:503-511`). Alternatively cache "this txn already recorded this fname" in the `DB_TXN` (process-local) so the cursor-create path skips the call after the first cursor.
- *Gain:* removes two acquisitions/txn/DB of a per-handle mutex shared by all threads using that handle. [UNSURE] — G13-DIAGNOSIS §2 shows `db handle` waits/txn **falling** with threads (18.5 → 0.97), so on TPC it is not the cliff; it is a single-handle hot spot on simpler workloads. Low priority.
- *ABI:* no. *Sig:* yes (atomic) / no (caching variant). *Format:* no.
- *Effort:* 1 day (caching variant).

### Large / architectural

**L1. Consolidated log append (RFC 0008 D2 — Aether consolidation array), after M1.**
- *Change:* as RFC 0008 §D2. After M1 the log latch is its own mutex, so D2 shrinks *its* arrivals by grouping: one acquisition per group, with the leader computing the intra-group back-chain. RFC 0008's multi-process hazard (a member dies between reserve and fill → zero hole = virtual EOF) is mitigated by **leader-copies-for-members**: members publish (ptr, len); the leader memcpys all of them under the latch; members spin on a completion word. Exposure becomes "the leader dies with the latch" — today's failure mode, which is loud (`DB_RUNRECOVERY`).
- *Comparison with alternatives.*
  - **PG's WAL insertion locks.** In PG 9.4 (Heikki, 2013-07-08), `XLogInsert` reserves space under a spinlock and then copies in parallel under one of N insertion locks. libdb **cannot** adopt the parallel-copy half. A dead copier leaves a silent zero hole, which libdb reads as EOF (`log_get.c:1238`). PG avoids this only because a dead backend crashes the whole cluster.
  - **A correction to RFC 0008's reason for rejecting it.** RFC 0008 (`:745`) says PG and InnoDB use *"flat byte spaces with no back-pointer"*, and that libdb's `hdr->prev` therefore serialises reservation in a way theirs does not. **PG does have a back-pointer**: `xl_prev`, which replay verifies against the previous record (Tom Lane, 2005-05-31, *"Add test to WAL replay to verify that xl_prev points back to the previous WAL record"*). PG computes it *inside* the reservation spinlock (`insertpos_lck`; Heikki 2013-09-04 padded exactly that lock). It does so from a "previous reservation start" field, which is the same information libdb's `lp->lsn − lp->len` gives. [The function-level detail — `ReserveXLogInsertLocation` / `PrevBytePos` — is from PG's `xlog.c` and was not re-verified against the archive here: UNSURE in detail, FOUND in principle.] So the back-chain is **not** what separates libdb from PG. Reservation is serial in both, and both can compute `prev` while reserving. The multi-process dead-copier hazard is the only real obstacle, and it is decisive.
  - **InnoDB 8.0's `link_buf`** has the same dead-writer problem.
  - **Aether's consolidation array** amortises the reservation *count*, which RFC 0008 Finding 3 identifies as the cost. With leader-copies it keeps today's failure mode. **D2 with leader-copies is the right shape for libdb.**
- *Gain:* RFC 0008: batching 4 rows/txn → +189% at t=32; D2 is projected to recover the same acquisition reduction without application change. [UNSURE] bound: the bytes-under-latch qualification (P5 row: +86.8% from an empty data item) means "leader copies" moves bytes back under the latch. Measure both D2 variants.
- *ABI:* no. *Sig:* **yes** (slot array in `LOG`). *Format:* no (byte stream unchanged).
- *Admission risk:* downstream of the log latch is **`mtx_flush`** + fsync (group commit) under SYNC, and **lock release** (`__lock_vec PUT_ALL`) + `TXN_SYSTEM_LOCK` in `__txn_end` under NOSYNC. With M1 done, `TXN_SYSTEM_LOCK` is a separate latch; it becomes the next serial stage. That is why L2 follows.
- *Benchmark:* P5 nosync/sync t=1..96; TPC; `-l` region waits/commit and appends/acquisition (new stat). Crash test: SIGKILL a member mid-group, assert recovery does **not** truncate (RFC 0008 D1's test (b)).
- *Effort:* 4–6 weeks.

**L2. A ProcArray-style active-transaction table (T-1, T-2, T-5).**
- *Change:* replace `active_txn` (`SH_TAILQ`) + `TXN_DETAIL` allocation with a **fixed array of TXN_DETAIL slots** sized by `tx_max` (already a configured bound, `txn_region.c:121`), with a per-slot state word. `txn_begin` = claim a free slot (per-CPU-hinted free list or atomic bitmap) + `atomic_fetch_add(&last_txnid)`; `txn_end` = state store + release slot. **No region latch on either.** `__txn_oldest_reader`/`__txn_getactive` scan the dense array with acquire loads (PG's `ProcGlobal->xids[]` dense array, Andres Freund 2020, §5). Snapshot `read_lsn` (MVCC) uses PG's **`xactCompletionCount`** idea: cache the oldest-reader result and recompute only when a txn with a read_lsn ended.
- *Multi-process:* slots are in the shared txn region; failchk must reclaim a dead process's slots (it already aborts its txns via `__txn_failchk`). The id counter wrap (`__txn_recycle_id`, `txn.c:426-475`) needs the scan, as today.
- *Gain:* removes T-1/T-2 from `TXN_SYSTEM_LOCK` entirely. On PG the snapshot scalability series gave "~1.03× on 2-socket, ~2× on larger machines" for snapshot caching alone. [UNSURE] for libdb — expected to remove the txn half of `c_region` (measured 0.35 waits/row no-log at t=64).
- *ABI:* **`sizeof(DB_TXN)` unchanged** (`txn->td` stays a pointer). *Sig:* **yes** (`DB_TXNREGION`, `TXN_DETAIL` layout). *Format:* **no** (txnids, LSNs, records unchanged).
- *Admission risk:* `txn_begin`'s next stop is `__lock_getlocker` (P12 per-stripe) and its first log record; `txn_end`'s is `__lock_vec PUT_ALL` (partitions) and `__lock_freelocker` (stripe). All already sharded. **The remaining serial stage is the log** → L1 must be in place or the gain lands on the log latch.
- *Benchmark:* P5 notdur (pure txn cost), TPC, Shape. `-t` region waits/commit → ~0 (engagement).
- *Effort:* 4 weeks.

**L3. Contention-free buffer pin on the default (shared-env) path.**
- *Change:* this is RFC 0007's remaining half and `perf/bhpin-r1`'s lesson. Two directions: (a) **PG's single-word buffer state** (2016, Andres, "Pin/UnpinBuffer lockfree"): fold `ref`, `BH_EXCLUSIVE`, and a "locked" bit into one 32-bit atomic in `BH`, and make the shared `mtx_buf` latch acquisition and the pin **one CAS** instead of `atomic_inc(ref)` + `sharecount` CAS + pid/tid store + stat. This halves the RMWs on the hot line but keeps them **true-shared** — it is a constant-factor fix, which PG measured as large on many-core. (b) **RFC 0007 optimistic reads on by default** once (i) the comparator-safety contract from tracker item **M2** (`test/KNOWN-ISSUES.md`; `docs_src/api/c/dbset_bt_compare.md`) is accepted as the default contract, and (ii) the cursor-lifecycle wall it hits at t=96 is gone (CURSOR-SHARD is merged; RFC 0007 says the residual is `__dbc_close`/`__db_cursor_int` = 81% at t=96 per-key). (c) **Per-CPU pin counts (BRAVO/biased refcount)** — would remove true sharing but makes eviction's "is it pinned" an O(ncpu) sum and must work across processes; it is the correct long-term answer and the riskiest.
- *What prior attempts teach:* `mpool-pin` (padding) is NULL — do not pad BH again. `bhpin-r1` was 1.5× where it fired but **0.885× in shared envs** and unsafe because `__memp_fput` did not know about the latch-free pin. Any L3 variant must (1) work in shared envs, (2) teach `__memp_fput` its own protocol in the same commit, (3) be gated by a DIAGNOSTIC build that actually arms it (`pinrm_panic_control.sh`).
- *ABI:* no. *Sig:* **yes** ((a) changes `BH`). *Format:* no.
- *Admission risk:* downstream of the pin is the cursor lifecycle (`CQ_LOCK`) and, for writes, the page lock partition. RFC 0007 already shows the cursor wall at t=96.
- *Benchmark:* Pin indiv+batch, shared+private env, one binary + one directory (PIN-REMEASURE §8.4 protocol), t=1/8/32/96. RFO/op.
- *Effort:* (a) 3–4 weeks; (c) months.

**L4. NUMA placement and per-node structures.**
- *Change:* (a) **Interleave the mpool and mutex regions** (`mbind(MPOL_INTERLEAVE)` after `mmap`, Linux-only, behind a flag) so no single node owns the hot lines — this is a no-code-change-to-algorithms first step and what most engines do by default. (b) **Per-node cache regions**: libdb already has `ncache` > 1 regions (`set_cachesize(..., ncache)`), and `MP_GET_REGION` already routes by page (`mp.h:342-353`). Binding cache region *i* to node *i mod nnodes* and routing a page by its hash makes eviction and region latches node-local for 1/nnodes of the traffic. (c) **Per-CPU counters** for the stats S1/S3 cannot simply drop: a fixed array `[MAX_CPU_SLOTS]` of padded counters in the region, indexed by `sched_getcpu() % slots`, summed by `db_stat`. Correct across processes (a slot is a per-CPU slot, not per-thread, so many threads/processes sharing a CPU use atomics on the same slot — contention is bounded by CPU, not by thread count).
- *Gain:* [MEASURED] spread/node0 = 0.18–0.46× today. [UNSURE] for the fix; the CROSS-ENGINE test is not a clean scale-across-sockets test.
- *ABI:* no. *Sig:* (a) no; (b) no; (c) yes. *Format:* no.
- *Admission risk:* none for (a); (b) makes eviction node-local, exposing global MVCC `__txn_oldest_reader` (T-5).
- *Benchmark:* 2-socket bare metal, aws-benchmark skill, TPC + Pin, `numactl --cpunodebind` sweeps of 1 vs 2 nodes at equal thread counts. Metric: aggregate throughput ratio 2-node/1-node at equal threads per node.
- *Effort:* (a) 2 days; (b) 2 weeks; (c) 1 week.

---

## 4. Recommended order of attack

| Step | Items | Why here | Gate before next |
|---|---|---|---|
| **0** | S7 (stats labels + Σ stripes) | Zero risk; makes every later measurement unambiguous. | — |
| **1** | **E1** (§6) — split the `c_region` counter by caller with an instrumented build | Confirms §1 #1 before the largest change. If txn+log are not ≥80% of `mtx_regenv` acquisitions on TPC, re-rank. | E1 result |
| **2** | S1, S2, S3 | Small, no format change, no serialisation removed (no admission risk). Ship together, measure as one arm and three arms. | no regression at any t; RFO/op ↓ |
| **3** | S4 (mvcc_mtx retain) | F4 shape; FreeBSD correctness of scaling under MVCC. | FreeBSD dtrace `__mutex_free_int`/commit → 0 |
| **4** | S5 **with** M1 in one release | One signature break, not two. S5 shortens the txn section; M1 splits the latch. **Measure M1 alone and M1+RFC 0008 D0** to detect P12-style admission. | Shape@96 improves; t=32 +10% significant; log waits/commit reported |
| **5** | M3 (detector) | After M1 so released `txn_begin`/`end` pressure lands on split latches. | `__lock_detect` <2% inclusive |
| **6** | M2 (allocation NTA) | Measured, independent of 1–5, but a log-format addition and recovery code. | lsc census pg0 waiters → ~0; recd* clean |
| **7** | L1 (consolidated log) | Once M1 makes the log latch its own, D2 attacks its arrival rate. | P5 appends/acquisition ↑; crash test |
| **8** | L2 (txn slot array) | Removes the txn latch from begin/end; lands on L1's log. | `-t` waits → ~0 |
| **9** | L3, L4, M4, M5 | Read path / NUMA / single-handle hot spots. | per-item |

S6 (padding) runs in parallel with step 2 on a 2-socket host; it is cheap to try and its result is either NULL (like mpool-pin) or a cross-socket win.

---

## 5. PostgreSQL precedents: where they map and where they don't

Commit messages retrieved from the pg.ddx.io archive (git_search over `postgres`).

| PG change | What it did | Maps to libdb? |
|---|---|---|
| **"Improve scalability of WAL insertions"** — Heikki Linnakangas, 2013-07-08 (9.4) | `WALInsertLock` → N insertion slots (default 8); reserve under a spinlock, copy in parallel. | **Partially.** PG also back-chains (`xl_prev`, verified on replay since Tom Lane 2005-05-31) and resolves `prev` inside the reservation spinlock, so RFC 0008's "back-chain serialises reservation" objection does not distinguish libdb from PG. The **parallel copy does not map**: a dead libdb process mid-copy leaves a zero hole that `log_get.c:1238` reads as EOF. → L1 uses consolidation + leader-copies instead. |
| **"Keep heavily-contended fields in XLogCtlInsert on different cache lines"** — Heikki, 2013-09-04 | "if the insertpos_lck spinlock and the fields it protects are on the same cache line with other frequently accessed variables, the false sharing can hurt performance a lot." | **Yes** → S6 for `LOG`, `MPOOL`, `DB_LOCKREGION`. |
| **"Increase the number of buffer mapping partitions to 128"** — Robert Haas, 2014-10-02 | 16 → 128 `BufMappingLock` partitions. | **Already better in libdb**: one shared latch per hash bucket (`mp_region.c:280-320`, `mp_mtxcount`); [MEASURED] 0 bucket waits. No action. |
| **"Lockless StrategyGetBuffer clock sweep hot path"** — Andres Freund, 2014-12-25 | Clock hand → atomic `nextVictimBuffer`; spinlock only on wrap. | **Yes** → `c_mp->last_checked++` under `MPOOL_REGION_LOCK` (`mp_alloc.c:202`). Make the hand an atomic fetch-add and take the region latch only for `__env_alloc`. Not in the top five because TPC is cache-resident; it matters for the cold-NVMe tier (ROADMAP #6). |
| **"Move buffer I/O and content LWLocks out of the main tranche"** — Robert Haas, 2015-12-15 | Content lock inside `BufferDesc` (one line for pin + lock); main-tranche LWLocks padded to a full line; "little difference on laptop-class machines, help significantly on larger servers, especially those with more than 2 sockets". | **Yes, two ways.** (1) libdb's `BH.mtx_buf` is an *index* into a separate mutex array — pin (`BH.ref`) and latch (`DB_MUTEX.sharecount`) are **two lines** in two regions. L3(a) folds them. (2) Pad `DB_MUTEX` → S6(a). |
| **"Allow Pin/UnpinBuffer to operate in a lockfree manner"** — Andres Freund, 2016-04-11 | `flags`, `usage_count`, header lock, `refcount` crammed into one 32-bit atomic; CAS loop. | **Yes** → L3(a). Note PG's `refcount` is still true-shared; it is a constant-factor win, which is what libdb needs first. |
| **Group XID clearing** (Amit Kapila, `0e141c0fbb`, 2015; group CLOG update 2017-09-01) | One backend clears XIDs for a queue of committers under a single `ProcArrayLock` acquisition. | **Yes, as an interim for L2**: `__txn_end`'s `TXN_SYSTEM_LOCK` section could be done by a group leader for N ending transactions (the same pattern as libdb's own group commit). Cheaper than L2, keeps the list. Consider it if L2 is too large. |
| **"Pad semaphores to avoid false sharing"** — Thomas Munro, 2018-07-24 | `sem_t` array padded; regression on an 8-socket box otherwise; "matches what we do for similar hot arrays (LWLockPadded, WALInsertLockPadded)". | **Yes** → `DB_MUTEX` array (M-7) is exactly an unpadded array of hot sync objects. |
| **Snapshot scalability series** — Andres Freund, 2020-07/08 (PG 14): dense `xids[]` array; `xactCompletionCount` snapshot caching ("~1.03× on 2-socket, roughly double on a larger machine") | Snapshot building scans a dense array; cached snapshots skip the scan entirely. | **Yes** → L2. libdb's `__txn_oldest_reader` walks a linked list under the region latch (`txn_region.c:436-438`). libdb snapshots do not scan (they take one `read_lsn`), so the *begin* cost is lower than PG's; the oldest-reader *horizon* is the analogue. |
| **"Use atomic ops to hand out pages to scan in parallel scan"** — Heikki/David Rowley, 2017-08-16 | Spinlocked counter → `fetch_add`. | **Yes** → `last_txnid` (L2), `lock_id` (L-3), `last_checked`. |
| **Fast-path relation locks** (Robert Haas, 9.2; "Convert FastPathStrongRelationLocks to atomics", Nathan Bossart, 2026-09) | Weak relation locks recorded per-backend; a strong locker scans all backends. | **Not directly.** libdb has no relation-level weak locks on the hot path except the **handle lock** (`DB_HANDLE_LOCK`, `db.in`). Its page locks are already partitioned. A fast-path would help `lk_partitions=1` users only. |

Where PG's path **does not** apply: every PG fix assumes one postmaster and that a dead backend ⇒ crash-restart. libdb's north star (multi-process regions with `failchk` recovery) forbids lock-free *publication* protocols that leave half-written shared state without an owner (RFC 0008 §Multi-process). L1 and L2 are designed around that; L3(c) is the hardest to make safe.

---

## 6. What could not be determined statically — experiments

All are measurement-only (an instrumented build or existing counters); none changes behaviour.

- **E1. Who takes `mtx_regenv`, and who waits?** Add a per-call-site acquire/wait counter (or `bpftrace` uprobe on `__db_tas_mutex_lock_int` filtered by `mutex == renv->mtx_regenv` index, with `ustack` aggregated to the caller). TPC scale 96 t=8/32/64. *Decides:* the M1 premise, and whether D0/L1 must come first. Prediction: `__txn_begin_int`+`__txn_end` ≈ 2/txn, `__log_put` ≈ 9/txn (G13's 9.33 records/txn), `__log_current_lsn_int` + others < 1/txn, summing to G13's 12.78. **Record the waits per caller, not just the acquisitions.** If txn callers wait disproportionately (they queue behind log bursts), M1 pays. If waits are proportional to arrivals, M1 alone moves ~20% of the arrivals and D0/L1 is the bigger lever.
- **E2. Does the txn half alone show the cliff?** `p5_log_bench notdur` already shows wait fraction 0.19 at t=64. Re-run with `db_stat -t` and `-e` per run, t=1..96, and with S5 as an arm. *Decides:* whether S5 is worth shipping before M1.
- **E3. Post-B2 detector share.** `perf record --call-graph dwarf` on TPC t=32/64, P13+P12 build. `__lock_detect` inclusive %. *Decides:* M3's rank.
- **E4. Mutex line writes.** On one box, A/B arms {base, S1, S2, S1+S2} via a runtime env var; `perf stat -e l2_rqsts.rfo_miss,mem_load_l3_hit_retired.xsnp_hitm` per op on Pin batched t=32 and TPC t=32. *Decides:* S1/S2 value; whether M-4/M-5 explain RFC 0008's 14× hold inflation.
- **E5. `put_counter` and per-fget stats.** Same harness; arm = S3. *Decides:* S3 value on the default path.
- **E6. `mutex_align=64` via `DB_CONFIG`** (no code change: `mutex_set_align 64` is already accepted, `env_config.c:79`). TPC + Pin on 1-socket and 2-socket. *Decides:* S6(a) **for free** — this experiment needs no build.
- **E7. MVCC on FreeBSD.** TPC `-m` t=1/2/8 on FreeBSD 14.5, dtrace `__mutex_free_int` count per commit. *Decides:* S4 urgency.
- **E8. Stripe 0 vs Σ stripes.** Instrumented `db_stat` summing all 64 `mtx_locker_stripe` waits on TPC t=64. *Decides:* whether P12's 0.076/commit understates locker contention.
- **E9. Checkpoint interference.** TPC with a 10 s `txn_checkpoint` thread vs without; p99 latency and throughput dip at checkpoint. *Decides:* B-9's rank.
- **E10. Pshared vs private futex cost.** `DB_PRIVATE` + `DB_THREAD` (all mutexes `PROCESS_ONLY`) vs shared env, same binary, TPC t=32 — but **hold the directory constant** (PIN-REMEASURE §7). *Decides:* §2.10 bullet 1.
- **E11. 2-socket scaling.** `r8i.metal`/`m6i.metal`, TPC + Pin, threads ∈ {16,32,64} pinned 1-node vs 2-node. *Decides:* L4 and S6.

---

## 7. Decisions the maintainer must make

1. **A second region-format break after v2026.10.5?** M1 (+S5) is the top measured cliff and is a *semantic* region break that `__env_struct_sig()` would not detect by `sizeof` alone. Ship it in **v2026.11** (a second recreate-regions upgrade one release after P12) or hold it until enough region changes accumulate (S5, S6(b-d), L1, L2) to justify one break? Recommendation: batch M1+S5+S6 into one break; run E1/E2 now on the current format.
2. **`db_stat` semantics.** S1/S3 stop counting uncontended acquisitions and fget hits on the hot path. Accept "nowait = derived/approximate" in `db_stat -x`/`-m`, or keep exact counts behind a configure flag (`--enable-full-statistics`) that benchmarks must not use?
3. **Owner-id stores (S2(b)).** Is it acceptable that `db_stat -x -A` no longer shows latch owners in environments where no process has ever configured `is_alive`? (Failchk is unaffected, because it requires `is_alive`, which sets the region bit.)
4. **Log format for M2.** Accept a new log record type (nested top-level page allocation) — a forward-compatible `DB_LOGVERSION` bump — to remove the meta-page convoy?
5. **`DB_PRIVATE` fast paths.** ROADMAP's design decision 1 blesses `DB_PRIVATE` as the speed target. L3(c) and some L2 variants are far simpler single-process. Does this project accept proposals that are faster *only* under `DB_PRIVATE` (and correct-but-unchanged multi-process), or must every scalability change win in shared environments too (bhpin-r1's 0.885× says the latter has teeth)?
6. **Interim vs final for the txn latch.** PG-style **group clearing** in `__txn_end` (small, keeps the list) vs **L2** (slot array, larger, removes the latch). Choose one to fund.
7. **Benchmark host.** Every padding / NUMA claim (S6, L4, E6, E11) needs a 2-socket box; G12's runner is still absent. Fund a recurring 2-socket EC2 slot or accept that cross-socket claims stay [UNSURE].

---

## North-star check

- **Embedded / no server:** nothing proposed adds a server or background thread except optionally L1's leader (which is a committing thread, not a daemon) and M3(a)'s timer, which reuses the existing on-block trigger.
- **ACID + WAL + recovery:** M2(c) adds a log record type through the normal versioned path; L1 keeps the byte order and back-chain, and its leader-copies variant keeps today's failure mode (latch holder dies → loud). No proposal reorders log bytes or leaves an ownerless reserved hole.
- **All access methods:** M1/S5/L1/L2 are below the AM layer. M2 changes `__db_new`, shared by btree/hash/heap/recno; queue does not use it.
- **Multi-process regions:** every proposal keeps shared state in the region and keeps failchk's ability to reclaim (S4 follows P13's `DB_ENV_FAILCHK` rule; L2 must reclaim slots in `__txn_failchk`). L4(c) per-CPU slots are per-CPU, not per-process. L3(c) is called out as the hardest.
- **Format stability:** stated per proposal. Public ABI changes: only M4(a)/(b) (`sizeof(DB)`), which joins A1's open decision.
- **Footprint:** S6(a) costs ~26% more mutex-region memory; L2 sizes the txn table to `tx_max` up front (it is already a bound).

## Alternatives considered

- **Lower `tas_spins`** — measured and rejected (RFC 0008 Risk 4: default best at t=8; P9 retracted). Not proposed.
- **Pure pthreads instead of hybrid** — `.agent/notes/scaling-findings.md` measured hybrid 30–64% slower on a 24-thread box *before* U6b made hybrid the default. That predates P1/P4/P12/P13 and was a different mutex mix; **[UNSURE]** whether it still holds. A one-binary A/B is impossible (configure-time), so it needs two builds with the env directory held constant. Worth one E-series run; not proposed as a change.
- **More lock partitions / more mpool buckets** — both already measured uncontended.
- **Reserve-then-copy log** — disqualified by RFC 0008 for multi-process.
- **Per-handle page caches below the root** — rsnap-ml, 0.063×.
- **Padding BH** — mpool-pin, NULL.

## Risks & open questions

- **The M1 admission risk is the main risk of this RFC.** The static count and G13's 12.78 acquisitions/txn already suggest the log is ~80% of the arrivals. Splitting removes the txn side's queueing behind log bursts, but it lets txn_begin/txn_end flood the log latch faster. That could be a net loss without L1 or D0 — exactly the P12 story. The plan measures M1 alone and with D0.
- **Static ranking of S1/S2/S3 may be wrong in either direction.** mpool-pin's NULL shows that "obvious" line-sharing fixes can be worth nothing; PG's LWLock padding shows they can matter on bigger boxes. E4–E6 settle it before any is claimed.
- **c_region per commit is roughly flat (1.43–1.59 in P13+P12) from t=16 to t=64**, while throughput falls. A flat per-commit wait count can still mean rising cost per wait (hold time inflates 14×, RFC 0008). E1 must record wait *time*, not just counts.
- **The deadlock detector's post-B2 share is unknown** (E3).
- **`mutex_align` attach compatibility** (S6(a)) is resolved statically. The region records `mutex_size`, and attachers index with it (`mutex_int.h:962-966`). Even so, a mixed-version attach test belongs in the gate.

## Prototype / evidence

None built. Everything above is either a `file:line` citation in `master` @ `2d8397dfe` or a number recomputed from TSVs already committed under `test/bench/`. Struct sizes and offsets are from `gdb ptype /o` on `build_unix/.libs/libdb-2026.0.so` (a pre-P12 DIAGNOSTIC tree, which is why `DB_LOCKREGION` shows 1000 there; the P12 number 5064 is from the matrix report). No build was run and nothing was provisioned.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Pending.
- **Rationale:**
- **Conditions / follow-ups:**

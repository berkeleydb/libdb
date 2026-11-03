# Cursor-cache A/B results (RFC 0021)

Per-thread transient-cursor cache, review item O-D1. base = `origin/master`
(`e1fe5c2c4`), branch = `perf/cursor-cache`.

## Host and method

- **Host:** `c6id.metal` (128 vCPU, 2 sockets, Debian 12), local NVMe formatted
  XFS at `/data`, mounted `noatime`. Governor `performance`, THP `never`,
  `swappiness=1`, `numa_balancing=0`.
- **Workload:** `test/bench/batch_bench.c` arm `indiv` — a shared `DB_THREAD`
  btree handle, each op a **non-transactional** `DB->get(db, NULL, &key, &data,
  0)` (random key, 1,000,000 keys, 100-byte values). This is the exact path the
  cache targets and the same workload that produced the 85%-cursor-lifecycle
  profile in `BATCHED-READS-RESULTS.md`.
- **Why not xrel_kv:** `xrel_kv` runs every op inside an explicit transaction.
  A txn-associated cursor is **not** cacheable (its locker belongs to a txn that
  commits/aborts between ops — see RFC 0021 §1 eligibility), so the cache does
  not engage there and `xrel_kv` shows no delta by construction. The cache
  helps the non-transactional `DB->get`/`DB->put` path, which `batch_bench
  indiv` measures directly.
- **A/B:** 6 reps, interleaved, arm order rotated each rep; each arm loads its
  own 1M-key database fresh per rep; 10 s measured window per cell. Working set
  (~104 MB data + btree) fits the 512 MB cache (notfound=0 throughout, so every
  key is present and in cache). Count is committed read ops (keys/s).

## Result — `DB->get` per key, keys/s (median of 6), CV, p99

| t | base med (k/s) | CV% | branch med (k/s) | CV% | ratio | base p99 µs | branch p99 µs |
|---|---------------|-----|------------------|-----|-------|-------------|---------------|
| 1 | 544,331 | 0.5 | 587,380 | 0.9 | 1.08× | 2.0 | 2.0 |
| 4 | 748,551 | 3.4 | 806,988 | 2.4 | 1.08× | 6.1 | 4.1 |
| 16 | 1,328,496 | 1.8 | 1,517,649 | 1.7 | 1.14× | 16.4 | 8.2 |
| 32 | 1,636,261 | 2.2 | 1,973,034 | 2.3 | 1.21× | 32.8 | 16.4 |
| 64 | 1,479,038 | 5.2 | 2,267,230 | 5.0 | **1.53×** | 65.5 | 32.8 |
| 96 | 997,152 | 2.8 | 2,802,036 | 4.4 | **2.81×** | 262.1 | 32.8 |

All cells CV ≤ 5.2% (≤ 10% gate met; no cell marked †).

**Reading.** Base scales to t=32, then the per-handle cursor-queue mutex becomes
the wall: throughput *declines* from 1.64M (t=32) to 1.48M (t=64) to 997k
(t=96), the classic negative-scaling signature of the cursor lifecycle. The
branch keeps climbing — 1.97M → 2.27M → 2.80M — so the gain grows with
contention: 1.08× single-thread, 1.53× at t=64, **2.81× at t=96**. Tail latency
improves in lockstep: p99 262 µs → 33 µs at t=96 (an 8× tail cut).

## Profile — t=96 read phase, both arms (`perf record -a --call-graph dwarf`)

The 96 worker threads are mostly **off-CPU** on the cursor-queue mutex, so the
decisive measure is how much of the 128-core machine stays idle:

| arm | `swapper` (idle) share | throughput |
|-----|------------------------|------------|
| base | **66.99%** | 997k/s |
| branch | **46.76%** | 2.80M/s |

Base leaves two-thirds of the machine idle — the threads are serialized on the
per-handle cursor-queue mutex (`__db_tas_mutex_lock_int`/`unlock` are the top
libdb on-CPU symbols, and the rest is the futex/TAS wait that shows as idle).
The branch cuts idle to under half and turns that recovered time into 2.8× the
useful read throughput. The remaining idle in both arms is background dirty-page
writeback from the load (`iomap`/`folio`/`nvme` kernel symbols), not the read
path.

Raw: `cc_indiv.tsv` (this directory). Profile summaries: `prof_base.summary`,
`prof_branch.summary`.

## Single-thread cost

t=1 is +8% (the cache saves the free-queue scan + two partition-mutex
acquisitions per op even uncontended), so there is **no single-thread
regression** — the common case is strictly faster. A worst-case all-miss arm
(cache never hits, e.g. a thread fanning across > `DBC_CACHE_SLOTS` handles)
pays only the few-instruction eligibility test and TLS lookup on top of the
master path.

## Functional gates (shared dev box, `--enable-diagnostic`)

- `test/db` tier: 30 PASS / 4 SKIP (no javac/dtrace) / **0 FAIL**, including the
  new `run_cursorcache_adjust.sh` (PASS) and `run_handle_sizes.sh` (PASS —
  `sizeof(DB)=1744`, `sizeof(DBC)=552`, **unchanged**).
- `run_cursorcache_adjust.sh` sabotage arm: with `-DDBC_CACHE_SABOTAGE`
  (stale-handle epoch guard disabled) the test **FAILS** deterministically
  (ASan SEGV, use-after-free at `__dbc_cache_put`), and **PASSES** with the
  guard intact — so the test exercises a real invariant.
- TCL `run_method -btree test001..010` and `-hash test001..010`: 20/20 PASS,
  0 failures.
- Region signature and `env_sig.c` byte-identical to base (no struct in the
  signature changed).

---

## Re-measurement after the review #238 fixes (P1b, 2026-11-03)

Review #238 found three defects in the first prototype (orphaned cursors on a
handle close anywhere; parked cursors from exited threads; a racy plain-`u_int32_t`
epoch). The fix reworks the stale-slot machinery (epoch as a change signal +
per-handle serial + `env->dblist` liveness reconcile; thread-exit flush; atomic
epoch). This section re-measures the read-path win **on the fixed branch** to
confirm the win survives.

- **Host:** `c6id.metal` (128 vCPU, 2 sockets, Debian 12), local NVMe XFS at
  `/data` `noatime`; governor `performance`, THP `never`, `swappiness=1`,
  `numa_balancing=0`.
- **Arms:** `base` = the fork point `e1fe5c2c4`; `branch` = fixed `perf/cursor-cache`
  (`15045e705`) with the cache **on**; `cache-off` = the **same** fixed binary
  with `DB_NO_CURSOR_CACHE=1` (the master-equivalent alloc/free path). The
  `cache-off` arm is the cleanest control: it isolates exactly the cache's
  effect in one binary.
- **A/B:** 6 reps, interleaved, arm order rotated each rep; DB loaded fresh per
  (arm, rep); 10 s measured window per cell; working set (~104 MB) in the 512 MB
  cache (`notfound=0`). `batch_bench indiv 1000000 1`.

### Result — non-txn `DB->get` per key, keys/s (median of 6), CV, p99

| t | base med (k/s) | CV% | branch med (k/s) | CV% | cache-off med (k/s) | CV% | ratio br/base | ratio br/off | base p99 µs | branch p99 µs |
|---|---------------|-----|------------------|-----|---------------------|-----|---------------|--------------|-------------|---------------|
| 1 | 521,420 | 0.4 | 548,624 | 0.5 | 510,240 | 1.5 | 1.05× | 1.08× | 2.0 | 2.0 |
| 16 | 1,280,632 | 1.8 | 1,470,616 | 1.9 | 1,319,645 | 1.6 | 1.15× | 1.11× | 16.4 | 8.2 |
| 32 | 1,663,194 | 2.2 | 1,896,430 | 1.9 | 1,654,966 | 4.8 | 1.14× | 1.15× | 16.4 | 16.4 |
| 64 | 1,603,846 | 4.9 | 2,224,867 | 6.4 | 1,607,494 | 5.5 | 1.39× | 1.38× | 65.5 | 32.8 |
| 96 | 991,014 | 0.7 | 2,780,778 | 5.7 | 980,008 | 1.5 | **2.81×** | **2.84×** | 262.1 | 32.8 |

All cells CV ≤ 6.4% (≤ 10% gate met).

**The win survives the fix unchanged:** 2.81× at t=96 (identical to the
prototype's 2.81×), 1.39× at t=64. The `cache-off` arm tracks `base` at every
point (e.g. 980k vs 991k at t=96), confirming the entire delta is the cache and
not some other branch difference. The t=64 ratio reads 1.39× here vs the
prototype's 1.53× purely because this run's `base`/`cache-off` t=64 median ran
higher (1.60M vs the prototype's 1.48M, a noisier cell); the same-binary
`branch/cache-off` ratio (1.38×) is the apples-to-apples figure and agrees.

### Profile — t=96, system-wide `perf record --call-graph dwarf` (fixed branch)

Top on-CPU libdb symbols (this metal box's mutex backend spins — TAS/hybrid —
so the cursor-queue contention shows as on-CPU spin rather than off-CPU futex,
but the signature is the same):

| arm | top symbol | share | reading |
|-----|-----------|-------|---------|
| base   | `__db_tas_mutex_lock_int` (+`unlock`) | **~90%** | CPU burned spinning on the per-handle cursor-queue mutex |
| branch | `__bam_cmp` / `__bam_search` (key compare + descent); mutex lock 0.43% | **~6% real work** | the cursor-queue mutex is gone from the hot path; time is now the actual B-tree get |

The cache removes the cursor-lifecycle mutex exactly as RFC 0021 predicts and
moves the bottleneck to legitimate work. Raw data `p1b-ab.tsv`, profiles
`p1b-base96.prof` / `p1b-branch96.prof` in `results/cursor-cache-p1b/`.

### Functional gates after the fix (dev box, `--enable-diagnostic`)

- `sizeof(DB)` **unchanged at 1744** (`cursor_cache_gen` fits the existing 4-byte
  padding hole after `adj_fileid`), `sizeof(DBC)` unchanged at 552 — `run_handle_sizes`
  PASS, **no ABI size change, no escape token needed**.
- `run_cursorcache_adjust` PASS (correct) / sabotage arm FAIL (ASan use-after-free).
- `run_cursorcache_orphan` PASS (ratio 1.02, +0 KB) — FAILs 23.98× on the pre-fix head.
- `run_cursorcache_parkcost` PASS (ratio 1.03 at 256 exited threads) — FAILs 2.14× pre-fix.
- Diagnostic `test/db` tier `run_all` rc=0, 0 FAIL.
- A1–A4 + A6 ASan-clean; A7 epoch race under **TSan** PASS with zero races on the
  epoch/serial globals (the prototype's race is gone).
- TCL btree+hash test001–010: 20/20 PASS.

---

## Re-measurement after the live-handle-registry fix (P1b rework, 2026-11-03)

Review of the first fix found a cross-thread ENV use-after-free (rev238_a10):
the stale-slot validator dereferenced a slot's ENV, which can be freed when the
thread that closes a handle is not the one that cached a cursor on it.  The
rework replaces the `env->dblist` liveness check with a process-global
live-handle registry (keyed `(dbp, cursor_cache_gen)`, one process mutex, no
env); reconcile and eviction validate against the registry only, dereferencing
nothing.  The registry mutex is taken on an **epoch change** (a handle closed),
on **open** and on **close** -- never per `DB->get`.  This section re-measures
the read win to confirm the rework did not add a hot-path lock.

Same host/method as above (`c6id.metal`, 6 interleaved reps, `batch_bench
indiv 1000000 1`, 10 s cells; `base` = fork point `e1fe5c2c4`, `branch` = the
reworked fix, `cache-off` = same binary with `DB_NO_CURSOR_CACHE=1`).

| t | base (k/s) | CV% | branch (k/s) | CV% | cache-off (k/s) | CV% | br/base | br/off | base p99 µs | branch p99 µs |
|---|-----------|-----|--------------|-----|-----------------|-----|---------|--------|-------------|---------------|
| 1 | 517,026 | 1.0 | 546,892 | 0.8 | 514,076 | 0.9 | 1.06× | 1.06× | 2.0 | 2.0 |
| 16 | 1,307,876 | 2.2 | 1,489,628 | 1.8 | 1,314,154 | 2.4 | 1.14× | 1.13× | 16.4 | 8.2 |
| 32 | 1,649,375 | 3.4 | 1,924,697 | 4.0 | 1,610,175 | 2.8 | 1.17× | 1.20× | 24.6 | 16.4 |
| 64 | 1,510,169 | 7.9 | 2,281,464 | 8.6 | 1,599,098 | 4.1 | **1.51×** | 1.43× | 65.5 | 32.8 |
| 96 | 976,010 | 1.1 | 2,814,246 | 7.8 | 1,005,420 | 2.4 | **2.88×** | 2.80× | 262.1 | 49.1 |

**The win is intact: 2.88× at t=96, 1.51× at t=64** -- statistically identical
to the pre-registry fix (2.81× / 1.39-1.53×, within the t=64 cell's run-to-run
noise).  `cache-off` still tracks `base` at every point, so the registry adds no
per-get cost, confirming the structural fact that the hot path takes no registry
mutex.  Raw data `results/cursor-cache-p1b/p1b-registry-ab.tsv`.

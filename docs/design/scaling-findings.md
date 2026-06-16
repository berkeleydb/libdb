# Empirical multi-core scaling findings

Measured on **meh** (Linux 6.12, Xeon E5-2697 v2, 12c/24t, single socket) with
`lab/bench/scale_bench.c` against a stock Autoconf build of `master`. All data
fit in a 512 MB cache (no device I/O on the read paths).

## Throughput vs threads

Read-random, in-cache point gets on a 200k-key B-tree:

| threads | ops/sec | scaling vs 1t |
|--------:|--------:|--------------:|
| 1 | 200,010 | 1.0x |
| 2 | 234,004 | 1.2x |
| 4 | 399,883 | 2.0x |
| 8 | **464,661** | 2.3x (peak) |
| 12 | 448,775 | — |
| 16 | 416,366 | — |
| 24 | 408,518 | **negative** |

Read throughput **peaks at ~8 threads and then declines** — on a 24-thread box
we get ~2.3x, not ~12-24x.

## Where the time goes (perf, rrand @ 24t, self-time)

```
66.70%  [kernel] (futex)                 <- threads blocked in the kernel
35.43%  __db_pthread_mutex_lock
31.58%  __db_pthread_mutex_unlock
30.55%  __lll_lock_wait / 28% __lll_lock_wake
26.33%  __memp_fget        22.55%  __memp_fput
21.90%  __atomic_inc       20.91%  __atomic_dec
48.56%  __bam_search (B-tree descent; calls __memp_fget per level)
```

BDB's own wait counters (`lockpart%`, `mpoolhash%`, region waits) are **near
zero** — the contention does not show up there because it is the **per-page
buffer mutex** (a pthread mutex → futex) and the **page reference count** atomic,
not a lock-region/partition mutex.

## Root cause

Every B-tree search descends through the **root and internal pages**. Each
`__memp_fget` (a) takes that buffer header's mutex and (b) atomically increments
its pin/reference count; `__memp_fput` reverses it. Because the **root page is
fetched by every operation on every thread**, its mutex and refcount cache line
become a single global serialization point:

- under the pthread-mutex build, that mutex goes to the kernel → the **66.7%
  futex** storm (and negative scaling past the point where futex contention
  dominates);
- the refcount atomic inc/dec (~43% combined self-time) ping-pongs one cache
  line across all cores.

## Workload contrasts (confirming the cause)

- **rhot** (all threads read ONE key): adds lock-manager **partition-mutex**
  contention — `lockpart%` rises 13% (4t) → 37.5% (24t) — because every read
  takes a *page read lock* on the single hot page. Reads don't conflict
  (`conflict%=0`) but acquiring the read lock latches the partition. Page-level
  read locking is pure overhead for read-mostly workloads.
- **wrand** (random writes, auto-commit): ~**733 ops/sec single-threaded** —
  fsync-per-commit bound — and the **lock region mutex** is heavily contended
  (`lockreg_w` in the thousands). Writes need group commit.

## What this means for the ROADMAP (data-driven re-prioritization)

1. **#2 latch-free / contention-free buffer-header access is the #1 read-scaling
   fix.** The dominant cost is the per-page mutex + refcount on hot (root /
   internal) pages. Directions: don't take a kernel-bound mutex to pin a
   resident page (optimistic/version-validated reads, LeanStore-style), shard or
   bias-lock the pin count, and avoid pinning hot internal pages on the read
   path.
2. **#7 cache-line / false-sharing** is the close-second cost (the refcount
   atomic). Splitting/aligning the pin counter pairs directly with #2.
3. **#3 group-commit WAL** is the clear write-path win (writes are fsync-bound),
   plus the lock-region mutex needs attention under writes.
4. **#4 lock manager**: page-level read locks are needless overhead for
   read-mostly access (snapshot/SI reads already skip them — see SSI) and are
   the hot-key bottleneck.
5. **Lower priority than the ROADMAP assumed on these boxes:** the mpool **hash**
   mutex and lock **partitions** are *not* contended here (~0 wait), and both
   hosts are single-socket so NUMA placement (part of #1) can't be validated.
   The sharded-buffer-pool hash work matters less than the per-page-pin work
   until we test on a multi-socket NUMA box.

**Next target: #2 (+#7).** Prototype a contention-free pin for resident pages
and re-run this sweep to confirm the 8-thread ceiling lifts.

## Isolation experiments (which shared structure is the cap?)

Two workloads were added to separate the candidate causes. Numbers below are an
in-cache random-read sweep on a 12-core Apple Silicon laptop (noisier and fewer
cores than `meh`, but the *pattern* matches the 24-core `perf` profile and is
enough to rank the causes). ops/sec:

| threads | `rrand` (locked, shared db) | `sepdb` (own db/thread) | `snap` (MVCC, no page locks) |
|--------:|----------------------------:|------------------------:|-----------------------------:|
| 1       | ~560k                       | ~627k                   | 463k                         |
| 2       | ~600k                       | ~693k                   | 572k                         |
| 4       | ~480k                       | ~638k                   | 631k                         |
| 8       | ~340k                       | ~502k                   | 459k                         |
| 12      | ~340k                       | ~417k                   | 389k                         |

- **`sepdb`** gives every thread its own database file, so there is no shared
  root/internal page. It is **30–50% faster than `rrand`** at 4–12 threads,
  which confirms the **shared hot page is a real bottleneck**. It still declines,
  so it is not the *only* one — the threads still share one env (mpool region,
  locker table); the `lockers%` signal is the per-operation locker allocation.
- **`snap`** reads in a per-thread `DB_TXN_SNAPSHOT` transaction: MVCC reads take
  **no page read locks** and reuse a single locker, removing the entire
  lock-manager per-op cost. It scales to **4 threads (1.36×)** where `rrand` is
  already flat — but it **plateaus at 8–12 threads at the same level as
  `rrand`**. The only per-op work `snap` still does that `rrand` also does is
  `__memp_fget`/`__memp_fput` (pin/unpin every page on the root→leaf path).

**Conclusion / ranking (measured, not assumed):**

1. **#2 buffer-header page pin is the dominant cap.** Even with *all* locking
   removed (`snap`), throughput still ceilings at 8–12 threads, because every
   read pins the shared root/internal pages through `__memp_fget` (per-page
   mutex + atomic refcount). This is the change that can lift the ceiling.
2. **#4 lock manager is a secondary cost** in the 2–8 thread range: page read
   locks + per-op locker allocation (`lockers%` 25–62%). Snapshot/lock-free
   reads relieve it but do not remove the #2 ceiling.
3. **#3 write path** remains fsync-bound and independent (group commit).

This is why the prototype order is **#2 first** (contention-free pin for
resident pages), then #4 (cache/reuse lockers, or default read-mostly access to
the lock-free path), then #3 (group commit).

## Reproduce

```sh
# on a build host:
cc -O2 -pthread lab/bench/scale_bench.c -Ibuild_unix -Lbuild_unix/.libs -ldb-5.3 -o scale_bench
LD_LIBRARY_PATH=build_unix/.libs ./scale_bench rrand 200000 3 1 2 4 8 12 16 24
#   workloads: rrand | rhot | wrand | sepdb | snap
#     rrand  shared db, locked reads (baseline)
#     sepdb  one db file per thread (isolates shared-page contention)
#     snap   per-thread MVCC snapshot txn (isolates lock-manager cost)
```


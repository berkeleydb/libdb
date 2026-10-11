# v2026.10.5 release benchmark — FreeBSD 14.5, metal, x86_64 and aarch64

**What it measures.** The released code paths against the pre-2026.10.4 paths **on one binary**, switched at
runtime (`DB_LOCK_REFRESH_LOCK_MUTEX=1 DB_NO_LOCKER_MUTEX_REUSE=1 DB_NO_LOCKER_SHARD=1` = "old"). The region
*layout* is P12's in both arms (it is compile-time), so this isolates the three algorithms. Harness:
[`relbench.sh`](relbench.sh). Every arm recreates its environment (P12's switch is read at region create);
arms alternate within each rep with a rotating start; a discarded prewarm precedes the measured reps; both
`db_stat -c` (lock) and `-x` (mutex) are recorded per rep. TPROC-C via `xe_tproc_c`, scale 96, 256-byte pad,
4 GiB cache, 20 s measured after 8 s warmup, local NVMe (UFS). `kern.ipc.umtx_vnode_persistent=1` for the run,
because the loader exits before the measured process attaches (B3).

## x86_64 — c6id.metal, 128 vCPU, Intel Xeon (Ice Lake)

Data: [`RELBENCH-2026.10.5-freebsd-amd64.tsv`](RELBENCH-2026.10.5-freebsd-amd64.tsv), 91 rows, 0 degenerate
(no `LOAD_FAILED`/`TIMEOUT_STALL`/`NO_VERDICT`). 6–7 reps per cell (the sweep was stopped one rep short at
t ≥ 16 to release the host; every cell has ≥ 6).

| threads | 2026.10.5 tpm (cv) | old paths tpm (cv) | speedup |
|--:|--:|--:|--:|
| 1  | 640,521 (0.9%) | 353 (2.0%) | **1,815×** |
| 2  | 731,968 (**21.1%**) | 288 (4.3%) | 2,542× |
| 4  | 958,017 (7.5%) | 296 (3.2%) | **3,237×** |
| 8  | 639,728 (3.5%) | 313 (1.6%) | **2,044×** |
| 16 | 393,282 (2.5%) | 332 (1.7%) | **1,183×** |
| 32 | 323,878 (2.2%) | 384 (2.2%) | **843×** |
| 64 | 290,609 (4.1%) | 466 (1.0%) | **624×** |

The t=2 "new" cell is over the project's 10% cv ceiling (per-rep 541k–1,020k); its speedup is directionally
the same as its neighbours but **is not quoted as a number**. Every other cell is within the ceiling.

**What the speedup is.** Almost all of it is F4 + F5: the pre-2026.10.4 paths destroy a process-shared pthread
mutex per transaction and per contended lock release, and FreeBSD's libthr makes each destroy O(live shared
objects). This is not a claim that 2026.10.5 is three orders of magnitude faster than 2026.10.4 — 2026.10.4
already shipped F5 and P13. P12's own contribution on top of P13 was measured on Linux (+12% to +29% at
t=16–64, [`P13-P12-MATRIX-2026-10.md`](P13-P12-MATRIX-2026-10.md)).

**Where it still does not scale.** 2026.10.5 peaks at t=4 and falls 70% by t=64. Per-commit latch waits,
"new" arm (medians):

| t | `c_region` | `c_locker` | `c_part` | `x_region` |
|--:|--:|--:|--:|--:|
| 2  | 0.321 | 0.0001 | 0.044 | 0.0008 |
| 8  | 0.930 | 0.0031 | 0.123 | 0.0021 |
| 32 | 1.039 | 0.0459 | 0.266 | 0.0046 |
| 64 | 0.913 | 0.0759 | 0.311 | 0.0055 |

`c_region` dominates at every thread count — and per RFC 0015 it is **not** a lock-manager latch: lock, txn
and log "region" locks are one physical mutex, `renv->mtx_regenv`. That is the next cliff, and RFC 0015's
first medium proposal.

## aarch64 — c7gd.metal, 64 vCPU, Graviton3 (Neoverse-V1)

Data: [`RELBENCH-2026.10.5-freebsd-arm64.tsv`](RELBENCH-2026.10.5-freebsd-arm64.tsv), 98 rows, 0 degenerate,
**7 reps in every cell, every cell's cv ≤ 6%**. (Completed after the tag; published as an addendum.)

| threads | 2026.10.5 tpm (cv) | old paths tpm (cv) | speedup | `c_region`/commit |
|--:|--:|--:|--:|--:|
| 1  | 47,606 (0.7%)  | 481 (5.9%) | **99×**  | 0.000 |
| 2  | 82,609 (1.0%)  | 503 (3.9%) | **164×** | 0.026 |
| 4  | 132,336 (0.6%) | 500 (4.5%) | **265×** | 0.060 |
| 8  | 144,823 (2.3%) | 504 (3.8%) | **287×** | 0.088 |
| 16 | 142,548 (2.0%) | 539 (3.8%) | **264×** | 0.097 |
| 32 | 134,979 (1.4%) | 587 (3.5%) | **230×** | 0.092 |
| 64 | 132,059 (1.6%) | 666 (2.9%) | **198×** | 0.099 |

**Two things differ from x86_64, and both are explained by the mutex backend.** aarch64 has no TAS mutex in
this tree, so `configure` selects plain `POSIX/pthreads/library`: every mutex acquire is a pthread call.

1. **Absolute single-thread throughput is ~13× lower** (47.6 k vs 640.5 k tpm at t=1). Not CPU speed: a
   pthread-backed mutex on FreeBSD pays a `_umtx_op` path the x86 hybrid's spin fast path avoids.
2. **It scales flatter.** aarch64 plateaus from t=8 and holds within 9% to t=64; x86_64 peaks at t=4 and loses
   70% by t=64. `c_region` waits per commit are **~10× lower on aarch64** (0.09 vs 0.91–1.09) — the slower
   per-acquire path is itself admission control in front of `mtx_regenv`, the same mechanism RFC 0012 named.
   This is the expected *shape* of a slower lock, not evidence that aarch64 is better at scaling.

So the two platforms point at the same two follow-ups: an **AArch64 TAS/hybrid mutex** (which would also fix B3
for arm64's default build), and **RFC 0015's `mtx_regenv` split**, which x86_64 shows is the cliff once the
mutex path is fast. Expect the second to matter on aarch64 only after the first lands.

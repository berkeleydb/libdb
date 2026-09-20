# RFC 0008: Scalable WAL append — shrinking `__log_put`'s critical section

- **Status:** Draft
- **Type:** Prospective
- **Author:** libdb maintainers
- **Date:** 2026-09-20
- **Tracking:** defect **P5** in `test/KNOWN-ISSUES.md`. Follows P1 (locker
  stripes) and P4 (cursor-walk latch), both fixed, which exposed this as the
  next ceiling.
- **Prototype:** measurement harness in `test/bench/p5_log_bench.c`,
  `test/bench/p5_sweep.sh`; results in `test/bench/P5-LOG-APPEND-2026-09.md`.

---

## Summary

`__log_put` serializes every log append behind one region latch
(`LOG_SYSTEM_LOCK`), and with P1 and P4 fixed that latch is where the engine
stops scaling: **56% of all time at t=64, 87% of it on this one latch.** This
RFC establishes the anatomy of that critical section with line citations,
surveys the two systems that have solved precisely this problem in production
(PostgreSQL's WAL insertion locks, InnoDB 8.0's redo-log rewrite) plus the
canonical research (Aether), and ranks concrete designs for libdb.

**The RFC's central finding is negative and it changes the ranking.** The
obvious design — PostgreSQL-style *reserve-then-copy*, which shortens the
critical section by moving the buffer copy outside it — addresses a cost this
workload does not actually pay. Two measurements say so. First, the profile:
`__memmove` is **1.46%** of total time while mutex acquire+release is
**90.45%**. Second, a zero-code experiment: raising the log buffer from 32 KB
to 8 MB cut writes issued *inside* the latch by **170×** (2,608/s → 15/s) and
changed throughput **not at all** (t=96: 88,435 → 94,421 ops/s, inside the
noise floor). If neither the copy nor the syscall inside the critical section
is the cost, then shortening the critical section is not the fix.

A control with the log append path suppressed but transactions and locking
intact (`DB_TXN_NOT_DURABLE`, valid at every thread count) quantifies the prize:
the log is **36–62% of removable per-transaction cost at t≥8**, so even a
*perfect* append fix buys at most +57% at t=96. Reserve-then-copy's 10–20% of
the critical section, applied to that, is a few percent of throughput.

What the same experiments *do* implicate is the **handoff**: on a 96-vCPU box
each waiter burns up to **4,800** test-and-set attempts on one shared cacheline
per acquisition (`MUTEX_SPINS_PER_PROCESSOR` = 50 × 96 CPUs,
`src/dbinc/mutex.h:30`, `src/mutex/mut_region.c:56-60`), and every successful
acquisition *writes* to that cacheline twice (`mutexp->pid`/`tid`,
`src/mutex/mut_tas.c:203-204`). A spin-count probe that is pure configuration —
no code change — moves throughput more than any critical-section surgery is
projected to. That reorders the proposals: **D0 (spin/handoff tuning, no format
break, no durability argument) before D1 (reserve-then-copy, format break, hard
durability argument).**

The recommendation is therefore **do not implement reserve-then-copy yet**, and
land the cheap, safe, measurable thing first.

## Motivation

### The measured ceiling

With P1 and P4 fixed, throughput on a 96-vCPU box peaks at **t=2 (~203k
ops/s)** and *declines* to **~104k at t=96** — a scaling curve that goes the
wrong way. Profiling at t=64 attributes **56% of all time to `__log_put`, 87%
of that to the log-region latch**.

The device is nowhere near saturated. On the reference box (`fio`, 4k random
write, io_uring, iodepth 64, 4 jobs): **494k IOPS / 1930 MiB/s**. The log
subsystem's own counters, read from `DB_ENV->log_stat` in the same runs, show
libdb pushing **79.5 MiB/s at t=1 and 43.1 MiB/s at t=96** — i.e. **4.1%
falling to 2.2%** of the device. Throughput *and* device utilisation both
decline as threads are added. Whatever the log is doing at t=96, it is not
waiting for storage.

### Why upstream fixes did not help

P1 and P4 each removed real contention and each produced a modest or null
throughput change. That is the expected behaviour of a queueing network with a
saturated serial stage downstream: removing contention upstream of a serialized
bottleneck moves the queue rather than shortening it. Every committing
transaction must append to the log, and today the append is serialized. This is
the stage that has to change for the others to pay off.

## North-star check

This RFC recommends **no library change**, so nothing here breaks anything
today. The check below is what each *proposed* design would have to satisfy, and
it is the reason the ranking came out as it did.

| Constraint | D0 (fewer appends) | D1 (reserve-then-copy) | D2 (consolidation) | D3 (backpressure) |
|---|---|---|---|---|
| Embedded / no server process | OK | OK | OK — but note D2 needs no background thread, unlike InnoDB's model | OK |
| ACID | OK | **At risk** — see multi-process below | OK with leader-completes-for-members | OK |
| Crash recovery | OK; byte order and back-chain unchanged | **At risk** — a reserve-then-die hole is virtual EOF (`log_get.c:1238-1240`), so recovery silently truncates | Hazard bounded to leader death, equivalent to today's "holder dies with latch" | OK |
| All access methods (B-tree/Hash/Queue/Recno/Heap) | **Needs verification** — `__db_pitem` has callers in several AMs (Risk 2) | OK, AM-agnostic | OK, AM-agnostic | OK |
| Multi-process correctness | OK | **Fails as specified.** PG and InnoDB are single-process; libdb's buffer is shared across mutually untrusting processes and `__mut_failchk` only reclaims `DB_MUTEX_PROCESS_ONLY` mutexes (`mut_failchk.c:50-52`). Needs a crash-visible reservation + a `failchk` completion pass before it is admissible. | Admissible with the leader-completes mitigation | OK |
| On-disk log format | **Log version bump** (new record type) — forward-compatible, the standard path | Unchanged | Unchanged | Unchanged |
| Region format / ABI (`__env_struct_sig`) | **Unchanged** — no new region field | **Breaks** — new fields in `struct __log` (`env_sig.c:76`) | **Breaks** — slot array in the region | Unchanged if the lag is derived from existing `DB_LOG_STAT` fields |

**The gate, stated plainly.** D1 is the only design that fails a north-star
constraint outright rather than merely needing work: in libdb's multi-process
configuration it converts a *detectable* failure (a process dying while holding
the log latch wedges the environment into `DB_RUNRECOVERY`) into *silent data
loss* (a hole that recovery reads as end-of-log). That is disqualifying on its
own, independently of the performance findings, and it is the single most
important difference between libdb and the two systems whose pattern D1 copies.

## `__log_put` anatomy

All citations are `src/log/log_put.c` unless noted.

### The call chain under the latch

```
__log_put                          :91
  ├─ (outside the latch) __os_calloc + memcpy of caller record   :161-165
  ├─ (outside the latch) __log_encrypt_record                    :167
  ├─ (outside the latch) __db_chksum                             :173   ← precedent
  ├─ LOG_SYSTEM_LOCK                                             :176
  │   ├─ __log_put_next                                          :179 → :452
  │   │    ├─ old_lsn = lp->lsn                                  :476
  │   │    ├─ version check / adv_file                           :483
  │   │    ├─ file-switch test                                   :513
  │   │    │    └─ __log_newfile  (rare)                          :524 → :665
  │   │    │         └─ __log_flush_int(NULL, 0)                  :705  ← fsync under latch
  │   │    └─ __log_putr                                         :542 → :798
  │   │         ├─ save b_off / w_off / f_lsn for rollback       :833-835
  │   │         ├─ hdr->prev, hdr->len                           :842-843
  │   │         ├─ LOG_HDR_SUM (fold prev+len into caller chksum) :868
  │   │         ├─ *lsn = lp->lsn        ← LSN ASSIGNED HERE     :879
  │   │         ├─ __log_fill(hdr)                               :886 → :1335
  │   │         ├─ __log_fill(payload)                           :894 → :1335
  │   │         │    ├─ f_lsn = *lsn when b_off == 0             :1363
  │   │         │    ├─ memcpy(bufp + b_off, addr, nw)           :1382  ← DATA MOVEMENT
  │   │         │    ├─ b_off += nw                              :1385
  │   │         │    └─ if buffer full: __log_write (pwrite!)    :1388-1392
  │   │         ├─ lp->len = hdr->size + dbt->size               :897
  │   │         └─ lp->lsn.offset += lp->len                     :898
  │   ├─ lsnp->file/offset = lsn.*   (return LSN to caller)      :189-190
  │   ├─ [rep master only] unlock, send, relock                  :207,270-284
  │   ├─ __log_flush_commit (if DB_FLUSH|DB_LOG_WRNOSYNC)        :331 → :550
  │   └─ STAT(++lp->stat.st_record)                              :343
  └─ LOG_SYSTEM_UNLOCK                                           :358
```

**The confirmed opportunity:** the latch covers both *reservation* (bump
`lp->lsn` at `:898`, `lp->b_off` at `:1385`) and *data movement* (the `memcpy`
at `:1382`). Only reservation needs mutual exclusion. **The confirmed
complication:** measurement says the data movement is not what costs (§Findings).

**Precedent for splitting work out of this exact critical section already
exists in this exact function.** `__log_putr` accepts a checksum its caller
computed *outside* the latch, and the comment at `:846-852` names it as an
optimization: *"If we were passed in a nonzero checksum, our caller calculated
the checksum before acquiring the log mutex, as an optimization."* The
`LOG_HDR_SUM` at `:868` then folds in the two fields (`prev`, `len`) that were
not yet known outside. So "compute outside, commit inside" is established
practice here, not a foreign import — which materially lowers the design risk
of doing more of it.

### State under the latch, and what each piece actually needs

| State | What it is | Needs | Fast path? |
|---|---|---|---|
| `lp->lsn` | append frontier, the LSN handed to the caller (`:879`, `:898`) | **atomic reservation + total order** | yes |
| `lp->b_off` | write cursor in the shared buffer (`:1385`) | **atomic reservation** | yes |
| `dblp->bufp[...]` | the shared buffer bytes (`:1382`) | none once the range is privately reserved | yes |
| `lp->len` | length of the *last* record; feeds `hdr->prev` of the next (`:897`) | **ordering** — it is the back-chain | yes |
| `hdr->prev` | byte-wise back-pointer (`:842`) | **ordering** (derived from `lsn.offset - len`) | yes |
| `lp->f_lsn` | LSN owning the buffer's first byte (`:1363`) | mutual exclusion with the flusher | yes (first record per buffer) |
| `lp->w_off` | file offset of next write (`:1594`) | mutual exclusion with the flusher | on buffer-full only |
| `lp->stat.st_*` | counters (`:343`, `:1596-1608`) | **atomicity only** | yes |
| `lp->s_lsn` | durable watermark | already on `mtx_flush`, not the region latch (`log.h:263-266`) | flush only |
| `lp->commits`, `ncommit`, `t_lsn`, `in_flush` | group-commit queue (`:1240-1260`) | mutual exclusion | commit only |
| file-switch bookkeeping | `lp->lsn.file++`, `w_off = 0`, persist record (`:713-719`) | mutual exclusion | **rare** |

Three entries are the hard part of any sharding design:

1. **`lp->len` / `hdr->prev` make the log a byte-wise back-chained list.** A
   record's header stores the *offset of the previous record*. So a reservation
   cannot compute its own header without knowing the length of the record
   immediately before it. This is a genuine serial dependency in the *format*,
   not an implementation artifact.
2. **`lp->f_lsn` and `w_off` couple the append path to the flusher.** A
   reserved-but-not-yet-filled hole in the buffer must never be written.
3. **File switch** flushes *and* fsyncs while holding the region latch
   (`:705` → `__log_flush_int`), and the comment at `:689-698` explains why the
   latch cannot be dropped there: a thread with a smaller record could otherwise
   see space in the old file after the switch decision.

### What recovery requires of the on-disk byte order

This is the invariant that kills any design that reorders bytes. Three
independent mechanisms each require that the log be a **densely packed,
strictly ascending, gap-free byte sequence per file**:

1. **Forward scan stops at the first bad record.** `__log_recover`
   (`src/log/log.c:303`) positions at the start of the last file and walks
   `DB_NEXT` until `__logc_get` fails (`src/log/log.c:375-381`); the end of the
   log is *defined* as where that walk stops, and `lp->lsn` is set from it
   (`src/log/log.c:393-397`). A hole in the middle of the file therefore
   truncates the log at the hole — **every record after it is silently lost**,
   even though the bytes are present.
2. **A zero header is virtual EOF.** `__logc_hdrchk`-adjacent logic in
   `__logc_get` treats `hdr->prev == 0 && hdr->chksum[0] == 0 && hdr->len == 0`
   as end-of-file (`src/log/log_get.c:1238-1240`). A reserved-but-unfilled
   region is exactly this bit pattern (buffers and files are zero-filled —
   `__db_file_extend`/`DBLOG_ZERO`, `:1546-1552`). So a crash mid-copy does not
   produce a detectably corrupt log; it produces a log that **ends early and
   looks clean.**
3. **The back-chain is load-bearing in both directions.** `DB_PREV` walks via
   `logc->prev = hdr.prev` (`src/log/log_get.c:775`, `:489`), and the
   partial-record reassembly in `__logc_inregion` walks the in-buffer chain
   looking for the record whose `prev` points at the target
   (`src/log/log_get.c:990-1001`). Both break if `prev` is wrong.

**Conclusion, stated for the record: any design that writes log bytes out of
order, or that can leave a gap in the byte stream at a crash, is dead on
arrival.** Not "risky" — it silently discards committed transactions, because
mechanism (1) and (2) turn a gap into a clean-looking early end-of-log rather
than an error. Every design below is evaluated against this first.

### Multi-process: where libdb differs from PG and InnoDB

PostgreSQL and InnoDB both solve this problem inside a **single process**, with
all writers as threads sharing a heap and a fate: if a backend dies mid-copy,
PostgreSQL restarts the whole cluster and replays WAL. libdb's log buffer is in
a **shared-memory region attached by mutually untrusting processes**, and libdb
is expected to survive one of them dying via `DB_ENV->failchk`.

`__mut_failchk` (`src/mutex/mut_failchk.c:20`) recovers a mutex held by a dead
process **only if** `DB_MUTEX_PROCESS_ONLY` is set (`:50-52`) — which the log
region latch is not. So today a process that dies holding the log latch wedges
the environment into `DB_RUNRECOVERY`, which is at least *loud*. Under
reserve-then-copy, a process that dies **after reserving and before filling**
leaves a hole in the buffer while holding no lock at all, and by the invariant
above that hole becomes a silent early end-of-log. **Reserve-then-copy converts
a detectable failure into silent data loss in exactly the configuration libdb
supports and its two model systems do not.** This is the single most important
asymmetry in this RFC and §Design weighs every proposal against it.

## Prior art

### PostgreSQL WAL insertion locks — the closest analogue

PostgreSQL faced this exact defect: a single `WALInsertLock` serializing all
appends. The fix (8.1 through 9.4, `src/backend/access/transam/xlog.c`) is
*reserve-then-copy-in-parallel* and is the direct model for design D1.

**Mechanism.** `XLogInsertRecord` reserves space by atomically advancing a
shared counter — `ReserveXLogInsertLocation` bumps `Insert->CurrBytePos` under
a dedicated spinlock (`insertpos_lck`), or `ReserveXLogSwitch` for the
rarer file-switch case — returning the byte range `[StartBytePos, EndBytePos)`.
The record is then copied into the WAL buffers **outside** that spinlock, by the
inserting backend itself, in parallel with other backends copying into their own
ranges.

**The hard part — tracking which reserved ranges are filled.** This, not the
reservation, is the real content of the design. PostgreSQL uses
`NUM_XLOGINSERT_LOCKS` (8 by default) *insertion locks*, each with an
`insertingAt` field (`XLogCtlInsert->WALInsertLocks[i].l.insertingAt`). A
backend acquires one insertion lock (round-robin by backend, so the 8 locks are
much less contended than one), reserves its range, publishes how far it has
progressed in `insertingAt`, and releases. A process that needs to know the
safe, contiguous, fully-written prefix — the WAL writer flushing, or
`XLogWrite` — calls `WaitXLogInsertionsToFinish`, which reads every insertion
lock's `insertingAt` and **waits for the minimum to pass the target LSN**. The
"filled prefix" is thus derived as `min(insertingAt)` over a small fixed array,
not tracked per byte. `LWLockWaitForVar`/`LWLockUpdateVar` exist specifically to
make "wait until this lock's variable passes X" cheap.

**Assumptions, and whether they hold for libdb.**

| PG assumption | libdb |
|---|---|
| Single process; a backend crash → crash-recovery of the whole cluster | **False.** Multi-process, `failchk` is expected to recover. This is the fatal asymmetry (§Multi-process). |
| WAL is a flat byte space with **no back-chain**; a record's header has no pointer to the previous record | **False.** `hdr->prev` (`:842`) is a byte-wise back-pointer, so reservation *n* needs the length of reservation *n−1*. |
| A fixed small number of concurrent inserters, bounded by `NUM_XLOGINSERT_LOCKS` | Transferable. |
| Recovery reads WAL as one ordered byte stream | **True, and preserved** — PG's reservation assigns *contiguous ascending* ranges, so the on-disk order is unchanged. No merge at recovery. |

**Does it preserve a single recoverable byte order?** Yes — and this is why it
is the right model. Reservation is a monotonic counter bump, so byte order on
disk is identical to what a single global lock would produce. The only new
hazard is a *temporally* incomplete prefix, which is precisely what
`insertingAt`/`WaitXLogInsertionsToFinish` exists to prevent the flusher from
writing.

### InnoDB 8.0 redo log rewrite — the other production answer

MySQL 8.0 removed `log_sys_t::mutex` from the redo append fast path (WL#10310).
Pre-8.0 InnoDB looked much like libdb today: one mutex covering LSN assignment
and the copy into the log buffer.

**Mechanism.** Writers atomically reserve an LSN range
(`log_buffer_reserve`, a CAS on `log.sn`), copy into the (now much larger) log
buffer without holding any global mutex, and then *announce* completion. Three
dedicated background threads replaced the inline work: **`log_writer`** (moves
buffer → OS), **`log_flusher`** (fsync), **`log_closer`** (advances the dirty
frontier); plus `log_write_notifier`/`log_flush_notifier` to wake waiters.

**The hard part — `link_buf`.** InnoDB's answer to "which reserved ranges are
filled" is a dedicated lock-free data structure, `link_buf<lsn_t>`, used twice:
`log.recent_written` tracks ranges copied into the buffer, and
`log.recent_closed` tracks ranges whose dirty-page bookkeeping is done. A
`link_buf` is a fixed-size circular array of slots indexed by LSN mod capacity;
a writer finishing `[start, end)` stores `end` at slot `start`, and a reader
(`log_advance_ready_for_write_lsn`) walks forward from the current frontier
following the links, advancing while contiguous. That walk yields exactly the
**contiguous filled prefix**. Capacity is bounded
(`innodb_log_recent_written_size`), so a writer that would outrun the window
waits — which *is* InnoDB's append backpressure.

**Comparison of the two "filled prefix" structures** — this is the design
choice libdb would have to make:

| | PostgreSQL | InnoDB 8.0 |
|---|---|---|
| Structure | 8 insertion locks, each with `insertingAt` | `link_buf`, circular array of LSN links |
| Prefix derived by | `min(insertingAt)` over a fixed array | forward link-walk from frontier |
| Concurrency bound | `NUM_XLOGINSERT_LOCKS` inserters | window capacity, not inserter count |
| Cost per append | acquire/release one of 8 LWLocks | one CAS + one store |
| Backpressure | buffer-full wait | window-full wait (explicit) |
| Fits libdb's back-chain? | No better than InnoDB — neither has one | No |

**Assumptions.** Same single-process assumption as PG (a crashed thread takes
the server down; recovery replays redo). Also assumes redo records are
**self-describing without a back-pointer**, so a reserved range can be filled
independently. Preserves one recoverable order; no merge at recovery.

### Aether (Johnson, Pandis, Stoica, Athanassoulis, Ailamaki; VLDB 2010)

"Aether: A Scalable Approach to Logging" is the canonical research treatment,
and it names four distinct log bottlenecks — which is useful here because it
separates the one libdb has from the ones it does not.

- **Early lock release** — release locks before the commit record is durable.
- **Flush pipelining** — decouple a transaction's worker thread from its flush;
  the worker detaches at commit and a separate thread completes it after the
  fsync. Group commit batches flushes; flush pipelining stops the *worker* from
  blocking. libdb's `__log_flush_int` leader/follower is group commit but not
  flush pipelining — the follower blocks on `mtx_txnwait` (`:1125`).
- **Consolidation array (consolidated buffer allocation)** — the mechanism most
  relevant to P5. Instead of every thread taking the log mutex, threads
  *combine* their requests: a small fixed array of slots, each thread CASes its
  length into a slot to join a group, one thread per slot becomes the leader,
  acquires the mutex **once** for the whole group's combined length, and the
  group members then copy their records into the group's reserved region in
  parallel. This converts N lock acquisitions into N/k, and is a better fit for
  a back-chained format than per-record reservation, because the *group leader*
  can compute the intra-group back-chain locally.
- **Decoupled buffer fill** — the copy happens outside the critical section.

**Assumptions.** Shared-memory multicore, single process, atomic CAS. Preserves
a single byte order (the consolidation array hands out contiguous ranges).
Aether's measurements are on Shore-MT.

### Silo (Tu, Zheng, Kohler, Liskov, Madden; SOSP 2013)

Silo decentralises logging: per-core log buffers written by per-disk logger
threads, and **epoch-based group commit** — time is divided into short epochs
(~40 ms) and a transaction is durable once its epoch is; the *epoch*, not a
per-record LSN, is the recovery unit.

**Does it preserve a single recoverable byte order? No — and it does not need
one.** Silo's serialization order within an epoch is recovered from
transaction-local read/write sets, and recovery merges per-core logs by epoch.
This requires that no transaction's outcome depend on intra-epoch order across
logs. libdb's redo records carry **page-LSN preconditions** (`CHECK_LSN`,
`src/dbinc/log.h:411`; `__log_check_page_lsn` at `:2256`), i.e. physiological
redo against a specific page image — so a merge that reorders two records
touching the same page is unsound. **Silo's decentralisation does not transfer.**
Its epoch idea *does* inform backpressure (§D0/D3).

### Scalable Logging through Emerging Non-Volatile Memory (Wang & Johnson, VLDB 2014)

Distributed logging with per-core log buffers in NVM, using **passive group
commit**: because NVM writes are durable at cacheline granularity with no
fsync, the log can be decentralised and ordering resolved by LSN comparison at
recovery. **Assumes byte-addressable persistent memory** — hardware libdb does
not require and cannot require. Requires a merge at recovery. Not transferable;
included because it is the standard citation for "multiple logs, one order."

### Taurus / Taurus-MM (Xia et al., VLDB 2020 / 2023)

Taurus provides **multiple parallel logs with no global LSN**: each transaction
records a vector of dependencies (an "LSN vector" — per-log watermarks) instead
of a single sequence number, and recovery replays respecting the partial order
the vectors encode. Taurus-MM extends this to multi-socket/NUMA with vector
compression.

**Mechanism relevance.** Taurus is the most rigorous answer to "can a
multi-core engine have N logs and still recover?" — yes, if you replace the
total order with an explicit partial order. **This requires a merge at recovery
and a format change to every record** (to carry the dependency vector). For
libdb this means: new record format, new recovery driver, `db_log_verify`
rewritten, and the `hdr->prev` back-chain replaced. That is a different product,
not a patch. Recorded as out of scope and *why*, so nobody re-derives it.

### Kafka and Redpanda — where the analogy breaks

Both scale writes by **partitioning**: a topic is split into partitions, each
partition is an independently-ordered append-only log owned by one broker (and
in Redpanda, by one core — a thread-per-core Seastar shared-nothing design with
one log per core, its own memory, its own io_uring queue, no cross-core locks).
Ordering is guaranteed **within** a partition and explicitly **not** across
partitions.

**Where it breaks, precisely.** The transferable part of partitioning is
availability of an *independent* order per partition. A single-node ACID engine
cannot use it, for a reason more specific than "it needs a total order":

- libdb's redo is **physiological** — a record says "apply this delta to page
  P, whose LSN must be X" (`CHECK_LSN`, `src/dbinc/log.h:411`). Two
  transactions touching the same page produce records whose replay order is
  fixed by the page-LSN chain. Partitioning the log by transaction or by thread
  puts those two records in different partitions with no defined relative
  order, and recovery cannot reconstruct one, because the page LSN tells it only
  that it is missing something, not where to find it.
- Partitioning by **page** (or by database file) would preserve the per-page
  chain — and is the one variant that is not immediately unsound — but a
  transaction spanning two pages then commits across two logs, requiring
  two-phase commit between them, plus a merged order for the *transaction*
  records. libdb's single `__txn_regop` commit record has no place to express
  "durable in log 3 at position 91 and log 7 at position 12."
- Kafka's consumers tolerate per-partition order because application semantics
  supply the cross-partition ordering (or do not need it). Recovery has no
  application to defer to.

**What does transfer:** (a) per-core/per-process *buffering* with a
consolidation step before a single ordered commit — that is Aether's
consolidation array, arrived at from the other direction; (b) **backpressure**,
below.

### Backpressure: how these systems signal "storage is saturated"

libdb has **no** mechanism here, which is a gap independent of P5's latch.

| System | Mechanism |
|---|---|
| Kafka | Client quotas (byte-rate/request-rate per principal). The broker computes a throttle delay and **delays the response**, pushing the stall into the producer without an error. `max.in.flight`/`buffer.memory` bound the producer's own queue; `BufferExhaustedException` when full. |
| Redpanda | Built-in, no tuning: per-shard io_uring queues with explicit scheduling groups, and Seastar's reactor accounts for disk bandwidth so an overloaded shard naturally stops accepting. |
| PostgreSQL | `wal_buffers` full → the inserting backend must *itself* write WAL before it can reserve (`AdvanceXLInsertBuffer` → `XLogWrite`), so the producer becomes the writer. Plus `max_wal_size`/checkpoint throttling. |
| InnoDB | Two layers. `log_free_check()` before a mini-transaction: if the redo log's free space is below a watermark, the thread **stalls** until the checkpointer advances. And the `link_buf` window: a writer that would outrun `recent_written` capacity waits. Space is *reserved* before work begins, so the stall happens before a transaction is half-done. |

**The libdb analogue.** The natural seams are `DB_TXN->commit()` and
`DB->put()`. Today, if the device cannot keep up, the log buffer fills and
`__log_fill` performs the write inline under the region latch (`:1388-1392`) —
the queue forms *on the latch*, invisible to the caller, and every other
appender waits behind a syscall. That is accidental, unbounded-in-latency
backpressure. A deliberate design would: (1) reserve log space before doing
transactional work (InnoDB's `log_free_check` shape); (2) when the durable
frontier lags the append frontier by more than a configured bound, return a
retryable indication or block *outside* the latch; (3) expose the lag as a
statistic so an operator can see saturation. Note libdb has a precedent for a
"retry, the resource is exhausted" return in `DB_LOCK_NOTGRANTED`/
`DB_LOCK_DEADLOCK`, which applications already handle — so `DB_LOG_FULL`-style
backpressure would fit existing call sites. Detailed as **D3**.

---

## Findings: what actually limits the append path

All measurements: 96-vCPU box, production build (`--enable-shared`, no
DIAGNOSTIC), `/nvme` striped local NVMe, insert workload
(`test/bench/p5_log_bench.c`), `DB_TXN_NOSYNC`, 100-byte values, throughput in
rows/sec. Counters are read from `DB_ENV->log_stat`, i.e. from the engine, not
inferred.

### The noise floor is wide, and that shapes what can be claimed

Base against itself (same binary in both arms, arms alternating within each rep,
5 reps, ratio computed **paired within each rep**):

| t | 1 | 2 | 4 | 8 | 16 | 32 | 64 | 96 |
|---|---|---|---|---|---|---|---|---|
| max \|dev\| | 4.1% | 3.9% | 4.5% | 10.4% | 9.3% | 13.6% | **22.2%** | 10.6% |

**Floor: ±22.2%.** Much wider than the ±4.6% P4 measured on the same box,
because this workload is *bimodal* at high thread counts — at t=64 one rep
returned 148k and 115k where the other four returned ~85k. Anything below ±22%
at t=64 is not a result. Median throughput, batch=1:

| t | 1 | 2 | 4 | 8 | 16 | 32 | 64 | 96 |
|---|---|---|---|---|---|---|---|---|
| rows/s | 163,738 | 211,587 | 169,452 | 146,786 | 90,333 | 78,439 | 86,017 | 89,896 |

This reproduces the shape P5 reported: a peak at t=2 and a decline thereafter.

### Finding 1 — the critical section's *contents* are not the cost

Two independent controls, both pointing the same way.

**The profile.** `perf record --call-graph dwarf` at t=64 attributes **85.57%**
to `__db_tas_mutex_lock_int` and **4.88%** to `__db_tas_mutex_unlock` (90.45%
combined), of which `__log_put` is 52.31% — reproducing the 56%/87% split P5
reported. But `__memmove_evex_unaligned_erms`, which is the `__log_fill` copy at
`:1382`, is **1.46%** of total time. The mean log record here is **157 bytes**
(795 MB over 5.06M records), i.e. a copy of roughly 10 ns.

**The buffer-size control.** `lg_bsize` 32 KB (default) → 8 MB changes *how
often a `pwrite` happens inside the latch* by two orders of magnitude and
nothing else:

| | writes/s inside latch | t=32 rows/s | t=96 rows/s |
|---|---|---|---|
| 32 KB | 2,608 | 78,614 | 88,435 |
| 8 MB | **15** | 78,537 | 94,421 |

`wcount_fill` ≈ `wcount` in both arms (10,779 of 10,811 at 32 KB), confirming
that nearly every log write is issued by `__log_fill` **under the region latch**
rather than by the flusher. Removing **170×** of those syscalls from inside the
critical section changed throughput by **−0.1% / +6.8%**, both inside the floor.

**Consequence: shortening this critical section optimises a term worth ~1.5%,
plus a syscall whose removal buys nothing.** That is a direct argument against
D1 as a first move, and it was worth measuring before designing.

### Finding 2 — the model bounds reserve-then-copy at ~10–20%

`test/bench/p5_cslen.c` models the critical section standalone under a
BDB-shaped TAS latch (including the owner-identity write to the same cacheline
that `mut_tas.c:203-204` performs), A/Bing copy-inside (mode 0, today) against
reserve-inside-copy-outside (mode 1, the PG/InnoDB shape), 157-byte records,
each arm run twice, alternating:

| t | mode 0 acq/s | mode 1 acq/s | gain | mode 0 hold | mode 1 hold |
|---|---|---|---|---|---|
| 1 | 14.0M | 14.1M | +0.8% | 21.6 ns | 19.3 ns |
| 8 | 1.79M / 1.68M | 1.62M / 1.62M | −6% | 221–225 ns | 227–228 ns |
| 32 | 1.10M / 1.05M | 1.22M / 1.21M | +13% | 364–377 ns | 308–320 ns |
| 96 | 1.13M / 1.31M | 1.56M / 1.32M | +18% / +1% | 310–422 ns | 269–339 ns |

Two readings. First, **reserve-then-copy's ideal-case upside is 10–20%**, not a
multiple — consistent with Finding 1, since it removes a ~10 ns copy from a
~300 ns section. Second, and more informative: mean hold time grows
**21 ns → ~300 ns (14×)** as threads rise *while the work under the latch is
constant*. That inflation is cache-coherence traffic on the latch line and the
`lp->*` counters, not computation — which is both why the `memcpy` is not the
cost and why removing it does not help.

### Finding 3 — the ceiling is transactions/sec, not appends/sec

The decisive experiment. A single-key btree insert costs **3.10 log records**,
verified with `db_printlog` over a t=1 run: 2,013,278 `__db_addrem` +
1,006,640 `__txn_regop` + 50,326 `__db_pg_alloc` + 50,323 `__bam_split` for
1,006,639 rows. The two `__db_addrem` are the key and the data, each a separate
`__db_pitem` (`src/btree/bt_put.c:366`, `:478-483`), each its own `__log_put`.
So **one row enters the serialized append stage three times.**

Batching rows per transaction changes how often that stage is entered per unit
of user work. 5 reps, arms alternating within each rep, rows/s for both arms so
the unit of work is identical:

| t | batch=1 | CV% | batch=4 | CV% | 4/1 | vs floor |
|---|---|---|---|---|---|---|
| 1 | 163,738 | 3.97 | 188,363 | 4.07 | **+15.0%** | outside (4.1%) |
| 2 | 211,587 | 5.31 | 257,813 | 4.78 | **+21.9%** | outside (3.9%) |
| 4 | 169,452 | 2.08 | 235,224 | 2.51 | **+38.8%** | outside (4.5%) |
| 8 | 146,786 | 2.92 | 241,359 | 1.72 | **+64.4%** | outside (10.4%) |
| 16 | 90,333 | 4.15 | 239,005 | 1.90 | **+164.6%** | outside (9.3%) |
| 32 | 78,439 | 5.32 | 226,841 | 1.22 | **+189.2%** | outside (13.6%) |
| 64 | 86,017 | 2.76 | 213,982 | 1.42 | **+148.8%** | outside (22.2%) |
| 96 | 89,896 | 4.44 | 197,229 | 1.10 | **+119.4%** | outside (10.6%) |

Every cell from t=4 up is far outside the floor, and CV *falls* from ~4–5% to
~1–2% — the bimodality disappears. Batching also flattens the curve: batch=4
holds 197k–257k across the whole range instead of collapsing from 211k to 78k.

Now the mechanism. Records per row falls only 3.10 → 2.35 (−24%) at batch=4,
yet throughput rises up to +189%. The win is **not** proportional to the
reduction in appends. Sustained appends/sec:

| t | appends/s batch=1 | appends/s batch=4 | ratio |
|---|---|---|---|
| 16 | 280,032 | 561,662 | **2.01×** |
| 32 | 243,161 | 533,076 | **2.19×** |
| 64 | 266,653 | 502,858 | 1.89× |
| 96 | 278,678 | 463,488 | 1.66× |

**The same latch sustains 2.19× more appends per second when those appends
arrive in bursts of ~9 from one thread instead of 3 from each of many.** A latch
capacity-limited at 243k appends/s could not do 533k. So 243k is not the latch's
capacity — it is what the latch delivers when every acquisition comes from a
different core and therefore takes the latch line and the `lp->*` counters as
cold misses.

Holding the unit of work fixed while varying batch shows the real invariant:

| batch | rows/s | **txns/s** | appends/s | appends/txn |
|---|---|---|---|---|
| 1 | 91,609 | **91,609** | 283,988 | 3.10 |
| 2 | 175,494 | **87,747** | 456,284 | 5.20 |
| 4 | 229,921 | 57,480 | 540,314 | 9.40 |
| 8 | 215,330 | 26,916 | 480,186 | 17.84 |

From batch=1 to batch=2, rows/s nearly doubles while **txns/s is flat** (91.6k →
87.7k). The system is limited to **~90k transactions/sec**, largely independently
of how much work each transaction does. The log-latch wait rate corroborates: at
t=32 the region-lock wait fraction falls from **58.4%** to **27.7%** at batch=4
while appends/s doubles.

**Caveat, and then its resolution.** Batching reduces *per-transaction* costs
across the whole engine — `txn_begin`/`txn_end` (the P1 locker path), lock
acquisition, and the commit record and its flush — not only log appends. So the
batch experiment alone establishes that the **per-transaction fixed cost** is the
ceiling, but not how much of it is the log.

### Finding 4 — the log is 36–62% of removable per-transaction cost

Resolved with a control that is valid at **every** thread count.
`DB_TXN_NOT_DURABLE` on the DB handle keeps full transactions, full locking and
the same commit path, but `__log_put_record_int` takes the `is_durable == 0`
branch and queues the record on the transaction instead of appending it
(`log_put.c:2278-2302`) — so the log region latch is never taken for data
records. Verified by the harness reporting **0.00 records per row**. 5 reps,
medians:

| t | logged rows/s | log-suppressed rows/s | ratio | log's share of removable per-row cost |
|---:|---:|---:|---:|---:|
| 1 | 164,782 | 206,902 | 1.26× | 20.4% |
| 2 | 215,456 | 266,052 | 1.23× | 19.0% |
| 4 | 167,944 | 206,091 | 1.23× | 18.5% |
| 8 | 144,141 | 235,102 | 1.63× | **38.7%** |
| 16 | 88,018 | 231,106 | 2.63× | **61.9%** |
| 32 | 78,626 | 169,749 | 2.16× | **53.7%** |
| 64 | 88,362 | 138,182 | 1.56× | 36.1% |
| 96 | 83,296 | 131,170 | 1.57× | 36.5% |

**The log is the largest single component of per-transaction cost at t≥8 — 36%
to 62% — but it is not all of it.** Two further readings matter:

1. **Even with the log entirely out of the append path, throughput still
   declines** with thread count (235k at t=8 → 131k at t=96) and CV rises to
   17%. So the log is not the *only* thing that fails to scale here; removing it
   raises the ceiling without making the curve monotonic. An upper bound on what
   any P5 fix can achieve is therefore roughly the `notdur` column, and that
   column is itself falling.
2. **This bounds every design in this RFC.** A perfect fix to the log append
   path — one that made appends free — would buy at most +57% at t=96 and
   +163% at t=16. D1's measured 10–20% of the *critical section*, applied to a
   component worth 36–62% of the cost, is a few percent of throughput. D0/D2,
   which attack acquisitions per transaction, can plausibly capture a real
   fraction of it. This is the quantitative case for the ranking.

### What this means for the device ceiling

Against 494k IOPS / 1930 MiB/s measured with `fio`:

| arm | t=32 | t=96 | share of 1930 MiB/s |
|---|---|---|---|
| batch=1 | 38.2 MiB/s | 43.8 MiB/s | 2.0% / 2.3% |
| batch=4 | 103.5 MiB/s | 89.9 MiB/s | 5.4% / 4.7% |

Even the best arm uses **~5%** of the device. `avg_write_bytes` is 31,966 — the
log writes full 32 KB buffers, so at ~1,440 writes/s it is nowhere near the 494k
IOPS limit either. **No design here is device-bound; storage is not the
constraint at any measured point, which makes the backpressure work (D3) about
future-proofing rather than a current stall.**

## Design

Four designs, ranked by measured benefit against correctness risk. The ranking
is driven by §Findings, and it is **not** the ranking this RFC set out to
produce.

### D0 — Reduce log appends per transaction (recommended first)

**Mechanism.** Attack the 3.10 records per row where the engine emits more
records than the format requires:

1. **Combine the key and data `__db_addrem` records.** `__bam_iitem` calls
   `__db_pitem` twice for one logical insert (`bt_put.c:366`, `:478-483`), and
   each call logs independently (`db_dup.c:205`). One record carrying both
   items — or a vector of items for one page — halves the dominant record count.
   Both items go to the **same page** under the **same page latch** in the same
   operation, so the recovery argument is local: the redo applies both or
   neither, which is *stronger* than today, where a crash can land the key
   record without the data record and recovery depends on the transaction being
   rolled back.
2. **Avoid re-entering the latch for an adjacent commit record.** A committing
   transaction's last data record and its `__txn_regop` are usually emitted
   back-to-back by the same thread; a combined path would take the latch once.

**Critical section shrinks to:** unchanged in *duration*; what falls is
**acquisitions per transaction**, 3.10 → ~2.1 for (1) alone. Per Finding 3 that
is the quantity that matters — and unlike batching it needs no application
change.

**Recovery/durability argument.** On-disk byte order is unchanged: records are
still appended by one latch holder, densely packed, back-chained. A combined
record needs a new type plus recovery function — routine versioned work
(`DB_LOGVERSION`, the `_read`/`_recover` pair, `log_verify`) — and old logs keep
replaying through the old record's recovery function, which is how libdb has
always added record types.

**ABI/format.** **Log format version bump** (new record type): a *forward*
change with the well-trodden migration path, a new library reading old logs.
**No region-layout change, so `__env_struct_sig()` is unchanged and existing
environments still attach.** A much weaker break than D1's.

**Measurement.** The batch experiment already gives the upper bound (+119% to
+189% at t≥16 from a 24% record reduction). For D0: A/B records-per-row (the
harness prints it from `st_record`) and rows/s, 5 reps, arms alternating, against
the ±22.2% floor. **Falsifier stated in advance:** if halving `__db_addrem`
records moves throughput less than the floor at t≥16, the per-transaction cost
is dominated by `txn_begin`/`txn_end` or the commit flush rather than by appends,
and D0 should be abandoned in favour of attacking those.

**Risk.** Low-to-moderate. A new record type touches recovery — the dangerous
part of the engine — but through the mechanism designed for it. No concurrency
invariant changes.

### D1 — Reserve-then-copy (PostgreSQL/InnoDB shape) — **not recommended now**

**Mechanism.** Hold the latch only to (a) compute `hdr->prev` from `lp->len`,
(b) advance `lp->lsn` and `lp->b_off` by the record length, (c) update `lp->len`
and `lp->f_lsn`; release; then `memcpy` header and payload into the reserved
range in parallel. A completion structure tells the flusher the contiguous
filled prefix — PG's array of `insertingAt` watermarks with the flusher taking
`min()`, or InnoDB's `link_buf` walked forward from the frontier.
`__log_flush_int` must then wait for that prefix to cover `flush_lsn` before
writing, replacing today's implicit guarantee that everything below `b_off` is
present.

**Critical section shrinks to:** two counter bumps and two field writes — ~300 ns
becomes maybe ~100 ns, per the model.

**Why it is not recommended.** Three reasons, in severity order.

1. **Measured upside is 10–20%, inside or barely outside the floor at the thread
   counts that matter** (Findings 1–2). D0 and batching each showed multiples.
2. **The multi-process crash hazard is real and specific** (§Multi-process). A
   process dying between reserve and fill leaves a zero-filled hole while
   holding no lock. By `log_get.c:1238-1240` and `log.c:375-381` that hole is
   *virtual EOF*: recovery stops there and **silently discards every committed
   transaction after it.** PG and InnoDB are immune because a dead writer means
   a dead server and a full replay; libdb promises `failchk` recovery of a
   surviving environment. Mitigation requires making the reservation
   crash-visible — e.g. a length-only placeholder header written under the latch
   so a hole is *detectably* incomplete rather than indistinguishable from EOF,
   plus a `failchk` pass that can complete or invalidate an abandoned
   reservation. That is substantial new mechanism in the durability frontier for
   a 10–20% gain.
3. **The back-chain serializes reservations anyway.** `hdr->prev` needs
   `lp->len` of the immediately preceding record (`:842`, `:898`), so
   reservations cannot be computed independently as PG's and InnoDB's can — both
   formats are flat byte spaces with no back-pointer. The reservation stays
   serial; only the copy parallelises. This is why libdb gets less from the
   pattern than its two model systems.

**ABI/format.** The completion structure must live in the log region, i.e. new
fields in `struct __log`, which `env_sig.c:76` hashes via `__ADD(__log)`.
**`__env_struct_sig()` changes and every existing environment refuses to attach:
a hard format break requiring clean shutdown and re-creation.** On-disk log
bytes and their order are unchanged, so this breaks *region* compatibility, not
log compatibility.

**Measurement.** Same protocol; beyond throughput, (a) `st_scount` /
`maxcommitperflush` to prove group commit still batches, (b) a crash-mid-copy
test that kills a process between reserve and fill and asserts recovery does
**not** silently truncate, (c) `db_verify` + `db_log_verify` after every crash
test.

### D2 — Consolidated append (Aether's consolidation array)

**Mechanism.** The one design that targets Finding 3 without changing
application behaviour or the record format. A small fixed array of slots in the
log region; a thread CASes its length into a slot to join a group. One thread
per slot becomes leader, takes the region latch **once** for the group's
combined length, computes the intra-group back-chain locally (it knows every
member's length, so it can fill each `hdr->prev` correctly), releases, and
members copy into their assigned sub-ranges in parallel. This is precisely the
structure that makes the back-chain tractable: the *leader* resolves the serial
dependency for the whole group.

**Critical section shrinks to:** one acquisition per *group* rather than per
record — the same 2.19× effect the batch experiment produced, achieved inside
the engine instead of by asking the application to batch.

**Recovery/durability argument.** Byte order unchanged; back-chain correct by
construction. The crash hazard is D1's — a member dying before filling leaves a
hole — with the same required mitigation, but concentrable: the leader can copy
on behalf of members that have not yet done so (members publish a pointer,
leader completes), bounding exposure to the leader's own death and making the
hazard equivalent to today's "holder dies with the latch".

**ABI/format.** New region fields (the slot array) → **`__env_struct_sig()`
changes, hard format break.** No log-format change.

**Measurement.** Same protocol; the specific metric is appends/s at fixed
rows/s, which should approach the batch=4 arm's 533k at t=32.

**Risk.** Higher than D0 (new concurrency protocol in the durability path),
better than D1 per unit of benefit because the benefit is a multiple rather than
10–20%. Pursue **if and only if** D0's measured gain is insufficient.

### D3 — Backpressure at the API boundary

**Mechanism.** libdb has none today, and per §Findings the device sits at ~5%,
so this is not currently a throughput lever — it is a **robustness** gap that
any of D0–D2 makes more pressing by raising append rates. Following InnoDB's
`log_free_check()`: (1) track the lag between the append frontier (`lp->lsn`)
and the durable frontier (`lp->s_lsn`); (2) at `DB_TXN->commit()` — or better,
before a transaction does work, which is InnoDB's placement — if the lag exceeds
a configured bound, block *outside* the region latch or return a retryable
error; (3) expose the lag via `DB_ENV->log_stat` so saturation is observable.
`DB_LOCK_NOTGRANTED` is the precedent for a retryable "resource exhausted"
return that applications already handle.

**Critical section:** unchanged.

**ABI/format.** A new `DB_LOG_STAT` field would change `__ADD(__db_log_stat)`
(`env_sig.c:57`) → format break — **unless** the lag is derived from existing
fields (`st_cur_file`/`st_cur_offset` vs `st_disk_file`/`st_disk_offset`), which
it can be, so **a read-only implementation needs no break at all.** The
blocking/erroring behaviour needs a new flag, which is additive.

**Measurement.** Not a throughput experiment. Throttle the device (cgroup
`io.max`), confirm that without D3 latency grows without bound while the log
buffer queues, and that with D3 the lag stays bounded and the API reports
saturation.

### Rejected outright

- **Partitioned/sharded WAL with per-partition order** (Kafka/Redpanda/Silo/
  Taurus shape). Physiological redo's page-LSN precondition (`CHECK_LSN`,
  `src/dbinc/log.h:411`) makes a cross-log merge unsound, and the format carries
  no dependency vector. §Prior art has the full argument. Not a patch to libdb;
  a different engine.
- **Removing the `hdr->prev` back-chain** to make reservations independent. It
  would make D1 behave as it does in PG, but `DB_PREV` log cursors
  (`log_get.c:489`, `:775`), partial-record reassembly (`:990-1001`) and
  `db_log_verify` all consume it. A log-format break of the most invasive kind
  for a 10–20% gain.

## Alternatives considered

See §Design's four options and §Rejected outright. The alternative to all of
them is **do nothing to the log and attack `txn_begin`/`txn_end` instead**,
which Finding 3 makes a serious contender: if the ceiling is ~90k
transactions/sec and the log is roughly half the per-transaction cost, the other
half is in the transaction and lock subsystems, where P1 already found and fixed
one convoy. Establishing that split is the cheapest next experiment and needs no
design at all.

## Risks & open questions

1. ~~**The batch result may overstate the log's share.**~~ **Resolved** by
   Finding 4: a `DB_TXN_NOT_DURABLE` control, valid at all thread counts, puts
   the log at 36–62% of removable per-transaction cost at t≥8. The residual
   open question is the *other* 38–64%, and the fact that the log-suppressed arm
   still declines from 235k at t=8 to 131k at t=96 — i.e. there is a second,
   non-log scaling defect behind P5 which this RFC does not identify.
2. **D0's record-combining may be blocked by an access-method detail I have not
   verified.** `__db_pitem` is called from Btree, Hash, Recno and recovery; the
   two calls in `__bam_iitem` are the common case, not the only one. **Open:**
   whether every two-`pitem` site shares a page and an operation.
3. **The 14× hold-time inflation with thread count is unexplained in detail.**
   Consistent with coherence traffic on the latch line, but I have not measured
   cache misses per acquisition. If part of it is the two owner-identity stores
   (`mut_tas.c:203-204`), a cheaper acquisition would help *every* latch in the
   engine, not just the log's — a much larger prize than P5. **Open, and the
   highest-value follow-up here.**
4. **`tas_spins` is not the lever it first appeared to be.** An early single-rep
   probe suggested large gains at low spin counts; repeated properly, the
   default (4800) was *best* at t=8 (145k vs 110k at spins=1). Recorded so it is
   not re-tried.
5. **Group-commit properties under D1/D2** need explicit proof, not inspection:
   the leader/follower protocol's correctness rests on "everything below `b_off`
   is filled", which both designs break.

## Prototype / evidence

No library change is proposed, so there is no prototype of a fix. What exists:

- `test/bench/p5_log_bench.c` — the workload, with levers for `lg_bsize`,
  durability mode, `tas_spins`, rows-per-transaction, and a no-log control.
  Reads the engine's own log counters.
- `test/bench/p5_cslen.c` — standalone model of the critical section; A/Bs
  copy-inside against reserve-then-copy to bound D1's benefit at **10–20%**
  without touching the library.
- `test/bench/p5_ab.sh`, `p5_batch_ab.sh`, `p5_sweep.sh`, `p5_report.py` — A/B
  drivers (arms alternate within each rep) and the median/CV reporter.
- `test/bench/P5-LOG-APPEND-2026-09.md` — the full tables.

Two harness defects of mine are recorded in the commit history because each
briefly looked like a library bug: a missing `set_lk_detect` (batched
transactions really can deadlock, and without a detector they block forever),
and hangs at `tas_spins=1` that were my own overlapping 96-thread sweeps
oversubscribing the box — 0 of 6 runs hang on an idle machine.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Pending — Draft. The authors' recommendation is **reject D1
  (reserve-then-copy) as the first move, and do not implement it on the current
  evidence.** It was the hypothesis this RFC was opened to pursue; the
  measurements do not support it. Pursue **D0** first — the only option with a
  measured multiple behind it, no region-format break, and no new concurrency
  protocol in the durability path.
- **Rationale:** the critical section's contents are not the bottleneck
  (memmove 1.46%; removing 170× of the in-latch syscalls changed nothing), the
  ideal-case model bounds reserve-then-copy at 10–20%, and the same latch
  demonstrably sustains 2.19× more appends when entered in bursts — so the cost
  is entry into the serialized stage, not the stage itself. Against that, D1
  requires a region-format break *and* converts a detectable
  process-death-under-latch into silent log truncation, in the multi-process
  configuration that distinguishes libdb from both of its model systems.
- **Conditions / follow-ups:** (a) settle Risk 1 with a valid high-thread
  no-logging control before attributing the whole per-transaction ceiling to the
  log; (b) investigate Risk 3 — the 14× hold-time inflation — since a cheaper
  mutex acquisition would benefit every subsystem; (c) treat D3 (backpressure)
  as independent of P5 and worth doing on its own merits, noting it can be
  implemented read-only with **no** format break; (d) if D0's gain proves
  insufficient, D2 (consolidation array) is the next design, not D1, because it
  targets acquisitions-per-transaction rather than hold time and resolves the
  back-chain dependency at the group leader.


# Write-path commit throughput: where the cost actually is (2026-09)

Corrects a claim this repository published about its own write path, and
replaces it with a measurement.

## The claim that was wrong

`CROSS-ENGINE-2026-09.md` attributed libdb's 20–48× write-side gap versus
WiredTiger to "**fsync-per-commit and no group commit**". That is inaccurate.

**Berkeley DB has had leader/follower group commit since the Sleepycat code.**
It lives in `__log_flush_int` (`src/log/log_put.c`): a committer that arrives
while a flush is in progress enqueues a `struct __db_commit` on `lp->commits`
and blocks on its own `DB_MUTEX_SELF_BLOCK` `mtx_txnwait`; the flushing leader,
after its `fsync`, walks that queue and wakes each waiter whose commit LSN the
completed flush actually made durable — the coverage test is
`LOG_COMPARE(&lp->s_lsn, &commit->lsn) > 0`. Waiters the flush did *not* cover
receive the `DB_COMMIT_FLUSH` baton and flush for themselves on waking. The
durability contract was already correct, and the batching already works.

The claim originated from grepping the source for the phrase "group commit"
rather than for the mechanism. A feature's absence cannot be established by
searching for its name.

## What the batching actually achieves

96-vCPU `c7i.24xlarge`, Debian 13, stock `a7857c847`, `DB_TXN_SYNC` (full
durability, no `NOSYNC` escape), 8-second runs; fsyncs counted via the log
region's own `st_scount`, so "flush/commit" is measured, not inferred.

| threads | ops/s | flush/commit | max commits per fsync | p50 | p99 |
|--------:|------:|-------------:|----------------------:|----:|----:|
| 1 | 828 | 1.0000 | 1 | 907 µs | 4.0 ms |
| 8 | 2,284 | 0.2621 | 7 | 3.7 ms | 8.2 ms |
| 32 | 4,097 | 0.1192 | 28 | 3.8 ms | 68.8 ms |
| 96 | 3,623 | 0.1369 | 31 | 3.8 ms | 272.6 ms |

Fsyncs per commit fall from 1.0 to **0.12** — roughly 8× coalescing, with up to
31 commits retired per flush. Stack sampling under load shows the same thing
structurally: **380 threads parked in the follower wait (`condwait`/futex)
against exactly one thread inside `__os_fsync`.** The batching lever is already
pulled.

## Where the cost really is

> **Superseded in part, 2026-09-16.** The three items below correctly identify
> that flush coalescing is not the limiter, and that conclusion stands. But the
> *diagnosis* in items 2 and 3 is wrong, and both errors trace to this file's
> own instrument rather than to libdb. `test/bench/WRITE-TAIL-2026-09.md`
> measures the handoff directly and finds: the waiter queue **is** fair
> (rounds-waited p50 = p99 = p99.9 = 2, so no waiter is overtaken repeatedly);
> software handoff is **13 µs** per round, not 700 µs (item 2's subtraction
> differences two incomparable fsyncs and is not even sign-correct on that
> hardware); and the 272 ms tail is in `db->put`, not the commit — the commit
> phase's own p99 is 5.7 ms against a 3.7 ms p50. `commit_bench` had timed
> begin+put+commit as one interval and never warmed up, so the numbers below
> describe page-split lock convoying during an insert ramp. Warmed, the same
> library does 16,244 ops/s at 96 threads with a 7.5 ms p99. Read that file
> before acting on this section.

Three things in the same measurement identify the actual limiter, and none of
them is the fsync count:

1. **Throughput ceilings and then regresses** — ~4,100 ops/s at 32 threads,
   falling to 3,623 at 96.
2. **Software handoff dominates each round.** The system completes only ~507
   fsync *rounds*/second, i.e. ~2.0 ms per round, against a device `fsync`
   measured at ~1.2 ms single-threaded (`fsync_probe`). That leaves **~0.7 ms
   per round of pure software handoff** — the wake chain, the region-lock hold,
   and the baton transfer — not device time.
3. **Latency fairness collapses.** p50 stays flat at ~3.8 ms while p99 rises to
   **272 ms at 96 threads, a 68× spread.** The protocol hands the leader baton
   to a freshly-woken follower between every round and the wait queue has no
   fairness property, so a subset of threads starves.

So the write-path target is **round-to-round leader-handoff latency and
wait-queue fairness**, not flush coalescing. That is a different piece of work
from the one the gap was previously attributed to, and it is a tail-latency
problem at least as much as a throughput one.

> **Superseded, 2026-09-16.** Measured directly, neither handoff latency nor
> wait-queue fairness is a defect: the queue is fair and the handoff is 0.65% of
> a round. The target named here does not exist. The real one, which the convoy
> in these unwarmed numbers was concealing, is that `db->put` holds page locks
> across the durable commit — a B-tree lock-scope question. See
> `WRITE-TAIL-2026-09.md`.

## What this does and does not license

- It does **not** mean libdb's write path is competitive: the 20–48× gap in
  `CROSS-ENGINE-2026-09.md` stands as measured. Only its *explanation* changes.
- It does **not** identify a durability defect. The follower coverage test was
  already correct. A defensive re-verification of the follower's own LSN was
  added on this branch (`ALREADY_FLUSHED` before returning success); it is
  belt-and-braces, **not** a bug fix, and should not be described as one.
- The instruments are committed so the numbers are reproducible:
  `test/bench/fsync_probe.c` (device `fdatasync` latency and the maximum serial
  fsync rate, to separate device time from software handoff) and
  `test/bench/commit_bench.c` (durable-commit throughput, flush/commit ratio,
  and the latency distribution). `test/sim/test_sim_group_commit.c` plus
  `test/sim/gc-durability-gate.sh` gate the follower path against crash, with a
  control.

## Method notes

Fresh environment per run; fsync counts read from the log region rather than
counted at the syscall boundary; `DB_TXN_SYNC` explicitly set so no run silently
benefited from `NOSYNC`. The p99 figures are the important ones and are the
least stable across reps — treat the tail numbers as order-of-magnitude, the
flush/commit ratio as solid.

> **Superseded, 2026-09-16.** "Fresh environment per run" is precisely the
> defect: fresh meant *empty*, so every run measured a load ramp rather than
> steady state, and the tail numbers describe page splits. The tail figures also
> turned out to be highly *stable* (p99 CV 2.9% over 5 reps) — they were
> reproducible measurements of the wrong thing, which is more dangerous than
> noisy ones. `commit_bench` now prepopulates by default and reports
> begin/put/commit separately.

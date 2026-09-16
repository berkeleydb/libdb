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

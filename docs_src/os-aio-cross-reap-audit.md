# os_aio cross-reap audit (branch `perf/os-aio-audit`, base `53a8fc45a`)

## The question

os_aio does async mpool DATA-PAGE writeback (never the log), enabled only by
`DB_ENV->set_flags(DB_MPOOL_AIO)`.  A prior audit fixed a real durability hole
(`__memp_aio_drain` discarded write-completion status) and left one documented
residual as the reason aio stays default-off:

> Can two concurrent `__memp_sync_int` callers (checkpoint / trickle / DB->sync
> / eviction) sharing the single per-process `aio_ctx` mis-attribute completions
> — i.e. can caller A's drain reap caller B's completions ("cross-reap"), so A
> believes writes are durable that are not?

## VERDICT: cross-reap is POSSIBLE

Nothing in the code prevents it.  Five facts, each with file:function evidence.

### 1. There is exactly one aio context per process, and it is unowned

`src/mp/mp_region.c:__memp_open` (~182) creates one context and hangs it on the
process-local `DB_MPOOL`:

```c
if (F_ISSET(env->dbenv, DB_ENV_MPOOL_AIO))
        (void)__os_aio_create(env, 0, &dbmp->aio_ctx);
```

`src/dbinc/mp.h:struct __db_mpool` has a `mutex` field described as "Thread
mutex" (allocated `MTX_MPOOL_HANDLE`, protecting `dbmfq`/`dbregq` only, taken
and released around each list walk) and then `aio_ctx` with no lock and no
ownership field.  Every thread in the process that calls `__memp_sync_int`
uses the same `dbmp->aio_ctx`.

### 2. Reaping is by COUNT off a SHARED queue, with no per-op ownership

`src/mp/mp_bh.c:__memp_aio_drain` (~703, master):

```c
for (got = 0; got < n; )
        got += __os_aio_reap(env, aioc, -1, 1);
for (j = 0; j < n; j++) {
        if ((t_ret = __memp_aio_writeback_finish(dbmp, &w[j])) != 0 && ...
```

The loop condition is "have I *counted* n completions", not "have MY n ops
completed".  It then unconditionally finishes all `n` of its own slots.

`__os_aio_reap` is the shared-queue drain in every backend:

- io_uring (`src/os/os_aio_uring.c:__aio_uring_reap`): `io_uring_wait_cqe` /
  `io_uring_peek_cqe` on one ring per context, returning `got` = however many
  CQEs were ready.  Nothing filters by submitter.  `ctx->inflight` is a plain
  `u_int32_t` (`src/dbinc/os_aio.h:struct __db_aio_context`), incremented in
  `__aio_uring_submit` and decremented in reap — a non-atomic counter mutated
  by multiple threads.
- thread pool (`src/os/os_aio_pool.c:__aio_pool_reap`): pops
  `st->cmp_head` — a *per-context* completion FIFO, i.e. shared by all threads
  using that context, not per-caller.
- POSIX aio (`src/os/os_aio_posix.c:__aio_posix_reap`): scans `st->ops[0..depth]`,
  the whole per-context slot table, finishing every slot it finds no longer
  `EINPROGRESS`.  It finishes *other callers'* slots too — including calling
  their `done` callbacks — and counts them toward `got`.

So a drain returns the count of *whatever was ready*.  If thread B has ops in
flight, thread A's `got` can reach `n` from B's completions while some of A's
own ops are still outstanding.

### 3. Completions carry no owner, and the cookie is a STACK address

`src/mp/mp_bh.c:__memp_bhwrite_async` sets `op.cookie = w` where `w` is
`&aiow[nflight]` — `aiow` is `MEMP_AIO_W aiow[MEMP_AIO_WINDOW]`, a local array
in the `__memp_sync_int` stack frame (`src/mp/mp_sync.c:~311`).  The completion
`__memp_aio_writeback_done` just writes `w->io_ret` and `w->done = 1`.  Neither
the op record (`AIO_URING_OP`, `AIO_WORK`, `AIO_POSIX_OP`) nor the completion
path records which sync call submitted it.  There is no tag to filter on.

### 4. Nothing serializes the callers

`__memp_sync_int` is reached from ten call sites; at least four can run
concurrently in one process:

| caller | flags | site |
|---|---|---|
| `txn_checkpoint` | `DB_SYNC_CHECKPOINT` | `src/txn/txn_chkpt.c:278` |
| `memp_trickle` | `DB_SYNC_TRICKLE` | `src/mp/mp_trickle.c:105` |
| `memp_sync` / `DB->sync` | `DB_SYNC_CACHE` / `DB_SYNC_FILE` | `src/mp/mp_sync.c:178,244,278` |
| buffer allocator, desperate | `DB_SYNC_ALLOC` | `src/mp/mp_alloc.c:264` |

`__txn_checkpoint` holds `region->mtx_ckp` (`src/txn/txn_chkpt.c:159`), which
serializes *checkpoints against each other* — it says nothing about trickle,
`DB->sync`, or the allocator.  `__memp_sync_int` itself takes
`MPOOL_SYSTEM_LOCK` only briefly to read `mp_maxopenfd`, and thereafter takes
`hp->mtx_hash` / `bhp->mtx_buf` / `dbmp->mutex` per page and releases them —
by design, so syncs *can* overlap.  `DB_SYNC_ALLOC` in particular is issued
from inside `__memp_alloc` by any thread that cannot find an eviction victim,
so it cannot be assumed rare or single-threaded.

### 5. Consequence

`__memp_aio_writeback_finish` → `__memp_pgwrite_finish(&w->ctx, 1, w->io_ret)`
with `w->io_ret` still 0 (never set, because that op never completed) →
BH_DIRTY cleared, `mfp->writers` decremented, the pgout page copy
(`w->ctx.buf`) freed, then the buffer unpinned (`atomic_dec(&bhp->ref)`,
`MUTEX_UNLOCK(bhp->mtx_buf)`) — all while the device write against that very
buffer is still outstanding.  Two distinct failures:

- **False durable frontier.**  `ret` stays 0, so `required_write`'s
  `__os_fsync` runs and `__txn_checkpoint` writes its checkpoint record.  The
  page is clean in cache and the log has been trimmed past it.  A crash loses
  it.  This is precisely the bug class the async error propagation was added to
  prevent — the earlier fix closed the "write failed and we ignored it" hole;
  this is the "write hasn't happened yet and we said it did" hole.
- **Write-after-free / live-buffer race.**  `w->ctx.buf` is freed while the
  kernel (or a pool worker) still reads it, and BH_DIRTY/refcount are mutated
  on a buffer another thread can now claim.

Also worth noting independently: caller A can *return from `__memp_sync_int`*
with its own ops still in flight, leaving the backend holding `op.cookie`
pointers into a dead stack frame.  The completion then writes `w->done` through
freed stack.

## Reproduction status (honest)

Not reproduced by a run.  A cross-reap needs two syncs overlapping on the same
context with real device latency; it is a race whose window is exactly the
in-flight period, and the observable damage (a lost page) needs a crash to
surface.  The verdict rests on the code, which is unambiguous: a drain-by-count
against a shared, untagged completion queue with no serialization *is* a
cross-reap.  I did not have budget to build a probabilistic crash harness, and
say so rather than claim a reproduction I do not have.

## The fix

Per the brief: do NOT flip the default; implement the smallest provably-correct
fix.  That is an exclusive-use latch, `DB_MPOOL.mtx_aio`
(`MTX_MPOOL_AIO`, `DB_MUTEX_PROCESS_ONLY` — the context is process-local, so
the latch must be too).  `__memp_sync_int` takes it with `MUTEX_TRYLOCK` at
entry and releases it on every exit path; a caller that does not win it sets
`use_aio = 0` and writes synchronously.

Why this is minimal and correct:

- **Correct**: the winner is the context's only submitter from its first submit
  to its final drain.  Every reason cross-reap was possible collapses — the
  completion queue has one consumer, `ctx->inflight` has one mutator, the
  backend submission queues (an io_uring SQE ring has no internal locking; the
  POSIX slot table is scanned unlocked) have one user, and no other thread's
  ops can be in flight to be counted.
- **Minimal**: one `db_mutex_t`, one trylock, one unlock per exit.  No per-op
  owner tags, no change to the backends, no change to the op records, no new
  region layout, no ABI change (`DB_MPOOL` is process-private).
- **Deadlock-free by construction**: it is *never* waited on.  It is held
  across page writes that take `hp->mtx_hash`, `bhp->mtx_buf` and
  `dbmp->mutex`, but since no thread ever blocks acquiring it, it can never be
  the blocking edge of a cycle, and it introduces no lock-ordering rule.
- **Degrades to the reference path**: the loser writes synchronously, which is
  what every build does today with aio off.  Contention costs throughput, never
  correctness.
- **Belt-and-braces**: `__memp_aio_drain` now checks `w[j].done` per slot before
  finishing it.  Under the latch this is always true (`DB_ASSERT`); if it ever
  is not, the slot is reported `EIO` so BH_DIRTY is *left set* and the caller's
  checkpoint fails — the safe direction.  The reap loop also breaks when a reap
  returns 0 with `inflight == 0` instead of spinning forever on a lost
  completion.
- `__memp_sync_int` asserts `nflight == 0` at its single exit label, so a
  future early-return that forgets a drain is caught rather than leaving ops
  pointing at a dead frame.

Per-op owner tagging would allow concurrent syncs to share the context, and is
the better long-term answer; it is strictly larger (a tag in `DB_AIO_OP`, an
owner field in every backend's op record, and an owner-filtered reap in all
five backends) and is not needed to close the residual.

## Default recommendation: leave `DB_MPOOL_AIO` OFF

Off, and off for two independent reasons:

1. **Unmeasured.**  No A/B was run (explicitly out of scope here, and the
   attempt that tried it is what burned 17 hours).  A default must not change
   on an unmeasured benefit.
2. **The residual is closed, not the whole question.**  With the latch, aio is
   *safe*; but it also means concurrent syncs now silently fall back to
   synchronous writeback, so the performance profile under real concurrency is
   exactly the thing nobody has measured.  Flipping the default would ship an
   unmeasured change to the durability-critical path.

The honest label is **safe but unmeasured**.  The next step, if aio is to be
default-on, is a measured A/B of checkpoint latency with the latch in place
(and a count of how often the trylock fails, which is the real question the
latch raises).

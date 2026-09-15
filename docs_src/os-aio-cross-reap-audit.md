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

## Reproduction: cross-reap OBSERVED, not just argued

The verdict was reached by code reading, then confirmed by a run.
`test/c/aio_concurrent_sync` drives all four sync callers plus eviction
pressure against one environment simultaneously.  With the latch **neutered**
(`mtx_aio` deliberately not allocated, so it stays `MUTEX_INVALID` and
`MUTEX_TRYLOCK` returns 0 for every caller — exactly the pre-fix unserialized
behaviour), on a `--enable-diagnostic` build:

```
######## TEETH: aio mode with latch NEUTERED ########
aio_concurrent_sync: mode=aio (ASYNC writeback), 8.0s, 6 writers + ckp + trickle + memp_sync + db->sync
aio_conc: BDB0059 assert failure: ../../aio2_wt/src/mp/mp_bh.c/738: "w[j].done"
aio_conc: ...libdb-2026.0.so(__memp_aio_drain+0x17b)
aio_conc: ...libdb-2026.0.so(__memp_sync_int+0xe73)
aio_conc: ...libdb-2026.0.so(__memp_sync+0x153)
aio_conc: ...libdb-2026.0.so(__memp_sync_pp+0x158)
Aborted
```

That is the cross-reap caught in the act: a `memp_sync` caller's drain counted
its `n` completions and reached a slot whose *own* completion callback had
never run.  Pre-fix, that slot would have been finished with `io_ret == 0` —
BH_DIRTY cleared and the page declared durable while its write was still in
flight.  With the latch restored, the same binary and workload are clean
(below).

What is *not* reproduced: the end-to-end lost record after a power cut.  That
needs a crash harness, which was out of scope.  The assert is the tighter
signal anyway — it fires on the mis-attribution itself rather than on one of
its downstream consequences.

## Test results (real output)

`test/c/aio_concurrent_sync [aio|sync] <seconds>`: 6 writers doing
`DB_TXN_SYNC` commits over a 60000-key space in a deliberately small 2 MB
cache (so `__memp_alloc` reaches its aggressive `DB_SYNC_ALLOC` sync — the
fourth concurrent caller, which an application cannot invoke directly), with
checkpoint, trickle, `memp_sync` and `DB->sync` threads all running.  Every
committed key is then audited through a fresh environment, and `db_verify` is
run.

With the fix in place, 8s per mode:

```
######## sync (reference, aio OFF) ########
drivers: commits=2805 ckp=28(err 0) trickle=1602(err 0) memp_sync=32(err 0) db_sync=28(err 0)
audit: lost=0 stale-but-present=0
sync: PASS (0 failures)
######## aio (DB_MPOOL_AIO on, latch under test) ########
drivers: commits=1282 ckp=347(err 0) trickle=4285(err 0) memp_sync=352(err 0) db_sync=173(err 0)
audit: lost=0 stale-but-present=0
aio: PASS (0 failures)
```

Five `aio`-mode runs and four `sync`-mode runs, all `lost=0`, all
`stale-but-present=0`, `db_verify` clean, no sync/checkpoint call returning an
error.  Not a performance measurement, and not offered as one.

### Three harness bugs found and fixed on the way (all mode-independent)

The test as inherited from the failed attempt could not have distinguished
anything; each of these reproduced *identically with aio OFF*, which is how I
know they were harness bugs and not findings:

1. **No deadlock detector.**  All six writers blocked forever in
   `__lock_get_internal` and the test hung (this is what a 17-hour run looks
   like).  The writers' key ranges are disjoint by *value* (stride `NWRITER`)
   but not by *page* — adjacent keys share a btree leaf, so they genuinely
   ww-conflict.  Fix: `set_lk_detect(DB_LOCK_DEFAULT)`; the writer loop already
   retried `DB_LOCK_DEADLOCK`.
2. **Ledger written before commit.**  Keys were recorded "provisionally" at
   `put` time and never rolled back, so once aborts existed, every aborted
   txn's keys were reported LOST — 8840 phantom losses in `sync` mode, 10595 in
   `aio` mode.  Fix: buffer the txn's writes and publish to the ledger only
   after `commit` returns 0.
3. **`DB->verify` on an open handle** → `BDB1565 method not permitted after
   handle's open method`.  Fix: verify through a fresh handle.

## pkg-config workaround

`pkg-config` is absent on this box, so configure's liburing probe leaves
`EXTRALIBS` empty and `-luring` never reaches the link line, leaving
`io_uring_*` undefined in `libdb`.  Workaround: pass `LIBS=-luring` to
configure, which puts it in `LIBS` for the library *and* every test binary:

```
../../aio2_wt/dist/configure --enable-diagnostic --with-mutex=POSIX/pthreads LIBS=-luring
```

Verified: `objdump -p libdb-2026.0.so | grep NEEDED` → `liburing.so.2`, and
`HAVE_IO_URING 1` in `db_config.h`, so the io_uring backend is the one actually
exercised above (not the synchronous fallback).  This is a harness gap on this
box, not a code bug — no build file was changed for it.

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

## Default recommendation: leave `DB_MPOOL_AIO` OFF (off-until-measured)

**Off**, and off for two independent reasons:

1. **Unmeasured.**  No A/B was run (explicitly out of scope, and it is what
   burned the previous attempt).  A default must not change on an unmeasured
   benefit.
2. **The residual is closed, not the whole question.**  With the latch, aio is
   *safe*; but the latch also means concurrent syncs silently fall back to
   synchronous writeback, so the performance profile under real concurrency is
   exactly the thing nobody has measured.  The test's own driver counts hint
   that this matters: in `aio` mode the sync callers complete far more
   iterations (trickle 4285 vs 1602, ckp 347 vs 28) while writers commit *less*
   (1282 vs 2805).  That is a suggestive shape, not a measurement — different
   dirty-page volumes, one run each — and it is precisely the wrong basis for a
   default change in either direction.

The honest label is **safe but unmeasured**.  The next step, if aio is to be
default-on, is a measured A/B of checkpoint latency with the latch in place,
plus a counter for how often the trylock is *lost* — that number is the real
question the latch raises, and it is currently unknown.

## What I did not do

- No aio-ON vs aio-OFF equivalence sweep across all backends (only the
  io_uring backend was exercised; the thread-pool and POSIX backends are
  covered by the code argument, not by a run).
- No multi-rep performance A/B.
- No crash/recovery harness for the end-to-end lost-record consequence.
- No change to `dist/RELEASE`, version files, or `.agent*/`.
- The failed attempt's tree also contained an out-of-scope `DB_MPOOL_NO_AIO`
  default-flip (`dist/api_flags`, `src/dbinc/db.in`, `env_method.c`,
  `env_config.c`, regenerated `build_windows/db.h` + `build_android/db.h`).  I
  extracted only the latch and dropped the flip, so `DB_MPOOL_AIO` keeps its
  existing opt-in semantics and there is no header regeneration or flag-space
  change to qualify.

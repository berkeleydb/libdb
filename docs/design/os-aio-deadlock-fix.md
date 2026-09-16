# os_aio deadlock (issue S1): diagnosis and fix

Status: **fixed**. `DB_MPOOL_AIO` remains default-OFF; this removes the liveness
defect that blocked ever turning it on.

- Branch: `fix/os-aio-deadlock`
- Fix: `src/mp/mp_sync.c` (`__memp_sync_int`), 1 file, function bodies only
- Reproducer: `test/c/aio_concurrent_sync` (already existed and is
  manifest-declared) plus a run harness, `test/c/aio-stall-run.sh`
- Measurement box: dedicated `c7i.24xlarge`, 96 vCPU, Debian 6.1.0-53-cloud-amd64,
  `--enable-debug --enable-diagnostic`, `HAVE_IO_URING 1`, liburing backend live

## 1. Measured baseline hang rate, with interval

A "hang" is scored operationally, not by feel: the driver is given `secs=8` of
workload and then a **90 s** deadline. Its own audit takes about a second, so a
process still alive at 90 s is stalled permanently, not slow. Exit 124 / still
alive is the signal; every other exit code is recorded separately so a crash can
never be miscounted as a stall.

| arm | binary | hangs / runs | rate | 95% CI (Wilson) |
|---|---|---|---|---|
| baseline batch 1 | master, `MEMP_AIO_WINDOW=16` | 5 / 96 | 5.2% | 2.2% – 11.6% |
| baseline batch 2 | master, `MEMP_AIO_WINDOW=16` | 6 / 96 | 6.2% | 2.9% – 13.0% |
| **baseline pooled** | master, `MEMP_AIO_WINDOW=16` | **11 / 192** | **5.7%** | **3.2% – 10.0%** |
| baseline sharpened | master, `MEMP_AIO_WINDOW=64` | 18 / 96 | 18.8% | 12.2% – 27.7% |
| sync-mode control | master, sync writeback | 0 / 96 | 0% | — |

The pooled 5.7% reproduces the previously reported 2/40 and 3/67 exactly, so the
box is measuring the same defect and not a local artifact.

**Sharpening note, and a wrong first guess worth recording.** The obvious sharpener
looked like *shrinking* `MEMP_AIO_WINDOW`. That is backwards: `WIN=2` drains almost
every iteration, so it *suppresses* the bug (1/96, below the shipped rate). The
precondition is a window that is **partly full for a long time**, so the sharpener
is a *larger* window. `WIN=64` more than triples the rate to 18.8%, and that arm is
the strongest discriminator available.

## 2. The wait chain from gdb — the recorded diagnosis was half right

Every stall was captured live with `thread apply all bt`, `bt full`, and a
purpose-written gdb script that walks the wait graph, reads each blocked thread's
mutex index out of the `__db_tas_mutex_*` frame, and for the aio winner dumps
`use_aio`, `nflight`, and the `mtx_buf` index of every pin it still holds.

The shared root cause is exactly as documented: a deferred async write holds its
buffer's pin (`ref` + shared `mtx_buf`) until the write is reaped, and the window
was reaped only at `nflight >= MEMP_AIO_WINDOW`, so the deferred path **waits while
holding pins**.

But there are **two** stall variants, not one, and the more common one is not the
one that was written down. Classified across all 29 captures (11 at the shipped
window, 18 sharpened) by locating the thread whose `__memp_sync_int` frame has
`use_aio == 1` and reading its stall line:

| variant | winner's state | count (shipped window) | count (sharpened) |
|---|---|---|---|
| A — hold-and-**block** | blocked at `mp_sync.c:602`, `MUTEX_READLOCK(bhp->mtx_buf)` | 6 / 11 | 6 / 18 |
| B — hold-and-**spin** | RUNNABLE at `mp_sync.c:559`, retry-loop `__os_yield` | 5 / 11 | 12 / 18 |

### Variant A — hold-and-block (the documented cycle, confirmed)

```
T9  trickle   __memp_sync_int mp_sync.c:602   use_aio=1  nflight=1..5
      MUTEX_READLOCK(bhp->mtx_buf)  <- blocks for a NEW buffer
      ... while holding the pins of every deferred write in aiow[]
T2  writer    __memp_fget mp_fget.c:402 (MUTEX_LOCK, exclusive)
      <- __db_new <- __bam_page <- __bam_split
      wants one of T9's pinned buffers EXCLUSIVE
T3..T7 writers __lock_get_internal lock.c:1561
      piled up behind T2, which holds the PGNO_BASE_MD write lock
```

Confirmed down to the mutex state. In one capture the contended `mtx_buf` is
index 1328 with `sharecount = 1, alloc_id = 15 (MTX_MPOOL_BH), wait = 1` — one
shared holder (the aio winner's pin) and a waiter (the writer). The other
mutexes in the graph are `alloc_id = 9 (MTX_LOGICAL_LOCK)`, `tas = 1,
sharecount = 0`: the writers queued on the lock manager. Nothing waits on
`mtx_aio`, confirming the TRYLOCK-only latch is not an edge.

### Variant B — hold-and-spin (missed by the original diagnosis)

```
T9  trickle   __memp_sync_int mp_sync.c:559   use_aio=1  nflight=4..61
      __os_yield in the retry loop  <- RUNNABLE, no mutex wait at all
      every remaining tracked buffer is BH_EXCLUSIVE, so the loop
      never decrements `remaining` (required_write is set), and it
      still holds every deferred pin while it spins
T5  writer    __memp_fget (exclusive)  waiting on one of T9's pinned buffers
T2,T3,T4,T6,T7 writers  __lock_get_internal, behind that writer
```

This is why the original write-up saw only one variant: **the winner has no
mutex wait in its own backtrace.** It is running. `bt` shows it in `__os_yield`,
which reads like ordinary throttling, and the summary line "the other sync
callers merely spin in the required_write retry" describes the victims — but here
the *winner* is the spinner, and `nflight` is 4 to 61 rather than the recorded 5.
The cycle closes through pins held by a runnable thread, which no mutex-wait
graph can see.

The two variants are one defect (pins held across a wait the pin holder cannot
end) with two waits, so a fix at one wait leaves the other. That is decisive for
the choice below.

**Why the lock-order checker cannot see either.** As already recorded, `mtx_buf`
is a page *pin*, not an ordered latch — `__memp_fget` returns holding it — so
`src/mutex/mut_order.c` does not model it. Variant B is further out of reach on
principle: there is no acquisition to check, because the stalled thread is not
acquiring anything. Confirmed, not worked around; the reproducer is the gate.

## 3. Fix chosen, and why

The task named two candidates: (a) drain before blocking on a new buffer, or
(b) `MUTEX_TRY_READLOCK` and defer. The backtraces say **both, and neither
alone**, because there are two waits:

- Candidate (b) alone fixes variant A only. Variant B never reaches the
  readlock — it loops over buffers it rejects at the `BH_EXCLUSIVE` test, well
  before the acquire.
- Candidate (a) applied only at the readlock is the same code as (b) and has the
  same gap.

So the fix is one invariant enforced at both waits: **never wait while holding
deferred pins.**

```c
/* retry-loop wait: drain before sleeping */
if (i >= ar_cnt) {
        i = 0;
        if (nflight > 0) { ...__memp_aio_drain...; nflight = 0; }
        __os_yield(env, 1, 0);
}

/* buffer-pin wait: try, and drain before blocking */
if (nflight > 0 &&
    (t_ret = MUTEX_TRY_READLOCK(env, bhp->mtx_buf)) != 0) {
        trylock_err = t_ret == DB_LOCK_NOTGRANTED ? 0 : t_ret;
        ...__memp_aio_drain...; nflight = 0;
        if (trylock_err != 0) { atomic_dec(&bhp->ref); ret = ...; goto err; }
        MUTEX_READLOCK(env, bhp->mtx_buf);
} else if (nflight == 0)
        MUTEX_READLOCK(env, bhp->mtx_buf);
```

Note the deliberate "drain even on a failchk error" ordering in the second hunk.
The obvious spelling — bail out on a non-`DB_LOCK_NOTGRANTED` trylock result — is
wrong here, and my first draft had that bug: it jumps to `err` with `nflight > 0`,
which trips `DB_ASSERT(env, nflight == 0)` and, in a non-diagnostic build, leaves
async writes outstanding against `aiow[]` and `w->ctx` in a **dead stack frame**.
Reaping first and only then reporting the error is what keeps that invariant.

A related side effect is now true for free and is worth stating, since it was
previously a latent hazard: both blocking `MUTEX_READLOCK`s of `mtx_buf` are
reached only with `nflight == 0`, so the macro's bare `return (DB_RUNRECOVERY)`
can no longer abandon a live window either.

This is not a timeout, not a widened window, and not a disabling of AIO. The
window still exists and still batches; it is simply reaped before the function
waits instead of only when full.

## 4. Liveness: post-fix run counts, and what they buy

| arm | binary | hangs / runs |
|---|---|---|
| fixed, shipped window | HEAD, `WIN=16` | **0 / 384** (96 + 288) |
| fixed, sharpened | HEAD, `WIN=64` | **0 / 384** (96 + 288) |
| sync-mode control | HEAD, sync writeback | 0 / 96 |

All 864 fixed-arm runs also reported `NONZERO-NONHANG: 0`, i.e. no crash and no
assertion firing was miscounted as "not a stall" — the two outcomes are scored
separately by design.

What the counts buy, stated as the probability of seeing zero stalls if the
defect were in fact untouched:

- at the pooled baseline rate of 5.7%: `P(0 in 384) = 1.6e-10`
- at the *lower* bound of the baseline CI, 2.9% — the most conservative reading
  of the baseline: `P(0 in 384) = 1.2e-05`
- at the sharpened rate of 18.8%, same sharpening on both arms:
  `P(0 in 384) = 1.9e-35`

Turned around: 0/384 puts a one-sided 95% upper bound of **0.78%** on the
residual rate, against a 5.7% baseline — a reduction of at least 7x established
at 95%, and the sharpened A/B (18/96 → 0/96 at identical `MEMP_AIO_WINDOW=64`,
`p ≈ 2e-09`) makes the causal attribution to this change rather than to ambient
timing.

A handful of clean runs would have proved nothing at a 5% base rate; that is why
the sharpened arm exists, and why both arms were run to 384.

## 5. Durability and "did it stay async" proofs

**A write error still surfaces.** This area already produced a "fast liar"
(`__memp_aio_drain` swallowing write errors), and the fix adds two new drain call
sites, so this is the load-bearing check. Injecting `EIO` into a completion's
`io_ret` at `__memp_aio_writeback_finish` — the exact field a real device error
carries — through one of the new early-drain paths:

```
INJECTED EIO into w->io_ret at completion            (1 injection)
FAIL: memp_sync: Input/output error                  <- error reached the caller
drivers: ... memp_sync=34(err 1) ...                 <- counted as an error
audit: lost=0 stale-but-present=0                    <- page stayed dirty, retried
aio: FAIL (1 failure)                                <- correct: we injected one
```

The same binary with no injection: `lost=0`, `aio: PASS (0 failures)`, so the
failure above is the injection and not the fix. Error propagation and the
"failed page stays `BH_DIRTY` and is rewritten" property both hold through the
new paths.

**The AIO path is still genuinely asynchronous.** A fix that quietly degraded to
synchronous writeback would "solve" the hang by deleting the feature. Measured
by breakpoint hit counts in one 8-second run:

```
__os_aio_submit      hit 18999 times
__memp_aio_drain     hit  1441 times
__aio_uring_reap     hit  1455 times   <- the io_uring backend, not a fallback
```

plus the kernel-visible check: 72 `iou-*` worker threads present in
`/proc/<pid>/task` during an aio run. The same instrumentation in `sync` mode
records **zero** hits on `__os_aio_submit` and `__memp_aio_drain`, which is what
makes the aio-mode counts mean something.

**Durability by content, every run.** The driver's own verdict is
`audit: lost=0 stale-but-present=0` plus a clean `DB->verify`, checked after a
close/reopen with `DB_RECOVER`, over 768 fixed-arm runs plus the controls.

**Default path untouched.** `DB_MPOOL_AIO` off: 0/96 stalls, `sync: PASS`, and
the changed code is unreachable — `nflight` is always 0 when `use_aio == 0`, so
both new branches collapse to the original `MUTEX_READLOCK`.

## 6. Region-signature and ABI proofs

Both are byte-identical to master. The fix changes function bodies and adds one
`int` stack local (`trylock_err`); no struct, typedef, or `#define` is touched
(verified by grepping the diff).

```
env struct signature (dist/env_sig_print.sh, absolute paths, source of each rev):
  master: 0xb86f77f0
  HEAD:   0xb86f77f0        -> UNCHANGED

public ABI:
  got:      DB=1744 DBC=552 DB_ENV=2088 DB_TXN=336
  expected: DB=1744 DBC=552 DB_ENV=2088 DB_TXN=336
```

The signature check follows the recorded trap for this gate: absolute paths, each
revision's **source** extracted to its own directory and measured by the HEAD
script, both values required non-empty, and an empty value treated as a hard
failure rather than a match. `sizeof(struct __db_mpool)` and
`sizeof(struct __db_txnmgr)` compile-time guards are untouched and still pass.

## 7. Harness change: the excuse branch is gone, and it has teeth

`test/c/leak-run.sh` used to treat exit 124 on the `aio` arm as `KNOWN ISSUE ...
not counted as a failure` and emit a `skip` verdict. With the stall fixed, the
only thing that branch can still do is hide the regression — the exact
vacuous-green shape this repo has shipped repeatedly. A timeout on that arm is
now a hard `FAIL`, with the diagnosis pointer inline: check whether the
`use_aio == 1` frame is blocked at the `mtx_buf` readlock (variant A) or spinning
in the retry loop with `nflight > 0` (variant B).

Verified two ways, since "the gate is now strict" is itself a claim that can be
vacuous.

**The tier passes for real,** run exactly as `.github/workflows/test-tiers.yml`
invokes it (`working-directory: test/c`, `TIMEOUT=300 AIO_SECONDS=20`,
`--enable-debug --enable-diagnostic`):

```
--- aio_concurrent_sync sync: PASS      audit: lost=0   sync: PASS (0 failures)
--- aio_concurrent_sync aio:  PASS      audit: lost=0   aio:  PASS (0 failures)
ALL LEAK TESTS PASS                     leak-run rc=0
manifest gate: OK   (14 verdict lines)
RESULT leak aio_concurrent_sync@sync pass
RESULT leak aio_concurrent_sync@aio  pass
```

**And it fails when it should.** `leak-run.sh` recompiles its drivers from
`libdb.a` at startup, so faking a binary would give a false PASS — the teeth test
therefore patches the driver *source* to stall on demand:

```
--- aio_concurrent_sync sync: PASS
--- aio_concurrent_sync aio: FAIL (STALLED, timed out after 25s)
    This is the os_aio deferred-pin stall REGRESSING. ...
LEAK TESTS FAILED                       leak-run rc=1
RESULT leak aio_concurrent_sync@aio  fail
```

Before this change that same input produced `skip` and an overall pass.

## 8. Residual risk

- Draining earlier means shallower average queue depth when the sync loop
  contends, so async writeback batches less under heavy contention. This is a
  throughput question, not a correctness one, and it is not measured here.
- 0/384 bounds the residual stall rate at 0.78% (95%), not at zero. If a stall
  is ever seen again, the classification recipe in §7 tells the two variants
  apart in one capture.
- Everything here is single-process. A multi-process aio environment is a
  different exposure: the AIO context is per-process, and another process's
  in-transit buffer is waited on via `mtx_buf` by code this fix does not touch.

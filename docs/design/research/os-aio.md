# OS / mpool I/O survey — basis for an internal async-I/O abstraction

Read-only survey of `/Users/gregburd/oss/libdb` @ `master`. No source was
modified. All line numbers are from the working tree at survey time
(2026-06-17). Paths are repo-relative.

---

## 1. The OS I/O abstraction (`src/os/`, `src/os_windows/`)

All page and byte I/O funnels through a single dispatcher, `__os_io`, which
takes an opcode (`DB_IO_READ` / `DB_IO_WRITE`). Everything below it is
**fully synchronous and blocking**.

Opcodes and the file handle:

- `src/dbinc/os.h:92-93` — `#define DB_IO_READ 1`, `#define DB_IO_WRITE 2`.
- `src/dbinc/os.h:112-151` — `struct __fh_t` (`DB_FH`). Holds `int fd`
  (POSIX) and `HANDLE handle` (Win32, `:130`), plus `db_mutex_t mtx_fh`
  (`:124`) that "protects the handle/fd across seek and read/write pairs."
  Last-seek bookkeeping `pgno/pgsize/offset` (`:139-141`).

### POSIX — `src/os/os_rw.c`

| Function | Lines | Signature | Notes |
|---|---|---|---|
| `__os_io` | 20-128 | `(ENV*, int op, DB_FH*, db_pgno_t pgno, u_int32_t pgsize, u_int32_t relative, u_int32_t io_len, u_int8_t *buf, size_t *niop)` | Entry point for page I/O. |
| `__os_read` | 134-196 | `(ENV*, DB_FH*, void *addr, size_t len, size_t *nrp)` | Loop over `read(2)`. |
| `__os_write` | 204-229 | `(ENV*, DB_FH*, void *addr, size_t len, size_t *nwp)` | Zero-fill wrapper → `__os_physwrite`. |
| `__os_physwrite` | 236-310 | `(ENV*, DB_FH*, void *addr, size_t len, size_t *nwp)` | Loop over `write(2)`. |
| `__os_seek` | `src/os/os_seek.c:19-66` | `(ENV*, DB_FH*, db_pgno_t, u_int32_t pgsize, off_t relative)` | `lseek(2)`. |

Behavior of `__os_io` (`os_rw.c:48-101`): if `pread`/`pwrite` are available
and no `j_*` hook / no-zero-fill quirk applies, it computes
`offset = relative ? relative : (off_t)pgno * pgsize` and issues a single
**blocking** `pread`/`pwrite` (`:67-69`, `:88-90`) with no `mtx_fh` held. On
a short transfer or when a hook is installed, it falls through to the `slow:`
label (`:101`) which takes `MUTEX_LOCK(env, fhp->mtx_fh)`, then
`__os_seek` + `__os_read`/`__os_write`, then unlocks (`:101-122`).
`LAST_PANIC_CHECK_BEFORE_IO(env)` (`os.h:104`) is invoked immediately before
every syscall.

### Open — `src/os/os_open.c`

`__os_open` (`:18-200`), signature
`(ENV*, const char *name, u_int32_t page_size, u_int32_t flags, int mode, DB_FH **fhpp)`.
Translates `DB_OSO_*` to `open(2)` `oflags`. Relevant flags for AIO:

- `DB_OSO_DIRECT` → `O_DIRECT` (`:78-80`) and Solaris `directio()` (`:166-170`).
  Direct I/O is the natural pairing for io_uring / POSIX-aio buffered bypass.
- `DB_OSO_DSYNC` → `O_DSYNC` + sets `DB_FH_NOSYNC` (`:82-84`, `:148-152`).
- `DB_OSO_SEQ` is accepted in `OKFLAGS` (`:42`) but, notably, **never mapped
  to any `posix_fadvise`/`madvise` hint** — there is no readahead plumbing
  (see §3).

There is **no async open path and no per-fd AIO context** anywhere in the
handle. `__os_openhandle` (`src/os/os_handle.c`) just wraps `open(2)`.

### Windows — `src/os_windows/os_rw.c`

`__os_io` (`:16-93`) is structurally identical but uses an `OVERLAPPED`
struct **with `over.hEvent = 0` and the comment "we don't want asynchronous
notifications"** (`:36`). It calls `ReadFile`/`WriteFile` with the
`OVERLAPPED` purely to pass a 64-bit offset (`:33-35`), then treats the result
synchronously (`:57-71`). `__os_read` (`:103`), `__os_write` (`:163`),
`__os_physwrite` (`:184`) loop on `ReadFile`/`WriteFile` with a NULL
`OVERLAPPED`. The handle (`fhp->handle`) is **not** opened
`FILE_FLAG_OVERLAPPED`, so it cannot today be associated with an IOCP. See
`src/os_windows/os_open.c` (`__os_open`, 7.6 KB) — `CreateFile` is issued
without `FILE_FLAG_OVERLAPPED`.

**Takeaway:** the entire library reaches disk for data pages through exactly
one choke point, `__os_io(env, DB_IO_{READ,WRITE}, fhp, pgno, pgsize, 0, len, buf, niop)`.
An async abstraction can be introduced beside it without touching callers.

---

## 2. mpool I/O call sites — where sync becomes a candidate for async

### 2a. Read-in (prefetch candidate)

`__memp_pgread` — `src/mp/mp_bh.c:217-310`, signature
`(DB_MPOOLFILE *dbmfp, BH *bhp, int can_create)`.

- Pre-conditions asserted: buffer is held `BH_EXCLUSIVE` (`:233`), and marked
  `BH_TRASH` on entry (`:236`).
- The actual blocking read: `mp_bh.c:298-300`
  ```c
  ret = __os_io(env, DB_IO_READ, dbmfp->fhp,
      bhp->pgno, pagesize, 0, pagesize, bhp->buf, &nr);
  ```
- On success clears `BH_TRASH` (`:309`).

**Where a missing page is read in** — `src/mp/mp_fget.c` (`__memp_fget`):

- The single demand-read site is the `BH_TRASH` block at
  `mp_fget.c:920-933`; the call is `mp_fget.c:927-929`:
  ```c
  if ((ret = __memp_pgread(dbmfp,
      bhp, LF_ISSET(DB_MPOOL_CREATE) ? 1 : 0)) != 0)
  ```
- The cache-miss buffer is allocated and set up in the `SECOND_MISS` arm
  (`mp_fget.c:775-868`): `__memp_alloc` is reached via the `alloc:` label
  (`:649-651`), the new `bhp` is latched exclusive
  (`MUTEX_LOCK(env, bhp->mtx_buf)` at `:806`, `F_SET(bhp, BH_EXCLUSIVE)` at
  `:808`), inserted into the hash bucket, and marked `F_SET(bhp, BH_TRASH)`
  at `:865` for the non-extending (real disk read) case.

  **This is the prefetch insertion point.** A `__memp_fget` variant (or a
  `DB_MPOOL_PREFETCH` flag) could: allocate + insert the buffer, mark it
  `BH_TRASH | BH_EXCLUSIVE`, **submit** an async read via the new interface
  instead of calling `__memp_pgread`, and return without blocking. The
  completion handler would fill `bhp->buf`, clear `BH_TRASH`, and drop the
  exclusive latch — exactly what `__memp_pgread:298-309` does inline today.
  Sequential-scan callers (btree `__bam_*`) are the obvious clients.

### 2b. Eviction / sync / trickle writes (writeback candidates)

`__memp_pgwrite` — `src/mp/mp_bh.c:315-510` (static), reached only through
`__memp_bhwrite` (`mp_bh.c:20-209`). The blocking write is `mp_bh.c:469-471`:
```c
ret = __os_io(env, DB_IO_WRITE, dbmfp->fhp, bhp->pgno,
    mfp->pagesize, 0, mfp->pagesize, buf, &nw);
```
Three callers of `__memp_bhwrite`, each an async-writeback candidate:

1. **Eviction (allocation pressure)** — `src/mp/mp_alloc.c:509`
   `ret = __memp_bhwrite(dbmp, hp, bh_mfp, bhp, 0);` inside `__memp_alloc`,
   in the `this_buffer:` arm (`:473-533`). The victim is latched exclusive via
   `MUTEX_TRYLOCK(env, bhp->mtx_buf)` (`:489`) + `F_SET(bhp, BH_EXCLUSIVE)`
   (`:492`); a dirty victim is written synchronously before reuse
   (`dirty_eviction`, `:505-533`). Candidate for async writeback **with a
   pending-completion barrier before the slot is handed out**.

2. **Checkpoint / explicit sync & trickle** — `src/mp/mp_sync.c`,
   `__memp_sync_int` (proto `src/dbinc_auto/mp_ext.h:99`:
   `(ENV*, DB_MPOOLFILE*, u_int32_t, u_int32_t flags, u_int32_t *wrote, int *interrupted)`).
   The write loop is `mp_sync.c:477-...`; the write call is `mp_sync.c:562-563`
   `t_ret = __memp_bhwrite(dbmp, hp, mfp, bhp, 1)`. Buffers are gathered into
   `bharray`, sorted by `__bhcmp` for file/page order (`:451-452`) — already a
   batch, ideal for a single async **submit-many / reap** cycle.

3. **Trickle (background cleaner)** — `src/mp/mp_trickle.c`, `__memp_trickle`
   (static, `:50-...`) computes `need_clean` then calls
   `__memp_sync_int(env, NULL, need_clean, DB_SYNC_TRICKLE | DB_SYNC_INTERRUPT_OK, &wrote, NULL)`.
   This is the lowest-risk first target for async writeback: it is already
   "best effort," already batched, and already interruptible
   (`DB_SYNC_INTERRUPT_OK`, `src/dbinc/mp.h:48`; `DB_SYNC_TRICKLE`,
   `mp.h:51`).

Note `mp_sync.c:512-519` already **skips buffers held `BH_EXCLUSIVE`** by
another thread ("come back to it"), and `:505-510` pins each via
`atomic_inc(&bhp->ref)` + `MUTEX_READLOCK(env, bhp->mtx_buf)` — the same
reference/latch discipline an async writer must maintain until completion is
reaped.

---

## 3. Existing read-ahead / prefetch facilities

**None.** Grep across `src/` for
`prefetch|readahead|read_ahead|posix_fadvise|madvise|MP_FOR_PREFETCH|POSIX_FADV`
returns zero matches. The only related artifacts:

- `DB_OSO_SEQ` open flag exists (`src/os/os_open.c:42`, `OKFLAGS`) but is
  **dead** with respect to I/O hints — never translated to `posix_fadvise`
  or `O_*`.
- `DB_MPOOL_*` `__memp_fget` flags (`src/mp/mp_fget.c:58-59`):
  `DB_MPOOL_CREATE | DB_MPOOL_DIRTY | DB_MPOOL_EDIT | DB_MPOOL_LAST |
  DB_MPOOL_NEW` — none express "fetch ahead / don't block." There is no
  `DB_MPOOL_PREFETCH`.
- `DB->stat` / `DB_MPOOL_STAT` (`src/mp/mp_stat.c`) track `st_page_in`,
  `st_cache_miss`, `st_page_trickle`, etc., but no readahead counters.

So a prefetch facility is greenfield: it needs both a new public hint
(`DB_MPOOL_PREFETCH` or a `DB_MPOOLFILE->prefetch(pgno, npages)` method) and
the async submit path of §2a.

---

## 4. Proposed minimal internal async-I/O abstraction (`src/os/os_aio.c`)

### Design constraints discovered

- Single choke point (`__os_io`) → the abstraction can be a sibling layer.
- Buffers are page-sized, page-aligned `bhp->buf` regions; offset is
  `pgno * pgsize`. The op already carries everything io_uring/aio needs.
- **Multi-process shared mpool is the hard constraint** (see §4b).

### 4a. Interface (platform-neutral)

```
os_aio_env_create(env)            -> per-ENV aio context (ring/kqueue/iocp/aiocb pool)
os_aio_env_destroy(env)
os_aio_submit_read (ctx, fhp, pgno, pgsize, buf, cookie) -> handle | EAGAIN
os_aio_submit_write(ctx, fhp, pgno, pgsize, buf, cookie) -> handle | EAGAIN
os_aio_reap(ctx, min, max, completions[], timeout)       -> n completions
os_aio_cancel(ctx, handle)        -> best-effort
```

`cookie` carries `{ bhp, hp, mfp }` so the completion handler can run the
exact tail of `__memp_pgread` (clear `BH_TRASH`, drop latch) or
`__memp_pgwrite` (clear `BH_DIRTY`, `atomic_dec(&hp->hash_page_dirty)`,
`atomic_dec(&bhp->ref)`, drop latch). The context lives off `ENV` alongside
`env->mp_handle`, created in `__env_open` and torn down in `__env_close`.

### 4b. Backend mapping

| Backend | Submit | Complete / reap | Notes |
|---|---|---|---|
| **Linux io_uring** | `io_uring_get_sqe` + `io_uring_prep_read/write` (offset, `bhp->buf`, `pgsize`) + `io_uring_submit` | `io_uring_wait_cqe` / `io_uring_peek_batch_cqe`; `user_data` = cookie | One ring per ENV; pairs with `O_DIRECT` (`DB_OSO_DIRECT`). Cleanest fit. |
| **BSD/macOS kqueue + aio** | `aio_read`/`aio_write` on an `aiocb` whose `aio_sigevent` is `EVFILT_AIO` against the kqueue | `kevent()`; `kev.udata`/`aiocbp` = cookie | macOS aio is limited & no `O_DIRECT` (use `F_NOCACHE`); modest depth. |
| **Windows overlapped/IOCP** | `ReadFile`/`WriteFile` with `OVERLAPPED` on a `FILE_FLAG_OVERLAPPED` handle bound to an IOCP | `GetQueuedCompletionStatus(Ex)`; `lpOverlapped`→cookie | **Requires reopening the handle with `FILE_FLAG_OVERLAPPED`** — today `os_windows/os_open.c` does not set it, and `os_rw.c:36` deliberately uses `hEvent=0` synchronous overlapped. |
| **POSIX aio fallback** | `aio_read`/`aio_write`/`lio_listio` on `aiocb` array | `aio_suspend` + `aio_error`/`aio_return`, or `SIGEV_THREAD` callback | Portable floor; often thread-pool emulated. Last resort. |

A `j_aio_*` global hook (mirroring the existing `DB_GLOBAL(j_pread)` etc. in
`os_rw.c`) lets the test harness stub the backend.

### 4c. Per-process AIO context vs. shared mpool — the correctness pivot

AIO contexts (io_uring ring fds, kqueues, IOCP handles, `aiocb`s) are
**per-process**. The mpool buffer-cache region is **shared across processes**.
Therefore a buffer marked "I/O in flight" by process A **cannot have its
completion reaped by process B**. Resolution:

1. Async submit is only ever issued by the process that **holds
   `bhp->mtx_buf` exclusive** (reads: `mp_fget.c:806`/`mp_bh.c:233`; writes:
   the exclusive/pinned victim in `mp_alloc.c:489-492` and the pinned buffer
   in `mp_sync.c:505-526`). The latch is held across submit→reap and released
   **only by the submitting process in its completion handler**.

2. Any **other** process/thread that wants that page does **not** try to reap
   foreign completions. It uses the **existing in-transit wait path**: it
   blocks on `bhp->mtx_buf`, then re-checks `BH_TRASH`. That path already
   exists and is unchanged:
   - Waiter acquires the latch: `mp_fget.c:309-319`
     (`MUTEX_LOCK`/`MUTEX_READLOCK(env, bhp->mtx_buf)`), with the
     shared→exclusive upgrade-on-`BH_TRASH` dance at `mp_fget.c:322-329`.
     The `BH_TRASH` re-check after acquiring is `mp_fget.c:324` and the
     `revive`/`break` re-read at `mp_fget.c:485-505`.
   - The flush/eviction scan simply **skips** buffers another process holds
     `BH_EXCLUSIVE` (`mp_sync.c:512-519`) and pins+waits on `mtx_buf`
     otherwise (`mp_sync.c:505-526`).

   So the fallback is "block on `bhp->mtx_buf` and re-read on wake" — the same
   semantics used today when one thread is mid-`__memp_pgread` and another
   requests the page. Async simply widens the window during which the latch is
   held; it does not introduce a new cross-process reaping requirement.

3. **Crash/abandonment:** if the submitting process dies with an op in flight,
   the buffer stays `BH_TRASH`+latched. This is the same failure surface as a
   process dying inside synchronous `__memp_pgread` today, handled by existing
   environment-recovery / dead-process latch cleanup (`mtx_buf` recovery on
   `DB_RECOVER`). No new invariant.

---

## 5. WAL-rule enforcement before a dirty page write (must be preserved)

The "log-flush-before-page-write" (WAL) rule is enforced in two places that
async writeback **must continue to honor before submitting a write**:

1. **Per-page, inside `__memp_pgwrite`** — `src/mp/mp_bh.c:347-354`:
   ```c
   if (LOGGING_ON(env) && mfp->lsn_off != DB_LSN_OFF_NOTSET &&
       !IS_CLIENT_PGRECOVER(env)) {
       memcpy(&lsn, bhp->buf + mfp->lsn_off, sizeof(DB_LSN));
       if (!IS_NOT_LOGGED_LSN(lsn) &&
           (ret = __log_flush(env, &lsn)) != 0)
           goto err;          /* do NOT write the page */
   }
   ```
   It reads the page's LSN at `mfp->lsn_off` (`src/dbinc/mp.h:498`) and forces
   the log up to that LSN via `__log_flush` before the `__os_io(DB_IO_WRITE)`
   at `mp_bh.c:469`. A `DIAGNOSTIC` assertion re-verifies the durable log LSN
   (`lp->s_lsn`) has passed the page LSN (`mp_bh.c:356-405`).

2. **Batch pre-flush, inside `__memp_sync_int`** — `src/mp/mp_sync.c:464-473`:
   ```c
   if (LOGGING_ON(env) && (ret = __log_flush(env, NULL)) != 0)
       goto err;
   ```
   A whole-log flush before the write loop, as an optimization; the per-page
   check in `__memp_pgwrite` still runs because pages may be re-dirtied after
   this flush.

**Implication for async:** the `__log_flush(env, &lsn)` at `mp_bh.c:352` must
complete (synchronously, or as an ordering dependency) **before** the async
write SQE for that page is submitted. The clean integration is to keep the
WAL check at the top of the write submit routine — i.e. factor
`__memp_pgwrite` into `prep (WAL + pgout) → submit → finish`, where `prep`
retains lines `347-405` verbatim and only the `__os_io` at `469` is replaced
by `os_aio_submit_write`. The dirty-state bookkeeping in the `err:`/
`file_dead:` tail (`mp_bh.c:480-507`: clear `BH_DIRTY`, decrement
`hash_page_dirty`) moves into the completion handler.

---

## Summary (10 lines)

1. All data-page I/O passes through one synchronous choke point,
   `__os_io(DB_IO_READ|DB_IO_WRITE, fhp, pgno, pgsize, …)` in
   `src/os/os_rw.c:20` (and the Win32 twin in `src/os_windows/os_rw.c:16`).
2. Reads block in `__memp_pgread` (`mp_bh.c:298`); writes block in
   `__memp_pgwrite` (`mp_bh.c:469`), reached only via `__memp_bhwrite`.
3. The demand read-in site is `__memp_fget`'s `BH_TRASH` block,
   `mp_fget.c:927`; the buffer is allocated/latched/marked at
   `mp_fget.c:806-865` — the natural **prefetch submit** point.
4. Writeback candidates: eviction (`mp_alloc.c:509`), checkpoint/sync
   (`mp_sync.c:562`), and trickle (`mp_trickle.c` → `__memp_sync_int`);
   trickle is the safest first target (already batched + interruptible).
5. There is **no** existing prefetch/readahead — zero matches for
   `prefetch|readahead|fadvise|madvise`; `DB_OSO_SEQ` is accepted but unused.
6. Proposed `os_aio` interface: per-ENV context +
   `submit_read/submit_write/reap/cancel`, with a `{bhp,hp,mfp}` cookie that
   runs the existing pgread/pgwrite completion tail.
7. Backends map cleanly: io_uring (prep+submit/CQE), kqueue+aio
   (`aiocb`/`EVFILT_AIO`/`kevent`), Windows IOCP (`OVERLAPPED`/
   `GetQueuedCompletionStatus` — needs `FILE_FLAG_OVERLAPPED`), POSIX aio
   (`lio_listio`/`aio_suspend`) as the floor.
8. AIO contexts are per-process but mpool is shared, so only the process
   holding `bhp->mtx_buf` exclusive submits/reaps; other processes fall back
   to the **existing in-transit wait** (`MUTEX_*LOCK(bhp->mtx_buf)` at
   `mp_fget.c:309-329`, skip-if-`BH_EXCLUSIVE` at `mp_sync.c:512-519`).
9. The WAL rule (`__log_flush(env,&lsn)` at `mp_bh.c:352`, plus batch flush at
   `mp_sync.c:472`) must be evaluated **before** a write is submitted; refactor
   `__memp_pgwrite` into prep→submit→finish keeping `mp_bh.c:347-405` intact.
10. Net: introduce `os_aio` beside `__os_io`, add a `DB_MPOOL_PREFETCH`
    fetch path, and convert trickle→sync→eviction writeback incrementally,
    with no change to the cross-process latch invariants.

---

## Proposed `os_aio` interface — C header sketch

```c
/*-
 * src/dbinc/os_aio.h  (PROPOSED — not yet implemented)
 *
 * Internal asynchronous page-I/O abstraction layered beside __os_io().
 * One context per ENV; submit/reap is single-process. A buffer with an
 * I/O in flight is held BH_EXCLUSIVE by the submitting process; other
 * processes never reap foreign completions and instead block on
 * bhp->mtx_buf (the existing in-transit wait).  WAL: callers MUST have
 * satisfied the log-flush-before-write rule (mp_bh.c __memp_pgwrite)
 * prior to os_aio_submit_write().
 */
#define DB_AIO_READ   1            /* mirrors DB_IO_READ  */
#define DB_AIO_WRITE  2            /* mirrors DB_IO_WRITE */

struct __db_aio_op {              /* one submitted operation */
    DB_FH      *fhp;              /* target file handle              */
    db_pgno_t   pgno;            /* page number (offset = pgno*pgsize) */
    u_int32_t   pgsize;          /* transfer length == page size     */
    u_int8_t   *buf;             /* page buffer (bhp->buf)           */
    void       *cookie;          /* opaque: {BH*, DB_MPOOL_HASH*, MPOOLFILE*} */
    int         op;              /* DB_AIO_READ | DB_AIO_WRITE       */
};

struct __db_aio_completion {
    void       *cookie;          /* as submitted                     */
    size_t      nbytes;          /* bytes transferred                */
    int         ret;             /* 0 or __os_posix_err()-style code */
};

typedef struct __db_aio_ctx DB_AIO_CTX;   /* opaque per-ENV backend state */

/* Lifecycle — created in __env_open, destroyed in __env_close. */
int  __os_aio_env_create  __P((ENV *, DB_AIO_CTX **));
void __os_aio_env_destroy __P((ENV *, DB_AIO_CTX *));

/* Submit one op. Returns 0, or EAGAIN when the queue is full (caller
 * then falls back to synchronous __os_io and proceeds). */
int  __os_aio_submit __P((ENV *, DB_AIO_CTX *, struct __db_aio_op *));

/* Reap between min and max completions; honors DB_SYNC_INTERRUPT_OK-style
 * cancellation via a timeout. Returns count in *np. */
int  __os_aio_reap   __P((ENV *, DB_AIO_CTX *, u_int32_t min, u_int32_t max,
                          struct __db_aio_completion *, u_int32_t *np,
                          db_timeout_t timeout));

/* Best-effort cancel (used on env shutdown / interrupt). */
int  __os_aio_cancel __P((ENV *, DB_AIO_CTX *, struct __db_aio_op *));

/*
 * Backend selection (compile/config time):
 *   HAVE_AIO_IO_URING  -> io_uring        (Linux; pair with DB_OSO_DIRECT)
 *   HAVE_AIO_KQUEUE    -> kqueue + aio(4)  (BSD/macOS; F_NOCACHE)
 *   HAVE_AIO_IOCP      -> overlapped/IOCP  (Windows; needs FILE_FLAG_OVERLAPPED)
 *   HAVE_AIO_POSIX     -> POSIX aio        (portable fallback / floor)
 * Test override: DB_GLOBAL(j_aio_*) hooks, mirroring j_pread/j_pwrite.
 */
```

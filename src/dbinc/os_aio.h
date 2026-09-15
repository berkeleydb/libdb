/*-
 * See the file LICENSE for redistribution information.
 *
 * Asynchronous I/O abstraction (Stage 2).
 *
 * A thin, pluggable async-I/O layer used by the buffer pool to (a) prefetch
 * pages and (b) trickle dirty pages to disk in the background without blocking
 * a foreground thread on a device write.  The default backend is synchronous
 * (submit performs the I/O inline and the completion runs immediately), so the
 * abstraction is behaviour-preserving everywhere; platform backends
 * (Linux io_uring, BSD/macOS kqueue+aio, Windows IOCP, POSIX aio) override it.
 *
 * Per-process: an AIO context is owned by the process that created it; in a
 * multi-process environment a page marked in-transit by one process is reaped
 * only by that process, and other processes fall back to the existing
 * in-transit buffer wait (mtx_buf).
 */
#ifndef	_DB_OS_AIO_H_
#define	_DB_OS_AIO_H_

#if defined(__cplusplus)
extern "C" {
#endif

struct __db_aio_context;	typedef struct __db_aio_context DB_AIO_CONTEXT;
struct __db_aio_op;		typedef struct __db_aio_op DB_AIO_OP;

/*
 * Completion callback: invoked (in the reaping thread, or inline for the
 * synchronous backend) when an op finishes.  "ret" is 0 on success or an
 * errno.  "cookie" is the caller's opaque pointer (the buffer header).
 */
typedef void (*db_aio_done_fn) __P((ENV *, void *cookie, int ret));

/* One outstanding async I/O. */
struct __db_aio_op {
	int		 op;		/* DB_IO_READ / DB_IO_WRITE. */
	DB_FH		*fhp;		/* Target file. */
	db_pgno_t	 pgno;		/* Page number. */
	u_int32_t	 pagesize;	/* Bytes. */
	void		*buf;		/* Data buffer (page-aligned). */
	void		*cookie;	/* Caller context (BH *). */
	db_aio_done_fn	 done;		/* Completion callback. */
};

/*
 * Backend vtable.  A backend implements submit/reap/cancel; the generic layer
 * owns the context lifecycle and the synchronous fallback.
 */
typedef struct __db_aio_backend {
	const char *name;
	int  (*submit)  __P((ENV *, DB_AIO_CONTEXT *, DB_AIO_OP *));
	/* Reap up to max completions; -1 max means "all ready". */
	int  (*reap)    __P((ENV *, DB_AIO_CONTEXT *, int max, int wait));
	int  (*cancel)  __P((ENV *, DB_AIO_CONTEXT *));
	int  (*destroy) __P((ENV *, DB_AIO_CONTEXT *));
} DB_AIO_BACKEND;

/*
 * AIO context.  Owned by the process that created it.  A NULL backend
 * means the synchronous fallback (see os_aio.c); a platform backend
 * installs its vtable and per-context state via priv.
 *
 * mtx_aio serializes USE of this context among the concurrent
 * __memp_sync_int callers in this process (checkpoint, trickle,
 * memp_sync/fsync, and DB_SYNC_ALLOC from eviction).  A single sync call
 * must be the context's only submitter for the whole span from its first
 * submit to its final drain, for three reasons:
 *
 *  1) __memp_aio_drain must not consume another caller's completions.
 *     Reaping is a shared-queue operation (io_uring drains whatever CQEs
 *     are ready); if a second caller's completions could satisfy the first
 *     caller's drain, the first caller would run the write completion --
 *     clearing BH_DIRTY, unpinning the buffer, and freeing the pgout page
 *     copy -- for writes still in flight.  That is a false durable
 *     frontier AND a write-after-free.
 *  2) Each caller's MEMP_AIO_W window is a stack array in its own
 *     __memp_sync_int frame, and is the cookie of its in-flight ops.  A
 *     caller that returned while its ops were outstanding would leave the
 *     backend holding pointers into a dead stack frame.
 *  3) The backend submission queues are not themselves thread-safe (an
 *     io_uring SQE ring has no internal locking, and the POSIX aio slot
 *     table is scanned unlocked), and inflight is a plain counter.
 *     Exclusive use makes all three single-threaded.
 *
 * It is acquired with MUTEX_TRYLOCK and never waited on: a caller that does
 * not get it simply writes synchronously, which is the reference behaviour.
 * Because it is never blocked on, it cannot participate in a deadlock cycle
 * and adds no lock-ordering rule.
 *
 * It lives HERE, not in DB_MPOOL, on purpose.  Both structs are
 * process-private, but env_sig.c hashes sizeof(struct __db_mpool)
 * unconditionally into the build signature, and __env_region_attach rejects
 * any environment whose stored signature differs (BDB1539, returning
 * DB_VERSION_MISMATCH) -- so a field added to DB_MPOOL breaks
 * upgrade-in-place against every existing environment, silently as far as
 * libabigail is concerned.  env_sig.c does not hash this struct (dist/s_sig
 * emits __ADD only for structs reachable from the headers it scans, and
 * os_aio.h is not one of them), so per-process aio state belongs here.
 * dist/s_sig output must stay byte-identical to master's env_sig.c: if a
 * regen ever adds __ADD(__db_aio_context), this field's home is no longer
 * signature-neutral and must move again.
 *
 * KNOWN ISSUE (opt-in path only).  With DB_MPOOL_AIO on and this latch in
 * place, test/c/aio_concurrent_sync in "aio" mode hangs in roughly 3 runs in
 * 67 (>900s, permanent), while "sync" mode is 0/84.  It is a sync-loop stall,
 * not the cross-reap corruption this latch fixes: lost=0 and db_verify is
 * clean on every recovered run, and on master the same test SEGVs in
 * __aio_uring_reap.  DB_MPOOL_AIO is default-OFF, so no default path is
 * affected.  See /tmp/os-aio-abi-fix-report.md in the fix commit's notes.
 */
struct __db_aio_context {
	const DB_AIO_BACKEND *backend;	/* NULL = synchronous fallback. */
	void		*priv;		/* Backend-private state. */
	u_int32_t	 depth;		/* Requested queue depth. */
	u_int32_t	 inflight;	/* Ops submitted, not yet reaped. */
	db_mutex_t	 mtx_aio;	/* Exclusive-use latch; see above. */
};

/*
 * PUBLIC: int __os_aio_create __P((ENV *, u_int32_t, DB_AIO_CONTEXT **));
 * PUBLIC: int __os_aio_submit __P((ENV *, DB_AIO_CONTEXT *, DB_AIO_OP *));
 * PUBLIC: int __os_aio_reap __P((ENV *, DB_AIO_CONTEXT *, int, int));
 * PUBLIC: int __os_aio_destroy __P((ENV *, DB_AIO_CONTEXT *));
 * PUBLIC: int __os_aio_available __P((ENV *));
 */
int __os_aio_create __P((ENV *, u_int32_t, DB_AIO_CONTEXT **));
int __os_aio_submit __P((ENV *, DB_AIO_CONTEXT *, DB_AIO_OP *));
int __os_aio_reap __P((ENV *, DB_AIO_CONTEXT *, int /*max*/, int /*wait*/));
int __os_aio_destroy __P((ENV *, DB_AIO_CONTEXT *));
int __os_aio_available __P((ENV *));	/* 1 if a real async backend is active */
int __os_aio_ctx_available __P((DB_AIO_CONTEXT *));	/* per-context async? */

/*
 * PUBLIC: int __os_aio_uring_init __P((ENV *, DB_AIO_CONTEXT *));
 *	Install the Linux io_uring backend on a context (HAVE_IO_URING
 *	builds only).  Returns 0 and sets ctx->backend on success, or a
 *	non-zero error leaving the context on the synchronous fallback.
 */
int __os_aio_uring_init __P((ENV *, DB_AIO_CONTEXT *));

/*
 * PUBLIC: int __os_aio_posix_init __P((ENV *, DB_AIO_CONTEXT *));
 *	Install the POSIX.1b aio backend (aio_read/aio_write + aio_suspend) on
 *	a context (HAVE_AIO_POSIX builds only).  Native async path on
 *	Solaris/illumos and macOS; portable fallback ahead of the thread pool.
 */
int __os_aio_posix_init __P((ENV *, DB_AIO_CONTEXT *));

/*
 * PUBLIC: int __os_aio_kqueue_init __P((ENV *, DB_AIO_CONTEXT *));
 *	Install the BSD kqueue + aio backend (EVFILT_AIO completions) on a
 *	context (HAVE_AIO_KQUEUE builds only; FreeBSD/BSD, not macOS).
 */
int __os_aio_kqueue_init __P((ENV *, DB_AIO_CONTEXT *));

/*
 * PUBLIC: int __os_aio_pool_init __P((ENV *, DB_AIO_CONTEXT *));
 *	Install the portable thread-pool offload backend (HAVE_AIO_THREADPOOL
 *	builds only).  Returns 0 and sets ctx->backend on success.
 */
int __os_aio_pool_init __P((ENV *, DB_AIO_CONTEXT *));

/*
 * PUBLIC: int __os_aio_iocp_init __P((ENV *, DB_AIO_CONTEXT *));
 *	Install the Windows IOCP native file-AIO backend (HAVE_IOCP builds
 *	only).  Returns 0 and sets ctx->backend on success.
 */
int __os_aio_iocp_init __P((ENV *, DB_AIO_CONTEXT *));

/* Queue depth requested at create time; backends may clamp. */
#define	DB_AIO_DEFAULT_DEPTH	64

#if defined(__cplusplus)
}
#endif
#endif /* !_DB_OS_AIO_H_ */

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
 */
struct __db_aio_context {
	const DB_AIO_BACKEND *backend;	/* NULL = synchronous fallback. */
	void		*priv;		/* Backend-private state. */
	u_int32_t	 depth;		/* Requested queue depth. */
	u_int32_t	 inflight;	/* Ops submitted, not yet reaped. */
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
 * PUBLIC: int __os_aio_pool_init __P((ENV *, DB_AIO_CONTEXT *));
 *	Install the portable thread-pool offload backend (HAVE_AIO_THREADPOOL
 *	builds only).  Returns 0 and sets ctx->backend on success.
 */
int __os_aio_pool_init __P((ENV *, DB_AIO_CONTEXT *));

/* Queue depth requested at create time; backends may clamp. */
#define	DB_AIO_DEFAULT_DEPTH	64

#if defined(__cplusplus)
}
#endif
#endif /* !_DB_OS_AIO_H_ */

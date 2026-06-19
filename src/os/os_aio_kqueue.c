/*-
 * See the file LICENSE for redistribution information.
 *
 * BSD kqueue + POSIX aio backend for the os_aio abstraction.
 *
 * Submits page I/O with aio_read/aio_write whose aiocb requests a kqueue
 * completion (sigev_notify == SIGEV_KEVENT, sigev_notify_kqueue == our kq);
 * each finished op posts an EVFILT_AIO kevent whose udata is the op record,
 * so reaping is a single kevent() call rather than polling every aiocb with
 * aio_error.  This is the native event-driven async path on FreeBSD (and
 * other BSDs that provide sigev_notify_kqueue).
 *
 * macOS is intentionally NOT this backend: its <sys/signal.h> lacks
 * sigev_notify_kqueue and its historical SIGEV_KEVENT aio support is
 * unreliable, so macOS uses the POSIX-aio backend (aio_suspend) instead.
 * Selection is decided at configure time via HAVE_AIO_KQUEUE.
 *
 * Built only when configured with kqueue-aio support (HAVE_AIO_KQUEUE);
 * otherwise this file is an empty translation unit and contexts use the
 * synchronous fallback in os_aio.c.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/os_aio.h"

#ifdef HAVE_AIO_KQUEUE

#include <sys/event.h>
#include <sys/types.h>
#include <aio.h>
#include <errno.h>
#include <unistd.h>

/* One in-flight op: control block plus the caller's completion info. */
typedef struct __aio_kq_op {
	struct aiocb	 cb;		/* control block (kevent udata = this). */
	void		*cookie;	/* caller context (BH *). */
	db_aio_done_fn	 done;		/* completion callback. */
	u_int32_t	 len;		/* expected transfer length. */
	int		 op;		/* DB_IO_READ / DB_IO_WRITE. */
	int		 active;	/* slot in use. */
} AIO_KQ_OP;

typedef struct __aio_kq_state {
	int		 kq;		/* kqueue descriptor. */
	AIO_KQ_OP	*ops;		/* in-flight table, depth entries. */
	u_int32_t	 depth;		/* table capacity. */
} AIO_KQ_STATE;

static int __aio_kq_submit __P((ENV *, DB_AIO_CONTEXT *, DB_AIO_OP *));
static int __aio_kq_reap __P((ENV *, DB_AIO_CONTEXT *, int, int));
static int __aio_kq_destroy __P((ENV *, DB_AIO_CONTEXT *));

static const DB_AIO_BACKEND __aio_kq_backend = {
	"kqueue-aio",
	__aio_kq_submit,
	__aio_kq_reap,
	NULL,				/* cancel: unused */
	__aio_kq_destroy
};

/*
 * __os_aio_kqueue_init --
 *	Create a kqueue and attach the backend to the context.
 *
 * PUBLIC: int __os_aio_kqueue_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_kqueue_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_KQ_STATE *st;
	int ret;

	if ((ret = __os_calloc(env, 1, sizeof(*st), &st)) != 0)
		return (ret);
	st->depth = ctx->depth == 0 ? DB_AIO_DEFAULT_DEPTH : ctx->depth;
	if ((st->kq = kqueue()) < 0) {
		ret = __os_get_errno();
		__os_free(env, st);
		return (ret);
	}
	if ((ret = __os_calloc(env,
	    st->depth, sizeof(AIO_KQ_OP), &st->ops)) != 0) {
		(void)close(st->kq);
		__os_free(env, st);
		return (ret);
	}
	ctx->priv = st;
	ctx->backend = &__aio_kq_backend;
	return (0);
}

/*
 * __aio_kq_submit --
 *	Fill a free slot's aiocb to post an EVFILT_AIO kevent on completion,
 *	then submit via aio_read/aio_write.  DB_OPNOTSUP if the table is full
 *	or the kernel refuses the op (caller writes the page synchronously).
 */
static int
__aio_kq_submit(env, ctx, aio)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	DB_AIO_OP *aio;
{
	AIO_KQ_STATE *st;
	AIO_KQ_OP *slot;
	u_int32_t i;
	int r;

	st = ctx->priv;
	for (slot = NULL, i = 0; i < st->depth; i++)
		if (!st->ops[i].active) {
			slot = &st->ops[i];
			break;
		}
	if (slot == NULL)
		return (DB_OPNOTSUP);

	memset(&slot->cb, 0, sizeof(slot->cb));
	slot->cb.aio_fildes = aio->fhp->fd;
	slot->cb.aio_buf = aio->buf;
	slot->cb.aio_nbytes = aio->pagesize;
	slot->cb.aio_offset = (off_t)aio->pgno * aio->pagesize;
	slot->cb.aio_sigevent.sigev_notify = SIGEV_KEVENT;
	slot->cb.aio_sigevent.sigev_notify_kqueue = st->kq;
	slot->cb.aio_sigevent.sigev_value.sival_ptr = slot;
	slot->cookie = aio->cookie;
	slot->done = aio->done;
	slot->len = aio->pagesize;
	slot->op = aio->op;

	r = aio->op == DB_IO_WRITE ?
	    aio_write(&slot->cb) : aio_read(&slot->cb);
	if (r != 0)
		return (DB_OPNOTSUP);

	slot->active = 1;
	ctx->inflight++;
	return (0);
}

/*
 * __aio_kq_finish_slot --
 *	Collect a completed slot's result, run its completion, free the slot.
 */
static void
__aio_kq_finish_slot(env, ctx, slot)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	AIO_KQ_OP *slot;
{
	ssize_t n;
	int err, io_ret;

	err = aio_error(&slot->cb);
	if (err == 0) {
		n = aio_return(&slot->cb);
		io_ret = (n == (ssize_t)slot->len) ? 0 : EIO;
	} else {
		(void)aio_return(&slot->cb);
		io_ret = err;
	}
	if (slot->done != NULL)
		slot->done(env, slot->cookie, io_ret);
	slot->active = 0;
	if (ctx->inflight != 0)
		ctx->inflight--;
}

/*
 * __aio_kq_reap --
 *	Reap up to "max" completions (max < 0 means all ready).  Drains
 *	EVFILT_AIO kevents; with "wait" set and ops outstanding, blocks in
 *	kevent until at least one completes.
 */
static int
__aio_kq_reap(env, ctx, max, wait)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	int max, wait;
{
	AIO_KQ_STATE *st;
	struct kevent evs[64];
	struct timespec zero;
	AIO_KQ_OP *slot;
	int batch, got, i, n;

	st = ctx->priv;
	got = 0;
	zero.tv_sec = 0;
	zero.tv_nsec = 0;

	for (;;) {
		if (max >= 0 && got >= max)
			break;
		batch = (int)(sizeof(evs) / sizeof(evs[0]));
		if (max >= 0 && (max - got) < batch)
			batch = max - got;

		/*
		 * Block only when asked to wait, nothing has been reaped yet,
		 * and ops are still outstanding; otherwise poll (zero timeout).
		 */
		n = kevent(st->kq, NULL, 0, evs, batch,
		    (wait && got == 0 && ctx->inflight != 0) ? NULL : &zero);
		if (n < 0) {
			if (errno == EINTR)
				continue;
			break;
		}
		if (n == 0)
			break;
		for (i = 0; i < n; i++) {
			slot = evs[i].udata;
			if (slot == NULL || !slot->active)
				continue;
			__aio_kq_finish_slot(env, ctx, slot);
			got++;
		}
	}
	return (got);
}

/*
 * __aio_kq_destroy --
 *	Drain all in-flight ops, close the kqueue, free state.
 */
static int
__aio_kq_destroy(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_KQ_STATE *st;

	if ((st = ctx->priv) == NULL)
		return (0);
	while (ctx->inflight != 0)
		(void)__aio_kq_reap(env, ctx, -1, 1);
	if (st->kq >= 0)
		(void)close(st->kq);
	__os_free(env, st->ops);
	__os_free(env, st);
	ctx->priv = NULL;
	return (0);
}

#else /* !HAVE_AIO_KQUEUE */

/*
 * __os_aio_kqueue_init --
 *	kqueue aio not configured.
 *
 * PUBLIC: int __os_aio_kqueue_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_kqueue_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	COMPQUIET(env, NULL);
	COMPQUIET(ctx, NULL);
	return (DB_OPNOTSUP);
}

#endif /* HAVE_AIO_KQUEUE */

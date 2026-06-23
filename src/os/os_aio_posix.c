/*-
 * See the file LICENSE for redistribution information.
 *
 * POSIX.1b asynchronous I/O backend for the os_aio abstraction.
 *
 * Uses the standard <aio.h> facility: aio_read/aio_write to submit and
 * aio_suspend + aio_error/aio_return to reap.  This is the native async
 * path on systems that provide a working POSIX aio implementation but no
 * other completion engine -- notably Solaris/illumos (libaio) -- and a
 * portable fallback ahead of the thread pool elsewhere.
 *
 * Per-context state holds a fixed table of in-flight slots (ctx->depth
 * entries); a submit that finds the table full reports DB_OPNOTSUP so the
 * caller writes that one page synchronously, exactly as it would when no
 * async backend is configured.  The buffer-pool writeback path never has
 * more than MEMP_AIO_WINDOW writes outstanding, so a reasonable depth keeps
 * the table from filling in practice.
 *
 * Built only when configured with POSIX aio support (HAVE_AIO_POSIX);
 * otherwise this file is an empty translation unit and contexts use the
 * synchronous fallback in os_aio.c.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/os_aio.h"

#ifdef HAVE_AIO_POSIX

#include <aio.h>
#include <errno.h>

/* One in-flight op: its control block plus the caller's completion info. */
typedef struct __aio_posix_op {
	struct aiocb	 cb;		/* kernel/library control block. */
	void		*cookie;	/* caller context (BH *). */
	db_aio_done_fn	 done;		/* completion callback. */
	u_int32_t	 len;		/* expected transfer length. */
	int		 op;		/* DB_IO_READ / DB_IO_WRITE. */
	int		 active;	/* slot in use. */
} AIO_POSIX_OP;

typedef struct __aio_posix_state {
	AIO_POSIX_OP	*ops;		/* in-flight table, depth entries. */
	u_int32_t	 depth;		/* table capacity. */
} AIO_POSIX_STATE;

static int __aio_posix_submit __P((ENV *, DB_AIO_CONTEXT *, DB_AIO_OP *));
static int __aio_posix_reap __P((ENV *, DB_AIO_CONTEXT *, int, int));
static int __aio_posix_destroy __P((ENV *, DB_AIO_CONTEXT *));

static const DB_AIO_BACKEND __aio_posix_backend = {
	"posixaio",
	__aio_posix_submit,
	__aio_posix_reap,
	NULL,				/* cancel: unused */
	__aio_posix_destroy
};

/*
 * __os_aio_posix_init --
 *	Attach the POSIX aio backend to a context.
 *
 * PUBLIC: int __os_aio_posix_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_posix_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_POSIX_STATE *st;
	int ret;

	if ((ret = __os_calloc(env, 1, sizeof(*st), &st)) != 0)
		return (ret);
	st->depth = ctx->depth == 0 ? DB_AIO_DEFAULT_DEPTH : ctx->depth;
	if ((ret = __os_calloc(env,
	    st->depth, sizeof(AIO_POSIX_OP), &st->ops)) != 0) {
		__os_free(env, st);
		return (ret);
	}
	ctx->priv = st;
	ctx->backend = &__aio_posix_backend;
	return (0);
}

/*
 * __aio_posix_submit --
 *	Find a free slot, fill its aiocb, and hand it to aio_read/aio_write.
 *	Returns DB_OPNOTSUP if the table is full (caller writes synchronously).
 */
static int
__aio_posix_submit(env, ctx, aio)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	DB_AIO_OP *aio;
{
	AIO_POSIX_STATE *st;
	AIO_POSIX_OP *slot;
	u_int32_t i;
	int r;

	st = ctx->priv;
	for (slot = NULL, i = 0; i < st->depth; i++)
		if (!st->ops[i].active) {
			slot = &st->ops[i];
			break;
		}
	if (slot == NULL)
		return (DB_OPNOTSUP);	/* table full: caller does it inline. */

	memset(&slot->cb, 0, sizeof(slot->cb));
	slot->cb.aio_fildes = aio->fhp->fd;
	slot->cb.aio_buf = aio->buf;
	slot->cb.aio_nbytes = aio->pagesize;
	slot->cb.aio_offset = (off_t)aio->pgno * aio->pagesize;
	slot->cb.aio_sigevent.sigev_notify = SIGEV_NONE;
	slot->cookie = aio->cookie;
	slot->done = aio->done;
	slot->len = aio->pagesize;
	slot->op = aio->op;

	r = aio->op == DB_IO_WRITE ?
	    aio_write(&slot->cb) : aio_read(&slot->cb);
	if (r != 0)
		return (DB_OPNOTSUP);	/* submit refused: caller does it. */

	slot->active = 1;
	ctx->inflight++;
	return (0);
}

/*
 * __aio_posix_finish_slot --
 *	Collect one completed slot's result, run its completion, free the slot.
 */
static void
__aio_posix_finish_slot(env, ctx, slot, aio_err)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	AIO_POSIX_OP *slot;
	int aio_err;
{
	ssize_t n;
	int io_ret;

	/*
	 * aio_error returns 0 on success, an errno on failure.  On success
	 * aio_return yields the byte count and must be called exactly once to
	 * release the kernel/library resources; treat a short transfer as EIO.
	 */
	if (aio_err == 0) {
		n = aio_return(&slot->cb);
		io_ret = (n == (ssize_t)slot->len) ? 0 : EIO;
	} else {
		(void)aio_return(&slot->cb);
		io_ret = aio_err;
	}
	if (slot->done != NULL)
		slot->done(env, slot->cookie, io_ret);
	slot->active = 0;
	if (ctx->inflight != 0)
		ctx->inflight--;
}

/*
 * __aio_posix_reap --
 *	Reap up to "max" completions (max < 0 means all ready).  When "wait"
 *	is set and nothing is ready, block in aio_suspend on the in-flight set.
 */
static int
__aio_posix_reap(env, ctx, max, wait)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	int max, wait;
{
	AIO_POSIX_STATE *st;
	const struct aiocb **list;
	u_int32_t i;
	int err, got, nlist;

	st = ctx->priv;
	got = 0;

	for (;;) {
		if (max >= 0 && got >= max)
			break;

		/* Collect every still-active slot that has finished. */
		nlist = 0;
		for (i = 0; i < st->depth; i++) {
			if (!st->ops[i].active)
				continue;
			err = aio_error(&st->ops[i].cb);
			if (err == EINPROGRESS)
				continue;
			__aio_posix_finish_slot(env, ctx, &st->ops[i], err);
			if (++got >= max && max >= 0)
				return (got);
		}

		if (got > 0 || !wait || ctx->inflight == 0)
			break;

		/*
		 * Nothing was ready but the caller asked to wait and ops are
		 * outstanding: build the in-flight aiocb list and block until
		 * at least one finishes, then loop to harvest it.
		 */
		if ((__os_malloc(env,
		    st->depth * sizeof(*list), &list)) != 0)
			break;
		for (nlist = 0, i = 0; i < st->depth; i++)
			if (st->ops[i].active)
				list[nlist++] = &st->ops[i].cb;
		if (nlist == 0) {
			__os_free(env, list);
			break;
		}
		while (aio_suspend(list, nlist, NULL) != 0 && errno == EINTR)
			;
		__os_free(env, list);
	}
	return (got);
}

/*
 * __aio_posix_destroy --
 *	Drain all in-flight ops, then free the context state.
 */
static int
__aio_posix_destroy(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_POSIX_STATE *st;

	if ((st = ctx->priv) == NULL)
		return (0);
	while (ctx->inflight != 0)
		(void)__aio_posix_reap(env, ctx, -1, 1);
	__os_free(env, st->ops);
	__os_free(env, st);
	ctx->priv = NULL;
	return (0);
}

#else /* !HAVE_AIO_POSIX */

/*
 * __os_aio_posix_init --
 *	POSIX aio not configured.
 *
 * PUBLIC: int __os_aio_posix_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_posix_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	COMPQUIET(env, NULL);
	COMPQUIET(ctx, NULL);
	return (DB_OPNOTSUP);
}

#endif /* HAVE_AIO_POSIX */

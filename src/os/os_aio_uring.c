/*-
 * See the file LICENSE for redistribution information.
 *
 * Linux io_uring backend for the os_aio abstraction.
 *
 * The submit/reap mechanics (io_uring_prep_read/write, SQE acquisition
 * with a submit-and-retry on a full ring, and the CQE drain that maps
 * each completion back to its op) are adapted from the XTC Project's
 * libxtc (src/io/io_uring.c, ISC License) with the author's permission;
 * the readiness/poll machinery there is dropped since the buffer pool
 * only needs file reads/writes.
 *
 * Built only when configured with io_uring support (HAVE_IO_URING);
 * otherwise this file is an empty translation unit and contexts use
 * the synchronous fallback in os_aio.c.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/os_aio.h"

#ifdef HAVE_IO_URING

#include <liburing.h>

typedef struct __aio_uring_state {
	struct io_uring	ring;
} AIO_URING_STATE;

/* Per-op completion record; user_data points here until reaped. */
typedef struct __aio_uring_op {
	void		*cookie;
	db_aio_done_fn	 done;
	u_int32_t	 len;		/* expected transfer (read/write). */
	int		 op;		/* DB_IO_READ / DB_IO_WRITE. */
} AIO_URING_OP;

static int __aio_uring_submit __P((ENV *, DB_AIO_CONTEXT *, DB_AIO_OP *));
static int __aio_uring_reap __P((ENV *, DB_AIO_CONTEXT *, int, int));
static int __aio_uring_destroy __P((ENV *, DB_AIO_CONTEXT *));

static const DB_AIO_BACKEND __aio_uring_backend = {
	"io_uring",
	__aio_uring_submit,
	__aio_uring_reap,
	NULL,				/* cancel: unused */
	__aio_uring_destroy
};

/*
 * __os_aio_uring_init --
 *	Bring up an io_uring and attach the backend to the context.
 */
int
__os_aio_uring_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_URING_STATE *st;
	int ret;

	if ((ret = __os_calloc(env, 1, sizeof(*st), &st)) != 0)
		return (ret);
	if (io_uring_queue_init((unsigned)ctx->depth, &st->ring, 0) < 0) {
		ret = __os_get_errno();
		__os_free(env, st);
		return (ret);
	}
	ctx->priv = st;
	ctx->backend = &__aio_uring_backend;
	return (0);
}

/*
 * __aio_uring_get_sqe --
 *	Get an SQE, flushing the ring once if it is momentarily full.
 */
static struct io_uring_sqe *
__aio_uring_get_sqe(st)
	AIO_URING_STATE *st;
{
	struct io_uring_sqe *sqe;

	if ((sqe = io_uring_get_sqe(&st->ring)) == NULL) {
		(void)io_uring_submit(&st->ring);
		sqe = io_uring_get_sqe(&st->ring);
	}
	return (sqe);
}

static int
__aio_uring_submit(env, ctx, aio)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	DB_AIO_OP *aio;
{
	AIO_URING_STATE *st;
	AIO_URING_OP *rec;
	struct io_uring_sqe *sqe;
	off_t off;
	int ret;

	st = ctx->priv;
	if ((sqe = __aio_uring_get_sqe(st)) == NULL)
		return (EAGAIN);

	off = (off_t)aio->pgno * aio->pagesize;
	switch (aio->op) {
	case DB_IO_READ:
		io_uring_prep_read(sqe, aio->fhp->fd,
		    aio->buf, aio->pagesize, (unsigned long long)off);
		break;
	case DB_IO_WRITE:
		io_uring_prep_write(sqe, aio->fhp->fd,
		    aio->buf, aio->pagesize, (unsigned long long)off);
		break;
	default:
		return (EINVAL);
	}

	if ((ret = __os_calloc(env, 1, sizeof(*rec), &rec)) != 0)
		return (ret);
	rec->cookie = aio->cookie;
	rec->done = aio->done;
	rec->len = aio->pagesize;
	rec->op = aio->op;
	io_uring_sqe_set_data(sqe, rec);

	(void)io_uring_submit(&st->ring);
	ctx->inflight++;
	return (0);
}

/*
 * __aio_uring_reap --
 *	Drain up to "max" completions (max < 0 means all currently ready),
 *	invoking each op's completion callback.  If "wait" is set and ops
 *	are outstanding, block for at least one completion first.
 */
static int
__aio_uring_reap(env, ctx, max, wait)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	int max, wait;
{
	AIO_URING_STATE *st;
	AIO_URING_OP *rec;
	struct io_uring_cqe *cqe;
	int got, ret;

	st = ctx->priv;
	got = 0;

	if (wait && ctx->inflight != 0) {
		if (io_uring_wait_cqe(&st->ring, &cqe) < 0)
			return (got);
	} else if (io_uring_peek_cqe(&st->ring, &cqe) != 0)
		return (got);

	for (;;) {
		rec = io_uring_cqe_get_data(cqe);
		if (rec != NULL) {
			/*
			 * cqe->res is the byte count (>= 0) or a negative
			 * errno; a short transfer is treated as an I/O error.
			 */
			if (cqe->res < 0)
				ret = -cqe->res;
			else
				ret = ((u_int32_t)cqe->res == rec->len) ?
				    0 : EIO;
			if (rec->done != NULL)
				rec->done(env, rec->cookie, ret);
			__os_free(env, rec);
			if (ctx->inflight != 0)
				ctx->inflight--;
			got++;
		}
		io_uring_cqe_seen(&st->ring, cqe);

		if (max >= 0 && got >= max)
			break;
		if (io_uring_peek_cqe(&st->ring, &cqe) != 0)
			break;
	}
	return (got);
}

static int
__aio_uring_destroy(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_URING_STATE *st;

	if ((st = ctx->priv) != NULL) {
		/* Drain any stragglers so their records are freed. */
		(void)__aio_uring_reap(env, ctx, -1, 0);
		io_uring_queue_exit(&st->ring);
		__os_free(env, st);
		ctx->priv = NULL;
	}
	return (0);
}

#else /* !HAVE_IO_URING */

/*
 * __os_aio_uring_init --
 *	io_uring not configured; leave the context on the synchronous
 *	fallback.
 */
int
__os_aio_uring_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	COMPQUIET(env, NULL);
	COMPQUIET(ctx, NULL);
	return (DB_OPNOTSUP);
}

#endif /* HAVE_IO_URING */

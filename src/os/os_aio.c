/*-
 * See the file LICENSE for redistribution information.
 *
 * Asynchronous I/O abstraction -- generic layer + synchronous backend.
 *
 * The synchronous backend performs each submitted op inline and invokes its
 * completion immediately; it is the behaviour-preserving default and the
 * fallback when no platform async backend (io_uring/kqueue/IOCP/POSIX aio) is
 * available or enabled.  Platform backends plug in via DB_AIO_BACKEND.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/os_aio.h"

struct __db_aio_context {
	const DB_AIO_BACKEND *backend;	/* Active backend (NULL = synchronous). */
	void		*priv;		/* Backend-private state. */
	u_int32_t	 depth;		/* Requested queue depth. */
	u_int32_t	 inflight;	/* Ops submitted, not yet reaped. */
};

/*
 * __os_aio_create --
 *	Create an AIO context.  Selects a platform backend if one is available
 *	and enabled, else uses the synchronous fallback.
 */
int
__os_aio_create(env, depth, ctxp)
	ENV *env;
	u_int32_t depth;
	DB_AIO_CONTEXT **ctxp;
{
	DB_AIO_CONTEXT *ctx;
	int ret;

	*ctxp = NULL;
	if ((ret = __os_calloc(env, 1, sizeof(DB_AIO_CONTEXT), &ctx)) != 0)
		return (ret);
	ctx->depth = depth == 0 ? DB_AIO_DEFAULT_DEPTH : depth;
	ctx->backend = NULL;		/* synchronous fallback */
	ctx->priv = NULL;
	ctx->inflight = 0;

	/*
	 * A platform backend would be probed and installed here, e.g.
	 *	(void)__os_aio_uring_init(env, ctx);
	 * leaving ctx->backend NULL (synchronous) on failure.  Kept as the
	 * synchronous fallback until the io_uring backend lands.
	 */
	*ctxp = ctx;
	return (0);
}

/*
 * __os_aio_submit --
 *	Submit one op.  The synchronous backend performs it now and runs the
 *	completion inline; a real backend queues it for later reaping.
 */
int
__os_aio_submit(env, ctx, aio)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	DB_AIO_OP *aio;
{
	int ret;
	size_t nio;

	if (ctx->backend != NULL)
		return (ctx->backend->submit(env, ctx, aio));

	/* Synchronous fallback: do the I/O now, complete inline. */
	nio = 0;
	ret = __os_io(env, aio->op, aio->fhp, aio->pgno,
	    aio->pagesize, 0, aio->pagesize, (u_int8_t *)aio->buf, &nio);
	if (aio->done != NULL)
		aio->done(env, aio->cookie, ret);
	return (ret);
}

/*
 * __os_aio_reap --
 *	Reap up to "max" completions (max < 0 means all ready).  For the
 *	synchronous backend there is never anything outstanding.
 */
int
__os_aio_reap(env, ctx, max, wait)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	int max, wait;
{
	COMPQUIET(env, NULL);
	COMPQUIET(max, 0);
	COMPQUIET(wait, 0);
	if (ctx->backend != NULL)
		return (ctx->backend->reap(env, ctx, max, wait));
	return (0);
}

/*
 * __os_aio_destroy --
 *	Tear down an AIO context.
 */
int
__os_aio_destroy(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	int ret;

	ret = 0;
	if (ctx == NULL)
		return (0);
	if (ctx->backend != NULL && ctx->backend->destroy != NULL)
		ret = ctx->backend->destroy(env, ctx);
	__os_free(env, ctx);
	return (ret);
}

/*
 * __os_aio_available --
 *	Return 1 if a real (non-synchronous) async backend is active.
 */
int
__os_aio_available(env)
	ENV *env;
{
	COMPQUIET(env, NULL);
	return (0);	/* synchronous fallback only, for now */
}

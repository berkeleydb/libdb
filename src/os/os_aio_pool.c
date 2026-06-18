/*-
 * See the file LICENSE for redistribution information.
 *
 * Thread-pool offload backend for the os_aio abstraction.
 *
 * The portable async path for every platform that lacks a native file
 * completion engine (i.e. everything but Linux io_uring and Windows
 * IOCP): a small fixed pool of worker threads drains a submission FIFO,
 * performs each op with the normal synchronous __os_io, and posts the
 * result to a completion FIFO that __os_aio_reap drains.  Regular files
 * are not pollable, so this offload is exactly how libxtc (src/ptc/
 * blocking.c + aio.c, ISC) does file AIO off io_uring/IOCP; the pool
 * structure (worker loop, start-on-first-use, shutdown-join) is adapted
 * from it, with a completion queue + condvar replacing libxtc's
 * fiber-park wakeup.
 *
 * Built when configured with HAVE_AIO_THREADPOOL.  POSIX uses pthreads;
 * a Win32 path (CreateThread/CONDITION_VARIABLE) lands behind the same
 * surface.  Otherwise this is an empty translation unit and contexts
 * fall back to the synchronous path in os_aio.c.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/os_aio.h"

#ifdef HAVE_AIO_THREADPOOL

#include <pthread.h>

#define	AIO_POOL_THREADS	4

typedef struct __aio_pool_item {
	DB_AIO_OP		 op;	/* caller's op, copied. */
	int			 result;	/* 0 or errno. */
	struct __aio_pool_item	*next;
} AIO_POOL_ITEM;

typedef struct __aio_pool_state {
	ENV		*env;
	pthread_mutex_t	 lock;
	pthread_cond_t	 work_cv;	/* workers wait for submissions. */
	pthread_cond_t	 done_cv;	/* reap waits for completions. */
	AIO_POOL_ITEM	*sub_head, *sub_tail;	/* submission FIFO. */
	AIO_POOL_ITEM	*cmp_head, *cmp_tail;	/* completion FIFO. */
	pthread_t	 threads[AIO_POOL_THREADS];
	int		 nthreads;
	int		 stopping;
} AIO_POOL_STATE;

static int __aio_pool_submit __P((ENV *, DB_AIO_CONTEXT *, DB_AIO_OP *));
static int __aio_pool_reap __P((ENV *, DB_AIO_CONTEXT *, int, int));
static int __aio_pool_destroy __P((ENV *, DB_AIO_CONTEXT *));

static const DB_AIO_BACKEND __aio_pool_backend = {
	"threadpool",
	__aio_pool_submit,
	__aio_pool_reap,
	NULL,
	__aio_pool_destroy
};

/* FIFO helpers (caller holds st->lock). */
static void
__aio_q_push(head, tail, it)
	AIO_POOL_ITEM **head, **tail, *it;
{
	it->next = NULL;
	if (*tail != NULL)
		(*tail)->next = it;
	else
		*head = it;
	*tail = it;
}

static AIO_POOL_ITEM *
__aio_q_pop(head, tail)
	AIO_POOL_ITEM **head, **tail;
{
	AIO_POOL_ITEM *it;

	if ((it = *head) == NULL)
		return (NULL);
	if ((*head = it->next) == NULL)
		*tail = NULL;
	it->next = NULL;
	return (it);
}

static void *
__aio_pool_worker(arg)
	void *arg;
{
	AIO_POOL_STATE *st;
	AIO_POOL_ITEM *it;
	size_t nio;
	int ret;

	st = arg;
	for (;;) {
		(void)pthread_mutex_lock(&st->lock);
		while (st->sub_head == NULL && !st->stopping)
			(void)pthread_cond_wait(&st->work_cv, &st->lock);
		if (st->stopping && st->sub_head == NULL) {
			(void)pthread_mutex_unlock(&st->lock);
			return (NULL);
		}
		it = __aio_q_pop(&st->sub_head, &st->sub_tail);
		(void)pthread_mutex_unlock(&st->lock);

		/* Run the op synchronously on this worker thread. */
		nio = 0;
		ret = __os_io(st->env, it->op.op, it->op.fhp, it->op.pgno,
		    it->op.pagesize, 0, it->op.pagesize,
		    (u_int8_t *)it->op.buf, &nio);
		it->result = ret;

		(void)pthread_mutex_lock(&st->lock);
		__aio_q_push(&st->cmp_head, &st->cmp_tail, it);
		(void)pthread_cond_signal(&st->done_cv);
		(void)pthread_mutex_unlock(&st->lock);
	}
}

/*
 * __os_aio_pool_init --
 *	Bring up the worker pool and attach the backend to the context.
 *
 * PUBLIC: int __os_aio_pool_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_pool_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_POOL_STATE *st;
	int i, ret;

	if ((ret = __os_calloc(env, 1, sizeof(*st), &st)) != 0)
		return (ret);
	st->env = env;
	if (pthread_mutex_init(&st->lock, NULL) != 0)
		goto err0;
	if (pthread_cond_init(&st->work_cv, NULL) != 0)
		goto err1;
	if (pthread_cond_init(&st->done_cv, NULL) != 0)
		goto err2;

	for (i = 0; i < AIO_POOL_THREADS; i++) {
		if (pthread_create(&st->threads[i], NULL,
		    __aio_pool_worker, st) != 0)
			break;
		st->nthreads++;
	}
	if (st->nthreads == 0)
		goto err3;

	ctx->priv = st;
	ctx->backend = &__aio_pool_backend;
	return (0);

err3:	(void)pthread_cond_destroy(&st->done_cv);
err2:	(void)pthread_cond_destroy(&st->work_cv);
err1:	(void)pthread_mutex_destroy(&st->lock);
err0:	__os_free(env, st);
	return (ret == 0 ? DB_OPNOTSUP : ret);
}

static int
__aio_pool_submit(env, ctx, aio)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	DB_AIO_OP *aio;
{
	AIO_POOL_STATE *st;
	AIO_POOL_ITEM *it;
	int ret;

	st = ctx->priv;
	if ((ret = __os_calloc(env, 1, sizeof(*it), &it)) != 0)
		return (ret);
	it->op = *aio;			/* copy: caller's op may be transient. */
	it->result = 0;

	(void)pthread_mutex_lock(&st->lock);
	__aio_q_push(&st->sub_head, &st->sub_tail, it);
	ctx->inflight++;
	(void)pthread_cond_signal(&st->work_cv);
	(void)pthread_mutex_unlock(&st->lock);
	return (0);
}

/*
 * __aio_pool_reap --
 *	Drain completions, invoking each op's callback.  With wait set and
 *	ops outstanding, block until at least one completes.  Callbacks run
 *	without the lock held.
 */
static int
__aio_pool_reap(env, ctx, max, wait)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	int max, wait;
{
	AIO_POOL_STATE *st;
	AIO_POOL_ITEM *it;
	int got;

	st = ctx->priv;
	got = 0;

	(void)pthread_mutex_lock(&st->lock);
	if (wait)
		while (st->cmp_head == NULL && ctx->inflight != 0)
			(void)pthread_cond_wait(&st->done_cv, &st->lock);

	for (;;) {
		if (max >= 0 && got >= max)
			break;
		if ((it = __aio_q_pop(&st->cmp_head, &st->cmp_tail)) == NULL)
			break;
		if (ctx->inflight != 0)
			ctx->inflight--;
		(void)pthread_mutex_unlock(&st->lock);

		if (it->op.done != NULL)
			it->op.done(env, it->op.cookie, it->result);
		__os_free(env, it);
		got++;

		(void)pthread_mutex_lock(&st->lock);
	}
	(void)pthread_mutex_unlock(&st->lock);
	return (got);
}

static int
__aio_pool_destroy(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_POOL_STATE *st;
	AIO_POOL_ITEM *it;
	int i;

	if ((st = ctx->priv) == NULL)
		return (0);

	(void)pthread_mutex_lock(&st->lock);
	st->stopping = 1;
	(void)pthread_cond_broadcast(&st->work_cv);
	(void)pthread_mutex_unlock(&st->lock);
	for (i = 0; i < st->nthreads; i++)
		(void)pthread_join(st->threads[i], NULL);

	/* Discard any uncollected completions (and pending submissions). */
	while ((it = __aio_q_pop(&st->cmp_head, &st->cmp_tail)) != NULL)
		__os_free(env, it);
	while ((it = __aio_q_pop(&st->sub_head, &st->sub_tail)) != NULL)
		__os_free(env, it);

	(void)pthread_cond_destroy(&st->done_cv);
	(void)pthread_cond_destroy(&st->work_cv);
	(void)pthread_mutex_destroy(&st->lock);
	__os_free(env, st);
	ctx->priv = NULL;
	return (0);
}

#else /* !HAVE_AIO_THREADPOOL */

/*
 * __os_aio_pool_init --
 *	Thread-pool offload not configured.
 *
 * PUBLIC: int __os_aio_pool_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_pool_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	COMPQUIET(env, NULL);
	COMPQUIET(ctx, NULL);
	return (DB_OPNOTSUP);
}

#endif /* HAVE_AIO_THREADPOOL */

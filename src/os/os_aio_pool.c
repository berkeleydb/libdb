/*-
 * See the file LICENSE for redistribution information.
 *
 * Thread-pool offload backend for the os_aio abstraction.
 *
 * The portable async path for platforms without a native file completion
 * engine (everything but Linux io_uring and Windows IOCP).  A single,
 * process-wide pool of worker threads -- created lazily on the first
 * submitted op, not at context creation -- drains a global submission FIFO,
 * performs each op with the normal synchronous __os_io, and routes the result
 * to the submitting context's completion FIFO, which __os_aio_reap drains.
 *
 * Design notes:
 *   - Lazy + global: a context (one per environment) costs nothing until it
 *     actually issues an async write, and many environments share one pool
 *     rather than each spawning its own threads.
 *   - Fork-safe: a pthread_atfork child handler resets the pool to "unstarted"
 *     (the worker threads do not survive fork) so a child re-spawns lazily on
 *     its next submit and never deadlocks on an inherited-locked pool mutex.
 *   - The pool structure is adapted from libxtc (src/ptc/blocking.c, ISC).
 *
 * Built when configured with HAVE_AIO_THREADPOOL; otherwise an empty TU.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/os_aio.h"

#ifdef HAVE_AIO_THREADPOOL

#include <pthread.h>

#define	AIO_POOL_THREADS	4

/* One unit of work on the global pool. */
typedef struct __aio_work {
	ENV			*env;
	DB_AIO_OP		 op;		/* copied from the caller */
	int			 result;	/* 0 or errno */
	struct __aio_pool_state	*owner;		/* completion routed here */
	struct __aio_work	*next;
} AIO_WORK;

/* Per-context (per-environment) completion state. */
typedef struct __aio_pool_state {
	pthread_cond_t	 done_cv;		/* reap waits here */
	AIO_WORK	*cmp_head, *cmp_tail;	/* this context's completions */
} AIO_POOL_STATE;

/*
 * Process-wide lazy worker pool.  Shared by every context; reset by the
 * pthread_atfork child handler.  g_lock protects the submission FIFO and
 * every context's completion FIFO; each context has its own done_cv.
 */
static pthread_mutex_t	g_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t	g_work_cv = PTHREAD_COND_INITIALIZER;
static AIO_WORK	       *g_sub_head, *g_sub_tail;
static pthread_t	g_threads[AIO_POOL_THREADS];
static int		g_nthreads, g_started, g_stopping;
static pthread_once_t	g_atfork_once = PTHREAD_ONCE_INIT;

static AIO_WORK *
__aio_q_pop(head, tail)
	AIO_WORK **head, **tail;
{
	AIO_WORK *w;

	if ((w = *head) == NULL)
		return (NULL);
	if ((*head = w->next) == NULL)
		*tail = NULL;
	w->next = NULL;
	return (w);
}

static void
__aio_q_push(head, tail, w)
	AIO_WORK **head, **tail, *w;
{
	w->next = NULL;
	if (*tail != NULL)
		(*tail)->next = w;
	else
		*head = w;
	*tail = w;
}

static void *
__aio_pool_worker(arg)
	void *arg;
{
	AIO_WORK *w;
	size_t nio;

	COMPQUIET(arg, NULL);
	for (;;) {
		(void)pthread_mutex_lock(&g_lock);
		while (g_sub_head == NULL && !g_stopping)
			(void)pthread_cond_wait(&g_work_cv, &g_lock);
		if (g_stopping && g_sub_head == NULL) {
			(void)pthread_mutex_unlock(&g_lock);
			return (NULL);
		}
		w = __aio_q_pop(&g_sub_head, &g_sub_tail);
		(void)pthread_mutex_unlock(&g_lock);

		nio = 0;
		w->result = __os_io(w->env, w->op.op, w->op.fhp, w->op.pgno,
		    w->op.pagesize, 0, w->op.pagesize,
		    (u_int8_t *)w->op.buf, &nio);

		(void)pthread_mutex_lock(&g_lock);
		__aio_q_push(&w->owner->cmp_head, &w->owner->cmp_tail, w);
		(void)pthread_cond_signal(&w->owner->done_cv);
		(void)pthread_mutex_unlock(&g_lock);
	}
}

static void
__aio_atfork_prepare()
{
	(void)pthread_mutex_lock(&g_lock);
}

static void
__aio_atfork_parent()
{
	(void)pthread_mutex_unlock(&g_lock);
}

static void
__aio_atfork_child()
{
	/*
	 * The worker threads did not survive the fork.  Reset the pool to
	 * unstarted (re-init the lock we held across the fork, drop inherited
	 * submissions) so the child re-spawns lazily on its next submit and
	 * never blocks on a pool mutex no live thread will release.
	 */
	(void)pthread_mutex_init(&g_lock, NULL);
	(void)pthread_cond_init(&g_work_cv, NULL);
	g_sub_head = g_sub_tail = NULL;
	g_nthreads = 0;
	g_started = 0;
	g_stopping = 0;
}

static void
__aio_install_atfork()
{
	(void)pthread_atfork(__aio_atfork_prepare,
	    __aio_atfork_parent, __aio_atfork_child);
}

/* Start the pool on first use.  Caller holds g_lock.  Returns 0 on success. */
static int
__aio_pool_start_locked()
{
	int i;

	if (g_started)
		return (0);
	g_stopping = 0;
	g_nthreads = 0;
	for (i = 0; i < AIO_POOL_THREADS; i++) {
		if (pthread_create(&g_threads[i], NULL,
		    __aio_pool_worker, NULL) != 0)
			break;
		g_nthreads++;
	}
	if (g_nthreads == 0)
		return (-1);
	g_started = 1;
	return (0);
}

static int
__aio_pool_submit(env, ctx, aio)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	DB_AIO_OP *aio;
{
	AIO_POOL_STATE *st;
	AIO_WORK *w;
	int ret;

	(void)pthread_once(&g_atfork_once, __aio_install_atfork);
	st = ctx->priv;
	if ((ret = __os_calloc(env, 1, sizeof(*w), &w)) != 0)
		return (ret);
	w->env = env;
	w->op = *aio;			/* caller's op may be transient. */
	w->result = 0;
	w->owner = st;

	(void)pthread_mutex_lock(&g_lock);
	if (__aio_pool_start_locked() != 0) {
		(void)pthread_mutex_unlock(&g_lock);
		__os_free(env, w);
		return (DB_OPNOTSUP);	/* caller writes synchronously */
	}
	__aio_q_push(&g_sub_head, &g_sub_tail, w);
	ctx->inflight++;
	(void)pthread_cond_signal(&g_work_cv);
	(void)pthread_mutex_unlock(&g_lock);
	return (0);
}

static int
__aio_pool_reap(env, ctx, max, wait)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	int max, wait;
{
	AIO_POOL_STATE *st;
	AIO_WORK *w;
	int got;

	st = ctx->priv;
	got = 0;

	(void)pthread_mutex_lock(&g_lock);
	if (wait)
		while (st->cmp_head == NULL && ctx->inflight != 0)
			(void)pthread_cond_wait(&st->done_cv, &g_lock);

	for (;;) {
		if (max >= 0 && got >= max)
			break;
		if ((w = __aio_q_pop(&st->cmp_head, &st->cmp_tail)) == NULL)
			break;
		if (ctx->inflight != 0)
			ctx->inflight--;
		(void)pthread_mutex_unlock(&g_lock);

		if (w->op.done != NULL)
			w->op.done(env, w->op.cookie, w->result);
		__os_free(env, w);
		got++;

		(void)pthread_mutex_lock(&g_lock);
	}
	(void)pthread_mutex_unlock(&g_lock);
	return (got);
}

static int
__aio_pool_destroy(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_POOL_STATE *st;
	AIO_WORK *w;

	if ((st = ctx->priv) == NULL)
		return (0);

	/* Drain any writes still in flight so no worker references st. */
	while (ctx->inflight != 0)
		(void)__aio_pool_reap(env, ctx, -1, 1);

	(void)pthread_mutex_lock(&g_lock);
	while ((w = __aio_q_pop(&st->cmp_head, &st->cmp_tail)) != NULL)
		__os_free(env, w);
	(void)pthread_mutex_unlock(&g_lock);

	(void)pthread_cond_destroy(&st->done_cv);
	__os_free(env, st);
	ctx->priv = NULL;
	return (0);
}

static const DB_AIO_BACKEND __aio_pool_backend = {
	"threadpool",
	__aio_pool_submit,
	__aio_pool_reap,
	NULL,
	__aio_pool_destroy
};

/*
 * __os_aio_pool_init --
 *	Attach the thread-pool backend to a context.  No worker threads are
 *	created here; the shared pool starts lazily on the first submit.
 *
 * PUBLIC: int __os_aio_pool_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_pool_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_POOL_STATE *st;
	int ret;

	if ((ret = __os_calloc(env, 1, sizeof(*st), &st)) != 0)
		return (ret);
	if (pthread_cond_init(&st->done_cv, NULL) != 0) {
		__os_free(env, st);
		return (DB_OPNOTSUP);
	}
	ctx->priv = st;
	ctx->backend = &__aio_pool_backend;
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

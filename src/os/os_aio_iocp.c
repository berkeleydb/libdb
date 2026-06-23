/*-
 * See the file LICENSE for redistribution information.
 *
 * Windows IOCP native file-AIO backend for the os_aio abstraction.
 *
 * Adapted from the XTC Project's libxtc (src/io/io_iocp.c, ISC, with
 * the author's permission): a pread/pwrite is an overlapped
 * ReadFile/WriteFile on a file HANDLE associated with a completion
 * port, and completions are dequeued in batch by
 * GetQueuedCompletionStatusEx -- no worker thread, no event handle.
 * The socket AFD-poll machinery in libxtc is dropped; only file I/O
 * is needed.
 *
 * Built only when configured with HAVE_IOCP (a Windows build).  NOTE:
 * this backend is a faithful adaptation but has NOT yet been compiled
 * or validated on a Windows host; the portable thread-pool backend
 * (os_aio_pool.c) is the validated Windows async path until then.
 * Otherwise this is an empty translation unit.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/os_aio.h"

#ifdef HAVE_IOCP

#include <windows.h>
#include <io.h>

typedef struct __aio_iocp_pend {
	OVERLAPPED	*ov;		/* kernel-owned while pending. */
	HANDLE		 fh;		/* file handle (for GetOverlappedResult). */
	void		*cookie;
	db_aio_done_fn	 done;
	u_int32_t	 len;		/* expected transfer. */
} AIO_IOCP_PEND;

typedef struct __aio_iocp_state {
	HANDLE		 iocp;		/* the completion port. */
	AIO_IOCP_PEND	*pend;		/* in-flight ops. */
	int		 n, cap;
} AIO_IOCP_STATE;

static int __aio_iocp_submit __P((ENV *, DB_AIO_CONTEXT *, DB_AIO_OP *));
static int __aio_iocp_reap __P((ENV *, DB_AIO_CONTEXT *, int, int));
static int __aio_iocp_destroy __P((ENV *, DB_AIO_CONTEXT *));

static const DB_AIO_BACKEND __aio_iocp_backend = {
	"iocp",
	__aio_iocp_submit,
	__aio_iocp_reap,
	NULL,
	__aio_iocp_destroy
};

/*
 * __os_aio_iocp_init --
 *	Create a completion port and attach the backend.
 *
 * PUBLIC: int __os_aio_iocp_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_iocp_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_IOCP_STATE *st;
	int ret;

	if ((ret = __os_calloc(env, 1, sizeof(*st), &st)) != 0)
		return (ret);
	st->iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
	if (st->iocp == NULL) {
		__os_free(env, st);
		return (__os_get_errno());
	}
	ctx->priv = st;
	ctx->backend = &__aio_iocp_backend;
	return (0);
}

static int
__aio_iocp_submit(env, ctx, aio)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	DB_AIO_OP *aio;
{
	AIO_IOCP_STATE *st;
	OVERLAPPED *ov;
	HANDLE fh;
	BOOL ok;
	DWORD err;
	__uint64_t off;
	int ret;

	st = ctx->priv;
	if (aio->op != DB_IO_READ && aio->op != DB_IO_WRITE)
		return (EINVAL);
	fh = aio->fhp->handle;
	if (fh == INVALID_HANDLE_VALUE)
		return (EINVAL);

	/* Associate the file with the port once; ignore "already done". */
	if (CreateIoCompletionPort(fh, st->iocp, 0, 0) == NULL) {
		err = GetLastError();
		if (err != ERROR_INVALID_PARAMETER)
			return (EAGAIN);
	}

	if ((ret = __os_calloc(env, 1, sizeof(*ov), &ov)) != 0)
		return (ret);
	off = (__uint64_t)aio->pgno * aio->pagesize;
	ov->Offset = (DWORD)(off & 0xFFFFFFFFu);
	ov->OffsetHigh = (DWORD)(off >> 32);

	if (aio->op == DB_IO_READ)
		ok = ReadFile(fh, aio->buf, (DWORD)aio->pagesize, NULL, ov);
	else
		ok = WriteFile(fh, aio->buf, (DWORD)aio->pagesize, NULL, ov);
	if (!ok && (err = GetLastError()) != ERROR_IO_PENDING) {
		__os_free(env, ov);
		return (EAGAIN);
	}

	if (st->n >= st->cap) {
		int nc = st->cap == 0 ? 16 : st->cap * 2;
		void *p = NULL;
		if ((ret = __os_realloc(env,
		    sizeof(*st->pend) * (size_t)nc, &p)) != 0) {
			(void)CancelIoEx(fh, ov);
			__os_free(env, ov);
			return (ret);
		}
		st->pend = p;
		st->cap = nc;
	}
	st->pend[st->n].ov = ov;
	st->pend[st->n].fh = fh;
	st->pend[st->n].cookie = aio->cookie;
	st->pend[st->n].done = aio->done;
	st->pend[st->n].len = aio->pagesize;
	st->n++;
	ctx->inflight++;
	return (0);
}

static int
__aio_iocp_reap(env, ctx, max, wait)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	int max, wait;
{
	AIO_IOCP_STATE *st;
	OVERLAPPED_ENTRY batch[64];
	ULONG n_done, j;
	DWORD tmo, nbytes;
	int got, i, bmax, ret;

	st = ctx->priv;
	got = 0;
	tmo = wait ? INFINITE : 0;
	bmax = (int)(sizeof(batch) / sizeof(batch[0]));
	if (max >= 0 && max < bmax)
		bmax = max;

	if (!GetQueuedCompletionStatusEx(st->iocp, batch,
	    (ULONG)bmax, &n_done, tmo, FALSE))
		return (got);			/* WAIT_TIMEOUT or error. */

	for (j = 0; j < n_done; j++) {
		OVERLAPPED *ov = batch[j].lpOverlapped;
		for (i = 0; i < st->n; i++)
			if (st->pend[i].ov == ov)
				break;
		if (i == st->n)
			continue;		/* unknown completion. */

		nbytes = 0;
		if (GetOverlappedResult(st->pend[i].fh, ov, &nbytes, FALSE))
			ret = (nbytes == st->pend[i].len) ? 0 : EIO;
		else
			ret = EIO;
		if (st->pend[i].done != NULL)
			st->pend[i].done(env, st->pend[i].cookie, ret);
		__os_free(env, ov);

		st->n--;
		if (i != st->n)
			st->pend[i] = st->pend[st->n];
		if (ctx->inflight != 0)
			ctx->inflight--;
		got++;
	}
	return (got);
}

static int
__aio_iocp_destroy(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	AIO_IOCP_STATE *st;
	int i;

	if ((st = ctx->priv) == NULL)
		return (0);
	for (i = 0; i < st->n; i++) {
		(void)CancelIoEx(st->pend[i].fh, st->pend[i].ov);
		__os_free(env, st->pend[i].ov);
	}
	if (st->pend != NULL)
		__os_free(env, st->pend);
	if (st->iocp != NULL)
		(void)CloseHandle(st->iocp);
	__os_free(env, st);
	ctx->priv = NULL;
	return (0);
}

#else /* !HAVE_IOCP */

/*
 * __os_aio_iocp_init --
 *	IOCP not configured (non-Windows or thread-pool preferred).
 *
 * PUBLIC: int __os_aio_iocp_init __P((ENV *, DB_AIO_CONTEXT *));
 */
int
__os_aio_iocp_init(env, ctx)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
{
	COMPQUIET(env, NULL);
	COMPQUIET(ctx, NULL);
	return (DB_OPNOTSUP);
}

#endif /* HAVE_IOCP */

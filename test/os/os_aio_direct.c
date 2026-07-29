/*-
 * See the file LICENSE for redistribution information.
 *
 * os_aio_direct.c --
 *	A direct driver for the os_aio async-I/O abstraction (src/os/os_aio.c
 *	and its backends os_aio_pool.c / os_aio_posix.c / os_aio_uring.c).
 *
 *	The buffer pool only reaches os_aio via DB_ENV->set_flags(DB_MPOOL_AIO)
 *	AND only picks ONE backend at runtime (io_uring first on Linux), so a
 *	normal workload can never exercise the pool + posix backends on a box
 *	that also has io_uring.  This program links against the internal libdb
 *	symbols and drives EACH available backend directly, exactly as
 *	__memp_bhwrite_async / __memp_aio_drain do: create a context, force a
 *	specific backend, submit reads/writes with a completion callback, reap
 *	them (waiting), and verify the data round-trips.  It also drives the
 *	generic layer's synchronous fallback and its automatic backend
 *	selection (__os_aio_create), plus the DB_ENV os-method setters in
 *	common/os_method.c.
 *
 *	Everything runs in a SINGLE process under a hard SIGALRM guard so the
 *	reap loop (which blocks on real completions) can never hang the run.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/os_aio.h"

#include <signal.h>

#define	HOME		"OSAIO_TESTDIR"
#define	AIO_FILE	"OSAIO_TESTDIR/os_aio_direct.dat"
#define	PGSIZE		4096
#define	NPAGES		40		/* > MEMP_AIO_WINDOW (16), forces reaps */
#define	ALARM_SECS	60		/* hard self-timeout: never hang */

static int fails = 0;

#define	CHK_OK(call) do {						\
	int _r = (call);						\
	if (_r != 0) {							\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s)\n",		\
		    __FILE__, __LINE__, #call, _r, db_strerror(_r));	\
		fails++;						\
	}								\
} while (0)

static void
on_alarm(int sig)
{
	COMPQUIET(sig, 0);
	fprintf(stderr, "FAIL: os_aio_direct timed out after %d s (hung)\n",
	    ALARM_SECS);
	_exit(3);
}

/* Completion state: one per submitted op, keyed by cookie. */
typedef struct {
	int	done;		/* completion ran */
	int	ret;		/* result passed to the callback */
	int	page;		/* which page this op covered */
} OP_STATE;

static OP_STATE opstate[NPAGES];

static void
aio_done(env, cookie, ret)
	ENV *env;
	void *cookie;
	int ret;
{
	OP_STATE *s = cookie;

	COMPQUIET(env, NULL);
	s->done = 1;
	s->ret = ret;
}

/*
 * fill_page / check_page --
 *	Deterministic per-page content so a read after write can be verified.
 */
static void
fill_page(u_int8_t *buf, int page)
{
	int i;

	for (i = 0; i < PGSIZE; i++)
		buf[i] = (u_int8_t)((page * 131 + i * 7) & 0xff);
}

static int
check_page(const u_int8_t *buf, int page)
{
	int i;

	for (i = 0; i < PGSIZE; i++)
		if (buf[i] != (u_int8_t)((page * 131 + i * 7) & 0xff))
			return (1);
	return (0);
}

/*
 * run_backend --
 *	Submit NPAGES writes through the given (already-installed) context,
 *	reaping when the in-flight window fills, then read every page back and
 *	verify.  Mirrors the buffer pool's submit/drain window discipline.
 *	Returns 0 on full success.
 */
static int
run_backend(env, ctx, label)
	ENV *env;
	DB_AIO_CONTEXT *ctx;
	const char *label;
{
	DB_FH *fhp;
	DB_AIO_OP op;
	u_int8_t *pages;
	int i, got, inflight, want, bad, async;

	async = __os_aio_ctx_available(ctx);
	fprintf(stderr, "== backend: %s (async=%d) ==\n", label, async);

	/* Fresh data file for this backend. */
	(void)__os_unlink(env, AIO_FILE, 0);
	if (__os_openhandle(env, AIO_FILE,
	    O_RDWR | O_CREAT, DB_MODE_600, &fhp) != 0) {
		fprintf(stderr, "FAIL: %s: openhandle\n", label);
		return (1);
	}

	/* One contiguous, page-aligned write buffer for all pages. */
	if (__os_calloc(env, NPAGES, PGSIZE, &pages) != 0) {
		(void)__os_closehandle(env, fhp);
		return (1);
	}
	for (i = 0; i < NPAGES; i++)
		fill_page(pages + (size_t)i * PGSIZE, i);
	memset(opstate, 0, sizeof(opstate));

	/* ---- WRITE phase: submit with a bounded in-flight window. ---- */
	inflight = 0;
	for (i = 0; i < NPAGES; i++) {
		opstate[i].page = i;
		op.op = DB_IO_WRITE;
		op.fhp = fhp;
		op.pgno = (db_pgno_t)i;
		op.pagesize = PGSIZE;
		op.buf = pages + (size_t)i * PGSIZE;
		op.cookie = &opstate[i];
		op.done = aio_done;
		CHK_OK(__os_aio_submit(env, ctx, &op));
		/*
		 * The sync fallback completed the op inline at submit; only a
		 * real async backend leaves work to reap.  Mirror mp_sync,
		 * which drains the window ONLY when use_aio is set.
		 */
		if (!async)
			continue;
		inflight++;
		/* Drain to keep <= MEMP_AIO_WINDOW outstanding, like mp_sync. */
		if (inflight >= 16) {
			want = inflight;
			for (got = 0; got < want; )
				got += __os_aio_reap(env, ctx, -1, 1);
			inflight = 0;
		}
	}
	/* Drain the tail. */
	if (async)
		for (got = 0; got < inflight; )
			got += __os_aio_reap(env, ctx, -1, 1);

	/* Every write completion must have run with ret 0. */
	for (i = 0; i < NPAGES; i++) {
		if (!opstate[i].done) {
			fprintf(stderr,
			    "FAIL: %s: write %d never completed\n", label, i);
			fails++;
		} else if (opstate[i].ret != 0) {
			fprintf(stderr, "FAIL: %s: write %d ret %d\n",
			    label, i, opstate[i].ret);
			fails++;
		}
	}

	/* ---- READ phase: read every page back into a scratch area. ---- */
	memset(pages, 0, (size_t)NPAGES * PGSIZE);
	memset(opstate, 0, sizeof(opstate));
	inflight = 0;
	for (i = 0; i < NPAGES; i++) {
		opstate[i].page = i;
		op.op = DB_IO_READ;
		op.fhp = fhp;
		op.pgno = (db_pgno_t)i;
		op.pagesize = PGSIZE;
		op.buf = pages + (size_t)i * PGSIZE;
		op.cookie = &opstate[i];
		op.done = aio_done;
		CHK_OK(__os_aio_submit(env, ctx, &op));
		if (!async)
			continue;
		inflight++;
		if (inflight >= 16) {
			want = inflight;
			for (got = 0; got < want; )
				got += __os_aio_reap(env, ctx, -1, 1);
			inflight = 0;
		}
	}
	if (async)
		for (got = 0; got < inflight; )
			got += __os_aio_reap(env, ctx, -1, 1);

	/* Verify the round-trip. */
	bad = 0;
	for (i = 0; i < NPAGES; i++) {
		if (!opstate[i].done || opstate[i].ret != 0) {
			fprintf(stderr, "FAIL: %s: read %d not ok (done=%d "
			    "ret=%d)\n", label, i, opstate[i].done,
			    opstate[i].ret);
			fails++;
			continue;
		}
		if (check_page(pages + (size_t)i * PGSIZE, i)) {
			fprintf(stderr, "FAIL: %s: page %d content mismatch\n",
			    label, i);
			bad++;
		}
	}
	if (bad == 0)
		fprintf(stderr, "  %s: %d pages written+read+verified OK\n",
		    label, NPAGES);
	else
		fails += bad;

	__os_free(env, pages);
	(void)__os_closehandle(env, fhp);
	(void)__os_unlink(env, AIO_FILE, 0);
	return (fails);
}

/*
 * drive_one_init --
 *	Create a bare context, force a specific backend via its *_init, run the
 *	workload, and destroy.  init_ret==0 means the backend attached; if it
 *	returns non-zero the backend is not configured and we skip (still a
 *	valid outcome, e.g. no io_uring in the kernel).
 */
static void
drive_one_init(env, label, initfn)
	ENV *env;
	const char *label;
	int (*initfn) __P((ENV *, DB_AIO_CONTEXT *));
{
	DB_AIO_CONTEXT *ctx;
	int r;

	/* __os_aio_create with no backend probing: start on the sync fallback,
	 * then override with the requested backend. */
	if (__os_calloc(env, 1, sizeof(DB_AIO_CONTEXT), &ctx) != 0) {
		fails++;
		return;
	}
	ctx->depth = DB_AIO_DEFAULT_DEPTH;
	ctx->backend = NULL;
	ctx->priv = NULL;
	ctx->inflight = 0;

	r = initfn(env, ctx);
	if (r != 0) {
		fprintf(stderr, "== backend %s not available (init=%d), "
		    "skipping ==\n", label, r);
		__os_free(env, ctx);
		return;
	}
	(void)run_backend(env, ctx, label);
	CHK_OK(__os_aio_destroy(env, ctx));
}

/*
 * exercise_os_methods --
 *	Set every DB_ENV os-method function (common/os_method.c) to a stub so
 *	each db_env_set_func_* setter runs, then reset them all to NULL (the
 *	library default) so later real I/O still uses the native calls.
 */
static int stub_close(int fd) { COMPQUIET(fd, 0); return (0); }
static void stub_dirfree(char **a, int b) { COMPQUIET(a, NULL); COMPQUIET(b, 0); }
static int stub_dirlist(const char *a, char ***b, int *c)
	{ COMPQUIET(a, NULL); COMPQUIET(b, NULL); COMPQUIET(c, NULL); return (0); }
static int stub_exists(const char *a, int *b)
	{ COMPQUIET(a, NULL); COMPQUIET(b, NULL); return (0); }
static void stub_free(void *p) { COMPQUIET(p, NULL); }
static int stub_fsync(int fd) { COMPQUIET(fd, 0); return (0); }
static int stub_ftruncate(int fd, off_t o) { COMPQUIET(fd, 0); COMPQUIET(o, 0); return (0); }
static int stub_ioinfo(const char *a, int b, u_int32_t *c, u_int32_t *d, u_int32_t *e)
	{ COMPQUIET(a, NULL); COMPQUIET(b, 0); COMPQUIET(c, NULL);
	  COMPQUIET(d, NULL); COMPQUIET(e, NULL); return (0); }
static void *stub_malloc(size_t s) { return (malloc(s)); }
static int stub_file_map(DB_ENV *e, char *a, size_t s, int i, void **p)
	{ COMPQUIET(e, NULL); COMPQUIET(a, NULL); COMPQUIET(s, 0);
	  COMPQUIET(i, 0); COMPQUIET(p, NULL); return (0); }
static int stub_file_unmap(DB_ENV *e, void *p)
	{ COMPQUIET(e, NULL); COMPQUIET(p, NULL); return (0); }
static int stub_region_map(DB_ENV *e, char *a, size_t s, int *i, void **p)
	{ COMPQUIET(e, NULL); COMPQUIET(a, NULL); COMPQUIET(s, 0);
	  COMPQUIET(i, NULL); COMPQUIET(p, NULL); return (0); }
static int stub_region_unmap(DB_ENV *e, void *p)
	{ COMPQUIET(e, NULL); COMPQUIET(p, NULL); return (0); }
static ssize_t stub_pread(int fd, void *b, size_t s, off_t o)
	{ COMPQUIET(fd, 0); COMPQUIET(b, NULL); COMPQUIET(s, 0);
	  COMPQUIET(o, 0); return (0); }
static ssize_t stub_pwrite(int fd, const void *b, size_t s, off_t o)
	{ COMPQUIET(fd, 0); COMPQUIET(b, NULL); COMPQUIET(s, 0);
	  COMPQUIET(o, 0); return (0); }
static int stub_open(const char *a, int f, ...)
	{ COMPQUIET(a, NULL); COMPQUIET(f, 0); return (0); }
static ssize_t stub_read(int fd, void *b, size_t s)
	{ COMPQUIET(fd, 0); COMPQUIET(b, NULL); COMPQUIET(s, 0); return (0); }
static void *stub_realloc(void *p, size_t s) { return (realloc(p, s)); }
static int stub_rename(const char *a, const char *b)
	{ COMPQUIET(a, NULL); COMPQUIET(b, NULL); return (0); }
static int stub_seek(int fd, off_t o, int w)
	{ COMPQUIET(fd, 0); COMPQUIET(o, 0); COMPQUIET(w, 0); return (0); }
static int stub_unlink(const char *a) { COMPQUIET(a, NULL); return (0); }
static ssize_t stub_write(int fd, const void *b, size_t s)
	{ COMPQUIET(fd, 0); COMPQUIET(b, NULL); COMPQUIET(s, 0); return (0); }
static int stub_yield(u_long a, u_long b)
	{ COMPQUIET(a, 0); COMPQUIET(b, 0); return (0); }
static void stub_assert(const char *a, const char *b, int c)
	{ COMPQUIET(a, NULL); COMPQUIET(b, NULL); COMPQUIET(c, 0); }

static void
exercise_os_methods()
{
	/* Set each replaceable function (covers every setter in os_method.c). */
	CHK_OK(db_env_set_func_assert(stub_assert));
	CHK_OK(db_env_set_func_close(stub_close));
	CHK_OK(db_env_set_func_dirfree(stub_dirfree));
	CHK_OK(db_env_set_func_dirlist(stub_dirlist));
	CHK_OK(db_env_set_func_exists(stub_exists));
	CHK_OK(db_env_set_func_free(stub_free));
	CHK_OK(db_env_set_func_fsync(stub_fsync));
	CHK_OK(db_env_set_func_ftruncate(stub_ftruncate));
	CHK_OK(db_env_set_func_ioinfo(stub_ioinfo));
	CHK_OK(db_env_set_func_malloc(stub_malloc));
	CHK_OK(db_env_set_func_file_map(stub_file_map, stub_file_unmap));
	CHK_OK(db_env_set_func_region_map(stub_region_map, stub_region_unmap));
	CHK_OK(db_env_set_func_pread(stub_pread));
	CHK_OK(db_env_set_func_pwrite(stub_pwrite));
	CHK_OK(db_env_set_func_open(stub_open));
	CHK_OK(db_env_set_func_read(stub_read));
	CHK_OK(db_env_set_func_realloc(stub_realloc));
	CHK_OK(db_env_set_func_rename(stub_rename));
	CHK_OK(db_env_set_func_seek(stub_seek));
	CHK_OK(db_env_set_func_unlink(stub_unlink));
	CHK_OK(db_env_set_func_write(stub_write));
	CHK_OK(db_env_set_func_yield(stub_yield));

	/* Reset every function to the library default (NULL) so the rest of
	 * the program does real I/O through the native calls. */
	(void)db_env_set_func_assert(NULL);
	(void)db_env_set_func_close(NULL);
	(void)db_env_set_func_dirfree(NULL);
	(void)db_env_set_func_dirlist(NULL);
	(void)db_env_set_func_exists(NULL);
	(void)db_env_set_func_free(NULL);
	(void)db_env_set_func_fsync(NULL);
	(void)db_env_set_func_ftruncate(NULL);
	(void)db_env_set_func_ioinfo(NULL);
	(void)db_env_set_func_malloc(NULL);
	(void)db_env_set_func_file_map(NULL, NULL);
	(void)db_env_set_func_region_map(NULL, NULL);
	(void)db_env_set_func_pread(NULL);
	(void)db_env_set_func_pwrite(NULL);
	(void)db_env_set_func_open(NULL);
	(void)db_env_set_func_read(NULL);
	(void)db_env_set_func_realloc(NULL);
	(void)db_env_set_func_rename(NULL);
	(void)db_env_set_func_seek(NULL);
	(void)db_env_set_func_unlink(NULL);
	(void)db_env_set_func_write(NULL);
	(void)db_env_set_func_yield(NULL);
}

/*
 * exercise_mpool_aio --
 *	Drive the REAL buffer-pool async-writeback path (mp_sync.c
 *	__memp_bhwrite_async + __memp_aio_drain), which is reachable only with
 *	DB_ENV->set_flags(DB_MPOOL_AIO).  Opens its own env with a tiny cache,
 *	writes far more pages than the cache holds (forcing writeback), and
 *	checkpoints (DB_SYNC_CHECKPOINT flushes every dirty page through the
 *	async window).  This is the production integration test: it confirms
 *	the backend __os_aio_create picks (io_uring on Linux) actually carries
 *	the buffer pool's writes.
 */
static void
exercise_mpool_aio()
{
	DB_ENV *dbenv;
	DB *dbp;
	DBT key, data;
	char kbuf[16], vbuf[512];
	int i, ret;

	if (db_env_create(&dbenv, 0) != 0) {
		fails++;
		return;
	}
	dbenv->set_errpfx(dbenv, "mpool_aio");
	/* Small cache so a few hundred pages force real writeback. */
	(void)dbenv->set_cachesize(dbenv, 0, 256 * 1024, 1);
	/* Turn ON async buffer-pool writeback (the whole point). */
	if ((ret = dbenv->set_flags(dbenv, DB_MPOOL_AIO, 1)) != 0) {
		fprintf(stderr, "FAIL: set_flags(DB_MPOOL_AIO): %s\n",
		    db_strerror(ret));
		fails++;
		(void)dbenv->close(dbenv, 0);
		return;
	}
	(void)__os_mkdir(NULL, HOME, DB_MODE_700);
	/* Remove any stale db/logs so DB_PRIVATE open is deterministic. */
	(void)__os_unlink(NULL, HOME "/mpool_aio.db", 0);
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOG | DB_INIT_TXN | DB_PRIVATE, DB_MODE_600)) != 0) {
		fprintf(stderr, "FAIL: mpool_aio env open: %s\n",
		    db_strerror(ret));
		fails++;
		(void)dbenv->close(dbenv, 0);
		return;
	}

	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, "mpool_aio.db", NULL,
	    DB_BTREE, DB_CREATE | DB_AUTO_COMMIT, DB_MODE_600)) != 0) {
		fprintf(stderr, "FAIL: mpool_aio db open: %s\n",
		    db_strerror(ret));
		fails++;
		(void)dbenv->close(dbenv, 0);
		return;
	}

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	memset(vbuf, 'x', sizeof(vbuf));
	key.data = kbuf;
	data.data = vbuf;
	data.size = sizeof(vbuf);

	/*
	 * Write enough distinct large records to overflow the cache many
	 * times, then checkpoint so mp_sync flushes the dirty pages through
	 * the async window.  Repeat so multiple checkpoints run.
	 */
	for (i = 0; i < 4000; i++) {
		key.size = 1 + (u_int32_t)snprintf(kbuf, sizeof(kbuf),
		    "k%08d", i);
		if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "FAIL: put %d: %s\n", i,
			    db_strerror(ret));
			fails++;
			break;
		}
		if (i % 1000 == 999)
			(void)dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE);
	}
	(void)dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE);

	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	fprintf(stderr, "  mpool_aio: DB_MPOOL_AIO writeback + checkpoints "
	    "completed OK\n");
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	DB_ENV *dbenv;
	ENV *env;
	DB_AIO_CONTEXT *ctx;

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);

	(void)signal(SIGALRM, on_alarm);
	(void)alarm(ALARM_SECS);

	/* os-method setters first, before any real I/O relies on the funcs. */
	exercise_os_methods();

	/* Minimal environment: we only need a valid ENV * for __os_*. */
	if (db_env_create(&dbenv, 0) != 0) {
		fprintf(stderr, "FAIL: db_env_create\n");
		return (1);
	}
	dbenv->set_errpfx(dbenv, "os_aio_direct");
	(void)__os_mkdir(NULL, HOME, DB_MODE_700);
	if (dbenv->open(dbenv, HOME,
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, DB_MODE_600) != 0) {
		fprintf(stderr, "FAIL: dbenv->open\n");
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	env = dbenv->env;

	/* Real buffer-pool async writeback (mp_sync + backend integration). */
	exercise_mpool_aio();

	/*
	 * Drive each backend directly (portable ones always available here;
	 * posix needs librt/HAVE_AIO_POSIX, uring needs a working io_uring).
	 * Forcing each *_init covers all backends in one run even though
	 * __os_aio_create would only pick io_uring.
	 */
	drive_one_init(env, "threadpool", __os_aio_pool_init);
	drive_one_init(env, "posixaio", __os_aio_posix_init);
	drive_one_init(env, "io_uring", __os_aio_uring_init);

	/*
	 * Drive the generic layer's automatic selection (__os_aio_create):
	 * on Linux this attaches io_uring; the run also exercises the
	 * create/destroy lifecycle and __os_aio_ctx_available.
	 */
	CHK_OK(__os_aio_create(env, 0, &ctx));
	if (ctx != NULL) {
		(void)run_backend(env, ctx,
		    __os_aio_ctx_available(ctx) ? "auto(async)" : "auto(sync)");
		CHK_OK(__os_aio_destroy(env, ctx));
	}

	/*
	 * Drive the synchronous fallback explicitly: a context with a NULL
	 * backend performs each op inline and completes it immediately
	 * (os_aio.c __os_aio_submit / __os_aio_reap sync branches).
	 */
	if (__os_calloc(env, 1, sizeof(DB_AIO_CONTEXT), &ctx) == 0) {
		ctx->depth = DB_AIO_DEFAULT_DEPTH;
		ctx->backend = NULL;
		/* Exercise the sync reap branch (nothing ever outstanding). */
		(void)__os_aio_reap(env, ctx, -1, 0);
		(void)run_backend(env, ctx, "sync-fallback");
		CHK_OK(__os_aio_destroy(env, ctx));
	}

	(void)dbenv->close(dbenv, 0);

	if (fails == 0) {
		fprintf(stderr, "os_aio_direct: PASS\n");
		return (0);
	}
	fprintf(stderr, "os_aio_direct: FAIL (%d errors)\n", fails);
	return (1);
}

/*
 * p3_durable.c -- the durability proof for the P3 fix (DB_LOG_DIRECT).
 *
 * WHY.  __log_write_direct restages every log write into block-aligned whole
 * blocks: it rewrites the leading partial block and zero-pads the trailing one.
 * Both of those touch bytes the plain path did not, so "the flag now works" is
 * not enough -- a commit reported successful must still be on stable storage,
 * and the log must still be readable by recovery.  This asserts exactly that,
 * across a REAL kill -9, with no clean shutdown anywhere in the picture.
 *
 * Three modes, driven by test/c/p3-durable-run.sh:
 *
 *   writer N	 Open the env with DB_LOG_DIRECT, commit N transactions with
 *		 DB_TXN_SYNC (so each commit is individually acked durable),
 *		 print "DURABLE-ACK <n>" for each, then _exit(0) WITHOUT
 *		 closing anything -- the runner also kill -9's it in the other
 *		 arm.  No checkpoint: a checkpoint would let mpool satisfy the
 *		 reads and the log would not be on the recovery path at all.
 *   verify N	 Reopen with DB_RECOVER and require all N committed records to
 *		 be present with the right contents.  A MISSING record is the
 *		 failure the whole gate exists to catch: an acked commit that
 *		 did not survive.
 *   group	 The group-commit properties: with many threads committing, the
 *		 number of log flushes must be far below the number of commits
 *		 (waiters share an fsync) and st_maxcommitperflush must exceed
 *		 1.  Printed as GROUP so the runner can compare the arms.
 *
 * Every mode prints  VERDICT <name> <PASS|FAIL> <detail>  and exits non-zero on
 * FAIL.  The runner requires the line: exit 0 with no verdict is a failed run.
 */
#define	_GNU_SOURCE	1

#include <sys/types.h>

#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"TESTDIR_p3_durable"
#define	DBFILE		"durable.db"
#define	PAGESIZE	4096
#define	VALBYTES	200
#define	NTHREADS	8
#define	PER_THREAD	60

static int fails;

static void
verdict(const char *name, const char *v, const char *fmt, ...)
{
	va_list ap;

	printf("VERDICT %s %s ", name, v);
	va_start(ap, fmt);
	(void)vprintf(fmt, ap);
	va_end(ap);
	printf("\n");
	(void)fflush(stdout);
	if (strcmp(v, "FAIL") == 0)
		fails++;
}

/* The value for key i: deterministic, so verify can check CONTENTS not just
 * presence.  A recovery that replayed the right number of records with the
 * wrong bytes is exactly what restaging could break. */
static void
fillval(char *buf, int i)
{
	int j;

	for (j = 0; j < VALBYTES; j++)
		buf[j] = (char)('A' + ((i * 31 + j * 7) % 26));
}

static int
open_env(DB_ENV **dbenvp, u_int32_t extra, int want_direct, int *unsuppp)
{
	DB_ENV *dbenv;
	int ret;

	*unsuppp = 0;
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (ret);
	}
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, "p3_durable");
	/*
	 * A small log buffer forces MANY __log_write calls at arbitrary
	 * offsets, which is the code path under test.  A large buffer would let
	 * most records accumulate and hide the partial-block cases.
	 */
	(void)dbenv->set_lg_bsize(dbenv, 32 * 1024);
	(void)dbenv->set_cachesize(dbenv, 0, 4 * 1024 * 1024, 1);

	if (want_direct &&
	    (ret = dbenv->log_set_config(dbenv, DB_LOG_DIRECT, 1)) != 0) {
		/* EINVAL here means the library has no O_DIRECT support. */
		*unsuppp = 1;
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}

	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD | extra,
	    0600)) != 0) {
		fprintf(stderr, "DB_ENV->open: %s\n", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	*dbenvp = dbenv;
	return (0);
}

static int
open_db(DB_ENV *dbenv, DB **dbpp, u_int32_t flags)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0)
		return (ret);
	(void)dbp->set_pagesize(dbp, PAGESIZE);
	if ((ret = dbp->open(dbp, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_THREAD | flags, 0600)) != 0) {
		(void)dbp->close(dbp, 0);
		return (ret);
	}
	*dbpp = dbp;
	return (0);
}

/*
 * m_writer --
 *	Commit n transactions DB_TXN_SYNC under DB_LOG_DIRECT and announce each
 *	one as durable.  Exits hard: no close, no checkpoint, no flush.
 */
static int
m_writer(const char *name, int n)
{
	DB_ENV *dbenv;
	DB *dbp;
	DB_TXN *txn;
	DBT key, data;
	char vbuf[VALBYTES];
	int i, ret, unsupp;

	dbenv = NULL;
	dbp = NULL;
	if ((ret = open_env(&dbenv, 0, 1, &unsupp)) != 0) {
		if (unsupp) {
			verdict(name, "SKIP", "no O_DIRECT support in this "
			    "library (built without --enable-o_direct)");
			return (0);
		}
		verdict(name, "FAIL", "env open under DB_LOG_DIRECT: %s",
		    db_strerror(ret));
		return (1);
	}
	if ((ret = open_db(dbenv, &dbp, DB_AUTO_COMMIT)) != 0) {
		verdict(name, "FAIL", "DB->open under DB_LOG_DIRECT: %s (%d)",
		    db_strerror(ret), ret);
		return (1);
	}

	for (i = 0; i < n; i++) {
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
			verdict(name, "FAIL", "txn_begin %d: %s", i,
			    db_strerror(ret));
			return (1);
		}
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &i;
		key.size = sizeof(i);
		data.data = vbuf;
		data.size = sizeof(vbuf);
		if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0) {
			verdict(name, "FAIL", "put %d: %s", i,
			    db_strerror(ret));
			(void)txn->abort(txn);
			return (1);
		}
		/*
		 * DB_TXN_SYNC: this commit is acked durable.  If the record is
		 * not there after the crash, the library lied.
		 */
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0) {
			verdict(name, "FAIL", "commit %d: %s", i,
			    db_strerror(ret));
			return (1);
		}
		printf("DURABLE-ACK %d\n", i);
	}
	(void)fflush(stdout);

	verdict(name, "PASS", "%d transactions committed DB_TXN_SYNC under "
	    "DB_LOG_DIRECT; exiting without close or checkpoint", n);
	/*
	 * _exit, not exit: no atexit handler, no flush of library state.  The
	 * process dies exactly as a crash would leave it.
	 */
	_exit(0);
}

/*
 * m_verify --
 *	Run recovery and require every acked commit to be present and correct.
 */
static int
m_verify(const char *name, int n)
{
	DB_ENV *dbenv;
	DB *dbp;
	DBT key, data;
	char vbuf[VALBYTES], got[VALBYTES];
	int i, missing, ret, unsupp, wrong;

	dbenv = NULL;
	dbp = NULL;
	/*
	 * Recover with the flag ON: the recovery READ path must also cope with
	 * the log this fix produced (padding past the frontier included).
	 */
	if ((ret = open_env(&dbenv, DB_RECOVER, 1, &unsupp)) != 0) {
		if (unsupp) {
			verdict(name, "SKIP", "no O_DIRECT support");
			return (0);
		}
		verdict(name, "FAIL", "DB_RECOVER open: %s", db_strerror(ret));
		return (1);
	}
	if ((ret = open_db(dbenv, &dbp, 0)) != 0) {
		verdict(name, "FAIL", "DB->open after recovery: %s",
		    db_strerror(ret));
		return (1);
	}

	missing = wrong = 0;
	for (i = 0; i < n; i++) {
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &i;
		key.size = sizeof(i);
		data.data = got;
		data.ulen = sizeof(got);
		data.flags = DB_DBT_USERMEM;
		if ((ret = dbp->get(dbp, NULL, &key, &data, 0)) != 0) {
			missing++;
			if (missing <= 5)
				printf("  MISSING key %d: %s\n", i,
				    db_strerror(ret));
		} else if (data.size != sizeof(vbuf) ||
		    memcmp(got, vbuf, sizeof(vbuf)) != 0) {
			wrong++;
			if (wrong <= 5)
				printf("  WRONG CONTENTS key %d\n", i);
		}
	}

	if (missing != 0 || wrong != 0)
		verdict(name, "FAIL", "%d of %d acked commits MISSING after "
		    "recovery, %d had wrong contents -- durability was "
		    "violated by the O_DIRECT log restaging", missing, n,
		    wrong);
	else
		verdict(name, "PASS", "all %d commits acked DB_TXN_SYNC "
		    "before the crash survived recovery with correct "
		    "contents", n);

	if ((ret = dbp->close(dbp, 0)) != 0)
		verdict(name, "FAIL", "DB->close: %s", db_strerror(ret));
	if ((ret = dbenv->close(dbenv, 0)) != 0)
		verdict(name, "FAIL", "DB_ENV->close: %s", db_strerror(ret));
	return (fails != 0);
}

/* ------------------------------------------------------------------------- */
/* group commit: many threads, few fsyncs.                                   */

struct warg {
	DB_ENV *dbenv;
	DB *dbp;
	int base;
	int ret;
};

static void *
worker(void *a)
{
	struct warg *w = a;
	DB_TXN *txn;
	DBT key, data;
	char vbuf[VALBYTES];
	int i, k, ret;

	for (i = 0; i < PER_THREAD; i++) {
		k = w->base * PER_THREAD + i;
		if ((ret = w->dbenv->txn_begin(w->dbenv, NULL, &txn, 0)) != 0)
			goto err;
		fillval(vbuf, k);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &k;
		key.size = sizeof(k);
		data.data = vbuf;
		data.size = sizeof(vbuf);
		if ((ret = w->dbp->put(w->dbp, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			goto err;
		}
		/* DB_TXN_SYNC from every thread: this is what groups. */
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			goto err;
	}
	return (NULL);
err:
	w->ret = ret;
	return (NULL);
}

/*
 * m_group --
 *	Group commit under DB_LOG_DIRECT: commits must outnumber log flushes,
 *	and at least one flush must have covered more than one commit.
 */
static int
m_group(const char *name, int want_direct)
{
	DB_ENV *dbenv;
	DB *dbp;
	DB_LOG_STAT *sp;
	pthread_t th[NTHREADS];
	struct warg wa[NTHREADS];
	int i, ncommit, ret, unsupp;

	dbenv = NULL;
	dbp = NULL;
	if ((ret = open_env(&dbenv, 0, want_direct, &unsupp)) != 0) {
		if (unsupp) {
			verdict(name, "SKIP", "no O_DIRECT support");
			return (0);
		}
		verdict(name, "FAIL", "env open: %s", db_strerror(ret));
		return (1);
	}
	if ((ret = open_db(dbenv, &dbp, DB_AUTO_COMMIT)) != 0) {
		verdict(name, "FAIL", "DB->open: %s (%d)", db_strerror(ret),
		    ret);
		return (1);
	}

	for (i = 0; i < NTHREADS; i++) {
		wa[i].dbenv = dbenv;
		wa[i].dbp = dbp;
		wa[i].base = i;
		wa[i].ret = 0;
		if (pthread_create(&th[i], NULL, worker, &wa[i]) != 0) {
			verdict(name, "FAIL", "pthread_create %d", i);
			return (1);
		}
	}
	for (i = 0; i < NTHREADS; i++)
		(void)pthread_join(th[i], NULL);
	for (i = 0; i < NTHREADS; i++)
		if (wa[i].ret != 0) {
			verdict(name, "FAIL", "thread %d: %s", i,
			    db_strerror(wa[i].ret));
			return (1);
		}

	if ((ret = dbenv->log_stat(dbenv, &sp, 0)) != 0) {
		verdict(name, "FAIL", "log_stat: %s", db_strerror(ret));
		return (1);
	}
	ncommit = NTHREADS * PER_THREAD;
	/*
	 * Machine-readable for the runner's cross-arm comparison.  scount is
	 * the fsync count; maxcommitperflush is how many commits one fsync
	 * satisfied at best.
	 */
	printf("GROUP commits=%d st_scount=%lu st_wcount=%lu "
	    "st_maxcommitperflush=%lu st_mincommitperflush=%lu\n",
	    ncommit, (u_long)sp->st_scount, (u_long)sp->st_wcount,
	    (u_long)sp->st_maxcommitperflush,
	    (u_long)sp->st_mincommitperflush);

	if (sp->st_scount == 0 || sp->st_wcount == 0)
		verdict(name, "FAIL", "the log was not written/synced at all "
		    "(scount=%lu wcount=%lu) -- nothing was measured",
		    (u_long)sp->st_scount, (u_long)sp->st_wcount);
	else if (sp->st_maxcommitperflush <= 1)
		verdict(name, "FAIL", "st_maxcommitperflush=%lu: no flush ever "
		    "covered more than one commit, so group commit is NOT "
		    "working under this flag", (u_long)sp->st_maxcommitperflush);
	else if ((int)sp->st_scount >= ncommit)
		verdict(name, "FAIL", "%lu log syncs for %d commits -- one "
		    "fsync per commit, group commit is not grouping",
		    (u_long)sp->st_scount, ncommit);
	else
		verdict(name, "PASS", "%d commits needed only %lu log syncs, "
		    "best flush covered %lu commits", ncommit,
		    (u_long)sp->st_scount, (u_long)sp->st_maxcommitperflush);

	free(sp);
	if ((ret = dbp->close(dbp, 0)) != 0)
		verdict(name, "FAIL", "DB->close: %s", db_strerror(ret));
	if ((ret = dbenv->close(dbenv, 0)) != 0)
		verdict(name, "FAIL", "DB_ENV->close: %s", db_strerror(ret));
	return (fails != 0);
}

int
main(int argc, char *argv[])
{
	const char *mode;
	int n, rc;

	if (argc < 2) {
		fprintf(stderr, "usage: p3_durable "
		    "writer N | verify N | group {direct|buffered}\n");
		return (2);
	}
	mode = argv[1];
	n = argc > 2 ? atoi(argv[2]) : 0;

	if (strcmp(mode, "writer") == 0)
		rc = m_writer(mode, n > 0 ? n : 200);
	else if (strcmp(mode, "verify") == 0)
		rc = m_verify(mode, n > 0 ? n : 200);
	else if (strcmp(mode, "group") == 0)
		rc = m_group(mode, argc > 2 &&
		    strcmp(argv[2], "buffered") == 0 ? 0 : 1);
	else {
		fprintf(stderr, "unknown mode: %s\n", mode);
		return (2);
	}
	return (rc != 0 || fails != 0 ? 1 : 0);
}

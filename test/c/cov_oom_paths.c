/*-
 * See the file LICENSE for redistribution information.
 *
 * cov_oom_paths.c --
 *	An OOM-error-path driver aimed at COVERAGE rather than at bug-finding.
 *
 *	Why a second driver next to fi_sweep.c.  test/faultinject/fi_sweep.c
 *	is the bug-finding sweep: for each K it forks a watchdogged child that
 *	`_exit()`s with a classification code.  `_exit()` deliberately skips
 *	atexit handlers -- which is exactly right for a watchdogged sweep, and
 *	exactly wrong for gcov, because gcov writes its .gcda from an atexit
 *	handler.  So fi_sweep's 947 failure points contribute ZERO measured
 *	coverage even though they execute an enormous number of error-return
 *	branches.  Report #3's branch gap is dominated by those branches:
 *	6,016 of the 38,998 missing branches sit on `if ((ret = f()) != 0)`
 *	lines, i.e. the error propagation an OOM is the easiest way to trigger.
 *
 *	This driver closes that measurement gap WITHOUT touching fi_sweep.c:
 *	  * it runs each K in a forked child like fi_sweep does (so a crash or
 *	    hang at one failure point cannot wedge the sweep), but the child
 *	    calls __gcov_dump() before _exit(), so the .gcda for that child's
 *	    execution is merged into the accumulation;
 *	  * it sweeps a workload chosen for BREADTH of subsystem rather than
 *	    for depth of any one -- env open, btree/hash/recno/queue/heap
 *	    opens, cursors, secondary indices, join, bulk, compaction, txn
 *	    commit/abort/prepare, checkpoint, verify, stat, upgrade, backup --
 *	    because each distinct code path that an OOM can interrupt is a
 *	    different set of error-return branches;
 *	  * it does NOT assert on crashes.  Report-only.  fi_sweep is the
 *	    correctness gate for OOM behaviour and it already documents 5
 *	    crashes + a family of leaks as known engine bugs (README.md).
 *	    Re-failing on those here would make the coverage run red for bugs
 *	    another PR owns, so this driver classifies and reports them and
 *	    exits 0 unless the sweep could not run at all.
 *
 *	Requires --enable-faultinject (HAVE_FAULT_INJECT).  Without it the
 *	injection hook compiles to constant 0 and the driver prints SKIP and
 *	exits 0, so it is safe to run unconditionally from a coverage driver.
 */
#include "db_config.h"

#include "db_int.h"

#include <signal.h>
#include <sys/wait.h>

#ifdef HAVE_FAULT_INJECT
#include "fi_alloc.h"
#endif

/*
 * gcov's dump entry point.  Declared rather than #included: gcov.h is not
 * always present, and this is the documented symbol (-fprofile-arcs provides
 * it).  Weak so a non-coverage build links fine.
 */
#if defined(__GNUC__)
extern void __gcov_dump(void) __attribute__((weak));
#else
static void (*__gcov_dump)(void) = NULL;
#endif

#define	HOME		"COVOOM_TESTDIR"
#define	ALARM_SECS	20		/* per-child watchdog */
#define	TOTAL_SECS	1800		/* whole-sweep guard */

static void
clean_home()
{
	/*
	 * Every artifact, not just *.db: queue extent files (__dbq.<db>.<n>)
	 * are not matched by *.db, and a leftover extent whose page LSNs are
	 * ahead of the fresh log makes the next open fail with DB_RUNRECOVERY
	 * (BDB2506).  That matters doubly here: this function runs before
	 * EVERY swept K, so a stale extent would poison the rest of the sweep.
	 */
	(void)system("rm -f " HOME "/__db.* " HOME "/__dbq.* "
	    HOME "/log.* " HOME "/*.db " HOME "/*.pag " HOME "/*.bak "
	    "2>/dev/null");
	(void)system("mkdir -p " HOME);
}

/*
 * A secondary-index key extractor: makes the associate path real.
 */
static int
skey(sdbp, pkey, pdata, skeyp)
	DB *sdbp;
	const DBT *pkey, *pdata;
	DBT *skeyp;
{
	COMPQUIET(sdbp, NULL);
	COMPQUIET(pkey, NULL);
	memset(skeyp, 0, sizeof(*skeyp));
	/* Index on the first byte of the data. */
	if (pdata->size < 1)
		return (DB_DONOTINDEX);
	skeyp->data = pdata->data;
	skeyp->size = 1;
	return (0);
}

/*
 * workload --
 *	A BREADTH-first workload: touch as many distinct subsystems as
 *	possible with as few allocations each as possible, so a sweep over K
 *	lands failures in many different functions rather than deep inside
 *	one.  Returns the first non-zero error it sees (or 0).
 *
 *	Every step is tolerant: once an OOM has been injected, later steps are
 *	expected to fail too, and we keep going so the teardown paths run.
 */
static int
workload()
{
	DB_ENV *dbenv;
	DB *dbp, *sdbp, *db2;
	DBC *dbc, *jc, *carray[2];
	DB_TXN *txn, *child;
	DBT key, data, skeydbt;
	DB_COMPACT cdata;
	DB_LOGC *logc;
	DB_LSN lsn;
	DB_MPOOLFILE *mpf;
	DB_BTREE_STAT *bstat;
	void *statp;
	u_int8_t gid[DB_GID_SIZE];
	char kbuf[32];
	int first_err, ret, i;
	u_int32_t a;

	first_err = 0;
#define	NOTE(r) do { if ((r) != 0 && first_err == 0) first_err = (r); } while (0)

	/* ---- env create + open (the region-setup allocations). */
	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (ret);
	dbenv->set_errfile(dbenv, NULL);
	dbenv->set_msgfile(dbenv, NULL);
	(void)dbenv->set_cachesize(dbenv, 0, 1024 * 1024, 1);
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0600)) != 0) {
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}

	/* ---- a btree, populated under a txn. */
	dbp = NULL;
	NOTE(ret = db_create(&dbp, dbenv, 0));
	if (dbp != NULL) {
		(void)dbp->set_pagesize(dbp, 512);
		NOTE(ret = dbenv->txn_begin(dbenv, NULL, &txn, 0));
		if (ret == 0) {
			NOTE(ret = dbp->open(dbp, txn, "oom_bt.db", NULL,
			    DB_BTREE, DB_CREATE, 0600));
			if (ret == 0) {
				memset(&key, 0, sizeof(key));
				memset(&data, 0, sizeof(data));
				for (i = 0; i < 60; i++) {
					(void)snprintf(kbuf, sizeof(kbuf),
					    "k%06d", i);
					key.data = kbuf;
					key.size = (u_int32_t)strlen(kbuf);
					data.data = kbuf;
					data.size = (u_int32_t)strlen(kbuf);
					if ((ret = dbp->put(dbp, txn, &key,
					    &data, 0)) != 0) {
						NOTE(ret);
						break;
					}
				}
				/* An overflow item: the big-page alloc path. */
				{
					char big[2048];
					memset(big, 'x', sizeof(big));
					key.data = "bigk"; key.size = 4;
					data.data = big;
					data.size = sizeof(big);
					NOTE(dbp->put(dbp, txn, &key,
					    &data, 0));
				}
			}
			/* Commit; an OOM here exercises txn commit teardown. */
			NOTE(txn->commit(txn, 0));
		}

		/* ---- a cursor walk (cursor alloc + page get per step). */
		if (dbp->cursor(dbp, NULL, &dbc, 0) == 0) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			while (dbc->get(dbc, &key, &data, DB_NEXT) == 0)
				;
			/* A duplicate cursor: __dbc_idup. */
			if (dbc->dup(dbc, &carray[0], DB_POSITION) == 0)
				(void)carray[0]->close(carray[0]);
			NOTE(dbc->close(dbc));
		}

		/* ---- bulk get: the DB_MULTIPLE_KEY buffer alloc. */
		if (dbp->cursor(dbp, NULL, &dbc, 0) == 0) {
			char *bulk;
			if ((bulk = malloc(65536)) != NULL) {
				memset(&key, 0, sizeof(key));
				memset(&data, 0, sizeof(data));
				data.data = bulk;
				data.ulen = 65536;
				data.flags = DB_DBT_USERMEM;
				(void)dbc->get(dbc, &key, &data,
				    DB_FIRST | DB_MULTIPLE_KEY);
				free(bulk);
			}
			(void)dbc->close(dbc);
		}

		/* ---- DB->stat + stat_print: the stat struct allocs. */
		bstat = NULL;
		if (dbp->stat(dbp, NULL, &bstat, 0) == 0 && bstat != NULL)
			__os_ufree(dbenv->env, bstat);
		(void)dbp->stat_print(dbp, 0);

		/* ---- DB->compact: the compaction working-set allocs. */
		memset(&cdata, 0, sizeof(cdata));
		(void)dbp->compact(dbp, NULL, NULL, NULL, &cdata,
		    DB_FREE_SPACE, NULL);

		/* ---- DB->verify needs its own handle (verify closes it). */
		(void)dbp->close(dbp, 0);
		dbp = NULL;
	}

	/* ---- a secondary index + a join: db_join.c's cursor array alloc. */
	dbp = sdbp = NULL;
	if (db_create(&dbp, dbenv, 0) == 0 &&
	    dbp->open(dbp, NULL, "oom_pri.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0 &&
	    db_create(&sdbp, dbenv, 0) == 0 &&
	    sdbp->set_flags(sdbp, DB_DUP) == 0 &&
	    sdbp->open(sdbp, NULL, "oom_sec.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0) {
		if (dbp->associate(dbp, NULL, sdbp, skey, DB_CREATE) == 0) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			for (i = 0; i < 20; i++) {
				(void)snprintf(kbuf, sizeof(kbuf),
				    "p%06d", i);
				key.data = kbuf;
				key.size = (u_int32_t)strlen(kbuf);
				data.data = kbuf;
				data.size = (u_int32_t)strlen(kbuf);
				NOTE(dbp->put(dbp, NULL, &key, &data, 0));
			}
			/* pget through the secondary. */
			memset(&skeydbt, 0, sizeof(skeydbt));
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			skeydbt.data = "p"; skeydbt.size = 1;
			(void)sdbp->pget(sdbp, NULL, &skeydbt, &key,
			    &data, 0);
			/* A join over one secondary: __db_join. */
			if (sdbp->cursor(sdbp, NULL, &dbc, 0) == 0) {
				memset(&skeydbt, 0, sizeof(skeydbt));
				memset(&data, 0, sizeof(data));
				skeydbt.data = "p"; skeydbt.size = 1;
				if (dbc->get(dbc, &skeydbt, &data,
				    DB_SET) == 0) {
					carray[0] = dbc;
					carray[1] = NULL;
					if (dbp->join(dbp, carray, &jc,
					    0) == 0)
						(void)jc->close(jc);
				}
				(void)dbc->close(dbc);
			}
		}
	}
	if (sdbp != NULL)
		(void)sdbp->close(sdbp, 0);
	if (dbp != NULL)
		(void)dbp->close(dbp, 0);

	/* ---- a hash DB (different access-method open + page code). */
	dbp = NULL;
	if (db_create(&dbp, dbenv, 0) == 0) {
		(void)dbp->set_pagesize(dbp, 512);
		if (dbp->open(dbp, NULL, "oom_h.db", NULL, DB_HASH,
		    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			for (i = 0; i < 40; i++) {
				(void)snprintf(kbuf, sizeof(kbuf),
				    "h%06d", i);
				key.data = kbuf;
				key.size = (u_int32_t)strlen(kbuf);
				data.data = kbuf;
				data.size = (u_int32_t)strlen(kbuf);
				NOTE(dbp->put(dbp, NULL, &key, &data, 0));
			}
		}
		(void)dbp->close(dbp, 0);
	}

	/* ---- a queue and a recno and a heap: the remaining AM opens. */
	dbp = NULL;
	if (db_create(&dbp, dbenv, 0) == 0) {
		(void)dbp->set_re_len(dbp, 16);
		if (dbp->open(dbp, NULL, "oom_q.db", NULL, DB_QUEUE,
		    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			data.data = "0123456789abcdef"; data.size = 16;
			for (i = 0; i < 20; i++)
				NOTE(dbp->put(dbp, NULL, &key, &data,
				    DB_APPEND));
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			(void)dbp->get(dbp, NULL, &key, &data, DB_CONSUME);
		}
		(void)dbp->close(dbp, 0);
	}
	dbp = NULL;
	if (db_create(&dbp, dbenv, 0) == 0) {
		if (dbp->open(dbp, NULL, "oom_r.db", NULL, DB_RECNO,
		    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			data.data = "rec"; data.size = 3;
			for (i = 0; i < 20; i++)
				NOTE(dbp->put(dbp, NULL, &key, &data,
				    DB_APPEND));
		}
		(void)dbp->close(dbp, 0);
	}
	dbp = NULL;
	if (db_create(&dbp, dbenv, 0) == 0) {
		if (dbp->open(dbp, NULL, "oom_hp.db", NULL, DB_HEAP,
		    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0) {
			DB_HEAP_RID rid;
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = &rid;
			key.size = key.ulen = sizeof(rid);
			key.flags = DB_DBT_USERMEM;
			data.data = "heap"; data.size = 4;
			for (i = 0; i < 20; i++)
				NOTE(dbp->put(dbp, NULL, &key, &data,
				    DB_APPEND));
		}
		(void)dbp->close(dbp, 0);
	}

	/* ---- an in-memory DB (NULL filename): the no-file open path. */
	dbp = NULL;
	if (db_create(&dbp, dbenv, 0) == 0) {
		if (dbp->open(dbp, NULL, NULL, NULL, DB_BTREE,
		    DB_CREATE, 0600) == 0) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = "m"; key.size = 1;
			data.data = "m"; data.size = 1;
			NOTE(dbp->put(dbp, NULL, &key, &data, 0));
		}
		(void)dbp->close(dbp, 0);
	}

	/* ---- a subdatabase inside a container file. */
	dbp = db2 = NULL;
	if (db_create(&dbp, dbenv, 0) == 0 &&
	    dbp->open(dbp, NULL, "oom_sub.db", "s1", DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0 &&
	    db_create(&db2, dbenv, 0) == 0)
		(void)db2->open(db2, NULL, "oom_sub.db", "s2", DB_BTREE,
		    DB_CREATE | DB_AUTO_COMMIT, 0600);
	if (db2 != NULL)
		(void)db2->close(db2, 0);
	if (dbp != NULL)
		(void)dbp->close(dbp, 0);

	/* ---- an ABORTED txn: the whole undo path. */
	dbp = NULL;
	if (db_create(&dbp, dbenv, 0) == 0 &&
	    dbp->open(dbp, NULL, "oom_abort.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0 &&
	    dbenv->txn_begin(dbenv, NULL, &txn, 0) == 0) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		for (i = 0; i < 30; i++) {
			(void)snprintf(kbuf, sizeof(kbuf), "a%06d", i);
			key.data = kbuf;
			key.size = (u_int32_t)strlen(kbuf);
			data.data = kbuf;
			data.size = (u_int32_t)strlen(kbuf);
			NOTE(dbp->put(dbp, txn, &key, &data, 0));
		}
		/* A nested child, also aborted: the nested-undo path. */
		if (dbenv->txn_begin(dbenv, txn, &child, 0) == 0) {
			key.data = "child"; key.size = 5;
			data.data = "c"; data.size = 1;
			NOTE(dbp->put(dbp, child, &key, &data, 0));
			NOTE(child->abort(child));
		}
		NOTE(txn->abort(txn));
	}
	if (dbp != NULL)
		(void)dbp->close(dbp, 0);

	/* ---- a PREPARED txn: the 2PC gid alloc + resolve. */
	dbp = NULL;
	if (db_create(&dbp, dbenv, 0) == 0 &&
	    dbp->open(dbp, NULL, "oom_prep.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0 &&
	    dbenv->txn_begin(dbenv, NULL, &txn, 0) == 0) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = "pp"; key.size = 2;
		data.data = "pp"; data.size = 2;
		NOTE(dbp->put(dbp, txn, &key, &data, 0));
		memset(gid, 0, sizeof(gid));
		memcpy(gid, "cov_oom_paths_gid_0", 19);
		if (txn->prepare(txn, gid) == 0)
			NOTE(txn->commit(txn, 0));
		else
			(void)txn->abort(txn);
	}
	if (dbp != NULL)
		(void)dbp->close(dbp, 0);

	/* ---- a log cursor walk: log_get's record buffer allocs. */
	logc = NULL;
	if (dbenv->log_cursor(dbenv, &logc, 0) == 0) {
		memset(&data, 0, sizeof(data));
		if (logc->get(logc, &lsn, &data, DB_FIRST) == 0)
			while (logc->get(logc, &lsn, &data, DB_NEXT) == 0)
				;
		(void)logc->close(logc, 0);
	}

	/* ---- a standalone mpool file: mp_fopen/mp_fget allocs. */
	mpf = NULL;
	if (dbenv->memp_fcreate(dbenv, &mpf, 0) == 0) {
		if (mpf->open(mpf, "oom_mp.pag", DB_CREATE, 0600, 4096) == 0) {
			db_pgno_t pgno = 0;
			void *pagep = NULL;
			if (mpf->get(mpf, &pgno, NULL, DB_MPOOL_CREATE,
			    &pagep) == 0)
				(void)mpf->put(mpf, pagep,
				    DB_PRIORITY_UNCHANGED, 0);
			(void)mpf->sync(mpf);
		}
		(void)mpf->close(mpf, 0);
	}

	/* ---- lock subsystem directly: locker + lock allocs. */
	{
		u_int32_t locker;
		DB_LOCK lock;
		DBT obj;
		if (dbenv->lock_id(dbenv, &locker) == 0) {
			memset(&obj, 0, sizeof(obj));
			obj.data = "oomlockobj"; obj.size = 10;
			if (dbenv->lock_get(dbenv, locker, 0, &obj,
			    DB_LOCK_WRITE, &lock) == 0)
				(void)dbenv->lock_put(dbenv, &lock);
			(void)dbenv->lock_id_free(dbenv, locker);
		}
		(void)dbenv->lock_detect(dbenv, 0, DB_LOCK_DEFAULT, &i);
	}

	/* ---- env-level stat allocs across every subsystem. */
	statp = NULL;
	if (dbenv->log_stat(dbenv, (DB_LOG_STAT **)&statp, 0) == 0 &&
	    statp != NULL)
		__os_ufree(dbenv->env, statp);
	statp = NULL;
	if (dbenv->lock_stat(dbenv, (DB_LOCK_STAT **)&statp, 0) == 0 &&
	    statp != NULL)
		__os_ufree(dbenv->env, statp);
	statp = NULL;
	if (dbenv->txn_stat(dbenv, (DB_TXN_STAT **)&statp, 0) == 0 &&
	    statp != NULL)
		__os_ufree(dbenv->env, statp);
	statp = NULL;
	if (dbenv->memp_stat(dbenv, (DB_MPOOL_STAT **)&statp, NULL, 0) == 0 &&
	    statp != NULL)
		__os_ufree(dbenv->env, statp);
	statp = NULL;
	if (dbenv->mutex_stat(dbenv, (DB_MUTEX_STAT **)&statp, 0) == 0 &&
	    statp != NULL)
		__os_ufree(dbenv->env, statp);
	(void)dbenv->stat_print(dbenv, DB_STAT_ALL);

	/* ---- log_archive: the file-name list alloc. */
	{
		char **flist = NULL;
		if (dbenv->log_archive(dbenv, &flist, DB_ARCH_ABS) == 0 &&
		    flist != NULL)
			__os_ufree(dbenv->env, flist);
	}

	/* ---- a checkpoint + a flush: the sync path. */
	NOTE(dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE));
	(void)dbenv->log_flush(dbenv, NULL);

	/* ---- get the open-flags back (cheap, exercises the getter). */
	(void)dbenv->get_open_flags(dbenv, &a);

	/* ---- teardown: env close runs the whole refresh path. */
	NOTE(dbenv->close(dbenv, 0));

	/* ---- a verify pass in a fresh handle-less env. */
	dbp = NULL;
	if (db_create(&dbp, NULL, 0) == 0) {
		dbp->set_errfile(dbp, NULL);
		(void)dbp->verify(dbp, HOME "/oom_bt.db", NULL, NULL, 0);
		/* verify() closes the handle itself; do not close again. */
	}

	return (first_err);
#undef NOTE
}

#ifdef HAVE_FAULT_INJECT
static void
on_child_alarm(sig)
	int sig;
{
	COMPQUIET(sig, 0);
	/*
	 * The child hung (a lock held on the OOM return path).  Flush what
	 * this child DID cover before dying, so a hang still contributes its
	 * partial coverage; then die by the signal the parent classifies.
	 */
	if (__gcov_dump != NULL)
		__gcov_dump();
	_exit(90);
}
#endif

int
main(argc, argv)
	int argc;
	char *argv[];
{
#ifndef HAVE_FAULT_INJECT
	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	printf("cov_oom_paths: SKIP (built without --enable-faultinject)\n");
	return (0);
#else
	long k, maxk, M, step;
	int status, ret;
	int n_ok, n_err, n_crash, n_hang;
	const char *p;
	pid_t pid;
	time_t t_start;

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);

	printf("cov_oom_paths: OOM error-path sweep (coverage-oriented)\n");

	/* --- Phase 1: baseline, injection off, measure M. */
	__db_fi_disarm();
	__db_fi_reset();
	clean_home();
	if ((ret = workload()) != 0) {
		fprintf(stderr, "cov_oom_paths: FATAL: baseline workload "
		    "failed with no injection: %d (%s)\n", ret,
		    db_strerror(ret));
		return (2);
	}
	M = __db_fi_count();
	printf("cov_oom_paths: baseline allocation count M = %ld\n", M);
	if (M <= 0) {
		printf("cov_oom_paths: SKIP (injection hook inactive)\n");
		return (0);
	}

	maxk = M;
	if ((p = getenv("COV_OOM_MAXK")) != NULL && atol(p) > 0)
		maxk = atol(p) < maxk ? atol(p) : maxk;

	/*
	 * A stride lets the sweep be bounded on a slow (-O0 --coverage) build
	 * while still spreading failure points across the whole workload.
	 * COV_OOM_STRIDE=1 is the exhaustive sweep.
	 */
	step = 1;
	if ((p = getenv("COV_OOM_STRIDE")) != NULL && atol(p) > 0)
		step = atol(p);

	printf("cov_oom_paths: sweeping K = 1..%ld step %ld "
	    "(per-child watchdog %ds)\n", maxk, step, ALARM_SECS);

	n_ok = n_err = n_crash = n_hang = 0;
	t_start = time(NULL);
	for (k = 1; k <= maxk; k += step) {
		if (time(NULL) - t_start > TOTAL_SECS) {
			printf("cov_oom_paths: total time budget reached at "
			    "K=%ld\n", k);
			break;
		}
		fflush(stdout);
		fflush(stderr);
		if ((pid = fork()) < 0) {
			perror("fork");
			break;
		}
		if (pid == 0) {
			/* Child. */
			(void)signal(SIGALRM, on_child_alarm);
			(void)alarm(ALARM_SECS);
			clean_home();
			__db_fi_arm(k);
			ret = workload();
			/*
			 * THE POINT: flush this child's arc counts into the
			 * .gcda before _exit, which fi_sweep.c cannot do
			 * without changing its classification contract.
			 */
			if (__gcov_dump != NULL)
				__gcov_dump();
			_exit(ret == 0 ? 0 : 1);
		}
		if (waitpid(pid, &status, 0) < 0) {
			perror("waitpid");
			break;
		}
		if (WIFSIGNALED(status)) {
			n_crash++;
			continue;
		}
		if (!WIFEXITED(status)) {
			n_crash++;
			continue;
		}
		switch (WEXITSTATUS(status)) {
		case 0:  n_ok++;    break;
		case 1:  n_err++;   break;
		case 90: n_hang++;  break;
		default: n_crash++; break;
		}
	}

	printf("cov_oom_paths: swept %d failure points: %d tolerated, "
	    "%d clean error, %d crashed, %d hung\n",
	    n_ok + n_err + n_crash + n_hang, n_ok, n_err, n_crash, n_hang);
	if (n_crash != 0 || n_hang != 0)
		printf("cov_oom_paths: NOTE: crashes/hangs on OOM paths are "
		    "KNOWN engine bugs owned by test/faultinject/README.md; "
		    "this driver measures coverage and does not gate on "
		    "them.\n");
	printf("cov_oom_paths: PASS (coverage sweep completed)\n");
	return (0);
#endif /* HAVE_FAULT_INJECT */
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * mvcc_mtx_doublefree.c -- F6 regression.
 *
 * __mutex_failchk walks the mutex region BY SLOT INDEX and frees every
 * DB_MUTEX_PROCESS_ONLY mutex whose owning process has died
 * (mut_failchk.c:69).  It cannot clear the owner's db_mutex_t, because it does
 * not know who points at the slot -- so a structure still holding the id keeps
 * a stale, non-INVALID reference.
 *
 * In a DB_PRIVATE environment every mutex is forced PROCESS_ONLY
 * (mut_alloc.c:43), so failchk reclaims a TXN_DETAIL's mvcc_mtx, and
 * __txn_env_refresh's snapshot sweep (txn_region.c:517, same shape at :597)
 * frees it a SECOND time when the environment closes:
 *
 *     BDB0059 assert failure: mut_alloc.c/245:
 *         "F_ISSET(mutexp, DB_MUTEX_ALLOCATED)"
 *       __os_abort <- __mutex_free_int <- __txn_env_refresh
 *                  <- __env_refresh <- __env_close
 *
 * The production reading is the worse one: with DB_ASSERT compiled out there is
 * no abort, and the slot is linked into the mutex free list TWICE -- silent
 * corruption of the allocator, surfacing later as a wrong mutex handed to an
 * unrelated subsystem.
 *
 * This reproduces it deterministically: a child opens the environment with
 * DB_FAILCHK and an is_alive callback, starts an MVCC transaction (which is
 * what allocates mvcc_mtx), and dies with it open.  The parent then runs
 * DB_ENV->failchk -- which prints BDB2017 per reclaimed slot -- and closes the
 * environment, where the second free happens.
 *
 * VERDICT line, not rc: the failure mode is an abort inside env->close, so the
 * runner has to distinguish "aborted" from "ran and disagreed".
 */
#include "db_config.h"

#include "db_int.h"

#include <sys/wait.h>
#include <signal.h>

#define	HOME	"TESTDIR_f6"
#define	DBFILE	"f6.db"

/*
 * Report every pid as dead EXCEPT our own, so failchk reclaims the child's
 * slots while leaving the parent's alone.  Returning 0 for everything would
 * have failchk reclaim the parent's own mutexes mid-run.
 */
static int
my_isalive(dbenv, pid, tid, flags)
	DB_ENV *dbenv;
	pid_t pid;
	db_threadid_t tid;
	u_int32_t flags;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(tid, 0);
	COMPQUIET(flags, 0);

	return (pid == getpid() ? 1 : 0);
}

static int
open_env(dbenvp, extra)
	DB_ENV **dbenvp;
	u_int32_t extra;
{
	DB_ENV *dbenv;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (ret);
	dbenv->set_errfile(dbenv, stderr);
	(void)dbenv->set_thread_count(dbenv, 16);
	(void)dbenv->set_isalive(dbenv, my_isalive);
	/*
	 * DB_PRIVATE is what makes every mutex PROCESS_ONLY and so reachable by
	 * failchk; without it this defect does not reproduce.  DB_MULTIVERSION
	 * on the database is what allocates mvcc_mtx.
	 */
	if ((ret = dbenv->open(dbenv, HOME,
	    DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_PRIVATE | DB_THREAD | DB_FAILCHK | extra,
	    0600)) != 0) {
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	*dbenvp = dbenv;
	return (0);
}

int
main(int argc, char *argv[])
{
	DB *dbp;
	DB_ENV *dbenv;
	DB_TXN *txn;
	DBT key, data;
	pid_t kid;
	int ret, status;

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);

	if ((ret = open_env(&dbenv, DB_RECOVER)) != 0) {
		printf("VERDICT f6_mvcc_doublefree SKIP env open: %s\n",
		    db_strerror(ret));
		return (0);
	}
	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION | DB_THREAD,
	    0600)) != 0) {
		printf("VERDICT f6_mvcc_doublefree SKIP db open: %s\n",
		    db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (0);
	}
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)"k"; key.size = 2;
	data.data = (void *)"v"; data.size = 2;
	(void)dbp->put(dbp, NULL, &key, &data, 0);
	(void)dbp->close(dbp, 0);

	/*
	 * The child must DIE with an MVCC snapshot transaction open, so its
	 * TXN_DETAIL keeps a live mvcc_mtx for failchk to reclaim.
	 */
	(void)fflush(NULL);
	if ((kid = fork()) == 0) {
		DB_ENV *cenv;
		DB *cdbp;

		if (open_env(&cenv, 0) != 0)
			_exit(2);
		if (db_create(&cdbp, cenv, 0) != 0)
			_exit(2);
		if (cdbp->open(cdbp, NULL, DBFILE, NULL, DB_BTREE,
		    DB_AUTO_COMMIT | DB_MULTIVERSION | DB_THREAD, 0600) != 0)
			_exit(2);
		if (cenv->txn_begin(cenv, NULL, &txn, DB_TXN_SNAPSHOT) != 0)
			_exit(2);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = (void *)"k"; key.size = 2;
		data.data = (void *)"child"; data.size = 6;
		/*
		 * It must be a WRITE.  mvcc_mtx is allocated lazily in
		 * __memp_fget (mp_fget.c:267) only when the fetch is dirty or
		 * creating -- a read-only get under a snapshot allocates
		 * nothing, so an earlier version of this test left no mvcc_mtx
		 * behind and PASSED EVEN WITH THE GUARD REMOVED.  Verified by
		 * counting BDB2017 lines from failchk: 0 with a get, non-zero
		 * with a put.
		 */
		(void)cdbp->put(cdbp, txn, &key, &data, 0);
		(void)fflush(NULL);
		(void)raise(SIGKILL);
		_exit(0);
	}
	(void)waitpid(kid, &status, 0);
	if (!WIFSIGNALED(status)) {
		printf("VERDICT f6_mvcc_doublefree SKIP child exited %d "
		    "without being killed; DB_TXN_SNAPSHOT or DB_MULTIVERSION "
		    "is unavailable, so no mvcc_mtx was left behind\n",
		    WIFEXITED(status) ? WEXITSTATUS(status) : -1);
		(void)dbenv->close(dbenv, 0);
		return (0);
	}

	/* Reclaims the dead child's PROCESS_ONLY mutexes, mvcc_mtx among them. */
	if ((ret = dbenv->failchk(dbenv, 0)) != 0) {
		printf("VERDICT f6_mvcc_doublefree FAIL failchk: %s\n",
		    db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (1);
	}

	/*
	 * THE DEFECT IS HERE.  __env_refresh -> __txn_env_refresh sweeps the
	 * snapshot list and frees mvcc_mtx a second time.  Without the guard in
	 * __mutex_free this aborts; the runner reports a signal death as the F6
	 * reproduction.
	 */
	if ((ret = dbenv->close(dbenv, 0)) != 0) {
		printf("VERDICT f6_mvcc_doublefree FAIL env close: %s\n",
		    db_strerror(ret));
		return (1);
	}

	printf("VERDICT f6_mvcc_doublefree PASS failchk + env close survived "
	    "a reclaimed mvcc_mtx without a double free\n");
	return (0);
}

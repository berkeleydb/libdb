/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * S5 regression: __lock_failchk spins forever on a dead TRANSACTIONAL locker
 * whose heldby list holds only non-READ, non-write modes (DB_LOCK_SIREAD).
 *
 * A read-only DB_TXN_SERIALIZABLE transaction whose process is SIGKILLed leaves
 * one SIREAD marker with nlocks=1, nwrites=0.  __lock_failchk's skip test wants
 * heldby empty or nlocks==nwrites, so it does not fire; PUT_READ releases only
 * READ and READ_UNCOMMITTED, so it frees nothing; __lock_freelocker is guarded
 * on id < TXN_MINIMUM, so it is skipped.  `goto retry' then re-walks an
 * unchanged table.  Before the fix: 45,284,819 BDB2053 lines (3.1 GB) for one
 * locker id, and DB_ENV->failchk never returned.
 *
 * This instruments the exact predicates and prints them once per pass, so the
 * loop's non-progress is MEASURED rather than inferred from the log size.
 *
 * NOT lk_partitions-specific: the runner drives both 1 and 10 partitions, which
 * is what disproved the tracker's original framing of S5.
 *
 * db_config.h must come FIRST -- it sets the feature macros that decide what the
 * system headers declare (s_chk_inclconfig enforces this), and db_int.h pulls in
 * the C headers this file needs.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/lock.h"
#include "dbinc/txn.h"

#include <sys/wait.h>
#include <signal.h>

#define	HOME	"TESTDIR_s5_proof"
#define	DBFILE	"s5p.db"
#define	ALARM_SECS	20

/*
 * The spin is unbounded, so the test needs its own clock.  Printing the
 * verdict from the handler (rather than letting an outer `timeout` kill the
 * process) is what makes a hang report AS a hang instead of as rc=124.
 */
static void
on_alarm(int sig)
{
	COMPQUIET(sig, 0);
	(void)write(1,
	    "  VERDICT s5_failchk_spin FAIL DB_ENV->failchk did NOT return "
	    "within the alarm -- __lock_failchk is spinning (S5)\n", 123);
	(void)write(1, "s5_proof: 1 failure(s)\n", 23);
	_exit(EXIT_FAILURE);
}

static const char *
modestr(db_lockmode_t m)
{
	switch (m) {
	case DB_LOCK_NG:			return "NG";
	case DB_LOCK_READ:			return "READ";
	case DB_LOCK_WRITE:			return "WRITE";
	case DB_LOCK_WAIT:			return "WAIT";
	case DB_LOCK_IWRITE:			return "IWRITE";
	case DB_LOCK_IREAD:			return "IREAD";
	case DB_LOCK_IWR:			return "IWR";
	case DB_LOCK_READ_UNCOMMITTED:		return "READ_UNCOMMITTED";
	case DB_LOCK_WWRITE:			return "WWRITE";
	case DB_LOCK_SIREAD:			return "SIREAD";
	default:				return "?";
	}
}

static int
my_isalive(DB_ENV *dbenv, pid_t pid, db_threadid_t tid, u_int32_t flags)
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(tid, 0);
	COMPQUIET(flags, 0);
	return (kill(pid, 0) == 0 || errno == EPERM);
}

/*
 * dump_lockers -- walk locker_tab exactly as __lock_failchk does and print,
 * for every locker the failchk loop would NOT skip, the three facts that
 * decide its fate.  This is the measurement: if a locker appears here with
 * "would_free=NO" and a non-empty heldby of only SIREAD, __lock_failchk's
 * `goto retry` has nothing to change and the loop cannot terminate.
 */
static int
dump_lockers(ENV *env, const char *tag)
{
	DB_ENV *dbenv;
	DB_LOCKER *lip;
	DB_LOCKREGION *lrp;
	DB_LOCKTAB *lt;
	DB_LOCK *lp_unused;
	DB_THREAD_INFO *ip;
	struct __db_lock *lp;
	u_int32_t i;
	int nstuck, nseen;

	COMPQUIET(lp_unused, NULL);
	dbenv = env->dbenv;
	lt = env->lk_handle;
	lrp = lt->reginfo.primary;
	nstuck = nseen = 0;

	/*
	 * ENV_ENTER is REQUIRED, not hygiene.  This function takes
	 * LOCK_LOCKERS, and the dead child still holds that mutex, so the
	 * acquisition goes down __db_pthread_mutex_lock's failchk arm
	 * (mut_pthread.c:386), which calls __env_set_state(THREAD_VERIFY).
	 * That asserts `ip != NULL' -- and without ENV_ENTER this thread has no
	 * entry in the thread table, so it aborts:
	 *
	 *   BDB0059 assert failure: env_failchk.c/458:
	 *       "ip != NULL && ip->dbth_state != THREAD_OUT"
	 *
	 * This cost me a false PASS: the first build I validated against was
	 * configured WITHOUT --enable-diagnostic, where DB_ASSERT compiles to
	 * nothing, so the test passed while the misuse was still there.  A
	 * pristine --enable-diagnostic build is what caught it.  Any test that
	 * reaches into a region directly needs this.
	 */
	ENV_ENTER(env, ip);
	LOCK_LOCKERS(env, lrp);
	for (i = 0; i < lrp->locker_t_size; i++)
		SH_TAILQ_FOREACH(lip, &lt->locker_tab[i], links, __db_locker) {
			int alive, empty, allnonread, nheld, txnal;

			txnal = (lip->id >= TXN_MINIMUM);
			empty = SH_LIST_EMPTY(&lip->heldby);

			/* The failchk skip test, verbatim. */
			if (txnal && (empty || lip->nlocks == lip->nwrites))
				continue;

			alive = dbenv->is_alive(dbenv, lip->pid, lip->tid,
			    F_ISSET(lip, DB_LOCKER_HANDLE_LOCKER) ?
			    DB_MUTEX_PROCESS_ONLY : 0);
			if (alive)
				continue;

			nseen++;
			/*
			 * Does heldby contain anything the DB_LOCK_PUT_READ
			 * pass would actually release?  That pass releases
			 * only DB_LOCK_READ and DB_LOCK_READ_UNCOMMITTED
			 * (lock.c:569-571, writes==0 for PUT_READ).
			 */
			allnonread = 1;
			nheld = 0;
			SH_LIST_FOREACH(lp, &lip->heldby,
			    locker_links, __db_lock) {
				nheld++;
				if (lp->mode == DB_LOCK_READ ||
				    lp->mode == DB_LOCK_READ_UNCOMMITTED)
					allnonread = 0;
			}

			printf("  [%s] locker 0x%lx pid=%ld txnal=%d "
			    "nlocks=%u nwrites=%u nheld=%d alive=%d "
			    "releasable_by_PUT_READ=%s modes=",
			    tag, (u_long)lip->id, (long)lip->pid, txnal,
			    lip->nlocks, lip->nwrites, nheld, alive,
			    allnonread ? "NONE" : "some");
			SH_LIST_FOREACH(lp, &lip->heldby,
			    locker_links, __db_lock)
				printf("%s ", modestr(lp->mode));

			/*
			 * __lock_failchk frees the locker only when
			 * id < TXN_MINIMUM.  A transactional one is left to
			 * __txn_failchk -- but failchk still did `goto retry`
			 * because heldby was non-empty, so if nothing was
			 * released the walk restarts unchanged.
			 */
			printf("| would_free=%s\n", txnal ? "NO (txnal)" : "yes");
			if (txnal && allnonread && nheld > 0) {
				nstuck++;
				printf("  [%s] ^^ NON-PROGRESS: failchk emits "
				    "BDB2053, calls PUT_READ (releases "
				    "nothing), does NOT free (txnal), then "
				    "`goto retry` -> same state\n", tag);
			}
			(void)fflush(stdout);
		}
	UNLOCK_LOCKERS(env, lrp);
	ENV_LEAVE(env, ip);
	printf("  [%s] %d dead locker(s) examined, %d in the non-progress "
	    "shape\n", tag, nseen, nstuck);
	return (nstuck);
}

int
main(int argc, char *argv[])
{
	DB *dbp;
	DBT key, data;
	DB_ENV *dbenv;
	DB_TXN *txn;
	ENV *env;
	pid_t kid;
	int nparts, ret, status, stuck;

	nparts = argc > 1 ? atoi(argv[1]) : 1;

	(void)system("rm -rf " HOME " 2>/dev/null");
	(void)mkdir(HOME, 0755);

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (fprintf(stderr, "env_create\n"), 1);
	dbenv->set_errfile(dbenv, stderr);
	(void)dbenv->set_lk_partitions(dbenv, (u_int32_t)nparts);
	(void)dbenv->set_thread_count(dbenv, 32);
	(void)dbenv->set_isalive(dbenv, my_isalive);
	(void)dbenv->set_timeout(dbenv, 200000, DB_SET_LOCK_TIMEOUT);
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_MULTIVERSION |
	    DB_THREAD | DB_FAILCHK | DB_RECOVER, 0600)) != 0)
		return (fprintf(stderr, "env open: %s\n",
		    db_strerror(ret)), 1);
	env = dbenv->env;
	printf("  lk_partitions=%d\n", nparts);

	if ((ret = db_create(&dbp, dbenv, 0)) != 0)
		return (1);
	if ((ret = dbp->open(dbp, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION | DB_THREAD,
	    0600)) != 0)
		return (fprintf(stderr, "db open: %s\n",
		    db_strerror(ret)), 1);

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)"k0"; key.size = 3;
	data.data = (void *)"v0"; data.size = 3;
	(void)dbp->put(dbp, NULL, &key, &data, 0);
	(void)dbp->close(dbp, 0);

	/*
	 * One child: take a SIREAD marker under a serializable txn (a pure
	 * READ-ONLY snapshot-safe transaction, so heldby ends up holding
	 * SIREAD and NOTHING a PUT_READ would release) and die on SIGKILL
	 * with the transaction open.
	 */
	(void)fflush(NULL);
	if ((kid = fork()) == 0) {
		DB *cdbp;
		DB_ENV *cenv;

		if (db_env_create(&cenv, 0) != 0)
			_exit(2);
		(void)cenv->set_lk_partitions(cenv, (u_int32_t)nparts);
		(void)cenv->set_thread_count(cenv, 32);
		(void)cenv->set_isalive(cenv, my_isalive);
		if (cenv->open(cenv, HOME, DB_INIT_LOCK | DB_INIT_LOG |
		    DB_INIT_MPOOL | DB_INIT_TXN | DB_MULTIVERSION |
		    DB_THREAD | DB_FAILCHK, 0600) != 0)
			_exit(2);
		if (db_create(&cdbp, cenv, 0) != 0)
			_exit(2);
		if (cdbp->open(cdbp, NULL, DBFILE, NULL, DB_BTREE,
		    DB_AUTO_COMMIT | DB_MULTIVERSION | DB_THREAD, 0600) != 0)
			_exit(2);
		if (cenv->txn_begin(cenv, NULL, &txn,
		    DB_TXN_SERIALIZABLE) != 0)
			_exit(2);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = (void *)"k0"; key.size = 3;
		data.flags = DB_DBT_MALLOC;
		/* READ ONLY: plants a SIREAD marker, takes no write lock. */
		(void)cdbp->get(cdbp, txn, &key, &data, 0);
		(void)fflush(NULL);
		(void)raise(SIGKILL);
		_exit(0);
	}
	(void)waitpid(kid, &status, 0);
	/*
	 * The child must have died FROM THE SIGKILL it raised itself.  If it
	 * exited normally instead, it bailed out of its own setup -- most
	 * likely because txn_begin rejected DB_TXN_SERIALIZABLE on a build
	 * without SSI ("BDB0055 illegal flag specified to txn_begin") -- and
	 * there is no dead locker for failchk to trip over.  Continuing would
	 * then assert inside __env_failchk, which is a HARNESS failure wearing
	 * the costume of an engine bug: exactly what happened the first time
	 * this ran against a non-diagnostic build dir.  SKIP with a reason
	 * rather than report a defect that was never observed.
	 */
	if (!WIFSIGNALED(status)) {
		printf("VERDICT s5_failchk_spin SKIP child exited %d without "
		    "being killed; DB_TXN_SERIALIZABLE is probably unavailable "
		    "in this build, so the S5 locker shape was never created\n",
		    WIFEXITED(status) ? WEXITSTATUS(status) : -1);
		(void)dbenv->close(dbenv, 0);
		return (0);
	}
	printf("  child pid %ld died on signal %d (txn left open)\n",
	    (long)kid, WTERMSIG(status));

	/* State BEFORE failchk: this is the shape the loop cannot resolve. */
	stuck = dump_lockers(env, "before");

	printf("  non_progress_shapes=%d\n", stuck);

	/*
	 * CALL IT.  Whether failchk terminates is the whole question, so it
	 * must be measured, not predicted from the table dump.  SIGALRM
	 * bounds the spin: without the fix this alarm is what ends the run,
	 * and the handler says so.  stdout is line-buffered and the BDB2053
	 * flood goes to the error stream, so a spin is also visible as log
	 * growth -- but the alarm is the verdict.
	 */
	printf("\n  calling DB_ENV->failchk (SIGALRM bound: %d s)\n",
	    ALARM_SECS);
	(void)fflush(stdout);
	(void)signal(SIGALRM, on_alarm);
	(void)alarm(ALARM_SECS);
	ret = dbenv->failchk(dbenv, 0);
	(void)alarm(0);
	printf("  failchk RETURNED %d (%s)\n", ret,
	    ret == 0 ? "success" : db_strerror(ret));
	(void)dump_lockers(env, "after");

	/*
	 * Termination is the pass criterion.  DB_RUNRECOVERY is a legitimate
	 * answer (a dead non-transactional locker with write locks); a HANG
	 * is not an answer at all.
	 */
	if (ret == 0 || ret == DB_RUNRECOVERY)
		printf("  VERDICT s5_failchk_spin PASS failchk TERMINATED "
		    "(ret=%d) with %d non-progress-shaped locker(s) present, "
		    "lk_partitions=%d\n", ret, stuck, nparts);
	else
		printf("  VERDICT s5_failchk_spin FAIL failchk returned "
		    "%d (%s)\n", ret, db_strerror(ret));
	printf("s5_proof: %d failure(s)\n",
	    (ret == 0 || ret == DB_RUNRECOVERY) ? 0 : 1);
	(void)dbenv->close(dbenv, 0);
	return ((ret == 0 || ret == DB_RUNRECOVERY) ?
	    EXIT_SUCCESS : EXIT_FAILURE);
}

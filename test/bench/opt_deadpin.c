/*-
 * opt_deadpin: RFC 0007 risk 4 -- a stale pin record left by a DEAD process
 * must NOT block eviction, while a live thread's pin record MUST.
 *
 * WHY THIS SHAPE.  The obvious version of this test (fork a child, have it hold
 * cursors, SIGKILL it, then hammer the cache) does NOT test the new mechanism:
 * a positioned cursor also holds bhp->ref, and eviction already refuses any
 * frame with a nonzero refcount, so both arms pass no matter what
 * __memp_bh_pinned does.  Written that way first, and both arms passed on a
 * build where the liveness check had been deleted -- a vacuous green.
 *
 * What is actually new in RFC 0007 phase 1 is a pin record with NO refcount
 * behind it (that is the entire point of the optimistic path), and the rule
 * eviction must follow for it:
 *
 *   live owner  -> the frame IS pinned, eviction must skip it
 *   dead owner  -> the pin is garbage failchk will reclaim, eviction must
 *                  proceed, or one SIGKILLed reader wedges the frame forever
 *
 * So this test drives __memp_bh_pinned() directly, with a real frame and a real
 * DB_THREAD_INFO, and flips only the owner's liveness between the two
 * assertions.  It links against libdb internals (like test/bench/bh_layout.c)
 * because the pin list is shared-region state with no public accessor.
 *
 * Teeth: a build whose __memp_bh_pinned drops the is_alive check FAILS assertion
 * 3 (dead owner reported as pinned).  A build that never consults the pin list
 * at all FAILS assertion 2 (live owner reported as unpinned).
 *
 * usage: opt_deadpin <home>
 */
#include "db_config.h"
#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/mp.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int fails;

static void
check(const char *what, int got, int want)
{
	if (got == want)
		printf("  ok   %-46s -> %d\n", what, got);
	else {
		printf("  FAIL %-46s -> %d (want %d)\n", what, got, want);
		fails++;
	}
}

static int
is_alive(dbenv, pid, tid, flags)
	DB_ENV *dbenv;
	pid_t pid;
	db_threadid_t tid;
	u_int32_t flags;
{
	COMPQUIET(dbenv, NULL);
	COMPQUIET(tid, 0);
	COMPQUIET(flags, 0);

	return (kill(pid, 0) == 0 || errno != ESRCH);
}

/* Our own DB_THREAD_INFO: the slot in the thread hash whose pid/tid is ours. */
static DB_THREAD_INFO *
myinfo(ENV *env)
{
	DB_HASHTAB *htab;
	DB_THREAD_INFO *ip;
	pid_t pid;
	u_int32_t i;

	pid = getpid();
	if ((htab = env->thr_hashtab) == NULL)
		return (NULL);
	for (i = 0; i < env->thr_nbucket; i++)
		SH_TAILQ_FOREACH(ip, &htab[i], dbth_links, __db_thread_info)
			if (ip->dbth_pid == pid &&
			    ip->dbth_state != THREAD_SLOT_NOT_IN_USE)
				return (ip);
	return (NULL);
}

/* A pid that is definitely dead: fork, exit immediately, reap. */
static pid_t
deadpid()
{
	pid_t pid;
	int status;

	if ((pid = fork()) == 0)
		_exit(0);
	if (pid < 0)
		return (-1);
	(void)waitpid(pid, &status, 0);
	return (pid);
}

int
main(argc, argv)
	int argc;
	char **argv;
{
	BH *bhp;
	DB *db;
	DB_ENV *dbenv;
	DB_MPOOL *dbmp;
	DB_MPOOLFILE *mpf;
	DB_THREAD_INFO *ip;
	DBT key, data;
	ENV *env;
	PAGE *h;
	PIN_LIST *list, *lp;
	REGINFO *infop;
	db_pgno_t pgno;
	pid_t dead, save_pid;
	char vbuf[64];
	unsigned kb, i;
	int ret;

	if (argc != 2) {
		fprintf(stderr, "usage: %s <home>\n", argv[0]);
		return (2);
	}

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (2);
	dbenv->set_errfile(dbenv, stderr);
	(void)dbenv->set_cachesize(dbenv, 0, 8 * 1024 * 1024, 1);
	(void)dbenv->set_flags(dbenv, DB_TXN_NOSYNC, 1);
	/* Both are preconditions: no thread region -> no pin list at all. */
	(void)dbenv->set_thread_count(dbenv, 32);
	(void)dbenv->set_isalive(dbenv, is_alive);
	if ((ret = dbenv->open(dbenv, argv[1], DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_TXN | DB_INIT_LOG | DB_THREAD, 0)) != 0) {
		dbenv->err(dbenv, ret, "env open");
		return (2);
	}
	env = dbenv->env;
	dbmp = env->mp_handle;

	if ((ret = db_create(&db, dbenv, 0)) != 0)
		return (2);
	if ((ret = db->open(db, NULL, "dead.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
		dbenv->err(dbenv, ret, "db open");
		return (2);
	}
	memset(vbuf, 'd', sizeof(vbuf));
	for (i = 0; i < 2000; i++) {
		memset(&key, 0, sizeof(key));
		kb = i;
		key.data = &kb;
		key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));
		data.data = vbuf;
		data.size = sizeof(vbuf);
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
			dbenv->err(dbenv, ret, "load");
			return (2);
		}
	}
	(void)dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE);

	/*
	 * Get a real frame: fetch a page, remember its BH, put it back.  After
	 * the put the frame holds no ref and no pin record, which is the state
	 * an optimistic reader operates on.
	 */
	mpf = db->get_mpf(db);
	pgno = 1;
	/*
	 * Calling mpool internals directly requires being registered as a
	 * thread of control (env_failchk.c asserts on it), which the public API
	 * does via ENV_ENTER.  Register once here and stay registered: the pin
	 * list we manipulate below IS this registration's state.
	 */
	if ((ret = __env_set_state(env, &ip, THREAD_ACTIVE)) != 0) {
		dbenv->err(dbenv, ret, "set_state");
		return (2);
	}
	if ((ret = __memp_fget(mpf, &pgno, ip, NULL, 0, &h)) != 0) {
		dbenv->err(dbenv, ret, "fget");
		return (2);
	}
	bhp = PAGE_TO_BH(h);
	infop = &dbmp->reginfo[bhp->region];
	if ((ret = __memp_fput(mpf, ip, h, DB_PRIORITY_UNCHANGED)) != 0) {
		dbenv->err(dbenv, ret, "fput");
		return (2);
	}

	if (ip == NULL) {
		printf("FAIL opt-deadpin no DB_THREAD_INFO for this process "
		    "(thread region missing?)\n");
		return (1);
	}
	if ((dead = deadpid()) < 0) {
		printf("FAIL opt-deadpin could not create a dead pid\n");
		return (1);
	}

	printf("opt-deadpin: frame=%p pgno=%lu region=%d owner_pid=%ld "
	    "dead_pid=%ld\n", (void *)bhp, (u_long)bhp->pgno, bhp->region,
	    (long)ip->dbth_pid, (long)dead);

	/* 1. No pin record anywhere: not pinned. */
	check("no pin record -> not pinned",
	    __memp_bh_pinned(env, infop, bhp), 0);

	/* Publish a pin record for this frame, exactly as __memp_fget_opt does. */
	list = R_ADDR(env->reginfo, ip->dbth_pinlist);
	for (lp = list; lp < &list[ip->dbth_pinmax]; lp++)
		if (lp->b_ref == INVALID_ROFF)
			break;
	if (lp == &list[ip->dbth_pinmax]) {
		printf("FAIL opt-deadpin no free pin slot\n");
		return (1);
	}
	lp->b_ref = R_OFFSET(infop, bhp);
	lp->region = bhp->region;
	ip->dbth_pincount++;

	/* 2. Pin record owned by a LIVE thread: pinned, eviction must skip. */
	check("pin record, owner ALIVE -> pinned",
	    __memp_bh_pinned(env, infop, bhp), 1);

	/*
	 * 3. THE LIVENESS RULE.  Same pin record, same frame; only the owner's
	 * pid changes to one that is definitely dead.  Eviction must be allowed
	 * to reuse the frame, or a single SIGKILLed reader wedges it until
	 * failchk runs.  A build that drops the is_alive check fails here.
	 */
	save_pid = ip->dbth_pid;
	ip->dbth_pid = dead;
	check("pin record, owner DEAD -> NOT pinned",
	    __memp_bh_pinned(env, infop, bhp), 0);
	ip->dbth_pid = save_pid;

	/* 4. Back to a live owner: pinned again (the change was the liveness). */
	check("owner restored ALIVE -> pinned again",
	    __memp_bh_pinned(env, infop, bhp), 1);

	/* 5. A pin record for a DIFFERENT frame must not protect this one. */
	lp->b_ref = R_OFFSET(infop, bhp) + (roff_t)sizeof(BH);
	check("pin record for another frame -> not pinned",
	    __memp_bh_pinned(env, infop, bhp), 0);

	lp->b_ref = INVALID_ROFF;
	ip->dbth_pincount--;

	(void)db->close(db, 0);
	(void)dbenv->close(dbenv, 0);

	if (fails != 0) {
		printf("FAIL opt-deadpin %d of 5 assertions failed\n", fails);
		return (1);
	}
	printf("VERDICT opt-deadpin dead-process pin does NOT block eviction; "
	    "live pin does; 5/5 assertions\n");
	return (0);
}

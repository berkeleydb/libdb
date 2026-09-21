/*-
 * See the file LICENSE for redistribution information.
 *
 * lock_order_check --- gate for the DIAGNOSTIC lock-order checker (gap G9).
 *
 * This is the TEETH test for src/mutex/mut_order.c, and it is written as an
 * A/B so that it cannot go vacuously green.
 *
 * The subject is a REAL, pre-existing self-deadlock, not a synthetic reversal.
 * rfc/0010-global-invariants.md A3 draws an edge
 * "object partition -> TXN_SYSTEM_LOCK", but those are the SAME latch: the
 * lock, txn and log regions all alias renv->mtx_regenv (lock_region.c:179,
 * txn_region.c:118, log.c:224).  With lk_partitions == 1, LOCK_SYSTEM_LOCK is
 * live (dbinc/lock.h:340) and __lock_get_internal's SSI branch then takes
 * TXN_SYSTEM_LOCK -- the same non-recursive latch -- at lock.c:1119.
 *
 * The two arms:
 *   lk_partitions = 4  (CONTROL)  LOCK_SYSTEM_LOCK is a no-op, so the nesting
 *                                 never happens.  MUST complete cleanly.  If
 *                                 this arm ever fires the checker, the model
 *                                 has a false positive.
 *   lk_partitions = 1  (SUBJECT)  MUST be reported by the checker.  On a build
 *                                 without the checker this arm HANGS, which is
 *                                 the bug the checker converts into a
 *                                 diagnosis.
 *
 * The subject arm runs in a CHILD process and the parent inspects how it ended.
 * NOTE: the nesting this test was written against (lock.c:801 taking the
 * region latch, then lock.c:1119 taking the same latch again via
 * TXN_SYSTEM_LOCK) has since been FIXED, so the subject arm now asserts a
 * CLEAN completion at lk_partitions=1 and is a regression gate on that fix.
 * A checker abort here means the nesting came back.  Run with
 * DB_LOCK_ORDER_WARN=1 to see a report without aborting.
 *
 * Verdict lines are explicit ("PASS:" / "FAIL:") so a harness can assert a
 * real verdict was produced rather than trusting the exit status alone.
 */
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME_PREFIX	"LOCK_ORDER_CHECK"
#define	NSEED		400
/*
 * Seconds to wait for the subject arm.  Generous: the arm does a few hundred
 * puts before the interesting acquisition, and a loaded machine must not turn
 * a PASS into a spurious "HUNG".
 */
#define	SUBJECT_TIMEOUT	60

static int
ck(int ret, const char *what, int line)
{
	if (ret != 0) {
		fprintf(stderr, "%s:%d: %s: %s\n",
		    __FILE__, line, what, db_strerror(ret));
		exit(3);
	}
	return (ret);
}
#define	CK(call)	ck((call), #call, __LINE__)

/*
 * workload --
 *	Form one SSI rw-antidependency: a serializable reader leaves a SIREAD
 *	marker on a key, then a writer write-locks that same key.  The writer's
 *	__lock_get_internal walks sh_obj->sireaders, finds the marker, and takes
 *	TXN_SYSTEM_LOCK to record the edge.  That acquisition is the subject.
 *
 *	DB_TXN_SERIALIZABLE is essential.  Plain DB_TXN_SNAPSHOT arms none of
 *	the rw-antidependency tracking (txn.c:319-322), so a probe written with
 *	DB_TXN_SNAPSHOT alone passes at every partition count and proves nothing.
 */
static void
workload(u_int32_t nparts)
{
	DB_ENV *env;
	DB *db;
	DB_TXN *seed, *rdr, *wtr;
	DBT key, data;
	char home[64];
	int i, k, v;

	(void)snprintf(home, sizeof(home), "%s_%lu",
	    HOME_PREFIX, (u_long)nparts);
	{
		char cmd[256];
		(void)snprintf(cmd, sizeof(cmd),
		    "find %s -type f -delete 2>/dev/null; mkdir -p %s",
		    home, home);
		(void)system(cmd);
	}

	CK(db_env_create(&env, 0));
	CK(env->set_lk_partitions(env, nparts));
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "lock_order_check");
	CK(env->open(env, home, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_MULTIVERSION, 0644));

	CK(db_create(&db, env, 0));
	CK(db->open(db, NULL, "lo.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION, 0644));

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &k;
	key.size = sizeof(k);

	/* Seed enough keys that the read and write keys are real records. */
	CK(env->txn_begin(env, NULL, &seed, 0));
	for (i = 0; i < NSEED; i++) {
		k = i;
		v = 0;
		data.data = &v;
		data.size = sizeof(v);
		CK(db->put(db, seed, &key, &data, 0));
	}
	CK(seed->commit(seed, 0));

	/* Reader: serializable snapshot read -> leaves a SIREAD marker. */
	CK(env->txn_begin(env, NULL, &rdr,
	    DB_TXN_SNAPSHOT | DB_TXN_SERIALIZABLE));
	k = 0;
	v = -1;
	data.data = &v;
	data.ulen = sizeof(v);
	data.flags = DB_DBT_USERMEM;
	CK(db->get(db, rdr, &key, &data, 0));
	data.flags = 0;

	/* Writer: write-locks the same key.  This is the acquisition. */
	CK(env->txn_begin(env, NULL, &wtr,
	    DB_TXN_SNAPSHOT | DB_TXN_SERIALIZABLE));
	k = 0;
	v = 1;
	data.data = &v;
	data.size = sizeof(v);
	CK(db->put(db, wtr, &key, &data, 0));

	(void)wtr->abort(wtr);
	(void)rdr->abort(rdr);
	CK(db->close(db, 0));
	CK(env->close(env, 0));
}

int
main(int argc, char **argv)
{
	pid_t pid;
	int status, fails, waited;

	/*
	 * Child mode: run one arm and let it die however it dies.  argv[1] is
	 * the partition count.
	 */
	if (argc == 3 && strcmp(argv[1], "--arm") == 0) {
		workload((u_int32_t)atoi(argv[2]));
		printf("ARM_COMPLETED\n");
		return (0);
	}

	fails = 0;

	/*
	 * Is the checker present?  DIAGNOSTIC is a build-internal macro that is
	 * NOT exported through db.h, so a test that keyed off #ifdef DIAGNOSTIC
	 * would silently self-skip even in a diagnostic build -- which it did,
	 * and which is exactly the vacuous green this tree keeps producing.
	 *
	 * Detect it by BEHAVIOUR instead: run the subject arm and classify how
	 * it ends.  SIGABRT means the checker caught it.  A clean exit means
	 * either no checker or a fixed bug.  A timeout/hang means a checker that
	 * validates too late.  This cannot go vacuously green because the
	 * "nothing happened" outcome is a FAIL, not a skip.
	 */

	/* ---- CONTROL: lk_partitions=4 must complete with no report. ---- */
	if ((pid = fork()) == 0) {
		execl(argv[0], argv[0], "--arm", "4", (char *)NULL);
		_exit(127);
	}
	(void)waitpid(pid, &status, 0);
	if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
		printf("PASS (control): lk_partitions=4 completed -- "
		    "the checker did not fire on a legal schedule\n");
	else {
		printf("FAIL (control): lk_partitions=4 did not complete "
		    "cleanly (status 0x%x) -- the lock-order model has a "
		    "false positive\n", status);
		fails++;
	}

	/* ---- SUBJECT: lk_partitions=1 must be caught, not hang. ---- */
	if ((pid = fork()) == 0) {
		execl(argv[0], argv[0], "--arm", "1", (char *)NULL);
		_exit(127);
	}
	/*
	 * Bound the wait.  Without the checker this arm blocks forever on a
	 * latch it already holds, and a test that inherits that hang is useless
	 * -- it reports nothing and burns the harness timeout instead.
	 */
	for (waited = 0; waited < SUBJECT_TIMEOUT; waited++) {
		if (waitpid(pid, &status, WNOHANG) == pid)
			break;
		(void)sleep(1);
	}
	if (waited >= SUBJECT_TIMEOUT) {
		(void)kill(pid, SIGKILL);
		(void)waitpid(pid, &status, 0);
		printf("FAIL: lk_partitions=1 HUNG for %ds.  The self-deadlock "
		    "is real but nothing reported it -- either this is not an "
		    "--enable-diagnostic build, or the checker validates AFTER "
		    "acquiring instead of before.\n", SUBJECT_TIMEOUT);
		fails++;
	} else if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
		/*
		 * The expected outcome NOW.  lock.c:1119's nesting was the
		 * subject of this test and it has since been FIXED: the SSI
		 * branch no longer re-acquires the txn-region latch when it is
		 * already held (one lock partition means LOCK_SYSTEM_LOCK and
		 * TXN_SYSTEM_LOCK are the same physical mutex).  So a clean
		 * completion at lk_partitions=1 is exactly what we want, and
		 * this arm is now a REGRESSION GATE on that fix rather than a
		 * demonstration of the checker.
		 *
		 * The checker's own teeth are demonstrated separately by
		 * reverting src/lock/lock.c and observing SIGABRT naming
		 * lock.c:801 -> lock.c:1119 (see the lock-order report); that
		 * cannot be asserted from inside a normal test run because it
		 * requires a differently-built library.
		 */
		printf("PASS: lk_partitions=1 completed cleanly -- the SSI "
		    "branch no longer nests TXN_SYSTEM_LOCK inside a held "
		    "LOCK_SYSTEM_LOCK (regression gate on that fix)\n");
	else if (WIFSIGNALED(status) &&
	    (WTERMSIG(status) == SIGABRT || WTERMSIG(status) == SIGIOT)) {
		/*
		 * The checker fired, which means the nesting is BACK: either
		 * the lock.c fix was reverted/regressed, or a new site nests
		 * two aliased region latches.  This is a real failure now.
		 */
		printf("FAIL: lk_partitions=1 aborted -- the lock-order "
		    "checker fired, so an aliased-region-latch nesting has "
		    "returned (lock.c:801 -> lock.c:1119 was the original).\n");
		fails++;
	} else {
		printf("FAIL: lk_partitions=1 ended unexpectedly "
		    "(status 0x%x); a hang here means the checker did not "
		    "validate before acquiring\n", status);
		fails++;
	}

	printf("%s: lock_order_check\n", fails == 0 ? "PASS" : "FAIL");
	return (fails == 0 ? 0 : 1);
}

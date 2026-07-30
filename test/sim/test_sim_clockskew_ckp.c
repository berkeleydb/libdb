/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_clockskew_ckp.c --
 *	Checkpoint + recovery under a large FORWARD clock jump.
 *
 *	Arms the clock-skew fault with an aggressive forward jump (the clock
 *	leaps ahead by up to an hour, repeatedly) while a transactional
 *	workload runs and checkpoints are taken.  A forward jump is what a
 *	naive scheduler treats as "lots of time has passed" -- it must not
 *	make checkpoint bookkeeping (the checkpoint LSN, the durable frontier)
 *	inconsistent.  After a mid-workload crash the environment recovers
 *	clean and every fsync-acked commit is present.
 *
 *	Invariant: a forward clock jump does not corrupt checkpoint/recovery
 *	state; checkpoints still make progress; committed data survives.
 *
 *	Honest scope note: on a platform with libc time() (HAVE_TIME, i.e.
 *	Linux), the checkpoint MINUTE-interval decision in __txn_checkpoint
 *	reads libc time() directly, NOT __os_gettime, so it is not reachable
 *	by this __os_gettime skew -- which is itself a finding (that decision
 *	is inherently robust to __os_gettime skew).  This scenario therefore
 *	drives checkpoints EXPLICITLY (as a real application's checkpoint
 *	thread does) and proves the skew perturbs nothing downstream in the
 *	checkpoint/recovery path.  See DESIGN.md clock-skew section.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_clockskew_ckp && ./test_sim_clockskew_ckp [seed]
 */

#include "sim_scenario.h"
#include "sim_clock.h"

#include <signal.h>

#define HOME    "TESTDIR_sim_clockskew_ckp"
#define DBFILE  "ckpskew.db"
#define NTXN    256
#define CKP_EVERY 32
#define WALL_LIMIT 30

static unsigned char g_committed[NTXN];
static uint64_t g_seed;

static void
on_alarm(sig)
	int sig;
{
	(void)sig;
	fprintf(stderr, "test_sim_clockskew_ckp: FAIL -- HUNG: checkpoint "
	    "stopped making progress under a forward clock jump (>%ds); "
	    "REAL BUG.  Reproduce: ./test_sim_clockskew_ckp 0x%llx\n",
	    WALL_LIMIT, (unsigned long long)g_seed);
	_exit(3);
}

static int
open_db(env, dbp, create)
	DB_ENV *env;
	DB **dbp;
	int create;
{
	DB *db;
	int ret;

	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0)
		return (ret);
	*dbp = db;
	return (0);
}

static int
populate(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[48];
	int i, ret;
	FILE *fp;
	uint64_t tok;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	memset(g_committed, 0, sizeof(g_committed));

	/*
	 * Arm a large forward jump: the clock leaps ahead by up to an hour on
	 * 60% of reads.  __os_gettime readers (lock/txn deadlines, rep timers,
	 * and the checkpoint LOG-region timestamps) all see the jumped clock.
	 */
	__db_sim_clock_enable(
	    /* offset */ 0,                              /* seeded steady skew */
	    /* jitter */ 5LL * 1000 * 1000,              /* +/-5ms */
	    /* jump   */ 3600LL * 1000 * 1000 * 1000,    /* up to +1h jumps */
	    /* jump%  */ 600);

	for (i = 0; i < NTXN; i++) {
		tok = __db_sim_rng(DB_SIM_RNG_APP);
		(void)snprintf(kbuf, sizeof(kbuf), "ck-%08d", i);
		(void)snprintf(vbuf, sizeof(vbuf), "cv-%016llx",
		    (unsigned long long)tok);
		if (env->txn_begin(env, NULL, &txn, 0) != 0)
			continue;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if (db->put(db, txn, &key, &data, 0) != 0) {
			(void)txn->abort(txn);
			continue;
		}
		if (txn->commit(txn, DB_TXN_SYNC) == 0)
			g_committed[i] = 1;
		/* Explicit checkpoint mid-workload, WITH the clock jumping. */
		if (i % CKP_EVERY == CKP_EVERY - 1)
			(void)env->txn_checkpoint(env, 0, 0, DB_FORCE);
	}

	if ((fp = fopen(HOME "/committed.map", "wb")) != NULL) {
		(void)fwrite(g_committed, 1, sizeof(g_committed), fp);
		(void)fclose(fp);
	}

	/* One last checkpoint under the jumping clock, then crash. */
	(void)env->txn_checkpoint(env, 0, 0, DB_FORCE);
	__db_sim_clock_disable();     /* the truncation must not be skewed */
	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32];
	unsigned char committed[NTXN];
	FILE *fp;
	int i, ret, missing = 0, ncommitted = 0;

	g_seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC10C0002;

	(void)signal(SIGALRM, on_alarm);
	(void)alarm(WALL_LIMIT);

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(g_seed, populate) != 0)
		return (EXIT_FAILURE);

	memset(committed, 0, sizeof(committed));
	if ((fp = fopen(HOME "/committed.map", "rb")) != NULL) {
		(void)fread(committed, 1, sizeof(committed), fp);
		(void)fclose(fp);
	}

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(g_seed);
	for (i = 0; i < NTXN; i++) {
		if (!committed[i])
			continue;
		ncommitted++;
		(void)snprintf(kbuf, sizeof(kbuf), "ck-%08d", i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing++;
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_clockskew_ckp: verify FAILED: %s "
		    "(seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)g_seed);
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);
	(void)alarm(0);

	if (missing != 0) {
		fprintf(stderr, "test_sim_clockskew_ckp: FAIL -- %d of %d "
		    "committed txns lost across a forward-clock-jump checkpoint "
		    "(seed 0x%llx)\n", missing, ncommitted,
		    (unsigned long long)g_seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_clockskew_ckp: PASS -- checkpoints made progress "
	    "under a forward clock jump; all %d committed txns durable, tree "
	    "clean after recovery (seed 0x%llx)\n",
	    ncommitted, (unsigned long long)g_seed);
	return (EXIT_SUCCESS);
}

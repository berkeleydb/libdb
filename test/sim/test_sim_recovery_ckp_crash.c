/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_recovery_ckp_crash.c --
 *	Crash during the RECOVERY CHECKPOINT write, then recover: recovery
 *	must converge.  After the undo+redo passes, __db_apprec writes a
 *	recovery checkpoint (a __txn_ckp log record + a log fsync) so a
 *	subsequent recovery can start from there instead of replaying the
 *	whole log again.  If the process dies AFTER redo is applied but
 *	while / just before that recovery checkpoint becomes durable, the
 *	next recovery must re-derive the same state -- it simply replays the
 *	log from the prior (still-valid) checkpoint again.  A recovery that
 *	trusted a half-written checkpoint, or that acked success before the
 *	checkpoint LSN was fsync'd, would diverge or lose committed data.
 *
 *	This scenario crashes recovery at each of the LAST few recovery I/O
 *	ops (the checkpoint-write region), then finishes recovery and
 *	asserts the full committed set matches a reference full-state hash
 *	taken from a clean recovery -- convergence regardless of a
 *	checkpoint-time interruption.
 *
 *	Deterministic: same seed => same recovery => same tail ops => same
 *	outcome.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_recovery_ckp_crash && ./test_sim_recovery_ckp_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_rec_ckp"
#define DBFILE  "recckp.db"
#define NCOMMIT 110

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	int j;

	(void)snprintf(kbuf, 32, "rk-%08d", i);
	for (j = 0; j < 22; j++)
		vbuf[j] = (char)('0' + ((i * 11 + j) % 10));
	vbuf[22] = '\0';
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
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "open failed: %s\n", db_strerror(ret));
		return (ret);
	}
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
	char kbuf[32], vbuf[32];
	int i, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0)
			return (ret);
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
	}
	mkrec(NCOMMIT, kbuf, vbuf);
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	(void)db->put(db, txn, &key, &data, 0);

	SIM_CRASH_EXIT();
	return (0);
}

static int
snap(save) const char *save;
{
	char cmd[512];
	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && cp -a %s %s",
	    save, HOME, save);
	return (system(cmd));
}
static int
rest(save) const char *save;
{
	char cmd[512];
	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && cp -a %s %s",
	    HOME, save, HOME);
	return (system(cmd));
}

static int
recover_and_hash(seed, hashp)
	uint64_t seed;
	uint64_t *hashp;
{
	DB_ENV *env;
	DB *db;
	DBC *dbc;
	DBT key, data;
	uint64_t h = 1469598103934665603ull;
	int ret;
	const unsigned char *p, *e;

	if (sim_env_recover(HOME, &env) != 0)
		return (-1);
	if (open_db(env, &db, 0) != 0) {
		(void)env->close(env, 0);
		return (-1);
	}
	if ((ret = db->cursor(db, NULL, &dbc, 0)) != 0) {
		(void)db->close(db, 0);
		(void)env->close(env, 0);
		return (-1);
	}
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	while ((ret = dbc->get(dbc, &key, &data, DB_NEXT)) == 0) {
		for (p = key.data, e = p + key.size; p < e; p++) {
			h ^= *p; h *= 1099511628211ull;
		}
		for (p = data.data, e = p + data.size; p < e; p++) {
			h ^= *p; h *= 1099511628211ull;
		}
	}
	(void)dbc->close(dbc);
	(void)db->close(db, 0);
	{
		DB *vdb;
		if (db_create(&vdb, env, 0) == 0 &&
		    vdb->verify(vdb, DBFILE, NULL, NULL, 0) != 0) {
			(void)env->close(env, 0);
			return (-1);
		}
	}
	(void)env->close(env, 0);
	if (ret != DB_NOTFOUND)
		return (-1);
	*hashp = h;
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xCC12;
	const char *save = HOME ".saved";
	uint64_t ref = 0, trial = 0;
	unsigned long full_ticks = 0, c;
	int rc, trials = 0, crashes = 0, k, ntail;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);
	if (snap(save) != 0)
		return (EXIT_FAILURE);

	rc = sim_recover_child(seed, HOME, 0, &full_ticks);
	if (rc != 0) {
		fprintf(stderr, "test_sim_recovery_ckp_crash: probe recovery "
		    "failed (rc=%d, seed 0x%llx)\n", rc,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (rest(save) != 0)
		return (EXIT_FAILURE);

	if (recover_and_hash(seed, &ref) != 0) {
		fprintf(stderr, "test_sim_recovery_ckp_crash: reference "
		    "recovery failed (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}

	printf("test_sim_recovery_ckp_crash: full recovery %lu I/O ops; "
	    "reference %016llx (seed 0x%llx)\n", full_ticks,
	    (unsigned long long)ref, (unsigned long long)seed);

	if (full_ticks == 0) {
		(void)sim_fresh_home(save);
		printf("test_sim_recovery_ckp_crash: PASS -- recovery correct "
		    "(no interruptible I/O this seed)\n");
		return (EXIT_SUCCESS);
	}

	/* Crash at the LAST few recovery ops (the checkpoint-write tail). */
	ntail = (int)(full_ticks < 4 ? full_ticks : 4);
	for (k = 0; k < ntail; k++) {
		c = full_ticks - (unsigned long)k;   /* last, last-1, ... */
		if (c < 1)
			c = 1;
		if (rest(save) != 0)
			return (EXIT_FAILURE);
		rc = sim_recover_child(seed, HOME, c, NULL);
		if (rc < 0)
			return (EXIT_FAILURE);
		if (rc == 1)
			crashes++;
		if (recover_and_hash(seed, &trial) != 0) {
			fprintf(stderr, "test_sim_recovery_ckp_crash: FAIL -- "
			    "recovery after a checkpoint-time crash at op %lu "
			    "did not complete (seed 0x%llx)\n", c,
			    (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		if (trial != ref) {
			fprintf(stderr, "test_sim_recovery_ckp_crash: FAIL -- "
			    "state DIVERGED after a checkpoint-time crash at "
			    "op %lu: %016llx != reference %016llx (seed "
			    "0x%llx) -- recovery checkpoint not crash-safe\n",
			    c, (unsigned long long)trial,
			    (unsigned long long)ref, (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		trials++;
	}

	(void)sim_fresh_home(save);
	printf("test_sim_recovery_ckp_crash: PASS -- %d checkpoint-time "
	    "recovery crashes (%d hit), all converged to reference %016llx "
	    "(recovery checkpoint crash-safe; seed 0x%llx)\n", trials, crashes,
	    (unsigned long long)ref, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}

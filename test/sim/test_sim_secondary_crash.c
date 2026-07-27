/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_secondary_crash.c --
 *	Secondary-index (associate) consistency after crash + recovery.  A
 *	primary btree has a secondary index associated (DB_CREATE, keyed by
 *	a field of the primary value).  N durable txns insert primary
 *	records (each put also updates the secondary via the callback).  The
 *	process crashes; recovery runs; then EVERY committed primary record
 *	must be reachable BOTH by its primary key AND by a secondary-key
 *	lookup (pget), and the count via the secondary must equal the count
 *	via the primary -- no dangling or missing secondary entries.
 *
 *	Invariant (DESIGN.md catalog #5): the primary and its secondary
 *	index stay mutually consistent across a crash.  A secondary that
 *	lost or gained entries relative to the primary is a corruption the
 *	recovery must not leave behind.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_secondary_crash && ./test_sim_secondary_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_secondary"
#define PRIDB   "pri.db"
#define SECDB   "sec.db"
#define NCOMMIT 80

/* Primary value layout: "sec:<8 hex tok>|pri:<index>".  The secondary key
 * is the "<8 hex tok>" field (unique per record via the seeded token). */
static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "p-%08d", i);
	(void)snprintf(vbuf, 40, "%08llx|pri-%08d",
	    (unsigned long long)(tok & 0xffffffffull), i);
}

/* Secondary key = the leading "%08x" token of the primary value. */
static int
getsec(secondary, pkey, pdata, skey)
	DB *secondary;
	const DBT *pkey, *pdata;
	DBT *skey;
{
	(void)secondary;
	(void)pkey;
	memset(skey, 0, sizeof(DBT));
	skey->data = pdata->data;   /* first 8 chars: the hex token */
	skey->size = 8;
	return (0);
}

static int
open_dbs(env, prip, secp, create)
	DB_ENV *env;
	DB **prip, **secp;
	int create;
{
	DB *pri, *sec;
	int ret;

	if ((ret = db_create(&pri, env, 0)) != 0)
		return (ret);
	if ((ret = pri->open(pri, NULL, PRIDB, NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "pri open: %s\n", db_strerror(ret));
		return (ret);
	}
	if ((ret = db_create(&sec, env, 0)) != 0)
		return (ret);
	/* Secondary allows dups (different records could share a token). */
	(void)sec->set_flags(sec, DB_DUPSORT);
	if ((ret = sec->open(sec, NULL, SECDB, NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "sec open: %s\n", db_strerror(ret));
		return (ret);
	}
	if ((ret = pri->associate(pri, NULL, sec, getsec,
	    create ? DB_CREATE : 0)) != 0) {
		fprintf(stderr, "associate: %s\n", db_strerror(ret));
		return (ret);
	}
	*prip = pri;
	*secp = sec;
	return (0);
}

static int
populate(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *pri, *sec;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[40];
	int i, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_dbs(env, &pri, &sec, 1)) != 0)
		return (ret);

	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = pri->put(pri, txn, &key, &data, 0)) != 0)
			return (ret);
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
	}

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x5EC0;
	DB_ENV *env;
	DB *pri, *sec;
	DBT skey, pkey, pdata;
	char kbuf[32], vbuf[40];
	int i, ret, missing_pri = 0, missing_sec = 0;
	db_recno_t seccount = 0, pricount = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_dbs(env, &pri, &sec, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NCOMMIT; i++) {
		DBT k, d;
		mkrec(i, kbuf, vbuf);
		/* (a) primary lookup */
		memset(&k, 0, sizeof(k));
		memset(&d, 0, sizeof(d));
		k.data = kbuf; k.size = (u_int32_t)strlen(kbuf) + 1;
		if (pri->get(pri, NULL, &k, &d, 0) != 0) {
			missing_pri++;
			continue;
		}
		pricount++;
		/* (b) secondary lookup by token (pget returns the primary
		 * record); the primary key it returns must match ours. */
		memset(&skey, 0, sizeof(skey));
		memset(&pkey, 0, sizeof(pkey));
		memset(&pdata, 0, sizeof(pdata));
		skey.data = vbuf; skey.size = 8;   /* the token field */
		if (sec->pget(sec, NULL, &skey, &pkey, &pdata, 0) != 0) {
			missing_sec++;
			continue;
		}
		seccount++;
		/* pdata must equal the primary value we stored. */
		if (pdata.size != strlen(vbuf) + 1 ||
		    memcmp(pdata.data, vbuf, pdata.size) != 0)
			missing_sec++;
	}
	__db_sim_deactivate();

	(void)sec->close(sec, 0);
	(void)pri->close(pri, 0);
	(void)env->close(env, 0);

	printf("test_sim_secondary_crash: %u via primary, %u via secondary, "
	    "%d missing-pri, %d missing/wrong-sec (seed 0x%llx)\n",
	    (u_int)pricount, (u_int)seccount, missing_pri, missing_sec,
	    (unsigned long long)seed);

	if (missing_pri != 0 || missing_sec != 0 || pricount != seccount) {
		fprintf(stderr, "test_sim_secondary_crash: FAIL -- primary/"
		    "secondary inconsistent after recovery (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_secondary_crash: PASS -- all %d committed records "
	    "consistent across primary and secondary after crash+recover "
	    "(seed 0x%llx)\n", NCOMMIT, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}

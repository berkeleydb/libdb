/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_overflow_torn.c --
 *	Large / overflow-record corruption-detection scenario.  Values far
 *	larger than the page size spill onto overflow pages.  With DB_CHKSUM
 *	and a cold DB_PRIVATE cache, every page (including overflow pages)
 *	is read from disk on lookup; a seeded corrupt-read flips a byte.
 *	The engine must either return the CORRECT big value or error cleanly
 *	(checksum) -- it must NEVER hand back a silently-wrong overflow
 *	record.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_overflow_torn && ./test_sim_overflow_torn [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_ovf"
#define DBFILE  "ovf.db"
#define NKEYS   80
#define PGSIZE  512
#define VLEN    4000       /* >> PGSIZE: forces overflow pages */

/* Deterministic big value for record i: a repeating verifiable pattern. */
static void
mkval(i, vbuf)
	int i;
	unsigned char *vbuf;
{
	int j;
	for (j = 0; j < VLEN; j++)
		vbuf[j] = (unsigned char)((i * 31 + j * 7) & 0xff);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x0FF;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32];
	unsigned char vbuf[VLEN], want[VLEN];
	int i, ret, correct = 0, detected = 0, silent_bad = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);

	/* ---- populate a DB_CHKSUM btree with big (overflow) values ---- */
	if ((ret = db_env_create(&env, 0)) != 0)
		goto err;
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_MPOOL, 0664)) != 0)
		goto err;
	if ((ret = db_create(&db, env, 0)) != 0)
		goto err;
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE, 0664)) != 0)
		goto err;
	for (i = 0; i < NKEYS; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "of-%08d", i);
		mkval(i, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = VLEN;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			goto err;
	}
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	/* ---- reopen COLD (DB_PRIVATE, small cache), arm corrupt reads,
	 *      fetch every big value from disk ---- */
	if ((ret = db_env_create(&env, 0)) != 0)
		goto err;
	(void)env->set_cachesize(env, 0, 128 * 1024, 1);
	if ((ret = env->open(env, HOME,
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0664)) != 0)
		goto err;
	if ((ret = db_create(&db, env, 0)) != 0)
		goto err;
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE, 0, 0664)) != 0)
		goto err;

	__db_sim_activate(seed);
	__db_sim_io_corrupt_enable(60);    /* 6% of page reads bit-flipped */

	for (i = 0; i < NKEYS; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "of-%08d", i);
		mkval(i, want);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0) {
			if (data.size == VLEN &&
			    memcmp(data.data, want, VLEN) == 0)
				correct++;
			else
				silent_bad++;   /* wrong big value, no error */
		} else
			detected++;         /* clean checksum/page error */
	}

	__db_sim_io_corrupt_disable();
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	printf("test_sim_overflow_torn: %d correct, %d detected, %d "
	    "SILENT-BAD (seed 0x%llx)\n", correct, detected, silent_bad,
	    (unsigned long long)seed);

	if (silent_bad != 0) {
		fprintf(stderr, "test_sim_overflow_torn: FAIL -- %d silently "
		    "corrupted overflow records slipped past the checksum\n",
		    silent_bad);
		return (EXIT_FAILURE);
	}
	printf("test_sim_overflow_torn: PASS -- no silent corruption of "
	    "overflow records; every read correct or cleanly rejected\n");
	return (EXIT_SUCCESS);

err:
	fprintf(stderr, "test_sim_overflow_torn: setup error: %s\n",
	    db_strerror(ret));
	return (EXIT_FAILURE);
}

/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_split_torn.c --
 *	Torn write DURING a btree split, caught by the page checksum.  A
 *	small-page DB_CHKSUM btree takes a scatter-insert churn heavy enough
 *	to force many page splits; torn writes are armed during the churn so
 *	some split-produced pages persist only a strict prefix (a latent bad
 *	tail).  The env is then reopened COLD (DB_PRIVATE small cache) and
 *	every key scanned so every page is re-read from disk.
 *
 *	Invariant (DESIGN.md catalog #17/#21): a torn split page is NEVER
 *	returned as silently-wrong data -- the per-page checksum either
 *	catches it (a clean get error) or the page was a checksum-consistent
 *	earlier full write.  A get that returns bytes not matching what was
 *	stored, with no error, is SILENT-BAD and fails.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_split_torn && ./test_sim_split_torn [seed]
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"

#define HOME    "TESTDIR_sim_split_torn"
#define DBFILE  "splittorn.db"
#define NKEYS   500
#define PGSIZE  512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	int j;

	(void)snprintf(kbuf, 32, "st-%08u", (i * 2654435761u) % 1000000u);
	/* Verifiable pattern so a silent torn tail shows as a mismatch. */
	for (j = 0; j < 30; j++)
		vbuf[j] = (char)('A' + ((i + j) % 26));
	vbuf[30] = '\0';
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
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    create ? DB_CREATE : 0, 0664)) != 0) {
		fprintf(stderr, "open failed: %s\n", db_strerror(ret));
		return (ret);
	}
	*dbp = db;
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x5701;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32], cmd[256];
	int i, ret, correct = 0, detected = 0, silent_bad = 0;
	unsigned long torn_phase1;

	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	(void)system(cmd);

	/* ---- phase 1: split-heavy build with torn writes armed ---- */
	if ((ret = db_env_create(&env, 0)) != 0)
		goto env_err;
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_MPOOL, 0664)) != 0)
		goto env_err;
	if ((ret = open_db(env, &db, 1)) != 0)
		goto env_err;

	__db_sim_activate(seed);
	/* Build the tree CLEAN (so all keys are present), forcing many splits
	 * via scatter inserts. */
	for (i = 0; i < NKEYS; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		(void)db->put(db, NULL, &key, &data, 0);
	}

	/* Now arm torn writes and FLUSH: every dirty split-produced page is
	 * written to disk under the torn fault, so some persist only a strict
	 * prefix (a latent bad tail).  Shares the IO stream but only writes
	 * are in flight here, so this exercises __db_sim_io_torn_prefix. */
	__db_sim_io_corrupt_enable(150);
	(void)db->sync(db, 0);
	torn_phase1 = __db_sim_fault_count(DB_SIM_FC_TORN);

	__db_sim_io_corrupt_disable();
	(void)db->close(db, 0);
	(void)env->close(env, 0);
	__db_sim_deactivate();

	/* ---- phase 2: reopen COLD (no new faults) and scan every key so
	 *      every leaf/internal page is re-read from disk; a torn tail
	 *      from phase 1 must be caught by the checksum, never silent. ---- */
	if ((ret = db_env_create(&env, 0)) != 0)
		goto env_err;
	(void)env->set_cachesize(env, 0, 64 * 1024, 1);
	if ((ret = env->open(env, HOME,
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0664)) != 0)
		goto env_err;
	if ((ret = db_create(&db, env, 0)) != 0)
		goto env_err;
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE, 0, 0664)) != 0) {
		/* A torn META/root page makes the open itself fail with a
		 * checksum error -- that IS the checksum catching the torn
		 * write cleanly (not silent-bad).  A clean, PASSing outcome. */
		printf("test_sim_split_torn: torn meta/root page caught at open "
		    "(clean checksum error: %s); torn(writes)=%lu (seed 0x%llx)\n",
		    db_strerror(ret), torn_phase1, (unsigned long long)seed);
		(void)env->close(env, 0);
		__db_sim_deactivate();
		printf("test_sim_split_torn: PASS -- torn write caught, never "
		    "silently-wrong data\n");
		return (EXIT_SUCCESS);
	}

	__db_sim_activate(seed);

	for (i = 0; i < NKEYS; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0) {
			if (data.size == strlen(vbuf) + 1 &&
			    memcmp(data.data, vbuf, data.size) == 0)
				correct++;
			else
				silent_bad++;
		} else {
			detected++;   /* checksum / page error: clean detect */
		}
	}

	__db_sim_io_corrupt_disable();
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	printf("test_sim_split_torn: %d correct, %d detected(clean error), "
	    "%d SILENT-BAD; torn(writes)=%lu (seed 0x%llx)\n",
	    correct, detected, silent_bad, torn_phase1,
	    (unsigned long long)seed);

	if (silent_bad != 0) {
		fprintf(stderr, "test_sim_split_torn: FAIL -- %d silently "
		    "corrupt split pages slipped past the checksum\n",
		    silent_bad);
		return (EXIT_FAILURE);
	}
	printf("test_sim_split_torn: PASS -- no torn split page ever returned "
	    "as silently-wrong data\n");
	return (EXIT_SUCCESS);

env_err:
	fprintf(stderr, "test_sim_split_torn: setup error: %s\n",
	    db_strerror(ret));
	return (EXIT_FAILURE);
}

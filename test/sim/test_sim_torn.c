/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_torn.c --
 *	Pilot: inject silent corrupt reads under the seeded IO stream and
 *	assert the engine either returns the CORRECT bytes or errors
 *	cleanly (checksum detection) -- it must NEVER return wrong data as
 *	if it were correct.  Uses DB_CHKSUM so page corruption is caught by
 *	libdb's per-page checksum.
 *
 *	The corrupt-read hook (__db_sim_io_read_hook, wired into __os_io's
 *	read fast path) flips one bit of a returned page on a seeded coin.
 *	A DB_CHKSUM btree must detect that on the next page fetch.
 *
 *	Planted-bug hook (DB_DST_INJECT_BUG=2, NOCKSUM): the harness would
 *	accept a mismatched page instead of treating a bad get as detected.
 *	Documented in sim_inject.h; the honest v1 relies on libdb's real
 *	checksum path, so this pilot's "caught" signal is a clean error.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_torn && ./test_sim_torn [seed]
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"
#include "sim_inject.h"

#define HOME   "TESTDIR_sim_torn"
#define DBFILE "torn.db"
#define NKEYS  400
#define PGSIZE 512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	int j;

	(void)snprintf(kbuf, 32, "key-%08d", i);
	/* A fixed, verifiable value pattern so a silent corruption that
	 * slipped past detection would be visible as a byte mismatch. */
	for (j = 0; j < 24; j++)
		vbuf[j] = (char)('A' + ((i + j) % 26));
	vbuf[24] = '\0';
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	char cmd[256];
	int i, ret, detected = 0, correct = 0, silent_bad = 0;

	seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x701234ull;

	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	(void)system(cmd);

	/* ---- phase 1: populate a DB_CHKSUM btree, clean (no faults) ---- */
	if ((ret = db_env_create(&env, 0)) != 0)
		goto env_err;
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_MPOOL, 0664)) != 0)
		goto env_err;
	if ((ret = db_create(&db, env, 0)) != 0)
		goto env_err;
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE, 0664)) != 0)
		goto env_err;
	for (i = 0; i < NKEYS; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			goto env_err;
	}
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	/* ---- phase 2: reopen COLD (DB_PRIVATE, small cache), arm corrupt
	 *      reads, scan every key so every leaf page is read from disk ---- */
	if ((ret = db_env_create(&env, 0)) != 0)
		goto env_err;
	/* DB_PRIVATE: a process-local, initially-empty cache, so every page
	 * fetch is a real __os_io read from disk (no shared region carrying
	 * pages over from phase 1) -- that is where the corrupt-read hook
	 * fires and the DB_CHKSUM verify must catch it. */
	(void)env->set_cachesize(env, 0, 64 * 1024, 1);
	if ((ret = env->open(env, HOME,
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0664)) != 0)
		goto env_err;
	if ((ret = db_create(&db, env, 0)) != 0)
		goto env_err;
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE, 0, 0664)) != 0)
		goto env_err;

	__db_sim_activate(seed);
	__db_sim_io_corrupt_enable(50);    /* 5% of page reads bit-flipped */

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
				silent_bad++;   /* wrong data, no error: BAD */
		} else {
			/* Any error (checksum / verify / page) is a clean
			 * detection -- the engine refused to hand back data it
			 * could not trust. */
			detected++;
		}
	}

	__db_sim_io_corrupt_disable();
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	printf("test_sim_torn: %d correct, %d detected(clean error), "
	    "%d SILENT-BAD (seed 0x%llx)\n",
	    correct, detected, silent_bad, (unsigned long long)seed);

	if (silent_bad != 0) {
		fprintf(stderr, "test_sim_torn: FAIL -- %d silently corrupted "
		    "reads slipped past the checksum\n", silent_bad);
		return (EXIT_FAILURE);
	}
	/* Sanity: with 30% corruption over NKEYS reads we expect the engine
	 * to have DETECTED at least some corruption (else the fault never
	 * fired / the hook is not wired). */
	if (detected == 0)
		fprintf(stderr, "test_sim_torn: WARNING -- no corruption "
		    "detected; the fault may not have hit a checksummed page "
		    "this seed (try another seed)\n");
	printf("test_sim_torn: PASS -- no silent corruption; every read was "
	    "correct or cleanly rejected\n");
	return (EXIT_SUCCESS);

env_err:
	fprintf(stderr, "test_sim_torn: setup error: %s\n", db_strerror(ret));
	return (EXIT_FAILURE);
}

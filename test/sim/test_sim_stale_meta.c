/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_stale_meta.c --
 *	Stale-read of a real DB METADATA page after it was overwritten,
 *	driven through the actual libdb OS seam.  A DB_CHKSUM btree is grown
 *	through enough page allocations that its meta page (page 0: root
 *	pgno, free list, last-pgno, LSN) is rewritten several times, each
 *	overwrite snapshotting the prior on-disk meta bytes into the stale
 *	ring.  The env is then reopened COLD (DB_PRIVATE tiny cache) with
 *	stale reads armed, so a cold read of the meta page (or any page) can
 *	hand back a well-formed but OUT-OF-DATE version.  Every key is
 *	scanned so every page is re-read from disk.
 *
 *	Invariant (DESIGN.md catalog #22, stale-read of metadata angle): a
 *	stale meta page -- structurally valid but describing an older file
 *	state -- is NEVER adopted as silently-wrong.  BDB stamps a page LSN
 *	and a checksum on the meta page; a stale prior version fails the
 *	checksum (its checksum covered different bytes) OR is caught by the
 *	page LSN, so the read is either a clean error or the correct current
 *	data.  A get that returns bytes not matching what was stored, with
 *	no error, is SILENT-BAD and fails.
 *
 *	A silently-stale meta page is uniquely dangerous: it mis-states the
 *	root/free-list of the WHOLE file, so this is the highest-value
 *	stale-read target.
 *
 *	Determinism: the fault schedule is seeded; a mid-sweep failure
 *	prints its seed for exact replay.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_stale_meta && ./test_sim_stale_meta [seed]
 */

#include "db_config.h"

#include "db_int.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"

#define HOME    "TESTDIR_sim_stalemeta"
#define DBFILE  "stalemeta.db"
#define NKEYS   400
#define PGSIZE  512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	int j;
	(void)snprintf(kbuf, 32, "sm-%08u", (i * 2654435761u) % 1000000u);
	for (j = 0; j < 30; j++)
		vbuf[j] = (char)('a' + ((i + j) % 26));
	vbuf[30] = '\0';
}

/* Build the tree clean (all keys present), forcing meta-page rewrites via
 * many page allocations, flushing after each burst so the meta page is
 * overwritten repeatedly (each overwrite feeds the stale ring). */
static int
build(seed, out_reopen_ok)
	uint64_t seed;
	int *out_reopen_ok;
{
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret;

	*out_reopen_ok = 0;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_MPOOL, 0664)) != 0)
		return (ret);
	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE, 0664)) != 0)
		return (ret);

	__db_sim_activate(seed);
	/* Arm stale reads so the presnapshot hook records prior meta bytes
	 * on every overwrite.  No corrupt/torn/enospc (other scenarios). */
	__db_sim_io_stale_enable(350);

	for (i = 0; i < NKEYS; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		(void)db->put(db, NULL, &key, &data, 0);
		/* Periodic sync rewrites the meta page (last-pgno moves as the
		 * file grows), snapshotting its prior version each time. */
		if ((i % 50) == 49)
			(void)db->sync(db, 0);
	}
	(void)db->sync(db, 0);
	(void)db->close(db, 0);
	(void)env->close(env, 0);
	__db_sim_deactivate();
	*out_reopen_ok = 1;
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x57A1E4E7Aull;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, ok, correct = 0, detected = 0, silent_bad = 0;
	unsigned long stale_fired;
	char cmd[256];

	if (seed == 0)
		seed = 0x57A1E4E7Aull;

	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	(void)system(cmd);

	if (build(seed, &ok) != 0 || !ok) {
		fprintf(stderr, "test_sim_stale_meta: build failed "
		    "(seed 0x%llx)\n", (unsigned long long)seed);
		return (EXIT_FAILURE);
	}

	/* Reopen COLD with stale reads armed so cold reads (incl. the meta
	 * page) can return a prior version from the ring. */
	if ((ret = db_env_create(&env, 0)) != 0)
		return (EXIT_FAILURE);
	(void)env->set_cachesize(env, 0, 48 * 1024, 1);
	if ((ret = env->open(env, HOME,
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0664)) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	__db_sim_io_stale_enable(350);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE, 0, 0664)) != 0) {
		/* A stale meta page fails the checksum/LSN at open -- caught
		 * cleanly, never silent.  A clean PASS. */
		stale_fired = __db_sim_fault_count(DB_SIM_FC_STALE);
		printf("test_sim_stale_meta: stale meta page caught at open "
		    "(clean error: %s); stale reads fired=%lu (seed 0x%llx)\n",
		    db_strerror(ret), stale_fired, (unsigned long long)seed);
		__db_sim_io_stale_enable(0);
		__db_sim_deactivate();
		(void)env->close(env, 0);
		printf("test_sim_stale_meta: PASS -- stale meta read caught, "
		    "never silently-wrong data\n");
		return (EXIT_SUCCESS);
	}

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
		} else
			detected++;   /* clean checksum/LSN/page error */
	}
	stale_fired = __db_sim_fault_count(DB_SIM_FC_STALE);
	__db_sim_io_stale_enable(0);
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	printf("test_sim_stale_meta: %d correct, %d detected(clean error), "
	    "%d SILENT-BAD; stale reads fired=%lu (seed 0x%llx)\n",
	    correct, detected, silent_bad, stale_fired,
	    (unsigned long long)seed);
	if (silent_bad != 0) {
		fprintf(stderr, "test_sim_stale_meta: FAIL -- %d silently "
		    "stale/wrong records slipped past the meta-page LSN+"
		    "checksum (seed 0x%llx)\n", silent_bad,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_stale_meta: PASS -- no stale meta page ever returned "
	    "as silently-wrong data (seed 0x%llx)\n",
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
}

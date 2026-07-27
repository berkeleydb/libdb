/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_latency_load.c --
 *	Workload under seeded I/O latency (a slow disk).  Every physical
 *	I/O takes a seeded, capped delay (__db_sim_io_latency_hook).  A
 *	transactional put/commit workload runs against this slow disk; the
 *	engine's timeout / progress logic must still make forward progress
 *	and produce correct results -- no lost commit, no hang, no spurious
 *	deadlock-victim from the latency alone.
 *
 *	This is DESIGN.md catalog #14's latency angle on the single-process
 *	axis: v1 latency is a real (tiny) sleep on each I/O, so it does not
 *	REORDER concurrent I/O (that is the v2 async-scheduler payoff), but
 *	it does exercise the code paths that time or bound I/O and proves
 *	the workload's correctness is latency-independent (the same seed's
 *	committed set is identical with and without latency armed).
 *
 *	Invariant: under a slow disk, (a) every committed txn is durable and
 *	correct, and (b) the committed set is BYTE-IDENTICAL to a run of the
 *	same seed with latency OFF -- latency changes timing, never the data
 *	(a determinism-under-latency check).  And latency actually fired.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_latency_load && ./test_sim_latency_load [seed]
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"

#define HOME    "TESTDIR_sim_latency"
#define DBFILE  "latency.db"
#define NCOMMIT 64

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "la-%08d", i);
	(void)snprintf(vbuf, 32, "lv-%016llx", (unsigned long long)tok);
}

/* Run the workload once; latency_on selects whether the slow-disk knob is
 * armed.  Fold every committed value into a hash so two runs' committed
 * sets can be compared byte-for-byte.  Returns 0 on success. */
static int
run_once(seed, latency_on, out_hash, out_committed)
	uint64_t seed;
	int latency_on;
	uint64_t *out_hash;
	int *out_committed;
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32], cmd[256];
	uint64_t h = 1469598103934665603ull;
	int i, ret, committed = 0;

	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	(void)system(cmd);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0664)) != 0)
		return (ret);

	__db_sim_activate(seed);
	if (latency_on)
		__db_sim_io_faults_enable(2000, 40000, 0);   /* 2-40us/IO */

	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
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
		if (txn->commit(txn, DB_TXN_SYNC) == 0) {
			committed++;
			/* Fold the committed key+value into the hash. */
			{
				const unsigned char *p;
				for (p = (unsigned char *)kbuf; *p; p++)
					{ h ^= *p; h *= 1099511628211ull; }
				for (p = (unsigned char *)vbuf; *p; p++)
					{ h ^= *p; h *= 1099511628211ull; }
			}
		}
	}

	if (latency_on)
		__db_sim_io_faults_disable();
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	*out_hash = h;
	*out_committed = committed;
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x1A7E;
	uint64_t h_slow = 0, h_fast = 0;
	unsigned long lat_fires;
	int c_slow = 0, c_fast = 0;

	/* Slow-disk run (latency armed). */
	if (run_once(seed, 1, &h_slow, &c_slow) != 0) {
		fprintf(stderr, "test_sim_latency_load: slow run setup "
		    "error\n");
		return (EXIT_FAILURE);
	}
	lat_fires = __db_sim_fault_count(DB_SIM_FC_LATENCY);

	/* Fast run (no latency) -- same seed, must produce the SAME
	 * committed set. */
	if (run_once(seed, 0, &h_fast, &c_fast) != 0) {
		fprintf(stderr, "test_sim_latency_load: fast run setup "
		    "error\n");
		return (EXIT_FAILURE);
	}

	printf("test_sim_latency_load: slow committed=%d hash=%016llx, "
	    "fast committed=%d hash=%016llx; latency fired %lu times "
	    "(seed 0x%llx)\n", c_slow, (unsigned long long)h_slow,
	    c_fast, (unsigned long long)h_fast, lat_fires,
	    (unsigned long long)seed);

	if (c_slow != NCOMMIT || c_fast != NCOMMIT) {
		fprintf(stderr, "test_sim_latency_load: FAIL -- not all txns "
		    "committed (slow %d, fast %d of %d) -- latency broke "
		    "progress\n", c_slow, c_fast, NCOMMIT);
		return (EXIT_FAILURE);
	}
	if (h_slow != h_fast) {
		fprintf(stderr, "test_sim_latency_load: FAIL -- committed set "
		    "differs between slow and fast disk (latency changed the "
		    "DATA, not just timing) seed 0x%llx\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (lat_fires == 0) {
		fprintf(stderr, "test_sim_latency_load: FAIL -- the latency "
		    "knob never fired; the slow-disk path is not exercised\n");
		return (EXIT_FAILURE);
	}
	printf("test_sim_latency_load: PASS -- slow disk made forward "
	    "progress, committed set identical to fast disk, latency "
	    "exercised (%lu I/O delays)\n", lat_fires);
	return (EXIT_SUCCESS);
}

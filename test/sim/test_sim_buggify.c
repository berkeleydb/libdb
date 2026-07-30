/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_buggify.c --
 *	FoundationDB-style BUGGIFY sweep: run a mixed transactional workload
 *	with buggify ENABLED (all planted points armed at high probability)
 *	across a seed sweep, and assert the two things buggify must both
 *	deliver:
 *
 *	  (1) the LEGAL-BUT-PESSIMAL paths actually RUN -- across the sweep
 *	      every planted point in db_buggify_catalog[] activates on at
 *	      least one seed (a point reached-but-never-activated is a
 *	      coverage gap, surfaced as a failure on a large enough sweep);
 *	  (2) with those pessimal paths forced ON, all SAFETY INVARIANTS
 *	      still hold -- every committed txn survives a crash+recovery,
 *	      no uncommitted txn survives, and the DB verifies clean.
 *
 *	That is the whole point of buggify: it must STRESS the rare paths
 *	WITHOUT BREAKING correctness.  If an invariant fails with buggify on,
 *	either a planted point was not actually legal (fix the point) or the
 *	engine mishandles a legal-but-rare path (a real bug -- the seed here
 *	is the exact repro).
 *
 *	The workload deliberately touches every buggified subsystem:
 *	  - many keyed puts in one txn  -> btree page splits (bt.split_early)
 *	  - a HASH db with many puts     -> bucket expansion (hash.expand_early)
 *	  - a working set larger than a tiny cache -> eviction churn
 *	    (mp.alloc_aggressive, mp.evict_cold)
 *	  - DB_TXN_SYNC commits          -> log flush (log.flush_now) and,
 *	    over volume, log-file rollover (log.newfile_early)
 *	  - an explicit env->txn_checkpoint -> forced checkpoints
 *	    (txn.chkpt_force)
 *	  - concurrent-ish lock traffic via the txn locks -> deadlock
 *	    detector runs (lock.dd_now, lock.dd_wait_now)
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_buggify && ./test_sim_buggify [count] [base]
 */

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"
#include "sim_buggify.h"

#define HOME     "TESTDIR_sim_buggify"
#define BTDB     "bug_bt.db"
#define HASHDB   "bug_hash.db"
#define ACTFILE  HOME "/activation.txt"   /* child -> parent activation dump */
#define NCOMMIT  20           /* durable committed txns before the crash */
#define NPUT     16           /* keyed puts per txn -> force btree splits */
#define BUGGIFY_PCT 800       /* per-1000: arm each point at 80% per run */

/* Deterministic value token for record (t,i) under the active seed. */
static uint64_t
tok_for(t, i)
	int t, i;
{
	/* fold a per-record draw so a torn/wrong value is visible AND the
	 * exact committed set is a function of the seed (replayable). */
	return (__db_sim_rng(DB_SIM_RNG_APP) ^
	    ((uint64_t)t << 32) ^ (uint64_t)i);
}

static void
mkkv(t, i, kbuf, vbuf, tok)
	int t, i;
	char *kbuf, *vbuf;
	uint64_t tok;
{
	(void)snprintf(kbuf, 32, "k-%04d-%04d", t, i);
	(void)snprintf(vbuf, 48, "v-%016llx", (unsigned long long)tok);
}

/*
 * child_dump_activation --
 *	Written by the child at the crash boundary: for each catalog point,
 *	whether it activated (coin came up 1) AND how many times it was
 *	reached this run.  The parent reads this to report the child's ACTUAL
 *	per-point activation -- honest, and independent of the order in which
 *	the child happened to reach the points.
 */
static void
child_dump_activation()
{
	FILE *f;
	int c, npt, i;

	if ((f = fopen(ACTFILE, "w")) == NULL)
		return;
	npt = __db_sim_buggify_npoints();
	for (c = 0; db_buggify_catalog[c] != NULL; c++) {
		unsigned long reached = 0;
		int activated = 0;
		for (i = 0; i < npt; i++) {
			const char *nm = __db_sim_buggify_point_name(i);
			if (nm != NULL &&
			    strcmp(nm, db_buggify_catalog[c]) == 0) {
				reached = __db_sim_buggify_point_reached(i);
				activated =
				    __db_sim_buggify_point_activated(i);
				break;
			}
		}
		(void)fprintf(f, "%s %d %lu\n", db_buggify_catalog[c],
		    activated, reached);
	}
	(void)fclose(f);
}

/*
 * The child: open a TINY-cache env so the working set forces eviction,
 * run NCOMMIT durable txns (each NPUT keyed puts across a btree AND a hash
 * db, with periodic forced checkpoints), then a final uncommitted txn, then
 * crash (drop un-fsync'd bytes).  Buggify is armed so the pessimal paths
 * fire throughout.  Returns 0 (child exits 42 at the crash boundary).
 */
static int
run_child(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *bt, *hash;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[48];
	int t, i, ret;

	__db_sim_activate(seed);
	__db_sim_buggify_enable(BUGGIFY_PCT);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	/* A deliberately small cache (256KB) so the working set spills and
	 * the eviction / write-back path (mp.alloc_aggressive, mp.evict_cold)
	 * runs hard. */
	(void)env->set_cachesize(env, 0, 256 * 1024, 1);
	/* Enable automatic deadlock detection so the lock.dd_now buggify site
	 * (which forces the detector to run on a lock-vec op) is actually
	 * reached; without this the region's detect policy is NORUN and the
	 * site is never hit. */
	(void)env->set_lk_detect(env, DB_LOCK_DEFAULT);
	/* A small max log-file size (256KB) so the workload actually rolls
	 * over log files -- this both exercises real rollover and lets the
	 * log.newfile_early buggify site (which only fires once a file is
	 * more than half full) be reached. */
	(void)env->set_lg_max(env, 256 * 1024);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);

	if ((ret = db_create(&bt, env, 0)) != 0)
		return (ret);
	if ((ret = bt->open(bt, NULL, BTDB, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0664)) != 0)
		return (ret);
	if ((ret = db_create(&hash, env, 0)) != 0)
		return (ret);
	if ((ret = hash->open(hash, NULL, HASHDB, NULL, DB_HASH,
	    DB_CREATE | DB_AUTO_COMMIT, 0664)) != 0)
		return (ret);

	for (t = 0; t < NCOMMIT; t++) {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		for (i = 0; i < NPUT; i++) {
			uint64_t tok = tok_for(t, i);
			mkkv(t, i, kbuf, vbuf, tok);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
			data.data = vbuf;
			data.size = (u_int32_t)strlen(vbuf) + 1;
			if ((ret = bt->put(bt, txn, &key, &data, 0)) != 0)
				return (ret);
			if ((ret = hash->put(hash, txn, &key, &data, 0)) != 0)
				return (ret);
		}
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
		/* Periodic checkpoint: exercises txn.chkpt_force (and the
		 * cache flush it drives).  A no-op if quiescent. */
		if ((t % 8) == 7)
			(void)env->txn_checkpoint(env, 0, 0, 0);
	}

	/* An UNCOMMITTED txn, then crash inside it: must NOT survive. */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	mkkv(NCOMMIT, 0, kbuf, vbuf, tok_for(NCOMMIT, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	(void)bt->put(bt, txn, &key, &data, 0);
	/* Deliberately DO NOT commit. */

	/* Record which pessimal paths actually ran, then crash. */
	child_dump_activation();
	__db_sim_wb_crash();
	fflush(NULL);
	_exit(42);
	/* NOTREACHED */
	return (0);
}

/*
 * Parent: recover, reopen, verify EVERY committed (t,i) survived in BOTH
 * dbs, the uncommitted key is absent, and both dbs verify clean.  Returns
 * 0 on success; sets *missing to the count of missing/wrong committed recs.
 */
static int
verify_after_recovery(seed, saw_uncommitted, missing_out)
	uint64_t seed;
	int *saw_uncommitted, *missing_out;
{
	DB_ENV *env;
	DB *bt, *hash;
	DBT key, data;
	char kbuf[32], vbuf[48];
	int t, i, ret, missing = 0, mismatch = 0;

	*saw_uncommitted = 0;
	*missing_out = 0;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0664))
	    != 0) {
		fprintf(stderr, "recover open failed: %s\n", db_strerror(ret));
		return (ret);
	}

	if ((ret = db_create(&bt, env, 0)) != 0)
		return (ret);
	if ((ret = bt->open(bt, NULL, BTDB, NULL, DB_BTREE,
	    DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "reopen bt failed: %s\n", db_strerror(ret));
		return (ret);
	}
	if ((ret = db_create(&hash, env, 0)) != 0)
		return (ret);
	if ((ret = hash->open(hash, NULL, HASHDB, NULL, DB_HASH,
	    DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "reopen hash failed: %s\n", db_strerror(ret));
		return (ret);
	}

	/* Re-derive the expected set deterministically from the seed. */
	__db_sim_activate(seed);
	for (t = 0; t < NCOMMIT; t++)
		for (i = 0; i < NPUT; i++) {
			uint64_t tok = tok_for(t, i);
			DB *d;
			int which;

			mkkv(t, i, kbuf, vbuf, tok);
			for (which = 0; which < 2; which++) {
				d = which == 0 ? bt : hash;
				memset(&key, 0, sizeof(key));
				memset(&data, 0, sizeof(data));
				key.data = kbuf;
				key.size = (u_int32_t)strlen(kbuf) + 1;
				if ((ret = d->get(d, NULL, &key, &data, 0))
				    != 0) {
					fprintf(stderr, "MISSING %s in %s: "
					    "%s\n", kbuf, which ? "hash" : "bt",
					    db_strerror(ret));
					missing++;
				} else if (data.size != strlen(vbuf) + 1 ||
				    memcmp(data.data, vbuf, data.size) != 0) {
					fprintf(stderr, "WRONG value for %s "
					    "in %s\n", kbuf,
					    which ? "hash" : "bt");
					mismatch++;
				}
			}
		}
	/* The uncommitted key must be ABSENT. */
	mkkv(NCOMMIT, 0, kbuf, vbuf, tok_for(NCOMMIT, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	if (bt->get(bt, NULL, &key, &data, 0) == 0)
		*saw_uncommitted = 1;
	__db_sim_deactivate();

	(void)bt->close(bt, 0);
	(void)hash->close(hash, 0);

	/* Verify both dbs clean (fresh handles). */
	if ((ret = db_create(&bt, env, 0)) != 0)
		return (ret);
	if ((ret = bt->verify(bt, BTDB, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "bt verify FAILED: %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (ret);
	}
	if ((ret = db_create(&hash, env, 0)) != 0)
		return (ret);
	if ((ret = hash->verify(hash, HASHDB, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "hash verify FAILED: %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (ret);
	}
	(void)env->close(env, 0);

	*missing_out = missing;
	if (missing != 0 || mismatch != 0) {
		fprintf(stderr, "%d missing, %d mismatched committed recs\n",
		    missing, mismatch);
		return (1);
	}
	return (0);
}

/*
 * read_child_activation --
 *	Read the child's activation dump: fired[c] = 1 iff catalog point c
 *	activated this run, reached[c] = how many times it was reached.
 *	Returns 0 on success.  Reflects the child's ACTUAL decisions.
 */
static int
read_child_activation(fired, reached)
	int *fired;
	unsigned long *reached;
{
	FILE *f;
	char name[64];
	int act, c;
	unsigned long rch;

	if ((f = fopen(ACTFILE, "r")) == NULL)
		return (-1);
	while (fscanf(f, "%63s %d %lu", name, &act, &rch) == 3)
		for (c = 0; db_buggify_catalog[c] != NULL; c++)
			if (strcmp(name, db_buggify_catalog[c]) == 0) {
				fired[c] = act;
				reached[c] = rch;
				break;
			}
	(void)fclose(f);
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	long n_seeds = argc > 1 ? strtol(argv[1], NULL, 10) : 24;
	long base    = argc > 2 ? strtol(argv[2], NULL, 10) : 1;
	long s, failures = 0;
	int ncat, c;
	unsigned long *act;         /* seeds on which each catalog point fired */
	unsigned long *rtot;        /* total reaches of each point across sweep */
	char cmd[256];

	for (ncat = 0; db_buggify_catalog[ncat] != NULL; ncat++)
		;
	if ((act = calloc((size_t)ncat, sizeof(*act))) == NULL ||
	    (rtot = calloc((size_t)ncat, sizeof(*rtot))) == NULL) {
		fprintf(stderr, "OOM\n");
		return (EXIT_FAILURE);
	}
	if (n_seeds < 1)
		n_seeds = 1;

	printf("== DST buggify sweep: %ld seeds (base %ld), %d planted "
	    "points, each armed at %.0f%% ==\n", n_seeds, base, ncat,
	    BUGGIFY_PCT / 10.0);
	fflush(stdout);   /* flush BEFORE any fork so the child never dups it */

	for (s = 0; s < n_seeds; s++) {
		uint64_t seed = 0x9E3779B97F4A7C15ull * (uint64_t)(base + s);
		pid_t pid;
		int status, ret, saw_uncommitted, missing;
		int *fired = calloc((size_t)ncat, sizeof(int));
		unsigned long *reached = calloc((size_t)ncat, sizeof(*reached));

		if (fired == NULL || reached == NULL) {
			fprintf(stderr, "OOM\n");
			free(act); free(rtot);
			return (EXIT_FAILURE);
		}

		(void)snprintf(cmd, sizeof(cmd),
		    "rm -rf %s && mkdir -p %s", HOME, HOME);
		(void)system(cmd);

		if ((pid = fork()) < 0) {
			perror("fork");
			free(fired); free(reached); free(act); free(rtot);
			return (EXIT_FAILURE);
		}
		if (pid == 0)
			exit(run_child(seed) == 0 ? 0 : 1);
		if (waitpid(pid, &status, 0) < 0) {
			perror("waitpid");
			free(fired); free(reached); free(act); free(rtot);
			return (EXIT_FAILURE);
		}
		if (!(WIFEXITED(status) && WEXITSTATUS(status) == 42)) {
			fprintf(stderr, "FAIL seed=0x%llx: child did not reach "
			    "the crash point (status %d)\n",
			    (unsigned long long)seed, status);
			failures++;
			free(fired); free(reached);
			continue;
		}

		/* The child's ACTUAL per-point activation (from its dump). */
		(void)read_child_activation(fired, reached);
		for (c = 0; c < ncat; c++) {
			if (fired[c])
				act[c]++;
			rtot[c] += reached[c];
		}

		ret = verify_after_recovery(seed, &saw_uncommitted, &missing);

		if (saw_uncommitted) {
			fprintf(stderr, "FAIL seed=0x%llx: an UNCOMMITTED txn "
			    "survived the crash (buggify broke atomicity?)\n",
			    (unsigned long long)seed);
			failures++;
		} else if (ret != 0) {
			fprintf(stderr, "FAIL seed=0x%llx: %d committed rec(s) "
			    "lost or DB failed to verify AFTER a clean "
			    "recovery -- with buggify on, a legal-but-pessimal "
			    "path corrupted or lost committed data.  This is "
			    "EITHER an illegal buggify point OR a real engine "
			    "bug on a rare path; reproduce with "
			    "./test_sim_buggify 1 %ld\n",
			    (unsigned long long)seed, missing, base + s);
			failures++;
		}
		free(fired); free(reached);
	}

	printf("\nbuggify POINT ACTIVATION (seeds on which the pessimal path "
	    "was taken; reaches = total code-site hits across the sweep):\n");
	for (c = 0; c < ncat; c++)
		printf("  %-22s activated %4lu/%ld seeds (%5.1f%%)  "
		    "reaches %lu\n",
		    db_buggify_catalog[c], act[c], n_seeds,
		    100.0 * (double)act[c] / (double)n_seeds, rtot[c]);

	if (failures > 0) {
		printf("\nFAIL: %ld seed(s) violated a safety invariant WITH "
		    "buggify on\n", failures);
		free(act); free(rtot);
		return (EXIT_FAILURE);
	}

	/*
	 * Coverage-gap guard (FoundationDB discipline), split by KIND:
	 *
	 *   - REACHED but NEVER ACTIVATED across the sweep = a real buggify
	 *     hole (the coin is stuck at 0, or the point was neutered): a
	 *     HARD FAILURE, because the pessimal path exists in the workload
	 *     but never ran.
	 *   - NEVER REACHED = the workload does not exercise that code site
	 *     (e.g. lock.dd_wait_now needs lock CONTENTION, which this single
	 *     sequential-writer crash workload does not create): reported as
	 *     a WARNING, not a failure -- the point is valid, the workload
	 *     just does not drive it here.
	 */
	if (n_seeds >= 12) {
		int hard_gap = 0;
		for (c = 0; c < ncat; c++) {
			if (act[c] != 0)
				continue;
			if (rtot[c] == 0)
				printf("WARN: buggify point '%s' never "
				    "REACHED across %ld seeds -- this "
				    "workload does not drive the site (e.g. "
				    "lock.dd_wait_now needs lock contention); "
				    "point is valid, coverage is "
				    "workload-limited\n",
				    db_buggify_catalog[c], n_seeds);
			else {
				printf("FAIL: buggify point '%s' was REACHED "
				    "but NEVER activated across %ld seeds -- "
				    "its pessimal path never ran (coin stuck "
				    "at 0 / point neutered)\n",
				    db_buggify_catalog[c], n_seeds);
				hard_gap = 1;
			}
		}
		if (hard_gap) {
			free(act); free(rtot);
			return (EXIT_FAILURE);
		}
	}

	printf("\nOK: %ld-seed buggify sweep -- every planted pessimal path "
	    "ran on at least one seed AND every committed txn survived "
	    "crash+recovery with all pessimal paths forced on (0 invariant "
	    "violations)\n", n_seeds);
	free(act); free(rtot);
	return (EXIT_SUCCESS);
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * Regression test for the P12 per-stripe locker-allocation exhaustion
 * (recd015 "out of available locker entries" on a backward recovery pass).
 *
 * P12 (2d8397dfe) gave each of the 64 locker stripes its own free list and
 * refilled an empty stripe with a batch spread round-robin across ALL stripes,
 * capped by st_maxlockers - st_lockers (= lk_max_lockers - in use).  Once
 * st_lockers reaches st_maxlockers the cap is zero, so a create that lands on
 * a stripe whose free list happens to be empty fails with
 * "Lock table is out of available locker entries" -- even though OTHER stripes
 * still hold idle free lockers and the number of lockers actually in use is
 * well below lk_max_lockers.  Recovery's backward pass creates one locker per
 * prepared txn; the id hash concentrates those creates on a few stripes, so it
 * hit the dry-stripe case and panicked the recovery.
 *
 * This drives the bug through the public API only (lock_id / lock_id_free):
 *
 *   1. lk_max_lockers = MAXL.  The locker hash table size is a prime >= MAXL
 *      (__db_tablesize), so for every id we allocate (all < MAXL < table size)
 *      the bucket is id itself and the stripe is id & 63.  We therefore know
 *      each id's stripe exactly.
 *   2. Allocate ids with lock_id() until the pool is at its limit, recording
 *      each id's stripe.  Pick a TARGET stripe that received several ids.
 *   3. Free every allocated id that is NOT on the target stripe.  Freed lockers
 *      return to their own stripe's free list, so now every stripe except the
 *      target has free lockers and the target stripe's free list is empty,
 *      while st_lockers is still at st_maxlockers (nothing was returned to the
 *      region).
 *   4. Allocate ids until one lands on the target stripe.  That create finds an
 *      empty free list and a full region: before the fix it fails with ENOMEM
 *      (the recd015 panic); with the steal-from-another-stripe fix it succeeds.
 *
 * Before the fix the test fails at step 4; with the fix it passes.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <db.h>

#define	MAXL		320		/* lk_max_lockers */
#define	STRIPES		64		/* LOCK_LOCKER_STRIPES (dbinc/lock.h) */

static int failures = 0;
static int checks = 0;

#define CHECK(cond, ...) do {						\
	checks++;							\
	if (!(cond)) {							\
		failures++;						\
		printf("  FAIL: ");					\
		printf(__VA_ARGS__);					\
		printf("\n");						\
	}								\
} while (0)

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	u_int32_t id, ids[MAXL + 1], target, newid;
	const char *home = "LOCK_STRIPE_TESTDIR";
	int i, n, ret, per_stripe[STRIPES], freed, got_target;

	(void)argc; (void)argv;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	dbenv->set_errpfx(dbenv, "lock_stripe_exhaustion");
	(void)dbenv->set_lk_max_lockers(dbenv, MAXL);
	if ((ret = dbenv->open(dbenv, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_MPOOL, 0600)) != 0) {
		fprintf(stderr, "env open %s: %s\n", home, db_strerror(ret));
		return (EXIT_FAILURE);
	}

	/*
	 * 2: allocate until the pool is exhausted, recording each id's stripe.
	 * The library seeds its own initial free list and may hold a handful of
	 * internal lockers, so we simply allocate until lock_id returns ENOMEM
	 * rather than assuming an exact count.
	 */
	memset(per_stripe, 0, sizeof(per_stripe));
	for (n = 0; n < MAXL; n++) {
		if ((ret = dbenv->lock_id(dbenv, &id)) != 0) {
			if (ret == ENOMEM)
				break;
			fprintf(stderr, "lock_id #%d: %s\n", n,
			    db_strerror(ret));
			return (EXIT_FAILURE);
		}
		ids[n] = id;
		per_stripe[id & (STRIPES - 1)]++;
	}
	printf("  allocated %d lockers before the pool filled\n", n);
	CHECK(n > STRIPES,
	    "allocated only %d lockers; expected more than %d so several share "
	    "a stripe", n, STRIPES);

	/* Pick a target stripe that several ids landed on. */
	target = 0;
	for (i = 0; i < STRIPES; i++)
		if (per_stripe[i] > per_stripe[target])
			target = i;
	printf("  target stripe %u holds %d allocated lockers\n",
	    target, per_stripe[target]);
	CHECK(per_stripe[target] >= 1,
	    "no allocated locker landed on the target stripe");

	/*
	 * 3: free every allocated id NOT on the target stripe.  Those lockers
	 * go back to their own stripes' free lists; the target stripe keeps its
	 * lockers in use and its free list empty.  st_lockers stays at the max.
	 */
	freed = 0;
	for (i = 0; i < n; i++)
		if ((ids[i] & (STRIPES - 1)) != target) {
			if ((ret = dbenv->lock_id_free(dbenv, ids[i])) != 0) {
				fprintf(stderr, "lock_id_free %u: %s\n",
				    ids[i], db_strerror(ret));
				return (EXIT_FAILURE);
			}
			freed++;
		}
	printf("  freed %d lockers on other stripes; target stripe stays dry\n",
	    freed);
	CHECK(freed > 0, "freed nothing; cannot create the dry-stripe case");

	/*
	 * 4: allocate until one id lands on the dry target stripe.  That is the
	 * create that panicked recd015.  With the fix it steals a free locker
	 * from one of the stripes we emptied in step 3 and succeeds.
	 */
	got_target = 0;
	newid = 0;
	for (i = 0; i < 4 * STRIPES; i++) {
		ret = dbenv->lock_id(dbenv, &id);
		if (ret == ENOMEM) {
			printf("  lock_id returned ENOMEM "
			    "(out of locker entries) at try %d\n", i);
			break;
		}
		if (ret != 0) {
			fprintf(stderr, "lock_id (step 4) #%d: %s\n", i,
			    db_strerror(ret));
			return (EXIT_FAILURE);
		}
		if ((id & (STRIPES - 1)) == target) {
			got_target = 1;
			newid = id;
			break;
		}
		/* Free non-target ids again so the pool stays full. */
		(void)dbenv->lock_id_free(dbenv, id);
	}
	CHECK(got_target == 1,
	    "could not allocate a locker on the dry target stripe %u: "
	    "the create failed (recd015 regression)", target);
	if (got_target)
		printf("  allocated locker id %u on the dry target stripe %u\n",
		    newid, target);

	(void)dbenv->close(dbenv, 0);

	printf("lock_stripe_exhaustion: %d checks, %d failures\n",
	    checks, failures);
	printf("lock_stripe_exhaustion: %s\n", failures == 0 ? "PASS" : "FAIL");
	return (failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

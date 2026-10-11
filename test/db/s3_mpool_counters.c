/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * s3_mpool_counters.c -- RFC 0015 S3: the two per-op mpool counters.
 *
 * (b) The hash_examined mis-target.  __memp_fget updated st_hash_examined
 *     with STAT_ADJUST_VERB(..., hash_examined, c_mp->stat.st_hash_searches,
 *     st_hsearch, ...) -- the subcat label said hash_examined but the field
 *     written was st_hash_searches, so st_hash_examined was NEVER
 *     incremented (mp_fget.c:492, inherited from Oracle).  After the fix,
 *     N lookups that walk hash chains leave st_hash_examined > 0.  EXACT in
 *     direction: the pre-fix value is 0 no matter how many lookups run.
 *
 * (a) The put_counter move.  ++c_mp->put_counter used to fire on EVERY
 *     __memp_fput (mp_fput.c:147), a shared-line write per page put.  It is
 *     read only by __memp_alloc's give-up heuristic, which cares whether a
 *     buffer became reclaimable in this region.  S3 moves the bump to the
 *     put that actually releases the buffer's last reference.  The signal is
 *     still delivered: after real get/put work, summed put_counter > 0.  The
 *     sabotage that drops the final-release bump leaves it 0.
 *
 * Single-threaded and deterministic.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/mp.h"

#define	HOME	"TESTDIR_s3"
#define	NREC	2000		/* enough keys to build multi-entry chains */
#define	NLOOKUP	4000		/* repeated gets to drive hash searches */

/* Sum put_counter across all cache regions (default nreg == 1). */
static u_int32_t
sum_put_counter(ENV *env)
{
	DB_MPOOL *dbmp;
	MPOOL *mp;
	u_int32_t i, sum;

	dbmp = env->mp_handle;
	mp = dbmp->reginfo[0].primary;
	sum = 0;
	for (i = 0; i < mp->nreg; i++)
		sum += ((MPOOL *)dbmp->reginfo[i].primary)->put_counter;
	return (sum);
}

int
main(int argc, char *argv[])
{
#ifndef HAVE_STATISTICS
	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	printf("VERDICT s3_mpool_counters SKIP built without statistics\n");
	return (0);
#else
	DB *dbp;
	DB_ENV *dbenv;
	DB_MPOOL_STAT *gsp;
	DBT key, data;
	ENV *env;
	u_int32_t i, k, pc;
	int bad, ret;
	char vbuf[256];

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	bad = 0;

	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->set_cachesize(dbenv, 0, 256 * 1024, 1)) != 0 ||
	    (ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_MPOOL |
	    DB_THREAD, 0600)) != 0) {
		printf("VERDICT s3_mpool_counters FAIL env open: %s\n",
		    db_strerror(ret));
		return (1);
	}
	env = dbenv->env;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, "s3.db", NULL, DB_BTREE,
	    DB_CREATE, 0600)) != 0) {
		printf("VERDICT s3_mpool_counters FAIL db open: %s\n",
		    db_strerror(ret));
		return (1);
	}

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 0; i < NREC; i++) {
		k = i;
		key.data = &k;
		key.size = sizeof(k);
		snprintf(vbuf, sizeof(vbuf), "value-%u-padding-padding", i);
		data.data = vbuf;
		data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) != 0) {
			printf("VERDICT s3_mpool_counters FAIL put %u: %s\n",
			    i, db_strerror(ret));
			return (1);
		}
	}

	/* Repeated point lookups: each is an fget that walks a hash chain. */
	memset(&data, 0, sizeof(data));
	data.flags = DB_DBT_MALLOC;
	for (i = 0; i < NLOOKUP; i++) {
		k = i % NREC;
		key.data = &k;
		key.size = sizeof(k);
		if ((ret = dbp->get(dbp, NULL, &key, &data, 0)) != 0) {
			printf("VERDICT s3_mpool_counters FAIL get %u: %s\n",
			    i, db_strerror(ret));
			return (1);
		}
		free(data.data);
		data.data = NULL;
	}

	/* Snapshot the allocator progress signal before closing anything. */
	pc = sum_put_counter(env);

	if ((ret = dbenv->memp_stat(dbenv, &gsp, NULL, 0)) != 0) {
		printf("VERDICT s3_mpool_counters FAIL memp_stat: %s\n",
		    db_strerror(ret));
		return (1);
	}

	printf("  st_hash_searches=%lu st_hash_examined=%ju  "
	    "put_counter(sum)=%lu  (%d lookups)\n",
	    (u_long)gsp->st_hash_searches, gsp->st_hash_examined,
	    (u_long)pc, NLOOKUP);

	/* (b): the fget bug.  Pre-fix st_hash_examined is 0 for any workload. */
	if (gsp->st_hash_searches == 0) {
		printf("  FAIL (b): no hash searches recorded -- test did not "
		    "drive the fget path\n");
		bad = 1;
	} else if (gsp->st_hash_examined == 0) {
		printf("  FAIL (b): st_hash_examined is 0 after %d lookups -- "
		    "it is being folded into st_hash_searches (mp_fget.c)\n",
		    NLOOKUP);
		bad = 1;
	}

	/* (a): the allocator signal still fires on buffer release. */
	if (pc == 0) {
		printf("  FAIL (a): put_counter is 0 after %d gets and %d puts "
		    "-- the allocator's give-up signal is never set\n",
		    NLOOKUP, NREC);
		bad = 1;
	}

	free(gsp);
	if ((ret = dbp->close(dbp, 0)) != 0 ||
	    (ret = dbenv->close(dbenv, 0)) != 0) {
		printf("VERDICT s3_mpool_counters FAIL close: %s\n",
		    db_strerror(ret));
		return (1);
	}
	if (bad) {
		printf("VERDICT s3_mpool_counters FAIL\n");
		return (1);
	}
	printf("VERDICT s3_mpool_counters PASS st_hash_examined counts chain "
	    "entries; put_counter signals buffer release\n");
	return (0);
#endif
}

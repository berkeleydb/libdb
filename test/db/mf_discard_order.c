/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * mf_discard_order.c --
 *	Regression test: discarding a written MPOOLFILE must take its hash
 *	bucket before the mpool region latch, not under it.
 *
 *	__memp_mf_discard used to take MPOOL_SYSTEM_LOCK and then call
 *	__memp_mf_sync(locked=0), which takes the file's hash bucket: the
 *	allocator latch (DB_LO_RANK_ALLOC) held while acquiring a file bucket
 *	(DB_LO_RANK_MPOOL_FILE), the inverse of the global order.  The path is
 *	the DB_TRUNCATE open of an existing, written file: its pre-open
 *	probe closes the last handle and discards the MPOOLFILE with a
 *	pending sync.  A --enable-diagnostic build's lock-order checker
 *	PANICs there (BDB2077); a release build has no checker and runs on.
 *
 *	The test does that open, then checks the truncated database is empty
 *	and usable.  Under a diagnostic build the checker is the oracle (a
 *	violation aborts the process); under a release build the test still
 *	checks the functional result.  One VERDICT line.
 */
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include "db.h"

int
main(int argc, char *argv[])
{
	DB_ENV *env;
	DB *dbp;
	DBC *c;
	DBT k, v;
	int i, n, ret;
	char kb[16];

	if (argc != 2) {
		fprintf(stderr, "usage: mf_discard_order home\n");
		return (2);
	}
	/* DB_TRUNCATE is refused with locking or transactions: mpool only. */
	if (db_env_create(&env, 0) != 0 ||
	    env->open(env, argv[1], DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT mf_discard_order FAIL env\n");
		return (1);
	}
	env->set_errfile(env, stdout);

	/* A written file with dirty pages, so the discard has a sync to do. */
	if (db_create(&dbp, env, 0) != 0 || dbp->open(dbp,
	    NULL, "one.db", NULL, DB_BTREE, DB_CREATE, 0600) != 0) {
		printf("VERDICT mf_discard_order FAIL create\n");
		return (1);
	}
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	for (i = 0; i < 500; i++) {
		(void)snprintf(kb, sizeof(kb), "%06d", i);
		k.data = kb;
		k.size = 6;
		v.data = kb;
		v.size = 6;
		(void)dbp->put(dbp, NULL, &k, &v, 0);
	}
	(void)dbp->close(dbp, 0);

	/* The DB_TRUNCATE open: its probe discards the MPOOLFILE. */
	if (db_create(&dbp, env, 0) != 0) {
		printf("VERDICT mf_discard_order FAIL db_create\n");
		return (1);
	}
	ret = dbp->open(dbp, NULL, "one.db", NULL, DB_BTREE,
	    DB_CREATE | DB_TRUNCATE, 0600);
	if (ret != 0) {
		printf("VERDICT mf_discard_order FAIL truncate-open %d (%s)\n",
		    ret, db_strerror(ret));
		return (1);
	}
	k.data = "new";
	k.size = 3;
	v.data = "v";
	v.size = 1;
	ret = dbp->put(dbp, NULL, &k, &v, 0);
	n = 0;
	if (ret == 0 && dbp->cursor(dbp, NULL, &c, 0) == 0) {
		while (c->get(c, &k, &v, DB_NEXT) == 0)
			n++;
		(void)c->close(c);
	}
	(void)dbp->close(dbp, 0);
	(void)env->close(env, 0);
	if (ret != 0 || n != 1) {
		printf("VERDICT mf_discard_order FAIL put=%d rows=%d (want 0, 1)\n",
		    ret, n);
		return (1);
	}
	printf("VERDICT mf_discard_order PASS truncate-open discarded the "
	    "written file without a lock-order violation; 1 row after\n");
	return (0);
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * foreign_disassoc_leak.c --
 *	Regression test for RFC 0014 §8.2: __db_disassociate_foreign mallocs a
 *	DB_FOREIGN_INFO into `tmp' and then overwrites `tmp' before any use, so
 *	every DB->close of a secondary that has a foreign database leaks one
 *	allocation.
 *
 * LeakSanitizer cannot see this: the leak is inside a live environment and
 * the environment closes cleanly either way.  So count allocations instead,
 * through db_env_set_func_malloc / db_env_set_func_free (every __os_malloc
 * and __os_free routes through them).  live = mallocs - frees; realloc is
 * neither, and __os_realloc(NULL) already goes through __os_malloc.
 *
 * Two arms, each 100 x { close S; reopen S; re-associate }:
 *   control  without associate_foreign -- must be flat, which proves the
 *            counter sees handle churn without false positives (and the
 *            loop must have counted mallocs, or "flat" is vacuous);
 *   foreign  with associate_foreign    -- must be flat too.
 * Against the defect the foreign arm grows by exactly NITER.
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

#define	NITER	100

static long live, nmalloc;

static void *
cnt_malloc(size_t n)
{
	void *p;

	if ((p = malloc(n)) != NULL) {
		live++;
		nmalloc++;
	}
	return (p);
}

static void
cnt_free(void *p)
{
	if (p != NULL)
		live--;
	free(p);
}

static void
die(int ret, const char *what)
{
	printf("VERDICT foreign_disassoc_leak FAIL %s: %s\n",
	    what, db_strerror(ret));
	exit(1);
}

static int
skey_cb(DB *sdbp, const DBT *pkey, const DBT *pdata, DBT *skey)
{
	(void)sdbp;
	(void)pkey;
	memset(skey, 0, sizeof(*skey));
	skey->data = pdata->data;
	skey->size = pdata->size;
	return (0);
}

static DB *
open_db(DB_ENV *env, const char *name)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, env, 0)) != 0)
		die(ret, "db_create");
	if ((ret = dbp->open(dbp,
	    NULL, name, NULL, DB_BTREE, DB_CREATE, 0644)) != 0)
		die(ret, name);
	return (dbp);
}

static DB *
open_secondary(DB_ENV *env, DB *pdbp, DB *fdbp)
{
	DB *sdbp;
	int ret;

	sdbp = open_db(env, "s.db");
	if ((ret = pdbp->associate(pdbp, NULL, sdbp, skey_cb, 0)) != 0)
		die(ret, "associate");
	if (fdbp != NULL && (ret = fdbp->associate_foreign(fdbp,
	    sdbp, NULL, DB_FOREIGN_ABORT)) != 0)
		die(ret, "associate_foreign");
	return (sdbp);
}

/*
 * Returns the live-allocation delta across NITER secondary close/reopens;
 * *nm is the number of mallocs the loop made.
 */
static long
arm(DB_ENV *env, int foreign, long *nm)
{
	DB *fdbp, *pdbp, *sdbp;
	long before;
	int i, ret;

	pdbp = open_db(env, "p.db");
	fdbp = open_db(env, "f.db");
	sdbp = open_secondary(env, pdbp, foreign ? fdbp : NULL);

	/* One warm-up cycle so a first-use cache is not counted. */
	if ((ret = sdbp->close(sdbp, 0)) != 0)
		die(ret, "close S (warm-up)");
	sdbp = open_secondary(env, pdbp, foreign ? fdbp : NULL);

	before = live;
	*nm = nmalloc;
	for (i = 0; i < NITER; i++) {
		if ((ret = sdbp->close(sdbp, 0)) != 0)
			die(ret, "close S");
		sdbp = open_secondary(env, pdbp, foreign ? fdbp : NULL);
	}
	before = live - before;
	*nm = nmalloc - *nm;

	if ((ret = sdbp->close(sdbp, 0)) != 0 ||
	    (ret = fdbp->close(fdbp, 0)) != 0 ||
	    (ret = pdbp->close(pdbp, 0)) != 0)
		die(ret, "close");
	return (before);
}

int
main(int argc, char **argv)
{
	DB_ENV *env;
	long dc, df, nc, nf;
	int ret;

	(void)db_env_set_func_malloc(cnt_malloc);
	(void)db_env_set_func_free(cnt_free);

	if ((ret = db_env_create(&env, 0)) != 0)
		die(ret, "db_env_create");
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "foreign_disassoc_leak");
	if ((ret = env->open(env, argc > 1 ? argv[1] : ".",
	    DB_CREATE | DB_INIT_MPOOL, 0644)) != 0)
		die(ret, "env->open");

	dc = arm(env, 0, &nc);
	df = arm(env, 1, &nf);
	printf("control (no associate_foreign): live delta %ld, %ld mallocs, "
	    "%d cycles\n", dc, nc, NITER);
	printf("foreign (associate_foreign):    live delta %ld, %ld mallocs, "
	    "%d cycles\n", df, nf, NITER);

	if ((ret = env->close(env, 0)) != 0)
		die(ret, "env->close");

	/* A counter that saw no mallocs would make both deltas 0 vacuously. */
	if (nc < NITER || nf < NITER)
		printf("VERDICT foreign_disassoc_leak FAIL the malloc hook saw "
		    "%ld/%ld mallocs: it is not counting\n", nc, nf);
	else if (dc != 0)
		printf("VERDICT foreign_disassoc_leak FAIL control arm not flat "
		    "(%ld): the counter is not measuring handle churn\n", dc);
	else if (df != 0)
		printf("VERDICT foreign_disassoc_leak FAIL leaked %ld allocations "
		    "in %d secondary closes\n", df, NITER);
	else
		printf("VERDICT foreign_disassoc_leak PASS\n");
	return (nc >= NITER && nf >= NITER && dc == 0 && df == 0 ? 0 : 1);
}

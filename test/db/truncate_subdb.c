/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * truncate_subdb.c --
 *	Regression test: DB->open with DB_TRUNCATE and a subdatabase name.
 *
 *	DB_TRUNCATE truncates a file, so it cannot be applied to one
 *	subdatabase.  The open used to accept the combination: __db_open's
 *	pre-open truncated the WHOLE file to zero bytes, then the real open
 *	failed with EBADF, and every other subdatabase in the file was gone.
 *
 *	Cases:
 *	  a  subdatabases "keep" (2 rows) and "victim" (1 row) in one file;
 *	     truncate-open "victim" must fail EINVAL, and "keep" must still
 *	     hold 2 rows and "victim" 1 (nothing was touched).
 *	  b  control: DB_TRUNCATE with no subdatabase name still empties a
 *	     one-database file and leaves it usable (the documented use).
 *	One VERDICT line.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include "db.h"

static int nfail;
#define	CK(c, m) do {							\
	if (!(c)) { printf("  FAIL %s\n", m); nfail++; }		\
	else printf("  ok   %s\n", m);					\
} while (0)

static int
put1(DB_ENV *env, const char *file, const char *sub, const char *key)
{
	DB *dbp;
	DBT k, v;
	int ret;

	if ((ret = db_create(&dbp, env, 0)) != 0)
		return (ret);
	if ((ret = dbp->open(dbp,
	    NULL, file, sub, DB_BTREE, DB_CREATE, 0600)) == 0) {
		memset(&k, 0, sizeof(k));
		memset(&v, 0, sizeof(v));
		k.data = (void *)key;
		k.size = (u_int32_t)strlen(key);
		v.data = "v";
		v.size = 1;
		ret = dbp->put(dbp, NULL, &k, &v, 0);
	}
	(void)dbp->close(dbp, 0);
	return (ret);
}

/* Row count, or -errno if the open fails. */
static int
count(DB_ENV *env, const char *file, const char *sub)
{
	DB *dbp;
	DBC *c;
	DBT k, v;
	int n, ret;

	if ((ret = db_create(&dbp, env, 0)) != 0)
		return (-ret);
	if ((ret = dbp->open(dbp,
	    NULL, file, sub, DB_BTREE, DB_RDONLY, 0)) != 0) {
		(void)dbp->close(dbp, 0);
		return (ret > 0 ? -ret : -1);
	}
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	n = 0;
	if (dbp->cursor(dbp, NULL, &c, 0) == 0) {
		while (c->get(c, &k, &v, DB_NEXT) == 0)
			n++;
		(void)c->close(c);
	}
	(void)dbp->close(dbp, 0);
	return (n);
}

static int
topen(DB_ENV *env, const char *file, const char *sub, DB **dbpp)
{
	DB *dbp;
	int ret;

	*dbpp = NULL;
	if ((ret = db_create(&dbp, env, 0)) != 0)
		return (ret);
	ret = dbp->open(dbp, NULL, file, sub, DB_BTREE,
	    DB_CREATE | DB_TRUNCATE, 0600);
	if (ret != 0)
		(void)dbp->close(dbp, 0);
	else
		*dbpp = dbp;
	return (ret);
}

int
main(int argc, char *argv[])
{
	DB_ENV *env;
	DB *dbp;
	DBT k, v;
	int ret;

	if (argc != 2) {
		fprintf(stderr, "usage: truncate_subdb home\n");
		return (2);
	}
	/* DB_TRUNCATE is refused with locking or transactions: mpool only. */
	if (db_env_create(&env, 0) != 0 ||
	    env->open(env, argv[1], DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT truncate_subdb FAIL env\n");
		return (1);
	}
	env->set_errfile(env, stdout);

	/* a */
	CK(put1(env, "multi.db", "keep", "a") == 0 &&
	    put1(env, "multi.db", "keep", "b") == 0 &&
	    put1(env, "multi.db", "victim", "x") == 0, "a: setup");
	ret = topen(env, "multi.db", "victim", &dbp);
	if (dbp != NULL)
		(void)dbp->close(dbp, 0);
	printf("       truncate-open(victim) = %d (%s)\n",
	    ret, ret ? db_strerror(ret) : "ok");
	CK(ret == EINVAL, "a: DB_TRUNCATE with a subdatabase name is EINVAL");
	CK(count(env, "multi.db", "keep") == 2,
	    "a: the other subdatabase still has its 2 rows");
	CK(count(env, "multi.db", "victim") == 1,
	    "a: the named subdatabase was not touched either");

	/* b */
	CK(put1(env, "one.db", NULL, "a") == 0, "b: setup");
	ret = topen(env, "one.db", NULL, &dbp);
	CK(ret == 0, "b: DB_TRUNCATE with no subdatabase name opens");
	if (dbp != NULL) {
		memset(&k, 0, sizeof(k));
		memset(&v, 0, sizeof(v));
		k.data = "b";
		k.size = 1;
		v.data = "v";
		v.size = 1;
		CK(dbp->put(dbp, NULL, &k, &v, 0) == 0,
		    "b: the truncated database accepts a put");
		(void)dbp->close(dbp, 0);
	}
	CK(count(env, "one.db", NULL) == 1, "b: only the new row remains");

	(void)env->close(env, 0);
	printf("VERDICT truncate_subdb %s\n", nfail ? "FAIL" : "PASS");
	return (nfail != 0);
}

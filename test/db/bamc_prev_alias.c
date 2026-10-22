/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * bamc_prev_alias.c --
 *	Regression test for the strict-aliasing miscompile in the btree
 *	reverse walk.  IS_CUR_DELETED read the cursor index through
 *	DBC_INTERNAL * while __bamc_prev wrote it through BTREE_CURSOR *;
 *	clang -O2 on aarch64 hoisted the read above the write, so DB_PREV
 *	tested the slot it had just left and skipped live records next to
 *	deleted ones.
 *
 *	Load N rows, delete every third through a cursor that stays OPEN (so
 *	the deleted slots stay on the leaf pages), then count the live rows
 *	with fresh cursors walking DB_NEXT and DB_PREV.  Both must equal the
 *	arithmetic truth.  Against the defect, DB_PREV returned 13,321 of
 *	13,333.  Btree, plus the same walk on a Recno database, whose cursor
 *	shares __bamc_prev.  One VERDICT line.
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

#define	N	20000

static int
walk(DB *dbp, u_int32_t how)
{
	DBC *c;
	DBT k, v;
	int n;

	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	if (dbp->cursor(dbp, NULL, &c, 0) != 0)
		return (-1);
	for (n = 0; c->get(c, &k, &v, how) == 0; n++)
		;
	(void)c->close(c);
	return (n);
}

static int
one(DB_ENV *env, DBTYPE type, const char *name)
{
	DB *dbp;
	DBC *c;
	DBT k, v;
	db_recno_t rn;
	char kb[16];
	int del, fwd, i, live, rev;

	if (db_create(&dbp, env, 0) != 0 ||
	    dbp->open(dbp, NULL, name, NULL, type, DB_CREATE, 0600) != 0) {
		printf("  FAIL %s: create/open\n", name);
		return (1);
	}
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	for (i = 0; i < N; i++) {
		(void)snprintf(kb, sizeof(kb), "%08d", i);
		if (type == DB_RECNO) {
			rn = (db_recno_t)i + 1;
			k.data = &rn;
			k.size = sizeof(rn);
		} else {
			k.data = kb;
			k.size = 8;
		}
		v.data = kb;
		v.size = 8;
		if (dbp->put(dbp, NULL, &k, &v, 0) != 0) {
			printf("  FAIL %s: put %d\n", name, i);
			return (1);
		}
	}
	/* Keep c open: its deleted slots must stay on the pages. */
	if (dbp->cursor(dbp, NULL, &c, 0) != 0)
		return (1);
	for (i = 0, del = 0; c->get(c, &k, &v, DB_NEXT) == 0; i++)
		if (i % 3 == 0 && c->del(c, 0) == 0)
			del++;
	live = N - del;
	fwd = walk(dbp, DB_NEXT);
	rev = walk(dbp, DB_PREV);
	(void)c->close(c);
	(void)dbp->close(dbp, 0);
	printf("  %s %s: truth=%d DB_NEXT=%d DB_PREV=%d\n",
	    (fwd == live && rev == live) ? "ok  " : "FAIL", name, live, fwd, rev);
	return (fwd != live || rev != live);
}

int
main(int argc, char *argv[])
{
	DB_ENV *env;
	int nfail;

	if (argc != 2) {
		fprintf(stderr, "usage: bamc_prev_alias home\n");
		return (2);
	}
	if (db_env_create(&env, 0) != 0 || env->open(env, argv[1],
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0) != 0) {
		printf("VERDICT bamc_prev_alias FAIL env\n");
		return (1);
	}
	env->set_errfile(env, stderr);
	nfail = one(env, DB_BTREE, "bt.db") + one(env, DB_RECNO, "rn.db");
	(void)env->close(env, 0);
	printf("VERDICT bamc_prev_alias %s\n", nfail ? "FAIL" : "PASS");
	return (nfail != 0);
}

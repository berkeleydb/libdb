/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * soname_compat -- the cross-level compatibility promise of rfc/0020.
 *
 * Built twice: against the previous compatibility level (OLD) and the current
 * one (NEW), each statically.  Phases, each run as its own process:
 *
 *   old create <home>   OLD build: create an env, a btree, a hash and a queue
 *                       database with known records, commit, leave the env and
 *                       its region files in place (no clean close of the region)
 *   new attach <home>   NEW build: join the OLD region -> must fail with
 *                       DB_VERSION_MISMATCH and read nothing
 *   old close  <home>   OLD build: run recovery and remove the regions
 *   new open   <home>   NEW build: open with DB_RECOVER; every record the OLD
 *                       build committed must be present (databases and logs keep
 *                       their own version numbers, so they carry across)
 *
 * usage: soname_compat <create|attach|close|open> <home>
 * Exit 0 = the phase met its expectation.
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	NREC	500
static const char *files[] = { "b.db", "h.db", "q.db" };
static const DBTYPE types[] = { DB_BTREE, DB_HASH, DB_QUEUE };

static int
open_env(DB_ENV **envp, const char *home, u_int32_t extra)
{
	DB_ENV *env;
	int ret;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	env->set_errfile(env, stderr);
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | extra, 0)) != 0) {
		(void)env->close(env, 0);
		return (ret);
	}
	*envp = env;
	return (0);
}

static int
open_db(DB_ENV *env, DB **dbpp, int i, u_int32_t flags)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, env, 0)) != 0)
		return (ret);
	if (types[i] == DB_QUEUE)
		(void)dbp->set_re_len(dbp, 16);
	if ((ret = dbp->open(dbp, NULL, files[i], NULL, types[i],
	    flags | DB_AUTO_COMMIT, 0644)) != 0) {
		(void)dbp->close(dbp, 0);
		return (ret);
	}
	*dbpp = dbp;
	return (0);
}

static void
mkrec(int i, int n, DBT *k, DBT *d, char *kb, char *vb, db_recno_t *rn)
{
	memset(k, 0, sizeof(*k));
	memset(d, 0, sizeof(*d));
	if (types[i] == DB_QUEUE) {
		*rn = (db_recno_t)n + 1;
		k->data = rn;
		k->size = sizeof(*rn);
	} else {
		k->size = (u_int32_t)snprintf(kb, 32, "key%06d", n);
		k->data = kb;
	}
	(void)snprintf(vb, 17, "val%012d", n);
	d->data = vb;
	d->size = 16;
}

int
main(int argc, char *argv[])
{
	DB_ENV *env;
	DB *dbp;
	DB_TXN *txn;
	DBT k, d;
	db_recno_t rn;
	char kb[32], vb[32];
	const char *phase, *home;
	int a, b, c, i, n, ret, missing;

	if (argc != 3)
		return (2);
	phase = argv[1];
	home = argv[2];
	(void)db_version(&a, &b, &c);
	fprintf(stderr, "soname_compat %s: library level %d.%d.%d\n",
	    phase, a, b, c);

	if (strcmp(phase, "create") == 0) {
		if ((ret = open_env(&env, home, 0)) != 0)
			return (2);
		for (i = 0; i < 3; i++) {
			if ((ret = open_db(env, &dbp, i, DB_CREATE)) != 0)
				return (2);
			if (env->txn_begin(env, NULL, &txn, 0) != 0)
				return (2);
			for (n = 0; n < NREC; n++) {
				mkrec(i, n, &k, &d, kb, vb, &rn);
				if (dbp->put(dbp, txn, &k, &d, 0) != 0)
					return (2);
			}
			if (txn->commit(txn, 0) != 0 || dbp->close(dbp, 0) != 0)
				return (2);
		}
		/* Leave the region in place: exit without env->close. */
		printf("VERDICT soname_compat create PASS %d.%d: 3 dbs x %d "
		    "records committed, region left open\n", a, b, NREC);
		return (0);
	}
	if (strcmp(phase, "attach") == 0) {
		ret = open_env(&env, home, 0);
		if (ret == DB_VERSION_MISMATCH) {
			printf("VERDICT soname_compat attach PASS %d.%d refused "
			    "the other level's region: DB_VERSION_MISMATCH\n",
			    a, b);
			return (0);
		}
		printf("VERDICT soname_compat attach FAIL expected "
		    "DB_VERSION_MISMATCH, got %d (%s)\n", ret, db_strerror(ret));
		if (ret == 0)
			(void)env->close(env, 0);
		return (1);
	}
	if (strcmp(phase, "close") == 0) {
		if ((ret = open_env(&env, home, DB_RECOVER)) != 0)
			return (2);
		if (env->close(env, 0) != 0)
			return (2);
		if (db_env_create(&env, 0) != 0 ||
		    env->remove(env, home, 0) != 0)
			return (2);
		printf("VERDICT soname_compat close PASS recovered and "
		    "removed the regions\n");
		return (0);
	}
	if (strcmp(phase, "open") == 0) {
		if ((ret = open_env(&env, home, DB_RECOVER)) != 0) {
			printf("VERDICT soname_compat open FAIL env open with "
			    "recovery: %s\n", db_strerror(ret));
			return (1);
		}
		missing = 0;
		for (i = 0; i < 3; i++) {
			if ((ret = open_db(env, &dbp, i, 0)) != 0) {
				printf("VERDICT soname_compat open FAIL %s: "
				    "%s\n", files[i], db_strerror(ret));
				return (1);
			}
			for (n = 0; n < NREC; n++) {
				mkrec(i, n, &k, &d, kb, vb, &rn);
				memset(&d, 0, sizeof(d));
				if (dbp->get(dbp, NULL, &k, &d, 0) != 0 ||
				    d.size != 16 ||
				    memcmp(d.data, vb, 16) != 0)
					missing++;
			}
			(void)dbp->close(dbp, 0);
		}
		(void)env->close(env, 0);
		if (missing != 0) {
			printf("VERDICT soname_compat open FAIL %d of %d "
			    "records missing or wrong\n", missing, 3 * NREC);
			return (1);
		}
		printf("VERDICT soname_compat open PASS %d.%d recovered the "
		    "other level's log and read all %d records\n",
		    a, b, 3 * NREC);
		return (0);
	}
	return (2);
}

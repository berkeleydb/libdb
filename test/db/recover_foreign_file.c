/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 *
 * $Id$
 *
 * recover_foreign_file -- regression test for recovery meeting a data file
 * that is referenced in the log but is now a non-Berkeley DB file (empty,
 * truncated, or foreign).  __dbreg_open_file / __dbreg_register_recover
 * (src/dbreg) treated the resulting EINVAL from __fop_read_meta as fatal,
 * failing the whole recovery; the environment-open path turned that into
 * __env_panic, and a process abort in diagnostic builds.  A missing file
 * (ENOENT) was already tolerated.  A foreign file must be tolerated the
 * same way: skip it and recover everything else.
 *
 * The test uses a child that writes an uncommitted record and is SIGKILLed
 * so the next open must run recovery; it then replaces one database file
 * with an empty file and reopens.  Recovery must succeed, a second,
 * untouched database must keep its committed data, and the foreign file
 * must simply be gone/unusable -- not panic the environment.
 */
#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

static int fails;

#define	CHECK(cond, ...)						\
	do {								\
		if (!(cond)) {						\
			printf("  FAIL: ");				\
			printf(__VA_ARGS__);				\
			printf("\n");					\
			fails++;					\
		}							\
	} while (0)

static DB_ENV *
env_open(const char *home, int *rp)
{
	DB_ENV *e;

	if ((*rp = db_env_create(&e, 0)) != 0)
		return (NULL);
	e->set_errpfx(e, "recover_foreign_file");
	if ((*rp = e->open(e, home, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_REGISTER | DB_RECOVER |
	    DB_THREAD, 0644)) != 0) {
		(void)e->close(e, 0);
		return (NULL);
	}
	return (e);
}

static DB *
db_open(DB_ENV *e, const char *file, int *rp)
{
	DB *d;

	if ((*rp = db_create(&d, e, 0)) != 0)
		return (NULL);
	if ((*rp = d->open(d, NULL, file, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
		(void)d->close(d, 0);
		return (NULL);
	}
	return (d);
}

static void
put1(DB *d, DB_TXN *t, const char *key, const char *val, int autocommit)
{
	DBT k, v;

	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	k.data = (void *)key;
	k.size = (u_int32_t)strlen(key);
	v.data = (void *)val;
	v.size = (u_int32_t)strlen(val);
	(void)d->put(d, t, &k, &v, autocommit ? DB_AUTO_COMMIT : 0);
}

static int
get1(DB *d, const char *key, char *out, size_t outsz)
{
	DBT k, v;
	int r;

	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	k.data = (void *)key;
	k.size = (u_int32_t)strlen(key);
	v.data = out;
	v.ulen = (u_int32_t)outsz;
	v.flags = DB_DBT_USERMEM;
	if ((r = d->get(d, NULL, &k, &v, 0)) == 0)
		out[v.size < outsz ? v.size : outsz - 1] = '\0';
	return (r);
}

int
main(int argc, char *argv[])
{
	char home[] = "/tmp/RECOVER_FOREIGN_FILE.XXXXXX";
	char path[256], out[64];
	DB_ENV *e;
	DB *d1, *d2;
	pid_t p;
	int r, st;
	FILE *fp;

	(void)argc;
	(void)argv;

	printf("recover_foreign_file: %s\n", db_version(NULL, NULL, NULL));

	if (mkdtemp(home) == NULL)
		return (printf("mkdtemp\n"), 99);

	/* A: create two databases, commit one record in each. */
	if ((e = env_open(home, &r)) == NULL)
		return (printf("A env open: %s\n", db_strerror(r)), 2);
	if ((d1 = db_open(e, "f.db", &r)) == NULL ||
	    (d2 = db_open(e, "keep.db", &r)) == NULL)
		return (printf("A db open: %s\n", db_strerror(r)), 2);
	put1(d1, NULL, "k", "v", 1);
	put1(d2, NULL, "keep", "survive", 1);

	/* B: a registered peer that writes uncommitted and is SIGKILLed. */
	if ((p = fork()) == 0) {
		int cr;
		DB_ENV *be;
		DB *bd;
		DB_TXN *t;
		be = env_open(home, &cr);
		if (be != NULL && (bd = db_open(be, "f.db", &cr)) != NULL) {
			be->txn_begin(be, NULL, &t, 0);
			put1(bd, t, "dirty", "uncommitted", 0);
		}
		kill(getpid(), SIGKILL);
		_exit(0);
	}
	(void)waitpid(p, &st, 0);

	(void)d1->close(d1, 0);
	(void)d2->close(d2, 0);
	(void)e->close(e, 0);

	/* Replace f.db with an empty (foreign) file. */
	snprintf(path, sizeof(path), "%s/f.db", home);
	(void)unlink(path);
	if ((fp = fopen(path, "w")) != NULL)
		(void)fclose(fp);

	/* Reopen: recovery must cope with the foreign file, not panic. */
	if ((e = env_open(home, &r)) == NULL) {
		CHECK(0, "reopen after foreign f.db: %s", db_strerror(r));
		printf("recover_foreign_file: %d failure%s\n",
		    fails, fails == 1 ? "" : "s");
		return (1);
	}

	/* keep.db must still have its committed record. */
	if ((d2 = db_open(e, "keep.db", &r)) == NULL)
		CHECK(0, "reopen keep.db: %s", db_strerror(r));
	else {
		r = get1(d2, "keep", out, sizeof(out));
		CHECK(r == 0 && strcmp(out, "survive") == 0,
		    "keep.db lost its record: r=%d out=\"%s\"",
		    r, r == 0 ? out : "");
		(void)d2->close(d2, 0);
	}

	/*
	 * f.db is now the empty foreign file.  Recovery left it alone; what
	 * matters is that the environment survived.  (Opening it with
	 * DB_CREATE would simply make a fresh empty database, which is fine
	 * and not part of what we are testing, so we do not open it here.)
	 */

	(void)e->close(e, 0);

	printf("recover_foreign_file: %d failure%s\n",
	    fails, fails == 1 ? "" : "s");
	return (fails == 0 ? 0 : 1);
}

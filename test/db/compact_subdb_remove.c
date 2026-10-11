/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 *
 * $Id$
 *
 * compact_subdb_remove -- regression test for the compaction hang when a
 * sub-database is removed and a sibling's metadata page is then moved into
 * the freed page within the same transaction.  __db_move_metadata requested
 * the handle lock on the moved page under the dbp's own handle locker
 * instead of the transaction locker, self-deadlocking against the WRITE
 * handle lock the transaction already held from the pending remove of the
 * sub-database that previously owned that page.
 *
 * The whole test is wrapped in a SIGALRM: a hang is a failure, so it must
 * never hang the suite.
 */
#include <sys/types.h>

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	NSUB	4		/* s0..s3 */
#define	REMOVED	1		/* s1 is removed */
#define	VALSZ	600		/* big enough to need its own page */
#define	TIMEOUT	60

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

static void
onalarm(int sig)
{
	(void)sig;
	printf("  FAIL: compact_subdb_remove hung (>%d s)\n", TIMEOUT);
	fflush(stdout);
	_exit(2);
}

static int
sub_open(DB_ENV *e, DB_TXN *t, const char *name, DB **dp)
{
	int r;

	if ((r = db_create(dp, e, 0)) != 0)
		return (printf("  FAIL: db_create %s: %s\n",
		    name, db_strerror(r)), r);
	if ((r = (*dp)->open(*dp, t, "f.db", name, DB_BTREE,
	    DB_CREATE | DB_THREAD | DB_MULTIVERSION, 0644)) != 0)
		printf("  FAIL: open %s: %s\n", name, db_strerror(r));
	return (r);
}

static void
verify(DB_ENV *e, int removed)
{
	char kb[32];
	DB *d;
	DBT k, v;
	int i, r;

	for (i = 0; i < NSUB; i++) {
		char name[8];
		snprintf(name, sizeof(name), "s%d", i);
		if (db_create(&d, e, 0) != 0)
			return;
		r = d->open(d, NULL, "f.db", name, DB_BTREE, DB_THREAD, 0644);
		if (removed && i == REMOVED) {
			CHECK(r == ENOENT || r == DB_NOTFOUND,
			    "removed %s reopened (%s)", name,
			    r == 0 ? "exists" : db_strerror(r));
			if (r == 0)
				(void)d->close(d, 0);
			continue;
		}
		if (r != 0) {
			CHECK(0, "reopen %s: %s", name, db_strerror(r));
			continue;
		}
		snprintf(kb, sizeof(kb), "%s-000", name);
		memset(&k, 0, sizeof(k));
		memset(&v, 0, sizeof(v));
		k.data = kb;
		k.size = (u_int32_t)strlen(kb);
		v.flags = DB_DBT_MALLOC;
		r = d->get(d, NULL, &k, &v, 0);
		CHECK(r == 0 && v.size == VALSZ,
		    "read %s back: r=%d size=%u", name, r, v.size);
		if (r == 0)
			free(v.data);
		(void)d->close(d, 0);
	}
}

/*
 * run -- create the four sub-databases (one big record each), close the one
 * to be removed, open a second independent handle on a survivor, then in one
 * transaction optionally remove s1 (in a child) and compact the survivors.
 * with_remove 0 is the control: compaction alone must succeed.
 */
static void
run(const char *home, int with_remove)
{
	static char val[VALSZ];
	char kb[32];
	DB_ENV *e;
	DB_TXN *t, *c, *ot;
	DB *s[NSUB], *other;
	DBT k, v;
	int i, r;

	fails = 0;
	other = NULL;
	if ((r = db_env_create(&e, 0)) != 0) {
		CHECK(0, "db_env_create: %s", db_strerror(r));
		return;
	}
	e->set_errpfx(e, "compact_subdb_remove");
	e->set_flags(e, DB_MULTIVERSION | DB_TXN_NOWAIT, 1);
	(void)e->set_lk_detect(e, DB_LOCK_DEFAULT);
	if ((r = e->open(e, home, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0644)) != 0) {
		CHECK(0, "env open: %s", db_strerror(r));
		(void)e->close(e, 0);
		return;
	}

	/* Populate, committed; keep the handles. */
	if (e->txn_begin(e, NULL, &t, DB_TXN_SNAPSHOT) != 0)
		goto done;
	for (i = 0; i < NSUB; i++) {
		char name[8];
		snprintf(name, sizeof(name), "s%d", i);
		if (sub_open(e, t, name, &s[i]) != 0) {
			(void)t->abort(t);
			goto done;
		}
		snprintf(kb, sizeof(kb), "%s-000", name);
		memset(&k, 0, sizeof(k));
		memset(&v, 0, sizeof(v));
		k.data = kb;
		k.size = (u_int32_t)strlen(kb);
		v.data = val;
		v.size = sizeof(val);
		if ((r = s[i]->put(s[i], t, &k, &v, 0)) != 0) {
			CHECK(0, "put %s: %s", name, db_strerror(r));
			(void)t->abort(t);
			goto done;
		}
	}
	(void)t->commit(t, 0);
	(void)s[REMOVED]->close(s[REMOVED], 0);
	s[REMOVED] = NULL;

	/* A second, independent handle on a survivor (another connection). */
	if (e->txn_begin(e, NULL, &ot, 0) == 0) {
		if (sub_open(e, ot, "s2", &other) != 0)
			other = NULL;
		(void)ot->commit(ot, 0);
	}

	/* Remove s1 (in a child) and compact the survivors, in one txn. */
	if (e->txn_begin(e, NULL, &t, DB_TXN_SNAPSHOT) != 0)
		goto done;
	if (with_remove) {
		if (e->txn_begin(e, t, &c, DB_TXN_SNAPSHOT) != 0) {
			(void)t->abort(t);
			goto done;
		}
		r = e->dbremove(e, c, "f.db", "s1", 0);
		CHECK(r == 0, "dbremove s1: %s", db_strerror(r));
		(void)c->commit(c, 0);
	}
	for (i = 0; i < NSUB; i++) {
		if (i == REMOVED)
			continue;
		r = s[i]->compact(s[i], t, NULL, NULL, NULL,
		    DB_FREE_SPACE, NULL);
		CHECK(r == 0, "compact s%d: %s", i, db_strerror(r));
	}
	r = t->commit(t, 0);
	CHECK(r == 0, "commit: %s", db_strerror(r));

	if (other != NULL)
		(void)other->close(other, 0);
	for (i = 0; i < NSUB; i++)
		if (i != REMOVED && s[i] != NULL)
			(void)s[i]->close(s[i], 0);

	verify(e, with_remove);

done:	(void)e->close(e, 0);
}

int
main(int argc, char *argv[])
{
	char home[] = "/tmp/COMPACT_SUBDB_REMOVE.XXXXXX";
	int total;

	(void)argc;
	(void)argv;

	printf("compact_subdb_remove: %s\n", db_version(NULL, NULL, NULL));

	(void)signal(SIGALRM, onalarm);
	(void)alarm(TIMEOUT);

	total = 0;

	/* Control: compact without a prior remove must succeed. */
	if (mkdtemp(home) == NULL)
		return (printf("mkdtemp: control\n"), 99);
	printf(" control (compact, no remove):\n");
	run(home, 0);
	total += fails;

	/* The defect: remove s1, then compact the siblings, in one txn. */
	strcpy(home, "/tmp/COMPACT_SUBDB_REMOVE.XXXXXX");
	if (mkdtemp(home) == NULL)
		return (printf("mkdtemp: remove\n"), 99);
	printf(" remove then compact:\n");
	run(home, 1);
	total += fails;

	printf("compact_subdb_remove: %d failure%s\n",
	    total, total == 1 ? "" : "s");
	return (total == 0 ? 0 : 1);
}

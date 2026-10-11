/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * Regression test for the leak of a child transaction's remove event when
 * the transaction is aborted.
 *
 * DB_ENV->dbremove inside a child transaction queues a TXN_REMOVE event on
 * the child (__txn_remevent, from __db_inmem_remove for an in-memory
 * database and __fop_ondisk_swap for an on-disk one).  When the child ends,
 * __txn_doevents (src/txn/txn_util.c) hands a committing child's remove
 * event to its parent; on abort it jumps straight to the free code, which
 * still said "a child's event belongs to the parent now" and kept it, so the
 * event -- in no list any more -- leaked with its name and file id.
 *
 * Every allocation and free libdb makes is counted through
 * db_env_set_func_malloc/realloc/free, so the test needs no leak detector:
 * after the environment is closed the number of live blocks must be the
 * same as before the remove.
 *
 * Cases (each in a fresh private environment):
 *   1. in-memory database, dbremove in a child, abort the parent   (leaked)
 *   2. in-memory database, dbremove in a child, abort the child    (leaked)
 *   3. on-disk database,   dbremove in a child, abort the parent   (leaked)
 *   4. controls: no remove at all (calibrates the accounting); dbremove
 *      committed through child and parent; dbremove in a top-level txn
 *      that aborts -- no leak before or after the fix, and a committed
 *      remove really removes.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <db.h>

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

/*
 * Allocation accounting.  Each block carries a header so free() of a block
 * libdb allocated is counted, and foreign pointers (if any) pass through.
 */
#define	MAGIC	0x6c656b72u
struct hdr { unsigned magic; unsigned pad; size_t size; };
static long live;

static void *
t_malloc(size_t n)
{
	struct hdr *h;

	if ((h = malloc(sizeof(*h) + n)) == NULL)
		return (NULL);
	h->magic = MAGIC;
	h->size = n;
	live++;
	return (h + 1);
}

static void
t_free(void *p)
{
	struct hdr *h;

	if (p == NULL)
		return;
	h = (struct hdr *)p - 1;
	if (h->magic != MAGIC) {
		free(p);
		return;
	}
	h->magic = 0;
	live--;
	free(h);
}

static void *
t_realloc(void *p, size_t n)
{
	struct hdr *h, *nh;

	if (p == NULL)
		return (t_malloc(n));
	h = (struct hdr *)p - 1;
	if (h->magic != MAGIC)
		return (realloc(p, n));
	if ((nh = realloc(h, sizeof(*nh) + n)) == NULL)
		return (NULL);
	nh->size = n;
	return (nh + 1);
}

enum end { ABORT_PARENT, ABORT_CHILD, COMMIT_BOTH, TOP_ABORT, NOREMOVE };

/*
 * run --
 *	Create database "sub" (in memory, or file f.db), then remove it inside
 *	a txn family that ends as 'how' says.  Returns the number of blocks
 *	still live after the environment is closed that were allocated after
 *	it was created: 0 unless something leaked.
 */
static long
run(const char *label, int inmem, enum end how, int *existsp)
{
	char home[] = "/tmp/txnremleakXXXXXX";
	DB_ENV *e;
	DB_TXN *top, *child, *t;
	DB *d;
	long before;
	int r;

	if (mkdtemp(home) == NULL)
		return (-1000);
	before = live;		/* everything below is freed by e->close */
	if ((r = db_env_create(&e, 0)) != 0)
		return (-1001);
	e->set_errfile(e, stderr);
	if (inmem) {
		e->log_set_config(e, DB_LOG_IN_MEMORY, 1);
		e->set_lg_bsize(e, 4 * 1024 * 1024);
	}
	if ((r = e->open(e, home, DB_CREATE | DB_PRIVATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN, 0)) != 0) {
		printf("  %s: env open: %s\n", label, db_strerror(r));
		return (-1002);
	}
	e->txn_begin(e, NULL, &top, 0);
	db_create(&d, e, 0);
	r = d->open(d, top, inmem ? NULL : "f.db", inmem ? "sub" : NULL,
	    DB_BTREE, DB_CREATE, 0644);
	d->close(d, 0);
	top->commit(top, 0);
	CHECK(r == 0, "%s: create: %s", label, db_strerror(r));

	e->txn_begin(e, NULL, &top, 0);
	child = NULL;
	if (how != TOP_ABORT && how != NOREMOVE)
		e->txn_begin(e, top, &child, 0);
	t = child != NULL ? child : top;
	r = how == NOREMOVE ? 0 : e->dbremove(e, t, inmem ? NULL : "f.db",
	    inmem ? "sub" : NULL, 0);
	CHECK(r == 0, "%s: dbremove: %s", label, db_strerror(r));
	switch (how) {
	case ABORT_PARENT:
		r = top->abort(top);
		break;
	case ABORT_CHILD:
		if ((r = child->abort(child)) == 0)
			r = top->commit(top, 0);
		break;
	case COMMIT_BOTH:
		if ((r = child->commit(child, 0)) == 0)
			r = top->commit(top, 0);
		break;
	case TOP_ABORT:
	case NOREMOVE:
		r = top->abort(top);
		break;
	}
	CHECK(r == 0, "%s: end txn: %s", label, db_strerror(r));

	/* Does the database still exist? */
	db_create(&d, e, 0);
	e->txn_begin(e, NULL, &top, 0);
	*existsp = d->open(d, top, inmem ? NULL : "f.db",
	    inmem ? "sub" : NULL, DB_BTREE, 0, 0644) == 0;
	d->close(d, 0);
	top->commit(top, 0);
	e->close(e, 0);
	return (live - before);
}

int
main(void)
{
	long d;
	int exists;

	db_env_set_func_malloc(t_malloc);
	db_env_set_func_realloc(t_realloc);
	db_env_set_func_free(t_free);
	printf("txn_remevent_leak: %s\n", db_version(NULL, NULL, NULL));

	/* The accounting itself: no remove, nothing may stay live. */
	d = run("calibrate inmem", 1, NOREMOVE, &exists);
	CHECK(d == 0, "calibration inmem (no remove): %ld blocks live", d);
	d = run("calibrate disk", 0, NOREMOVE, &exists);
	CHECK(d == 0, "calibration disk (no remove): %ld blocks live", d);

	d = run("inmem abort-parent", 1, ABORT_PARENT, &exists);
	CHECK(d == 0, "inmem, dbremove in child, abort parent: %ld blocks "
	    "leaked", d);
	CHECK(exists, "inmem abort-parent: the aborted remove happened");

	d = run("inmem abort-child", 1, ABORT_CHILD, &exists);
	CHECK(d == 0, "inmem, dbremove in child, abort child: %ld blocks "
	    "leaked", d);
	CHECK(exists, "inmem abort-child: the aborted remove happened");

	d = run("disk abort-parent", 0, ABORT_PARENT, &exists);
	CHECK(d == 0, "on-disk, dbremove in child, abort parent: %ld blocks "
	    "leaked", d);
	CHECK(exists, "disk abort-parent: the aborted remove happened");

	/* Controls. */
	d = run("inmem commit", 1, COMMIT_BOTH, &exists);
	CHECK(d == 0, "control inmem commit: %ld blocks leaked", d);
	CHECK(!exists, "control inmem commit: database not removed");
	d = run("disk commit", 0, COMMIT_BOTH, &exists);
	CHECK(d == 0, "control disk commit: %ld blocks leaked", d);
	CHECK(!exists, "control disk commit: database not removed");
	d = run("inmem top-abort", 1, TOP_ABORT, &exists);
	CHECK(d == 0, "control inmem top-level abort: %ld blocks leaked", d);
	CHECK(exists, "control inmem top-abort: the aborted remove happened");

	printf("txn_remevent_leak: %d checks, %d failures\n", checks,
	    failures);
	return (failures != 0);
}

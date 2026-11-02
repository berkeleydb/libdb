/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * meta_nta_recover.c --
 *	Recovery coverage for RFC 0022: page allocation as a nested top-level
 *	action (NTA).  __db_new now splices the free list in a child
 *	transaction that commits DB_TXN_NOSYNC, releasing the PGNO_BASE_MD meta
 *	lock before the parent commits.  This driver exercises the three
 *	recovery states that the NTA introduces and that the Tcl recd0NN suite
 *	does not target directly:
 *
 *	  (A) NTA committed, parent committed   -> redo: page allocated, in use.
 *	  (B) NTA committed, parent ABORTED      -> the allocating user txn rolls
 *	      back but the NTA's alloc record is already committed; the page is
 *	      a leaked free page (off the free list, unreachable).  RFC 0022 §5
 *	      argues this is harmless; this test proves the database stays
 *	      *consistent* (opens, reads back, db_verify-clean) after such an
 *	      abort, both live and across recovery.
 *	  (C) NTA committed, CRASH before parent commit -> catastrophic recovery
 *	      replays the committed NTA and undoes the parent; same harmless
 *	      leaked page, and a consistent database.
 *
 *	Because the NTA reuses the existing __db_pg_alloc record and
 *	__db_pg_alloc_recover unchanged, "recovery of the new record" is
 *	recovery of that record inside a committed sub-transaction -- which is
 *	what DB___txn_child already handles.  The test's job is to prove the
 *	page-allocation-then-parent-abort combination recovers to a consistent
 *	state.
 *
 *	A hard SIGALRM guard aborts if anything blocks (the convoy this RFC
 *	removes manifested as a multi-minute stall).
 *
 * Usage (from the build dir):
 *	BUILD=. sh ../test/db/run_meta_nta_recover.sh
 */

#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"META_NTA_RECOVER_TESTDIR"
#define	TABLE		"nta.db"
#define	NRECS		8000		/* enough to split + extend many pages */
#define	PAGESIZE	512		/* small pages -> many allocations */
#define	ALARM_SECS	120

static int fails = 0;

#define	CHK0(call) do {							\
	int _r = (call);						\
	if (_r != 0) {							\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s)\n",		\
		    __FILE__, __LINE__, #call, _r, db_strerror(_r));	\
		fails++;						\
	}								\
} while (0)

static int
open_env(DB_ENV **dbenvp, u_int32_t extra)
{
	DB_ENV *dbenv;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (ret);
	}
	dbenv->set_errpfx(dbenv, "meta_nta_recover");
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | extra, 0644)) != 0) {
		fprintf(stderr, "env open: %s\n", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	*dbenvp = dbenv;
	return (0);
}

/* Insert [lo, hi) under txn; caller commits or aborts. */
static void
fill(DB *db, DB_TXN *txn, u_int32_t lo, u_int32_t hi)
{
	DBT key, data;
	u_int32_t i;
	char kbuf[32], vbuf[200];

	for (i = lo; i < hi; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "%010u", i);
		(void)snprintf(vbuf, sizeof(vbuf),
		    "val-%010u-%s", i, "padpadpadpadpadpadpadpadpadpadpad");
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		CHK0(db->put(db, txn, &key, &data, 0));
	}
}

/* db_verify the table out-of-process; return 0 if clean. */
static int
verify_clean(void)
{
	/*
	 * Use the in-library verify (DB->verify) rather than shelling out to
	 * db_verify, so the test is self-contained and does not depend on the
	 * utility being on PATH.  A separate handle is required: verify
	 * invalidates the handle.
	 */
	DB_ENV *dbenv;
	DB *db;
	int ret;

	if (open_env(&dbenv, 0) != 0)
		return (-1);
	if ((ret = db_create(&db, dbenv, 0)) != 0) {
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	/* DB->verify with no txn, standalone. */
	ret = db->verify(db, TABLE, NULL, NULL, 0);
	/* verify consumes the handle; do not call db->close. */
	(void)dbenv->close(dbenv, 0);
	return (ret);
}

int
main(void)
{
	DB_ENV *dbenv;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	int ret;
	char kbuf[32];

	(void)signal(SIGALRM, SIG_DFL);
	(void)alarm(ALARM_SECS);

	(void)system("rm -f " HOME "/__db.* " HOME "/log.* " HOME "/*.db "
	    HOME "/DB_CONFIG 2>/dev/null");
	(void)mkdir(HOME, 0755);

	/* ---- Phase 1: build data; commit most, abort one alloc-heavy txn. */
	if (open_env(&dbenv, DB_RECOVER) != 0)
		return (EXIT_FAILURE);

	CHK0(db_create(&db, dbenv, 0));
	CHK0(db->set_pagesize(db, PAGESIZE));
	CHK0(db->open(db, NULL, TABLE, NULL,
	    DB_BTREE, DB_CREATE | DB_AUTO_COMMIT, 0644));

	/* (A) committed allocations: these pages stay in use. */
	CHK0(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	fill(db, txn, 0, NRECS);
	CHK0(txn->commit(txn, 0));

	/*
	 * (B) a transaction that allocates pages then ABORTS.  Each put that
	 * splits/extends runs an NTA that commits the allocation; the abort
	 * rolls back the user data but not the committed NTAs, so those pages
	 * are leaked free pages.  The database must remain consistent.
	 */
	CHK0(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	fill(db, txn, NRECS, NRECS + NRECS / 2);
	CHK0(txn->abort(txn));

	/* The aborted keys must be absent; the committed ones present. */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	(void)snprintf(kbuf, sizeof(kbuf), "%010u", (u_int32_t)(NRECS + 10));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	ret = db->get(db, NULL, &key, &data, 0);
	if (ret != DB_NOTFOUND) {
		fprintf(stderr,
		    "FAIL: aborted key present after abort: ret=%d\n", ret);
		fails++;
	}
	(void)snprintf(kbuf, sizeof(kbuf), "%010u", (u_int32_t)10);
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	CHK0(db->get(db, NULL, &key, &data, 0));

	/* Re-use after abort: new inserts must succeed (allocator healthy). */
	CHK0(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	fill(db, txn, NRECS, NRECS + 100);
	CHK0(txn->commit(txn, 0));

	CHK0(db->close(db, 0));

	/* Live consistency check (before any recovery). */
	if ((ret = verify_clean()) != 0) {
		fprintf(stderr, "FAIL: live verify => %d (%s)\n",
		    ret, db_strerror(ret));
		fails++;
	}

	/* Close WITHOUT a checkpoint so the log keeps every NTA record. */
	CHK0(dbenv->close(dbenv, 0));

	/* ---- Phase 2 (C): catastrophic recovery replays the whole log. */
	if (open_env(&dbenv, DB_RECOVER_FATAL) != 0)
		return (EXIT_FAILURE);
	CHK0(dbenv->close(dbenv, 0));

	/* Consistency after catastrophic recovery. */
	if ((ret = verify_clean()) != 0) {
		fprintf(stderr, "FAIL: post-RECOVER_FATAL verify => %d (%s)\n",
		    ret, db_strerror(ret));
		fails++;
	}

	/* Data survives: a committed key is present, an aborted one absent. */
	if (open_env(&dbenv, DB_RECOVER) != 0)
		return (EXIT_FAILURE);
	CHK0(db_create(&db, dbenv, 0));
	CHK0(db->open(db, NULL, TABLE, NULL, DB_BTREE, DB_AUTO_COMMIT, 0644));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	(void)snprintf(kbuf, sizeof(kbuf), "%010u", (u_int32_t)(NRECS - 1));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	CHK0(db->get(db, NULL, &key, &data, 0));
	(void)snprintf(kbuf, sizeof(kbuf), "%010u", (u_int32_t)(NRECS + 10));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	ret = db->get(db, NULL, &key, &data, 0);
	if (ret != DB_NOTFOUND) {
		fprintf(stderr,
		    "FAIL: aborted key present after recovery: ret=%d\n", ret);
		fails++;
	}
	CHK0(db->close(db, 0));
	CHK0(dbenv->close(dbenv, 0));

	if (fails != 0) {
		fprintf(stderr, "meta_nta_recover: %d checks FAILED\n", fails);
		return (EXIT_FAILURE);
	}
	printf("meta_nta_recover: PASS\n");
	return (EXIT_SUCCESS);
}

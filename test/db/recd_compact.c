/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2024 Oracle and/or its affiliates.  All rights reserved.
 *
 * recd_compact.c --
 *	A standalone driver that exercises the btree-compaction and
 *	page-truncation recovery record handlers in src/db/db_rec.c:
 *	__db_merge_recover, __db_pgno_recover, __db_pg_trunc_recover.  The
 *	Tcl recd0NN tests never run compaction under recovery, so those three
 *	handlers (~330 lines) stay completely cold.
 *
 *	It builds a transactional environment, fills a btree with enough data
 *	to span many pages, deletes a large contiguous range to leave sparse
 *	and empty pages, then runs DB->compact(DB_FREE_SPACE) inside a
 *	transaction.  Compaction logs __db_merge (merge adjacent pages),
 *	__db_pgno (renumber a page's references) and __db_pg_trunc (truncate
 *	trailing free pages back to the OS) records.
 *
 *	It then closes everything and re-opens the environment with
 *	DB_RECOVER_FATAL (catastrophic recovery), which replays the entire log
 *	from the beginning -- forcing the redo (forward-roll) branches of
 *	__db_merge_recover / __db_pgno_recover / __db_pg_trunc_recover to run.
 *	A hard SIGALRM guard aborts if anything blocks.
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

#define	HOME		"RECD_COMPACT_TESTDIR"
#define	TABLE		"compact.db"
#define	NRECS		20000		/* enough to span many pages */
#define	PAGESIZE	512		/* small pages -> many of them */
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
	dbenv->set_errpfx(dbenv, "recd_compact");
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | extra, 0644)) != 0) {
		fprintf(stderr, "env open: %s\n", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	*dbenvp = dbenv;
	return (0);
}

int
main(void)
{
	DB_ENV *dbenv;
	DB *db;
	DB_TXN *txn;
	DB_COMPACT c;
	DBT key, data;
	u_int32_t i;
	int ret;
	char kbuf[32], vbuf[64];

	(void)signal(SIGALRM, SIG_DFL);
	(void)alarm(ALARM_SECS);

	(void)system("rm -f " HOME "/__db.* " HOME "/log.* " HOME "/*.db "
	    HOME "/DB_CONFIG 2>/dev/null");
	(void)mkdir(HOME, 0755);

	/* Phase 1: build data, delete a range, compact -- all logged. */
	if (open_env(&dbenv, DB_RECOVER) != 0)
		return (EXIT_FAILURE);

	CHK0(db_create(&db, dbenv, 0));
	CHK0(db->set_pagesize(db, PAGESIZE));
	CHK0(db->open(db, NULL, TABLE, NULL,
	    DB_BTREE, DB_CREATE | DB_AUTO_COMMIT, 0644));

	/* Insert NRECS records across a batch of committed transactions. */
	for (i = 0; i < NRECS; i++) {
		if (i % 1000 == 0)
			CHK0(dbenv->txn_begin(dbenv, NULL, &txn, 0));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "%010u", i);
		(void)snprintf(vbuf, sizeof(vbuf), "val-%010u-pad", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		CHK0(db->put(db, txn, &key, &data, 0));
		if (i % 1000 == 999 || i == NRECS - 1)
			CHK0(txn->commit(txn, 0));
	}

	/* Delete the middle 60% to leave large runs of empty pages. */
	CHK0(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	for (i = NRECS / 5; i < NRECS - NRECS / 5; i++) {
		memset(&key, 0, sizeof(key));
		(void)snprintf(kbuf, sizeof(kbuf), "%010u", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->del(db, txn, &key, 0);
		if (ret != 0 && ret != DB_NOTFOUND)
			CHK0(ret);
	}
	CHK0(txn->commit(txn, 0));

	/*
	 * Compact with DB_FREE_SPACE -> logs __db_merge / __db_pgno /
	 * __db_pg_trunc records that recovery will replay.
	 */
	memset(&c, 0, sizeof(c));
	CHK0(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	ret = db->compact(db, txn, NULL, NULL, &c, DB_FREE_SPACE, NULL);
	if (ret != 0) {
		fprintf(stderr, "FAIL: compact => %d (%s)\n",
		    ret, db_strerror(ret));
		fails++;
		(void)txn->abort(txn);
	} else
		CHK0(txn->commit(txn, 0));

	CHK0(db->close(db, 0));
	/* Close WITHOUT a final checkpoint so the records stay in the log. */
	CHK0(dbenv->close(dbenv, 0));

	/*
	 * Phase 2: catastrophic recovery replays the whole log from the
	 * start -> forward-roll redo of merge/pgno/pg_trunc handlers.
	 */
	if (open_env(&dbenv, DB_RECOVER_FATAL) != 0)
		return (EXIT_FAILURE);

	/* Verify the database still opens and reads back after recovery. */
	CHK0(db_create(&db, dbenv, 0));
	CHK0(db->open(db, NULL, TABLE, NULL, DB_BTREE, DB_AUTO_COMMIT, 0644));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	(void)snprintf(kbuf, sizeof(kbuf), "%010u", (u_int32_t)0);
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	CHK0(db->get(db, NULL, &key, &data, 0));
	CHK0(db->close(db, 0));

	/* A plain (non-fatal) recovery pass too, for the redo-from-ckpt path. */
	CHK0(dbenv->close(dbenv, 0));
	if (open_env(&dbenv, DB_RECOVER) != 0)
		return (EXIT_FAILURE);
	CHK0(dbenv->close(dbenv, 0));

	if (fails != 0) {
		fprintf(stderr, "recd_compact: %d checks FAILED\n", fails);
		return (EXIT_FAILURE);
	}
	printf("recd_compact: PASS\n");
	return (EXIT_SUCCESS);
}

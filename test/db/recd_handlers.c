/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2024 Oracle and/or its affiliates.  All rights reserved.
 *
 * recd_handlers.c --
 *	A standalone driver that exercises btree/db recovery-record handlers
 *	that the Tcl recd0NN suite never reaches (see the DELIBERATELY EXCLUDED
 *	note in test/coverage/run_coverage.sh: recd003 does NOT unlock the cold
 *	__bam_root / __bam_irep / __bam_rcuradj / __db_ovref handlers -- "they
 *	need scenarios no bounded recd test hits").  This driver produces those
 *	scenarios and then replays the log under recovery:
 *
 *	  __db_ovref_recover  (src/db/db_rec.c) -- DB->truncate() of a btree
 *	    that contains overflow items logs a __db_ovref (-1 refcount) record
 *	    per overflow page (src/db/db_reclaim.c __db_truncate_callback).
 *	    Commit + DB_RECOVER_FATAL replays the REDO branch; a separate run
 *	    aborts the truncate txn to drive the UNDO branch.
 *
 *	  __bam_root_recover  (src/btree/bt_rec.c) -- creating a sub-database
 *	    inside a file logs a __bam_root record that sets the new tree's
 *	    root page on the file meta-data page (src/btree/bt_open.c).  Commit
 *	    + recover replays REDO; abort drives UNDO.
 *
 *	  __bam_irep_recover  (src/btree/bt_rec.c) -- __bam_irep is logged from
 *	    __bam_pupdate -> __bam_pinsert(BPI_REPLACE) -> __bam_irep, and
 *	    BPI_UPDATE / __bam_pupdate is only ever driven by COMPACTION (plain
 *	    delete uses BTD_RELINK, never BTD_UPDATE).  Build a deep multi-level
 *	    btree, delete the front of the keyspace, then DB->compact(): merging
 *	    pages replaces parent separator records.  Commit + recover replays
 *	    REDO.
 *
 *	  __bam_rcuradj_recover  (src/btree/bt_rec.c) -- an rrecno (DB_RENUMBER)
 *	    cursor adjustment logged under a CHILD transaction (CURADJ_LOG
 *	    requires txn->parent != NULL, src/btree/bt_recno.c).  This handler
 *	    only acts on op == DB_TXN_ABORT, so we insert under a nested txn
 *	    with a second cursor positioned to be adjusted, then ABORT the
 *	    child txn -- the abort replays __bam_rcuradj.
 *
 *	NOT reachable here (documented, not covered): __db_cksum_recover
 *	(src/db/db_rec.c).  Its __db_cksum log record is only written when a
 *	checksum error is detected on a page-in with logging enabled
 *	(src/db/db_conv.c:161).  On the subsequent recovery pass, the redo of
 *	the records that built that page re-reads the still-corrupt page and
 *	panics (pgin failed) BEFORE recovery reaches the __db_cksum marker
 *	record -- so the handler never runs.  Tcl recd016 has the same
 *	limitation (it uses the standalone db_recover util, which also does not
 *	execute __db_cksum_recover in-process).  See the driver commit message
 *	for the full repro.
 *
 *	A hard SIGALRM guard aborts if anything blocks.  The home dir is
 *	self-cleaning.
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

#define	HOME		"RECD_HANDLERS_TESTDIR"
#define	PAGESIZE	512		/* small pages -> many, deep trees */
#define	ALARM_SECS	180

static int fails = 0;

#define	CHK(call) do {							\
	int _r = (call);						\
	if (_r != 0) {							\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s)\n",		\
		    __FILE__, __LINE__, #call, _r, db_strerror(_r));	\
		fails++;						\
	}								\
} while (0)

/* Like CHK but tolerate an expected non-zero (e.g. DB_NOTFOUND). */
#define	CHK_OK(call, ok) do {						\
	int _r = (call);						\
	if (_r != 0 && _r != (ok)) {					\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s)\n",		\
		    __FILE__, __LINE__, #call, _r, db_strerror(_r));	\
		fails++;						\
	}								\
} while (0)

static void
clean_home(void)
{
	(void)system("rm -f " HOME "/__db.* " HOME "/log.* " HOME "/*.db "
	    HOME "/DB_CONFIG 2>/dev/null");
	(void)mkdir(HOME, 0755);
}

static int
open_env(DB_ENV **dbenvp, u_int32_t extra)
{
	DB_ENV *dbenv;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (ret);
	}
	dbenv->set_errpfx(dbenv, "recd_handlers");
	/* Big lock table: deep-tree deletes + compaction hold many locks. */
	(void)dbenv->set_lk_max_locks(dbenv, 200000);
	(void)dbenv->set_lk_max_objects(dbenv, 200000);
	(void)dbenv->set_lk_max_lockers(dbenv, 200000);
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | extra, 0644)) != 0) {
		fprintf(stderr, "env open: %s\n", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	*dbenvp = dbenv;
	return (0);
}

/*
 * recover --
 *	Re-open the environment under DB_RECOVER (normal) or DB_RECOVER_FATAL
 *	(catastrophic, replays the whole log from the start), then close.  This
 *	is the replay pass that drives the *_recover handlers.
 */
static void
recover(u_int32_t mode)
{
	DB_ENV *dbenv;

	if (open_env(&dbenv, mode) != 0) {
		fails++;
		return;
	}
	CHK(dbenv->close(dbenv, 0));
}

/*
 * scenario_ovref --
 *	__db_ovref_recover: put a big (overflow) data item, then DB->truncate()
 *	the db under a txn.  Truncate logs one __db_ovref(-1) record per
 *	overflow page.  Run once committing the truncate (REDO on replay) and
 *	once aborting it (UNDO on replay + at abort time).
 */
static void
scenario_ovref(int do_abort)
{
	DB_ENV *dbenv;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	u_int32_t count, i;
	char kbuf[32];
	char *big;
	int ret;

	if (open_env(&dbenv, DB_RECOVER) != 0) {
		fails++;
		return;
	}

	/* A data item several pages long forces overflow pages. */
	big = malloc(8192);
	if (big == NULL) {
		fprintf(stderr, "FAIL: malloc\n");
		fails++;
		(void)dbenv->close(dbenv, 0);
		return;
	}
	memset(big, 'x', 8192);

	CHK(db_create(&db, dbenv, 0));
	CHK(db->set_pagesize(db, PAGESIZE));
	CHK(db->open(db, NULL, "ovref.db", NULL,
	    DB_BTREE, DB_CREATE | DB_AUTO_COMMIT, 0644));

	/* A handful of big items -> several overflow chains. */
	for (i = 0; i < 8; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "big%03u", i);
		key.data = kbuf;
		key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = big;
		data.size = 8192;
		CHK(db->put(db, NULL, &key, &data, 0));
	}
	free(big);

	/*
	 * DB->truncate must run under the txn we control so we can abort it.
	 * truncate needs the db handle with no other refs; close+reopen with
	 * the txn is the simplest.  Re-open the db inside the txn.
	 */
	CHK(db->close(db, 0));

	CHK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK(db_create(&db, dbenv, 0));
	CHK(db->open(db, txn, "ovref.db", NULL, DB_BTREE, 0, 0644));
	count = 0;
	ret = db->truncate(db, txn, &count, 0);
	if (ret != 0) {
		fprintf(stderr, "FAIL: truncate => %d (%s)\n",
		    ret, db_strerror(ret));
		fails++;
		(void)txn->abort(txn);
		(void)db->close(db, 0);
		(void)dbenv->close(dbenv, 0);
		return;
	}
	CHK(db->close(db, 0));

	if (do_abort)
		CHK(txn->abort(txn));		/* UNDO the ovref at abort */
	else
		CHK(txn->commit(txn, 0));	/* leave REDO records */

	/* Close without a final checkpoint so the records stay in the log. */
	CHK(dbenv->close(dbenv, 0));

	/* Replay: fatal recovery rolls the whole log forward (REDO). */
	recover(DB_RECOVER_FATAL);
	recover(DB_RECOVER);
}

/*
 * scenario_root --
 *	__bam_root_recover: creating a sub-database inside a file logs a
 *	__bam_root record (sets the subdb tree root on the file meta page).
 *	Do the create under a txn; commit -> REDO on replay, abort -> UNDO.
 */
static void
scenario_root(int do_abort)
{
	DB_ENV *dbenv;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	int ret;

	if (open_env(&dbenv, DB_RECOVER) != 0) {
		fails++;
		return;
	}

	CHK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK(db_create(&db, dbenv, 0));
	CHK(db->set_pagesize(db, PAGESIZE));
	/* subdb "sub1" inside file root.db -> __bam_root_log on meta page. */
	ret = db->open(db, txn, "root.db", "sub1",
	    DB_BTREE, DB_CREATE, 0644);
	if (ret != 0) {
		fprintf(stderr, "FAIL: subdb open => %d (%s)\n",
		    ret, db_strerror(ret));
		fails++;
		(void)txn->abort(txn);
		(void)dbenv->close(dbenv, 0);
		return;
	}
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "k"; key.size = 2;
	data.data = "v"; data.size = 2;
	CHK(db->put(db, txn, &key, &data, 0));
	CHK(db->close(db, 0));

	if (do_abort)
		CHK(txn->abort(txn));
	else
		CHK(txn->commit(txn, 0));

	CHK(dbenv->close(dbenv, 0));

	recover(DB_RECOVER_FATAL);
	recover(DB_RECOVER);
}

/*
 * scenario_irep --
 *	__bam_irep_recover: __bam_irep is logged from __bam_pupdate ->
 *	__bam_pinsert(BPI_REPLACE) -> __bam_irep on a BTREE internal page.
 *	BPI_UPDATE / __bam_pupdate is only ever driven by COMPACTION (plain
 *	delete uses BTD_RELINK, never BTD_UPDATE -- see bt_delete.c:284 and
 *	bt_compact.c:1504/1849).  So: build a deep multi-level btree, delete
 *	the FRONT of the keyspace (so the smallest surviving key on the
 *	leftmost pages moves up), then DB->compact() -- compaction merges
 *	pages and __bam_pupdate replaces the parent separator record.  Commit
 *	+ fatal recover replays the __bam_irep REDO branch.
 */
static void
scenario_irep(void)
{
	DB_ENV *dbenv;
	DB *db;
	DB_TXN *txn;
	DB_COMPACT c;
	DBT key, data;
	u_int32_t i;
	const u_int32_t nrecs = 60000;	/* deep, multi-level tree */
	char kbuf[32], vbuf[48];
	int ret;

	if (open_env(&dbenv, DB_RECOVER) != 0) {
		fails++;
		return;
	}

	CHK(db_create(&db, dbenv, 0));
	CHK(db->set_pagesize(db, PAGESIZE));
	CHK(db->open(db, NULL, "irep.db", NULL,
	    DB_BTREE, DB_CREATE | DB_AUTO_COMMIT, 0644));

	for (i = 0; i < nrecs; i++) {
		if (i % 2000 == 0)
			CHK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "%010u", i);
		(void)snprintf(vbuf, sizeof(vbuf), "v-%010u-pad", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		CHK(db->put(db, txn, &key, &data, 0));
		if (i % 2000 == 1999 || i == nrecs - 1)
			CHK(txn->commit(txn, 0));
	}

	/*
	 * Delete a sparse pattern across the front 3/4 of the keyspace so
	 * many leftmost pages end up mostly empty (compaction candidates)
	 * while the smallest surviving key on each shifts.  Keep every 7th
	 * record so pages stay partially populated and compaction merges.
	 * Batch into committed sub-txns so we don't hold every lock at once.
	 */
	txn = NULL;
	for (i = 0; i < (nrecs * 3) / 4; i++) {
		if (txn == NULL)
			CHK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
		if (i % 7 != 0) {
			memset(&key, 0, sizeof(key));
			(void)snprintf(kbuf, sizeof(kbuf), "%010u", i);
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
			ret = db->del(db, txn, &key, 0);
			if (ret != 0 && ret != DB_NOTFOUND)
				CHK(ret);
		}
		if (i % 1000 == 999) {
			CHK(txn->commit(txn, 0));
			txn = NULL;
		}
	}
	if (txn != NULL)
		CHK(txn->commit(txn, 0));

	/*
	 * Compact -> merges pages, __bam_pupdate replaces separators.  Pass
	 * NULL txn: compact runs its own bounded internal sub-transactions
	 * (still logged) instead of holding all locks in one big txn.
	 */
	memset(&c, 0, sizeof(c));
	ret = db->compact(db, NULL, NULL, NULL, &c, DB_FREE_SPACE, NULL);
	if (ret != 0) {
		fprintf(stderr, "FAIL: compact => %d (%s)\n",
		    ret, db_strerror(ret));
		fails++;
	}

	CHK(db->close(db, 0));
	CHK(dbenv->close(dbenv, 0));

	recover(DB_RECOVER_FATAL);
	recover(DB_RECOVER);
}

/*
 * scenario_rcuradj --
 *	__bam_rcuradj_recover: rrecno cursor adjustment logged under a nested
 *	(child) transaction, then abort the child txn.  The handler acts only
 *	on op == DB_TXN_ABORT, so the abort's undo pass replays __bam_rcuradj.
 *
 *	CURADJ_LOG requires txn->parent != NULL and the adjustment must move a
 *	*different* cursor (__ram_ca must find nc > 0), so we keep a second
 *	cursor positioned on the record we insert before.
 */
static void
scenario_rcuradj(void)
{
	DB_ENV *dbenv;
	DB *db;
	DB_TXN *ptxn, *ctxn;
	DBC *c1, *c2;
	DBT key, data;
	db_recno_t recno;
	u_int32_t i;
	char vbuf[32];
	int ret;

	if (open_env(&dbenv, DB_RECOVER) != 0) {
		fails++;
		return;
	}

	CHK(db_create(&db, dbenv, 0));
	CHK(db->set_pagesize(db, PAGESIZE));
	CHK(db->set_flags(db, DB_RENUMBER));	/* rrecno: renumbering recno */
	CHK(db->open(db, NULL, "rcuradj.db", NULL,
	    DB_RECNO, DB_CREATE | DB_AUTO_COMMIT, 0644));

	/* Seed a few records (auto-commit); DB_APPEND assigns recnos 1..10. */
	for (i = 1; i <= 10; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		recno = 0;
		key.data = &recno; key.size = sizeof(recno); key.ulen = sizeof(recno);
		key.flags = DB_DBT_USERMEM;
		(void)snprintf(vbuf, sizeof(vbuf), "rec-%u", i);
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		CHK(db->put(db, NULL, &key, &data, DB_APPEND));
	}

	/* Parent txn holds cursors; child txn does the logged adjust. */
	CHK(dbenv->txn_begin(dbenv, NULL, &ptxn, 0));

	/* c2 stays parked on record 5 so the insert-before must adjust it. */
	CHK(db->cursor(db, ptxn, &c2, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	recno = 5;
	key.data = &recno; key.size = sizeof(recno);
	CHK(c2->get(c2, &key, &data, DB_SET));

	/* Child (nested) txn: CURADJ_LOG needs txn->parent != NULL. */
	CHK(dbenv->txn_begin(dbenv, ptxn, &ctxn, 0));
	CHK(db->cursor(db, ctxn, &c1, 0));

	/* Position c1 on record 5, insert a new record BEFORE it. */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	recno = 5;
	key.data = &recno; key.size = sizeof(recno);
	CHK(c1->get(c1, &key, &data, DB_SET));

	memset(&data, 0, sizeof(data));
	data.data = "inserted"; data.size = 9;
	/* DB_BEFORE on rrecno renumbers -> logs __bam_rcuradj (nc>0 via c2). */
	CHK(c1->put(c1, NULL, &data, DB_BEFORE));

	CHK(c1->close(c1));
	/* Abort the child txn -> replays __bam_rcuradj with op==DB_TXN_ABORT. */
	CHK(ctxn->abort(ctxn));

	CHK(c2->close(c2));
	CHK(ptxn->commit(ptxn, 0));

	CHK(db->close(db, 0));

	/* Verify the DB is still consistent: 10 records, none renumbered. */
	CHK(db_create(&db, dbenv, 0));
	CHK(db->open(db, NULL, "rcuradj.db", NULL, DB_RECNO, 0, 0644));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	recno = 5;
	key.data = &recno; key.size = sizeof(recno);
	ret = db->get(db, NULL, &key, &data, 0);
	if (ret != 0)
		CHK(ret);
	CHK(db->close(db, 0));

	CHK(dbenv->close(dbenv, 0));

	recover(DB_RECOVER);
	recover(DB_RECOVER_FATAL);
}

int
main(void)
{
	(void)signal(SIGALRM, SIG_DFL);
	(void)alarm(ALARM_SECS);

	clean_home();

	printf("== ovref (truncate overflow) -- REDO ==\n");
	scenario_ovref(0);
	clean_home();
	printf("== ovref (truncate overflow) -- UNDO (abort) ==\n");
	scenario_ovref(1);
	clean_home();

	printf("== root (subdb create) -- REDO ==\n");
	scenario_root(0);
	clean_home();
	printf("== root (subdb create) -- UNDO (abort) ==\n");
	scenario_root(1);
	clean_home();

	printf("== irep (reverse-split internal replace) -- REDO ==\n");
	scenario_irep();
	clean_home();

	printf("== rcuradj (rrecno cursor adjust) -- UNDO (child abort) ==\n");
	scenario_rcuradj();
	clean_home();

	if (fails != 0) {
		fprintf(stderr, "recd_handlers: %d checks FAILED\n", fails);
		return (EXIT_FAILURE);
	}
	printf("recd_handlers: PASS\n");
	return (EXIT_SUCCESS);
}

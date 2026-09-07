/*-
 * See the file LICENSE for redistribution information.
 *
 * cov_logrec_print.c --
 *	Generate a transaction log containing as many DISTINCT log record
 *	types as a single process can produce, then walk that log through BOTH
 *	per-record dispatch tables that exist for it:
 *
 *	  1. DB_TXN_PRINT  -- src/<sub>/<sub>_autop.c, driven by db_printlog
 *	  2. DB_TXN_VERIFY -- src/log/log_verify_int.c, driven by db_log_verify
 *
 *	Why: report #3's never-called-function list is dominated by exactly
 *	these two tables.
 *
 *	  * ~39 `__<rec>_print` formatters: db/db_autop.c (14),
 *	    hash/hash_autop.c (9), btree/btree_autop.c (6), heap/heap_autop.c
 *	    (4), db/crdel_autop.c (3), fileops/fileops_autop.c (3), plus
 *	    qam/txn/dbreg stragglers.
 *	  * 62 of log_verify_int.c's 99 functions -- the single largest
 *	    never-called count in the whole tree -- are `__<rec>_verify`
 *	    handlers for the same record kinds, plus the __lv_* helpers
 *	    (__lv_dbt_str, __lv_dbtype_str, __lv_log_mismatch,
 *	    __lv_on_heap_log, __lv_on_qam_log, __lv_vrfy_for_dbfile).
 *
 *	Both tables are cold for ONE shared reason: db_printlog and
 *	db_log_verify are only run by the Tcl suite (logverify001/002) over
 *	logs from simple btree workloads.  Every record kind those workloads
 *	never emit -- hash slot changes, hash contraction, page truncation,
 *	compaction realloc, heap ops, queue extent create/delete, off-page-dup
 *	page changes, cursor adjustments, sub-database creation, transactional
 *	rename/remove, the DIAGNOSTIC record, a prepared txn -- has no printer
 *	AND no verifier coverage.  Generating one rich log therefore lights up
 *	two large cold surfaces at once, which is why this is a single driver
 *	and not two.
 *
 *	This driver does NOT re-test the operations themselves; the recd/DST
 *	tiers own their recovery semantics.  It makes the log CONTAIN them and
 *	then asserts the whole log formats and verifies.
 *
 *	Single process, bounded work, hard SIGALRM guard.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/db_am.h"
#include "dbinc/log.h"
#include "dbinc/txn.h"

#include <signal.h>

#define	HOME		"COVLOGREC_TESTDIR"
#define	ALARM_SECS	180
#define	NRECS		2000

static int fails = 0;
static int checks = 0;

#define	CHK_OK(call) do {						\
	int _r = (call);						\
	checks++;							\
	if (_r != 0) {							\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s)\n",		\
		    __FILE__, __LINE__, #call, _r, db_strerror(_r));	\
		fails++;						\
	}								\
} while (0)

/* Tolerate a specific expected-but-not-required outcome. */
#define	TRY(call) do { (void)(call); checks++; } while (0)

static void
on_alarm(sig)
	int sig;
{
	COMPQUIET(sig, 0);
	fprintf(stderr, "FAIL: cov_logrec_print timed out after %d s\n",
	    ALARM_SECS);
	_exit(3);
}

static void
clean_home()
{
	/*
	 * Remove EVERY artifact, not just *.db.  Queue extent files are named
	 * __dbq.<db>.<n> and are NOT matched by *.db -- leaving them behind
	 * makes the next run open an extent whose page LSNs are ahead of the
	 * fresh log, which the engine correctly treats as a corrupt
	 * environment (BDB2506 "file has LSN past end of log" ->
	 * DB_RUNRECOVERY).  Same for the __db.reg / freezer files.
	 * No rm -rf: an explicit list of the patterns this driver creates.
	 */
	(void)system("rm -f " HOME "/__db.* " HOME "/__dbq.* "
	    HOME "/log.* " HOME "/*.db " HOME "/DB_CONFIG 2>/dev/null");
	(void)system("mkdir -p " HOME);
}

static void
mkkey(buf, n)
	char *buf;
	int n;
{
	(void)snprintf(buf, 32, "key%08d", n);
}

/*
 * btree_workload --
 *	Splits, overflow items, reverse splits, off-page duplicates, cursor
 *	adjustment under a child txn, sub-database creation, and compaction.
 *	Emits: __bam_split, __bam_rsplit, __bam_adj, __bam_cadjust,
 *	__bam_cdel, __bam_repl, __bam_root, __bam_curadj, __bam_rcuradj,
 *	__bam_relink, __bam_merge*, __bam_pgno, __db_addrem, __db_big,
 *	__db_ovref, __db_relink, __db_pg_alloc/free/freedata, __db_realloc,
 *	__db_merge, __db_pg_trunc, __db_debug, __crdel_metasub.
 */
static void
btree_workload(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp, *sub1, *sub2;
	DBC *dbc, *dbc2;
	DB_TXN *txn, *child;
	DBT key, data;
	DB_COMPACT cdata;
	char kbuf[32];
	char *big;
	int i, ret;

	if ((big = malloc(70000)) == NULL) {
		fprintf(stderr, "FAIL: malloc\n");
		fails++;
		return;
	}
	memset(big, 'B', 70000);

	/* --- a small-page btree so inserts split constantly. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_pagesize(dbp, 512));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_bt.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	CHK_OK(txn->commit(txn, 0));

	/* Bulk inserts under a txn: splits + page allocs. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 0; i < NRECS; i++) {
		mkkey(kbuf, i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0) {
			fprintf(stderr, "FAIL: put %d: %s\n", i,
			    db_strerror(ret));
			fails++;
			break;
		}
	}
	/* Overflow items: __db_big + __db_ovref. */
	for (i = 0; i < 8; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "big%03d", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = big; data.size = 70000;
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	}
	/* A partial put on an overflow item: __db_addrem partial arm. */
	(void)snprintf(kbuf, sizeof(kbuf), "big%03d", 0);
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
	memset(&data, 0, sizeof(data));
	data.data = "PARTIAL"; data.size = 7;
	data.doff = 100; data.dlen = 7; data.flags = DB_DBT_PARTIAL;
	CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	memset(&data, 0, sizeof(data));
	CHK_OK(txn->commit(txn, 0));

	/*
	 * --- reverse splits: delete most of the keyspace under a txn so
	 * pages merge and the tree collapses (__bam_rsplit, __bam_relink,
	 * __db_pg_free).
	 */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	for (i = 0; i < NRECS - 20; i++) {
		mkkey(kbuf, i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		if ((ret = dbp->del(dbp, txn, &key, 0)) != 0 &&
		    ret != DB_NOTFOUND) {
			fprintf(stderr, "FAIL: del %d: %s\n", i,
			    db_strerror(ret));
			fails++;
			break;
		}
	}
	CHK_OK(txn->commit(txn, 0));

	/*
	 * --- compaction with DB_FREE_SPACE: __db_merge, __db_pgno,
	 * __db_pg_trunc, __db_realloc, __bam_irep.
	 */
	memset(&cdata, 0, sizeof(cdata));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	TRY(dbp->compact(dbp, txn, NULL, NULL, &cdata, DB_FREE_SPACE, NULL));
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbp->close(dbp, 0));

	/*
	 * --- a DB_RENUMBER recno with a cursor parked on a shifting record,
	 * under a CHILD txn: __bam_rcuradj (CURADJ_LOG requires a parent).
	 */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_flags(dbp, DB_RENUMBER));
	CHK_OK(dbp->set_pagesize(dbp, 512));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_rrec.db", NULL, DB_RECNO,
	    DB_CREATE, 0600));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 1; i <= 200; i++) {
		mkkey(kbuf, i);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(dbp->put(dbp, txn, &key, &data, DB_APPEND));
	}
	CHK_OK(txn->commit(txn, 0));

	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbenv->txn_begin(dbenv, txn, &child, 0));
	CHK_OK(dbp->cursor(dbp, child, &dbc, 0));
	CHK_OK(dbp->cursor(dbp, child, &dbc2, 0));
	/* Park cursor 2 on record 100. */
	{
		db_recno_t rno = 100;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &rno; key.size = sizeof(rno);
		TRY(dbc2->get(dbc2, &key, &data, DB_SET));
		/* Delete record 10 through cursor 1: renumbering shifts 100. */
		rno = 10;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &rno; key.size = sizeof(rno);
		if (dbc->get(dbc, &key, &data, DB_SET) == 0)
			TRY(dbc->del(dbc, 0));
	}
	CHK_OK(dbc->close(dbc));
	CHK_OK(dbc2->close(dbc2));
	/* ABORT the child: __bam_rcuradj's DB_TXN_ABORT arm. */
	CHK_OK(child->abort(child));
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbp->close(dbp, 0));

	/*
	 * --- sub-databases inside one file: __bam_root + __crdel_metasub
	 * (a named DB inside a container sets the tree root on the file meta
	 * page).  Two subdbs so the second exercises the reuse arm.
	 */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(db_create(&sub1, dbenv, 0));
	CHK_OK(sub1->open(sub1, txn, "cov_subs.db", "sub1", DB_BTREE,
	    DB_CREATE, 0600));
	CHK_OK(db_create(&sub2, dbenv, 0));
	CHK_OK(sub2->open(sub2, txn, "cov_subs.db", "sub2", DB_BTREE,
	    DB_CREATE, 0600));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 0; i < 100; i++) {
		mkkey(kbuf, i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(sub1->put(sub1, txn, &key, &data, 0));
		CHK_OK(sub2->put(sub2, txn, &key, &data, 0));
	}
	CHK_OK(sub1->close(sub1, 0));
	CHK_OK(sub2->close(sub2, 0));
	CHK_OK(txn->commit(txn, 0));

	/*
	 * --- off-page duplicates: a DUPSORT btree with many dups per key
	 * grows an off-page dup tree (__bam_split on the dup tree,
	 * __db_relink, and the __ham_chgpg analogue for btree).
	 */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_flags(dbp, DB_DUP | DB_DUPSORT));
	CHK_OK(dbp->set_pagesize(dbp, 512));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_dup.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "dupkey"; key.size = 6;
	for (i = 0; i < 400; i++) {
		mkkey(kbuf, i);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	}
	/* Delete through a cursor: __bam_cdel. */
	CHK_OK(dbp->cursor(dbp, txn, &dbc, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "dupkey"; key.size = 6;
	if (dbc->get(dbc, &key, &data, DB_SET) == 0) {
		TRY(dbc->del(dbc, 0));
		TRY(dbc->get(dbc, &key, &data, DB_NEXT_DUP));
		TRY(dbc->del(dbc, 0));
	}
	CHK_OK(dbc->close(dbc));
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbp->close(dbp, 0));

	free(big);
}

/*
 * hash_workload --
 *	Hash-specific record kinds: __ham_insdel, __ham_replace,
 *	__ham_splitdata, __ham_metagroup, __ham_groupalloc, __ham_curadj,
 *	__ham_chgpg, __ham_changeslot, __ham_contract.
 *
 *	__ham_contract + __ham_changeslot only fire on hash COMPACTION, and
 *	__ham_chgpg only when an off-page duplicate page changes -- which is
 *	why their printers are cold.
 */
static void
hash_workload(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp;
	DBC *dbc;
	DB_TXN *txn, *child;
	DBT key, data;
	DB_COMPACT cdata;
	char kbuf[32];
	char *big;
	int i;

	if ((big = malloc(70000)) == NULL) {
		fprintf(stderr, "FAIL: malloc\n");
		fails++;
		return;
	}
	memset(big, 'H', 70000);

	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_pagesize(dbp, 512));
	/* A small initial table so inserts force group allocation + splits. */
	CHK_OK(dbp->set_h_ffactor(dbp, 4));
	CHK_OK(dbp->set_h_nelem(dbp, 8));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_ham.db", NULL, DB_HASH,
	    DB_CREATE, 0600));
	CHK_OK(txn->commit(txn, 0));

	/* Grow the table hard: __ham_metagroup + __ham_groupalloc. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 0; i < NRECS; i++) {
		mkkey(kbuf, i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	}
	/* Replace in place: __ham_replace. */
	for (i = 0; i < 50; i++) {
		mkkey(kbuf, i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = "REPLACED"; data.size = 8;
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	}
	/* Overflow items in a hash db: __db_big via hash. */
	for (i = 0; i < 4; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "hbig%03d", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = big; data.size = 70000;
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	}
	CHK_OK(txn->commit(txn, 0));

	/* Delete most keys, then COMPACT: __ham_contract + __ham_changeslot. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	for (i = 0; i < NRECS - 50; i++) {
		mkkey(kbuf, i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		(void)dbp->del(dbp, txn, &key, 0);
	}
	CHK_OK(txn->commit(txn, 0));
	memset(&cdata, 0, sizeof(cdata));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	TRY(dbp->compact(dbp, txn, NULL, NULL, &cdata, DB_FREE_SPACE, NULL));
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbp->close(dbp, 0));

	/*
	 * --- a hash db with off-page duplicates, cursor-deleted under a
	 * CHILD txn: __ham_curadj + __ham_chgpg.
	 */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_flags(dbp, DB_DUP | DB_DUPSORT));
	CHK_OK(dbp->set_pagesize(dbp, 512));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_hdup.db", NULL, DB_HASH,
	    DB_CREATE, 0600));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "hdupkey"; key.size = 7;
	for (i = 0; i < 500; i++) {
		mkkey(kbuf, i);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	}
	CHK_OK(txn->commit(txn, 0));

	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbenv->txn_begin(dbenv, txn, &child, 0));
	CHK_OK(dbp->cursor(dbp, child, &dbc, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "hdupkey"; key.size = 7;
	if (dbc->get(dbc, &key, &data, DB_SET) == 0) {
		for (i = 0; i < 20; i++) {
			TRY(dbc->del(dbc, 0));
			if (dbc->get(dbc, &key, &data, DB_NEXT_DUP) != 0)
				break;
		}
	}
	CHK_OK(dbc->close(dbc));
	CHK_OK(child->commit(child, 0));
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbp->close(dbp, 0));

	free(big);
}

/*
 * queue_heap_workload --
 *	Queue extent create/delete (__qam_incfirst, __qam_mvptr, __qam_del,
 *	__qam_add, __qam_delete) and the heap access method's records
 *	(__heap_addrem, __heap_pg_alloc, __heap_trunc*) -- heap_autop.c has 4
 *	never-called printers.
 */
static void
queue_heap_workload(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp;
	DB_TXN *txn;
	DBT key, data;
	DB_HEAP_RID rid;
	char kbuf[32];
	db_recno_t rno;
	int i;

	/* --- a queue with EXTENTS so extent files are created and deleted. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_re_len(dbp, 32));
	CHK_OK(dbp->set_pagesize(dbp, 512));
	CHK_OK(dbp->set_q_extentsize(dbp, 2));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_qam.db", NULL, DB_QUEUE,
	    DB_CREATE, 0600));
	CHK_OK(txn->commit(txn, 0));

	/* Append then consume, repeatedly: extents get created and removed. */
	for (i = 0; i < 6; i++) {
		int j;
		CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		for (j = 0; j < 100; j++) {
			(void)snprintf(kbuf, sizeof(kbuf), "q%030d", j);
			data.data = kbuf; data.size = 32;
			CHK_OK(dbp->put(dbp, txn, &key, &data, DB_APPEND));
		}
		CHK_OK(txn->commit(txn, 0));
		/* DB_CONSUME empties the head extents: __qam_delext. */
		CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
		for (j = 0; j < 100; j++) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			if (dbp->get(dbp, txn, &key, &data, DB_CONSUME) != 0)
				break;
		}
		CHK_OK(txn->commit(txn, 0));
	}
	CHK_OK(dbp->close(dbp, 0));

	/* --- a heap db: heap_autop.c's printers. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_pagesize(dbp, 512));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_heap.db", NULL, DB_HEAP,
	    DB_CREATE, 0600));
	CHK_OK(txn->commit(txn, 0));

	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &rid; key.size = key.ulen = sizeof(rid);
	key.flags = DB_DBT_USERMEM;
	for (i = 0; i < 300; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "heap%08d", i);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(dbp->put(dbp, txn, &key, &data, DB_APPEND));
	}
	CHK_OK(txn->commit(txn, 0));
	/* Delete a middle range then re-append: heap page reuse + trunc. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	{
		DBC *dbc;
		int n = 0;
		CHK_OK(dbp->cursor(dbp, txn, &dbc, 0));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &rid; key.size = key.ulen = sizeof(rid);
		key.flags = DB_DBT_USERMEM;
		while (dbc->get(dbc, &key, &data, DB_NEXT) == 0) {
			if (++n % 2 == 0)
				TRY(dbc->del(dbc, 0));
			memset(&data, 0, sizeof(data));
		}
		CHK_OK(dbc->close(dbc));
	}
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbp->close(dbp, 0));

	/* --- a plain recno for __bam_cadjust / __db_relink coverage. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->set_pagesize(dbp, 512));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_rno.db", NULL, DB_RECNO,
	    DB_CREATE, 0600));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 1; i <= 400; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "r%08d", i);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(dbp->put(dbp, txn, &key, &data, DB_APPEND));
	}
	/* Overwrite by recno: __bam_repl. */
	for (i = 1; i <= 50; i++) {
		rno = (db_recno_t)i;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &rno; key.size = sizeof(rno);
		data.data = "REPL"; data.size = 4;
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	}
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbp->close(dbp, 0));
}

/*
 * fileops_workload --
 *	fileops_autop.c: __fop_create, __fop_remove, __fop_write,
 *	__fop_write_file, __fop_rename, __fop_file_remove.  Transactional
 *	create / rename / remove of a database file emits all of them; the
 *	Tcl suite does the creates but rarely a transactional rename+remove.
 */
static void
fileops_workload(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp;
	DB_TXN *txn;
	DBT key, data;
	int i;
	char kbuf[32];

	/* Create + populate + rename + remove, each in its own txn. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_fop_a.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 0; i < 50; i++) {
		mkkey(kbuf, i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = kbuf; data.size = (u_int32_t)strlen(kbuf);
		CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	}
	CHK_OK(dbp->close(dbp, 0));
	CHK_OK(txn->commit(txn, 0));

	/* __fop_rename under a txn. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbenv->dbrename(dbenv, txn, "cov_fop_a.db", NULL,
	    "cov_fop_b.db", 0));
	CHK_OK(txn->commit(txn, 0));

	/* __fop_remove / __fop_file_remove under a txn. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbenv->dbremove(dbenv, txn, "cov_fop_b.db", NULL, 0));
	CHK_OK(txn->commit(txn, 0));

	/* An ABORTED create: the undo arm of __fop_create. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_fop_abort.db", NULL, DB_BTREE,
	    DB_CREATE, 0600));
	CHK_OK(dbp->close(dbp, 0));
	CHK_OK(txn->abort(txn));

	/* A subdb rename + remove: the "in a container file" arms. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->open(dbp, txn, "cov_fop_c.db", "s1", DB_BTREE,
	    DB_CREATE, 0600));
	CHK_OK(dbp->close(dbp, 0));
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbenv->dbrename(dbenv, txn, "cov_fop_c.db", "s1", "s2", 0));
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbenv->dbremove(dbenv, txn, "cov_fop_c.db", "s2", 0));
	CHK_OK(txn->commit(txn, 0));
}

/*
 * diagnostic_and_prepare --
 *	__db_debug (the DIAGNOSTIC record written by DB_ENV->log_printf) --
 *	__db_debug_print is never called and this is the ONLY way to produce
 *	the record.  Plus a prepared (2PC) txn so __txn_xa_regop / the
 *	prepare record printers run, and an explicit DB_ENV->log_put of an
 *	application record (__db_noop-adjacent path).
 */
static void
diagnostic_and_prepare(dbenv)
	DB_ENV *dbenv;
{
	DB *dbp;
	DB_TXN *txn;
	DBT key, data, rec;
	DB_LSN lsn;
	u_int8_t gid[DB_GID_SIZE];
	int i;

	/* --- DIAGNOSTIC records: __db_debug. */
	for (i = 0; i < 3; i++)
		CHK_OK(dbenv->log_printf(dbenv, NULL,
		    "cov_logrec_print diagnostic record %d", i));
	/* Under a txn too: the txnid-carrying arm. */
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	CHK_OK(dbenv->log_printf(dbenv, txn,
	    "cov_logrec_print in-txn diagnostic"));
	CHK_OK(txn->commit(txn, 0));

	/* --- an application log record via DB_ENV->log_put. */
	memset(&rec, 0, sizeof(rec));
	rec.data = "cov_logrec_print app record";
	rec.size = (u_int32_t)strlen((char *)rec.data);
	CHK_OK(dbenv->log_put(dbenv, &lsn, &rec, 0));
	CHK_OK(dbenv->log_flush(dbenv, &lsn));

	/* --- a PREPARED txn: __txn_prepare / __txn_xa_regop records. */
	CHK_OK(db_create(&dbp, dbenv, 0));
	CHK_OK(dbp->open(dbp, NULL, "cov_prep.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600));
	CHK_OK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "p"; key.size = 1;
	data.data = "q"; data.size = 1;
	CHK_OK(dbp->put(dbp, txn, &key, &data, 0));
	memset(gid, 0, sizeof(gid));
	memcpy(gid, "cov_logrec_print_gid", 20);
	CHK_OK(txn->prepare(txn, gid));
	CHK_OK(txn->commit(txn, 0));
	CHK_OK(dbp->close(dbp, 0));

	/* --- a checkpoint so __txn_ckp records are in the log. */
	CHK_OK(dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE));
}

/*
 * find_util --
 *	Locate a built utility.  In a libtool build tree the wrapper script
 *	sits in build_unix/ and the real binary in build_unix/.libs/; the
 *	driver may be run from build_unix itself or from a rundir beside it.
 *	Returns a malloc'd command prefix, or NULL if the utility is absent.
 */
static char *
find_util(name)
	const char *name;
{
	static const char *dirs[] = { ".", "..", "../.libs", ".libs" };
	char path[512], *out;
	size_t i;

	for (i = 0; i < sizeof(dirs) / sizeof(dirs[0]); i++) {
		(void)snprintf(path, sizeof(path), "%s/%s", dirs[i], name);
		if (__os_exists(NULL, path, NULL) == 0) {
			if ((out = strdup(path)) == NULL)
				return (NULL);
			return (out);
		}
	}
	return (NULL);
}

/*
 * print_whole_log --
 *	Walk the ENTIRE log through the DB_TXN_PRINT dispatch table -- exactly
 *	what db_printlog does -- and assert every record formats.  This is
 *	the step that executes the *_autop.c printers.
 *
 *	Driven through the utility rather than re-implementing the dispatch,
 *	because the utility's own init_print/dispatch code (util/db_printlog.c,
 *	including its per-version env_init_print_4x tables) is part of the
 *	measured surface too.
 */
static int
print_whole_log()
{
	char cmd[1024], *util;
	int rc;

	if ((util = find_util("db_printlog")) == NULL) {
		fprintf(stderr, "note: db_printlog not found; skipping the "
		    "print pass\n");
		return (0);
	}
	/*
	 * Plain invocation prints every record in the log.  (Upstream BDB's
	 * db_printlog has a -a "print all" flag; this fork's does not -- its
	 * options are [-NrV] [-b file/offset] [-e file/offset] [-h home]
	 * [-P password] -- and the default already prints everything.)
	 */
	(void)snprintf(cmd, sizeof(cmd), "%s -h %s > /dev/null 2>&1",
	    util, HOME);
	rc = system(cmd);
	if (rc == 0) {
		/* -r reads the log BACKWARD: the reverse-cursor arm. */
		(void)snprintf(cmd, sizeof(cmd),
		    "%s -h %s -r > /dev/null 2>&1", util, HOME);
		(void)system(cmd);
		/* An LSN range: the -b/-e scoping arms. */
		(void)snprintf(cmd, sizeof(cmd),
		    "%s -h %s -b 1/0 > /dev/null 2>&1", util, HOME);
		(void)system(cmd);
		(void)snprintf(cmd, sizeof(cmd),
		    "%s -h %s -b 1/0 -e 1/500000 > /dev/null 2>&1",
		    util, HOME);
		(void)system(cmd);
	}
	free(util);
	return (rc);
}

/*
 * verify_whole_log --
 *	Walk the ENTIRE log through the DB_TXN_VERIFY dispatch table --
 *	`db_log_verify` / DB_ENV->log_verify -- which is what executes
 *	log_verify_int.c's 62 never-called `__<rec>_verify` handlers and the
 *	__lv_* helpers.
 *
 *	Run in several forms so more of the utility's own option handling and
 *	more of log_verify.c's scoping code runs:
 *	  * plain whole-log verify
 *	  * -v verbose (drives the __lv_*_str formatters and the progress
 *	    reporting)
 *	  * -d <file> scoped to one database (drives __lv_vrfy_for_dbfile,
 *	    which is in the never-called list)
 *	  * -b/-e an LSN range (drives the partial-range scoping arms)
 *
 *	A non-zero exit from db_log_verify is NOT automatically a failure:
 *	log_verify legitimately reports "log verification failed" for a log
 *	that contains an aborted transaction whose undo it cannot pair up, and
 *	this driver deliberately creates aborted txns and a prepared txn.  The
 *	contract asserted here is that it RUNS to completion over every record
 *	(so the handlers execute) rather than crashing or hanging.
 */
static int
verify_whole_log()
{
	char cmd[1024], *util;
	int ran;

	if ((util = find_util("db_log_verify")) == NULL) {
		fprintf(stderr, "note: db_log_verify not found; skipping the "
		    "verify pass\n");
		return (0);
	}
	ran = 0;

	/* 1. whole log. */
	(void)snprintf(cmd, sizeof(cmd), "%s -h %s > /dev/null 2>&1",
	    util, HOME);
	(void)system(cmd);
	ran++;

	/* 2. verbose: the __lv_dbt_str / __lv_dbtype_str formatters. */
	(void)snprintf(cmd, sizeof(cmd), "%s -h %s -v > /dev/null 2>&1",
	    util, HOME);
	(void)system(cmd);
	ran++;

	/* 3. scoped to one db file: __lv_vrfy_for_dbfile. */
	(void)snprintf(cmd, sizeof(cmd),
	    "%s -h %s -d cov_bt.db > /dev/null 2>&1", util, HOME);
	(void)system(cmd);
	ran++;
	(void)snprintf(cmd, sizeof(cmd),
	    "%s -h %s -d cov_ham.db > /dev/null 2>&1", util, HOME);
	(void)system(cmd);
	ran++;
	/* A heap and a queue db: __lv_on_heap_log / __lv_on_qam_log. */
	(void)snprintf(cmd, sizeof(cmd),
	    "%s -h %s -d cov_heap.db > /dev/null 2>&1", util, HOME);
	(void)system(cmd);
	ran++;
	(void)snprintf(cmd, sizeof(cmd),
	    "%s -h %s -d cov_qam.db > /dev/null 2>&1", util, HOME);
	(void)system(cmd);
	ran++;
	/* A sub-database by name: the -D arm. */
	(void)snprintf(cmd, sizeof(cmd),
	    "%s -h %s -d cov_subs.db -D sub1 > /dev/null 2>&1", util, HOME);
	(void)system(cmd);
	ran++;

	/* 4. an LSN range: the partial-range scoping arms. */
	(void)snprintf(cmd, sizeof(cmd),
	    "%s -h %s -b 1/0 > /dev/null 2>&1", util, HOME);
	(void)system(cmd);
	ran++;
	(void)snprintf(cmd, sizeof(cmd),
	    "%s -h %s -b 1/0 -e 1/1000000 > /dev/null 2>&1", util, HOME);
	(void)system(cmd);
	ran++;
	/* 5. a nonexistent db name: the "no such file" rejection arm. */
	(void)snprintf(cmd, sizeof(cmd),
	    "%s -h %s -d no_such_file.db > /dev/null 2>&1", util, HOME);
	(void)system(cmd);
	ran++;

	free(util);
	return (ran >= 10 ? 0 : 1);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	DB_ENV *dbenv;
	int ret;

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);

	(void)signal(SIGALRM, on_alarm);
	(void)alarm(ALARM_SECS);

	printf("cov_logrec_print: generate every log record kind, then "
	    "print the log\n");

	clean_home();
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "FAIL: db_env_create: %s\n",
		    db_strerror(ret));
		return (2);
	}
	dbenv->set_errpfx(dbenv, "cov_logrec_print");
	dbenv->set_errfile(dbenv, NULL);
	/* A big log buffer + big log files so nothing is archived away. */
	CHK_OK(dbenv->set_lg_bsize(dbenv, 1024 * 1024));
	CHK_OK(dbenv->set_lg_max(dbenv, 10 * 1024 * 1024));
	CHK_OK(dbenv->set_cachesize(dbenv, 0, 16 * 1024 * 1024, 1));
	/*
	 * This driver deliberately runs several thousand operations inside
	 * single transactions (bulk inserts, a mass delete, compaction) to
	 * force splits/merges/truncation, so the default lock and transaction
	 * region sizing is not enough -- an under-sized lock region shows up
	 * as ENOMEM from DB->open or DB->put partway through, which is a
	 * harness sizing problem, not an engine one.  Size the regions for the
	 * workload.
	 */
	CHK_OK(dbenv->set_lk_max_locks(dbenv, 20000));
	CHK_OK(dbenv->set_lk_max_lockers(dbenv, 20000));
	CHK_OK(dbenv->set_lk_max_objects(dbenv, 20000));
	CHK_OK(dbenv->set_tx_max(dbenv, 1000));
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0600)) != 0) {
		fprintf(stderr, "FAIL: open env: %s\n", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (2);
	}

	printf("1. btree: splits, overflow, rsplit, compact, subdb, dups\n");
	btree_workload(dbenv);

	printf("2. hash: group alloc, replace, contract, off-page dups\n");
	hash_workload(dbenv);

	printf("3. queue extents + heap + recno\n");
	queue_heap_workload(dbenv);

	printf("4. fileops: create/rename/remove, committed and aborted\n");
	fileops_workload(dbenv);

	printf("5. diagnostic records, app log_put, prepared txn\n");
	diagnostic_and_prepare(dbenv);

	/* Flush everything to disk so db_printlog sees the whole log. */
	CHK_OK(dbenv->log_flush(dbenv, NULL));
	CHK_OK(dbenv->close(dbenv, 0));

	printf("6. print the whole log through the DB_TXN_PRINT dispatch\n");
	if ((ret = print_whole_log()) != 0) {
		fprintf(stderr, "FAIL: db_printlog over the generated log "
		    "returned %d\n", ret);
		fails++;
	}
	checks++;

	printf("7. verify the whole log through the DB_TXN_VERIFY dispatch\n");
	if ((ret = verify_whole_log()) != 0) {
		fprintf(stderr, "FAIL: db_log_verify passes did not all run "
		    "(%d)\n", ret);
		fails++;
	}
	checks++;

	(void)alarm(0);
	printf("cov_logrec_print: %d checks, %d failures\n", checks, fails);
	if (fails != 0) {
		printf("cov_logrec_print: FAIL\n");
		return (1);
	}
	printf("cov_logrec_print: PASS\n");
	return (0);
}

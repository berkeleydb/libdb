/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2024 Oracle and/or its affiliates.  All rights reserved.
 *
 * xa_direct.c --
 *	A Tuxedo-free driver for Berkeley DB's X/Open XA resource-manager
 *	switch (db_xa_switch, src/xa/xa.c + xa_map.c).
 *
 *	The full test/xa harness (chk.xa + src1..src5) requires an Oracle
 *	Tuxedo transaction manager (buildserver/tmboot/atmi.h/fml32.h) which
 *	is not available in this build.  This program instead acts as its own
 *	minimal transaction manager: it calls the db_xa_switch entry points
 *	directly, exactly as a real TM would, and performs data-plane work
 *	through DB handles opened with DB_XA_CREATE.
 *
 *	Everything runs in a SINGLE process (no cross-process recovery, which
 *	requires an external orchestrator and can deadlock the in-doubt scan)
 *	and every transaction is fully resolved before exit, so the test can
 *	never leave a prepared txn holding locks and can never hang.  A hard
 *	SIGALRM guard (ALARM_SECS) aborts the process if anything blocks.
 *
 *	Exercises: __db_xa_open/close, __db_xa_start/end, __xa_get_txn/
 *	__xa_put_txn, __db_xa_prepare -> DB_TXN->prepare, __db_xa_commit/
 *	__db_xa_rollback, __db_xa_recover -> __txn_get_prepared (the XA
 *	recovery scan, DB_FIRST/DB_NEXT), __db_xa_forget, plus xa_map.c
 *	(__db_map_rmid, __db_rmid_to_env, __db_xid_to_txn, __db_unmap_rmid),
 *	and the protocol/argument error paths (XAER_NOTA/INVAL/PROTO/ASYNC).
 */

#include <sys/types.h>

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"
#include "xa.h"

/* db_xa_switch is exported by libdb but not declared in the public db.h. */
extern const struct xa_switch_t db_xa_switch;

#define	HOME		"XA_TESTDIR"
#define	RMID		1
#define	TABLE		"xa_table.db"
#define	ALARM_SECS	60		/* hard self-timeout: never hang */

static int fails = 0;

#define	CHK(call, want) do {						\
	int _r = (call);						\
	if (_r != (want)) {						\
		fprintf(stderr,						\
		    "FAIL: %s:%d: %s => %d, expected %d\n",		\
		    __FILE__, __LINE__, #call, _r, (want));		\
		fails++;						\
	}								\
} while (0)

#define	CHK_OK(call)	CHK(call, 0)

static void
on_alarm(int sig)
{
	(void)sig;
	fprintf(stderr, "FAIL: xa_direct timed out after %d s (hung)\n",
	    ALARM_SECS);
	_exit(3);
}

/* Build a distinct XID for transaction number n. */
static void
mkxid(XID *xid, int n)
{
	memset(xid, 0, sizeof(*xid));
	xid->formatID = 0x4244;			/* "DB" */
	xid->gtrid_length = 8;
	xid->bqual_length = 4;
	memcpy(&xid->data[0], "GTRID000", 8);
	xid->data[7] = (char)('0' + (n % 10));
	memcpy(&xid->data[8], "BQ00", 4);
	xid->data[11] = (char)('0' + (n % 10));
}

/*
 * xa_db_open --
 *	Open a DB_XA_CREATE handle bound to the current XA env.  Must be done
 *	OUTSIDE any global XA transaction (XA_NO_TXN); a real Tuxedo server
 *	opens its handles once in tpsvrinit, before any xa_start.
 */
static DB *
xa_db_open(void)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, NULL, DB_XA_CREATE)) != 0) {
		fprintf(stderr, "db_create(XA): %s\n", db_strerror(ret));
		return (NULL);
	}
	dbp->set_errfile(dbp, stderr);
	if ((ret = dbp->open(dbp, NULL, TABLE, NULL, DB_BTREE,
	    DB_AUTO_COMMIT | DB_CREATE | DB_THREAD, 0664)) != 0) {
		fprintf(stderr, "DB->open: %s\n", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return (NULL);
	}
	return (dbp);
}

static int
xa_put(DB *dbp, const char *k, const char *v)
{
	DBT key, data;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)k;
	key.size = (u_int32_t)strlen(k) + 1;
	data.data = (void *)v;
	data.size = (u_int32_t)strlen(v) + 1;
	return (dbp->put(dbp, NULL, &key, &data, 0));
}

/* Count records via the XA-owned handle (no autocommit txn: read-only). */
static int
count_records(DB *dbp)
{
	DBC *dbc;
	DBT key, data;
	int n, ret;

	if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0) {
		fprintf(stderr, "cursor: %s\n", db_strerror(ret));
		return (-1);
	}
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (n = 0; dbc->get(dbc, &key, &data, DB_NEXT) == 0; )
		n++;
	(void)dbc->close(dbc);
	return (n);
}

int
main(int argc, char *argv[])
{
	const struct xa_switch_t *xa = &db_xa_switch;
	XID xid_c, xid_a, xid_1p, xid_prep, bogus, list[10];
	DB *dbp;
	int found, i, n;

	(void)argc;
	(void)argv;

	/* Hard guard: if anything blocks, die instead of hanging forever. */
	(void)signal(SIGALRM, on_alarm);
	(void)alarm(ALARM_SECS);

	fprintf(stderr, "== xa_direct: TM drives db_xa_switch directly ==\n");

	/* xa_open: create/attach the RM (env) for this rmid. */
	CHK(xa->xa_open_entry((char *)HOME, RMID, TMNOFLAGS), XA_OK);
	/* xa_open again with same rmid: bumps ref count -> XA_OK. */
	CHK(xa->xa_open_entry((char *)HOME, RMID, TMNOFLAGS), XA_OK);

	/* Open the DB handle before any xa_start (see xa_db_open). */
	if ((dbp = xa_db_open()) == NULL) {
		fprintf(stderr, "XA DIRECT TEST: cannot open DB\n");
		return (1);
	}

	/* --- Branch 1: start -> put -> end -> prepare -> commit (2PC) --- */
	mkxid(&xid_c, 1);
	CHK(xa->xa_start_entry(&xid_c, RMID, TMNOFLAGS), XA_OK);
	CHK_OK(xa_put(dbp, "commit-k1", "v1"));
	CHK_OK(xa_put(dbp, "commit-k2", "v2"));
	CHK(xa->xa_end_entry(&xid_c, RMID, TMSUCCESS), XA_OK);
	CHK(xa->xa_prepare_entry(&xid_c, RMID, TMNOFLAGS), XA_OK);
	CHK(xa->xa_commit_entry(&xid_c, RMID, TMNOFLAGS), XA_OK);

	/* --- Branch 2: start -> put -> end -> prepare -> rollback --- */
	mkxid(&xid_a, 2);
	CHK(xa->xa_start_entry(&xid_a, RMID, TMNOFLAGS), XA_OK);
	CHK_OK(xa_put(dbp, "abort-k1", "x1"));
	CHK(xa->xa_end_entry(&xid_a, RMID, TMSUCCESS), XA_OK);
	CHK(xa->xa_prepare_entry(&xid_a, RMID, TMNOFLAGS), XA_OK);
	CHK(xa->xa_rollback_entry(&xid_a, RMID, TMNOFLAGS), XA_OK);

	/* --- Branch 3: one-phase commit (TMONEPHASE), no prepare --- */
	mkxid(&xid_1p, 3);
	CHK(xa->xa_start_entry(&xid_1p, RMID, TMNOFLAGS), XA_OK);
	CHK_OK(xa_put(dbp, "onephase-k1", "1p"));
	CHK(xa->xa_end_entry(&xid_1p, RMID, TMSUCCESS), XA_OK);
	CHK(xa->xa_commit_entry(&xid_1p, RMID, TMONEPHASE), XA_OK);

	/*
	 * --- Branch 4: start -> put -> end -> prepare, then resolve via the
	 * XA recovery scan (xa_recover), driving __txn_get_prepared with
	 * DB_FIRST/DB_NEXT.  We stay in-process so no external orchestrator is
	 * needed; the prepared txn is found and committed below.
	 */
	mkxid(&xid_prep, 4);
	CHK(xa->xa_start_entry(&xid_prep, RMID, TMNOFLAGS), XA_OK);
	CHK_OK(xa_put(dbp, "prepared-k1", "recovered"));
	CHK(xa->xa_end_entry(&xid_prep, RMID, TMSUCCESS), XA_OK);
	CHK(xa->xa_prepare_entry(&xid_prep, RMID, TMNOFLAGS), XA_OK);

	/* Recovery scan: enumerate prepared (in-doubt) txns. */
	memset(list, 0, sizeof(list));
	found = xa->xa_recover_entry(list, 10, RMID, TMSTARTRSCAN | TMENDRSCAN);
	fprintf(stderr, "xa_recover found %d prepared txn(s)\n", found);
	if (found < 1) {
		fprintf(stderr, "FAIL: expected >=1 prepared txn, got %d\n",
		    found);
		fails++;
	}
	/* Commit each recovered branch (only branch 4 is prepared here). */
	for (i = 0; i < found; i++)
		CHK(xa->xa_commit_entry(&list[i], RMID, TMNOFLAGS), XA_OK);

	/*
	 * Verify effect: branch1 (2 recs) + branch3 (1) + branch4 (1) = 4;
	 * branch2 rolled back.
	 */
	n = count_records(dbp);
	fprintf(stderr, "record count = %d (expected 4)\n", n);
	if (n != 4) {
		fprintf(stderr, "FAIL: record count %d != 4\n", n);
		fails++;
	}

	/* --- Error-path coverage: protocol / argument violations --- */
	mkxid(&bogus, 9);
	CHK(xa->xa_commit_entry(&bogus, RMID, TMNOFLAGS), XAER_NOTA);
	CHK(xa->xa_prepare_entry(&bogus, RMID, TMNOFLAGS), XAER_NOTA);
	CHK(xa->xa_rollback_entry(&bogus, RMID, TMNOFLAGS), XAER_NOTA);
	CHK(xa->xa_forget_entry(&bogus, RMID, TMNOFLAGS), XA_OK);
	CHK(xa->xa_open_entry((char *)HOME, RMID, TMASYNC), XAER_ASYNC);
	CHK(xa->xa_complete_entry(NULL, NULL, RMID, TMNOFLAGS), XAER_INVAL);
	CHK(xa->xa_start_entry(&bogus, 999 /* bad rmid */, TMNOFLAGS),
	    XAER_PROTO);

	/* Close the handle, then close the RM twice (ref count: 2 opens). */
	(void)dbp->close(dbp, 0);
	CHK(xa->xa_close_entry((char *)HOME, RMID, TMNOFLAGS), XA_OK);
	CHK(xa->xa_close_entry((char *)HOME, RMID, TMNOFLAGS), XA_OK);

	(void)alarm(0);
	if (fails != 0) {
		fprintf(stderr, "XA DIRECT TEST: %d FAILURE(S)\n", fails);
		return (1);
	}
	fprintf(stderr, "XA DIRECT TEST: PASS\n");
	return (0);
}

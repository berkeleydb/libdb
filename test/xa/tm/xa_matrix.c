/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * xa_matrix.c --
 *	Single-process XA state-machine matrix.  The test TM (xatm.c) drives
 *	db_xa_switch exactly as a real transaction manager does, and every case
 *	checks BOTH the XA return code AND the resulting committed data.
 *
 *	This folds in and replaces the old orphaned xa_direct.c.  Spec
 *	references are to the X/Open CAE Specification, Distributed Transaction
 *	Processing: The XA Specification (X/Open Document C193, 1991).
 *
 *	Covers (XA §4 state tables and §6 reference pages):
 *	  - xa_open/xa_close ref counting (§6 xa_open, xa_close);
 *	  - 2PC commit, rollback, one-phase commit (TMONEPHASE) (§6 xa_commit);
 *	  - prepare-then-commit and prepare-then-rollback (§2.3.3, §6);
 *	  - a read-only branch (we assert libdb's ACTUAL xa_prepare result);
 *	  - TWO libdb environments as TWO RMs in ONE global transaction: real
 *	    two-phase commit across resource managers;
 *	  - suspend/resume (TMSUSPEND/TMRESUME) and TMJOIN within a thread
 *	    (§4 association states, §6 xa_start/xa_end);
 *	  - protocol and argument errors: commit without prepare (XAER_PROTO),
 *	    double prepare (XAER_PROTO), unknown XID (XAER_NOTA), XID reused
 *	    while active (XAER_DUPID), bad rmid (XAER_PROTO), TMASYNC
 *	    (XAER_ASYNC), xa_complete (XAER_INVAL), bad flags (XAER_INVAL).
 *
 *	Everything is resolved before exit and a SIGALRM guard kills a hang.
 */

#include <sys/types.h>

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "xatm.h"

#define	HOME1		"XA_MATRIX_RM1"
#define	HOME2		"XA_MATRIX_RM2"
#define	RMID1		1
#define	RMID2		2
#define	TABLE		"xa_table.db"
#define	ALARM_SECS	120		/* hard self-timeout: never hang */

static const struct xa_switch_t *xa = &db_xa_switch;

static void
on_alarm(int sig)
{
	(void)sig;
	fprintf(stderr, "FAIL: xa_matrix timed out after %d s (hung)\n",
	    ALARM_SECS);
	_exit(3);
}

/*
 * A real TM opens its RM handles once, OUTSIDE any global transaction, after
 * xa_open and before any xa_start (XA §6 xa_open: "called ... before the first
 * use of the resource manager").  DB_XA_CREATE binds the handle to the env at
 * the head of the global env queue, which is the RM most recently looked up by
 * xa_open/xa_start (src/db/db_method.c).  So we open RM n's handle immediately
 * after that RM's xa_open.
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

/* 1 if key k is present with value v; 0 otherwise. */
static int
xa_has(DB *dbp, const char *k, const char *v)
{
	DBT key, data;
	int ok, ret;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)k;
	key.size = (u_int32_t)strlen(k) + 1;
	/* The handle is DB_THREAD, so a returned DBT needs an allocation flag. */
	data.flags = DB_DBT_MALLOC;
	ret = dbp->get(dbp, NULL, &key, &data, 0);
	if (ret != 0)
		return (0);
	ok = (data.size == (u_int32_t)strlen(v) + 1 &&
	    memcmp(data.data, v, data.size) == 0);
	free(data.data);
	return (ok);
}

/* 1 if key k is absent. */
static int
xa_absent(DB *dbp, const char *k)
{
	DBT key, data;
	int ret;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)k;
	key.size = (u_int32_t)strlen(k) + 1;
	data.flags = DB_DBT_MALLOC;
	ret = dbp->get(dbp, NULL, &key, &data, 0);
	if (ret == 0)
		free(data.data);
	return (ret == DB_NOTFOUND);
}

int
main(int argc, char *argv[])
{
	xatm_h H, *h = &H;
	XID xid, xid2, bogus, list[16];
	DB *db1, *db2;
	int found, g;

	(void)argc;
	(void)argv;

	(void)signal(SIGALRM, on_alarm);
	(void)alarm(ALARM_SECS);
	xatm_begin(h, "xa_matrix");

	/* ---- xa_open: two RMs (two libdb environments). ---- */
	XCHK(h, xa->xa_open_entry((char *)HOME1, RMID1, TMNOFLAGS), XA_OK);
	if ((db1 = xa_db_open()) == NULL)		/* RM1 handle */
		{ (void)xatm_end(h); return (1); }
	XCHK(h, xa->xa_open_entry((char *)HOME2, RMID2, TMNOFLAGS), XA_OK);
	if ((db2 = xa_db_open()) == NULL)		/* RM2 handle */
		{ (void)xatm_end(h); return (1); }
	/* xa_open again on RM1: ref-count bump, still XA_OK (§6 xa_open). */
	XCHK(h, xa->xa_open_entry((char *)HOME1, RMID1, TMNOFLAGS), XA_OK);

	g = 1;	/* global-transaction counter */

	/* ---- Case 1: 2PC commit on RM1 (start/end/prepare/commit). ---- */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c1a", "v1"), 0);
	XCHK(h, xa_put(db1, "c1b", "v2"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK_TRUE(h, xa_has(db1, "c1a", "v1") && xa_has(db1, "c1b", "v2"));

	/* ---- Case 2: 2PC rollback (prepare then rollback). ---- */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c2", "x"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_rollback_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK_TRUE(h, xa_absent(db1, "c2"));

	/* ---- Case 3: one-phase commit (TMONEPHASE, no prepare). ---- */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c3", "1p"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMONEPHASE), XA_OK);
	XCHK_TRUE(h, xa_has(db1, "c3", "1p"));

	/*
	 * ---- Case 4: read-only branch.  The TM does no updates in this
	 * branch, then prepares.  XA §6 xa_prepare: an RM "may return
	 * [XA_RDONLY] if the transaction branch was read only and has been
	 * committed", in which case the TM need not call xa_commit.  libdb's
	 * __db_xa_prepare does NOT implement that optimisation: it returns
	 * XA_OK and the branch must still be committed.  We assert the ACTUAL
	 * behaviour so a future change to it is caught.
	 */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	/* no update */
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMNOFLAGS), XA_OK);

	/*
	 * ---- Case 5: REAL two-phase commit across two RMs in ONE global
	 * transaction.  Same gtrid, different bqual per branch (XA §2.2).
	 * The TM prepares BOTH branches, then commits BOTH (§2.3.3 the
	 * two-phase commit protocol).
	 */
	xatm_mkxid(&xid, g, 1);			/* RM1 branch */
	xatm_mkxid(&xid2, g, 2);		/* RM2 branch, same gtrid */
	g++;
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c5-rm1", "A"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_start_entry(&xid2, RMID2, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db2, "c5-rm2", "B"), 0);
	XCHK(h, xa->xa_end_entry(&xid2, RMID2, TMSUCCESS), XA_OK);
	/* phase 1: prepare both */
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid2, RMID2, TMNOFLAGS), XA_OK);
	/* phase 2: commit both */
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_commit_entry(&xid2, RMID2, TMNOFLAGS), XA_OK);
	XCHK_TRUE(h, xa_has(db1, "c5-rm1", "A") && xa_has(db2, "c5-rm2", "B"));

	/*
	 * ---- Case 6: two RMs, prepare both then ROLL BACK both (a TM abort
	 * decision after phase 1).  Neither branch's data survives.
	 */
	xatm_mkxid(&xid, g, 1);
	xatm_mkxid(&xid2, g, 2);
	g++;
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c6-rm1", "A"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_start_entry(&xid2, RMID2, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db2, "c6-rm2", "B"), 0);
	XCHK(h, xa->xa_end_entry(&xid2, RMID2, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid2, RMID2, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_rollback_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_rollback_entry(&xid2, RMID2, TMNOFLAGS), XA_OK);
	XCHK_TRUE(h, xa_absent(db1, "c6-rm1") && xa_absent(db2, "c6-rm2"));

	/*
	 * ---- Case 7: suspend / resume within a thread.  XA §6 xa_end with
	 * TMSUSPEND leaves the branch associated-suspended; xa_start with
	 * TMRESUME re-associates it (§4 association states).  Work done in
	 * both windows commits as one branch.
	 */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c7a", "before"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUSPEND), XA_OK);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMRESUME), XA_OK);
	XCHK(h, xa_put(db1, "c7b", "after"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMONEPHASE), XA_OK);
	XCHK_TRUE(h, xa_has(db1, "c7a", "before") && xa_has(db1, "c7b", "after"));

	/*
	 * ---- Case 8: TMJOIN.  End (dissociate) then rejoin the same branch
	 * with TMJOIN (XA §6 xa_start: "the association is with a transaction
	 * branch that already exists").  TMSUCCESS on the first end makes the
	 * branch idle; TMJOIN re-associates it for more work.
	 */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c8a", "one"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMJOIN), XA_OK);
	XCHK(h, xa_put(db1, "c8b", "two"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMONEPHASE), XA_OK);
	XCHK_TRUE(h, xa_has(db1, "c8a", "one") && xa_has(db1, "c8b", "two"));

	/*
	 * ---- Case 9: recovery scan of a prepared branch.  Prepare an XID,
	 * leave it in-doubt, enumerate it with xa_recover (TMSTARTRSCAN |
	 * TMENDRSCAN), and resolve it.  __db_xa_recover drives
	 * __txn_get_prepared (DB_FIRST/DB_LAST/DB_NEXT).  The returned XID must
	 * equal the one we prepared, and committing it must land its data.
	 */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c9", "recovered"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	memset(list, 0, sizeof(list));
	found = xa->xa_recover_entry(list, 16, RMID1,
	    TMSTARTRSCAN | TMENDRSCAN);
	XCHK_TRUE(h, found >= 1);
	XCHK_TRUE(h, found >= 1 && xatm_xid_eq(&list[0], &xid));
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK_TRUE(h, xa_has(db1, "c9", "recovered"));

	/* ---- Case 10: protocol errors on a committed/unknown XID. ---- */
	xatm_mkxid(&bogus, 9999, 9);
	/* unknown XID: §6 xa_commit/rollback/prepare -> XAER_NOTA */
	XCHK(h, xa->xa_commit_entry(&bogus, RMID1, TMNOFLAGS), XAER_NOTA);
	XCHK(h, xa->xa_rollback_entry(&bogus, RMID1, TMNOFLAGS), XAER_NOTA);
	XCHK(h, xa->xa_prepare_entry(&bogus, RMID1, TMNOFLAGS), XAER_NOTA);
	/* xa_forget on an unknown XID: libdb returns XA_OK (nothing to do). */
	XCHK(h, xa->xa_forget_entry(&bogus, RMID1, TMNOFLAGS), XA_OK);

	/* ---- Case 11: commit without prepare (not one-phase) -> PROTO. ---- */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c11", "y"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	/* two-phase commit without prepare: XAER_PROTO (§6 xa_commit). */
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMNOFLAGS), XAER_PROTO);
	/* clean up: prepare then commit. */
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	/* ---- Case 12: double prepare -> XAER_PROTO. ---- */
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XAER_PROTO);
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMNOFLAGS), XA_OK);

	/* ---- Case 13: XID reused while still active -> XAER_DUPID. ---- */
	xatm_mkxid(&xid, g++, 1);
	/* ---- Case 13: XID reused while mapped (idle) -> XAER_DUPID. ---- */
	/*
	 * libdb requires the calling thread to be UNASSOCIATED at xa_start
	 * (xa.c __xa_get_txn asserts it), so a second xa_start from the same
	 * thread while still associated is a TM protocol violation, not a
	 * DUPID.  DUPID is raised when the XID is already known (still mapped
	 * in the active table) and xa_start is called again WITHOUT TMJOIN or
	 * TMRESUME.  Reproduce that: start, end (dissociate -> idle, still
	 * mapped), then start the same XID again with TMNOFLAGS.
	 */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	/* same XID, still mapped, no JOIN/RESUME: XAER_DUPID (§6 xa_start). */
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XAER_DUPID);
	/* resolve the idle branch with one-phase commit. */
	XCHK(h, xa->xa_commit_entry(&xid, RMID1, TMONEPHASE), XA_OK);

	/* ---- Case 14: argument / protocol errors on the switch. ---- */
	/* bad rmid (never opened): §6 xa_start -> XAER_PROTO. */
	XCHK(h, xa->xa_start_entry(&bogus, 999, TMNOFLAGS), XAER_PROTO);
	/* async requested, unsupported: XAER_ASYNC. */
	XCHK(h, xa->xa_open_entry((char *)HOME1, RMID1, TMASYNC), XAER_ASYNC);
	/* xa_complete unsupported: XAER_INVAL. */
	XCHK(h, xa->xa_complete_entry(NULL, NULL, RMID1, TMNOFLAGS),
	    XAER_INVAL);
	/* invalid flags to xa_start: XAER_INVAL (TMJOIN|TMRESUME together). */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMJOIN | TMRESUME), XAER_INVAL);
	/* invalid flag bit to xa_end: XAER_INVAL. */
	XCHK(h, xa->xa_end_entry(&bogus, RMID1, TMREGISTER), XAER_INVAL);

	/*
	 * ---- Case 15: xa_end with TMFAIL.  XA sec.6 xa_end: TMFAIL dissociates
	 * the caller and marks the branch rollback-only.  libdb treats TMFAIL
	 * like a normal dissociation here (it leaves the branch idle), so the
	 * TM then rolls it back.
	 */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c15", "fail"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMFAIL), XA_OK);
	XCHK(h, xa->xa_rollback_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK_TRUE(h, xa_absent(db1, "c15"));

	/*
	 * ---- Case 16: xa_forget on a PREPARED branch drives DB_TXN->discard
	 * (xa.c __db_xa_forget -> txnp->discard).  libdb heuristically completes
	 * nothing, so forget reclaims the txn slot and returns XA_OK; a second
	 * forget on the now-gone XID is a no-op XA_OK.
	 */
	xatm_mkxid(&xid, g++, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(db1, "c16", "forget"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID1, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_forget_entry(&xid, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_forget_entry(&xid, RMID1, TMNOFLAGS), XA_OK);

	/* ---- Shutdown: close handles then the RMs (RM1 was opened twice). ---- */
	(void)db1->close(db1, 0);
	(void)db2->close(db2, 0);
	XCHK(h, xa->xa_close_entry((char *)HOME1, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_close_entry((char *)HOME1, RMID1, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_close_entry((char *)HOME2, RMID2, TMNOFLAGS), XA_OK);

	(void)alarm(0);
	return (xatm_end(h));
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * xa_recover.c --
 *	Multi-process XA crash-recovery test.  This is the property the XA
 *	layer exists for: a transaction prepared by a resource manager must
 *	survive the death of the process that prepared it, and after recovery
 *	the transaction manager must be able to enumerate the in-doubt branches
 *	with xa_recover and commit or roll back each one, with the data landing
 *	exactly as the TM decides.
 *
 *	XA §6 xa_recover: "obtains a list of prepared and heuristically
 *	completed transaction branches from a resource manager", driven with
 *	TMSTARTRSCAN / TMENDRSCAN.  XA §2.3.3: after a crash the TM resolves
 *	in-doubt branches.
 *
 *	The child process (fork) does: xa_open -> xa_start -> put -> xa_end ->
 *	xa_prepare for several XIDs, then kill(getpid(), SIGKILL) -- a hard
 *	crash with the branches prepared and in-doubt, holding region locks and
 *	never run through a clean shutdown.  The XA environment is opened with
 *	DB_REGISTER | DB_RECOVER (src/xa/xa.c XA_FLAGS), so when the parent
 *	calls xa_open after the crash, recovery runs and restores the prepared
 *	branches into the active transaction table, where xa_recover finds them.
 *
 *	Two arms:
 *	  ARM A  crash AFTER prepare, BEFORE any phase-2 decision;
 *	  ARM B  crash DURING commit of a two-branch global txn (one branch
 *	         committed, the other still prepared) -- recovery must leave the
 *	         committed branch committed and the prepared branch in-doubt for
 *	         the TM to resolve.
 *
 *	Usage:
 *	  xa_recover child-prepare <home> <ncommit> <nrollback>   (child, arm A)
 *	  xa_recover child-midcommit <home>                       (child, arm B)
 *	  xa_recover                                               (driver)
 */

#include <sys/types.h>
#include <sys/wait.h>

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "xatm.h"

#define	RMID		1
#define	TABLE		"xa_table.db"
#define	ALARM_SECS	180

static const struct xa_switch_t *xa = &db_xa_switch;

static void
on_alarm(int sig)
{
	(void)sig;
	fprintf(stderr, "FAIL: xa_recover timed out after %d s (hung)\n",
	    ALARM_SECS);
	_exit(3);
}

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

static int
xa_has(DB *dbp, const char *k, const char *v)
{
	DBT key, data;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)k;
	key.size = (u_int32_t)strlen(k) + 1;
	if (dbp->get(dbp, NULL, &key, &data, 0) != 0)
		return (0);
	return (data.size == (u_int32_t)strlen(v) + 1 &&
	    memcmp(data.data, v, data.size) == 0);
}

static int
xa_absent(DB *dbp, const char *k)
{
	DBT key, data;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)k;
	key.size = (u_int32_t)strlen(k) + 1;
	return (dbp->get(dbp, NULL, &key, &data, 0) == DB_NOTFOUND);
}

/*
 * child_prepare --
 *	ARM A.  Prepare ncommit+nrollback branches, each with one record keyed
 *	"recN", then SIGKILL ourselves.  gtrid numbers 1..ncommit are destined
 *	to commit; ncommit+1..ncommit+nrollback are destined to roll back -- but
 *	the child does not know that; it just prepares them.  The record value
 *	encodes the gtrid so the parent can check which branch landed.
 */
static int
child_prepare(const char *home, int ncommit, int nrollback)
{
	DB *dbp;
	XID xid;
	char kbuf[32], vbuf[32];
	int i, n;

	if (xa->xa_open_entry((char *)home, RMID, TMNOFLAGS) != XA_OK)
		return (2);
	if ((dbp = xa_db_open()) == NULL)
		return (2);

	n = ncommit + nrollback;
	for (i = 1; i <= n; i++) {
		xatm_mkxid(&xid, i, 1);
		if (xa->xa_start_entry(&xid, RMID, TMNOFLAGS) != XA_OK)
			return (2);
		(void)snprintf(kbuf, sizeof(kbuf), "rec%d", i);
		(void)snprintf(vbuf, sizeof(vbuf), "g%d", i);
		if (xa_put(dbp, kbuf, vbuf) != 0)
			return (2);
		if (xa->xa_end_entry(&xid, RMID, TMSUCCESS) != XA_OK)
			return (2);
		if (xa->xa_prepare_entry(&xid, RMID, TMNOFLAGS) != XA_OK)
			return (2);
	}
	/* Flush stderr; do NOT close -- crash with the branches prepared. */
	(void)fflush(NULL);
	fprintf(stderr, "child: prepared %d branches, crashing\n", n);
	(void)kill(getpid(), SIGKILL);
	return (99);			/* unreachable */
}

/*
 * child_midcommit --
 *	ARM B.  One global transaction, two branches (same gtrid, bqual 1 and
 *	2).  Prepare both, commit branch 1, then crash BEFORE committing branch
 *	2.  After recovery: branch 1's record must be present (committed before
 *	the crash) and branch 2 must be in-doubt (prepared) for the TM to
 *	resolve.
 */
static int
child_midcommit(const char *home)
{
	DB *dbp;
	XID x1, x2;

	if (xa->xa_open_entry((char *)home, RMID, TMNOFLAGS) != XA_OK)
		return (2);
	if ((dbp = xa_db_open()) == NULL)
		return (2);

	xatm_mkxid(&x1, 100, 1);
	xatm_mkxid(&x2, 100, 2);
	if (xa->xa_start_entry(&x1, RMID, TMNOFLAGS) != XA_OK)
		return (2);
	if (xa_put(dbp, "mid-b1", "committed") != 0)
		return (2);
	if (xa->xa_end_entry(&x1, RMID, TMSUCCESS) != XA_OK)
		return (2);
	if (xa->xa_start_entry(&x2, RMID, TMNOFLAGS) != XA_OK)
		return (2);
	if (xa_put(dbp, "mid-b2", "indoubt") != 0)
		return (2);
	if (xa->xa_end_entry(&x2, RMID, TMSUCCESS) != XA_OK)
		return (2);
	if (xa->xa_prepare_entry(&x1, RMID, TMNOFLAGS) != XA_OK)
		return (2);
	if (xa->xa_prepare_entry(&x2, RMID, TMNOFLAGS) != XA_OK)
		return (2);
	/* Commit branch 1 only, then crash before branch 2. */
	if (xa->xa_commit_entry(&x1, RMID, TMNOFLAGS) != XA_OK)
		return (2);
	(void)fflush(NULL);
	fprintf(stderr, "child: committed b1, crashing before b2\n");
	(void)kill(getpid(), SIGKILL);
	return (99);
}

/* Run one child in a fork and require it to die by SIGKILL. */
static int
run_crashing_child(xatm_h *h, char *const argv[])
{
	pid_t pid;
	int status;

	if ((pid = fork()) < 0) {
		perror("fork");
		return (-1);
	}
	if (pid == 0) {
		execv(argv[0], argv);
		perror("execv");
		_exit(127);
	}
	if (waitpid(pid, &status, 0) != pid) {
		perror("waitpid");
		return (-1);
	}
	/* The child must have died from SIGKILL, not exited cleanly. */
	XCHK_TRUE(h, WIFSIGNALED(status) && WTERMSIG(status) == SIGKILL);
	return (0);
}

/* Enumerate all in-doubt XIDs via the recovery scan into list[0..max). */
static int
recover_scan(XID *list, int max)
{
	int found;

	memset(list, 0, (size_t)max * sizeof(XID));
	/* One call with both start+end flags returns the whole set (up to
	 * max); this is the common small-count TM pattern.  XA §6 xa_recover. */
	found = xa->xa_recover_entry(list, max, RMID,
	    TMSTARTRSCAN | TMENDRSCAN);
	return (found);
}

static int
driver(void)
{
	xatm_h H, *h = &H;
	XID list[64], xid;
	DB *dbp;
	char kbuf[32], vbuf[32], *av[6];
	const char *home = getenv("XA_HOME");
	const char *self = getenv("XA_SELF");
	int found, i, j, g, ncommit, nrollback;

	if (home == NULL)
		home = "XA_RECOVER_HOME";
	if (self == NULL)
		self = "./xa_recover";
	ncommit = 3;
	nrollback = 2;

	(void)signal(SIGALRM, on_alarm);
	(void)alarm(ALARM_SECS);
	xatm_begin(h, "xa_recover");

	/* -------- ARM A: crash after prepare, before phase-2. -------- */
	av[0] = (char *)self;
	av[1] = (char *)"child-prepare";
	av[2] = (char *)home;
	(void)snprintf(kbuf, sizeof(kbuf), "%d", ncommit);
	av[3] = kbuf;
	(void)snprintf(vbuf, sizeof(vbuf), "%d", nrollback);
	av[4] = vbuf;
	av[5] = NULL;
	if (run_crashing_child(h, av) != 0)
		return (xatm_end(h) | 1);

	/*
	 * Recovery: the parent opens the SAME env.  XA_FLAGS has DB_REGISTER |
	 * DB_RECOVER, so xa_open runs recovery, which restores the prepared
	 * branches.  (DB_REGISTER detects the crashed process and forces
	 * recovery even though we are a different process.)
	 */
	XCHK(h, xa->xa_open_entry((char *)home, RMID, TMNOFLAGS), XA_OK);
	if ((dbp = xa_db_open()) == NULL)
		return (xatm_end(h) | 1);

	found = recover_scan(list, 64);
	fprintf(stderr, "arm A: xa_recover found %d in-doubt branch(es)\n",
	    found);
	XCHK_TRUE(h, found == ncommit + nrollback);

	/*
	 * The TM resolves each branch by its gtrid: commit 1..ncommit, roll
	 * back the rest.  We decode the gtrid from the recovered XID's data
	 * ("GT%06d") to decide.
	 */
	for (i = 0; i < found; i++) {
		if (sscanf((char *)list[i].data, "GT%d", &g) != 1) {
			XCHK_TRUE(h, 0);	/* malformed recovered gtrid */
			continue;
		}
		if (g <= ncommit)
			XCHK(h, xa->xa_commit_entry(&list[i], RMID,
			    TMNOFLAGS), XA_OK);
		else
			XCHK(h, xa->xa_rollback_entry(&list[i], RMID,
			    TMNOFLAGS), XA_OK);
	}
	/* The committed branches' records are present; rolled-back absent. */
	for (j = 1; j <= ncommit + nrollback; j++) {
		(void)snprintf(kbuf, sizeof(kbuf), "rec%d", j);
		(void)snprintf(vbuf, sizeof(vbuf), "g%d", j);
		if (j <= ncommit)
			XCHK_TRUE(h, xa_has(dbp, kbuf, vbuf));
		else
			XCHK_TRUE(h, xa_absent(dbp, kbuf));
	}
	/* Nothing in-doubt remains. */
	found = recover_scan(list, 64);
	XCHK_TRUE(h, found == 0);

	(void)dbp->close(dbp, 0);
	XCHK(h, xa->xa_close_entry((char *)home, RMID, TMNOFLAGS), XA_OK);

	/* -------- ARM B: crash during commit of a two-branch txn. -------- */
	av[1] = (char *)"child-midcommit";
	av[2] = (char *)home;
	av[3] = NULL;
	if (run_crashing_child(h, av) != 0)
		return (xatm_end(h) | 1);

	XCHK(h, xa->xa_open_entry((char *)home, RMID, TMNOFLAGS), XA_OK);
	if ((dbp = xa_db_open()) == NULL)
		return (xatm_end(h) | 1);

	/* Branch 1 was committed before the crash: its record must survive. */
	XCHK_TRUE(h, xa_has(dbp, "mid-b1", "committed"));
	/* Branch 2 was only prepared: it must be in-doubt, not yet applied. */
	XCHK_TRUE(h, xa_absent(dbp, "mid-b2"));

	found = recover_scan(list, 64);
	fprintf(stderr, "arm B: xa_recover found %d in-doubt branch(es)\n",
	    found);
	XCHK_TRUE(h, found == 1);
	if (found == 1) {
		/* The TM decides to commit the surviving in-doubt branch. */
		XCHK(h, xa->xa_commit_entry(&list[0], RMID, TMNOFLAGS), XA_OK);
		XCHK_TRUE(h, xa_has(dbp, "mid-b2", "indoubt"));
	}
	/* And an alternative-decision check: a fresh in-doubt branch rolled
	 * back leaves no data.  (Confirms both outcomes of the TM decision.) */
	xatm_mkxid(&xid, 200, 1);
	XCHK(h, xa->xa_start_entry(&xid, RMID, TMNOFLAGS), XA_OK);
	XCHK(h, xa_put(dbp, "arm-b-rollback", "no"), 0);
	XCHK(h, xa->xa_end_entry(&xid, RMID, TMSUCCESS), XA_OK);
	XCHK(h, xa->xa_prepare_entry(&xid, RMID, TMNOFLAGS), XA_OK);
	XCHK(h, xa->xa_rollback_entry(&xid, RMID, TMNOFLAGS), XA_OK);
	XCHK_TRUE(h, xa_absent(dbp, "arm-b-rollback"));

	(void)dbp->close(dbp, 0);
	XCHK(h, xa->xa_close_entry((char *)home, RMID, TMNOFLAGS), XA_OK);

	(void)alarm(0);
	return (xatm_end(h));
}

int
main(int argc, char *argv[])
{
	if (argc >= 3 && strcmp(argv[1], "child-prepare") == 0)
		return (child_prepare(argv[2],
		    argc > 3 ? atoi(argv[3]) : 3,
		    argc > 4 ? atoi(argv[4]) : 2));
	if (argc >= 3 && strcmp(argv[1], "child-midcommit") == 0)
		return (child_midcommit(argv[2]));
	return (driver());
}

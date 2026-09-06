/*-
 * test/lockmatrix/test_lock_matrix.c --
 *	Tier B3: exhaustive lock-mode matrix through the lock-list paths.
 *
 * The blind spot this closes: adding a new DB_LOCK_* mode silently breaking a
 * pre-existing loop that enumerates modes exhaustively.  Issue #140 is exactly
 * that -- DB_LOCK_SIREAD (=9, the SSI addition) is neither counted as a write
 * by nwrites nor recognised by the read tests in __lock_vec's DB_LOCK_PUT_READ
 * / DB_LOCK_UPGRADE_WRITE path, so an SIREAD lock consumes an objlist DBT slot
 * that was never allocated: a heap out-of-bounds WRITE.  The DB_ASSERT that
 * would catch it is diagnostic-only and compiled out of release builds.
 *
 * What this exercises, entirely through the public DB_ENV lock API:
 *   1. Every mode in db_lockmode_t is acquired and released (lock_get /
 *	lock_put and lock_vec DB_LOCK_GET / DB_LOCK_PUT), so a mode missing
 *	from a conflict table or a mode-name switch shows up.
 *   2. Every conflict-matrix cell: for each (held, wanted) pair, a second
 *	locker requests the mode with DB_LOCK_NOWAIT and the outcome is
 *	compared against the engine's own conflict verdict for consistency.
 *   3. The lock-LIST operations that size an objlist from nwrites --
 *	DB_LOCK_PUT_READ and DB_LOCK_UPGRADE_WRITE -- driven by a locker
 *	holding a MIX of write locks and SIREAD locks, across a sweep of
 *	(nwrite, nsiread) shapes.  This is the #140 shape: the more SIREAD
 *	locks beyond the number of write locks, the further past the end of
 *	the allocation the loop writes.
 *
 * Run it under ASan (SAN=1, or point LIBDB_BUILD at build_asan_gate) so the
 * out-of-bounds write is observed rather than silently tolerated.
 *
 * EXPECTATIONS ARE WRITTEN FOR THE FIXED ENGINE.  On current master the
 * mixed-mode PUT_READ cases are expected to fault under an ASan-instrumented
 * libdb; that is the tier reproducing #140.  Because an ASan fault aborts the
 * process, the harness prints its progress line BEFORE each risky call, so the
 * last line in the log names the exact shape that faulted.  When #140 lands,
 * the whole matrix must run to completion and exit 0.
 *
 * Usage:
 *	./test_lock_matrix			every section
 *	./test_lock_matrix modes|conflicts|list	one section
 *
 * Exit status: 0 = the matrix completed and every check held, 1 = a check
 * failed, 2 = harness error.  An ASan abort (exit 1 from the sanitizer with
 * a heap-buffer-overflow report) is the #140 reproduction.
 */
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "db.h"

/*
 * Every mode in db_lockmode_t (src/dbinc/db.in).  DB_LOCK_NG (not granted)
 * and DB_LOCK_WAIT (an event, not a lock) are not requestable modes, so they
 * are listed but skipped for acquisition -- listing them keeps the table
 * exhaustive, which is the point of the tier.
 */
static const struct {
	db_lockmode_t	mode;
	const char     *name;
	int		requestable;
	int		is_write;	/* Per IS_WRITELOCK, src/dbinc/lock.h */
} modes[] = {
	{ DB_LOCK_NG,			"NG",			0, 0 },
	{ DB_LOCK_READ,			"READ",			1, 0 },
	{ DB_LOCK_WRITE,		"WRITE",		1, 1 },
	{ DB_LOCK_WAIT,			"WAIT",			0, 0 },
	{ DB_LOCK_IWRITE,		"IWRITE",		1, 1 },
	{ DB_LOCK_IREAD,		"IREAD",		1, 0 },
	{ DB_LOCK_IWR,			"IWR",			1, 1 },
	{ DB_LOCK_READ_UNCOMMITTED,	"READ_UNCOMMITTED",	1, 0 },
	{ DB_LOCK_WWRITE,		"WWRITE",		1, 1 },
	{ DB_LOCK_SIREAD,		"SIREAD",		1, 0 },
};
#define	NMODES ((int)(sizeof(modes) / sizeof(modes[0])))

static DB_ENV	*env;
static int	 failures;

static void lm_die(const char *, int) __attribute__((noreturn));

static void
lm_die(const char *what, int rc)
{
	fprintf(stderr, "harness error: %s: %s (%d)\n",
	    what, db_strerror(rc), rc);
	exit(2);
}

static void
lm_fail(const char *fmt, ...)
{
	va_list ap;

	printf("    FAIL: ");
	va_start(ap, fmt);
	(void)vprintf(fmt, ap);
	va_end(ap);
	printf("\n");
	failures++;
}

static const char *
rc_name(int rc)
{
	if (rc == 0)
		return ("granted");
	switch (rc) {
	case DB_LOCK_NOTGRANTED:	return ("NOTGRANTED");
	case DB_LOCK_DEADLOCK:		return ("DEADLOCK");
	default:			return (db_strerror(rc));
	}
}

static void
lm_env_open(void)
{
	static const char *home = "LOCKDIR";
	char cmd[256];
	int rc;

	(void)mkdir(home, 0755);
	(void)snprintf(cmd, sizeof(cmd),
	    "find '%s' -mindepth 1 -delete 2>/dev/null", home);
	(void)system(cmd);

	if ((rc = db_env_create(&env, 0)) != 0)
		lm_die("db_env_create", rc);
	/*
	 * Room for the widest shape the list section builds, and a lock
	 * timeout so a NOWAIT-less request can never hang the tier.
	 */
	if ((rc = env->set_lk_max_locks(env, 5000)) != 0 ||
	    (rc = env->set_lk_max_lockers(env, 500)) != 0 ||
	    (rc = env->set_lk_max_objects(env, 5000)) != 0)
		lm_die("set_lk_max_*", rc);
	if ((rc = env->set_timeout(env, 1000000, DB_SET_LOCK_TIMEOUT)) != 0)
		lm_die("set_timeout", rc);
	if ((rc = env->open(env, home,
	    DB_CREATE | DB_INIT_LOCK | DB_INIT_MPOOL | DB_THREAD, 0600)) != 0)
		lm_die("DB_ENV->open", rc);
}

/*
 * lm_obj --
 *	Build a lock object.  Real page locks are DB_LOCK_ILOCKs, and
 *	__lock_fix_list treats an object of exactly sizeof(DB_LOCK_ILOCK) as a
 *	page lock to be coalesced by fileid.  Using genuine ILOCKs is what
 *	drives that coalescing code, so the tier uses them rather than opaque
 *	blobs.
 */
static void
lm_obj(DBT *dbt, DB_LOCK_ILOCK *ilock, u_int8_t fileid, db_pgno_t pgno,
    u_int32_t type)
{
	memset(ilock, 0, sizeof(*ilock));
	memset(ilock->fileid, fileid, DB_FILE_ID_LEN);
	ilock->pgno = pgno;
	ilock->type = type;
	memset(dbt, 0, sizeof(*dbt));
	dbt->data = ilock;
	dbt->size = sizeof(*ilock);
}

/*
 * ---------------------------------------------------------------------------
 * Section 1: every mode acquired and released.
 * ---------------------------------------------------------------------------
 */
static void
section_modes(void)
{
	DB_LOCK lock;
	DB_LOCKREQ req;
	DB_LOCK_ILOCK ilock;
	DBT obj;
	u_int32_t locker;
	int i, rc;

	printf("== modes: acquire and release every db_lockmode_t ==\n");
	for (i = 0; i < NMODES; i++) {
		if (!modes[i].requestable) {
			printf("    %-18s skipped (not a requestable mode)\n",
			    modes[i].name);
			continue;
		}
		if ((rc = env->lock_id(env, &locker)) != 0)
			lm_die("lock_id", rc);
		lm_obj(&obj, &ilock, 1, (db_pgno_t)i, 0);

		/* lock_get / lock_put. */
		rc = env->lock_get(env, locker, DB_LOCK_NOWAIT, &obj,
		    modes[i].mode, &lock);
		if (rc != 0)
			lm_fail("%s: lock_get on an unheld object -> %s",
			    modes[i].name, rc_name(rc));
		else if ((rc = env->lock_put(env, &lock)) != 0)
			lm_fail("%s: lock_put -> %s",
			    modes[i].name, rc_name(rc));

		/* The same through lock_vec, which is a different path. */
		memset(&req, 0, sizeof(req));
		req.op = DB_LOCK_GET;
		req.mode = modes[i].mode;
		req.obj = &obj;
		if ((rc = env->lock_vec(env, locker, DB_LOCK_NOWAIT, &req, 1,
		    NULL)) != 0)
			lm_fail("%s: lock_vec DB_LOCK_GET -> %s",
			    modes[i].name, rc_name(rc));
		else {
			memset(&req, 0, sizeof(req));
			req.op = DB_LOCK_PUT_ALL;
			if ((rc = env->lock_vec(env, locker, 0, &req, 1,
			    NULL)) != 0)
				lm_fail("%s: lock_vec DB_LOCK_PUT_ALL -> %s",
				    modes[i].name, rc_name(rc));
		}
		if (rc == 0)
			printf("    %-18s get/put and vec get/put_all OK\n",
			    modes[i].name);
		if ((rc = env->lock_id_free(env, locker)) != 0)
			lm_die("lock_id_free", rc);
	}
	printf("\n");
}

/*
 * ---------------------------------------------------------------------------
 * Section 2: the whole conflict matrix.
 *
 * For every (held, wanted) pair, locker A takes `held' and locker B requests
 * `wanted' with DB_LOCK_NOWAIT.  We do not hard-code the expected verdict:
 * the engine's conflict table is the specification, and lock_stat's st_nmodes
 * tells us how wide it is.  What we ASSERT is the two properties a table must
 * have regardless of policy:
 *
 *   - it is total: every requestable mode gets a definite granted /
 *     NOTGRANTED answer, never an internal error, and never a hang.
 *   - it is symmetric in conflict: if held H blocks wanted W, then held W
 *     blocks wanted H.  An asymmetric cell is how a hand-edited table
 *     acquires a hole when a mode is appended.
 * ---------------------------------------------------------------------------
 */
static int
conflict_probe(int held, int wanted, db_pgno_t pgno, int *rc_out)
{
	DB_LOCK hl, wl;
	DB_LOCK_ILOCK ilock;
	DBT obj;
	u_int32_t la, lb;
	int rc;

	if ((rc = env->lock_id(env, &la)) != 0 ||
	    (rc = env->lock_id(env, &lb)) != 0)
		lm_die("lock_id", rc);
	lm_obj(&obj, &ilock, 2, pgno, 0);

	if ((rc = env->lock_get(env, la, DB_LOCK_NOWAIT, &obj,
	    modes[held].mode, &hl)) != 0) {
		/* Could not establish the precondition; not a matrix result. */
		(void)env->lock_id_free(env, la);
		(void)env->lock_id_free(env, lb);
		*rc_out = rc;
		return (-1);
	}
	rc = env->lock_get(env, lb, DB_LOCK_NOWAIT, &obj,
	    modes[wanted].mode, &wl);
	*rc_out = rc;
	if (rc == 0)
		(void)env->lock_put(env, &wl);
	(void)env->lock_put(env, &hl);
	(void)env->lock_id_free(env, la);
	(void)env->lock_id_free(env, lb);
	return (rc == 0 ? 0 : 1);		/* 0 = compatible, 1 = blocks */
}

static void
section_conflicts(void)
{
	DB_LOCK_STAT *lst;
	int blocks[NMODES][NMODES];
	int h, rc, w;
	db_pgno_t pgno;

	printf("== conflicts: every (held, wanted) cell ==\n");
	if ((rc = env->lock_stat(env, &lst, 0)) != 0)
		lm_die("lock_stat", rc);
	printf("    conflict table is %d modes wide; db_lockmode_t has %d "
	    "values\n", lst->st_nmodes, NMODES);
	if (lst->st_nmodes < NMODES)
		lm_fail("the conflict table (%d modes) is NARROWER than "
		    "db_lockmode_t (%d values) -- a mode was added without "
		    "widening the table", lst->st_nmodes, NMODES);
	free(lst);

	pgno = 100;
	for (h = 0; h < NMODES; h++)
		for (w = 0; w < NMODES; w++) {
			blocks[h][w] = -1;
			if (!modes[h].requestable || !modes[w].requestable)
				continue;
			blocks[h][w] = conflict_probe(h, w, pgno++, &rc);
			if (blocks[h][w] < 0)
				lm_fail("could not hold %s to probe %s: %s",
				    modes[h].name, modes[w].name, rc_name(rc));
			else if (rc != 0 && rc != DB_LOCK_NOTGRANTED &&
			    rc != DB_LOCK_DEADLOCK)
				lm_fail("held %s, wanted %s: unexpected %s",
				    modes[h].name, modes[w].name, rc_name(rc));
		}

	/* Print the matrix; "." = compatible, "X" = blocks, "-" = n/a. */
	printf("    held \\ wanted");
	for (w = 0; w < NMODES; w++)
		printf(" %2d", (int)modes[w].mode);
	printf("\n");
	for (h = 0; h < NMODES; h++) {
		printf("    %-18s", modes[h].name);
		for (w = 0; w < NMODES; w++)
			printf(" %2s", blocks[h][w] < 0 ? "-" :
			    blocks[h][w] ? "X" : ".");
		printf("\n");
	}

	for (h = 0; h < NMODES; h++)
		for (w = h + 1; w < NMODES; w++)
			if (blocks[h][w] >= 0 && blocks[w][h] >= 0 &&
			    blocks[h][w] != blocks[w][h])
				lm_fail("conflict table is asymmetric: "
				    "held %s / wanted %s says %s, but "
				    "held %s / wanted %s says %s",
				    modes[h].name, modes[w].name,
				    blocks[h][w] ? "conflict" : "compatible",
				    modes[w].name, modes[h].name,
				    blocks[w][h] ? "conflict" : "compatible");
	printf("\n");
}

/*
 * ---------------------------------------------------------------------------
 * Section 3: the lock-LIST operations -- the #140 shape.
 *
 * DB_LOCK_PUT_READ and DB_LOCK_UPGRADE_WRITE in __lock_vec walk a locker's
 * held-lock list and, when the caller passes an obj DBT, fill it with one DBT
 * per lock the loop did NOT release.  The allocation is sized from
 * sh_locker->nwrites.  So the invariant is:
 *
 *	{ locks the release-loop skips } == { locks counted in nwrites }
 *
 * A mode that is neither recognised as a read (and so released) nor counted as
 * a write (and so allocated for) breaks it and the loop writes past the end of
 * the allocation.  DB_LOCK_SIREAD is such a mode on current master (#140).
 *
 * The sweep below builds lockers holding nwrite WRITE locks and nsiread SIREAD
 * locks and then issues PUT_READ / UPGRADE_WRITE with an objlist.  The
 * overflow is (nsiread - nwrite) DBTs when nsiread > nwrite, so the sweep goes
 * well past nsiread == nwrite.  Each shape is announced BEFORE the call, so if
 * ASan aborts, the last printed line is the shape that overflowed.
 * ---------------------------------------------------------------------------
 */
#define	LIST_MAXLOCK	32

static int
list_shape(db_lockop_t op, const char *opname, int nwrite, int nsiread,
    int nread, db_pgno_t base)
{
	DB_LOCK locks[LIST_MAXLOCK];
	DB_LOCKREQ req;
	DB_LOCK_ILOCK ilock[LIST_MAXLOCK];
	DBT objs[LIST_MAXLOCK], objlist;
	u_int32_t locker;
	int i, n, rc;

	if ((rc = env->lock_id(env, &locker)) != 0)
		lm_die("lock_id", rc);

	n = 0;
	for (i = 0; i < nwrite; i++, n++) {
		lm_obj(&objs[n], &ilock[n], 3, base + (db_pgno_t)n, 0);
		if ((rc = env->lock_get(env, locker, DB_LOCK_NOWAIT, &objs[n],
		    DB_LOCK_WRITE, &locks[n])) != 0)
			lm_die("lock_get WRITE", rc);
	}
	for (i = 0; i < nsiread; i++, n++) {
		lm_obj(&objs[n], &ilock[n], 3, base + (db_pgno_t)n, 0);
		if ((rc = env->lock_get(env, locker, DB_LOCK_NOWAIT, &objs[n],
		    DB_LOCK_SIREAD, &locks[n])) != 0)
			lm_die("lock_get SIREAD", rc);
	}
	for (i = 0; i < nread; i++, n++) {
		lm_obj(&objs[n], &ilock[n], 3, base + (db_pgno_t)n, 0);
		if ((rc = env->lock_get(env, locker, DB_LOCK_NOWAIT, &objs[n],
		    DB_LOCK_READ, &locks[n])) != 0)
			lm_die("lock_get READ", rc);
	}

	/*
	 * Announce BEFORE the call: an ASan heap-buffer-overflow aborts the
	 * process inside lock_vec, so this line is the evidence of which
	 * shape did it.
	 */
	printf("    %-14s nwrite=%2d nsiread=%2d nread=%2d ...",
	    opname, nwrite, nsiread, nread);
	fflush(stdout);

	memset(&objlist, 0, sizeof(objlist));
	memset(&req, 0, sizeof(req));
	req.op = op;
	req.obj = &objlist;
	rc = env->lock_vec(env, locker, 0, &req, 1, NULL);
	printf(" %s", rc == 0 ? "ok" : rc_name(rc));
	if (rc == 0)
		printf(", objlist %u bytes", objlist.size);
	printf("\n");
	if (rc != 0)
		lm_fail("%s with nwrite=%d nsiread=%d nread=%d -> %s",
		    opname, nwrite, nsiread, nread, rc_name(rc));
	if (objlist.data != NULL)
		free(objlist.data);

	memset(&req, 0, sizeof(req));
	req.op = DB_LOCK_PUT_ALL;
	if ((rc = env->lock_vec(env, locker, 0, &req, 1, NULL)) != 0 &&
	    rc != DB_LOCK_NOTGRANTED)
		lm_fail("PUT_ALL after %s -> %s", opname, rc_name(rc));
	if ((rc = env->lock_id_free(env, locker)) != 0)
		lm_die("lock_id_free", rc);
	return (0);
}

static void
section_list(void)
{
	db_pgno_t base;
	int nsiread, nwrite;

	printf("== list: PUT_READ / UPGRADE_WRITE with an objlist ==\n");
	printf("    invariant: the locks the release loop SKIPS must all be "
	    "counted in nwrites,\n    because nwrites sizes the objlist "
	    "allocation.  A mode that is neither\n    released as a read nor "
	    "counted as a write overflows it (#140: SIREAD).\n");

	base = 1000;
	/* Pure write shapes: the case the original code was written for. */
	for (nwrite = 0; nwrite <= 4; nwrite++) {
		list_shape(DB_LOCK_PUT_READ, "PUT_READ",
		    nwrite, 0, 0, base);
		base += LIST_MAXLOCK;
	}
	/* Writes plus plain reads: reads are released, so still balanced. */
	for (nwrite = 1; nwrite <= 3; nwrite++) {
		list_shape(DB_LOCK_PUT_READ, "PUT_READ",
		    nwrite, 0, 3, base);
		base += LIST_MAXLOCK;
	}
	/*
	 * The #140 sweep: a MIX of write locks and SIREAD locks.  The
	 * overflow is (nsiread - nwrite) DBTs, so walk nsiread past nwrite.
	 */
	for (nwrite = 0; nwrite <= 3; nwrite++)
		for (nsiread = 0; nsiread <= 6; nsiread++) {
			list_shape(DB_LOCK_PUT_READ, "PUT_READ",
			    nwrite, nsiread, 0, base);
			base += LIST_MAXLOCK;
		}
	/* Same mix through the UPGRADE_WRITE arm of the same loop. */
	for (nwrite = 0; nwrite <= 2; nwrite++)
		for (nsiread = 0; nsiread <= 4; nsiread++) {
			list_shape(DB_LOCK_UPGRADE_WRITE, "UPGRADE_WRITE",
			    nwrite, nsiread, 0, base);
			base += LIST_MAXLOCK;
		}
	/* All three kinds at once. */
	for (nsiread = 1; nsiread <= 4; nsiread++) {
		list_shape(DB_LOCK_PUT_READ, "PUT_READ",
		    2, nsiread, 2, base);
		base += LIST_MAXLOCK;
	}
	printf("\n");
}

int
main(int argc, char **argv)
{
	int rc, want_conflicts, want_list, want_modes;

	setvbuf(stdout, NULL, _IOLBF, 0);
	want_conflicts = want_list = want_modes = (argc == 1);
	if (argc > 1) {
		int i;
		for (i = 1; i < argc; i++) {
			if (strcmp(argv[i], "modes") == 0)
				want_modes = 1;
			else if (strcmp(argv[i], "conflicts") == 0)
				want_conflicts = 1;
			else if (strcmp(argv[i], "list") == 0)
				want_list = 1;
			else {
				fprintf(stderr, "usage: %s "
				    "[modes|conflicts|list]...\n", argv[0]);
				return (2);
			}
		}
	}

	printf("%s\n\n", db_version(NULL, NULL, NULL));
	lm_env_open();
	if (want_modes)
		section_modes();
	if (want_conflicts)
		section_conflicts();
	if (want_list)
		section_list();
	if ((rc = env->close(env, 0)) != 0)
		lm_die("DB_ENV->close", rc);

	printf("%d check(s) failed\n", failures);
	return (failures != 0);
}

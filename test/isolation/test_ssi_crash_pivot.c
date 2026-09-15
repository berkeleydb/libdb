/*-
 * See the file LICENSE for redistribution information.
 *
 * test/isolation/test_ssi_crash_pivot.c --
 *	SSI correctness gate: a transaction that must abort as the PIVOT of a
 *	dangerous structure cannot become durable because the crash landed
 *	inside its commit window.
 *
 * WHY THIS TEST EXISTS
 *	Issue #136 was the LIVE race in the commit window: a writer arriving
 *	while the pivot was inside DB_TXN->commit, past its own pivot check, was
 *	told "still running, already flagged, it will abort at its check" and
 *	deferred to a check that had already happened -- both transactions
 *	committed a write skew.  The fix publishes TXN_DTL_SICHECKED under
 *	TXN_SYSTEM_LOCK atomically with the check, so the arriving writer resolves
 *	the edge itself.
 *
 *	Cahill's SSI has no crash model at all: the pivot argument is about a
 *	live execution.  So the fix answers "who aborts?" but not "what if
 *	nobody is alive to abort?".  This test asks the crash question directly:
 *	if the process dies at any point inside the pivot's commit window, can
 *	the recovered database hold a state that only a COMMITTED pivot could
 *	have produced?
 *
 * THE SCHEDULE (the child drives it, then dies)
 *	Two one-page databases, A and B, both 1.  Conflict detection is at PAGE
 *	granularity, so A and B are separate databases; that keeps the schedule
 *	about SSI edges and not about page-level write-write contention.
 *
 *	  1. T1 := txn_begin(SERIALIZABLE); read A     -> SIREAD marker on A
 *	  2. T2 := txn_begin(SERIALIZABLE); read B     -> SIREAD marker on B
 *	  3. T2: write A                               -> finds T1's marker.  T1 is
 *	                                                  running with no write end
 *	                                                  yet, so edge
 *	                                                  T1 --rw--> T2 gives T2
 *	                                                  the WRITE end and T1 the
 *	                                                  read end.
 *	  4. T1: write B                               -> finds T2's marker.  T2 is
 *	                                                  running and already holds
 *	                                                  a write end, so the engine
 *	                                                  defers to T2's own commit
 *	                                                  check: T2 gains the READ
 *	                                                  end, T1 gains nothing.
 *	  ***  T2 NOW HOLDS BOTH ENDS: T2 IS THE PIVOT, and the only thing that
 *	  ***  will stop it is its COMMIT-TIME pivot check -- which is exactly the
 *	  ***  window this test crashes inside.
 *	  5. T1 commits (it holds only the read end, so it is legal).  B := 0 is
 *	     now DURABLE (DB_TXN_SYNC).
 *	  6. T2 commits -- must be refused with DB_SNAPSHOT_CONFLICT -- and the
 *	     child is KILLED inside that call, at the point named by SSI_CRASH_AT.
 *
 *	T1 = "if A then B:=0", T2 = "if B then A:=0".  From (1,1) the two serial
 *	orders give (A,B) = (1,0) or (0,1).  Step 5 already made B=0 durable, so
 *	the recovered state is (1,0) -- serializable -- UNLESS T2's write also
 *	took effect, which gives (0,0).  (0,0) requires both transactions to have
 *	committed: it is exactly "the pivot became durable".  That is the one bit
 *	this test asserts, and putting T1's commit BEFORE the crash is what gives
 *	it teeth: the other transaction's write is already on disk, so a
 *	surviving pivot is immediately visible in the recovered state.
 *
 *	The schedule is single-threaded and deterministic -- no barriers, no race.
 *	T2 arriving at its commit-time check as a pivot follows from the flag
 *	rules in __lock_get_internal (a writer defers to a reader that still has a
 *	pivot check left), not from timing.
 *
 * THE KILL POINTS (SSI_CRASH_AT)
 *	1  Pivot decided under TXN_SYSTEM_LOCK, TXN_DTL_SICHECKED not yet
 *	   published.  The narrowest window: the #136 fix's critical section,
 *	   entered but not completed.
 *	2  Check done and TXN_DTL_SICHECKED published, before the
 *	   DB_SNAPSHOT_CONFLICT return.  This is the window where the flag says
 *	   "past the check" but no decision has been acted on.
 *	3  Read locks released, commit record NOT yet written.  Nothing about the
 *	   transaction is durable, so recovery must undo it.
 *	4  Commit record written AND (DB_TXN_SYNC) flushed.  A transaction that
 *	   reaches here IS durable and recovery WILL redo it -- so a PIVOT
 *	   reaching point 4 at all would be the bug.
 *	0  Control: no crash at all.  T2 must be refused, the child exits
 *	   cleanly, and the state must still be serializable.  This is the
 *	   anti-vacuity check -- it fails if the schedule stopped producing a
 *	   pivot.
 *
 *	OBSERVED, and now asserted: the pivot NEVER reaches point 3 or 4.  Its
 *	commit check returns DB_SNAPSHOT_CONFLICT, which is UPSTREAM of the log
 *	write, so no commit record for a pivot is ever produced.  The durability
 *	question is therefore answered structurally, not just empirically.
 *
 *	But "the hook never fired" is also exactly what a BROKEN hook looks like.
 *	So the sweep also arms points 3 and 4 on the OTHER, legal, NON-pivot
 *	transaction (SSI_PIVOT_VICTIM=t1).  Those runs DO die there, which proves
 *	(a) the hook reaches those points and (b) the pivot's absence from them is
 *	an engine property, not a test artefact.  Those runs also crash with T1's
 *	commit record half-written, so recovery has real work to do.
 *
 *	Points 1-4 are unreachable from userspace (all inside one library call),
 *	so they are reached by a DIAGNOSTIC-ONLY hook in src/txn/txn.c
 *	(__txn_ssi_crash / SSI_CRASH_POINT).  It compiles to nothing without
 *	--enable-diagnostic and is inert unless SSI_CRASH_AT is set AND the
 *	transaction is named "ssi-pivot" via DB_TXN->set_name.  See the comment
 *	on the hook.
 *
 * WHAT THE PARENT ASSERTS
 *	After each child, the parent reopens the environment with DB_RECOVER and
 *	reads A and B back.  It requires:
 *	  - (A,B) != (0,0)                 -- no pivot survived
 *	  - (A,B) in {(1,1),(1,0),(0,1)}   -- some serial order explains it
 *	  - db->verify clean                -- recovery produced a sane tree
 *	and additionally, that the schedule really did make T2 a pivot in at
 *	least the control run (else the sweep of kill points proves nothing).
 *
 *	Every (kill point, seed) pair gets a FRESH environment: a crash test
 *	that reuses a recovered environment tests recovery-of-recovery, not the
 *	commit window.
 *
 * Usage:
 *	./test_ssi_crash_pivot [nseeds]
 * Env:
 *	SSI_PIVOT_SEEDS		seeds per kill point (default 3)
 *	SSI_PIVOT_POINTS	comma list of kill points (default 0,1,2,3,4)
 *	SSI_PIVOT_VERBOSE=1	child chatter
 *
 * The sweep runs every kill point twice: once with the PIVOT armed (the
 * assertion) and once with the legal non-pivot transaction armed (the proof
 * that the hook reaches the later points at all).
 *
 * Exit: 0 = serializable at every kill point, 1 = a pivot survived (or the
 * recovered state was unexplainable), 2 = harness error.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"
#include "iso_knobs.h"

#define	HOME	"TESTDIR_ssi_crash_pivot"

static int verbose;

/*
 * Which transaction carries the name the crash hook looks for.  VICTIM_PIVOT
 * arms T2 (the pivot: the assertion).  VICTIM_LEGAL arms T1 (the legal
 * non-pivot txn: proves the hook reaches points 3 and 4, which a pivot
 * correctly never does).
 */
#define	VICTIM_PIVOT	0
#define	VICTIM_LEGAL	1
static int victim = VICTIM_PIVOT;

static void
die(const char *what, int ret)
{
	fprintf(stderr, "harness error: %s: %s (%d)\n",
	    what, db_strerror(ret), ret);
	exit(2);
}

static const char *
rc_name(int rc)
{
	if (rc == 0)
		return ("success");
	switch (rc) {
	case DB_SNAPSHOT_CONFLICT:	return ("DB_SNAPSHOT_CONFLICT");
	case DB_SNAPSHOT_UNSAFE:	return ("DB_SNAPSHOT_UNSAFE");
	case DB_LOCK_DEADLOCK:		return ("DB_LOCK_DEADLOCK");
	case DB_LOCK_NOTGRANTED:	return ("DB_LOCK_NOTGRANTED");
	default:			return (db_strerror(rc));
	}
}

static int
is_abort_rc(int rc)
{
	return (rc == DB_LOCK_DEADLOCK || rc == DB_LOCK_NOTGRANTED ||
	    rc == DB_SNAPSHOT_CONFLICT || rc == DB_SNAPSHOT_UNSAFE);
}

static int
get_int(DB *db, DB_TXN *txn, const char *key, int *out)
{
	DBT k, d;
	int val, rc;

	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = (void *)key;
	k.size = (u_int32_t)strlen(key);
	d.data = &val;
	d.ulen = sizeof(val);
	d.flags = DB_DBT_USERMEM;
	if ((rc = db->get(db, txn, &k, &d, 0)) != 0)
		return (rc);
	*out = val;
	return (0);
}

static int
put_int(DB *db, DB_TXN *txn, const char *key, int val)
{
	DBT k, d;

	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = (void *)key;
	k.size = (u_int32_t)strlen(key);
	d.data = &val;
	d.size = sizeof(val);
	return (db->put(db, txn, &k, &d, 0));
}

static void
rmtree(const char *dir)
{
	char cmd[600];

	(void)snprintf(cmd, sizeof(cmd),
	    "find '%s' -mindepth 1 -delete 2>/dev/null", dir);
	(void)system(cmd);
}

/*
 * env_open --
 *	Open the environment.  `extra' carries DB_CREATE or DB_RECOVER.
 *	DB_TXN_SYNC (the default) matters: kill point 4 is defined as "commit
 *	record written and flushed", so the log must really be flushed on
 *	commit for that point to mean what it says.
 */
static void
env_open(DB_ENV **envp, DB **ap, DB **bp, u_int32_t extra)
{
	DB_ENV *env;
	int rc;

	if ((rc = db_env_create(&env, 0)) != 0)
		die("db_env_create", rc);
	/* See iso_knobs.h: at 1 partition the lock and txn latches are one. */
	if ((rc = iso_set_partitions(env)) != 0)
		die("set_lk_partitions", rc);
	if ((rc = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		die("set_lk_detect", rc);
	if ((rc = env->set_timeout(env, 2000000, DB_SET_LOCK_TIMEOUT)) != 0)
		die("set_timeout", rc);
	if ((rc = env->set_cachesize(env, 0, 4 * 1024 * 1024, 1)) != 0)
		die("set_cachesize", rc);
	if ((rc = env->open(env, HOME, extra | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0600)) != 0)
		die("DB_ENV->open", rc);
	*envp = env;

	if (ap != NULL) {
		u_int32_t oflags = DB_MULTIVERSION | DB_AUTO_COMMIT |
		    DB_THREAD | DB_CREATE;
		if ((rc = db_create(ap, env, 0)) != 0)
			die("db_create A", rc);
		if ((rc = (*ap)->open(*ap, NULL, "alice.db", NULL, DB_BTREE,
		    oflags, 0600)) != 0)
			die("DB->open A", rc);
		if ((rc = db_create(bp, env, 0)) != 0)
			die("db_create B", rc);
		if ((rc = (*bp)->open(*bp, NULL, "bob.db", NULL, DB_BTREE,
		    oflags, 0600)) != 0)
			die("DB->open B", rc);
	}
}

/*
 * child_run --
 *	Drive the schedule and (if a kill point is armed) die inside T2's
 *	commit.  Runs in the forked child; never returns on a successful kill.
 *	Exit codes: 10 = T2 was refused (the correct live outcome),
 *	11 = T2 COMMITTED (which under SSI is already a bug the parent reports),
 *	12 = the schedule degenerated (T1 did not commit, so no pivot).
 */
static int
child_run(int point, unsigned int seed)
{
	DB_ENV *env;
	DB *dba, *dbb;
	DB_TXN *t1, *t2;
	int a, b, rc, t1_rc, t2_rc;

	(void)seed;
	env_open(&env, &dba, &dbb, DB_CREATE);

	/* Both records start at 1, outside any snapshot transaction. */
	if ((rc = put_int(dba, NULL, "on_call", 1)) != 0 ||
	    (rc = put_int(dbb, NULL, "on_call", 1)) != 0)
		die("child initial put", rc);
	if ((rc = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0)
		die("child checkpoint", rc);

	/* 1. T1 reads A. */
	if ((rc = env->txn_begin(env, NULL, &t1, DB_TXN_SERIALIZABLE)) != 0)
		die("T1 txn_begin", rc);
	if (victim == VICTIM_LEGAL &&
	    (rc = t1->set_name(t1, "ssi-pivot")) != 0)
		die("T1 set_name", rc);
	if ((rc = get_int(dba, t1, "on_call", &a)) != 0)
		die("T1 read A", rc);

	/* 2. T2 reads B.  Named (by default) so the crash hook finds it. */
	if ((rc = env->txn_begin(env, NULL, &t2, DB_TXN_SERIALIZABLE)) != 0)
		die("T2 txn_begin", rc);
	if (victim == VICTIM_PIVOT &&
	    (rc = t2->set_name(t2, "ssi-pivot")) != 0)
		die("T2 set_name", rc);
	if ((rc = get_int(dbb, t2, "on_call", &b)) != 0)
		die("T2 read B", rc);

	/* 3. T2 writes A: edge T1 --rw--> T2 gives T2 the WRITE end. */
	t2_rc = b ? put_int(dba, t2, "on_call", 0) : 0;
	if (t2_rc != 0 && !is_abort_rc(t2_rc))
		die("T2 write A", t2_rc);
	if (verbose)
		fprintf(stderr, "[child] T2 write A -> %s\n", rc_name(t2_rc));

	/*
	 * 4. T1 writes B.  T2 is running and already holds a write end, so the
	 * engine defers to T2's own commit check: T2 gains the READ end and is
	 * now the pivot; T1 gains nothing.
	 */
	t1_rc = a ? put_int(dbb, t1, "on_call", 0) : 0;
	if (t1_rc != 0 && !is_abort_rc(t1_rc))
		die("T1 write B", t1_rc);
	if (verbose)
		fprintf(stderr, "[child] T1 write B -> %s\n", rc_name(t1_rc));

	/*
	 * 5. T1 commits: it holds only the read end, so this is legal.  B := 0
	 * becomes DURABLE, which is what makes a surviving pivot show up as
	 * (0,0) in the recovered state.
	 */
	if (t1_rc != 0) {
		if ((rc = t1->abort(t1)) != 0)
			die("T1 abort", rc);
	} else if ((t1_rc = t1->commit(t1, DB_TXN_SYNC)) != 0 &&
	    !is_abort_rc(t1_rc))
		die("T1 commit", t1_rc);
	if (verbose)
		fprintf(stderr, "[child] T1 end -> %s\n", rc_name(t1_rc));

	/*
	 * 6. T2 ends.  If point != 0 the crash hook kills us INSIDE this call.
	 * Reaching the other side of it means the kill point was not reached
	 * (e.g. T2 was already refused at step 4, so commit is never entered
	 * with the pivot flags set) -- the parent still checks the recovered
	 * state, and the control (point 0) is what proves the pivot exists.
	 */
	if (t2_rc != 0) {
		if ((rc = t2->abort(t2)) != 0)
			die("T2 abort", rc);
	} else if ((t2_rc = t2->commit(t2, DB_TXN_SYNC)) != 0 &&
	    !is_abort_rc(t2_rc))
		die("T2 commit", t2_rc);
	if (verbose)
		fprintf(stderr, "[child] T2 end -> %s (kill point %d NOT"
		    " reached)\n", rc_name(t2_rc), point);

	/*
	 * Close cleanly: no crash happened, so this run is the live-behaviour
	 * control and the parent's recovery must find the same answer.
	 */
	if ((rc = dbb->close(dbb, 0)) != 0 || (rc = dba->close(dba, 0)) != 0)
		die("child DB->close", rc);
	if ((rc = env->close(env, 0)) != 0)
		die("child DB_ENV->close", rc);

	if (t1_rc != 0)
		return (12);
	return (t2_rc == 0 ? 11 : 10);
}

/*
 * verify_db --
 *	DB->verify needs a FRESH, never-opened handle (and it closes the handle
 *	itself), so this cannot reuse the read-back handles.
 */
static void
verify_db(DB_ENV *env, const char *name)
{
	DB *db;
	int rc;

	if ((rc = db_create(&db, env, 0)) != 0)
		die("db_create for verify", rc);
	if ((rc = db->verify(db, name, NULL, NULL, DB_NOORDERCHK)) != 0)
		die("DB->verify after recovery", rc);
}

/*
 * parent_verify --
 *	Recover and check serializability of the recovered state.  Returns 0 if
 *	serializable, 1 if not.  *state gets a 2-digit code 10*A + B for the
 *	report.
 */
static int
parent_verify(int *state)
{
	DB_ENV *env;
	DB *dba, *dbb;
	int a, b, rc, bad;

	env_open(&env, &dba, &dbb, DB_RECOVER | DB_CREATE);
	if ((rc = get_int(dba, NULL, "on_call", &a)) != 0 ||
	    (rc = get_int(dbb, NULL, "on_call", &b)) != 0)
		die("read back after recovery", rc);
	*state = 10 * a + b;

	/* Recovery must also leave a verifiable tree. */
	if ((rc = dbb->close(dbb, 0)) != 0 || (rc = dba->close(dba, 0)) != 0)
		die("parent DB->close", rc);
	verify_db(env, "alice.db");
	verify_db(env, "bob.db");
	if ((rc = env->close(env, 0)) != 0)
		die("parent DB_ENV->close", rc);

	/*
	 * From (1,1): serial T1;T2 gives (0,1), serial T2;T1 gives (1,0),
	 * neither running gives (1,1).  Step 5 made B=0 durable before the
	 * crash, so the expected recovered state is (1,0).  (0,0) needs BOTH
	 * transactions to have taken effect -- only a COMMITTED pivot does that.
	 */
	bad = (a == 0 && b == 0);
	return (bad);
}

int
main(int argc, char *argv[])
{
	const char *pointspec;
	char pbuf[64], *tok;
	int points[8], npoints;
	int i, p, v, s_i, nseeds, rc, state, status, fail;
	int sawpivot, ncrashed, nlegal_crashed;
	pid_t pid;

	nseeds = 3;
	npoints = 0;
	sawpivot = ncrashed = nlegal_crashed = fail = 0;

	if (getenv("SSI_PIVOT_SEEDS") != NULL)
		nseeds = atoi(getenv("SSI_PIVOT_SEEDS"));
	if (getenv("SSI_PIVOT_VERBOSE") != NULL)
		verbose = 1;
	if (argc > 1)
		nseeds = atoi(argv[1]);
	if (nseeds <= 0)
		return (2);

	pointspec = getenv("SSI_PIVOT_POINTS");
	if (pointspec == NULL)
		pointspec = "0,1,2,3,4";
	(void)snprintf(pbuf, sizeof(pbuf), "%s", pointspec);
	for (tok = strtok(pbuf, ","); tok != NULL && npoints < 8;
	    tok = strtok(NULL, ","))
		points[npoints++] = atoi(tok);
	if (npoints == 0)
		return (2);

	printf("=== SSI commit-window durability: a pivot must not survive a"
	    " crash\n");
	printf("    kill points=%s seeds/point=%d\n", pointspec, nseeds);
	/*
	 * A hang must fail with a name.  alarm() is cleared across fork(), so
	 * this covers only the parent's sweep loop -- the children are killed
	 * deliberately and reaped with waitpid.  See iso_knobs.h.
	 */
	iso_watchdog("test_ssi_crash_pivot", 600);

	for (i = 0; i < npoints; i++) {
	    p = points[i];
	    for (v = 0; v < 2; v++) {
		/*
		 * v == VICTIM_PIVOT: arm the pivot -- THE assertion.
		 * v == VICTIM_LEGAL: arm the legal non-pivot txn -- proves the
		 * hook reaches the later points at all (a pivot correctly never
		 * does, and "never fired" must be distinguishable from "hook
		 * broken").  Point 0 injects no crash, so only run it once.
		 */
		if (p == 0 && v != VICTIM_PIVOT)
			continue;
		victim = v;
		for (s_i = 0; s_i < nseeds; s_i++) {
			/* Fresh environment per (point, victim, seed). */
			(void)mkdir(HOME, 0755);
			rmtree(HOME);

			if ((pid = fork()) < 0) {
				perror("fork");
				return (2);
			}
			if (pid == 0) {
				char val[16];

				if (p != 0) {
					(void)snprintf(val, sizeof(val),
					    "%d", p);
					(void)setenv("SSI_CRASH_AT", val, 1);
				} else
					(void)unsetenv("SSI_CRASH_AT");
				_exit(child_run(p, (unsigned int)s_i));
			}
			if (waitpid(pid, &status, 0) != pid) {
				perror("waitpid");
				return (2);
			}

			printf("    point %d armed=%s seed %d: ", p,
			    v == VICTIM_PIVOT ? "PIVOT(T2)" : "legal(T1)",
			    s_i);
			if (WIFSIGNALED(status)) {
				if (v == VICTIM_PIVOT)
					ncrashed++;
				else
					nlegal_crashed++;
				printf("child killed by signal %d (crash"
				    " reached)", WTERMSIG(status));
			} else if (WIFEXITED(status)) {
				switch (WEXITSTATUS(status)) {
				case 10:
					if (v == VICTIM_PIVOT)
						sawpivot++;
					printf("T2 refused, no crash%s",
					    p == 0 ? "" :
					    " (point not reached)");
					break;
				case 11:
					printf("T2 COMMITTED as a pivot");
					if (v == VICTIM_PIVOT)
						fail = 1;
					break;
				case 12:
					printf("schedule degenerated (T1 did"
					    " not commit)");
					break;
				default:
					printf("child exit %d",
					    WEXITSTATUS(status));
					break;
				}
			} else
				printf("child status 0x%x", status);

			rc = parent_verify(&state);
			printf("  -> recovered (A,B)=(%d,%d)%s\n",
			    state / 10, state % 10, rc ? "  *** SKEWED" : "");
			if (rc) {
				printf("FAIL: recovered state (0,0) at kill"
				    " point %d (armed=%s) seed %d -- that"
				    " state requires BOTH transactions to have"
				    " taken effect, i.e. the PIVOT became"
				    " durable\n", p,
				    v == VICTIM_PIVOT ? "pivot" : "legal",
				    s_i);
				fail = 1;
			}
		}
	    }
	}

	if (fail)
		return (1);
	/*
	 * Anti-vacuity: at least one run must have shown the engine refusing
	 * T2 as a pivot.  Without that, every "serializable" verdict above
	 * could be a schedule that simply never built a dangerous structure.
	 */
	if (sawpivot == 0) {
		printf("FAIL: no run refused T2 -- the schedule never made a"
		    " pivot, so the sweep proves nothing\n");
		return (1);
	}
	/*
	 * Anti-vacuity #2: the hook must have reached the LATE points at all.
	 * The pivot never gets there (it returns DB_SNAPSHOT_CONFLICT upstream
	 * of the log write -- the correct answer), so if the legal transaction
	 * never crashed either, the hook is broken and every clean verdict
	 * above is meaningless.
	 */
	if (nlegal_crashed == 0 && npoints > 1) {
		printf("FAIL: no crash was reached with the legal transaction"
		    " armed -- the crash hook is not firing, so the sweep"
		    " proves nothing about the later commit-window points\n");
		return (1);
	}
	printf("PASS: serializable after recovery at every kill point"
	    " (%d crashes inside the PIVOT's commit, %d inside the legal"
	    " txn's commit, %d runs saw the engine refuse the pivot,"
	    " 0 recovered states required a committed pivot)\n",
	    ncrashed, nlegal_crashed, sawpivot);
	return (0);
}

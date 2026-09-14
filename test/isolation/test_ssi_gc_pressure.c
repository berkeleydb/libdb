/*-
 * See the file LICENSE for redistribution information.
 *
 * test/isolation/test_ssi_gc_pressure.c --
 *	SSI correctness gate: garbage collection of SIREAD markers must never
 *	drop a marker while a still-active transaction could still form the
 *	other end of the rw-antidependency it records.
 *
 * WHY THIS TEST EXISTS
 *	The existing SIREAD-marker tests cover the RESOURCE side of marker GC
 *	(test/c/leak_si_locker.c: the committed-reader footprint must stay a
 *	bounded sawtooth) and the LIFETIME side (no use-after-free).  Neither
 *	can see the CORRECTNESS failure of the same machinery: if __lock_sicleanup
 *	frees a marker too EARLY, the edge it represented is simply not there any
 *	more, the conflict is MISSED, and a non-serializable schedule commits
 *	SILENTLY -- no crash, no corrupt page, no sanitizer report, no ENOMEM.
 *	That is the worst possible failure mode for a serializable engine and it
 *	had no regression gate.
 *
 * THE SCHEDULE (single-threaded, deterministic -- no barriers, no races)
 *	Two one-page databases, A and B, both starting at 1.  Conflict detection
 *	in this engine is at PAGE granularity, so A and B live in separate
 *	databases; that keeps the schedule about SIREAD markers and not about
 *	page-level write-write contention.
 *
 *	  1. T2 := txn_begin(SERIALIZABLE);  read B      -> SIREAD marker on B
 *	  2. T1 := txn_begin(SERIALIZABLE);  read A      -> SIREAD marker on A
 *	  3. T1: write B                                 -> edge T2 --rw--> T1
 *	                                                    (T1 gets WCONF,
 *	                                                     T2 gets RCONF)
 *	  4. T1: commit                                  -> NOT a pivot (WCONF
 *	                                                    only), so it COMMITS.
 *	     T1's marker on A now belongs to a COMMITTED reader whose
 *	     serialization point (COMMITLSN) is NEWER than T2's read_lsn, so the
 *	     marker MUST survive: it is the only remaining record of the edge
 *	     T1 --rw--> (whoever writes A next).
 *	  5. *** GC PRESSURE ***  (see below)
 *	  6. T2: write A                                 -> MUST find T1's marker
 *	                                                    and abort T2 with
 *	                                                    DB_SNAPSHOT_UNSAFE
 *	  7. If step 6 was allowed, T2 commits.
 *
 *	A=1,B=1 initially.  T1 = "if A then B:=0", T2 = "if B then A:=0".  The
 *	only two serial orders give (A,B) = (1,0) or (0,1).  (0,0) is the write
 *	skew: reachable only if both committed, i.e. only if the conflict was
 *	MISSED.  So the assertion is one bit and it is exact:
 *
 *		after every iteration, (A,B) != (0,0).
 *
 *	Note that ONLY the lock-table mechanism (SIREAD markers) can see this
 *	edge: nobody writes A before step 6, so A's page has no newer MVCC
 *	version for __memp_si_rwconflict to find, and B had no newer version at
 *	the time T2 read it.  The test therefore isolates marker GC -- which is
 *	exactly what gives it teeth (neuter the GC safety predicate and it fails;
 *	see the TEETH note below).
 *
 * THE GC PRESSURE (step 5, and interleaved throughout)
 *	Three independent triggers, all of them the real ones:
 *	  (a) THE txn_begin PRESSURE SWEEP.  Each iteration runs batches of short
 *	      read-only SERIALIZABLE transactions over a wide filler keyspace.
 *	      Every one leaves a committed-reader marker behind, so the live
 *	      marker count crosses st_objects / SI_CLEANUP_TRIGGER_DIV and the
 *	      sweep fires -- repeatedly, and inside the window between step 4 and
 *	      step 6.  The batch size (SSI_GC_FILLER) is chosen so the live count
 *	      demonstrably passes that threshold: the run prints the threshold
 *	      and the observed peak, and FAILS if the peak never reached it, so
 *	      the test cannot silently stop applying pressure.
 *	  (b) EXPLICIT CHECKPOINTS.  __txn_checkpoint calls __lock_sicleanup
 *	      directly.  We force one (or several) between the reads and the
 *	      writes, i.e. squarely inside the window where T1's marker is the
 *	      only thing standing between the schedule and a write skew.
 *	  (c) A LONG-LIVED CONCURRENT SNAPSHOT READER.  A read-only
 *	      DB_TXN_SNAPSHOT transaction stays open across the whole window,
 *	      holding the oldest-reader frontier back, so the sweep runs with a
 *	      non-trivial old_lsn instead of the degenerate "everything is
 *	      obsolete" case.
 *
 *	The iteration count and the pressure knobs are varied per iteration from
 *	a simple seeded schedule, so a long run covers many orderings of sweep
 *	versus schedule step rather than one fixed phase.
 *
 * TEETH (how we know this test is not vacuous)
 *	Two ways, both exercised:
 *	  1. Run it at ISO_LEVEL=snapshot.  Plain snapshot isolation takes no
 *	     SIREAD markers at all, so the skew is legal and MUST appear.  The
 *	     test then asserts the OPPOSITE: at least one iteration commits the
 *	     skew.  A harness that had stopped driving the engine would fail
 *	     here, so a pass at ISO_LEVEL=serializable cannot be vacuous.
 *	  2. Neuter the GC safety predicate in the engine (make
 *	     __lock_siclean_obj reap a committed reader's marker regardless of
 *	     the oldest-reader comparison, and/or make __lock_sireap_lockers
 *	     ignore its si_ref guard) and this test fails at ISO_LEVEL=
 *	     serializable with a committed (0,0).
 *
 * Usage:
 *	./test_ssi_gc_pressure [iterations]
 * Env:
 *	ISO_LEVEL=serializable (default) | snapshot
 *	SSI_GC_ITER		iterations (default 120; argv overrides)
 *	SSI_GC_FILLER		filler read txns per iteration (default 220)
 *	SSI_GC_VERBOSE=1	per-iteration trace
 *
 * Exit: 0 = expectation met, 1 = expectation violated, 2 = harness error.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	HOME		"TESTDIR_ssi_gc_pressure"
#define	NFILLER_KEYS	512		/* width of the filler keyspace */
#define	GC_TRIGGER_DIV	8u		/* mirrors SI_CLEANUP_TRIGGER_DIV */

static DB_ENV	*env;
static DB	*db_a, *db_b, *db_f;
static int	 verbose;
static u_int32_t iso_level = DB_TXN_SERIALIZABLE;
static u_int32_t nobjects = 200;	/*
					 * Requested lock-object table size.
					 * __lock_region_size floors this at
					 * lk_partitions * 5, so the effective
					 * st_objects (and hence the sweep
					 * threshold) is usually larger; the run
					 * prints both and checks the threshold
					 * was actually crossed.
					 */
static u_int32_t peak_locks;		/* high-water live lock/marker count */

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

/* An rc that legitimately means "this transaction had to give up". */
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

static void
open_one(DB **dbpp, const char *name)
{
	int rc;

	if ((rc = db_create(dbpp, env, 0)) != 0)
		die("db_create", rc);
	if ((rc = (*dbpp)->open(*dbpp, NULL, name, NULL, DB_BTREE,
	    DB_CREATE | DB_MULTIVERSION | DB_AUTO_COMMIT | DB_THREAD,
	    0600)) != 0)
		die("DB->open", rc);
}

static void
env_open(void)
{
	int rc;

	if ((rc = db_env_create(&env, 0)) != 0)
		die("db_env_create", rc);
	/*
	 * Small lock-object table (GC pressure (a) above): the txn_begin sweep
	 * fires at st_objects / SI_CLEANUP_TRIGGER_DIV live markers, so a small
	 * table means a modest filler batch crosses the threshold repeatedly
	 * inside the decisive window.  The table still grows on demand
	 * (__lock_allocobj), so this cannot starve the run.
	 */
	if ((rc = env->set_memory_init(env, DB_MEM_LOCKOBJECT, nobjects)) != 0)
		die("set_memory_init(DB_MEM_LOCKOBJECT)", rc);
	if ((rc = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		die("set_lk_detect", rc);
	if ((rc = env->set_timeout(env, 2000000, DB_SET_LOCK_TIMEOUT)) != 0)
		die("set_timeout", rc);
	if ((rc = env->set_cachesize(env, 0, 8 * 1024 * 1024, 1)) != 0)
		die("set_cachesize", rc);
	if ((rc = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0600)) != 0)
		die("DB_ENV->open", rc);
	open_one(&db_a, "alice.db");
	open_one(&db_b, "bob.db");
	open_one(&db_f, "filler.db");
}

static void
env_close(void)
{
	int rc;

	if ((rc = db_f->close(db_f, 0)) != 0)
		die("DB->close filler", rc);
	if ((rc = db_b->close(db_b, 0)) != 0)
		die("DB->close bob", rc);
	if ((rc = db_a->close(db_a, 0)) != 0)
		die("DB->close alice", rc);
	if ((rc = env->close(env, 0)) != 0)
		die("DB_ENV->close", rc);
	env = NULL;
}

/*
 * filler_reads --
 *	GC pressure (a): n short read-only SERIALIZABLE transactions, each
 *	touching a different filler key.  Every one of them leaves a
 *	committed-reader SIREAD marker behind, so the live-marker count keeps
 *	crossing the txn_begin pressure threshold (st_objects /
 *	SI_CLEANUP_TRIGGER_DIV) and the sweep keeps running.
 */
static void
filler_reads(int n, unsigned int *cursor)
{
	DB_TXN *txn;
	char key[32];
	int i, rc, v;

	for (i = 0; i < n; i++) {
		(void)snprintf(key, sizeof(key), "f%06u",
		    (*cursor)++ % NFILLER_KEYS);
		if ((rc = env->txn_begin(env, NULL, &txn, iso_level)) != 0)
			die("filler txn_begin", rc);
		if ((rc = get_int(db_f, txn, key, &v)) != 0 &&
		    rc != DB_NOTFOUND) {
			(void)txn->abort(txn);
			if (is_abort_rc(rc))
				continue;
			die("filler read", rc);
		}
		if ((rc = txn->commit(txn, 0)) != 0 && !is_abort_rc(rc))
			die("filler commit", rc);
	}
}

/*
 * Track the high-water live lock count.  A committed reader's SIREAD marker IS
 * a live lock, so this is the marker population plus a small working set: the
 * observable that shows the run really drove the sweep threshold (peak near
 * st_objects / GC_TRIGGER_DIV) instead of never reaching it.
 */
static void
sample_locks(void)
{
	DB_LOCK_STAT *ls;
	int rc;

	if ((rc = env->lock_stat(env, &ls, 0)) != 0)
		die("lock_stat", rc);
	if (ls->st_nlocks > peak_locks)
		peak_locks = ls->st_nlocks;
	free(ls);
}

/*
 * The one bit that matters.  Returns 1 if this iteration committed the write
 * skew (A == 0 && B == 0), which is a serializability violation under SSI and
 * the expected outcome under plain snapshot isolation.
 */
static int
run_iteration(int iter, unsigned int *cursor, int nfiller, int *t1_ok,
    int *t2_ok)
{
	DB_TXN *t1, *t2, *holder;
	int a, b, rc, t1_rc, t2_rc;

	/* Reset the two records outside any snapshot transaction. */
	if ((rc = put_int(db_a, NULL, "on_call", 1)) != 0 ||
	    (rc = put_int(db_b, NULL, "on_call", 1)) != 0)
		die("reset", rc);

	/*
	 * GC pressure (c): a long-lived concurrent read-only snapshot reader,
	 * open across the entire window below, holding the oldest-reader
	 * frontier back so the sweep has to make a real decision about T1's
	 * marker rather than the degenerate "nothing is visible any more" one.
	 */
	if ((rc = env->txn_begin(env, NULL, &holder, DB_TXN_SNAPSHOT)) != 0)
		die("holder txn_begin", rc);
	if ((rc = get_int(db_f, holder, "f000000", &a)) != 0 &&
	    rc != DB_NOTFOUND)
		die("holder read", rc);

	/* Some pressure before the schedule starts. */
	filler_reads(nfiller / 2, cursor);
	sample_locks();

	/* 1. T2 reads B. */
	if ((rc = env->txn_begin(env, NULL, &t2, iso_level)) != 0)
		die("T2 txn_begin", rc);
	if ((rc = get_int(db_b, t2, "on_call", &b)) != 0)
		die("T2 read B", rc);

	/* 2. T1 reads A. */
	if ((rc = env->txn_begin(env, NULL, &t1, iso_level)) != 0)
		die("T1 txn_begin", rc);
	if ((rc = get_int(db_a, t1, "on_call", &a)) != 0)
		die("T1 read A", rc);

	/*
	 * GC pressure (b): a forced checkpoint BETWEEN the reads and the
	 * writes.  __txn_checkpoint calls __lock_sicleanup directly, so the
	 * sweep sees both live readers' markers here.
	 */
	if ((rc = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0)
		die("checkpoint (pre-write)", rc);

	/* 3. T1 writes B: records the edge T2 --rw--> T1. */
	t1_rc = a ? put_int(db_b, t1, "on_call", 0) : 0;
	if (t1_rc != 0 && !is_abort_rc(t1_rc))
		die("T1 write B", t1_rc);

	/* 4. T1 commits.  It holds only the write end, so it is not a pivot. */
	if (t1_rc != 0) {
		if ((rc = t1->abort(t1)) != 0)
			die("T1 abort", rc);
	} else if ((t1_rc = t1->commit(t1, 0)) != 0 && !is_abort_rc(t1_rc))
		die("T1 commit", t1_rc);
	*t1_ok = (t1_rc == 0);

	/*
	 * 5. *** THE WINDOW ***  T1 has committed.  Its SIREAD marker on A is
	 * now the ONLY record of the edge that must abort T2 when T2 writes A,
	 * and T1's serialization point is newer than T2's snapshot, so GC MUST
	 * keep it.  Hit it with everything: the pressure sweep at txn_begin,
	 * and one or more forced checkpoints.
	 */
	filler_reads(nfiller, cursor);
	sample_locks();
	if ((rc = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0)
		die("checkpoint (window)", rc);
	filler_reads(nfiller, cursor);
	sample_locks();
	if ((iter & 1) != 0 &&
	    (rc = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0)
		die("checkpoint (window 2)", rc);

	/* 6. T2 writes A.  Under SSI this must be refused. */
	t2_rc = b ? put_int(db_a, t2, "on_call", 0) : 0;
	if (t2_rc != 0 && !is_abort_rc(t2_rc))
		die("T2 write A", t2_rc);

	/* 7. T2 ends. */
	if (t2_rc != 0) {
		if ((rc = t2->abort(t2)) != 0)
			die("T2 abort", rc);
	} else if ((t2_rc = t2->commit(t2, 0)) != 0 && !is_abort_rc(t2_rc))
		die("T2 commit", t2_rc);
	*t2_ok = (t2_rc == 0);

	if ((rc = holder->commit(holder, 0)) != 0)
		die("holder commit", rc);

	/* Read the committed state back. */
	if ((rc = get_int(db_a, NULL, "on_call", &a)) != 0 ||
	    (rc = get_int(db_b, NULL, "on_call", &b)) != 0)
		die("read back", rc);

	if (verbose)
		printf("    iter %3d: T1 %-22s T2 %-22s -> (A,B)=(%d,%d)\n",
		    iter, rc_name(t1_rc), rc_name(t2_rc), a, b);

	return (a == 0 && b == 0);
}

int
main(int argc, char *argv[])
{
	DB_LOCK_STAT *lstat;
	const char *level;
	unsigned int cursor;
	u_int32_t gc_threshold;
	int i, iter, nfiller, nskew, rc, t1_ok, t2_ok, both, neither;

	iter = 120;
	nfiller = 220;
	cursor = 0;
	nskew = both = neither = 0;

	if ((level = getenv("ISO_LEVEL")) != NULL) {
		if (strcmp(level, "snapshot") == 0)
			iso_level = DB_TXN_SNAPSHOT;
		else if (strcmp(level, "serializable") == 0)
			iso_level = DB_TXN_SERIALIZABLE;
		else {
			fprintf(stderr, "ISO_LEVEL must be snapshot or "
			    "serializable\n");
			return (2);
		}
	}
	if (getenv("SSI_GC_ITER") != NULL)
		iter = atoi(getenv("SSI_GC_ITER"));
	if (getenv("SSI_GC_FILLER") != NULL)
		nfiller = atoi(getenv("SSI_GC_FILLER"));
	if (getenv("SSI_GC_OBJECTS") != NULL)
		nobjects = (u_int32_t)atoi(getenv("SSI_GC_OBJECTS"));
	if (getenv("SSI_GC_VERBOSE") != NULL)
		verbose = 1;
	if (argc > 1)
		iter = atoi(argv[1]);
	if (iter <= 0 || nfiller < 0)
		return (2);

	printf("=== SSI marker-GC correctness: write skew under GC pressure\n");
	printf("    level=%s iterations=%d filler-txns/iter=%d"
	    " lock-objects=%lu\n",
	    iso_level == DB_TXN_SERIALIZABLE ? "serializable" : "snapshot",
	    iter, nfiller, (u_long)nobjects);

	(void)mkdir(HOME, 0755);
	rmtree(HOME);
	env_open();

	/* Populate the filler keyspace so the lock-object table is wide. */
	for (i = 0; i < NFILLER_KEYS; i++) {
		char key[32];

		(void)snprintf(key, sizeof(key), "f%06d", i);
		if ((rc = put_int(db_f, NULL, key, i)) != 0)
			die("filler populate", rc);
	}

	for (i = 0; i < iter; i++) {
		if (run_iteration(i, &cursor, nfiller, &t1_ok, &t2_ok)) {
			nskew++;
			if (nskew <= 3)
				printf("    WRITE SKEW COMMITTED at iteration"
				    " %d: A=0 and B=0, which no serial order of"
				    " {T1,T2} can produce\n", i);
		}
		if (t1_ok && t2_ok)
			both++;
		if (!t1_ok && !t2_ok)
			neither++;
	}

	if ((rc = env->lock_stat(env, &lstat, 0)) != 0)
		die("lock_stat", rc);
	gc_threshold = lstat->st_objects / GC_TRIGGER_DIV;
	printf("    lock region: st_objects=%lu (sweep threshold %lu markers)"
	    " st_nobjects=%lu st_nlockers=%lu st_nlocks=%lu"
	    " peak-live-locks=%lu\n",
	    (u_long)lstat->st_objects, (u_long)gc_threshold,
	    (u_long)lstat->st_nobjects, (u_long)lstat->st_nlockers,
	    (u_long)lstat->st_nlocks, (u_long)peak_locks);
	free(lstat);
	env_close();

	printf("    iterations=%d  both-committed=%d  neither-committed=%d"
	    "  write-skews=%d\n", iter, both, neither, nskew);

	if (iso_level == DB_TXN_SERIALIZABLE) {
		/*
		 * THE assertion.  Reported first and unconditionally: a
		 * committed skew is the real finding, and it must never be
		 * masked by the secondary pressure check below (a neutered GC
		 * reaps markers so eagerly that the live population also drops,
		 * which would otherwise report the wrong reason).
		 */
		if (nskew != 0) {
			printf("FAIL: %d of %d iterations committed a write"
			    " skew under DB_TXN_SERIALIZABLE -- a SIREAD"
			    " marker was dropped while it was still needed,"
			    " so the rw-antidependency was MISSED\n",
			    nskew, iter);
			return (1);
		}
		/*
		 * Liveness: under SSI exactly one of the pair must survive
		 * every iteration.  "neither committed" would also be
		 * serializable but would mean the schedule stopped exercising
		 * the marker path, so require the real shape.
		 */
		if (both != 0) {
			printf("FAIL: %d iterations had BOTH transactions"
			    " commit\n", both);
			return (1);
		}
		if (neither == iter) {
			printf("FAIL: no iteration committed anything -- the"
			    " schedule is not exercising the engine\n");
			return (1);
		}
		/*
		 * Anti-vacuity: the run must actually have applied pressure.
		 * If the live-marker population never reached the txn_begin
		 * sweep threshold, only the checkpoint path swept and this is a
		 * weaker test than it claims to be -- say so rather than pass
		 * quietly.  Checked AFTER the skew assertion above.
		 */
		if (peak_locks <= gc_threshold) {
			printf("FAIL: peak live locks %lu never reached the"
			    " txn_begin sweep threshold %lu -- raise"
			    " SSI_GC_FILLER; the pressure trigger was not"
			    " exercised\n",
			    (u_long)peak_locks, (u_long)gc_threshold);
			return (1);
		}
		printf("PASS: 0 write skews in %d iterations under heavy"
		    " SIREAD-marker GC pressure (every skew pair had an"
		    " abort; peak live markers %lu > sweep threshold %lu, so"
		    " the txn_begin sweep did fire)\n",
		    iter, (u_long)peak_locks, (u_long)gc_threshold);
		return (0);
	}

	/*
	 * ISO_LEVEL=snapshot is the anti-vacuity control: plain snapshot
	 * isolation takes no SIREAD markers, so this schedule IS a legal
	 * write skew and MUST show up.  If it does not, the harness is not
	 * driving the engine and the serializable run proves nothing.
	 */
	if (nskew == 0) {
		printf("FAIL: plain DB_TXN_SNAPSHOT committed no write skew in"
		    " %d iterations -- the schedule is not a write skew, so a"
		    " clean serializable run would be vacuous\n", iter);
		return (1);
	}
	printf("PASS (control): plain DB_TXN_SNAPSHOT committed the write skew"
	    " in %d of %d iterations, as snapshot isolation permits -- the"
	    " schedule has teeth\n", nskew, iter);
	return (0);
}

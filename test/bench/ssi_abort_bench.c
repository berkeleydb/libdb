/*-
 * libdb SSI abort-rate probe -- WRITE-SKEW schedule.
 *
 * Serializable Snapshot Isolation (DB_TXN_SERIALIZABLE) prevents write skew
 * and other snapshot anomalies by aborting the pivot of a dangerous rw-
 * antidependency structure.  Plain snapshot isolation (DB_TXN_SNAPSHOT) does
 * NOT: it lets write skew commit.  This probe measures the difference DIRECTLY
 * by running the same binary at either level and reporting the abort rate --
 * so the page-granularity SSI abort cost is measured, not asserted
 * (ROADMAP #17).
 *
 * WHY WRITE SKEW (and not read-then-write-the-same-hot-key):
 *   A read that then writes an *overlapping* hot key acquires a WRITE lock, so
 *   concurrent txns collide on the lock manager and the DEADLOCK DETECTOR
 *   resolves them as DB_LOCK_DEADLOCK *before* the SSI commit-time pivot ever
 *   fires.  That schedule deadlocks under BOTH levels and masks the SSI-only
 *   abort -- it cannot distinguish SI from SSI.  Write skew is the canonical
 *   SSI-only anomaly and has NO write-write lock conflict:
 *
 *     T1: read A, write B          T2: read B, write A
 *
 *   Each txn writes only what the other merely READ.  Under snapshot isolation
 *   both commit (MVCC reads take no blocking lock; the writes touch disjoint
 *   keys) -- a non-serializable result.  Under SSI the rw-antidependency cycle
 *   (T1 read-depends on A that T2 writes; T2 read-depends on B that T1 writes)
 *   is a dangerous structure and SSI aborts one with DB_SNAPSHOT_CONFLICT.
 *   Because there is no ww-conflict, the deadlock detector does NOT fire: the
 *   abort is purely the SSI pivot.  THIS is the schedule that separates the
 *   levels.
 *
 * THE SCHEDULE (a scalable rw-antidependency RING):
 *   The hot set is `hotkeys` contiguous records (key k00000000 .. k<hotkeys-1>).
 *   For a run of N worker threads we pick RING POSITIONS = N and a SPREAD
 *   SP = hotkeys / N.  Worker i takes ring position p = i, OWNS the write key
 *   at index p*SP and READS the next position's write key at index
 *   ((p+1) mod N)*SP, then writes its own key = neighbour_value + 1.  So:
 *     - the N write keys are SP indices apart, and SP is kept >= SSI_MIN_SPREAD
 *       (> the ~3 records the btree packs per leaf), so no two workers' write
 *       keys share a leaf page: there is NO write-write lock conflict and NO
 *       page-granularity conflict, and the deadlock detector stays quiet;
 *     - the reads chain position 0 -> 1 -> ... -> N-1 -> 0: one big
 *       rw-antidependency RING, the general form of the two-txn write skew.
 *   Under SNAPSHOT every txn commits (reads are MVCC/lock-free; writes hit
 *   disjoint pages) -> ssi_abort == 0.  Under SERIALIZABLE the ring is a
 *   dangerous structure and SSI aborts pivots (reported as ssi_abort, counting
 *   both DB_SNAPSHOT_CONFLICT and DB_SNAPSHOT_UNSAFE -- see IS_SSI_ABORT) ->
 *   ssi_abort > 0.  Deadlock stays ~0 at BOTH levels; a nonzero deadlock count
 *   means SP fell below SSI_MIN_SPREAD (raise hotkeys relative to threads).
 *
 * EXPECTED OUTCOME (the whole point of the probe):
 *   ISO_LEVEL=snapshot      -> ssi_abort == 0  (SI permits write skew)
 *   ISO_LEVEL=serializable  -> ssi_abort  > 0  (SSI aborts the pivot)
 *   ...with deadlock ~0 at BOTH levels.  If deadlocks are nonzero the schedule
 *   is lock-conflict-bound (spread too small) and the result is meaningless.
 *
 *   cc -O2 -pthread ssi_abort_bench.c -I<build> -L<build>/.libs -ldb-2026 \
 *       -o ssi_abort_bench
 *
 *   ISO_LEVEL=serializable ./ssi_abort_bench <hotkeys> <secs> <t1> [t2 ...]
 *   ISO_LEVEL=snapshot     ./ssi_abort_bench <hotkeys> <secs> <t1> [t2 ...]
 *
 *   ISO_LEVEL selects the level (serializable|snapshot, default serializable,
 *   matching test/isolation's convention).  hotkeys is the size of the
 *   contiguous hot set; it is automatically grown to keep the spread
 *   SP = hotkeys/threads >= SSI_MIN_SPREAD for each thread count, so a small
 *   value + many threads still yields a clean, deadlock-free ring.  Use
 *   --selfcheck for a deterministic two-txn sanity of the write-skew anomaly.
 *
 *   e.g.  ISO_LEVEL=serializable ./ssi_abort_bench 16 5 4 8 16 32
 *
 * NOT a TPC benchmark and not comparable to any TPC result.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"
#include <stdatomic.h>

/*
 * Padded values so the btree packs only ~3 records per leaf page.  The value
 * stays INLINE (below the ~pagesize/4 overflow threshold): an overflow item
 * would collapse all the pointers onto one leaf page and reintroduce
 * page-granularity conflicts, defeating the whole design.
 */
#define	SSI_PAGESIZE	1024
#define	SSI_PAD		200	/* inline; ~3 records per 1024-byte leaf */

/*
 * Minimum index spread between two workers' write keys.  The btree packs ~3
 * records per leaf, so a spread of 8 comfortably lands every worker's write
 * key on a DISTINCT leaf page -> no page-granularity write conflict.
 */
#define	SSI_MIN_SPREAD	8

static DB_ENV *env;
static DB *db;
static int hotkeys;		/* size of the contiguous hot key set */
static u_int32_t iso_level = DB_TXN_SERIALIZABLE;
static const char *iso_level_name = "serializable";
static atomic_int stop;
static atomic_int go;
static atomic_int panicked;	/* set on DB_RUNRECOVERY: stop, don't spin */

typedef struct {
	pthread_t tid;
	int idx;		/* worker index / ring position 0..nthreads-1 */
	int nthreads;		/* number of ring positions this run */
	int spread;		/* SP = hotkeys / nthreads (>= SSI_MIN_SPREAD) */
	unsigned seed;
	long committed, aborted, deadlock, other;
} targ_t;

/* Record name for hot-set key index i (contiguous key order). */
static void
slot_key(char *kbuf, size_t sz, int i)
{
	snprintf(kbuf, sz, "k%08d", i);
}

/* Build the value string val, padded to SSI_PAD so ~2 records per leaf. */
static void
make_value(char *vbuf, size_t vsz, int newval)
{
	int n = snprintf(vbuf, vsz, "%d", newval);
	if (n < 0) n = 0;
	if ((size_t)n < vsz - 1) {
		memset(vbuf + n, 'x', vsz - 1 - (size_t)n);
		vbuf[vsz - 1] = '\0';
	}
}

/*
 * BDB's SSI reports an anomaly abort with EITHER code: DB_SNAPSHOT_CONFLICT
 * (the classic commit-time / first-updater pivot) or DB_SNAPSHOT_UNSAFE (an
 * earlier "potential serializable anomaly" detection).  Both are the SSI
 * mechanism aborting a dangerous rw-antidependency and both are ONLY possible
 * under DB_TXN_SERIALIZABLE -- so both count as ssi_abort.
 */
#define	IS_SSI_ABORT(r)	((r) == DB_SNAPSHOT_CONFLICT || (r) == DB_SNAPSHOT_UNSAFE)

/*
 * One write-skew transaction, RING form.  Worker at ring position p (0..N-1)
 * OWNS write key index p*spread and READS the next position's write key index
 * ((p+1) mod N)*spread, writing its own key = neighbour_value + 1.  The N
 * write keys are `spread` (>= SSI_MIN_SPREAD) indices apart, so they land on
 * distinct leaf pages -- no ww / no page conflict.  The reads chain
 * 0 -> 1 -> ... -> N-1 -> 0: one rw-antidependency ring.  Under SNAPSHOT all
 * commit; under SERIALIZABLE SSI aborts pivots with DB_SNAPSHOT_CONFLICT.
 */
static void
one_txn(targ_t *t)
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[SSI_PAD + 1];
	int wkey, rkey, ret, val;

	wkey = t->idx * t->spread;
	rkey = ((t->idx + 1) % t->nthreads) * t->spread;

	if (env->txn_begin(env, NULL, &txn, iso_level) != 0) {
		t->other++;
		return;
	}

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	/* Read the ring neighbour's write key. */
	slot_key(kbuf, sizeof(kbuf), rkey);
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.flags = DB_DBT_MALLOC;
	ret = db->get(db, txn, &key, &data, 0);
	if (ret == 0 && data.data != NULL) {
		val = atoi((char *)data.data);
		free(data.data);
	} else
		val = 0;
	if (ret == DB_LOCK_DEADLOCK || IS_SSI_ABORT(ret))
		goto conflict;

	/* Write own key = f(neighbour): a real read/write antidependency. */
	slot_key(kbuf, sizeof(kbuf), wkey);
	make_value(vbuf, sizeof(vbuf), val + 1);
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	ret = db->put(db, txn, &key, &data, 0);
	if (ret == DB_LOCK_DEADLOCK || IS_SSI_ABORT(ret))
		goto conflict;
	if (ret != 0) {
		(void)txn->abort(txn);
		if (ret == DB_RUNRECOVERY) atomic_store(&panicked, 1);
		t->other++;
		return;
	}

	ret = txn->commit(txn, 0);
	if (ret == 0) { t->committed++; return; }
	if (IS_SSI_ABORT(ret)) { t->aborted++; return; }
	if (ret == DB_LOCK_DEADLOCK) { t->deadlock++; return; }
	if (ret == DB_RUNRECOVERY) atomic_store(&panicked, 1);
	t->other++;
	return;

conflict:
	(void)txn->abort(txn);
	if (IS_SSI_ABORT(ret)) t->aborted++;
	else t->deadlock++;
}

static void *
worker(void *arg)
{
	targ_t *t = arg;
	while (!atomic_load(&go)) ;
	while (!atomic_load(&stop) && !atomic_load(&panicked))
		one_txn(t);
	return (NULL);
}

static void
run(int nthreads, int secs)
{
	targ_t *ta;
	int i, spread;
	long c = 0, a = 0, d = 0, o = 0, total;

	spread = hotkeys / nthreads;	/* index gap between adjacent write keys */

	ta = calloc((size_t)nthreads, sizeof(*ta));
	atomic_store(&stop, 0); atomic_store(&go, 0); atomic_store(&panicked, 0);
	for (i = 0; i < nthreads; i++) {
		ta[i].idx = i;
		ta[i].nthreads = nthreads;
		ta[i].spread = spread;
		ta[i].seed = (unsigned)(i * 2654435761u + 1);
		pthread_create(&ta[i].tid, NULL, worker, &ta[i]);
	}
	atomic_store(&go, 1);
	sleep(secs);
	atomic_store(&stop, 1);
	for (i = 0; i < nthreads; i++) {
		pthread_join(ta[i].tid, NULL);
		c += ta[i].committed; a += ta[i].aborted;
		d += ta[i].deadlock;  o += ta[i].other;
	}
	total = c + a + d + o;
	printf("level=%-12s threads=%-3d hot=%-5d spread=%-3d commit=%-8ld "
	    "ssi_abort=%-7ld deadlock=%-6ld other=%-5ld  abort_rate=%.1f%% "
	    "(%.0f txn/s)%s%s\n",
	    iso_level_name, nthreads, hotkeys, spread, c, a, d, o,
	    total ? 100.0 * (double)(a + d) / (double)total : 0.0,
	    (double)total / secs,
	    spread < SSI_MIN_SPREAD ? "  [SPREAD TOO SMALL -- deadlocks expected]" : "",
	    atomic_load(&panicked) ? "  [ENV PANIC -- see note]" : "");
	free(ta);
}

/* Read slot i's integer value under txn (0 if absent). */
static int
read_key(DB_TXN *txn, int i)
{
	DBT key, data;
	char kbuf[32];
	int val, ret;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	slot_key(kbuf, sizeof(kbuf), i);
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.flags = DB_DBT_MALLOC;
	ret = db->get(db, txn, &key, &data, 0);
	if (ret == 0 && data.data != NULL) {
		val = atoi((char *)data.data);
		free(data.data);
		return (val);
	}
	return (0);
}

static int
write_key(DB_TXN *txn, int i, int newval)
{
	DBT key, data;
	char kbuf[32], vbuf[SSI_PAD + 1];

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	slot_key(kbuf, sizeof(kbuf), i);
	make_value(vbuf, sizeof(vbuf), newval);
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	return (db->put(db, txn, &key, &data, 0));
}

/*
 * Deterministic two-txn write-skew sanity, at the CURRENT iso_level.  Uses two
 * slots A and B on DIFFERENT leaf pages (SSI_MIN_SPREAD indices apart) so
 * there is no page-granularity conflict -- the only thing that can abort is
 * the SSI pivot.
 *   A=B=0.  T1: read A, write B=A+1.  T2: read B, write A=B+1.
 *   Interleave both reads before both writes.
 *   Under snapshot: BOTH commit; final A=1,B=1 -- a skewed, non-serializable
 *     state (no serial order of T1,T2 yields A==B==1; serial gives {1,2} or
 *     {2,1}).
 *   Under serializable: exactly one commits with DB_SNAPSHOT_CONFLICT on the
 *     other.
 * Returns 0 if the level behaved as specified, 1 otherwise.
 */
#define	SC_A	0
#define	SC_B	SSI_MIN_SPREAD	/* >= records-per-leaf from A, so its own page */
static int
selfcheck(void)
{
	DB_TXN *t1, *t2;
	int rA, rB, rc1, rc2, a, b, fail = 0;

	/* Reset A=B=0 in autocommit txns. */
	(void)write_key(NULL, SC_A, 0);
	(void)write_key(NULL, SC_B, 0);

	if (env->txn_begin(env, NULL, &t1, iso_level) != 0 ||
	    env->txn_begin(env, NULL, &t2, iso_level) != 0) {
		fprintf(stderr, "selfcheck: txn_begin failed\n");
		return (1);
	}
	/* Both reads first (interleaved). */
	rA = read_key(t1, SC_A);
	rB = read_key(t2, SC_B);
	/* Then both writes: T1 writes B=rA+1, T2 writes A=rB+1. */
	rc1 = write_key(t1, SC_B, rA + 1);
	rc2 = write_key(t2, SC_A, rB + 1);
	if (rc1 == 0) rc1 = t1->commit(t1, 0); else (void)t1->abort(t1);
	if (rc2 == 0) rc2 = t2->commit(t2, 0); else (void)t2->abort(t2);

	a = read_key(NULL, SC_A);
	b = read_key(NULL, SC_B);

	printf("selfcheck[%s]: T1(read A=%d,write B)->%s  "
	    "T2(read B=%d,write A)->%s  final A=%d B=%d\n",
	    iso_level_name, rA, db_strerror(rc1), rB, db_strerror(rc2), a, b);

	if (iso_level == DB_TXN_SNAPSHOT) {
		/* SI: both commit to the skewed state A==B==1. */
		if (rc1 == 0 && rc2 == 0 && a == 1 && b == 1)
			printf("  PASS: snapshot permitted write skew "
			    "(non-serializable state A==B==1)\n");
		else {
			printf("  FAIL: expected both commit and A==B==1\n");
			fail = 1;
		}
	} else {
		/* SSI: exactly one aborts (DB_SNAPSHOT_CONFLICT or _UNSAFE). */
		int aborts = IS_SSI_ABORT(rc1) + IS_SSI_ABORT(rc2);
		int commits = (rc1 == 0) + (rc2 == 0);
		if (aborts == 1 && commits == 1)
			printf("  PASS: serializable aborted one pivot with an "
			    "SSI conflict (write skew prevented)\n");
		else {
			printf("  FAIL: expected exactly one commit and one "
			    "SSI abort (DB_SNAPSHOT_CONFLICT/_UNSAFE)\n");
			fail = 1;
		}
	}
	return (fail);
}

static int
parse_iso_level(void)
{
	const char *lvl = getenv("ISO_LEVEL");
	if (lvl == NULL || strcmp(lvl, "serializable") == 0) {
		iso_level = DB_TXN_SERIALIZABLE;
		iso_level_name = "serializable";
	} else if (strcmp(lvl, "snapshot") == 0) {
		iso_level = DB_TXN_SNAPSHOT;
		iso_level_name = "snapshot";
	} else {
		fprintf(stderr, "ISO_LEVEL must be serializable or snapshot\n");
		return (-1);
	}
	return (0);
}

int
main(int argc, char **argv)
{
	DB_TXN *txn;
	int i, secs, ret, do_selfcheck = 0;

	if (parse_iso_level() != 0)
		return (2);

	/* --selfcheck runs the deterministic two-txn sanity and exits. */
	if (argc >= 2 && strcmp(argv[1], "--selfcheck") == 0) {
		do_selfcheck = 1;
		hotkeys = SC_B + 1;	/* seed slots 0..SC_B for A and B */
		secs = 0;
	} else if (argc < 4) {
		fprintf(stderr,
		    "usage: %s <hotkeys> <secs> <t1> [t2 ...]\n"
		    "       %s --selfcheck\n"
		    "  ISO_LEVEL=serializable|snapshot (default serializable)\n",
		    argv[0], argv[0]);
		return (1);
	} else {
		int j, maxthreads = 0;
		hotkeys = atoi(argv[1]);
		secs = atoi(argv[2]);
		if (hotkeys < 2) {
			fprintf(stderr, "hotkeys (hot set size) must be >= 2\n");
			return (1);
		}
		/*
		 * Grow the hot set so the spread SP = hotkeys/threads stays
		 * >= SSI_MIN_SPREAD for the LARGEST thread count -- otherwise
		 * adjacent workers' write keys share a leaf page and deadlock.
		 */
		for (j = 3; j < argc; j++)
			if (atoi(argv[j]) > maxthreads)
				maxthreads = atoi(argv[j]);
		if (maxthreads > 0 &&
		    hotkeys < maxthreads * SSI_MIN_SPREAD) {
			hotkeys = maxthreads * SSI_MIN_SPREAD;
			fprintf(stderr,
			    "note: grew hot set to %d keys so spread stays "
			    ">= %d at %d threads\n",
			    hotkeys, SSI_MIN_SPREAD, maxthreads);
		}
	}

	(void)mkdir("/tmp/ssi_abort_env", 0755);
	if ((ret = db_env_create(&env, 0)) != 0) goto err;
	env->set_cachesize(env, 0, 64 * 1024 * 1024, 1);
	/* Size the lock region generously so exhaustion isn't the variable. */
	env->set_lk_max_locks(env, 20000);
	env->set_lk_max_objects(env, 20000);
	env->set_lk_max_lockers(env, 20000);
	/*
	 * DB_LOCK_MINWRITE deadlock detection stays on.  The write-skew
	 * schedule is deliberately ww-conflict-free (concurrent txns write
	 * DIFFERENT keys, each on its own leaf page), so the detector should
	 * essentially never fire -- and the reported deadlock counter being
	 * near zero under serializable is exactly the proof that the SSI pivot,
	 * not the detector, is what aborts.  Keeping it on also makes the run
	 * self-cleaning if the schedule is ever perturbed into a real cycle.
	 */
	env->set_lk_detect(env, DB_LOCK_MINWRITE);
	/*
	 * DB_RECOVER: a benchmark run may be killed (timeout) mid-transaction,
	 * leaving a dirty region.  Always run recovery on open so a stale
	 * environment is cleaned rather than hanging or crashing the next run.
	 */
	if ((ret = env->open(env, "/tmp/ssi_abort_env",
	    DB_CREATE | DB_RECOVER | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_THREAD | DB_MULTIVERSION, 0644)) != 0) goto err;
	if ((ret = db_create(&db, env, 0)) != 0) goto err;
	/* Small pages + padded values so the btree packs ~3 records per leaf. */
	if ((ret = db->set_pagesize(db, SSI_PAGESIZE)) != 0) goto err;
	if ((ret = db->open(db, NULL, "ssi.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD | DB_MULTIVERSION,
	    0644)) != 0) goto err;

	/*
	 * Seed the hot set with contiguous keys k00000000..k<hotkeys-1>, value 0,
	 * padded.  --selfcheck seeds only slots 0..SC_B (A and B).
	 */
	env->txn_begin(env, NULL, &txn, 0);
	if (do_selfcheck) {
		for (i = 0; i <= SC_B; i++)
			(void)write_key(txn, i, 0);
	} else {
		for (i = 0; i < hotkeys; i++)
			(void)write_key(txn, i, 0);
	}
	txn->commit(txn, 0);

	if (do_selfcheck) {
		ret = selfcheck();
		db->close(db, 0);
		env->close(env, 0);
		return (ret);
	}

	for (i = 3; i < argc; i++)
		run(atoi(argv[i]), secs);

	db->close(db, 0);
	env->close(env, 0);
	return (0);
err:
	fprintf(stderr, "error: %s\n", db_strerror(ret));
	return (1);
}

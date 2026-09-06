/*-
 * test/isolation/test_iso_anomaly.c --
 *	Tier B1: isolation / anomaly checker.
 *
 * Runs concurrent transaction schedules under DB_TXN_SNAPSHOT (which in this
 * fork means serializable snapshot isolation) and VALIDATES the committed
 * outcome against some serial order of the committed transactions.
 *
 * The verdict is COMPUTED, never hard-coded: every scenario declares, per
 * transaction, a `model' function -- the transaction's semantics as a pure
 * function over an abstract state vector.  After the schedule runs we read
 * the real committed state back out of the databases, then enumerate every
 * permutation of the transactions that actually committed and apply their
 * models serially.  If no permutation reproduces the observed state, the
 * history is not serializable and the scenario FAILS with the schedule
 * printed.
 *
 * The abstract state vector holds two kinds of slot:
 *	[0, ndbkeys)		one slot per database record; the "actual"
 *				value is read back from the database.
 *	[ndbkeys, nslots)	OBSERVATION slots: what a transaction claims
 *				it read.  This is what makes the read-only
 *				anomaly checkable -- there the final stored
 *				state is fine and only the read-only
 *				transaction's observation has no serial
 *				explanation.  Observation slots belonging to a
 *				transaction that did not commit are ignored.
 *
 * Scenarios are deterministic.  Most are driven single-threaded (snapshot
 * isolation cares about txn_begin / commit ORDER, not about wall-clock
 * concurrency), so no scheduler is needed.  The one schedule that genuinely
 * needs two threads is the issue #136 trigger, where T2's write must land
 * while T1 is INSIDE DB_TXN->commit; that uses pthread barriers plus atomic
 * flags from the application side exactly as the #136 reporter did -- no
 * engine hook, no HAVE_DST site, zero production overhead.
 *
 * Usage:
 *	./test_iso_anomaly			run every scenario
 *	./test_iso_anomaly SCENARIO ...		run the named scenarios
 *	./test_iso_anomaly --list		list scenario names
 *
 * Exit status: 0 = every scenario matched its expectation, 1 = a scenario
 * did not (a serializability violation where none was expected, or an
 * expected-fail scenario that unexpectedly PASSED -- i.e. an issue got
 * fixed and the expectation needs updating), 2 = harness error.
 */
#include <errno.h>
#include <pthread.h>
#include <sched.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "db.h"

#define	ISO_MAX_SLOT	8
#define	ISO_MAX_TXN	4
#define	ISO_MAX_DB	3

typedef struct {
	int v[ISO_MAX_SLOT];
} iso_state;

typedef struct {
	const char *name;
	void	  (*model)(iso_state *);	/* Serial semantics. */
	int	    obs_lo, obs_hi;		/* Observation slots owned. */
	int	    committed;			/* Filled in by the run. */
	int	    end_rc;			/* commit/abort-cause rc. */
	char	    log[256];			/* What the run observed. */
} iso_txn;

struct iso_scenario;
typedef int (*iso_run_fn)(struct iso_scenario *, iso_state *, iso_txn *);

typedef struct iso_scenario {
	const char *name;
	const char *shape;
	int	    ndbkeys;		/* Slots [0,ndbkeys) are DB records. */
	int	    nslots;		/* Total slots (DB + observation). */
	int	    ntxn;
	int	    expect_fail;	/* Known-broken on master. */
	const char *issue;		/* Issue it reproduces, if any. */
	/*
	 * How many times to run the schedule.  Single-threaded schedules are
	 * deterministic and need one attempt.  The two-thread schedules whose
	 * whole point is a narrow window (T2's write must land while T1 is
	 * inside commit) do NOT hit that window every time -- T1's commit can
	 * finish before T2's put reaches the conflict check, degenerating into
	 * the benign "late" schedule.  Those get several attempts: the tier's
	 * job is to FIND a non-serializable history if one exists, so a
	 * violation in any attempt is a reproduction, and a clean sweep of
	 * every attempt is the pass.
	 */
	int	    attempts;
	iso_run_fn  run;
} iso_scenario;

/*
 * Shared environment state.  One scenario at a time, so globals are fine and
 * keep the thread bodies short.
 */
static DB_ENV	*env;
static DB	*dbs[ISO_MAX_DB];
static int	 ndbs;
static char	 iso_home[512];
static int	 verbose;

static void iso_die(const char *, int) __attribute__((noreturn));

static void
iso_die(const char *what, int rc)
{
	fprintf(stderr, "harness error: %s: %s (%d)\n",
	    what, db_strerror(rc), rc);
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
	case DB_NOTFOUND:		return ("DB_NOTFOUND");
	case DB_KEYEXIST:		return ("DB_KEYEXIST");
	default:			return (db_strerror(rc));
	}
}

/* An rc that legitimately means "this transaction had to give up". */
static int
iso_is_abort_rc(int rc)
{
	return (rc == DB_LOCK_DEADLOCK || rc == DB_LOCK_NOTGRANTED ||
	    rc == DB_SNAPSHOT_CONFLICT || rc == DB_SNAPSHOT_UNSAFE);
}

/*
 * Scratch directory handling.  Each scenario gets a fresh home so a scenario
 * never inherits another's log files or MVCC state.
 */
static void
iso_rmtree(const char *dir)
{
	char cmd[600];

	/* Bounded, no recursion into the harness; find(1) is the tool. */
	(void)snprintf(cmd, sizeof(cmd),
	    "find '%s' -mindepth 1 -delete 2>/dev/null", dir);
	(void)system(cmd);
}

static void
iso_home_init(const char *scenario)
{
	(void)snprintf(iso_home, sizeof(iso_home),
	    "ISODIR.%s", scenario);
	(void)mkdir(iso_home, 0755);
	iso_rmtree(iso_home);
}

/*
 * iso_env_open --
 *	Open (creating if asked) the environment and `n' databases.  pagesize
 *	!= 0 forces a small page size, which the same-btree scenario needs to
 *	push two records onto different pages.
 */
static void
iso_env_open(u_int32_t create, int n, const char *const *names,
    u_int32_t pagesize)
{
	int i, rc;

	if ((rc = db_env_create(&env, 0)) != 0)
		iso_die("db_env_create", rc);
	if ((rc = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		iso_die("set_lk_detect", rc);
	/*
	 * A lock timeout keeps the tier self-bounding: a schedule where two
	 * transactions hold conflicting write locks and neither is waiting on
	 * the other is not a deadlock, so the detector will not break it and
	 * a blocked put would hang forever.  With a timeout it returns
	 * DB_LOCK_NOTGRANTED, which the checker treats as "this transaction
	 * had to give up" -- a legitimate serializable outcome.
	 */
	if ((rc = env->set_timeout(env, 2000000, DB_SET_LOCK_TIMEOUT)) != 0)
		iso_die("set_timeout", rc);
	/*
	 * A modest cache with plenty of MVCC room: too small a cache makes
	 * snapshot transactions fail with DB_SNAPSHOT_UNSAFE for cache
	 * reasons, which would mask the isolation property under test.
	 */
	if ((rc = env->set_cachesize(env, 0, 4 * 1024 * 1024, 1)) != 0)
		iso_die("set_cachesize", rc);
	if ((rc = env->open(env, iso_home, create | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0600)) != 0)
		iso_die("DB_ENV->open", rc);

	ndbs = n;
	for (i = 0; i < n; i++) {
		if ((rc = db_create(&dbs[i], env, 0)) != 0)
			iso_die("db_create", rc);
		if (pagesize != 0 && (rc =
		    dbs[i]->set_pagesize(dbs[i], pagesize)) != 0)
			iso_die("set_pagesize", rc);
		if ((rc = dbs[i]->open(dbs[i], NULL, names[i], NULL, DB_BTREE,
		    create | DB_MULTIVERSION | DB_AUTO_COMMIT | DB_THREAD,
		    0600)) != 0)
			iso_die("DB->open", rc);
	}
}

static void
iso_env_close(void)
{
	int i, rc;

	for (i = 0; i < ndbs; i++)
		if ((rc = dbs[i]->close(dbs[i], 0)) != 0)
			iso_die("DB->close", rc);
	ndbs = 0;
	if ((rc = env->close(env, 0)) != 0)
		iso_die("DB_ENV->close", rc);
	env = NULL;
}

/*
 * Record payload.  The first sizeof(int) bytes are the value; `pad' extra
 * bytes let a scenario make records big enough to force page splits.
 */
#define	ISO_PAD_MAX	512
static u_int32_t iso_pad;

static int
iso_get(DB *db, DB_TXN *txn, const char *key, int *out)
{
	DBT k, d;
	u_int8_t buf[sizeof(int) + ISO_PAD_MAX];
	int rc;

	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = (void *)key;
	k.size = (u_int32_t)strlen(key);
	d.data = buf;
	d.ulen = sizeof(buf);
	d.flags = DB_DBT_USERMEM;
	if ((rc = db->get(db, txn, &k, &d, 0)) != 0)
		return (rc);
	if (d.size < sizeof(int))
		iso_die("short record", EINVAL);
	memcpy(out, buf, sizeof(int));
	return (0);
}

static int
iso_put(DB *db, DB_TXN *txn, const char *key, int val)
{
	DBT k, d;
	u_int8_t buf[sizeof(int) + ISO_PAD_MAX];

	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	memset(buf, 0x5a, sizeof(buf));
	memcpy(buf, &val, sizeof(val));
	k.data = (void *)key;
	k.size = (u_int32_t)strlen(key);
	d.data = buf;
	d.size = (u_int32_t)(sizeof(int) + iso_pad);
	return (db->put(db, txn, &k, &d, 0));
}

static void
iso_note(iso_txn *t, const char *fmt, ...)
{
	va_list ap;
	size_t n;

	n = strlen(t->log);
	if (n + 2 >= sizeof(t->log))
		return;
	if (n != 0) {
		t->log[n++] = ';';
		t->log[n++] = ' ';
		t->log[n] = '\0';
	}
	va_start(ap, fmt);
	(void)vsnprintf(t->log + n, sizeof(t->log) - n, fmt, ap);
	va_end(ap);
}

/*
 * iso_finish --
 *	End a transaction: commit if its operations succeeded, abort if one of
 *	them already told us to give up.  Records the outcome on the txn.
 */
static void
iso_finish(iso_txn *t, DB_TXN *txn, int op_rc)
{
	int rc;

	if (op_rc != 0) {
		if ((rc = txn->abort(txn)) != 0)
			iso_die("DB_TXN->abort", rc);
		t->committed = 0;
		t->end_rc = op_rc;
		iso_note(t, "abort (%s)", rc_name(op_rc));
		if (!iso_is_abort_rc(op_rc))
			iso_die("unexpected operation failure", op_rc);
		return;
	}
	rc = txn->commit(txn, 0);
	t->end_rc = rc;
	t->committed = (rc == 0);
	iso_note(t, "commit -> %s", rc_name(rc));
	if (rc != 0 && !iso_is_abort_rc(rc))
		iso_die("unexpected commit failure", rc);
}

/*
 * ---------------------------------------------------------------------------
 * The serializability verdict.
 * ---------------------------------------------------------------------------
 */
static int
iso_state_matches(const iso_scenario *sc, const iso_state *model,
    const iso_state *actual, const iso_txn *t)
{
	int i, j;

	for (i = 0; i < sc->ndbkeys; i++)
		if (model->v[i] != actual->v[i])
			return (0);
	/* Only a committed transaction's observations are binding. */
	for (j = 0; j < sc->ntxn; j++) {
		if (!t[j].committed)
			continue;
		for (i = t[j].obs_lo; i < t[j].obs_hi; i++)
			if (model->v[i] != actual->v[i])
				return (0);
	}
	return (1);
}

/*
 * iso_try_orders --
 *	Depth-first enumeration of every permutation of the COMMITTED
 *	transactions.  Returns 1 (and fills `order') as soon as one serial
 *	order reproduces the observed state.
 */
static int
iso_try_orders(const iso_scenario *sc, const iso_state *cur,
    const iso_state *actual, iso_txn *t, int *used, int depth, int ncommitted,
    int *order)
{
	iso_state next;
	int i;

	if (depth == ncommitted)
		return (iso_state_matches(sc, cur, actual, t));

	for (i = 0; i < sc->ntxn; i++) {
		if (used[i] || !t[i].committed)
			continue;
		used[i] = 1;
		next = *cur;
		t[i].model(&next);
		order[depth] = i;
		if (iso_try_orders(sc, &next, actual, t, used, depth + 1,
		    ncommitted, order))
			return (1);
		used[i] = 0;
	}
	return (0);
}

/*
 * iso_serializable --
 *	Is `actual' reachable from `initial' by some serial order of the
 *	committed transactions?
 */
static int
iso_serializable(const iso_scenario *sc, const iso_state *initial,
    const iso_state *actual, iso_txn *t, int *order, int *ncommitted_out)
{
	int i, ncommitted, used[ISO_MAX_TXN];

	for (i = ncommitted = 0; i < sc->ntxn; i++) {
		used[i] = 0;
		if (t[i].committed)
			ncommitted++;
	}
	*ncommitted_out = ncommitted;
	return (iso_try_orders(sc, initial, actual, t, used, 0, ncommitted,
	    order));
}

/*
 * ---------------------------------------------------------------------------
 * Scenario 1-4: write skew (the two-doctors shape).
 *
 * alice and bob are both on call and at least one must stay on call.  T1
 * reads bob and takes alice off call; T2 reads alice and takes bob off call.
 * Both reads precede both writes, so under serializable isolation one of the
 * two must fail.
 *
 * Slots: 0 = alice.on_call, 1 = bob.on_call.
 * ---------------------------------------------------------------------------
 */
#define	SK_ALICE	0
#define	SK_BOB		1

static void
sk_t1_model(iso_state *s)		/* read bob, clear alice */
{
	if (s->v[SK_BOB])
		s->v[SK_ALICE] = 0;
}

static void
sk_t2_model(iso_state *s)		/* read alice, clear bob */
{
	if (s->v[SK_ALICE])
		s->v[SK_BOB] = 0;
}

/* Timing of T2's write relative to T1's commit. */
enum sk_mode { SK_TRIGGER, SK_CONTROL, SK_LATE };

static pthread_barrier_t sk_barrier;
static atomic_int sk_t1_in_commit, sk_t1_commit_done;
static enum sk_mode sk_mode;
static iso_txn *sk_txns;
static DB *sk_alice_db, *sk_bob_db;
static const char *sk_alice_key = "on_call", *sk_bob_key = "on_call";

static void
sk_sync(void)
{
	int rc = pthread_barrier_wait(&sk_barrier);

	if (rc != 0 && rc != PTHREAD_BARRIER_SERIAL_THREAD)
		iso_die("pthread_barrier_wait", rc);
}

static void *
sk_t2_thread(void *arg)
{
	DB_TXN *txn;
	iso_txn *t = &sk_txns[1];
	int alice, rc;

	(void)arg;
	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT)) != 0)
		iso_die("T2 txn_begin", rc);
	if ((rc = iso_get(sk_alice_db, txn, sk_alice_key, &alice)) != 0)
		iso_die("T2 read alice", rc);
	iso_note(t, "read alice=%d", alice);

	sk_sync();			/* both reads done */
	sk_sync();			/* T1's write done */

	/*
	 * The whole point of the tier: land T2's write while T1 is INSIDE
	 * DB_TXN->commit.  Barriers cannot express "inside a call", so T1
	 * publishes an atomic flag immediately before entering commit and we
	 * spin on it -- application-side only, exactly as issue #136 does.
	 */
	if (sk_mode == SK_TRIGGER)
		while (!atomic_load(&sk_t1_in_commit))
			sched_yield();
	else if (sk_mode == SK_LATE)
		while (!atomic_load(&sk_t1_commit_done))
			sched_yield();

	rc = alice ? iso_put(sk_bob_db, txn, sk_bob_key, 0) : 0;
	iso_note(t, "put bob=0 -> %s", rc_name(rc));
	iso_finish(t, txn, rc);
	if (sk_mode == SK_CONTROL)
		sk_sync();		/* T2 done before T1 commits */
	return (NULL);
}

static int
sk_run_common(iso_scenario *sc, iso_state *initial, iso_txn *t,
    int one_btree, u_int32_t pagesize, u_int32_t pad)
{
	static const char *two_dbs[] = { "alice.db", "bob.db" };
	static const char *one_db[] = { "roster.db" };
	pthread_t t2;
	DB_TXN *txn;
	int bob, rc;

	t[0].name = "T1";
	t[0].model = sk_t1_model;
	t[1].name = "T2";
	t[1].model = sk_t2_model;

	iso_pad = pad;
	iso_home_init(sc->name);
	if (one_btree) {
		iso_env_open(DB_CREATE, 1, one_db, pagesize);
		sk_alice_db = sk_bob_db = dbs[0];
		sk_alice_key = "alice";
		sk_bob_key = "bob";
	} else {
		iso_env_open(DB_CREATE, 2, two_dbs, pagesize);
		sk_alice_db = dbs[0];
		sk_bob_db = dbs[1];
		sk_alice_key = sk_bob_key = "on_call";
	}

	if ((rc = iso_put(sk_alice_db, NULL, sk_alice_key, 1)) != 0 ||
	    (rc = iso_put(sk_bob_db, NULL, sk_bob_key, 1)) != 0)
		iso_die("initial put", rc);
	initial->v[SK_ALICE] = initial->v[SK_BOB] = 1;

	if (one_btree) {
		/*
		 * The interesting shape is "two records on DIFFERENT pages of
		 * ONE B-tree".  Keys sort as alice < b* < bob, so filling the
		 * middle with records splits the leaf and pushes alice (the
		 * minimum key) and bob (the maximum key) onto different
		 * leaves.  The payload must stay under the B-tree overflow
		 * threshold (pagesize/4) or records move off-page and the leaf
		 * never splits.
		 */
		DB_BTREE_STAT *bst;
		char fill[16];
		int f;

		for (f = 0; f < 64; f++) {
			(void)snprintf(fill, sizeof(fill), "b%04d", f);
			if ((rc = iso_put(dbs[0], NULL, fill, f)) != 0)
				iso_die("filler put", rc);
		}
		if ((rc = dbs[0]->stat(dbs[0], NULL, &bst, 0)) != 0)
			iso_die("DB->stat", rc);
		f = (int)bst->bt_leaf_pg;
		free(bst);
		if (verbose)
			printf("    btree leaf pages = %d (need >= 2 so the "
			    "min and max key are on different pages)\n", f);
		if (f < 2) {
			fprintf(stderr, "    NOTE: tree did not split;"
			    " different-pages shape not exercised\n");
			return (-1);
		}
	}

	if ((rc = pthread_barrier_init(&sk_barrier, NULL, 2)) != 0)
		iso_die("pthread_barrier_init", rc);
	atomic_store(&sk_t1_in_commit, 0);
	atomic_store(&sk_t1_commit_done, 0);
	sk_txns = t;
	if ((rc = pthread_create(&t2, NULL, sk_t2_thread, NULL)) != 0)
		iso_die("pthread_create", rc);

	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT)) != 0)
		iso_die("T1 txn_begin", rc);
	if ((rc = iso_get(sk_bob_db, txn, sk_bob_key, &bob)) != 0)
		iso_die("T1 read bob", rc);
	iso_note(&t[0], "read bob=%d", bob);
	sk_sync();			/* both reads done */
	rc = bob ? iso_put(sk_alice_db, txn, sk_alice_key, 0) : 0;
	iso_note(&t[0], "put alice=0 -> %s", rc_name(rc));
	sk_sync();			/* T1's write done */
	if (sk_mode == SK_CONTROL)
		sk_sync();		/* wait for T2 to finish */
	atomic_store(&sk_t1_in_commit, 1);
	iso_finish(&t[0], txn, rc);
	atomic_store(&sk_t1_commit_done, 1);

	if ((rc = pthread_join(t2, NULL)) != 0)
		iso_die("pthread_join", rc);
	(void)pthread_barrier_destroy(&sk_barrier);
	return (0);
}

static int
sk_read_back(iso_scenario *sc, iso_state *actual, int one_btree,
    u_int32_t pagesize)
{
	static const char *two_dbs[] = { "alice.db", "bob.db" };
	static const char *one_db[] = { "roster.db" };
	int rc;

	(void)sc;
	iso_env_close();
	/* Reopen: the verdict is about the DURABLE committed state. */
	if (one_btree) {
		iso_env_open(0, 1, one_db, pagesize);
		if ((rc = iso_get(dbs[0], NULL, "alice",
		    &actual->v[SK_ALICE])) != 0 ||
		    (rc = iso_get(dbs[0], NULL, "bob",
		    &actual->v[SK_BOB])) != 0)
			iso_die("read back", rc);
	} else {
		iso_env_open(0, 2, two_dbs, pagesize);
		if ((rc = iso_get(dbs[0], NULL, "on_call",
		    &actual->v[SK_ALICE])) != 0 ||
		    (rc = iso_get(dbs[1], NULL, "on_call",
		    &actual->v[SK_BOB])) != 0)
			iso_die("read back", rc);
	}
	iso_env_close();
	return (0);
}

static int
sk_run(iso_scenario *sc, iso_state *st, iso_txn *t, enum sk_mode mode,
    int one_btree, u_int32_t pagesize, u_int32_t pad)
{
	iso_state initial;
	int rc;

	memset(&initial, 0, sizeof(initial));
	sk_mode = mode;
	if ((rc = sk_run_common(sc, &initial, t, one_btree, pagesize,
	    pad)) != 0)
		return (rc);
	st[0] = initial;
	return (sk_read_back(sc, &st[1], one_btree, pagesize));
}

static int
sk_trigger(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	return (sk_run(sc, st, t, SK_TRIGGER, 0, 0, 0));
}

static int
sk_control(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	return (sk_run(sc, st, t, SK_CONTROL, 0, 0, 0));
}

static int
sk_late(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	return (sk_run(sc, st, t, SK_LATE, 0, 0, 0));
}

/*
 * The possibly-SEPARATE defect reported alongside #136: two records on
 * DIFFERENT PAGES of ONE B-tree are said to detect no conflict at all, even
 * in the control where T2 commits before T1 calls commit.
 *
 * OBSERVED on master (c4811dc87), with the shape verified by DB->stat: the
 * CONTROL timing DOES return DB_SNAPSHOT_CONFLICT here, exactly like the
 * two-one-page-databases control.  Only the TRIGGER timing commits both.  So
 * on this construction the different-pages case has the SAME root cause as
 * #136 proper (the commit-window race), and is NOT an additional
 * page-granularity conflict-detection hole.  The reporter did not publish
 * their same-btree variant, so their shape may differ; the control is kept as
 * a live PASS expectation precisely so a real page-granularity regression
 * would surface here.
 *
 * 512-byte pages plus filler keys sorting between "alice" and "bob" split the
 * leaf so the two records land on different pages.  We only OBSERVE here;
 * fixing anything belongs to the #136 engine work.
 */
static int
sk_samebtree_control(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	return (sk_run(sc, st, t, SK_CONTROL, 1, 512, 100));
}

static int
sk_samebtree_trigger(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	return (sk_run(sc, st, t, SK_TRIGGER, 1, 512, 100));
}

/*
 * ---------------------------------------------------------------------------
 * Scenario: G2 / anti-dependency cycle (G2-item on a predicate read).
 *
 * Both transactions scan the database counting "marker" records; each inserts
 * a marker only if it saw none.  Under serializable isolation at most one
 * insert may commit.  The read is a full cursor scan -- a predicate read --
 * so the anti-dependency runs through the range, not a single record.
 *
 * Slots: 0 = number of markers stored.
 * ---------------------------------------------------------------------------
 */
static void
g2_model(iso_state *s)
{
	if (s->v[0] == 0)
		s->v[0] = s->v[0] + 1;
}

static int
g2_count_markers(DB *db, DB_TXN *txn, int *out)
{
	DBC *dbc;
	DBT k, d;
	int n, rc;

	if ((rc = db->cursor(db, txn, &dbc, 0)) != 0)
		return (rc);
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	for (n = 0; (rc = dbc->get(dbc, &k, &d, DB_NEXT)) == 0; )
		if (k.size >= 6 && memcmp(k.data, "marker", 6) == 0)
			n++;
	(void)dbc->close(dbc);
	if (rc != DB_NOTFOUND)
		return (rc);
	*out = n;
	return (0);
}

static int
g2_antidep(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	static const char *names[] = { "markers.db" };
	DB_TXN *txn1, *txn2;
	int n1, n2, rc, rc1, rc2;

	t[0].name = "T1";
	t[0].model = g2_model;
	t[1].name = "T2";
	t[1].model = g2_model;

	iso_pad = 0;
	iso_home_init(sc->name);
	iso_env_open(DB_CREATE, 1, names, 0);
	/* A non-marker record so the scan has something to walk. */
	if ((rc = iso_put(dbs[0], NULL, "anchor", 0)) != 0)
		iso_die("initial put", rc);
	memset(&st[0], 0, sizeof(st[0]));
	st[0].v[0] = 0;			/* no markers */

	if ((rc = env->txn_begin(env, NULL, &txn1, DB_TXN_SNAPSHOT)) != 0 ||
	    (rc = env->txn_begin(env, NULL, &txn2, DB_TXN_SNAPSHOT)) != 0)
		iso_die("txn_begin", rc);

	/* Both predicate reads happen before either write. */
	if ((rc = g2_count_markers(dbs[0], txn1, &n1)) != 0)
		iso_die("T1 scan", rc);
	if ((rc = g2_count_markers(dbs[0], txn2, &n2)) != 0)
		iso_die("T2 scan", rc);
	iso_note(&t[0], "scan saw %d markers", n1);
	iso_note(&t[1], "scan saw %d markers", n2);

	/*
	 * T1 writes and commits, then T2 writes from its stale snapshot.  The
	 * anti-dependency (T2's predicate read did not see T1's insert) is
	 * what SSI must catch; T2 must not be allowed to commit its own
	 * insert.  Writes are serialised this way on purpose: two overlapping
	 * uncommitted writes to the same page would simply block on the page
	 * lock, which tests the lock manager rather than isolation.
	 */
	rc1 = n1 == 0 ? iso_put(dbs[0], txn1, "marker.t1", 1) : 0;
	iso_note(&t[0], "insert marker.t1 -> %s", rc_name(rc1));
	iso_finish(&t[0], txn1, rc1);

	rc2 = n2 == 0 ? iso_put(dbs[0], txn2, "marker.t2", 1) : 0;
	iso_note(&t[1], "insert marker.t2 -> %s", rc_name(rc2));
	iso_finish(&t[1], txn2, rc2);

	iso_env_close();
	iso_env_open(0, 1, names, 0);
	memset(&st[1], 0, sizeof(st[1]));
	if ((rc = g2_count_markers(dbs[0], NULL, &st[1].v[0])) != 0)
		iso_die("read back", rc);
	iso_env_close();
	return (0);
}

/*
 * ---------------------------------------------------------------------------
 * Scenario: read-only anomaly (Fekete's three-transaction pattern).
 *
 * x = savings, y = checking, both start at 0.
 *	Tdep  (deposit):   y := y + 20
 *	Twdw  (withdraw):  reads x and y; if x + y >= 11 then y := y - 11
 *			   else y := y - 11 - 1  (overdraft penalty)
 *	Tro   (read-only): reports x and y.
 *
 * Schedule: Twdw takes its snapshot first, Tdep commits, Tro runs and commits,
 * then Twdw commits.  The stored state is fine; Tro's OBSERVATION is what has
 * no serial explanation, which is why the checker compares observation slots.
 *
 * Slots: 0 = x, 1 = y, 2/3 = Tro's observed x/y.
 * ---------------------------------------------------------------------------
 */
#define	RO_X	0
#define	RO_Y	1
#define	RO_OX	2
#define	RO_OY	3

static void
ro_dep_model(iso_state *s)
{
	s->v[RO_Y] += 20;
}

static void
ro_wdw_model(iso_state *s)
{
	if (s->v[RO_X] + s->v[RO_Y] >= 11)
		s->v[RO_Y] -= 11;
	else
		s->v[RO_Y] -= 12;
}

static void
ro_ro_model(iso_state *s)
{
	s->v[RO_OX] = s->v[RO_X];
	s->v[RO_OY] = s->v[RO_Y];
}

static int
read_only_anomaly(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	static const char *names[] = { "savings.db", "checking.db" };
	DB_TXN *tdep, *twdw, *tro;
	int rc, rc_dep, rc_wdw, wx, wy, y;

	t[0].name = "Tdep";
	t[0].model = ro_dep_model;
	t[1].name = "Twdw";
	t[1].model = ro_wdw_model;
	t[2].name = "Tro";
	t[2].model = ro_ro_model;
	t[2].obs_lo = RO_OX;
	t[2].obs_hi = RO_OY + 1;

	iso_pad = 0;
	iso_home_init(sc->name);
	iso_env_open(DB_CREATE, 2, names, 0);
	if ((rc = iso_put(dbs[0], NULL, "bal", 0)) != 0 ||
	    (rc = iso_put(dbs[1], NULL, "bal", 0)) != 0)
		iso_die("initial put", rc);
	memset(&st[0], 0, sizeof(st[0]));

	/* Twdw takes its snapshot first and reads both balances. */
	if ((rc = env->txn_begin(env, NULL, &twdw, DB_TXN_SNAPSHOT)) != 0)
		iso_die("Twdw txn_begin", rc);
	if ((rc = iso_get(dbs[0], twdw, "bal", &wx)) != 0 ||
	    (rc = iso_get(dbs[1], twdw, "bal", &wy)) != 0)
		iso_die("Twdw read", rc);
	iso_note(&t[1], "read x=%d y=%d", wx, wy);

	/* Tdep deposits and commits. */
	if ((rc = env->txn_begin(env, NULL, &tdep, DB_TXN_SNAPSHOT)) != 0)
		iso_die("Tdep txn_begin", rc);
	if ((rc = iso_get(dbs[1], tdep, "bal", &y)) != 0)
		iso_die("Tdep read", rc);
	rc_dep = iso_put(dbs[1], tdep, "bal", y + 20);
	iso_note(&t[0], "y %d -> %d (%s)", y, y + 20, rc_name(rc_dep));
	iso_finish(&t[0], tdep, rc_dep);

	/* Tro starts AFTER Tdep committed and BEFORE Twdw commits. */
	if ((rc = env->txn_begin(env, NULL, &tro, DB_TXN_SNAPSHOT)) != 0)
		iso_die("Tro txn_begin", rc);
	if ((rc = iso_get(dbs[0], tro, "bal", &st[1].v[RO_OX])) != 0 ||
	    (rc = iso_get(dbs[1], tro, "bal", &st[1].v[RO_OY])) != 0)
		iso_die("Tro read", rc);
	iso_note(&t[2], "observed x=%d y=%d",
	    st[1].v[RO_OX], st[1].v[RO_OY]);
	iso_finish(&t[2], tro, 0);

	/* Now Twdw writes from its stale snapshot and commits. */
	rc_wdw = iso_put(dbs[1], twdw, "bal",
	    wx + wy >= 11 ? wy - 11 : wy - 12);
	iso_note(&t[1], "y -> %d (%s)",
	    wx + wy >= 11 ? wy - 11 : wy - 12, rc_name(rc_wdw));
	iso_finish(&t[1], twdw, rc_wdw);

	iso_env_close();
	iso_env_open(0, 2, names, 0);
	if ((rc = iso_get(dbs[0], NULL, "bal", &st[1].v[RO_X])) != 0 ||
	    (rc = iso_get(dbs[1], NULL, "bal", &st[1].v[RO_Y])) != 0)
		iso_die("read back", rc);
	iso_env_close();
	return (0);
}

/*
 * ---------------------------------------------------------------------------
 * Scenario: lost update.  Both transactions read the same counter and write
 * read+1.  Under any correct isolation level one must fail; if both commit,
 * the counter is 1 and no serial order explains it (serial gives 2).
 *
 * Slots: 0 = counter.
 * ---------------------------------------------------------------------------
 */
static void
lu_model(iso_state *s)
{
	s->v[0] += 1;
}

static int
lost_update(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	static const char *names[] = { "counter.db" };
	DB_TXN *txn1, *txn2;
	int n1, n2, rc, rc1, rc2;

	t[0].name = "T1";
	t[0].model = lu_model;
	t[1].name = "T2";
	t[1].model = lu_model;

	iso_pad = 0;
	iso_home_init(sc->name);
	iso_env_open(DB_CREATE, 1, names, 0);
	if ((rc = iso_put(dbs[0], NULL, "n", 0)) != 0)
		iso_die("initial put", rc);
	memset(&st[0], 0, sizeof(st[0]));

	if ((rc = env->txn_begin(env, NULL, &txn1, DB_TXN_SNAPSHOT)) != 0 ||
	    (rc = env->txn_begin(env, NULL, &txn2, DB_TXN_SNAPSHOT)) != 0)
		iso_die("txn_begin", rc);
	if ((rc = iso_get(dbs[0], txn1, "n", &n1)) != 0 ||
	    (rc = iso_get(dbs[0], txn2, "n", &n2)) != 0)
		iso_die("read", rc);
	iso_note(&t[0], "read n=%d", n1);
	iso_note(&t[1], "read n=%d", n2);

	rc1 = iso_put(dbs[0], txn1, "n", n1 + 1);
	iso_note(&t[0], "put n=%d -> %s", n1 + 1, rc_name(rc1));
	iso_finish(&t[0], txn1, rc1);
	rc2 = iso_put(dbs[0], txn2, "n", n2 + 1);
	iso_note(&t[1], "put n=%d -> %s", n2 + 1, rc_name(rc2));
	iso_finish(&t[1], txn2, rc2);

	iso_env_close();
	iso_env_open(0, 1, names, 0);
	memset(&st[1], 0, sizeof(st[1]));
	if ((rc = iso_get(dbs[0], NULL, "n", &st[1].v[0])) != 0)
		iso_die("read back", rc);
	iso_env_close();
	return (0);
}

/*
 * ---------------------------------------------------------------------------
 * Scenario: read-your-writes sanity.  A single transaction must see its own
 * uncommitted write, and the stored value after commit must match.  This
 * exists so a vacuously-passing checker is detectable: if the harness ever
 * stops driving the engine at all, this scenario fails.
 *
 * Slots: 0 = value, 1 = the value the transaction read back from itself.
 * ---------------------------------------------------------------------------
 */
static void
ryw_model(iso_state *s)
{
	s->v[0] = 7;
	s->v[1] = 7;			/* it must observe its own write */
}

static int
read_your_writes(iso_scenario *sc, iso_state *st, iso_txn *t)
{
	static const char *names[] = { "ryw.db" };
	DB_TXN *txn;
	int rc, seen;

	t[0].name = "T1";
	t[0].model = ryw_model;
	t[0].obs_lo = 1;
	t[0].obs_hi = 2;

	iso_pad = 0;
	iso_home_init(sc->name);
	iso_env_open(DB_CREATE, 1, names, 0);
	if ((rc = iso_put(dbs[0], NULL, "k", 0)) != 0)
		iso_die("initial put", rc);
	memset(&st[0], 0, sizeof(st[0]));

	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT)) != 0)
		iso_die("txn_begin", rc);
	if ((rc = iso_put(dbs[0], txn, "k", 7)) != 0)
		iso_die("put", rc);
	if ((rc = iso_get(dbs[0], txn, "k", &seen)) != 0)
		iso_die("get own write", rc);
	iso_note(&t[0], "wrote 7, read back %d", seen);
	memset(&st[1], 0, sizeof(st[1]));
	st[1].v[1] = seen;
	iso_finish(&t[0], txn, 0);

	iso_env_close();
	iso_env_open(0, 1, names, 0);
	if ((rc = iso_get(dbs[0], NULL, "k", &st[1].v[0])) != 0)
		iso_die("read back", rc);
	iso_env_close();
	return (0);
}

/*
 * ---------------------------------------------------------------------------
 * The scenario table.
 *
 * expect_fail marks a scenario that is KNOWN to admit a non-serializable
 * history on current master.  It is not a licence to be wrong: the runner
 * still reports it loudly, and if it starts producing a serializable history
 * (i.e. the engine bug is fixed) the run FAILS so the expectation gets
 * updated.  Flip expect_fail to 0 when the referenced issue lands.
 * ---------------------------------------------------------------------------
 */
static iso_scenario scenarios[] = {
    { "write_skew_trigger",
      "two one-page DBs; T2's write lands while T1 is inside commit",
      2, 2, 2, 1, "#136", 40, sk_trigger },
    { "write_skew_control",
      "two one-page DBs; T2 writes and commits before T1 commits",
      2, 2, 2, 0, NULL, 5, sk_control },
    { "write_skew_late",
      "two one-page DBs; T2 writes after T1's commit returned",
      2, 2, 2, 0, NULL, 5, sk_late },
    { "write_skew_samebtree_control",
      "two records on DIFFERENT pages of ONE btree; control timing",
      2, 2, 2, 0, NULL, 5, sk_samebtree_control },
    { "write_skew_samebtree_trigger",
      "two records on DIFFERENT pages of ONE btree; trigger timing",
      2, 2, 2, 1, "#136", 40, sk_samebtree_trigger },
    { "g2_antidep",
      "G2-item: both txns scan for markers, both insert one",
      1, 1, 2, 0, NULL, 1, g2_antidep },
    { "read_only_anomaly",
      "Fekete 3-txn: read-only txn observes a non-serializable state",
      2, 4, 3, 0, NULL, 1, read_only_anomaly },
    { "lost_update",
      "both txns read the counter and write read+1",
      1, 1, 2, 0, NULL, 1, lost_update },
    { "read_your_writes",
      "sanity: a txn must observe its own uncommitted write",
      1, 2, 1, 0, NULL, 1, read_your_writes },
};
#define	NSCENARIOS ((int)(sizeof(scenarios) / sizeof(scenarios[0])))

static void
iso_print_state(const iso_scenario *sc, const char *tag, const iso_state *s)
{
	int i;

	printf("    %s: db[", tag);
	for (i = 0; i < sc->ndbkeys; i++)
		printf("%s%d", i ? "," : "", s->v[i]);
	printf("]");
	if (sc->nslots > sc->ndbkeys) {
		printf(" obs[");
		for (i = sc->ndbkeys; i < sc->nslots; i++)
			printf("%s%d", i > sc->ndbkeys ? "," : "", s->v[i]);
		printf("]");
	}
	printf("\n");
}

/*
 * run_one_attempt --
 *	Run the schedule once.  Returns 1 if the resulting history IS
 *	serializable, 0 if it is not, -1 if the shape could not be set up.
 *	Prints the schedule when `report' is set (or always, when the history
 *	is not serializable -- that is the evidence).
 */
static int
run_one_attempt(iso_scenario *sc, int attempt, int report)
{
	iso_state st[2];			/* [0]=initial, [1]=actual */
	iso_txn txns[ISO_MAX_TXN];
	int i, ncommitted, order[ISO_MAX_TXN], serializable, skipped;

	memset(txns, 0, sizeof(txns));
	memset(st, 0, sizeof(st));

	if ((skipped = sc->run(sc, st, txns)) != 0)
		return (-1);

	serializable = iso_serializable(sc, &st[0], &st[1], txns, order,
	    &ncommitted);

	if (!report && serializable)
		return (1);

	if (sc->attempts > 1)
		printf("    attempt %d/%d:\n", attempt + 1, sc->attempts);
	for (i = 0; i < sc->ntxn; i++)
		printf("    %-5s %s\n", txns[i].name, txns[i].log);
	iso_print_state(sc, "initial ", &st[0]);
	iso_print_state(sc, "observed", &st[1]);
	printf("    committed txns = %d/%d, serial order found = %s",
	    ncommitted, sc->ntxn, serializable ? "yes (" : "NO");
	if (serializable) {
		for (i = 0; i < ncommitted; i++)
			printf("%s%s", i ? " < " : "", txns[order[i]].name);
		printf(")");
	}
	printf("\n");
	return (serializable);
}

/*
 * run_scenario --
 *	Returns 0 if the outcome matched the expectation, 1 if not.
 *
 *	A scenario PASSES only if EVERY attempt produced a serializable
 *	history; one violation in any attempt is a reproduction.  That
 *	asymmetry is deliberate: a serializability violation is a real
 *	counterexample, whereas a single clean run of a racy schedule proves
 *	nothing.
 */
static int
run_scenario(iso_scenario *sc)
{
	int a, ok, r, violations;

	printf("== %s ==\n    shape: %s\n", sc->name, sc->shape);
	if (sc->attempts > 1)
		printf("    %d attempts (the interleaving is racy; any single "
		    "violation is a reproduction)\n", sc->attempts);

	for (a = violations = 0; a < sc->attempts; a++) {
		/* Report the first attempt, plus every violation. */
		r = run_one_attempt(sc, a, a == 0);
		if (r < 0) {
			printf("    SKIP: scenario shape could not be "
			    "established\n\n");
			return (0);
		}
		if (r == 0) {
			violations++;
			/*
			 * One counterexample is enough for an expect_fail
			 * scenario; keep going otherwise so the report shows
			 * how reproducible a surprise violation is.
			 */
			if (sc->expect_fail) {
				a++;
				break;
			}
		}
	}
	if (sc->attempts > 1)
		printf("    %d/%d attempt(s) produced a non-serializable "
		    "history\n", violations, a);

	ok = (violations == 0);
	if (ok == sc->expect_fail) {
		/* Outcome disagrees with the recorded expectation. */
		if (sc->expect_fail)
			printf("    UNEXPECTED PASS: %s is marked as "
			    "reproducing %s but no attempt produced a "
			    "non-serializable history -- the issue looks "
			    "FIXED; clear expect_fail for this scenario.\n\n",
			    sc->name, sc->issue);
		else
			printf("    FAIL: no serial order of the committed "
			    "transactions produces the observed state -- "
			    "this history is NOT serializable.\n\n");
		return (1);
	}
	if (sc->expect_fail)
		printf("    XFAIL (reproduces %s): the committed history is "
		    "not serializable, as the issue reports.\n\n", sc->issue);
	else
		printf("    PASS\n\n");
	return (0);
}

int
main(int argc, char **argv)
{
	int failures, i, j, ran;

	if (getenv("ISO_VERBOSE") != NULL)
		verbose = 1;
	if (argc == 2 && strcmp(argv[1], "--list") == 0) {
		for (i = 0; i < NSCENARIOS; i++)
			printf("%s%s\n", scenarios[i].name,
			    scenarios[i].expect_fail ? "\t(expect-fail)" : "");
		return (0);
	}

	setvbuf(stdout, NULL, _IOLBF, 0);
	printf("%s\n\n", db_version(NULL, NULL, NULL));

	failures = ran = 0;
	if (argc == 1) {
		for (i = 0; i < NSCENARIOS; i++, ran++)
			failures += run_scenario(&scenarios[i]);
	} else {
		for (j = 1; j < argc; j++) {
			for (i = 0; i < NSCENARIOS; i++)
				if (strcmp(argv[j], scenarios[i].name) == 0)
					break;
			if (i == NSCENARIOS) {
				fprintf(stderr, "unknown scenario: %s\n",
				    argv[j]);
				return (2);
			}
			failures += run_scenario(&scenarios[i]);
			ran++;
		}
	}

	printf("%d scenario(s) run, %d unexpected outcome(s)\n", ran, failures);
	return (failures != 0);
}

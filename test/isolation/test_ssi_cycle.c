/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * test/isolation/test_ssi_cycle.c --
 *	Adya-style dependency-cycle serializability checker for SSI.
 *
 * WHY THIS EXISTS (and why test_iso_anomaly is not enough)
 *	test_iso_anomaly is exact but only tractable for a handful of
 *	transactions and keys: it enumerates every permutation of the committed
 *	transactions and asks whether one reproduces the durable state.  That
 *	cannot answer the production question -- "does an anomaly EVER commit
 *	under a RANDOM workload, at HIGH thread counts, over a LONG run?" -- an
 *	anomaly that needs a particular 32-thread interleaving over millions of
 *	operations is invisible to a 2-4 transaction deterministic scenario.
 *
 *	This driver is the general oracle.  It does NOT enumerate permutations,
 *	so it scales.  Instead it records, for every COMMITTED transaction, its
 *	read set and write set and the exact VERSION it read for each key, then
 *	builds Adya's direct serialization graph (DSG) and requires it ACYCLIC.
 *	A cycle is a non-serializable history: a committed anomaly.  SSI's whole
 *	job is to keep this graph acyclic (it aborts the pivot of a dangerous
 *	rw-antidependency structure); this checker verifies the RESULT,
 *	independently of the mechanism that produced it.
 *
 * THE DEPENDENCY GRAPH (Adya 1999)
 *	Nodes are committed transactions.  For each key K, the committed writers
 *	of K form a total order by the per-key version they installed
 *	(ww-order).  Then:
 *	  - ww edge  Ti -> Tj : Ti and Tj both write K and Ti's version < Tj's.
 *	  - wr edge  Ti -> Tj : Tj READ the version of K that Ti wrote.
 *	  - rw edge  Ti -> Tj : Ti read version vx of K, Tj wrote a version
 *	                        vy of K with vy > vx and Tj committed
 *	                        (Ti's read is "antidependent" on Tj's write).
 *	A history is serializable iff this graph has no cycle (Adya's theorem
 *	for the no-predicate case; our reads are all key reads, no range
 *	predicates, so item-dependencies suffice).
 *
 * HOW A READER NAMES THE VERSION IT SAW
 *	The value stored for key K is a STAMP: the global commit-sequence number
 *	of the transaction that wrote it (0 = the initial load).  A reader
 *	records read(K)=stamp; the writer whose commit-seq equals that stamp is
 *	the version's producer.  Commit-seq is assigned atomically at the moment
 *	a transaction's DB_TXN->commit returns 0, so it is a real serialization
 *	witness, not a wall-clock guess.  Per-key version order is the order of
 *	those commit-seqs among writers of K.
 *
 * ANTI-VACUITY (the teeth)
 *	The identical workload run at ISO_LEVEL=snapshot (plain SI) MUST find a
 *	cycle -- plain SI admits write skew and the read-only anomaly, so a
 *	driver that is actually creating conflicting transactions WILL produce a
 *	non-serializable history under SI.  If the SI run finds NO cycle, the
 *	workload is not exercising conflicts and the serializable PASS is
 *	vacuous; the driver reports that as a failure.  So one invocation proves
 *	both directions: SI dirty, SSI clean.
 *
 * KNOBS (environment)
 *	ISO_LEVEL      serializable (default) | snapshot
 *	CYC_SEEDS      number of random seeds to run (default 4)
 *	CYC_THREADS    worker threads per seed (default 8)
 *	CYC_SECS       measured seconds per seed (default 2)
 *	CYC_KEYS       key-space size (default 64)
 *	CYC_RSET       max reads per txn (default 4)
 *	CYC_WSET       max writes per txn (default 2)
 *	CYC_AM         btree (default) | hash
 *	CYC_PAGESIZE   db page size, 0 = default (default 0)
 *	CYC_PROCS      run as N processes instead of threads (default 0 = threads)
 *	CYC_MAXTXN     cap on recorded committed txns per seed (default 400000)
 *	CYC_VERBOSE    1 = print per-seed detail
 *
 * EXIT
 *	0  every seed serializable under SSI, AND the SI anti-vacuity run found
 *	   a cycle (the workload has teeth).
 *	1  a cycle under SSI (a committed anomaly -- the counterexample is
 *	   printed), or the SI run found NO cycle (vacuous workload).
 *	2  harness error.
 */
#include <errno.h>
#include <pthread.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "db.h"
#include "iso_knobs.h"

/* ------------------------------------------------------------------ knobs */
static u_int32_t iso_level = DB_TXN_SERIALIZABLE;
static int	cyc_seeds = 4;
static int	cyc_threads = 8;
static int	cyc_secs = 2;
static int	cyc_keys = 64;
static int	cyc_rset = 4;
static int	cyc_wset = 2;
static int	cyc_pagesize = 0;
static int	cyc_procs = 0;
static long	cyc_maxtxn = 400000;
static int	cyc_verbose = 0;
static DBTYPE	cyc_am = DB_BTREE;
static char	cyc_home[512];

static void iso_die(const char *, int) __attribute__((noreturn));
static void
iso_die(const char *what, int rc)
{
	fprintf(stderr, "harness error: %s: %s (%d)\n",
	    what, db_strerror(rc), rc);
	exit(2);
}

/* ------------------------------------------------------- committed records
 *
 * One record per committed transaction.  Reads/writes are stored as flat
 * arrays of (key, stamp).  `commit_seq' is this txn's own stamp (what it
 * wrote into every key it wrote).  Workers fill a shared array under a mutex;
 * in multi-process mode the array lives in a shared mmap (see below).
 */
#define	CYC_MAXOPS	16		/* per txn, >= rset+wset */

typedef struct {
	int	key;
	uint64_t stamp;			/* version read / written */
} cyc_op;

typedef struct {
	uint64_t commit_seq;		/* this txn's version stamp */
	int	nread, nwrite;
	cyc_op	reads[CYC_MAXOPS];
	cyc_op	writes[CYC_MAXOPS];
} cyc_txn;

/*
 * Shared state.  In thread mode these are plain globals.  In process mode the
 * whole block is one anonymous shared mapping so every process appends to the
 * same log and increments the same commit counter.
 */
typedef struct {
	atomic_ullong	commit_seq;	/* next stamp; 0 reserved for load */
	atomic_long	ntxn;		/* committed txns recorded */
	long		cap;		/* capacity of txns[] */
	pthread_mutex_t	lock;		/* guards the append */
	cyc_txn		txns[1];	/* [cap], flexible */
} cyc_log;

static cyc_log	*g_log;
static DB_ENV	*env;
static DB	*db;

static size_t
cyc_log_bytes(long cap)
{
	return (sizeof(cyc_log) + (size_t)(cap - 1) * sizeof(cyc_txn));
}

static void
cyc_log_init(cyc_log *l, long cap, int shared)
{
	pthread_mutexattr_t a;

	atomic_store(&l->commit_seq, 1);	/* 0 = initial load */
	atomic_store(&l->ntxn, 0);
	l->cap = cap;
	(void)pthread_mutexattr_init(&a);
	if (shared)
		(void)pthread_mutexattr_setpshared(&a, PTHREAD_PROCESS_SHARED);
	(void)pthread_mutex_init(&l->lock, &a);
	(void)pthread_mutexattr_destroy(&a);
}

/* ------------------------------------------------------------------- db io */
static int
cyc_read(DB_TXN *txn, int key, uint64_t *stamp)
{
	DBT k, d;
	char kb[16];
	uint64_t v;
	int rc;

	(void)snprintf(kb, sizeof(kb), "k%08d", key);
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = kb;
	k.size = (u_int32_t)strlen(kb);
	d.data = &v;
	d.ulen = sizeof(v);
	d.flags = DB_DBT_USERMEM;
	if ((rc = db->get(db, txn, &k, &d, 0)) != 0)
		return (rc);
	*stamp = v;
	return (0);
}

static int
cyc_write(DB_TXN *txn, int key, uint64_t stamp)
{
	DBT k, d;
	char kb[16];

	(void)snprintf(kb, sizeof(kb), "k%08d", key);
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = kb;
	k.size = (u_int32_t)strlen(kb);
	d.data = &stamp;
	d.size = sizeof(stamp);
	return (db->put(db, txn, &k, &d, 0));
}

/* ------------------------------------------------------------- the worker
 *
 * A worker runs transactions until the deadline.  Each transaction:
 *   - reads cyc_rset random keys (recording the version stamp seen),
 *   - writes cyc_wset random keys with ITS OWN not-yet-known commit stamp
 *     (we reserve the stamp AFTER a clean commit, so a write stores a
 *     placeholder and we patch the record; see below).
 * On commit success the txn is recorded with a freshly reserved commit_seq,
 * and the stamp it wrote is that commit_seq -- which means the stored value
 * must be the commit_seq.  We cannot know commit_seq before committing, so we
 * store a per-worker monotonic local stamp during the txn and reconcile: the
 * recorded wr/ww edges use the GLOBAL commit order, and the stored value is a
 * globally-unique (worker<<40 | local) tag that identifies the writing txn
 * uniquely.  A reader records that tag; the checker maps tag->committed-txn.
 */
struct worker_arg {
	unsigned seed;
	int	 id;
	volatile atomic_int *stop;
};

/* A globally-unique per-write tag: identifies the producing txn precisely. */
static uint64_t
cyc_make_tag(int worker, uint64_t local)
{
	return (((uint64_t)worker << 40) | (local & 0xffffffffffULL));
}

static void *
cyc_worker(void *vp)
{
	struct worker_arg *wa = vp;
	DB_TXN *txn;
	cyc_txn rec;
	uint64_t local = 0, tag;
	unsigned seed = wa->seed ^ (unsigned)(wa->id * 2654435761u);
	int i, k, nr, nw, rc, op_rc;
	long slot;

	while (!atomic_load(wa->stop)) {
		memset(&rec, 0, sizeof(rec));
		nr = 1 + (int)(rand_r(&seed) % (unsigned)cyc_rset);
		nw = 1 + (int)(rand_r(&seed) % (unsigned)cyc_wset);
		if (nr > CYC_MAXOPS) nr = CYC_MAXOPS;
		if (nw > CYC_MAXOPS) nw = CYC_MAXOPS;
		tag = cyc_make_tag(wa->id, ++local);

		if ((rc = env->txn_begin(env, NULL, &txn, iso_level)) != 0) {
			/* Region pressure etc.: back off, retry. */
			if (rc == ENOMEM || rc == EAGAIN) { sched_yield(); continue; }
			iso_die("txn_begin", rc);
		}
		op_rc = 0;
		/* Reads first (SSI's rw edges come from read-before-write). */
		for (i = 0; i < nr && op_rc == 0; i++) {
			k = (int)(rand_r(&seed) % (unsigned)cyc_keys);
			rc = cyc_read(txn, k, &rec.reads[rec.nread].stamp);
			if (rc == 0) {
				rec.reads[rec.nread].key = k;
				rec.nread++;
			} else if (rc == DB_SNAPSHOT_CONFLICT ||
			    rc == DB_SNAPSHOT_UNSAFE ||
			    rc == DB_LOCK_DEADLOCK ||
			    rc == DB_LOCK_NOTGRANTED) {
				op_rc = rc;
			} else
				iso_die("read", rc);
		}
		for (i = 0; i < nw && op_rc == 0; i++) {
			k = (int)(rand_r(&seed) % (unsigned)cyc_keys);
			rc = cyc_write(txn, k, tag);
			if (rc == 0) {
				rec.writes[rec.nwrite].key = k;
				rec.writes[rec.nwrite].stamp = tag;
				rec.nwrite++;
			} else if (rc == DB_SNAPSHOT_CONFLICT ||
			    rc == DB_SNAPSHOT_UNSAFE ||
			    rc == DB_LOCK_DEADLOCK ||
			    rc == DB_LOCK_NOTGRANTED) {
				op_rc = rc;
			} else
				iso_die("write", rc);
		}
		if (op_rc != 0) {
			(void)txn->abort(txn);
			continue;
		}
		rc = txn->commit(txn, 0);
		if (rc != 0) {
			if (rc != DB_SNAPSHOT_CONFLICT &&
			    rc != DB_SNAPSHOT_UNSAFE &&
			    rc != DB_LOCK_DEADLOCK &&
			    rc != DB_LOCK_NOTGRANTED)
				iso_die("commit", rc);
			continue;		/* aborted at commit: not recorded */
		}
		/* Committed: reserve a global commit-seq and record. */
		rec.commit_seq = atomic_fetch_add(&g_log->commit_seq, 1);
		slot = atomic_fetch_add(&g_log->ntxn, 1);
		if (slot >= g_log->cap) {
			atomic_store(wa->stop, 1);	/* log full: stop */
			break;
		}
		g_log->txns[slot] = rec;
	}
	return (NULL);
}

/* ----------------------------------------------------------- the checker
 *
 * Build the DSG and look for a cycle.  Transactions are indexed by their slot
 * in the log.  We map each write tag -> producing txn index.  The stored
 * value a reader saw is a tag (or 0 = initial load); that tag names the writer.
 *
 * Per-key version order is the commit_seq order of the writers of that key.
 * A reader of version with tag T on key K is rw-antidependent on every writer
 * of K whose commit_seq > (producer of T)'s commit_seq.
 */
struct edgeset {
	int	*succ;			/* adjacency, flat */
	int	*off;			/* per-node offset into succ */
	int	*deg;
	int	 n;
};

/* tag -> txn index, linear (ntxn is bounded, tags unique). Build a hash. */
struct tagmap {
	uint64_t *tags;
	int	 *idx;
	int	  cap;
};

static int
tm_hash(const struct tagmap *tm, uint64_t t)
{
	return (int)((t * 1099511628211ULL) % (uint64_t)tm->cap);
}

static void
tm_put(struct tagmap *tm, uint64_t t, int i)
{
	int h = tm_hash(tm, t);

	while (tm->idx[h] != -1)
		h = (h + 1) % tm->cap;
	tm->tags[h] = t;
	tm->idx[h] = i;
}

static int
tm_get(const struct tagmap *tm, uint64_t t)
{
	int h = tm_hash(tm, t);

	while (tm->idx[h] != -1) {
		if (tm->tags[h] == t)
			return (tm->idx[h]);
		h = (h + 1) % tm->cap;
	}
	return (-1);			/* initial load, or unknown */
}

/*
 * Cycle detection by ITERATIVE DFS (an explicit stack, so a 400k-node graph
 * cannot overflow the C stack).  n transactions, m edges, edges sorted by
 * source with start[] the per-node offset.  color: 0 white, 1 gray (on the
 * DFS stack), 2 black.  On a back-edge to a gray node we have a cycle; we
 * reconstruct it from the gray path held in `path`.  Returns 1 and fills
 * `cyc' (length *cyclen) if a cycle exists.
 */
struct edge { int s, d; };

static int edge_cmp(const void *a, const void *b)
{
	const struct edge *x = a, *y = b;
	if (x->s != y->s) return (x->s - y->s);
	return (x->d - y->d);
}

/*
 * iter_cycle --
 *	Explicit-stack DFS from every unvisited node.  `it[u]' is the next
 *	out-edge index to try for node u; `path' is the current gray chain
 *	(node ids) and `pos[u]' its index in `path' when u is gray.
 */
static int
iter_cycle(long n, struct edge *E, int m, int *start, char *color,
    int *it, int *path, int *pos, int *cyc, int *cyclen)
{
	long root;
	int top, u, v, i;

	for (root = 0; root < n; root++) {
		if (color[root] != 0)
			continue;
		top = 0;
		path[top] = (int)root;
		pos[root] = top;
		color[root] = 1;
		it[root] = start[(int)root];
		while (top >= 0) {
			u = path[top];
			i = it[u];
			if (i < m && E[i].s == u) {
				it[u] = i + 1;
				v = E[i].d;
				if (color[v] == 1) {
					/* back-edge: cycle is path[pos[v]..top] */
					int j, k = 0;
					for (j = pos[v]; j <= top; j++)
						cyc[k++] = path[j];
					*cyclen = k;
					return (1);
				}
				if (color[v] == 0) {
					color[v] = 1;
					it[v] = start[v];
					path[++top] = v;
					pos[v] = top;
				}
			} else {
				color[u] = 2;
				top--;
			}
		}
	}
	return (0);
}

/*
 * cyc_check --
 *	Build the DSG from g_log and return 1 if serializable (acyclic), 0 if a
 *	cycle exists.  On a cycle, prints the offending transactions.
 */
static int
cyc_check(long n, const char *label)
{
	struct tagmap tm;
	struct edge *E = NULL;
	int *start = NULL, *stack = NULL, *color = NULL, *cyc = NULL;
	long i;
	int cap, cyclen, j, serializable = 1, m = 0, mmax;

	if (n <= 0)
		return (1);

	/* 1. tag -> index map (initial load tag 0 is not in it). */
	cap = 1;
	while (cap < n * 2)
		cap <<= 1;
	tm.cap = cap;
	tm.tags = calloc((size_t)cap, sizeof(uint64_t));
	tm.idx = malloc((size_t)cap * sizeof(int));
	for (j = 0; j < cap; j++)
		tm.idx[j] = -1;
	for (i = 0; i < n; i++) {
		cyc_txn *t = &g_log->txns[i];
		for (j = 0; j < t->nwrite; j++)
			tm_put(&tm, t->writes[j].stamp, (int)i);
	}

	/*
	 * 2. Per-key writer lists in commit_seq order.  Build (key -> list of
	 * (commit_seq, txn)).  cyc_keys is small; use an array of dynamic lists.
	 */
	{
		typedef struct { uint64_t seq; int txn; } wver;
		wver **wl = calloc((size_t)cyc_keys, sizeof(wver *));
		int *wn = calloc((size_t)cyc_keys, sizeof(int));
		int *wc = calloc((size_t)cyc_keys, sizeof(int));

		for (i = 0; i < n; i++) {
			cyc_txn *t = &g_log->txns[i];
			for (j = 0; j < t->nwrite; j++) {
				int key = t->writes[j].key;
				if (wn[key] == wc[key]) {
					wc[key] = wc[key] ? wc[key] * 2 : 8;
					wl[key] = realloc(wl[key],
					    (size_t)wc[key] * sizeof(wver));
				}
				wl[key][wn[key]].seq = t->commit_seq;
				wl[key][wn[key]].txn = (int)i;
				wn[key]++;
			}
		}
		/* Sort each key's writers by commit_seq. */
		for (j = 0; j < cyc_keys; j++) {
			int a, b;
			for (a = 1; a < wn[j]; a++)	/* insertion sort */
				for (b = a; b > 0 &&
				    wl[j][b-1].seq > wl[j][b].seq; b--) {
					wver tmp = wl[j][b-1];
					wl[j][b-1] = wl[j][b];
					wl[j][b] = tmp;
				}
		}

		/* 3. Edges.  Over-allocate: each txn contributes bounded edges. */
		mmax = (int)(n * (CYC_MAXOPS * 4 + 4)) + 16;
		E = malloc((size_t)mmax * sizeof(struct edge));

#define	ADD_EDGE(S,D) do { if ((S) != (D) && m < mmax) { \
	E[m].s = (S); E[m].d = (D); m++; } } while (0)

		/* ww edges: consecutive writers of a key. */
		for (j = 0; j < cyc_keys; j++)
			for (i = 1; i < wn[j]; i++)
				ADD_EDGE(wl[j][i-1].txn, wl[j][i].txn);

		/* wr and rw edges, from each txn's reads. */
		for (i = 0; i < n; i++) {
			cyc_txn *t = &g_log->txns[i];
			for (j = 0; j < t->nread; j++) {
				int key = t->reads[j].key;
				uint64_t rtag = t->reads[j].stamp;
				int producer = tm_get(&tm, rtag); /* -1=load */
				uint64_t pseq = 0;
				int w;

				/* wr edge producer -> reader. */
				if (producer >= 0) {
					pseq = g_log->txns[producer].commit_seq;
					ADD_EDGE(producer, (int)i);
				}
				/*
				 * rw edge: reader -> every writer of this key
				 * whose version is NEWER than the one read.
				 * "Newer" = larger commit_seq than the
				 * producer's (or any writer at all, if the read
				 * saw the initial load).
				 */
				for (w = 0; w < wn[key]; w++) {
					if (wl[key][w].txn == (int)i)
						continue;
					if (producer < 0 ||
					    wl[key][w].seq > pseq)
						ADD_EDGE((int)i, wl[key][w].txn);
				}
			}
		}
#undef ADD_EDGE

		for (j = 0; j < cyc_keys; j++)
			free(wl[j]);
		free(wl); free(wn); free(wc);
	}

	/* 4. Cycle detection (iterative DFS). */
	qsort(E, (size_t)m, sizeof(struct edge), edge_cmp);
	start = malloc((size_t)(n + 1) * sizeof(int));
	for (i = 0; i <= n; i++)
		start[i] = m;
	for (j = m - 1; j >= 0; j--)
		start[E[j].s] = j;
	color = calloc((size_t)n, 1);
	stack = malloc((size_t)n * sizeof(int));  /* path */
	cyc = malloc((size_t)n * sizeof(int));
	{
		int *it = malloc((size_t)n * sizeof(int));
		int *pos = malloc((size_t)n * sizeof(int));
		if (iter_cycle(n, E, m, start, color, it, stack, pos,
		    cyc, &cyclen)) {
			serializable = 0;
			printf("    [%s] NON-SERIALIZABLE: "
			    "dependency cycle of %d txns:\n",
			    label, cyclen);
			for (j = 0; j < cyclen; j++) {
				cyc_txn *t = &g_log->txns[cyc[j]];
				printf("      txn#%d seq=%llu r=%d w=%d\n",
				    cyc[j],
				    (unsigned long long)t->commit_seq,
				    t->nread, t->nwrite);
			}
		}
		free(it); free(pos);
	}

	free(tm.tags); free(tm.idx); free(E); free(start);
	free(color); free(stack); free(cyc);
	return (serializable);
}

/* ------------------------------------------------------------- env / run */
static void
cyc_rmtree(const char *dir)
{
	char cmd[600];
	(void)snprintf(cmd, sizeof(cmd),
	    "find '%s' -mindepth 1 -delete 2>/dev/null", dir);
	(void)system(cmd);
}

static void
cyc_env_open(u_int32_t create)
{
	int rc;

	if ((rc = db_env_create(&env, 0)) != 0)
		iso_die("db_env_create", rc);
	if ((rc = iso_set_partitions(env)) != 0)
		iso_die("set_lk_partitions", rc);
	if ((rc = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		iso_die("set_lk_detect", rc);
	if ((rc = env->set_timeout(env, 2000000, DB_SET_LOCK_TIMEOUT)) != 0)
		iso_die("set_timeout", rc);
	/* Plenty of locks: high thread counts take many SIREAD markers. */
	(void)env->set_lk_max_locks(env, 200000);
	(void)env->set_lk_max_objects(env, 200000);
	(void)env->set_lk_max_lockers(env, 200000);
	if ((rc = env->set_cachesize(env, 0, 64 * 1024 * 1024, 1)) != 0)
		iso_die("set_cachesize", rc);
	if ((rc = env->open(env, cyc_home, create | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0600)) != 0)
		iso_die("DB_ENV->open", rc);

	if ((rc = db_create(&db, env, 0)) != 0)
		iso_die("db_create", rc);
	if (cyc_pagesize != 0 &&
	    (rc = db->set_pagesize(db, (u_int32_t)cyc_pagesize)) != 0)
		iso_die("set_pagesize", rc);
	if ((rc = db->open(db, NULL, "cycle.db", NULL, cyc_am,
	    create | DB_MULTIVERSION | DB_AUTO_COMMIT | DB_THREAD, 0600)) != 0)
		iso_die("DB->open", rc);
}

static void
cyc_env_close(void)
{
	int rc;
	if ((rc = db->close(db, 0)) != 0)
		iso_die("DB->close", rc);
	if ((rc = env->close(env, 0)) != 0)
		iso_die("DB_ENV->close", rc);
	env = NULL; db = NULL;
}

static void
cyc_load(void)
{
	int k, rc;
	for (k = 0; k < cyc_keys; k++)
		if ((rc = cyc_write(NULL, k, 0)) != 0)	/* stamp 0 = load */
			iso_die("initial load", rc);
}

/*
 * Run one seed: spawn workers (threads or processes), run for cyc_secs, join,
 * then check.  Returns 1 if serializable.
 */
static int
run_seed(unsigned seed, int *found_cycle_out)
{
	atomic_int *stop;
	long n;
	int i, serializable;

	/* stop flag must be shared across processes too. */
	stop = mmap(NULL, sizeof(atomic_int), PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANONYMOUS, -1, 0);
	if (stop == MAP_FAILED)
		iso_die("mmap stop", errno);
	atomic_store(stop, 0);

	atomic_store(&g_log->ntxn, 0);
	atomic_store(&g_log->commit_seq, 1);

	cyc_env_open(DB_CREATE);
	cyc_load();

	if (cyc_procs > 0) {
		pid_t *kids = malloc((size_t)cyc_procs * sizeof(pid_t));
		/* Reopen the env in each child (handles are not fork-safe). */
		cyc_env_close();
		for (i = 0; i < cyc_procs; i++) {
			pid_t p = fork();
			if (p == 0) {
				struct worker_arg wa;
				cyc_env_open(0);
				wa.seed = seed; wa.id = i; wa.stop = stop;
				/* a process runs several logical workers */
				cyc_worker(&wa);
				cyc_env_close();
				_exit(0);
			}
			kids[i] = p;
		}
		/* Parent is the timer. */
		{
			struct timespec ts = { cyc_secs, 0 };
			nanosleep(&ts, NULL);
			atomic_store(stop, 1);
		}
		for (i = 0; i < cyc_procs; i++) {
			int st;
			(void)waitpid(kids[i], &st, 0);
		}
		free(kids);
		cyc_env_open(0);		/* reopen for the read-back/close */
	} else {
		pthread_t *th = malloc((size_t)cyc_threads * sizeof(pthread_t));
		struct worker_arg *wa =
		    malloc((size_t)cyc_threads * sizeof(*wa));
		for (i = 0; i < cyc_threads; i++) {
			wa[i].seed = seed; wa[i].id = i; wa[i].stop = stop;
			if (pthread_create(&th[i], NULL, cyc_worker, &wa[i]))
				iso_die("pthread_create", errno);
		}
		{
			struct timespec ts = { cyc_secs, 0 };
			nanosleep(&ts, NULL);
			atomic_store(stop, 1);
		}
		for (i = 0; i < cyc_threads; i++)
			(void)pthread_join(th[i], NULL);
		free(th); free(wa);
	}

	n = atomic_load(&g_log->ntxn);
	if (n > g_log->cap)
		n = g_log->cap;
	cyc_env_close();
	(void)munmap(stop, sizeof(atomic_int));

	serializable = cyc_check(n,
	    iso_level == DB_TXN_SERIALIZABLE ? "SSI" : "SI");
	if (cyc_verbose || !serializable)
		printf("    seed=%u committed=%ld serializable=%s\n",
		    seed, n, serializable ? "yes" : "NO");
	*found_cycle_out = !serializable;
	return (serializable);
}

static int
geti(const char *name, int dflt)
{
	const char *s = getenv(name);
	return (s != NULL && *s != '\0') ? atoi(s) : dflt;
}

int
main(void)
{
	const char *lvl, *am;
	int s, serializable_all = 1, any_cycle = 0, cycle;
	long cap;

	if ((lvl = getenv("ISO_LEVEL")) != NULL) {
		if (strcmp(lvl, "snapshot") == 0)
			iso_level = DB_TXN_SNAPSHOT;
		else if (strcmp(lvl, "serializable") == 0)
			iso_level = DB_TXN_SERIALIZABLE;
		else { fprintf(stderr, "bad ISO_LEVEL\n"); return (2); }
	}
	cyc_seeds = geti("CYC_SEEDS", cyc_seeds);
	cyc_threads = geti("CYC_THREADS", cyc_threads);
	cyc_secs = geti("CYC_SECS", cyc_secs);
	cyc_keys = geti("CYC_KEYS", cyc_keys);
	cyc_rset = geti("CYC_RSET", cyc_rset);
	cyc_wset = geti("CYC_WSET", cyc_wset);
	cyc_pagesize = geti("CYC_PAGESIZE", cyc_pagesize);
	cyc_procs = geti("CYC_PROCS", cyc_procs);
	cyc_maxtxn = geti("CYC_MAXTXN", (int)cyc_maxtxn);
	cyc_verbose = geti("CYC_VERBOSE", 0);
	if ((am = getenv("CYC_AM")) != NULL && strcmp(am, "hash") == 0)
		cyc_am = DB_HASH;
	if (cyc_rset > CYC_MAXOPS) cyc_rset = CYC_MAXOPS;
	if (cyc_wset > CYC_MAXOPS) cyc_wset = CYC_MAXOPS;

	setvbuf(stdout, NULL, _IOLBF, 0);
	iso_watchdog("test_ssi_cycle",
	    (unsigned)(cyc_secs * cyc_seeds + 120));

	(void)snprintf(cyc_home, sizeof(cyc_home), "CYCDIR");
	(void)mkdir(cyc_home, 0755);
	cyc_rmtree(cyc_home);

	/* Shared log: mmap so process mode works; threads use it too. */
	cap = cyc_maxtxn;
	g_log = mmap(NULL, cyc_log_bytes(cap), PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANONYMOUS, -1, 0);
	if (g_log == MAP_FAILED)
		iso_die("mmap log", errno);
	cyc_log_init(g_log, cap, cyc_procs > 0);

	printf("%s\n", db_version(NULL, NULL, NULL));
	printf("test_ssi_cycle: level=%s am=%s seeds=%d %s=%d secs=%d keys=%d "
	    "rset=%d wset=%d pagesize=%d maxtxn=%ld\n",
	    iso_level == DB_TXN_SERIALIZABLE ? "SERIALIZABLE" : "SNAPSHOT",
	    cyc_am == DB_HASH ? "hash" : "btree", cyc_seeds,
	    cyc_procs > 0 ? "procs" : "threads",
	    cyc_procs > 0 ? cyc_procs : cyc_threads,
	    cyc_secs, cyc_keys, cyc_rset, cyc_wset, cyc_pagesize, cyc_maxtxn);

	for (s = 0; s < cyc_seeds; s++) {
		unsigned seed = 0x9e3779b9u * (unsigned)(s + 1);
		cyc_rmtree(cyc_home);
		serializable_all &= run_seed(seed, &cycle);
		any_cycle |= cycle;
	}

	cyc_rmtree(cyc_home);
	(void)munmap(g_log, cyc_log_bytes(cap));

	if (iso_level == DB_TXN_SERIALIZABLE) {
		if (!serializable_all) {
			printf("FAIL: a committed history under SSI is NOT "
			    "serializable (cycle above)\n");
			return (1);
		}
		printf("PASS: every seed serializable under SSI\n");
		return (0);
	} else {
		/* Anti-vacuity: SI MUST admit a cycle or the workload is inert. */
		if (!any_cycle) {
			printf("FAIL (anti-vacuity): plain SI found NO cycle -- "
			    "the workload is not exercising conflicts, so an "
			    "SSI pass would be vacuous.  Raise CYC_THREADS / "
			    "lower CYC_KEYS.\n");
			return (1);
		}
		printf("PASS (anti-vacuity): plain SI admits a cycle, as it "
		    "must -- the workload has teeth.\n");
		return (0);
	}
}

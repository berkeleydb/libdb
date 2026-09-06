/*-
 * test/soak/test_soak_resources.c --
 *	Tier B2: long-running resource-accounting soak.
 *
 * The blind spot this closes: a slot/mutex/locker leak that only manifests
 * after thousands of SEQUENTIAL transactions.  Nothing crashes, no page is
 * corrupt, no sanitizer fires -- the region just fills up until some later
 * API call returns ENOMEM.  Crash/durability and memory-safety tiers cannot
 * see that shape.
 *
 * Method: open ONE long-lived environment, run N (>= 2000) sequential
 * transactions of a given workload, sampling the PUBLIC stat APIs
 * (DB_ENV->mutex_stat, lock_stat, txn_stat, memp_stat) at intervals.  A
 * healthy workload returns each counter to (or near) its post-warmup
 * baseline; a leaking workload grows it monotonically.  The verdict is a
 * least-squares slope over the samples taken AFTER warmup, expressed in
 * units per 1000 transactions, compared against a per-counter tolerance.
 * The growth curve is always printed so a regression is diagnosable from
 * CI logs alone.
 *
 * Warmup matters: the first transactions legitimately grow the region
 * (lockers get allocated, cache pages get faulted in, the free lists fill).
 * Only the steady state is asserted, so the tier does not fight normal
 * lazy allocation.
 *
 * Usage:
 *	./test_soak_resources                    every workload, default N
 *	./test_soak_resources -n 5000            N transactions per workload
 *	./test_soak_resources WORKLOAD ...       only the named workloads
 *	./test_soak_resources --list             list workload names
 *
 * Exit status: 0 = every workload matched its expectation, 1 = a workload
 * did not (unexpected growth, or an expected-leak workload that stayed flat
 * -- i.e. the leak got fixed and the expectation needs updating), 2 =
 * harness error.
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

#define	SOAK_MAX_SAMPLE	64

/*
 * The counters we track.  All come from the public stat APIs.  Each is a
 * region resource that a leak would consume without returning.
 */
enum soak_counter {
	C_MUTEX_INUSE,		/* mutex_stat: st_mutex_inuse */
	C_LOCK_LOCKERS,		/* lock_stat:  st_nlockers */
	C_LOCK_LOCKS,		/* lock_stat:  st_nlocks */
	C_LOCK_OBJECTS,		/* lock_stat:  st_nobjects */
	C_TXN_ACTIVE,		/* txn_stat:   st_nactive */
	C_TXN_SNAPSHOT,		/* txn_stat:   st_nsnapshot */
	C_MPOOL_DIRTY,		/* memp_stat:  st_page_dirty */
	C_NCOUNTER
};

static const struct {
	const char *name;
	/*
	 * Tolerated steady-state growth, in counter units per 1000
	 * transactions.  Zero would be ideal but is too brittle: the mutex
	 * and lock regions legitimately wobble by a slot or two as free
	 * lists are recycled, and MVCC page retention is asynchronous.  A
	 * genuine per-transaction leak grows by ~1000 units per 1000 txns,
	 * three orders of magnitude above these tolerances.
	 */
	double tolerance;
} counters[C_NCOUNTER] = {
	{ "mutex_inuse",	20.0 },
	{ "lock_lockers",	20.0 },
	{ "lock_locks",		20.0 },
	{ "lock_objects",	20.0 },
	{ "txn_active",		 2.0 },
	{ "txn_snapshot",	20.0 },
	{ "mpool_dirty",	50.0 },
};

typedef struct {
	long	 txns;
	double	 v[C_NCOUNTER];
} soak_sample;

struct soak_workload;
/*
 * A workload runs exactly one transaction and returns 0, or an rc the
 * workload considers a legitimate give-up (deadlock etc.).  Returning
 * anything else is a harness error.
 */
typedef int (*soak_txn_fn)(struct soak_workload *, long);

typedef struct soak_workload {
	const char *name;
	const char *shape;
	soak_txn_fn one;
	int	    expect_leak;	/* Known-broken on master. */
	const char *issue;
	/* Which counters this workload asserts on; 0 => all of them. */
	unsigned    mask;
} soak_workload;

static DB_ENV	*env;
static DB	*db;
static char	 soak_home[512];
static long	 soak_n = 2000;
static long	 soak_enomem_at = -1;	/* First txn that saw ENOMEM. */
static const char *soak_enomem_call;

static void soak_die(const char *, int) __attribute__((noreturn));

static void
soak_die(const char *what, int rc)
{
	fprintf(stderr, "harness error: %s: %s (%d)\n",
	    what, db_strerror(rc), rc);
	exit(2);
}

static int
soak_is_giveup(int rc)
{
	return (rc == DB_LOCK_DEADLOCK || rc == DB_LOCK_NOTGRANTED ||
	    rc == DB_SNAPSHOT_CONFLICT || rc == DB_SNAPSHOT_UNSAFE);
}

/*
 * soak_resource_rc --
 *	Is this rc the resource exhaustion we are hunting?  ENOMEM from a
 *	libdb region allocation is the #137/#138 failure mode.  It is NOT
 *	treated as a harness error: recording it and continuing gives a much
 *	better report ("ENOMEM at txn 1187") than dying does.
 */
static int
soak_resource_rc(int rc)
{
	return (rc == ENOMEM || rc == DB_RUNRECOVERY);
}

static void
soak_note_enomem(const char *call, long txn)
{
	if (soak_enomem_at < 0) {
		soak_enomem_at = txn;
		soak_enomem_call = call;
	}
}

static void
soak_rmtree(const char *dir)
{
	char cmd[600];

	(void)snprintf(cmd, sizeof(cmd),
	    "find '%s' -mindepth 1 -delete 2>/dev/null", dir);
	(void)system(cmd);
}

/*
 * soak_env_open --
 *	One long-lived environment.  The region sizes are deliberately left
 *	at their defaults: the point of the tier is that a correct engine
 *	does not need a bigger region to run 2000 sequential transactions,
 *	and a leak shows up as ENOMEM exactly because the default region is
 *	finite.
 */
static void
soak_env_open(const char *workload)
{
	int rc;

	(void)snprintf(soak_home, sizeof(soak_home), "SOAKDIR.%s", workload);
	(void)mkdir(soak_home, 0755);
	soak_rmtree(soak_home);

	if ((rc = db_env_create(&env, 0)) != 0)
		soak_die("db_env_create", rc);
	if ((rc = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		soak_die("set_lk_detect", rc);
	if ((rc = env->set_cachesize(env, 0, 8 * 1024 * 1024, 1)) != 0)
		soak_die("set_cachesize", rc);
	if ((rc = env->set_timeout(env, 2000000, DB_SET_LOCK_TIMEOUT)) != 0)
		soak_die("set_timeout", rc);
	if ((rc = env->open(env, soak_home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0600)) != 0)
		soak_die("DB_ENV->open", rc);

	if ((rc = db_create(&db, env, 0)) != 0)
		soak_die("db_create", rc);
	if ((rc = db->open(db, NULL, "soak.db", NULL, DB_BTREE, DB_CREATE |
	    DB_MULTIVERSION | DB_AUTO_COMMIT | DB_THREAD, 0600)) != 0)
		soak_die("DB->open", rc);
}

static void
soak_env_close(void)
{
	int rc;

	if ((rc = db->close(db, 0)) != 0)
		soak_die("DB->close", rc);
	if ((rc = env->close(env, 0)) != 0)
		soak_die("DB_ENV->close", rc);
	env = NULL;
	db = NULL;
}

/*
 * soak_sample_now --
 *	Read every counter through the public stat APIs.  DB_STAT_SUBSYSTEM
 *	is not used; each call is the plain documented one so the tier stays
 *	a legitimate API consumer.
 */
static void
soak_sample_now(soak_sample *s, long txns)
{
	DB_MUTEX_STAT *mst;
	DB_LOCK_STAT *lst;
	DB_TXN_STAT *tst;
	DB_MPOOL_STAT *gst;
	int rc;

	memset(s, 0, sizeof(*s));
	s->txns = txns;

	if ((rc = env->mutex_stat(env, &mst, 0)) != 0)
		soak_die("DB_ENV->mutex_stat", rc);
	s->v[C_MUTEX_INUSE] = mst->st_mutex_inuse;
	free(mst);

	if ((rc = env->lock_stat(env, &lst, 0)) != 0)
		soak_die("DB_ENV->lock_stat", rc);
	s->v[C_LOCK_LOCKERS] = lst->st_nlockers;
	s->v[C_LOCK_LOCKS] = lst->st_nlocks;
	s->v[C_LOCK_OBJECTS] = lst->st_nobjects;
	free(lst);

	if ((rc = env->txn_stat(env, &tst, 0)) != 0)
		soak_die("DB_ENV->txn_stat", rc);
	s->v[C_TXN_ACTIVE] = tst->st_nactive;
	s->v[C_TXN_SNAPSHOT] = tst->st_nsnapshot;
	free(tst);

	if ((rc = env->memp_stat(env, &gst, NULL, 0)) != 0)
		soak_die("DB_ENV->memp_stat", rc);
	s->v[C_MPOOL_DIRTY] = gst->st_page_dirty;
	free(gst);
}

/*
 * ---------------------------------------------------------------------------
 * Workloads.  Each runs ONE transaction; the driver repeats it.
 * ---------------------------------------------------------------------------
 */
/*
 * Keyspace width.  Wide enough that writes spread over many pages (so MVCC
 * versions accumulate and the cache turns over, which is what drives the
 * detail-reaping path), narrow enough that the working set stays in the
 * 8MB cache and no workload starts failing for cache reasons.
 */
#define	SOAK_NKEY	512

static int
soak_get(DB_TXN *txn, long i, int *out)
{
	DBT k, d;
	char key[32];
	int v, rc;

	(void)snprintf(key, sizeof(key), "k%06ld", i % SOAK_NKEY);
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = key;
	k.size = (u_int32_t)strlen(key);
	d.data = &v;
	d.ulen = sizeof(v);
	d.flags = DB_DBT_USERMEM;
	if ((rc = db->get(db, txn, &k, &d, 0)) != 0)
		return (rc);
	if (out != NULL)
		*out = v;
	return (0);
}

static int
soak_put(DB_TXN *txn, long i, int v)
{
	DBT k, d;
	char key[32];

	(void)snprintf(key, sizeof(key), "k%06ld", i % SOAK_NKEY);
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = key;
	k.size = (u_int32_t)strlen(key);
	d.data = &v;
	d.size = sizeof(v);
	return (db->put(db, txn, &k, &d, 0));
}

/*
 * ro_snapshot -- the #137 shape.
 *	A read-only DB_TXN_SNAPSHOT transaction, begun and committed with no
 *	write at all.  #137 reports that the SIREAD cleanup does not reclaim
 *	the committed reader's locker, so each such transaction consumes
 *	region resources permanently and txn_begin eventually returns ENOMEM.
 */
static int
wl_ro_snapshot(soak_workload *w, long i)
{
	DB_TXN *txn;
	int rc, v;

	(void)w;
	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT)) != 0) {
		if (soak_resource_rc(rc)) {
			soak_note_enomem("txn_begin", i);
			return (0);
		}
		return (rc);
	}
	if ((rc = soak_get(txn, i, &v)) != 0 && rc != DB_NOTFOUND) {
		(void)txn->abort(txn);
		return (soak_is_giveup(rc) ? 0 : rc);
	}
	if ((rc = txn->commit(txn, 0)) != 0)
		return (soak_is_giveup(rc) ? 0 : rc);
	return (0);
}

/*
 * mvcc_retained -- the #138 shape.
 *	A snapshot transaction that both READS (creating SIREAD markers, so
 *	__txn_end parks its detail on the mvcc_txn list with TXN_DTL_SNAPSHOT
 *	rather than freeing it) and WRITES (creating MVCC buffer versions, so
 *	mvcc_ref is nonzero too).  Once the markers are garbage-collected and
 *	the MVCC pages evicted, the detail is reclaimed by
 *	__txn_reap_si_details -- which per #138 frees the detail without
 *	freeing its mvcc_mtx, leaking one mutex slot per reaped detail.
 *
 *	Driving that path needs cache turnover, so the workload spreads its
 *	writes over a keyspace far wider than the ro_snapshot one and reads a
 *	range rather than a single key.
 */
static int
wl_mvcc_retained(soak_workload *w, long i)
{
	DB_TXN *txn;
	int j, rc, v;

	(void)w;
	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT)) != 0) {
		if (soak_resource_rc(rc)) {
			soak_note_enomem("txn_begin", i);
			return (0);
		}
		return (rc);
	}
	/* Read a few keys: this is what leaves SIREAD markers behind. */
	for (j = 0; j < 4; j++)
		if ((rc = soak_get(txn, i + j, &v)) != 0 &&
		    rc != DB_NOTFOUND) {
			(void)txn->abort(txn);
			return (soak_is_giveup(rc) ? 0 : rc);
		}
	if ((rc = soak_put(txn, i, (int)i)) != 0) {
		(void)txn->abort(txn);
		if (soak_resource_rc(rc)) {
			soak_note_enomem("DB->put", i);
			return (0);
		}
		return (soak_is_giveup(rc) ? 0 : rc);
	}
	if ((rc = txn->commit(txn, 0)) != 0)
		return (soak_is_giveup(rc) ? 0 : rc);
	return (0);
}

/* rw_plain -- ordinary read-write transaction, no snapshot.  Control. */
static int
wl_rw_plain(soak_workload *w, long i)
{
	DB_TXN *txn;
	int rc, v;

	(void)w;
	if ((rc = env->txn_begin(env, NULL, &txn, 0)) != 0) {
		if (soak_resource_rc(rc)) {
			soak_note_enomem("txn_begin", i);
			return (0);
		}
		return (rc);
	}
	if ((rc = soak_get(txn, i, &v)) != 0 && rc != DB_NOTFOUND) {
		(void)txn->abort(txn);
		return (soak_is_giveup(rc) ? 0 : rc);
	}
	if ((rc = soak_put(txn, i, (int)i)) != 0) {
		(void)txn->abort(txn);
		return (soak_is_giveup(rc) ? 0 : rc);
	}
	if ((rc = txn->commit(txn, 0)) != 0)
		return (soak_is_giveup(rc) ? 0 : rc);
	return (0);
}

/* aborted -- every transaction aborts; the undo path must free too. */
static int
wl_aborted(soak_workload *w, long i)
{
	DB_TXN *txn;
	int rc;

	(void)w;
	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT)) != 0) {
		if (soak_resource_rc(rc)) {
			soak_note_enomem("txn_begin", i);
			return (0);
		}
		return (rc);
	}
	if ((rc = soak_put(txn, i, (int)i)) != 0 && !soak_is_giveup(rc)) {
		(void)txn->abort(txn);
		if (soak_resource_rc(rc)) {
			soak_note_enomem("DB->put", i);
			return (0);
		}
		return (rc);
	}
	if ((rc = txn->abort(txn)) != 0)
		return (rc);
	return (0);
}

/*
 * cursor_churn -- cursor open/close accounting.
 *	Deliberately a PLAIN (non-snapshot) transaction: a snapshot reader
 *	would trip the #137 locker leak and the workload would then be a
 *	second copy of ro_snapshot instead of telling us anything about
 *	cursors.  With a plain txn, any growth here is genuinely a cursor or
 *	lock-list accounting problem.
 */
static int
wl_cursor_churn(soak_workload *w, long i)
{
	DB_TXN *txn;
	DBC *dbc;
	DBT k, d;
	int rc;

	(void)w;
	if ((rc = env->txn_begin(env, NULL, &txn, 0)) != 0) {
		if (soak_resource_rc(rc)) {
			soak_note_enomem("txn_begin", i);
			return (0);
		}
		return (rc);
	}
	if ((rc = db->cursor(db, txn, &dbc, 0)) != 0) {
		(void)txn->abort(txn);
		if (soak_resource_rc(rc)) {
			soak_note_enomem("DB->cursor", i);
			return (0);
		}
		return (rc);
	}
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	/* Walk a few records so the cursor actually acquires locks. */
	for (rc = dbc->get(dbc, &k, &d, DB_FIRST);
	    rc == 0; rc = dbc->get(dbc, &k, &d, DB_NEXT))
		continue;
	if (rc != DB_NOTFOUND && !soak_is_giveup(rc)) {
		(void)dbc->close(dbc);
		(void)txn->abort(txn);
		return (rc);
	}
	if ((rc = dbc->close(dbc)) != 0) {
		(void)txn->abort(txn);
		return (rc);
	}
	if ((rc = txn->commit(txn, 0)) != 0)
		return (soak_is_giveup(rc) ? 0 : rc);
	return (0);
}

static soak_workload workloads[] = {
    { "ro_snapshot",
      "read-only DB_TXN_SNAPSHOT txns, no write (the #137 shape)",
      wl_ro_snapshot, 1, "#137", 0 },
    { "mvcc_retained",
      "snapshot txns that read and write, details MVCC-retained (#138)",
      wl_mvcc_retained, 1, "#138", 0 },
    { "rw_plain",
      "ordinary read-write txns, no snapshot (control)",
      wl_rw_plain, 0, NULL, 0 },
    { "aborted",
      "snapshot txns that all abort (control)",
      wl_aborted, 0, NULL, 0 },
    { "cursor_churn",
      "plain txns that open, walk and close a cursor (control)",
      wl_cursor_churn, 0, NULL, 0 },
};
#define	NWORKLOADS ((int)(sizeof(workloads) / sizeof(workloads[0])))

/*
 * soak_slope --
 *	Least-squares slope of counter `c' over samples [lo, hi), in units
 *	per 1000 transactions.  Least squares rather than (last - first)
 *	because a single noisy endpoint should not decide the verdict.
 */
static double
soak_slope(const soak_sample *s, int lo, int hi, int c)
{
	double den, mx, my, num;
	int i, n;

	if ((n = hi - lo) < 2)
		return (0.0);
	for (i = lo, mx = my = 0.0; i < hi; i++) {
		mx += (double)s[i].txns;
		my += s[i].v[c];
	}
	mx /= n;
	my /= n;
	for (i = lo, num = den = 0.0; i < hi; i++) {
		double dx = (double)s[i].txns - mx;
		num += dx * (s[i].v[c] - my);
		den += dx * dx;
	}
	if (den == 0.0)
		return (0.0);
	return (num / den * 1000.0);
}

/*
 * run_workload --
 *	Returns 0 if the outcome matched the expectation, 1 if not.
 */
static int
run_workload(soak_workload *w)
{
	soak_sample s[SOAK_MAX_SAMPLE];
	double slope[C_NCOUNTER];
	long every, i;
	int c, leaked, nsample, ok, warm;

	printf("== %s ==\n    shape: %s\n    %ld sequential transactions\n",
	    w->name, w->shape, soak_n);
	soak_enomem_at = -1;
	soak_enomem_call = NULL;

	soak_env_open(w->name);
	/* Seed the keyspace so read-only workloads find records. */
	for (i = 0; i < SOAK_NKEY; i++)
		if (soak_put(NULL, i, 0) != 0)
			soak_die("seed put", EINVAL);

	every = soak_n / (SOAK_MAX_SAMPLE - 1);
	if (every < 1)
		every = 1;
	nsample = 0;
	soak_sample_now(&s[nsample++], 0);
	for (i = 1; i <= soak_n; i++) {
		int rc = w->one(w, i);

		if (rc != 0)
			soak_die("workload transaction", rc);
		if (i % every == 0 && nsample < SOAK_MAX_SAMPLE)
			soak_sample_now(&s[nsample++], i);
	}
	if (nsample < SOAK_MAX_SAMPLE && s[nsample - 1].txns != soak_n)
		soak_sample_now(&s[nsample++], soak_n);

	/*
	 * Warmup: ignore the first quarter of the samples.  Lazy region
	 * allocation and cache fill legitimately grow counters there.
	 */
	warm = nsample / 4;
	if (warm < 1)
		warm = 1;
	if (nsample - warm < 2)
		warm = 0;

	printf("    growth curve (steady-state samples marked *):\n");
	printf("        %8s", "txns");
	for (c = 0; c < C_NCOUNTER; c++)
		printf(" %14s", counters[c].name);
	printf("\n");
	for (i = 0; i < nsample; i++) {
		printf("      %s %8ld", i >= warm ? "*" : " ", s[i].txns);
		for (c = 0; c < C_NCOUNTER; c++)
			printf(" %14.0f", s[i].v[c]);
		printf("\n");
	}

	leaked = 0;
	printf("    steady-state slope (units per 1000 txns, tolerance):\n");
	for (c = 0; c < C_NCOUNTER; c++) {
		slope[c] = soak_slope(s, warm, nsample, c);
		printf("        %-14s %+10.2f  (tol %6.2f)%s\n",
		    counters[c].name, slope[c], counters[c].tolerance,
		    slope[c] > counters[c].tolerance ? "   <== GROWING" : "");
		if (slope[c] > counters[c].tolerance)
			leaked = 1;
	}
	if (soak_enomem_at >= 0) {
		printf("        ENOMEM/RUNRECOVERY from %s at transaction "
		    "%ld -- the region ran out\n",
		    soak_enomem_call, soak_enomem_at);
		leaked = 1;
	}

	soak_env_close();

	ok = !leaked;
	if (ok == w->expect_leak) {
		if (w->expect_leak)
			printf("    UNEXPECTED PASS: %s is marked as "
			    "reproducing %s but resources stayed flat -- the "
			    "issue looks FIXED; clear expect_leak for this "
			    "workload.\n\n", w->name, w->issue);
		else
			printf("    FAIL: a region resource grew "
			    "monotonically beyond tolerance over %ld "
			    "sequential transactions.\n\n", soak_n);
		return (1);
	}
	if (w->expect_leak)
		printf("    XFAIL (reproduces %s): resources grow with "
		    "transaction count, as the issue reports.\n\n", w->issue);
	else
		printf("    PASS: resources stayed within tolerance.\n\n");
	return (0);
}

int
main(int argc, char **argv)
{
	int failures, i, j, ran;

	setvbuf(stdout, NULL, _IOLBF, 0);

	/* -n N may precede the workload names. */
	i = 1;
	if (argc >= 3 && strcmp(argv[1], "-n") == 0) {
		soak_n = atol(argv[2]);
		if (soak_n < 10) {
			fprintf(stderr, "-n must be >= 10\n");
			return (2);
		}
		i = 3;
	}
	if (i < argc && strcmp(argv[i], "--list") == 0) {
		for (j = 0; j < NWORKLOADS; j++)
			printf("%s%s\n", workloads[j].name,
			    workloads[j].expect_leak ? "\t(expect-leak)" : "");
		return (0);
	}

	printf("%s\n\n", db_version(NULL, NULL, NULL));
	failures = ran = 0;
	if (i >= argc) {
		for (j = 0; j < NWORKLOADS; j++, ran++)
			failures += run_workload(&workloads[j]);
	} else {
		for (; i < argc; i++) {
			for (j = 0; j < NWORKLOADS; j++)
				if (strcmp(argv[i], workloads[j].name) == 0)
					break;
			if (j == NWORKLOADS) {
				fprintf(stderr, "unknown workload: %s\n",
				    argv[i]);
				return (2);
			}
			failures += run_workload(&workloads[j]);
			ran++;
		}
	}

	printf("%d workload(s) run, %d unexpected outcome(s)\n",
	    ran, failures);
	return (failures != 0);
}

/*-
 * See the file LICENSE for redistribution information.
 *
 * tproc_h -- a HammerDB-style "TPROC-H" analytic (OLAP) workload for libdb.
 *
 * Independently implemented; not the TPC-H benchmark and not comparable to
 * TPC results.  It models a star schema and runs long, read-mostly analytic
 * scans concurrently with a trickle of point updates, to exercise the case
 * MVCC is built for: long readers that must see a consistent snapshot without
 * blocking (or being blocked by) the writers.
 *
 * Schema (one DB per table, integer keys):
 *   lineitem  key=l_id            -> { orderkey, partkey, suppkey,
 *                                      quantity, price, shipdate }   (the fact)
 *   part      key=p_id            -> { retailprice, size }
 *   supplier  key=s_id            -> { acctbal, nation }
 *
 * Query threads (read-only) each run one of:
 *   Q-pricing  scan lineitem in a shipdate window, sum revenue (range scan)
 *   Q-partagg  scan lineitem, join part by partkey, sum by size bucket
 *   Q-suppagg  scan lineitem, join supplier by suppkey, sum acctbal
 * Writer threads apply point updates to part.retailprice and supplier.acctbal
 * (so the analytic scans run against a moving target).
 *
 * With -m the query threads use snapshot isolation and never block on the
 * writers.  Without -m they take read locks (and serialize against writers);
 * with -X txn they run with no isolation at all.  This contrast is the point.
 */
#include "bdb_bench.h"

#define	N_PART		20000
#define	N_SUPPLIER	2000
#define	SHIPDATE_RANGE	2557		/* ~7 years of days */

static DB_ENV	*g_env;
static bb_config g_cfg;
static DB	*g_line, *g_part, *g_supp;
static int	 g_lineitems;		/* scale * base */
static volatile int g_stop;

enum { Q_PRICING, Q_PARTAGG, Q_SUPPAGG, W_UPDATE, T_N };
static const char *g_tnames[T_N] = {
	"q-pricing", "q-partagg", "q-suppagg", "w-update"
};

typedef struct {
	int tid;
	int is_writer;
	bb_rng rng;
	uint64_t ops[T_N];
	uint64_t rows;		/* rows scanned (reader) sink */
	uint64_t retry[T_N];
} worker;

typedef struct {
	uint32_t orderkey, partkey, suppkey;
	int32_t quantity, price, shipdate;
	uint8_t pad[40];
} line_rec;
typedef struct { int32_t retailprice, size; uint8_t pad[56]; } part_rec;
typedef struct { int64_t acctbal; int32_t nation; uint8_t pad[52]; } supp_rec;

static int
open_db(DB **dbp, const char *name)
{
	DB *db;
	int ret;

	if ((ret = db_create(&db, g_env, 0)) != 0) return ret;
	if ((ret = db->open(db, NULL, name, NULL, DB_BTREE,
	    bb_db_flags(&g_cfg), 0)) != 0) {
		g_env->err(g_env, ret, "open %s", name);
		return ret;
	}
	*dbp = db;
	return 0;
}

static int
put_u32(DB *db, DB_TXN *txn, uint32_t key, void *rec, size_t sz)
{
	DBT k, d;

	memset(&k, 0, sizeof(k)); k.data = &key; k.size = sizeof(key);
	memset(&d, 0, sizeof(d)); d.data = rec; d.size = (u_int32_t)sz;
	return db->put(db, txn, &k, &d, 0);
}

static int
get_u32(DB *db, DB_TXN *txn, uint32_t key, void *out, size_t outsz)
{
	DBT k, d;

	memset(&k, 0, sizeof(k)); k.data = &key; k.size = sizeof(key);
	memset(&d, 0, sizeof(d));
	d.data = out; d.ulen = (u_int32_t)outsz; d.flags = DB_DBT_USERMEM;
	return db->get(db, txn, &k, &d, 0);
}

static int
populate(void)
{
	DB_TXN *txn = NULL;
	line_rec l; part_rec p; supp_rec s;
	uint32_t i;
	int ret, n;

	if ((ret = open_db(&g_line, "lineitem.db")) != 0) return ret;
	if ((ret = open_db(&g_part, "part.db")) != 0) return ret;
	if ((ret = open_db(&g_supp, "supplier.db")) != 0) return ret;

	if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
	n = 0;

	for (i = 0; i < N_PART; i++) {
		memset(&p, 0, sizeof(p));
		p.retailprice = 100 + (int32_t)(i % 9000);
		p.size = 1 + (int32_t)(i % 50);
		if ((ret = put_u32(g_part, txn, i, &p, sizeof(p))) != 0) goto err;
	}
	for (i = 0; i < N_SUPPLIER; i++) {
		memset(&s, 0, sizeof(s));
		s.acctbal = 1000 + (int64_t)(i % 100000);
		s.nation = (int32_t)(i % 25);
		if ((ret = put_u32(g_supp, txn, i, &s, sizeof(s))) != 0) goto err;
	}
	for (i = 0; i < (uint32_t)g_lineitems; i++) {
		memset(&l, 0, sizeof(l));
		l.orderkey = i / 4;
		l.partkey = i % N_PART;
		l.suppkey = i % N_SUPPLIER;
		l.quantity = 1 + (int32_t)(i % 50);
		l.price = 100 + (int32_t)(i % 90000);
		l.shipdate = (int32_t)(i % SHIPDATE_RANGE);
		if ((ret = put_u32(g_line, txn, i, &l, sizeof(l))) != 0) goto err;
		if (++n % 20000 == 0 && g_cfg.use_txn) {
			if ((ret = bb_commit(txn)) != 0) return ret;
			if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
		}
	}
	return bb_commit(txn);
err:
	(void)bb_abort(txn);
	return ret;
}

/* Range scan of lineitem in a shipdate window, summing revenue. */
static int
q_pricing(worker *w)
{
	DB_TXN *txn;
	DBC *dbc;
	DBT k, d;
	line_rec l;
	int32_t lo, hi;
	uint64_t revenue;
	int op = Q_PRICING, ret;

	lo = (int32_t)bb_rand_between(&w->rng, 0, SHIPDATE_RANGE - 200);
	hi = lo + 180;
	revenue = 0;
again:
	dbc = NULL;
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 1)) != 0) return ret;
	if ((ret = g_line->cursor(g_line, txn, &dbc, 0)) != 0) goto fail;
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d)); d.data = &l; d.ulen = sizeof(l); d.flags = DB_DBT_USERMEM;
	ret = dbc->get(dbc, &k, &d, DB_FIRST);
	while (ret == 0) {
		if (l.shipdate >= lo && l.shipdate <= hi)
			revenue += (uint64_t)l.price * (uint64_t)l.quantity;
		w->rows++;
		memset(&d, 0, sizeof(d)); d.data = &l; d.ulen = sizeof(l); d.flags = DB_DBT_USERMEM;
		ret = dbc->get(dbc, &k, &d, DB_NEXT);
	}
	if (ret == DB_LOCK_DEADLOCK) { (void)dbc->close(dbc); (void)bb_abort(txn); w->retry[op]++; goto again; }
	(void)dbc->close(dbc);
	if (ret != 0 && ret != DB_NOTFOUND) goto fail;
	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ops[op]++;
	return (revenue == ~0ULL) ? -1 : 0;	/* keep revenue live */
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

/* Scan lineitem, join part by partkey, accumulate by size. */
static int
q_partagg(worker *w)
{
	DB_TXN *txn;
	DBC *dbc;
	DBT k, d;
	line_rec l;
	part_rec p;
	uint64_t acc;
	int op = Q_PARTAGG, ret, scanned;

	acc = 0; scanned = 0;
again:
	dbc = NULL;
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 1)) != 0) return ret;
	if ((ret = g_line->cursor(g_line, txn, &dbc, 0)) != 0) goto fail;
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d)); d.data = &l; d.ulen = sizeof(l); d.flags = DB_DBT_USERMEM;
	ret = dbc->get(dbc, &k, &d, DB_FIRST);
	/* Sample every 8th row to bound the join cost. */
	while (ret == 0) {
		if ((scanned++ & 7) == 0) {
			if (get_u32(g_part, txn, l.partkey, &p, sizeof(p)) == 0)
				acc += (uint64_t)p.retailprice * (uint64_t)l.quantity;
			w->rows++;
		}
		memset(&d, 0, sizeof(d)); d.data = &l; d.ulen = sizeof(l); d.flags = DB_DBT_USERMEM;
		ret = dbc->get(dbc, &k, &d, DB_NEXT);
	}
	if (ret == DB_LOCK_DEADLOCK) { (void)dbc->close(dbc); (void)bb_abort(txn); w->retry[op]++; goto again; }
	(void)dbc->close(dbc);
	if (ret != 0 && ret != DB_NOTFOUND) goto fail;
	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ops[op]++;
	return (acc == ~0ULL) ? -1 : 0;
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

/* Scan a sample of lineitem, join supplier, sum acctbal. */
static int
q_suppagg(worker *w)
{
	DB_TXN *txn;
	DBC *dbc;
	DBT k, d;
	line_rec l;
	supp_rec s;
	int64_t acc;
	int op = Q_SUPPAGG, ret, scanned;

	acc = 0; scanned = 0;
again:
	dbc = NULL;
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 1)) != 0) return ret;
	if ((ret = g_line->cursor(g_line, txn, &dbc, 0)) != 0) goto fail;
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d)); d.data = &l; d.ulen = sizeof(l); d.flags = DB_DBT_USERMEM;
	ret = dbc->get(dbc, &k, &d, DB_FIRST);
	while (ret == 0) {
		if ((scanned++ & 7) == 0) {
			if (get_u32(g_supp, txn, l.suppkey, &s, sizeof(s)) == 0)
				acc += s.acctbal;
			w->rows++;
		}
		memset(&d, 0, sizeof(d)); d.data = &l; d.ulen = sizeof(l); d.flags = DB_DBT_USERMEM;
		ret = dbc->get(dbc, &k, &d, DB_NEXT);
	}
	if (ret == DB_LOCK_DEADLOCK) { (void)dbc->close(dbc); (void)bb_abort(txn); w->retry[op]++; goto again; }
	(void)dbc->close(dbc);
	if (ret != 0 && ret != DB_NOTFOUND) goto fail;
	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ops[op]++;
	return (acc == ~(int64_t)0) ? -1 : 0;
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

/* Writer: point-update a random part price and supplier balance. */
static int
w_update(worker *w)
{
	DB_TXN *txn;
	part_rec p;
	supp_rec s;
	uint32_t pid, sid;
	int op = W_UPDATE, ret;

	pid = bb_rand_between(&w->rng, 0, N_PART - 1);
	sid = bb_rand_between(&w->rng, 0, N_SUPPLIER - 1);
again:
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
	ret = get_u32(g_part, txn, pid, &p, sizeof(p));
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); w->retry[op]++; goto again; }
	if (ret != 0) goto fail;
	p.retailprice += 1;
	ret = put_u32(g_part, txn, pid, &p, sizeof(p));
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); w->retry[op]++; goto again; }
	if (ret != 0) goto fail;
	ret = get_u32(g_supp, txn, sid, &s, sizeof(s));
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); w->retry[op]++; goto again; }
	if (ret != 0) goto fail;
	s.acctbal += 1;
	ret = put_u32(g_supp, txn, sid, &s, sizeof(s));
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); w->retry[op]++; goto again; }
	if (ret != 0) goto fail;
	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ops[op]++;
	return 0;
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

static void *
worker_main(void *arg)
{
	worker *w = arg;
	int ret;

	while (!g_stop) {
		if (w->is_writer)
			ret = w_update(w);
		else {
			uint32_t r = bb_rand_between(&w->rng, 0, 2);
			ret = r == 0 ? q_pricing(w) :
			      r == 1 ? q_partagg(w) : q_suppagg(w);
		}
		if (ret != 0 && ret != DB_LOCK_DEADLOCK) {
			fprintf(stderr, "op error: %s\n", db_strerror(ret));
			break;
		}
	}
	return NULL;
}

static void
usage(const char *p)
{
	fprintf(stderr,
	    "usage: %s [-i] [-h home] [-S scale] [-t querythreads] [-w writers]\n"
	    "          [-s secs] [-c cachebytes] [-d sync|wnosync|nosync] [-m]\n"
	    "          [-X txn|lock|log] [-R seed]\n"
	    "  scale S gives ~S*250k lineitems\n", p);
}

int
main(int argc, char **argv)
{
	pthread_t *tids;
	worker *workers;
	uint64_t total[T_N], rows;
	double t0, elapsed;
	int i, t, ret, nthreads, writers, argi;

	bb_config_defaults(&g_cfg);

	/* Pre-scan for our extra -w (writers) flag, then the shared options. */
	writers = 1;
	for (argi = 1; argi < argc - 1; argi++)
		if (strcmp(argv[argi], "-w") == 0)
			writers = atoi(argv[argi + 1]);
	/* Strip -w from argv so bb_getopt doesn't choke. */
	{
		int j, k2 = 1;
		char **nv = calloc((size_t)argc, sizeof(char *));
		nv[0] = argv[0];
		for (j = 1; j < argc; j++) {
			if (strcmp(argv[j], "-w") == 0) { j++; continue; }
			nv[k2++] = argv[j];
		}
		if (bb_getopt(k2, nv, &g_cfg) != 0) { usage(argv[0]); return 1; }
		free(nv);
	}
	if (g_cfg.scale < 1) g_cfg.scale = 1;
	g_lineitems = g_cfg.scale * 250000;

	if ((ret = bb_env_open(&g_cfg, &g_env)) != 0) return 1;

	if (g_cfg.init) {
		bb_print_config(&g_cfg, "tproc-h populate");
		t0 = bb_now_ms();
		if ((ret = populate()) != 0) {
			fprintf(stderr, "populate: %s\n", db_strerror(ret));
			return 1;
		}
		printf("# populated %d lineitems in %.1f s\n",
		    g_lineitems, (bb_now_ms() - t0) / 1000.0);
		(void)g_env->close(g_env, 0);
		return 0;
	}

	if ((ret = open_db(&g_line, "lineitem.db")) != 0 ||
	    (ret = open_db(&g_part, "part.db")) != 0 ||
	    (ret = open_db(&g_supp, "supplier.db")) != 0) {
		fprintf(stderr, "open (did you -i first?): %s\n", db_strerror(ret));
		return 1;
	}

	nthreads = g_cfg.threads + writers;
	bb_print_config(&g_cfg, "tproc-h");
	printf("# %d query threads + %d writer threads\n", g_cfg.threads, writers);

	tids = calloc((size_t)nthreads, sizeof(*tids));
	workers = calloc((size_t)nthreads, sizeof(*workers));
	for (t = 0; t < nthreads; t++) {
		workers[t].tid = t;
		workers[t].is_writer = (t >= g_cfg.threads);
		bb_rng_seed(&workers[t].rng, g_cfg.seed + (uint64_t)t * 0x100);
	}

	g_stop = 0;
	{
		pthread_t ddtid; struct bb_dd_arg ddarg; int dd_on;
		dd_on = (bb_start_dd(&g_cfg, g_env, &ddtid, &ddarg, &g_stop) == 0
		    && g_cfg.dd_periodic > 0);
		t0 = bb_now_ms();
		for (t = 0; t < nthreads; t++)
			pthread_create(&tids[t], NULL, worker_main, &workers[t]);
		usleep((useconds_t)g_cfg.seconds * 1000000);
		g_stop = 1;
		for (t = 0; t < nthreads; t++)
			pthread_join(tids[t], NULL);
		if (dd_on)
			pthread_join(ddtid, NULL);
	}
	elapsed = (bb_now_ms() - t0) / 1000.0;

	memset(total, 0, sizeof(total));
	rows = 0;
	for (t = 0; t < nthreads; t++) {
		for (i = 0; i < T_N; i++) total[i] += workers[t].ops[i];
		rows += workers[t].rows;
	}

	printf("# op-type      completed\n");
	for (i = 0; i < T_N; i++)
		printf("%-12s %10llu\n", g_tnames[i],
		    (unsigned long long)total[i]);
	printf("tproc-h %d q + %d w  queries/s=%.1f  rows-scanned/s=%.0f  (%.1fs)\n",
	    g_cfg.threads, writers,
	    (double)(total[Q_PRICING] + total[Q_PARTAGG] + total[Q_SUPPAGG]) / elapsed,
	    (double)rows / elapsed, elapsed);

	free(tids); free(workers);
	(void)g_env->close(g_env, 0);
	return 0;
}

/*-
 * See the file LICENSE for redistribution information.
 *
 * tproc_c -- a HammerDB-style "TPROC-C" OLTP workload for libdb.
 *
 * Independently implemented; not the TPC-C benchmark and not comparable to
 * TPC results.  It models the same warehouse/order business with the five
 * classic weighted transactions:
 *
 *   New-Order   (45%)  read-write: pick items, decrement stock, create order
 *   Payment     (43%)  read-write: update warehouse/district/customer balances
 *   Order-Status (4%)  read-only:  look up a customer's most recent order
 *   Delivery     (4%)  read-write: mark oldest new-orders delivered
 *   Stock-Level  (4%)  read-only:  count district stock below a threshold
 *
 * The read-only transactions use snapshot isolation when -m (MVCC) is set, so
 * they never block the read-write transactions.  All five run with or without
 * transactions, locking, and logging via the bdb_bench.h toggle framework.
 *
 * Schema (one DB per table, integer keys, fixed-size records):
 *   warehouse   key=w_id                          -> w_ytd
 *   district    key=(w_id,d_id)                    -> d_ytd, d_next_o_id
 *   customer    key=(w_id,d_id,c_id)               -> c_balance, c_ytd, c_last_o
 *   stock       key=(w_id,i_id)                    -> s_quantity
 *   orders      key=(w_id,d_id,o_id)               -> o_c_id, o_carrier, o_ol_cnt
 *   neworder    key=(w_id,d_id,o_id)               -> (presence = undelivered)
 *   item        key=i_id                           -> i_price   (read-only, fixed)
 *
 * Scale: -S warehouses.  Per warehouse: DISTRICTS districts, CUST_PER_DIST
 * customers/district, ITEMS items (shared), STOCK_PER_WH stock rows.
 */
#include "bdb_bench.h"

#define	DISTRICTS	10
#define	CUST_PER_DIST	300	/* compact (spec is 3000) to keep load fast */
#define	ITEMS		10000
#define	ORDERS_PER_DIST	CUST_PER_DIST
#define	STOCK_LOW_THRESHOLD 10

/* Composite integer keys, big-endian so cursor scans are ordered. */
typedef struct { uint32_t a, b, c; } key3;

static DB_ENV	*g_env;
static bb_config g_cfg;
static DB	*g_wh, *g_dist, *g_cust, *g_stock, *g_ord, *g_neword, *g_item;

static volatile int g_stop;		/* set when the timer expires */

/* Per-transaction-type counters, summed across threads at the end. */
enum { T_NEWORDER, T_PAYMENT, T_ORDERSTATUS, T_DELIVERY, T_STOCKLEVEL, T_N };
static const char *g_tnames[T_N] = {
	"new-order", "payment", "order-status", "delivery", "stock-level"
};

typedef struct {
	int tid;
	bb_rng rng;
	uint64_t ok[T_N];
	uint64_t retry[T_N];	/* deadlock/conflict retries */
	uint64_t stock_low_seen;	/* sink so stock-level scan isn't elided */
} worker;

static void
mkkey(DBT *dbt, key3 *k, uint32_t a, uint32_t b, uint32_t c)
{
	k->a = a; k->b = b; k->c = c;
	memset(dbt, 0, sizeof(*dbt));
	dbt->data = k;
	dbt->size = sizeof(*k);
}

/* ---- record bodies (fixed size, padded to one cache line) ---- */
typedef struct { int64_t ytd; uint8_t pad[56]; } wh_rec;
typedef struct { int64_t ytd; uint32_t next_o_id; uint8_t pad[52]; } dist_rec;
typedef struct { int64_t balance, ytd; uint32_t last_o; uint8_t pad[44]; } cust_rec;
typedef struct { int32_t quantity; uint8_t pad[60]; } stock_rec;
typedef struct { uint32_t c_id, carrier, ol_cnt; uint8_t pad[52]; } ord_rec;
typedef struct { uint32_t price; uint8_t pad[60]; } item_rec;

/* ---------------------------------------------------------------- */
/* Population                                                        */
/* ---------------------------------------------------------------- */
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
populate(void)
{
	DBT k, d;
	key3 kk;
	DB_TXN *txn = NULL;
	wh_rec w; dist_rec di; cust_rec cu; stock_rec st; ord_rec o; item_rec it;
	int wid, did, cid, iid, oid, ret, nput;

	if ((ret = open_db(&g_wh, "warehouse.db")) != 0) return ret;
	if ((ret = open_db(&g_dist, "district.db")) != 0) return ret;
	if ((ret = open_db(&g_cust, "customer.db")) != 0) return ret;
	if ((ret = open_db(&g_stock, "stock.db")) != 0) return ret;
	if ((ret = open_db(&g_ord, "orders.db")) != 0) return ret;
	if ((ret = open_db(&g_neword, "neworder.db")) != 0) return ret;
	if ((ret = open_db(&g_item, "item.db")) != 0) return ret;

	/* Items are global and read-only at run time. */
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
	memset(&it, 0, sizeof(it));
	nput = 0;
	for (iid = 0; iid < ITEMS; iid++) {
		it.price = 100 + (uint32_t)(iid % 9900);
		mkkey(&k, &kk, (uint32_t)iid, 0, 0);
		memset(&d, 0, sizeof(d)); d.data = &it; d.size = sizeof(it);
		if ((ret = g_item->put(g_item, txn, &k, &d, 0)) != 0) goto err;
		if (++nput % 10000 == 0 && g_cfg.use_txn) {
			if ((ret = bb_commit(txn)) != 0) return ret;
			if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
		}
	}

	for (wid = 0; wid < g_cfg.scale; wid++) {
		memset(&w, 0, sizeof(w)); w.ytd = 0;
		mkkey(&k, &kk, (uint32_t)wid, 0, 0);
		memset(&d, 0, sizeof(d)); d.data = &w; d.size = sizeof(w);
		if ((ret = g_wh->put(g_wh, txn, &k, &d, 0)) != 0) goto err;

		for (iid = 0; iid < ITEMS; iid++) {
			memset(&st, 0, sizeof(st));
			st.quantity = 10 + (int32_t)(iid % 90);
			mkkey(&k, &kk, (uint32_t)wid, (uint32_t)iid, 0);
			memset(&d, 0, sizeof(d)); d.data = &st; d.size = sizeof(st);
			if ((ret = g_stock->put(g_stock, txn, &k, &d, 0)) != 0) goto err;
			if (++nput % 10000 == 0 && g_cfg.use_txn) {
				if ((ret = bb_commit(txn)) != 0) return ret;
				if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
			}
		}

		for (did = 0; did < DISTRICTS; did++) {
			memset(&di, 0, sizeof(di));
			di.next_o_id = ORDERS_PER_DIST;
			mkkey(&k, &kk, (uint32_t)wid, (uint32_t)did, 0);
			memset(&d, 0, sizeof(d)); d.data = &di; d.size = sizeof(di);
			if ((ret = g_dist->put(g_dist, txn, &k, &d, 0)) != 0) goto err;

			for (cid = 0; cid < CUST_PER_DIST; cid++) {
				memset(&cu, 0, sizeof(cu));
				cu.balance = -1000; cu.ytd = 1000; cu.last_o = (uint32_t)cid;
				mkkey(&k, &kk, (uint32_t)wid,
				    (uint32_t)(did * CUST_PER_DIST + cid), 0);
				memset(&d, 0, sizeof(d)); d.data = &cu; d.size = sizeof(cu);
				if ((ret = g_cust->put(g_cust, txn, &k, &d, 0)) != 0) goto err;
			}
			for (oid = 0; oid < ORDERS_PER_DIST; oid++) {
				memset(&o, 0, sizeof(o));
				o.c_id = (uint32_t)(oid % CUST_PER_DIST);
				o.carrier = 0; o.ol_cnt = 10;
				mkkey(&k, &kk, (uint32_t)wid,
				    (uint32_t)did, (uint32_t)oid);
				memset(&d, 0, sizeof(d)); d.data = &o; d.size = sizeof(o);
				if ((ret = g_ord->put(g_ord, txn, &k, &d, 0)) != 0) goto err;
				/* Half the orders are undelivered (new-order). */
				if (oid >= ORDERS_PER_DIST / 2) {
					memset(&d, 0, sizeof(d)); d.data = ""; d.size = 0;
					if ((ret = g_neword->put(g_neword, txn,
					    &k, &d, 0)) != 0) goto err;
				}
				if (++nput % 10000 == 0 && g_cfg.use_txn) {
					if ((ret = bb_commit(txn)) != 0) return ret;
					if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
				}
			}
		}
	}
	if ((ret = bb_commit(txn)) != 0) return ret;
	return 0;
err:
	(void)bb_abort(txn);
	return ret;
}

/* ---------------------------------------------------------------- */
/* Transactions                                                      */
/* ---------------------------------------------------------------- */
#define	RETRY(expr) do {						\
	ret = (expr);							\
	if (ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED) {	\
		(void)bb_abort(txn); txn = NULL;			\
		w->retry[op]++;						\
		goto again;						\
	}								\
} while (0)

static int
get_rec(DB *db, DB_TXN *txn, uint32_t a, uint32_t b, uint32_t c,
    void *out, size_t outsz)
{
	DBT k, d;
	key3 kk;

	mkkey(&k, &kk, a, b, c);
	memset(&d, 0, sizeof(d));
	d.data = out; d.ulen = (u_int32_t)outsz; d.flags = DB_DBT_USERMEM;
	return db->get(db, txn, &k, &d, 0);
}

static int
put_rec(DB *db, DB_TXN *txn, uint32_t a, uint32_t b, uint32_t c,
    void *in, size_t insz)
{
	DBT k, d;
	key3 kk;

	mkkey(&k, &kk, a, b, c);
	memset(&d, 0, sizeof(d)); d.data = in; d.size = (u_int32_t)insz;
	return db->put(db, txn, &k, &d, 0);
}

static int
do_new_order(worker *w)
{
	DB_TXN *txn;
	dist_rec di;
	stock_rec st;
	ord_rec o;
	uint32_t wid, did, oid, i, nitems;
	int op = T_NEWORDER, ret;

	wid = bb_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
	did = bb_rand_between(&w->rng, 0, DISTRICTS - 1);
	nitems = bb_rand_between(&w->rng, 5, 15);
again:
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;

	/* Allocate the next order id from the district. */
	RETRY(get_rec(g_dist, txn, wid, did, 0, &di, sizeof(di)));
	if (ret != 0) goto fail;
	oid = di.next_o_id++;
	RETRY(put_rec(g_dist, txn, wid, did, 0, &di, sizeof(di)));
	if (ret != 0) goto fail;

	/* Decrement stock for each order line. */
	for (i = 0; i < nitems; i++) {
		uint32_t iid = bb_rand_between(&w->rng, 0, ITEMS - 1);
		RETRY(get_rec(g_stock, txn, wid, iid, 0, &st, sizeof(st)));
		if (ret != 0) goto fail;
		st.quantity -= 1;
		if (st.quantity < 10) st.quantity += 91;
		RETRY(put_rec(g_stock, txn, wid, iid, 0, &st, sizeof(st)));
		if (ret != 0) goto fail;
	}

	/* Insert the order + new-order marker. */
	memset(&o, 0, sizeof(o));
	o.c_id = bb_rand_between(&w->rng, 0, CUST_PER_DIST - 1);
	o.ol_cnt = nitems;
	RETRY(put_rec(g_ord, txn, wid, did, oid, &o, sizeof(o)));
	if (ret != 0) goto fail;
	RETRY(put_rec(g_neword, txn, wid, did, oid, "", 0));
	if (ret != 0) goto fail;

	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ok[op]++;
	return 0;
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

static int
do_payment(worker *w)
{
	DB_TXN *txn;
	wh_rec wh;
	dist_rec di;
	cust_rec cu;
	uint32_t wid, did, cid;
	int64_t amount;
	int op = T_PAYMENT, ret;

	wid = bb_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
	did = bb_rand_between(&w->rng, 0, DISTRICTS - 1);
	cid = bb_rand_between(&w->rng, 0, DISTRICTS * CUST_PER_DIST - 1);
	amount = (int64_t)bb_rand_between(&w->rng, 1, 5000);
again:
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;

	RETRY(get_rec(g_wh, txn, wid, 0, 0, &wh, sizeof(wh)));
	if (ret != 0) goto fail;
	wh.ytd += amount;
	RETRY(put_rec(g_wh, txn, wid, 0, 0, &wh, sizeof(wh)));
	if (ret != 0) goto fail;

	RETRY(get_rec(g_dist, txn, wid, did, 0, &di, sizeof(di)));
	if (ret != 0) goto fail;
	di.ytd += amount;
	RETRY(put_rec(g_dist, txn, wid, did, 0, &di, sizeof(di)));
	if (ret != 0) goto fail;

	RETRY(get_rec(g_cust, txn, wid, cid, 0, &cu, sizeof(cu)));
	if (ret != 0) goto fail;
	cu.balance -= amount;
	cu.ytd += amount;
	RETRY(put_rec(g_cust, txn, wid, cid, 0, &cu, sizeof(cu)));
	if (ret != 0) goto fail;

	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ok[op]++;
	return 0;
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

static int
do_order_status(worker *w)
{
	DB_TXN *txn;
	cust_rec cu;
	ord_rec o;
	uint32_t wid, did, cid;
	int op = T_ORDERSTATUS, ret;

	wid = bb_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
	did = bb_rand_between(&w->rng, 0, DISTRICTS - 1);
	cid = bb_rand_between(&w->rng, 0, DISTRICTS * CUST_PER_DIST - 1);
again:
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 1)) != 0) return ret;	/* rdonly */

	RETRY(get_rec(g_cust, txn, wid, cid, 0, &cu, sizeof(cu)));
	if (ret != 0) goto fail;
	/* Look up the customer's most recent order (approx via last_o). */
	ret = get_rec(g_ord, txn, wid, did, cu.last_o, &o, sizeof(o));
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); txn=NULL; w->retry[op]++; goto again; }
	if (ret != 0 && ret != DB_NOTFOUND) goto fail;

	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ok[op]++;
	return 0;
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

static int
do_delivery(worker *w)
{
	DB_TXN *txn;
	DBC *dbc;
	DBT k, d;
	key3 kk, *fk;
	uint32_t wid, did;
	int op = T_DELIVERY, ret;

	wid = bb_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
	did = bb_rand_between(&w->rng, 0, DISTRICTS - 1);
again:
	dbc = NULL;
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;

	/* Find the oldest new-order in this (w,d) and delete it. */
	RETRY(g_neword->cursor(g_neword, txn, &dbc, 0));
	if (ret != 0) goto fail;
	mkkey(&k, &kk, wid, did, 0);
	memset(&d, 0, sizeof(d));
	ret = dbc->get(dbc, &k, &d, DB_SET_RANGE);
	if (ret == DB_LOCK_DEADLOCK) { (void)dbc->close(dbc); (void)bb_abort(txn); txn=NULL; w->retry[op]++; goto again; }
	if (ret == 0) {
		fk = (key3 *)k.data;
		if (fk->a == wid && fk->b == did) {
			ord_rec o;
			ret = dbc->del(dbc, 0);
			if (ret == DB_LOCK_DEADLOCK) { (void)dbc->close(dbc); (void)bb_abort(txn); txn=NULL; w->retry[op]++; goto again; }
			if (ret != 0) { (void)dbc->close(dbc); goto fail; }
			/* Mark the order delivered (set a carrier id). */
			(void)dbc->close(dbc); dbc = NULL;
			if (get_rec(g_ord, txn, fk->a, fk->b, fk->c,
			    &o, sizeof(o)) == 0) {
				o.carrier = 1;
				RETRY(put_rec(g_ord, txn, fk->a, fk->b, fk->c,
				    &o, sizeof(o)));
				if (ret != 0) goto fail;
			}
		}
	} else if (ret != DB_NOTFOUND) {
		(void)dbc->close(dbc);
		goto fail;
	}
	if (dbc != NULL) (void)dbc->close(dbc);

	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ok[op]++;
	return 0;
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

static int
do_stock_level(worker *w)
{
	DB_TXN *txn;
	DBC *dbc;
	DBT k, d;
	key3 kk, *fk;
	stock_rec st;
	uint32_t wid;
	int op = T_STOCKLEVEL, ret, low;

	wid = bb_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
again:
	dbc = NULL; low = 0;
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 1)) != 0) return ret;	/* rdonly */

	RETRY(g_stock->cursor(g_stock, txn, &dbc, 0));
	if (ret != 0) goto fail;
	mkkey(&k, &kk, wid, 0, 0);
	memset(&d, 0, sizeof(d)); d.data = &st; d.ulen = sizeof(st); d.flags = DB_DBT_USERMEM;
	ret = dbc->get(dbc, &k, &d, DB_SET_RANGE);
	while (ret == 0) {
		fk = (key3 *)k.data;
		if (fk->a != wid) break;	/* left this warehouse */
		if (((stock_rec *)d.data)->quantity < STOCK_LOW_THRESHOLD) low++;
		memset(&d, 0, sizeof(d)); d.data = &st; d.ulen = sizeof(st); d.flags = DB_DBT_USERMEM;
		ret = dbc->get(dbc, &k, &d, DB_NEXT);
	}
	if (ret == DB_LOCK_DEADLOCK) { (void)dbc->close(dbc); (void)bb_abort(txn); txn=NULL; w->retry[op]++; goto again; }
	(void)dbc->close(dbc);
	if (ret != 0 && ret != DB_NOTFOUND) goto fail;

	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->stock_low_seen += (uint64_t)low;
	w->ok[op]++;
	return 0;
fail:
	(void)bb_abort(txn);
	return ret == DB_LOCK_DEADLOCK ? 0 : ret;
}

/* Weighted transaction mix (out of 100). */
static int
run_one(worker *w)
{
	uint32_t r = bb_rand_between(&w->rng, 0, 99);

	if (r < 45) return do_new_order(w);
	if (r < 88) return do_payment(w);
	if (r < 92) return do_order_status(w);
	if (r < 96) return do_delivery(w);
	return do_stock_level(w);
}

static void *
worker_main(void *arg)
{
	worker *w = arg;
	int ret;

	while (!g_stop) {
		if ((ret = run_one(w)) != 0 && ret != DB_LOCK_DEADLOCK) {
			fprintf(stderr, "txn error: %s\n", db_strerror(ret));
			break;
		}
	}
	return NULL;
}

static void
usage(const char *p)
{
	fprintf(stderr,
	    "usage: %s [-i] [-h home] [-S warehouses] [-t threads] [-s secs]\n"
	    "          [-c cachebytes] [-d sync|wnosync|nosync] [-m] [-C]\n"
	    "          [-X txn|lock|log] [-R seed]\n", p);
}

int
main(int argc, char **argv)
{
	pthread_t *tids;
	worker *workers;
	uint64_t total[T_N], rtot[T_N], grand;
	double t0, elapsed;
	int i, t, ret;

	bb_config_defaults(&g_cfg);
	if (bb_getopt(argc, argv, &g_cfg) != 0) { usage(argv[0]); return 1; }
	if (g_cfg.scale < 1) g_cfg.scale = 1;

	if ((ret = bb_env_open(&g_cfg, &g_env)) != 0) return 1;

	if (g_cfg.init) {
		bb_print_config(&g_cfg, "tproc-c populate");
		t0 = bb_now_ms();
		if ((ret = populate()) != 0) {
			fprintf(stderr, "populate: %s\n", db_strerror(ret));
			return 1;
		}
		printf("# populated scale=%d in %.1f s\n",
		    g_cfg.scale, (bb_now_ms() - t0) / 1000.0);
		(void)g_env->close(g_env, 0);
		return 0;
	}

	/* Open the (already populated) tables. */
	if ((ret = open_db(&g_wh, "warehouse.db")) != 0 ||
	    (ret = open_db(&g_dist, "district.db")) != 0 ||
	    (ret = open_db(&g_cust, "customer.db")) != 0 ||
	    (ret = open_db(&g_stock, "stock.db")) != 0 ||
	    (ret = open_db(&g_ord, "orders.db")) != 0 ||
	    (ret = open_db(&g_neword, "neworder.db")) != 0 ||
	    (ret = open_db(&g_item, "item.db")) != 0) {
		fprintf(stderr, "open (did you -i first?): %s\n", db_strerror(ret));
		return 1;
	}

	bb_print_config(&g_cfg, "tproc-c");

	tids = calloc((size_t)g_cfg.threads, sizeof(*tids));
	workers = calloc((size_t)g_cfg.threads, sizeof(*workers));
	for (t = 0; t < g_cfg.threads; t++) {
		workers[t].tid = t;
		bb_rng_seed(&workers[t].rng, g_cfg.seed + (uint64_t)t * 0x100);
	}

	g_stop = 0;
	t0 = bb_now_ms();
	for (t = 0; t < g_cfg.threads; t++)
		pthread_create(&tids[t], NULL, worker_main, &workers[t]);

	/* Run for the requested wall-clock, then signal stop. */
	usleep((useconds_t)g_cfg.seconds * 1000000);
	g_stop = 1;
	for (t = 0; t < g_cfg.threads; t++)
		pthread_join(tids[t], NULL);
	elapsed = (bb_now_ms() - t0) / 1000.0;

	memset(total, 0, sizeof(total));
	memset(rtot, 0, sizeof(rtot));
	for (t = 0; t < g_cfg.threads; t++)
		for (i = 0; i < T_N; i++) {
			total[i] += workers[t].ok[i];
			rtot[i] += workers[t].retry[i];
		}
	grand = 0;
	for (i = 0; i < T_N; i++) grand += total[i];

	printf("# txn-type        committed   retries\n");
	for (i = 0; i < T_N; i++)
		printf("%-16s %10llu %9llu\n", g_tnames[i],
		    (unsigned long long)total[i], (unsigned long long)rtot[i]);
	printf("tproc-c %2d threads  %12.0f tpmC-like (%.0f txn/s over %.1fs)\n",
	    g_cfg.threads, (double)grand / elapsed * 60.0,
	    (double)grand / elapsed, elapsed);

	free(tids); free(workers);
	(void)g_env->close(g_env, 0);
	return 0;
}

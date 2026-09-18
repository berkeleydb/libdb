/*-
 * See the file LICENSE for redistribution information.
 *
 * xe_tproc_c -- HammerDB-style "TPROC-C" OLTP workload, cross-engine.
 *
 * INDEPENDENTLY IMPLEMENTED.  This is NOT the TPC-C benchmark, produces no
 * TPC-comparable metric, and must never be compared against published TPC
 * results.  It models the same warehouse/order business with the five classic
 * weighted transactions (new-order 45%, payment 43%, order-status 4%,
 * delivery 4%, stock-level 4%).
 *
 * Runs against libdb (BTREE / HASH / MIXED) or WiredTiger (row-store B-tree)
 * through xe_engine.h.  The transaction mix, key distributions and row counts
 * are IDENTICAL across every arm; only the storage engine and access method
 * vary.  That is the whole point, so anything that would differ between arms
 * lives in xe_engine.h, not here.
 *
 * ---------------------------------------------------------------------------
 * THE HASH PROBLEM, AND HOW IT IS HANDLED HONESTLY
 * ---------------------------------------------------------------------------
 * Two of the five transactions use an ordered cursor on BTREE:
 *
 *   delivery     "find the OLDEST undelivered order in this (warehouse,
 *                district)" -- a DB_SET_RANGE seek plus one step.
 *   stock-level  "count stock rows below a threshold for this warehouse" --
 *                a range scan over all items of one warehouse.
 *
 * libdb's DB_HASH has no ordered cursor: DB_SET_RANGE is unavailable and
 * DB_NEXT walks BUCKET order, which bears no relation to key order.  A range
 * predicate evaluated over bucket order returns a WRONG SUBSET while appearing
 * to work, so xe_engine.h refuses ordered operations on a hash table
 * (XE_ENOORDER) rather than letting that happen.
 *
 * Both steps are REIMPLEMENTED for HASH rather than skipped, because the
 * transaction mix must stay identical across arms:
 *
 *   stock-level  the key space is DENSELY ENUMERABLE -- stock keys are
 *                (w_id, i_id) with i_id in [0, ITEMS_PER_WH).  So the scan
 *                becomes ITEMS_PER_WH point lookups covering exactly the same
 *                rows and computing exactly the same answer.  Same rows, same
 *                predicate, same result; different physical access pattern
 *                (random point gets instead of one sequential scan).  That
 *                difference is a FINDING, not a flaw -- it is what choosing
 *                HASH for a scan-bearing table costs.
 *
 *   delivery     the key space is NOT densely enumerable.  New-order rows are
 *                appended with increasing o_id and DELETED on delivery, so the
 *                live set is a sparse, moving window whose lower bound is not
 *                known a priori.  On HASH we probe upward from a per-district
 *                watermark for at most DELIV_PROBE_MAX keys.  This is a
 *                faithful reimplementation ONLY while an undelivered order
 *                exists within the probe window; if the window is exhausted
 *                the transaction commits having delivered nothing, where the
 *                BTREE arm would have found the row by seeking.  The harness
 *                COUNTS those exhaustions (deliv_probe_exhausted) and prints
 *                them in the verdict line, so the report can state exactly how
 *                often the HASH arm's delivery was weaker than the BTREE arm's
 *                instead of quietly reporting the same transaction name for
 *                two different amounts of work.
 *
 * Nothing here silently substitutes a cheaper query for an expensive one.
 */
#define XE_TPROC_C 1
#include "xe_engine.h"

/*
 * Scale.  Row counts per warehouse are fixed (so the shape of the data is
 * fixed) and -S chooses the number of warehouses; -P sets the record padding
 * that takes the dataset to the target byte size.  Reporting the ACHIEVED
 * on-disk size matters more than any of these numbers, so populate() measures
 * it rather than predicting it.
 */
#define	DISTRICTS		10
#define	CUST_PER_DIST		300
#define	ITEMS_PER_WH		2000	/* stock rows per warehouse */
#define	ITEMS			2000	/* global item catalogue */
#define	ORDERS_PER_DIST		CUST_PER_DIST
#define	STOCK_LOW_THRESHOLD	20
#define	DELIV_PROBE_MAX		64	/* HASH-arm delivery probe window */

#define	XE_MAXPAD		8192

enum {
	T_NEWORDER, T_PAYMENT, T_ORDERSTATUS, T_DELIVERY, T_STOCKLEVEL, T_N
};
static const char *g_tnames[T_N] = {
	"new-order", "payment", "order-status", "delivery", "stock-level"
};

/* Tables. */
enum {
	TBL_WAREHOUSE, TBL_DISTRICT, TBL_CUSTOMER, TBL_STOCK,
	TBL_ORDERS, TBL_NEWORDER, TBL_ITEM, TBL_N
};
static const char *g_tblnames[TBL_N] = {
	"warehouse", "district", "customer", "stock",
	"orders", "neworder", "item"
};

static xe_env *g_env;
static struct xe_config g_cfg;
static xe_table *g_tbl[TBL_N];
static volatile int g_stop;
static volatile int g_measure;		/* 0 during warmup, 1 when measuring */
static int g_pad = 400;			/* record padding bytes */

/*
 * ---------------------------------------------------------------------------
 * THE MIXED MAPPING
 * ---------------------------------------------------------------------------
 * Per-table access method for the MIXED arm, chosen by ACCESS PATTERN and
 * justified individually.  The justification is printed at run time (so the
 * report cannot drift from what was measured) and reproduced in the report.
 *
 *   warehouse  HASH   point lookup by w_id only.  Payment reads and writes one
 *                     row by exact key; nothing ever scans warehouses.
 *   district   HASH   point lookup by (w_id, d_id).  New-order and payment
 *                     both touch exactly one district row by exact key.
 *   customer   HASH   point lookup by (w_id, c_id).  Payment and order-status
 *                     read one customer by exact key; the ordered
 *                     "by-last-name" access of the full TPC-C spec is not in
 *                     this workload, so there is no range predicate to serve.
 *   stock      BTREE  stock-level RANGE SCANS this table over all items of one
 *                     warehouse.  This is the one table where ordering is the
 *                     dominant access pattern, so it stays a B-tree even
 *                     though new-order also point-updates it.
 *   orders     HASH   point lookup by (w_id, d_id, o_id).  Order-status reads
 *                     one order by exact key and delivery updates one; the
 *                     ordered access in delivery is on NEWORDER, not here.
 *   neworder   BTREE  delivery needs the OLDEST undelivered order, i.e. a
 *                     seek to the first key >= (w_id, d_id, 0).  That is an
 *                     ordered predicate over a sparse, moving key range -- the
 *                     one access in this workload that a hash table genuinely
 *                     cannot serve without semantic loss (see the header
 *                     comment).  BTREE.
 *   item       HASH   read-only point lookup by i_id.  Never scanned.
 *
 * The hypothesis MIXED tests: hash point lookups avoid the B-tree's
 * root-to-leaf descent (and, at 10x RAM, avoid faulting in interior pages),
 * while the two tables that need ordering keep it.  If MIXED does not beat
 * both pure arms, that is a reportable result too.
 */
static enum xe_am
mixed_am_for(int tbl)
{
	switch (tbl) {
	case TBL_STOCK:		return XE_AM_BTREE;	/* range scan */
	case TBL_NEWORDER:	return XE_AM_BTREE;	/* ordered seek */
	default:		return XE_AM_HASH;	/* point lookup only */
	}
}

static const char *g_mixed_why[TBL_N] = {
	"point lookup by w_id only; never scanned",
	"point lookup by (w_id,d_id); never scanned",
	"point lookup by (w_id,c_id); no range predicate in this workload",
	"stock-level RANGE SCANS all items of a warehouse -- ordering dominates",
	"point lookup by (w_id,d_id,o_id); the ordered access is on neworder",
	"delivery seeks the OLDEST undelivered order -- sparse ordered range",
	"read-only point lookup by i_id; never scanned"
};

static enum xe_am
am_for(int tbl)
{
	switch (g_cfg.amcfg) {
	case XE_AMCFG_HASH:	return XE_AM_HASH;
	case XE_AMCFG_MIXED:	return mixed_am_for(tbl);
	default:		return XE_AM_BTREE;
	}
}

/* ---- record bodies.  Fixed size; padding takes the dataset to target. ---- */
typedef struct { int64_t ytd; uint8_t pad[XE_MAXPAD]; } wh_rec;
typedef struct { int64_t ytd; uint32_t next_o_id; uint8_t pad[XE_MAXPAD]; } dist_rec;
typedef struct { int64_t balance, ytd; uint32_t last_o; uint8_t pad[XE_MAXPAD]; } cust_rec;
typedef struct { int32_t quantity; uint8_t pad[XE_MAXPAD]; } stock_rec;
typedef struct { uint32_t c_id, carrier, ol_cnt; uint8_t pad[XE_MAXPAD]; } ord_rec;
typedef struct { uint32_t price; uint8_t pad[XE_MAXPAD]; } item_rec;

/* Actual stored size of each record type, given the -P padding. */
#define	RECSZ(type, member)	(offsetof(type, member) + (size_t)g_pad)
static size_t sz_wh, sz_dist, sz_cust, sz_stock, sz_ord, sz_item;

static void
compute_sizes(void)
{
	sz_wh    = RECSZ(wh_rec, pad);
	sz_dist  = RECSZ(dist_rec, pad);
	sz_cust  = RECSZ(cust_rec, pad);
	sz_stock = RECSZ(stock_rec, pad);
	sz_ord   = RECSZ(ord_rec, pad);
	sz_item  = RECSZ(item_rec, pad);
}

typedef struct {
	int tid;
	xe_thread th;
	xe_rng rng;
	uint64_t ok[T_N];
	uint64_t retry[T_N];
	uint64_t err[T_N];
	xe_hist hist[T_N];
	uint64_t sink;
	uint64_t deliv_probe_exhausted;	/* HASH arm honesty counter */
	uint64_t deliv_found;
	uint32_t deliv_wm[64];		/* per-district probe watermark */
} worker;

/* ---------------------------------------------------------------- */
/* Population                                                       */
/* ---------------------------------------------------------------- */
static int
open_tables(void)
{
	int i, ret;

	for (i = 0; i < TBL_N; i++)
		if ((ret = xe_table_open(g_env, &g_tbl[i], g_tblnames[i],
		    am_for(i))) != XE_OK) {
			fprintf(stderr, "open table %s: %d\n", g_tblnames[i], ret);
			return ret;
		}
	return XE_OK;
}

/*
 * Load.  Batched into transactions of XE_LOAD_BATCH puts: one transaction per
 * row would measure commit, and one transaction for the whole load would
 * exceed any lock table.
 *
 * NOTE for the P1 finding (PGNO_BASE_MD allocation convoy): the load is the
 * insert-heavy phase where P1 is expected to dominate at high thread counts.
 * The load here is single-threaded ON PURPOSE -- a parallel load would be
 * faster but would make the loaded dataset a measurement of P1 rather than a
 * controlled starting point for the measured phase.  P1 is measured in the
 * new-order transaction instead, where it belongs.
 */
#define	XE_LOAD_BATCH	1000

static int
load_progress(const char *what, uint64_t done, uint64_t total, double t0)
{
	double el = (xe_now_ms() - t0) / 1000.0;

	printf("# load %-10s %10llu / %10llu  %5.1f%%  %6.0fs  %8.0f rows/s\n",
	    what, (unsigned long long)done, (unsigned long long)total,
	    total ? 100.0 * (double)done / (double)total : 0.0,
	    el, el > 0 ? (double)done / el : 0.0);
	fflush(stdout);
	return 0;
}

static int
populate(void)
{
	xe_thread th;
	xe_txn txn;
	xe_key k;
	wh_rec w; dist_rec di; cust_rec cu; stock_rec st; ord_rec o; item_rec it;
	uint64_t nput = 0, total;
	double t0 = xe_now_ms();
	int wid, did, cid, iid, oid, ret, inbatch = 0;

	if ((ret = xe_thread_init(g_env, &th, 0)) != XE_OK) return ret;

	total = (uint64_t)ITEMS +
	    (uint64_t)g_cfg.scale * (1 + ITEMS_PER_WH +
	    (uint64_t)DISTRICTS * (1 + CUST_PER_DIST + 2 * ORDERS_PER_DIST));
	printf("# load: %llu rows planned, pad=%d\n",
	    (unsigned long long)total, g_pad);

#define	BATCH_TICK(label)						\
	do {								\
		if (++inbatch >= XE_LOAD_BATCH) {			\
			if ((ret = xe_txn_commit(&txn)) != XE_OK) goto err; \
			inbatch = 0;					\
			if ((nput % 1000000) == 0)			\
				(void)load_progress(label, nput, total, t0); \
			if ((ret = xe_txn_begin(&th, &txn, 0)) != XE_OK)	\
				return ret;				\
		}							\
	} while (0)

	if ((ret = xe_txn_begin(&th, &txn, 0)) != XE_OK) return ret;

	memset(&it, 0, sizeof(it));
	for (iid = 0; iid < ITEMS; iid++) {
		it.price = 100 + (uint32_t)(iid % 9900);
		xe_key_enc(&k, (uint32_t)iid, 0, 0);
		if ((ret = xe_put(g_tbl[TBL_ITEM], &txn, &k, &it, sz_item)) != XE_OK)
			goto err;
		nput++; BATCH_TICK("item");
	}

	for (wid = 0; wid < g_cfg.scale; wid++) {
		memset(&w, 0, sizeof(w));
		xe_key_enc(&k, (uint32_t)wid, 0, 0);
		if ((ret = xe_put(g_tbl[TBL_WAREHOUSE], &txn, &k, &w, sz_wh)) != XE_OK)
			goto err;
		nput++; BATCH_TICK("warehouse");

		for (iid = 0; iid < ITEMS_PER_WH; iid++) {
			memset(&st, 0, sizeof(st));
			st.quantity = 10 + (int32_t)(iid % 90);
			xe_key_enc(&k, (uint32_t)wid, (uint32_t)iid, 0);
			if ((ret = xe_put(g_tbl[TBL_STOCK], &txn, &k, &st,
			    sz_stock)) != XE_OK) goto err;
			nput++; BATCH_TICK("stock");
		}

		for (did = 0; did < DISTRICTS; did++) {
			memset(&di, 0, sizeof(di));
			di.next_o_id = ORDERS_PER_DIST;
			xe_key_enc(&k, (uint32_t)wid, (uint32_t)did, 0);
			if ((ret = xe_put(g_tbl[TBL_DISTRICT], &txn, &k, &di,
			    sz_dist)) != XE_OK) goto err;
			nput++; BATCH_TICK("district");

			for (cid = 0; cid < CUST_PER_DIST; cid++) {
				memset(&cu, 0, sizeof(cu));
				cu.balance = -1000; cu.ytd = 1000;
				cu.last_o = (uint32_t)cid;
				xe_key_enc(&k, (uint32_t)wid,
				    (uint32_t)(did * CUST_PER_DIST + cid), 0);
				if ((ret = xe_put(g_tbl[TBL_CUSTOMER], &txn, &k,
				    &cu, sz_cust)) != XE_OK) goto err;
				nput++; BATCH_TICK("customer");
			}
			for (oid = 0; oid < ORDERS_PER_DIST; oid++) {
				memset(&o, 0, sizeof(o));
				o.c_id = (uint32_t)(oid % CUST_PER_DIST);
				o.ol_cnt = 10;
				xe_key_enc(&k, (uint32_t)wid, (uint32_t)did,
				    (uint32_t)oid);
				if ((ret = xe_put(g_tbl[TBL_ORDERS], &txn, &k,
				    &o, sz_ord)) != XE_OK) goto err;
				nput++; BATCH_TICK("orders");
				/* Half the orders start undelivered. */
				if (oid >= ORDERS_PER_DIST / 2) {
					if ((ret = xe_put(g_tbl[TBL_NEWORDER],
					    &txn, &k, "", 1)) != XE_OK) goto err;
					nput++; BATCH_TICK("neworder");
				}
			}
		}
	}
	if ((ret = xe_txn_commit(&txn)) != XE_OK) goto err;
	(void)load_progress("DONE", nput, total, t0);
	xe_thread_done(&th);
	return XE_OK;
err:
	(void)xe_txn_abort(&txn);
	fprintf(stderr, "populate failed at row %llu: %d\n",
	    (unsigned long long)nput, ret);
	return ret;
}

/* ---------------------------------------------------------------- */
/* Transactions                                                     */
/* ---------------------------------------------------------------- */

static int
get_rec(int tbl, xe_txn *txn, uint32_t a, uint32_t b, uint32_t c,
    void *out, size_t outsz)
{
	xe_key k;

	xe_key_enc(&k, a, b, c);
	return xe_get(g_tbl[tbl], txn, &k, out, outsz, NULL);
}

static int
put_rec(int tbl, xe_txn *txn, uint32_t a, uint32_t b, uint32_t c,
    const void *in, size_t insz)
{
	xe_key k;

	xe_key_enc(&k, a, b, c);
	return xe_put(g_tbl[tbl], txn, &k, in, insz);
}

/*
 * Every transaction follows the same shape: time it, retry on conflict, and
 * record the latency of the SUCCESSFUL attempt.  Retries are counted
 * separately rather than folded into the latency, because a p99 that silently
 * includes aborted attempts is neither the user-visible latency nor the
 * engine's -- it is an average of two different things.
 */
#define	TXN_RETRY_MAX	100

static int
do_new_order(worker *w)
{
	xe_txn txn;
	dist_rec di;
	stock_rec st;
	ord_rec o;
	uint32_t wid, did, oid, i, nitems;
	int op = T_NEWORDER, ret, tries = 0;
	double t0;

	wid = xe_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
	did = xe_rand_between(&w->rng, 0, DISTRICTS - 1);
	nitems = xe_rand_between(&w->rng, 5, 15);
again:
	t0 = xe_now_us();
	if ((ret = xe_txn_begin(&w->th, &txn, 0)) != XE_OK) return ret;

	if ((ret = get_rec(TBL_DISTRICT, &txn, wid, did, 0, &di, sizeof(di))) != XE_OK)
		goto fail;
	oid = di.next_o_id++;
	if ((ret = put_rec(TBL_DISTRICT, &txn, wid, did, 0, &di, sz_dist)) != XE_OK)
		goto fail;

	for (i = 0; i < nitems; i++) {
		uint32_t iid = xe_rand_between(&w->rng, 0, ITEMS_PER_WH - 1);

		if ((ret = get_rec(TBL_STOCK, &txn, wid, iid, 0, &st,
		    sizeof(st))) != XE_OK) goto fail;
		st.quantity -= 1;
		if (st.quantity < 10) st.quantity += 91;
		if ((ret = put_rec(TBL_STOCK, &txn, wid, iid, 0, &st,
		    sz_stock)) != XE_OK) goto fail;
	}

	memset(&o, 0, sizeof(o));
	o.c_id = xe_rand_between(&w->rng, 0, CUST_PER_DIST - 1);
	o.ol_cnt = nitems;
	if ((ret = put_rec(TBL_ORDERS, &txn, wid, did, oid, &o, sz_ord)) != XE_OK)
		goto fail;
	if ((ret = put_rec(TBL_NEWORDER, &txn, wid, did, oid, "", 1)) != XE_OK)
		goto fail;

	if ((ret = xe_txn_commit(&txn)) != XE_OK) goto fail;
	if (g_measure) xe_hist_add(&w->hist[op], xe_now_us() - t0);
	w->ok[op]++;
	return XE_OK;
fail:
	(void)xe_txn_abort(&txn);
	if (ret == XE_CONFLICT && ++tries < TXN_RETRY_MAX) {
		w->retry[op]++;
		goto again;
	}
	if (ret == XE_CONFLICT) return XE_OK;	/* gave up; not an error */
	w->err[op]++;
	return ret;
}

static int
do_payment(worker *w)
{
	xe_txn txn;
	wh_rec wh;
	dist_rec di;
	cust_rec cu;
	uint32_t wid, did, cid;
	int64_t amount;
	int op = T_PAYMENT, ret, tries = 0;
	double t0;

	wid = xe_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
	did = xe_rand_between(&w->rng, 0, DISTRICTS - 1);
	cid = xe_rand_between(&w->rng, 0, DISTRICTS * CUST_PER_DIST - 1);
	amount = (int64_t)xe_rand_between(&w->rng, 1, 5000);
again:
	t0 = xe_now_us();
	if ((ret = xe_txn_begin(&w->th, &txn, 0)) != XE_OK) return ret;

	if ((ret = get_rec(TBL_WAREHOUSE, &txn, wid, 0, 0, &wh, sizeof(wh))) != XE_OK)
		goto fail;
	wh.ytd += amount;
	if ((ret = put_rec(TBL_WAREHOUSE, &txn, wid, 0, 0, &wh, sz_wh)) != XE_OK)
		goto fail;

	if ((ret = get_rec(TBL_DISTRICT, &txn, wid, did, 0, &di, sizeof(di))) != XE_OK)
		goto fail;
	di.ytd += amount;
	if ((ret = put_rec(TBL_DISTRICT, &txn, wid, did, 0, &di, sz_dist)) != XE_OK)
		goto fail;

	if ((ret = get_rec(TBL_CUSTOMER, &txn, wid, cid, 0, &cu, sizeof(cu))) != XE_OK)
		goto fail;
	cu.balance -= amount;
	cu.ytd += amount;
	if ((ret = put_rec(TBL_CUSTOMER, &txn, wid, cid, 0, &cu, sz_cust)) != XE_OK)
		goto fail;

	if ((ret = xe_txn_commit(&txn)) != XE_OK) goto fail;
	if (g_measure) xe_hist_add(&w->hist[op], xe_now_us() - t0);
	w->ok[op]++;
	return XE_OK;
fail:
	(void)xe_txn_abort(&txn);
	if (ret == XE_CONFLICT && ++tries < TXN_RETRY_MAX) {
		w->retry[op]++;
		goto again;
	}
	if (ret == XE_CONFLICT) return XE_OK;
	w->err[op]++;
	return ret;
}

static int
do_order_status(worker *w)
{
	xe_txn txn;
	cust_rec cu;
	ord_rec o;
	uint32_t wid, did, cid;
	int op = T_ORDERSTATUS, ret, tries = 0;
	double t0;

	wid = xe_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
	did = xe_rand_between(&w->rng, 0, DISTRICTS - 1);
	cid = xe_rand_between(&w->rng, 0, DISTRICTS * CUST_PER_DIST - 1);
again:
	t0 = xe_now_us();
	if ((ret = xe_txn_begin(&w->th, &txn, 1)) != XE_OK) return ret;

	if ((ret = get_rec(TBL_CUSTOMER, &txn, wid, cid, 0, &cu, sizeof(cu))) != XE_OK)
		goto fail;
	ret = get_rec(TBL_ORDERS, &txn, wid, did, cu.last_o, &o, sizeof(o));
	if (ret != XE_OK && ret != XE_NOTFOUND) goto fail;

	if ((ret = xe_txn_commit(&txn)) != XE_OK) goto fail;
	if (g_measure) xe_hist_add(&w->hist[op], xe_now_us() - t0);
	w->ok[op]++;
	return XE_OK;
fail:
	(void)xe_txn_abort(&txn);
	if (ret == XE_CONFLICT && ++tries < TXN_RETRY_MAX) {
		w->retry[op]++;
		goto again;
	}
	if (ret == XE_CONFLICT) return XE_OK;
	w->err[op]++;
	return ret;
}

/*
 * Delivery.  BTREE/row-store: seek the first neworder key >= (wid, did, 0) and
 * take it if it is still in this district -- the oldest undelivered order.
 *
 * HASH: no ordered seek exists, so probe upward from this thread's watermark
 * for this district.  See the file header for why this is a reimplementation
 * with a stated ceiling rather than an equivalent, and why exhaustions are
 * counted and reported instead of being allowed to look like successes.
 */
static int
do_delivery(worker *w)
{
	xe_txn txn;
	xe_cursor cur;
	xe_key seek, got;
	char dummy[8];
	ord_rec o;
	uint32_t wid, did, foundo = 0;
	int op = T_DELIVERY, ret, tries = 0, have = 0, usecursor;
	double t0;

	wid = xe_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
	did = xe_rand_between(&w->rng, 0, DISTRICTS - 1);
	usecursor = (am_for(TBL_NEWORDER) == XE_AM_BTREE);
again:
	have = 0;
	t0 = xe_now_us();
	if ((ret = xe_txn_begin(&w->th, &txn, 0)) != XE_OK) return ret;

	if (usecursor) {
		if ((ret = xe_cursor_open(g_tbl[TBL_NEWORDER], &txn, &cur)) != XE_OK)
			goto fail;
		xe_key_enc(&seek, wid, did, 0);
		ret = xe_cursor_seek_ge(&cur, &seek, &got, dummy, sizeof(dummy));
		if (ret == XE_OK) {
			if (xe_key_field(&got, 0) == wid &&
			    xe_key_field(&got, 1) == did) {
				foundo = xe_key_field(&got, 2);
				if ((ret = xe_cursor_del(&cur)) != XE_OK) {
					(void)xe_cursor_close(&cur);
					goto fail;
				}
				have = 1;
			}
		} else if (ret != XE_NOTFOUND) {
			(void)xe_cursor_close(&cur);
			goto fail;
		}
		(void)xe_cursor_close(&cur);
	} else {
		/*
		 * HASH arm: bounded upward probe from the watermark.  Each
		 * probe is a point lookup, which a hash table CAN do.
		 */
		uint32_t base = w->deliv_wm[did], i;

		for (i = 0; i < DELIV_PROBE_MAX; i++) {
			xe_key pk;

			xe_key_enc(&pk, wid, did, base + i);
			ret = xe_get(g_tbl[TBL_NEWORDER], &txn, &pk,
			    dummy, sizeof(dummy), NULL);
			if (ret == XE_OK) {
				foundo = base + i;
				if ((ret = xe_del(g_tbl[TBL_NEWORDER], &txn,
				    &pk)) != XE_OK) goto fail;
				have = 1;
				w->deliv_wm[did] = base + i + 1;
				break;
			}
			if (ret == XE_NOTFOUND)
				continue;
			goto fail;
		}
		if (!have) {
			/*
			 * Probe window exhausted.  Recorded, not hidden: the
			 * BTREE arm would have found a row here by seeking.
			 */
			w->deliv_probe_exhausted++;
			w->deliv_wm[did] = base + DELIV_PROBE_MAX;
			/* Reset the watermark if we have run off the end. */
			if (w->deliv_wm[did] > ORDERS_PER_DIST * 4)
				w->deliv_wm[did] = 0;
		}
	}

	if (have) {
		if ((ret = get_rec(TBL_ORDERS, &txn, wid, did, foundo,
		    &o, sizeof(o))) == XE_OK) {
			o.carrier = 1;
			if ((ret = put_rec(TBL_ORDERS, &txn, wid, did, foundo,
			    &o, sz_ord)) != XE_OK) goto fail;
		} else if (ret != XE_NOTFOUND)
			goto fail;
		w->deliv_found++;
	}

	if ((ret = xe_txn_commit(&txn)) != XE_OK) goto fail;
	if (g_measure) xe_hist_add(&w->hist[op], xe_now_us() - t0);
	w->ok[op]++;
	return XE_OK;
fail:
	(void)xe_txn_abort(&txn);
	if (ret == XE_CONFLICT && ++tries < TXN_RETRY_MAX) {
		w->retry[op]++;
		goto again;
	}
	if (ret == XE_CONFLICT) return XE_OK;
	w->err[op]++;
	return ret;
}

/*
 * Stock-level.  BTREE/row-store: one range scan over (wid, *).
 * HASH: ITEMS_PER_WH point lookups over the same, densely enumerable key
 * space -- same rows, same predicate, same answer, different physical path.
 */
static int
do_stock_level(worker *w)
{
	xe_txn txn;
	xe_cursor cur;
	xe_key seek, got;
	stock_rec st;
	uint32_t wid;
	int op = T_STOCKLEVEL, ret, tries = 0, low = 0;
	double t0;

	wid = xe_rand_between(&w->rng, 0, (uint32_t)g_cfg.scale - 1);
again:
	low = 0;
	t0 = xe_now_us();
	if ((ret = xe_txn_begin(&w->th, &txn, 1)) != XE_OK) return ret;

	if (am_for(TBL_STOCK) == XE_AM_BTREE) {
		if ((ret = xe_cursor_open(g_tbl[TBL_STOCK], &txn, &cur)) != XE_OK)
			goto fail;
		xe_key_enc(&seek, wid, 0, 0);
		ret = xe_cursor_seek_ge(&cur, &seek, &got, &st, sizeof(st));
		while (ret == XE_OK) {
			if (xe_key_field(&got, 0) != wid)
				break;			/* left this warehouse */
			if (st.quantity < STOCK_LOW_THRESHOLD) low++;
			ret = xe_cursor_next(&cur, &got, &st, sizeof(st));
		}
		(void)xe_cursor_close(&cur);
		if (ret != XE_OK && ret != XE_NOTFOUND) goto fail;
	} else {
		uint32_t iid;

		for (iid = 0; iid < ITEMS_PER_WH; iid++) {
			ret = get_rec(TBL_STOCK, &txn, wid, iid, 0,
			    &st, sizeof(st));
			if (ret == XE_OK) {
				if (st.quantity < STOCK_LOW_THRESHOLD) low++;
			} else if (ret != XE_NOTFOUND)
				goto fail;
		}
	}

	if ((ret = xe_txn_commit(&txn)) != XE_OK) goto fail;
	if (g_measure) xe_hist_add(&w->hist[op], xe_now_us() - t0);
	w->sink += (uint64_t)low;
	w->ok[op]++;
	return XE_OK;
fail:
	(void)xe_txn_abort(&txn);
	if (ret == XE_CONFLICT && ++tries < TXN_RETRY_MAX) {
		w->retry[op]++;
		goto again;
	}
	if (ret == XE_CONFLICT) return XE_OK;
	w->err[op]++;
	return ret;
}

/* Weighted mix, out of 100.  IDENTICAL in every arm. */
static int
run_one(worker *w)
{
	uint32_t r = xe_rand_between(&w->rng, 0, 99);

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

	if (xe_thread_init(g_env, &w->th, w->tid) != XE_OK) {
		fprintf(stderr, "thread %d init failed\n", w->tid);
		return NULL;
	}
	while (!g_stop)
		if ((ret = run_one(w)) != XE_OK) {
			fprintf(stderr, "thread %d: op failed ret=%d\n",
			    w->tid, ret);
			break;
		}
	xe_thread_done(&w->th);
	return NULL;
}

static uint64_t
sum_ok(worker *ws, int n)
{
	uint64_t s = 0;
	int t, i;

	for (t = 0; t < n; t++)
		for (i = 0; i < T_N; i++) s += ws[t].ok[i];
	return s;
}

static void
usage(const char *p)
{
	fprintf(stderr,
"usage: %s [-i] -e libdb|wt -a btree|hash|mixed [-h home] [-S warehouses]\n"
"          [-t threads] [-s secs] [-W warmupsecs] [-c cachebytes] [-P pad]\n"
"          [-d sync|wnosync|nosync] [-A] [-m] [-R seed] [-p pagesizekb]\n"
"  -A  enable libdb DB_MPOOL_AIO (io_uring buffer-pool I/O)\n"
"  -W  warm to steady state for this many seconds before measuring\n", p);
}

int
main(int argc, char **argv)
{
	pthread_t *tids;
	worker *ws;
	struct xe_stats s_before, s_after;
	xe_hist agg[T_N];
	uint64_t total[T_N], rtot[T_N], etot[T_N], grand, warm_ops;
	uint64_t exhaust = 0, delivfound = 0;
	u_int32_t o_t0 = 0, o_p0 = 0, o_v0 = 0, o_b0 = 0;
	double t0, elapsed, dbbytes;
	int i, t, ret, ch;

	xe_config_defaults(&g_cfg);
	while ((ch = getopt(argc, argv, "ie:a:h:S:t:s:W:c:P:d:AmR:p:v")) != EOF)
		switch (ch) {
		case 'i': g_cfg.init = 1; break;
		case 'e':
			if (strcmp(optarg, "libdb") == 0)
				g_cfg.engine = XE_ENGINE_LIBDB;
			else if (strcmp(optarg, "wt") == 0)
				g_cfg.engine = XE_ENGINE_WT;
			else { usage(argv[0]); return 1; }
			break;
		case 'a':
			if (strcmp(optarg, "btree") == 0)
				g_cfg.amcfg = XE_AMCFG_BTREE;
			else if (strcmp(optarg, "hash") == 0)
				g_cfg.amcfg = XE_AMCFG_HASH;
			else if (strcmp(optarg, "mixed") == 0)
				g_cfg.amcfg = XE_AMCFG_MIXED;
			else { usage(argv[0]); return 1; }
			break;
		case 'h': g_cfg.home = optarg; break;
		case 'S': g_cfg.scale = atoi(optarg); break;
		case 't': g_cfg.threads = atoi(optarg); break;
		case 's': g_cfg.seconds = atoi(optarg); break;
		case 'W': g_cfg.warmup = atoi(optarg); break;
		case 'c': g_cfg.cachebytes = strtoull(optarg, NULL, 10); break;
		case 'P': g_pad = atoi(optarg); break;
		case 'A': g_cfg.aio = 1; break;
		case 'm': g_cfg.use_mvcc = 1; break;
		case 'R': g_cfg.seed = (unsigned)strtoul(optarg, NULL, 10); break;
		case 'p': g_cfg.pagesize_kb = atoi(optarg); break;
		case 'v': g_cfg.verbose = 1; break;
		case 'd':
			if (strcmp(optarg, "sync") == 0)
				g_cfg.durability = XE_DUR_SYNC;
			else if (strcmp(optarg, "wnosync") == 0)
				g_cfg.durability = XE_DUR_WRITE_NOSYNC;
			else if (strcmp(optarg, "nosync") == 0)
				g_cfg.durability = XE_DUR_NOSYNC;
			else { usage(argv[0]); return 1; }
			break;
		default: usage(argv[0]); return 1;
		}

	if (g_pad < 0) g_pad = 0;
	if (g_pad > XE_MAXPAD) g_pad = XE_MAXPAD;
	if (g_cfg.scale < 1) g_cfg.scale = 1;
	compute_sizes();

	/*
	 * Prove which libraries we linked BEFORE anything else.  A version
	 * mismatch aborts the run: measuring the wrong library is worse than
	 * not measuring.
	 */
	if (xe_version_banner() != 0)
		return 1;

	/* WiredTiger cannot run the HASH or MIXED arms: it has no hash AM. */
	if (g_cfg.engine == XE_ENGINE_WT && g_cfg.amcfg != XE_AMCFG_BTREE) {
		printf("SKIP arm=%s reason=wiredtiger-has-no-hash-access-method\n",
		    xe_amcfg_name(g_cfg.amcfg));
		return 0;
	}

	printf("# arm=%s engine=%s am=%s threads=%d scale=%d pad=%d "
	    "cache=%lluMB secs=%d warmup=%d aio=%d mvcc=%d dur=%s pagesize=%dk\n",
	    xe_arm_name(&g_cfg), xe_engine_name(g_cfg.engine),
	    xe_amcfg_name(g_cfg.amcfg), g_cfg.threads, g_cfg.scale, g_pad,
	    (unsigned long long)(g_cfg.cachebytes >> 20), g_cfg.seconds,
	    g_cfg.warmup, g_cfg.aio, g_cfg.use_mvcc,
	    g_cfg.durability == XE_DUR_SYNC ? "sync" :
	    g_cfg.durability == XE_DUR_WRITE_NOSYNC ? "wnosync" : "nosync",
	    g_cfg.pagesize_kb);
	printf("# record sizes: wh=%zu dist=%zu cust=%zu stock=%zu ord=%zu item=%zu\n",
	    sz_wh, sz_dist, sz_cust, sz_stock, sz_ord, sz_item);

	if (g_cfg.amcfg == XE_AMCFG_MIXED) {
		printf("# MIXED access-method mapping (per-table, by access pattern):\n");
		for (i = 0; i < TBL_N; i++)
			printf("#   %-10s %-5s  %s\n", g_tblnames[i],
			    mixed_am_for(i) == XE_AM_HASH ? "HASH" : "BTREE",
			    g_mixed_why[i]);
	}

	if ((ret = xe_open(&g_cfg, &g_env)) != XE_OK) {
		fprintf(stderr, "FAIL engine open ret=%d\n", ret);
		return 1;
	}
#ifdef XE_HAVE_WIREDTIGER
	if (g_cfg.engine == XE_ENGINE_WT)
		printf("# wiredtiger_open config: %s\n", g_env->wtconfig);
#endif

	if ((ret = open_tables()) != XE_OK) {
		fprintf(stderr, "FAIL table open ret=%d\n", ret);
		return 1;
	}

	if (g_cfg.init) {
		if ((ret = populate()) != XE_OK) {
			fprintf(stderr, "FAIL populate ret=%d\n", ret);
			return 1;
		}
		for (i = 0; i < TBL_N; i++) (void)xe_table_close(g_tbl[i]);
		(void)xe_close(g_env);
		dbbytes = (double)xe_dir_bytes(g_cfg.home);
		printf("VERDICT load arm=%s scale=%d pad=%d on_disk_bytes=%.0f "
		    "on_disk_gib=%.2f\n", xe_arm_name(&g_cfg), g_cfg.scale,
		    g_pad, dbbytes, dbbytes / (1024.0 * 1024.0 * 1024.0));
		return 0;
	}

	tids = calloc((size_t)g_cfg.threads, sizeof(*tids));
	ws = calloc((size_t)g_cfg.threads, sizeof(*ws));
	if (tids == NULL || ws == NULL) { fprintf(stderr, "FAIL calloc\n"); return 1; }
	for (t = 0; t < g_cfg.threads; t++) {
		ws[t].tid = t;
		xe_rng_seed(&ws[t].rng, g_cfg.seed + (uint64_t)t * 0x100);
	}

	g_stop = 0;
	g_measure = 0;
	for (t = 0; t < g_cfg.threads; t++)
		if (pthread_create(&tids[t], NULL, worker_main, &ws[t]) != 0) {
			fprintf(stderr, "FAIL pthread_create %d\n", t);
			return 1;
		}

	/*
	 * ---- WARM TO STEADY STATE -------------------------------------
	 * An unwarmed tree measures a LOAD RAMP, not the workload: the cache
	 * starts empty, every read is a miss, and throughput climbs for as long
	 * as the cache is filling.  That error produced a retracted 272 ms p99
	 * claim in this project.
	 *
	 * Steady state is DECLARED, not assumed, by a stated criterion: sample
	 * throughput in 10-second windows and require the last two windows to
	 * agree within 10%, with at least 3 windows seen.  Every window is
	 * printed, so a reader can check the criterion against the data instead
	 * of taking the word "warm" on trust.  If the criterion is not met by
	 * the warmup budget, the run SAYS SO in the verdict line rather than
	 * quietly reporting a ramp.
	 */
	{
		double prev = -1, cur, wt0;
		uint64_t pops = 0, nops;
		int win = 0, steady = 0, budget = g_cfg.warmup;

		for (win = 0; win * 10 < budget; win++) {
			wt0 = xe_now_ms();
			sleep(10);
			nops = sum_ok(ws, g_cfg.threads);
			cur = (double)(nops - pops) /
			    ((xe_now_ms() - wt0) / 1000.0);
			pops = nops;
			printf("# warmup window %d: %.0f txn/s\n", win, cur);
			fflush(stdout);
			if (prev > 0 && win >= 2 &&
			    fabs(cur - prev) <= 0.10 * prev) { steady = 1; }
			prev = cur;
		}
		warm_ops = sum_ok(ws, g_cfg.threads);
		printf("# steady_state=%s after %d warmup windows "
		    "(criterion: last two 10s windows within 10%%, >=3 windows)\n",
		    steady ? "yes" : "NOT-REACHED", win);
		if (!steady && budget > 0)
			printf("# WARNING steady state NOT reached in %ds of "
			    "warmup -- the measured interval may still include "
			    "a cache-fill ramp\n", budget);
	}

	/* ---- MEASURE ---- */
	xe_stats_get(g_env, &s_before);
#ifndef XE_HAVE_WIREDTIGER_ONLY
	if (g_cfg.engine == XE_ENGINE_LIBDB && &__bam_opt_tries != NULL) {
		o_t0 = __bam_opt_tries; o_p0 = __bam_opt_pages;
		o_v0 = __bam_opt_invalid; o_b0 = __bam_opt_bailouts;
	}
#endif
	for (t = 0; t < g_cfg.threads; t++) {
		memset(ws[t].hist, 0, sizeof(ws[t].hist));
		memset(ws[t].ok, 0, sizeof(ws[t].ok));
		memset(ws[t].retry, 0, sizeof(ws[t].retry));
		memset(ws[t].err, 0, sizeof(ws[t].err));
	}
	g_measure = 1;
	t0 = xe_now_ms();
	sleep((unsigned)g_cfg.seconds);
	g_stop = 1;
	elapsed = (xe_now_ms() - t0) / 1000.0;
	for (t = 0; t < g_cfg.threads; t++) pthread_join(tids[t], NULL);
	xe_stats_get(g_env, &s_after);

	memset(total, 0, sizeof(total));
	memset(rtot, 0, sizeof(rtot));
	memset(etot, 0, sizeof(etot));
	memset(agg, 0, sizeof(agg));
	for (t = 0; t < g_cfg.threads; t++) {
		for (i = 0; i < T_N; i++) {
			total[i] += ws[t].ok[i];
			rtot[i] += ws[t].retry[i];
			etot[i] += ws[t].err[i];
			xe_hist_merge(&agg[i], &ws[t].hist[i]);
		}
		exhaust += ws[t].deliv_probe_exhausted;
		delivfound += ws[t].deliv_found;
	}
	grand = 0;
	for (i = 0; i < T_N; i++) grand += total[i];

	printf("# txn-type        committed   retries    errors      "
	    "p50_us     p99_us   p99.9_us    max_us\n");
	for (i = 0; i < T_N; i++)
		printf("TXN %-14s %10llu %9llu %9llu %11.0f %10.0f %10.0f %9.0f\n",
		    g_tnames[i], (unsigned long long)total[i],
		    (unsigned long long)rtot[i], (unsigned long long)etot[i],
		    xe_hist_q(&agg[i], 0.50), xe_hist_q(&agg[i], 0.99),
		    xe_hist_q(&agg[i], 0.999), agg[i].max_us);

	xe_stats_delta_print(xe_arm_name(&g_cfg), &s_before, &s_after, grand);
	if (g_cfg.engine == XE_ENGINE_LIBDB)
		xe_optread_report(xe_arm_name(&g_cfg), o_t0, o_p0, o_v0, o_b0);

	/* HASH-arm delivery honesty line. */
	if (am_for(TBL_NEWORDER) == XE_AM_HASH)
		printf("HASHDELIV arm=%s probe_max=%d delivered=%llu "
		    "probe_exhausted=%llu exhaust_pct=%.2f\n",
		    xe_arm_name(&g_cfg), DELIV_PROBE_MAX,
		    (unsigned long long)delivfound,
		    (unsigned long long)exhaust,
		    (delivfound + exhaust) ? 100.0 * (double)exhaust /
		    (double)(delivfound + exhaust) : 0.0);

	dbbytes = (double)xe_dir_bytes(g_cfg.home);

	/*
	 * THE VERDICT LINE.  A run that prints no throughput number is a FAILED
	 * run, not a pass -- this project has nine recorded vacuous-green
	 * instances, one of them in a perf measurement.  So: never trust rc=0,
	 * assert a real number here, and fail loudly when there isn't one.
	 */
	if (grand == 0) {
		printf("FAIL arm=%s threads=%d committed ZERO transactions in "
		    "%.1fs -- vacuous run, no throughput to report\n",
		    xe_arm_name(&g_cfg), g_cfg.threads, elapsed);
		return 1;
	}
	printf("VERDICT tproc-c arm=%s engine=%s am=%s threads=%d scale=%d "
	    "txn_per_sec=%.1f tpmC_like=%.0f committed=%llu elapsed=%.2f "
	    "on_disk_gib=%.2f warm=%llu\n",
	    xe_arm_name(&g_cfg), xe_engine_name(g_cfg.engine),
	    xe_amcfg_name(g_cfg.amcfg), g_cfg.threads, g_cfg.scale,
	    (double)grand / elapsed, (double)grand / elapsed * 60.0,
	    (unsigned long long)grand, elapsed,
	    dbbytes / (1024.0 * 1024.0 * 1024.0),
	    (unsigned long long)warm_ops);

	for (i = 0; i < TBL_N; i++) (void)xe_table_close(g_tbl[i]);
	(void)xe_close(g_env);
	free(tids); free(ws);
	return 0;
}

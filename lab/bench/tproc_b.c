/*-
 * See the file LICENSE for redistribution information.
 *
 * tproc_b -- a HammerDB-style "TPROC-B" debit/credit workload for libdb.
 *
 * Independently implemented; not the TPC-B benchmark and not comparable to
 * TPC results.  It is the classic single-transaction money-transfer workload
 * (the modern, parameterized successor to examples/c/ex_tpcb.c): each
 * transaction picks a random account, teller, and branch, applies a delta to
 * all three balances, and appends a history row -- one short write
 * transaction touching four records.
 *
 * It exists alongside the richer TPROC-C so the suite has a write-only,
 * minimal-logic transaction to isolate pure transaction/commit/log overhead,
 * and (via the shared toggle framework) to measure that overhead with and
 * without each BDB safety feature.
 *
 * Schema:
 *   account  key=a_id  -> balance     (SCALE * ACCOUNTS_PER rows)
 *   teller   key=t_id  -> balance     (SCALE * TELLERS_PER rows)
 *   branch   key=b_id  -> balance     (SCALE rows)
 *   history  recno     -> {a,t,b,delta}  (append-only)
 */
#include "bdb_bench.h"

#define	ACCOUNTS_PER	100000
#define	TELLERS_PER	100
#define	BRANCHES_PER	1

static DB_ENV	*g_env;
static bb_config g_cfg;
static DB	*g_acct, *g_tell, *g_branch, *g_hist;
static int	 g_naccts, g_ntellers, g_nbranches;
static volatile int g_stop;

typedef struct { int64_t balance; uint8_t pad[56]; } bal_rec;
typedef struct { uint32_t a, t, b; int32_t delta; } hist_rec;

typedef struct {
	int tid;
	bb_rng rng;
	uint64_t ok;
	uint64_t retry;
} worker;

static int
open_db(DB **dbp, const char *name, DBTYPE type, u_int32_t extra)
{
	DB *db;
	int ret;

	if ((ret = db_create(&db, g_env, 0)) != 0) return ret;
	if ((ret = db->open(db, NULL, name, NULL, type,
	    bb_db_flags(&g_cfg) | extra, 0)) != 0) {
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
adjust(DB *db, DB_TXN *txn, uint32_t key, int32_t delta)
{
	DBT k, d;
	bal_rec b;
	int ret;

	memset(&k, 0, sizeof(k)); k.data = &key; k.size = sizeof(key);
	memset(&d, 0, sizeof(d));
	d.data = &b; d.ulen = sizeof(b); d.flags = DB_DBT_USERMEM;
	if ((ret = db->get(db, txn, &k, &d, 0)) != 0)
		return ret;
	b.balance += delta;
	memset(&d, 0, sizeof(d)); d.data = &b; d.size = sizeof(b);
	return db->put(db, txn, &k, &d, 0);
}

static int
populate(void)
{
	DB_TXN *txn = NULL;
	bal_rec b;
	uint32_t i;
	int ret, n;

	if ((ret = open_db(&g_acct, "account.db", DB_BTREE, 0)) != 0) return ret;
	if ((ret = open_db(&g_tell, "teller.db", DB_BTREE, 0)) != 0) return ret;
	if ((ret = open_db(&g_branch, "branch.db", DB_BTREE, 0)) != 0) return ret;
	if ((ret = open_db(&g_hist, "history.db", DB_RECNO, 0)) != 0) return ret;

	if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
	memset(&b, 0, sizeof(b)); b.balance = 0;
	n = 0;
	for (i = 0; i < (uint32_t)g_naccts; i++) {
		if ((ret = put_u32(g_acct, txn, i, &b, sizeof(b))) != 0) goto err;
		if (++n % 20000 == 0 && g_cfg.use_txn) {
			if ((ret = bb_commit(txn)) != 0) return ret;
			if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;
		}
	}
	for (i = 0; i < (uint32_t)g_ntellers; i++)
		if ((ret = put_u32(g_tell, txn, i, &b, sizeof(b))) != 0) goto err;
	for (i = 0; i < (uint32_t)g_nbranches; i++)
		if ((ret = put_u32(g_branch, txn, i, &b, sizeof(b))) != 0) goto err;
	return bb_commit(txn);
err:
	(void)bb_abort(txn);
	return ret;
}

static int
do_txn(worker *w)
{
	DB_TXN *txn;
	DBT k, d;
	hist_rec h;
	db_recno_t rno;
	uint32_t aid, tid, bid;
	int32_t delta;
	int ret;

	aid = bb_rand_between(&w->rng, 0, (uint32_t)g_naccts - 1);
	tid = bb_rand_between(&w->rng, 0, (uint32_t)g_ntellers - 1);
	bid = bb_rand_between(&w->rng, 0, (uint32_t)g_nbranches - 1);
	delta = (int32_t)bb_rand_between(&w->rng, 1, 1000) - 500;
again:
	if ((ret = bb_begin(&g_cfg, g_env, &txn, 0)) != 0) return ret;

	ret = adjust(g_acct, txn, aid, delta);
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); w->retry++; goto again; }
	if (ret != 0) goto fail;
	ret = adjust(g_tell, txn, tid, delta);
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); w->retry++; goto again; }
	if (ret != 0) goto fail;
	ret = adjust(g_branch, txn, bid, delta);
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); w->retry++; goto again; }
	if (ret != 0) goto fail;

	h.a = aid; h.t = tid; h.b = bid; h.delta = delta;
	memset(&k, 0, sizeof(k)); k.data = &rno; k.ulen = sizeof(rno); k.flags = DB_DBT_USERMEM;
	memset(&d, 0, sizeof(d)); d.data = &h; d.size = sizeof(h);
	ret = g_hist->put(g_hist, txn, &k, &d, DB_APPEND);
	if (ret == DB_LOCK_DEADLOCK) { (void)bb_abort(txn); w->retry++; goto again; }
	if (ret != 0) goto fail;

	if ((ret = bb_commit(txn)) != 0) { txn = NULL; goto fail; }
	w->ok++;
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
		if ((ret = do_txn(w)) != 0 && ret != DB_LOCK_DEADLOCK) {
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
	    "usage: %s [-i] [-h home] [-S scale] [-t threads] [-s secs]\n"
	    "          [-c cachebytes] [-d sync|wnosync|nosync] [-m] [-C]\n"
	    "          [-X txn|lock|log] [-R seed]\n", p);
}

int
main(int argc, char **argv)
{
	pthread_t *tids;
	worker *workers;
	uint64_t okall, retryall;
	double t0, elapsed;
	int t, ret;

	bb_config_defaults(&g_cfg);
	if (bb_getopt(argc, argv, &g_cfg) != 0) { usage(argv[0]); return 1; }
	if (g_cfg.scale < 1) g_cfg.scale = 1;
	g_naccts = g_cfg.scale * ACCOUNTS_PER;
	g_ntellers = g_cfg.scale * TELLERS_PER;
	g_nbranches = g_cfg.scale * BRANCHES_PER;

	if ((ret = bb_env_open(&g_cfg, &g_env)) != 0) return 1;

	if (g_cfg.init) {
		bb_print_config(&g_cfg, "tproc-b populate");
		t0 = bb_now_ms();
		if ((ret = populate()) != 0) {
			fprintf(stderr, "populate: %s\n", db_strerror(ret));
			return 1;
		}
		printf("# populated %d accounts in %.1f s\n",
		    g_naccts, (bb_now_ms() - t0) / 1000.0);
		(void)g_env->close(g_env, 0);
		return 0;
	}

	if ((ret = open_db(&g_acct, "account.db", DB_BTREE, 0)) != 0 ||
	    (ret = open_db(&g_tell, "teller.db", DB_BTREE, 0)) != 0 ||
	    (ret = open_db(&g_branch, "branch.db", DB_BTREE, 0)) != 0 ||
	    (ret = open_db(&g_hist, "history.db", DB_RECNO, 0)) != 0) {
		fprintf(stderr, "open (did you -i first?): %s\n", db_strerror(ret));
		return 1;
	}

	bb_print_config(&g_cfg, "tproc-b");

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
	usleep((useconds_t)g_cfg.seconds * 1000000);
	g_stop = 1;
	for (t = 0; t < g_cfg.threads; t++)
		pthread_join(tids[t], NULL);
	elapsed = (bb_now_ms() - t0) / 1000.0;

	okall = retryall = 0;
	for (t = 0; t < g_cfg.threads; t++) {
		okall += workers[t].ok;
		retryall += workers[t].retry;
	}
	printf("tproc-b %2d threads  %.0f txn/s  committed=%llu retries=%llu (%.1fs)\n",
	    g_cfg.threads, (double)okall / elapsed,
	    (unsigned long long)okall, (unsigned long long)retryall, elapsed);

	free(tids); free(workers);
	(void)g_env->close(g_env, 0);
	return 0;
}

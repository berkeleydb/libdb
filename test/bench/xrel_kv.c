/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * xrel_kv -- transactional YCSB-style key/value workloads over every Berkeley
 * DB access method, written only against the public API that Berkeley DB 5.3
 * already had, so one source builds against 5.3.28 and every libdb release.
 * It exists to compare releases, not engines.
 *
 * usage: xrel_kv <home> <am> <workload> <records> <threads> <secs> [load]
 *	am		btree | hash | recno | queue | heap
 *	workload	A  50% read / 50% update           (YCSB-A, update-heavy)
 *			B  95% read /  5% update           (YCSB-B, read-mostly)
 *			C 100% read                         (YCSB-C, read-only)
 *			F  read-modify-write in one txn     (YCSB-F)
 *			E  short range scans, 95% / 5% ins (YCSB-E; btree/recno/queue only)
 *			I  insert-only, appended keys      (load throughput)
 *	load		populate <records> first, then exit
 *
 * Every operation runs in its own transaction (explicit txn_begin/commit), the
 * environment is fully transactional (DB_INIT_TXN + LOCK + LOG + MPOOL) with
 * DB_TXN_WRITE_NOSYNC, and the deadlock detector runs on every conflict
 * (DB_LOCK_DEFAULT).  A deadlocked transaction is aborted and counted, not
 * scored: the headline number is committed operations per second.
 *
 * Keys are chosen with the YCSB scrambled-zipfian generator (theta 0.99) so a
 * few records are hot, as in the YCSB core workloads.  XREL_UNIFORM=1 switches
 * to uniform keys.
 *
 * Record ids map to keys per access method: btree and hash use an 8-byte
 * big-endian id (so btree order is id order); recno and queue use the record
 * number id+1; heap stores the id in the record and keeps a btree secondary
 * from id to DB_HEAP_RID, so heap is measured the way an application addresses
 * it.  Values are 100 bytes (queue: fixed-length 100-byte records).
 *
 * Output: one TSV line
 *	am wl threads secs committed_ops ops_per_s aborts p50_us p99_us
 */
#include <sys/types.h>
#include <errno.h>
#include <math.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#include <db.h>

#define	VALSZ		100
#define	SCANLEN		50
#define	MAXTHREADS	256
#define	LATMAX		(1 << 20)	/* latency samples kept per thread */

static DB_ENV *env;
static DB *db, *idx;			/* idx: heap's id -> RID secondary */
static DBTYPE am;
static char amname[16], wl;
static uint64_t nrec;
static volatile int stop, go, dd_done;
static volatile uint64_t appended;	/* next id for inserts */
static int uniform;

static void *
ddthread(void *a)
{
	(void)a;
	int ret;

	while (!dd_done) {
		if ((ret = env->lock_detect(env, 0, DB_LOCK_DEFAULT, NULL)) != 0) {
			env->err(env, ret, "ddthread lock_detect");
			exit(3);
		}
		usleep(10000);
	}
	return (NULL);
}

/* ---- YCSB scrambled zipfian ------------------------------------------ */
static double zeta_n, zeta2, alpha, eta, theta = 0.99;

static double
zeta(uint64_t n, double th)
{
	double s = 0;
	uint64_t i;

	for (i = 1; i <= n; i++)
		s += 1.0 / pow((double)i, th);
	return (s);
}

typedef struct { uint64_t s; } rng_t;

static uint64_t
rnext(rng_t *r)
{
	uint64_t x = r->s;

	x ^= x << 13; x ^= x >> 7; x ^= x << 17;
	return (r->s = x);
}

static double
runit(rng_t *r)
{
	return ((rnext(r) >> 11) * (1.0 / 9007199254740992.0));
}

static uint64_t
fnv64(uint64_t v)
{
	uint64_t h = 0xcbf29ce484222325ULL;
	int i;

	for (i = 0; i < 8; i++) {
		h ^= v & 0xff;
		h *= 0x100000001b3ULL;
		v >>= 8;
	}
	return (h);
}

static uint64_t
next_id(rng_t *r)
{
	double u, uz;
	uint64_t z;

	if (uniform)
		return (rnext(r) % nrec);
	u = runit(r);
	uz = u * zeta_n;
	if (uz < 1.0)
		z = 0;
	else if (uz < 1.0 + pow(0.5, theta))
		z = 1;
	else
		z = (uint64_t)((double)nrec * pow(eta * u - eta + 1.0, alpha));
	return (fnv64(z) % nrec);	/* scramble: hot ids spread over the keyspace */
}

/* ---- key encoding per access method ---------------------------------- */
typedef struct {
	unsigned char kbuf[8];
	db_recno_t recno;
	DB_HEAP_RID rid;
	DBT key;
} keyctx;

static void
id_to_key(uint64_t id, keyctx *k)
{
	int i;

	memset(&k->key, 0, sizeof(k->key));
	switch (am) {
	case DB_RECNO:
	case DB_QUEUE:
		k->recno = (db_recno_t)(id + 1);
		k->key.data = &k->recno;
		k->key.size = sizeof(k->recno);
		break;
	default:			/* btree, hash, and heap's secondary */
		for (i = 7; i >= 0; i--) {
			k->kbuf[i] = (unsigned char)(id & 0xff);
			id >>= 8;
		}
		k->key.data = k->kbuf;
		k->key.size = 8;
		break;
	}
}

static void
fill_val(unsigned char *v, uint64_t id, uint64_t gen)
{
	memset(v, 'v', VALSZ);
	memcpy(v, &id, sizeof(id));
	memcpy(v + 8, &gen, sizeof(gen));
}

/* heap: look up the RID for id inside txn. */
static int
heap_rid(DB_TXN *txn, uint64_t id, keyctx *k, u_int32_t flags)
{
	DBT d;

	id_to_key(id, k);
	memset(&d, 0, sizeof(d));
	d.data = &k->rid;
	d.ulen = sizeof(k->rid);
	d.flags = DB_DBT_USERMEM;
	return (idx->get(idx, txn, &k->key, &d, flags));
}

static int
op_read(DB_TXN *txn, uint64_t id, u_int32_t flags)
{
	keyctx k;
	DBT d, rk;
	unsigned char v[VALSZ + 16];
	int ret;

	memset(&d, 0, sizeof(d));
	d.data = v;
	d.ulen = sizeof(v);
	d.flags = DB_DBT_USERMEM;
	if (am == DB_HEAP) {
		if ((ret = heap_rid(txn, id, &k, 0)) != 0)
			return (ret);
		memset(&rk, 0, sizeof(rk));
		rk.data = &k.rid;
		rk.size = sizeof(k.rid);
		return (db->get(db, txn, &rk, &d, flags));
	}
	id_to_key(id, &k);
	return (db->get(db, txn, &k.key, &d, flags));
}

static int
op_write(DB_TXN *txn, uint64_t id, uint64_t gen)
{
	keyctx k;
	DBT d, rk;
	unsigned char v[VALSZ];
	int ret;

	fill_val(v, id, gen);
	memset(&d, 0, sizeof(d));
	d.data = v;
	d.size = VALSZ;
	if (am == DB_HEAP) {
		if ((ret = heap_rid(txn, id, &k, DB_RMW)) != 0)
			return (ret);
		memset(&rk, 0, sizeof(rk));
		rk.data = &k.rid;
		rk.size = sizeof(k.rid);
		return (db->put(db, txn, &rk, &d, 0));
	}
	id_to_key(id, &k);
	return (db->put(db, txn, &k.key, &d, 0));
}

/* Append a new record (heap: plus its secondary entry). */
static int
op_insert(DB_TXN *txn, uint64_t id)
{
	keyctx k;
	DBT d, rk, rd;
	unsigned char v[VALSZ];
	int ret;

	fill_val(v, id, 0);
	memset(&d, 0, sizeof(d));
	d.data = v;
	d.size = VALSZ;
	if (am == DB_HEAP) {
		memset(&rk, 0, sizeof(rk));
		rk.data = &k.rid;
		rk.ulen = sizeof(k.rid);
		rk.flags = DB_DBT_USERMEM;
		if ((ret = db->put(db, txn, &rk, &d, DB_APPEND)) != 0)
			return (ret);
		id_to_key(id, &k);
		memset(&rd, 0, sizeof(rd));
		rd.data = &k.rid;
		rd.size = sizeof(k.rid);
		return (idx->put(idx, txn, &k.key, &rd, 0));
	}
	if (am == DB_QUEUE) {
		memset(&rk, 0, sizeof(rk));
		rk.data = &k.recno;
		rk.ulen = sizeof(k.recno);
		rk.flags = DB_DBT_USERMEM;
		return (db->put(db, txn, &rk, &d, DB_APPEND));
	}
	id_to_key(id, &k);
	return (db->put(db, txn, &k.key, &d, 0));
}

/* Short range scan from id (btree/recno/queue: ordered). */
static int
op_scan(DB_TXN *txn, uint64_t id)
{
	DBC *c;
	keyctx k;
	DBT d;
	unsigned char v[VALSZ + 16];
	int i, ret, t_ret;

	if ((ret = db->cursor(db, txn, &c, 0)) != 0)
		return (ret);
	id_to_key(id, &k);
	memset(&d, 0, sizeof(d));
	d.data = v;
	d.ulen = sizeof(v);
	d.flags = DB_DBT_USERMEM;
	ret = c->get(c, &k.key, &d, am == DB_BTREE ? DB_SET_RANGE : DB_SET);
	for (i = 1; ret == 0 && i < SCANLEN; i++)
		ret = c->get(c, &k.key, &d, DB_NEXT);
	if (ret == DB_NOTFOUND)
		ret = 0;
	if ((t_ret = c->close(c)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/* ---- workers ---------------------------------------------------------- */
typedef struct {
	pthread_t th;
	rng_t rng;
	uint64_t ok, aborts, gen, nlat;
	uint32_t *lat;			/* microseconds */
} worker_t;

static double
now_us(void)
{
	struct timeval tv;

	(void)gettimeofday(&tv, NULL);
	return ((double)tv.tv_sec * 1e6 + (double)tv.tv_usec);
}

static void *
worker(void *arg)
{
	worker_t *w = arg;
	DB_TXN *txn;
	uint64_t id;
	double t0;
	int ret, pct;

	while (!go)
		;
	while (!stop) {
		pct = (int)(rnext(&w->rng) % 100);
		id = next_id(&w->rng);
		t0 = now_us();
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0) {
			env->err(env, ret, "txn_begin");
			exit(1);
		}
		switch (wl) {
		case 'A':
			ret = pct < 50 ? op_read(txn, id, 0) :
			    op_write(txn, id, ++w->gen);
			break;
		case 'B':
			ret = pct < 95 ? op_read(txn, id, 0) :
			    op_write(txn, id, ++w->gen);
			break;
		case 'C':
			ret = op_read(txn, id, 0);
			break;
		case 'F':
			if ((ret = op_read(txn, id, DB_RMW)) == 0)
				ret = op_write(txn, id, ++w->gen);
			break;
		case 'E':
			ret = pct < 95 ? op_scan(txn, id) :
			    op_insert(txn, __sync_fetch_and_add(&appended, 1));
			break;
		case 'I':
			ret = op_insert(txn, __sync_fetch_and_add(&appended, 1));
			break;
		}
		if (ret == 0)
			ret = txn->commit(txn, 0);
		else {
			(void)txn->abort(txn);
			if (ret != DB_LOCK_DEADLOCK && ret != DB_LOCK_NOTGRANTED &&
			    ret != DB_NOTFOUND) {
				env->err(env, ret, "op %c id %llu", wl,
				    (unsigned long long)id);
				exit(1);
			}
		}
		if (ret == 0) {
			w->ok++;
			if (w->nlat < LATMAX)
				w->lat[w->nlat++] = (uint32_t)(now_us() - t0);
		} else
			w->aborts++;
	}
	return (NULL);
}

static int
cmp32(const void *a, const void *b)
{
	uint32_t x = *(const uint32_t *)a, y = *(const uint32_t *)b;

	return (x < y ? -1 : x > y);
}

/* ---- setup ------------------------------------------------------------ */
static void
open_all(const char *home, int create)
{
	u_int32_t of;
	int ret;

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		exit(1);
	}
	env->set_errfile(env, stderr);
	/* Resident: every arm gets 16 GB, more than any dataset here. */
	(void)env->set_cachesize(env, 16, 0, 4);
	(void)env->set_lk_max_locks(env, 1000000);
	(void)env->set_lk_max_objects(env, 1000000);
	(void)env->set_lk_max_lockers(env, 100000);
	(void)env->set_lg_bsize(env, 64 * 1024 * 1024);
	(void)env->set_lg_max(env, 256 * 1024 * 1024);
	(void)env->set_tx_max(env, 4096);
	/* XREL_DD=periodic: no detect-on-conflict; a 10 ms timer thread runs it. */
	if (getenv("XREL_DD") == NULL)
		(void)env->set_lk_detect(env, DB_LOCK_DEFAULT);
	(void)env->set_flags(env, DB_TXN_WRITE_NOSYNC, 1);
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN | DB_THREAD |
	    DB_RECOVER, 0644)) != 0) {
		env->err(env, ret, "env open %s", home);
		exit(1);
	}
	if ((ret = db_create(&db, env, 0)) != 0) {
		env->err(env, ret, "db_create");
		exit(1);
	}
	if (am == DB_QUEUE)
		(void)db->set_re_len(db, VALSZ);
	if (am == DB_HASH)
		(void)db->set_h_nelem(db, (u_int32_t)nrec);
	of = DB_THREAD | DB_AUTO_COMMIT | (create ? DB_CREATE : 0);
	if ((ret = db->open(db, NULL, "xrel.db", NULL, am, of, 0644)) != 0) {
		env->err(env, ret, "db open");
		exit(1);
	}
	if (am == DB_HEAP) {
		if ((ret = db_create(&idx, env, 0)) != 0 ||
		    (ret = idx->open(idx, NULL, "xrel_idx.db", NULL, DB_BTREE,
		    of, 0644)) != 0) {
			env->err(env, ret, "heap index open");
			exit(1);
		}
	}
}

static void
load(void)
{
	DB_TXN *txn;
	uint64_t id;
	int ret;

	for (id = 0; id < nrec; ) {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			exit(1);
		for (; id < nrec && (id % 10000) != 9999; id++)
			if ((ret = op_insert(txn, id)) != 0) {
				env->err(env, ret, "load %llu",
				    (unsigned long long)id);
				exit(1);
			}
		if (id < nrec && (ret = op_insert(txn, id++)) != 0)
			exit(1);
		if ((ret = txn->commit(txn, 0)) != 0) {
			env->err(env, ret, "load commit");
			exit(1);
		}
	}
	(void)env->txn_checkpoint(env, 0, 0, 0);
}

/* Read every page once so the run starts with the data resident. */
static void
warm(void)
{
	DBC *c;
	DBT k, d;

	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.flags = d.flags = DB_DBT_MALLOC;
	if (db->cursor(db, NULL, &c, 0) == 0) {
		while (c->get(c, &k, &d, DB_NEXT) == 0) {
			free(k.data);
			free(d.data);
		}
		(void)c->close(c);
	}
	if (idx != NULL && idx->cursor(idx, NULL, &c, 0) == 0) {
		while (c->get(c, &k, &d, DB_NEXT) == 0) {
			free(k.data);
			free(d.data);
		}
		(void)c->close(c);
	}
}

int
main(int argc, char *argv[])
{
	static worker_t w[MAXTHREADS];
	uint32_t *all;
	uint64_t ok, ab, n, i;
	double t0, el;
	int nth, secs, t;

	if (argc < 7) {
		fprintf(stderr, "usage: xrel_kv <home> <btree|hash|recno|queue|heap> "
		    "<A|B|C|E|F|I> <records> <threads> <secs> [load]\n");
		return (1);
	}
	snprintf(amname, sizeof(amname), "%s", argv[2]);
	am = strcmp(amname, "hash") == 0 ? DB_HASH :
	    strcmp(amname, "recno") == 0 ? DB_RECNO :
	    strcmp(amname, "queue") == 0 ? DB_QUEUE :
	    strcmp(amname, "heap") == 0 ? DB_HEAP : DB_BTREE;
	wl = argv[3][0];
	nrec = strtoull(argv[4], NULL, 10);
	nth = atoi(argv[5]);
	secs = atoi(argv[6]);
	uniform = getenv("XREL_UNIFORM") != NULL;
	if (nth < 1 || nth > MAXTHREADS)
		return (1);
	if (wl == 'E' && (am == DB_HASH || am == DB_HEAP)) {
		fprintf(stderr, "workload E needs an ordered access method\n");
		return (2);
	}

	open_all(argv[1], argc > 7);
	if (argc > 7) {
		load();
		(void)db->close(db, 0);
		if (idx != NULL)
			(void)idx->close(idx, 0);
		(void)env->close(env, 0);
		return (0);
	}
	warm();

	zeta_n = zeta(nrec, theta);
	zeta2 = zeta(2, theta);
	alpha = 1.0 / (1.0 - theta);
	eta = (1.0 - pow(2.0 / (double)nrec, 1.0 - theta)) /
	    (1.0 - zeta2 / zeta_n);
	appended = nrec;

	for (t = 0; t < nth; t++) {
		w[t].rng.s = 0x9e3779b97f4a7c15ULL * (uint64_t)(t + 1);
		if ((w[t].lat = malloc(LATMAX * sizeof(uint32_t))) == NULL)
			return (1);
		pthread_create(&w[t].th, NULL, worker, &w[t]);
	}
	if (getenv("XREL_DD") != NULL) {
		pthread_t dd;
		pthread_create(&dd, NULL, ddthread, NULL);
		pthread_detach(dd);
	}
	t0 = now_us();
	go = 1;
	sleep((unsigned)secs);
	stop = 1;
	/*
	 * Workers no longer retry once stop is set, so a deadlock victim exits
	 * and on-conflict detection is never triggered again: a residual cycle
	 * of the survivors would wait forever.  Run the detector until every
	 * worker has finished its last operation.
	 */
	if (getenv("XREL_DD") == NULL) {
		pthread_t dd;
		pthread_create(&dd, NULL, ddthread, NULL);
		pthread_detach(dd);
	}
	for (t = 0; t < nth; t++)
		pthread_join(w[t].th, NULL);
	dd_done = 1;
	el = (now_us() - t0) / 1e6;

	ok = ab = n = 0;
	for (t = 0; t < nth; t++) {
		ok += w[t].ok;
		ab += w[t].aborts;
		n += w[t].nlat;
	}
	all = malloc((n ? n : 1) * sizeof(uint32_t));
	for (n = 0, t = 0; t < nth; t++)
		for (i = 0; i < w[t].nlat; i++)
			all[n++] = w[t].lat[i];
	qsort(all, n, sizeof(uint32_t), cmp32);
	printf("%s\t%c\t%d\t%.1f\t%llu\t%.0f\t%llu\t%u\t%u\n", amname, wl, nth,
	    el, (unsigned long long)ok, (double)ok / el, (unsigned long long)ab,
	    n ? all[n / 2] : 0, n ? all[(n * 99) / 100] : 0);
	(void)db->close(db, 0);
	if (idx != NULL)
		(void)idx->close(idx, 0);
	(void)env->close(env, 0);
	return (0);
}

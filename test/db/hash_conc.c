/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * hash_conc.c --
 *	RFC 0017 change 1 correctness driver.  Concurrent HASH inserts and
 *	lookups across forced bucket splits, from T threads in each of P
 *	processes, then a full verification:
 *	  - every COMMITTED key is present exactly once;
 *	  - no ABORTED key is present;
 *	  - db_verify (via DB->verify) reports the database is well formed.
 *
 *	Splits are forced with a tiny fill factor (set_h_ffactor), so the
 *	metapage mapping (max_bucket/masks/spares) changes constantly while
 *	lookups and inserts map through the per-cursor cache -- exactly the
 *	window change 1's LSN validation must cover.
 *
 *	Determinism of the oracle: key k is COMMITTED iff (k % 7 != 0) and
 *	ABORTED iff (k % 7 == 0).  Every process/thread inserts a disjoint key
 *	range, so after the run the set of keys that must exist is known
 *	exactly without any inter-process bookkeeping.
 *
 *	Modes:
 *	  <home> build  <nproc> <nthread> <nkeys>   parent: create, fork, verify
 *	  <home> child  <proc_id> <nthread> <nkeys> exec'd worker
 *
 *	Exit 0 = PASS, non-zero = FAIL.  Built with -DRFC17_SABOTAGE_NO_VALIDATE
 *	in the library, the verification must FAIL (teeth).
 */
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	DBFILE		"hc.db"
#define	ABORT_EVERY	7		/* key % 7 == 0 is aborted */
#define	VALSZ		40		/* big enough to split fast */

static const char *g_home;

static DB_ENV *
openenv(const char *home)
{
	DB_ENV *e;
	int ret;

	if ((ret = db_env_create(&e, 0)) != 0) {
		fprintf(stderr, "env_create: %d\n", ret);
		return (NULL);
	}
	(void)e->set_lk_detect(e, DB_LOCK_YOUNGEST);
	e->set_errfile(e, stderr);
	e->set_cachesize(e, 0, 64 * 1024 * 1024, 1);
	if ((ret = e->open(e, home, DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_TXN | DB_THREAD, 0)) != 0) {
		e->err(e, ret, "env open");
		(void)e->close(e, 0);
		return (NULL);
	}
	return (e);
}

static int g_notdurable;	/* open the DB DB_TXN_NOT_DURABLE (lead review #1) */

static DB *
opendb(DB_ENV *e, int create)
{
	DB *db;
	int ret;
	u_int32_t flags;

	if ((ret = db_create(&db, e, 0)) != 0) {
		e->err(e, ret, "db_create");
		return (NULL);
	}
	if (create)
		(void)db->set_h_ffactor(db, 4);	/* tiny -> frequent splits */
	if (g_notdurable)
		(void)db->set_flags(db, DB_TXN_NOT_DURABLE);
	flags = DB_THREAD | DB_AUTO_COMMIT | (create ? DB_CREATE : 0);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_HASH, flags, 0)) != 0) {
		e->err(e, ret, "db open");
		(void)db->close(db, 0);
		return (NULL);
	}
	return (db);
}

/* A worker thread's key range: [base, base+nkeys). */
typedef struct {
	DB_ENV *env;
	DB *db;
	uint32_t base;
	uint32_t nkeys;
} warg_t;

static void *
worker(void *a)
{
	warg_t *w = a;
	DB_ENV *e = w->env;
	DB *db = w->db;
	DB_TXN *txn;
	DBT key, data;
	char val[VALSZ];
	uint32_t i, k;
	int ret;

	memset(val, 'x', sizeof(val));
	for (i = 0; i < w->nkeys; i++) {
		k = w->base + i;
again:
		txn = NULL;
		if ((ret = e->txn_begin(e, NULL, &txn, 0)) != 0) {
			e->err(e, ret, "txn_begin");
			return ((void *)1);
		}
		memset(&key, 0, sizeof(key));
		key.data = &k;
		key.size = sizeof(k);
		memset(&data, 0, sizeof(data));
		data.data = val;
		data.size = sizeof(val);

		ret = db->put(db, txn, &key, &data, 0);
		if (ret == DB_LOCK_DEADLOCK) {
			(void)txn->abort(txn);
			goto again;
		}
		if (ret != 0) {
			e->err(e, ret, "put k=%u", k);
			(void)txn->abort(txn);
			return ((void *)1);
		}
		/* Also do a concurrent lookup of an earlier committed key to
		 * exercise the cached read path against live splits. */
		if (i > 0) {
			uint32_t pk = w->base + (i - 1);
			DBT pkey, pdata;
			char rbuf[256];
			if (pk % ABORT_EVERY != 0) {
				memset(&pkey, 0, sizeof(pkey));
				pkey.data = &pk; pkey.size = sizeof(pk);
				memset(&pdata, 0, sizeof(pdata));
				pdata.data = rbuf; pdata.ulen = sizeof(rbuf);
				pdata.flags = DB_DBT_USERMEM;
				ret = db->get(db, txn, &pkey, &pdata, 0);
				if (ret == DB_LOCK_DEADLOCK) {
					(void)txn->abort(txn);
					goto again;
				}
				if (ret == DB_NOTFOUND) {
					fprintf(stderr,
					    "FAIL: committed key %u absent "
					    "under live split\n", pk);
					(void)txn->abort(txn);
					return ((void *)2);
				}
			}
		}

		if (k % ABORT_EVERY == 0)
			ret = txn->abort(txn);
		else
			ret = txn->commit(txn, 0);
		if (ret == DB_LOCK_DEADLOCK)
			goto again;
		if (ret != 0) {
			e->err(e, ret, "txn end k=%u", k);
			return ((void *)1);
		}
	}
	return (NULL);
}

static int
run_threads(DB_ENV *e, DB *db, uint32_t proc_base, int nthread, uint32_t nkeys)
{
	pthread_t th[256];
	warg_t wa[256];
	int i, rc = 0;
	void *r;

	if (nthread > 256)
		nthread = 256;
	for (i = 0; i < nthread; i++) {
		wa[i].env = e;
		wa[i].db = db;
		wa[i].base = proc_base + (uint32_t)i * nkeys;
		wa[i].nkeys = nkeys;
		if (pthread_create(&th[i], NULL, worker, &wa[i]) != 0) {
			fprintf(stderr, "pthread_create %d\n", i);
			rc = 1;
		}
	}
	for (i = 0; i < nthread; i++) {
		pthread_join(th[i], &r);
		if (r != NULL)
			rc = 1;
	}
	return (rc);
}

/*
 * Abort-mid-split worker: one big transaction that inserts enough keys to
 * force several bucket splits, then ABORTS.  None of its keys may survive and
 * the table extent must be back where it started.  Runs alongside reader
 * threads that must never observe the in-flight (soon-aborted) keys.
 */
typedef struct { DB_ENV *env; DB *db; uint32_t base; uint32_t n; int *stop; }
    aarg_t;

static void *
abort_splitter(void *a)
{
	aarg_t *w = a;
	DB_ENV *e = w->env;
	DB_TXN *txn;
	DBT key, data;
	char val[VALSZ];
	uint32_t i, k;
	int ret;

	memset(val, 'y', sizeof(val));
retry:
	txn = NULL;
	if ((ret = e->txn_begin(e, NULL, &txn, 0)) != 0)
		return ((void *)1);
	for (i = 0; i < w->n; i++) {
		k = w->base + i;
		memset(&key, 0, sizeof(key));
		key.data = &k; key.size = sizeof(k);
		memset(&data, 0, sizeof(data));
		data.data = val; data.size = sizeof(val);
		ret = w->db->put(w->db, txn, &key, &data, 0);
		if (ret == DB_LOCK_DEADLOCK) {
			(void)txn->abort(txn);
			goto retry;
		}
		if (ret != 0) {
			e->err(e, ret, "abort-splitter put %u", k);
			(void)txn->abort(txn);
			return ((void *)1);
		}
	}
	/* Deliberately abort after forcing splits. */
	(void)txn->abort(txn);
	*w->stop = 1;
	return (NULL);
}

typedef struct { DB_ENV *env; DB *db; uint32_t base; uint32_t n; int *stop;
    int seen; } rarg_t;

static void *
abort_reader(void *a)
{
	rarg_t *w = a;
	DBT key, data;
	char rbuf[256];
	uint32_t k;
	int ret;

	while (!*w->stop) {
		k = w->base + (uint32_t)(rand() % (w->n ? w->n : 1));
		memset(&key, 0, sizeof(key));
		key.data = &k; key.size = sizeof(k);
		memset(&data, 0, sizeof(data));
		data.data = rbuf; data.ulen = sizeof(rbuf);
		data.flags = DB_DBT_USERMEM;
		ret = w->db->get(w->db, NULL, &key, &data, 0);
		if (ret == 0) {
			/* An uncommitted-then-aborted key became visible: a
			 * dirty read the isolation + validation must prevent. */
			w->seen = 1;
			return (NULL);
		}
	}
	return (NULL);
}

static int
abort_main(const char *home, int nreader, uint32_t nkeys)
{
	DB_ENV *e;
	DB *db, *vdb;
	pthread_t sp, rd[64];
	aarg_t sa;
	rarg_t ra[64];
	int i, ret, rc = 0, stop = 0;
	DBT key, data;
	char rbuf[256];
	uint32_t k;

	if (nreader > 64)
		nreader = 64;
	if ((e = openenv(home)) == NULL)
		return (1);
	if ((db = opendb(e, 1)) == NULL) {
		(void)e->close(e, 0);
		return (1);
	}

	sa.env = e; sa.db = db; sa.base = 1; sa.n = nkeys; sa.stop = &stop;
	for (i = 0; i < nreader; i++) {
		ra[i].env = e; ra[i].db = db; ra[i].base = 1; ra[i].n = nkeys;
		ra[i].stop = &stop; ra[i].seen = 0;
		pthread_create(&rd[i], NULL, abort_reader, &ra[i]);
	}
	pthread_create(&sp, NULL, abort_splitter, &sa);
	pthread_join(sp, NULL);
	stop = 1;
	for (i = 0; i < nreader; i++) {
		pthread_join(rd[i], NULL);
		if (ra[i].seen) {
			fprintf(stderr, "FAIL: reader saw an aborted key\n");
			rc = 1;
		}
	}

	/* After the abort, none of the splitter's keys may exist. */
	for (k = 1; k <= nkeys; k++) {
		memset(&key, 0, sizeof(key));
		key.data = &k; key.size = sizeof(k);
		memset(&data, 0, sizeof(data));
		data.data = rbuf; data.ulen = sizeof(rbuf);
		data.flags = DB_DBT_USERMEM;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0) {
			fprintf(stderr, "FAIL: aborted key %u present\n", k);
			rc = 1;
			break;
		} else if (ret != DB_NOTFOUND) {
			e->err(e, ret, "get after abort %u", k);
			rc = 1;
			break;
		}
	}
	(void)db->close(db, 0);
	(void)e->close(e, 0);

	/* db_verify the structure after the aborted splits. */
	if ((e = openenv(home)) == NULL)
		return (1);
	if ((ret = db_create(&vdb, e, 0)) == 0) {
		if ((ret = vdb->verify(vdb, DBFILE, NULL, NULL, 0)) != 0) {
			e->err(e, ret, "DB->verify");
			rc = 1;
		} else
			printf("db_verify: OK\n");
	}
	(void)e->close(e, 0);

	printf("hash_conc(abort): %s\n", rc == 0 ? "PASS" : "FAIL");
	return (rc);
}

/* Child process: open the shared env/db and drive its thread set. */
static int
child_main(const char *home, int proc_id, int nthread, uint32_t nkeys)
{
	DB_ENV *e;
	DB *db;
	uint32_t proc_base;
	int rc;

	if ((e = openenv(home)) == NULL)
		return (1);
	if ((db = opendb(e, 0)) == NULL) {
		(void)e->close(e, 0);
		return (1);
	}
	/* Disjoint key range per process: proc_id * (nthread * nkeys * 4). */
	proc_base = (uint32_t)proc_id * (uint32_t)nthread * nkeys * 4u + 1u;
	rc = run_threads(e, db, proc_base, nthread, nkeys);
	(void)db->close(db, 0);
	(void)e->close(e, 0);
	return (rc);
}

/* Parent: verify the committed/aborted oracle across every process range. */
static int
verify_all(DB_ENV *e, DB *db, int nproc, int nthread, uint32_t nkeys)
{
	DBT key, data;
	char rbuf[256];
	uint32_t p, t, i, k, base;
	int ret, bad = 0;
	uint64_t nfound = 0, nmissing = 0, naborted_present = 0;

	for (p = 0; p < (uint32_t)nproc; p++) {
		base = p * (uint32_t)nthread * nkeys * 4u + 1u;
		for (t = 0; t < (uint32_t)nthread; t++) {
			for (i = 0; i < nkeys; i++) {
				k = base + t * nkeys + i;
				memset(&key, 0, sizeof(key));
				key.data = &k; key.size = sizeof(k);
				memset(&data, 0, sizeof(data));
				data.data = rbuf; data.ulen = sizeof(rbuf);
				data.flags = DB_DBT_USERMEM;
				ret = db->get(db, NULL, &key, &data, 0);
				if (k % ABORT_EVERY == 0) {
					/* aborted: must be absent */
					if (ret == 0) {
						naborted_present++;
						bad = 1;
					} else if (ret != DB_NOTFOUND) {
						e->err(e, ret, "get(abort) %u",
						    k);
						bad = 1;
					}
				} else {
					/* committed: must be present once */
					if (ret == DB_NOTFOUND) {
						nmissing++;
						bad = 1;
					} else if (ret != 0) {
						e->err(e, ret, "get %u", k);
						bad = 1;
					} else
						nfound++;
				}
			}
		}
	}
	printf("verify: committed_found=%llu missing=%llu "
	    "aborted_present=%llu\n",
	    (unsigned long long)nfound, (unsigned long long)nmissing,
	    (unsigned long long)naborted_present);
	return (bad);
}

static int
parent_main(const char *home, int nproc, int nthread, uint32_t nkeys,
    const char *self)
{
	DB_ENV *e;
	DB *db, *vdb;
	pid_t pids[64];
	char a_proc[16], a_nt[16], a_nk[16];
	int i, ret, rc = 0, status;

	/* Create the env + db so children only join. */
	if ((e = openenv(home)) == NULL)
		return (1);
	if ((db = opendb(e, 1)) == NULL) {
		(void)e->close(e, 0);
		return (1);
	}
	(void)db->close(db, 0);

	if (nproc > 64)
		nproc = 64;
	snprintf(a_nt, sizeof(a_nt), "%d", nthread);
	snprintf(a_nk, sizeof(a_nk), "%u", nkeys);
	for (i = 0; i < nproc; i++) {
		pids[i] = fork();
		if (pids[i] == 0) {
			snprintf(a_proc, sizeof(a_proc), "%d", i);
			execl(self, self, home, "child", a_proc, a_nt, a_nk,
			    (char *)NULL);
			perror("execl");
			_exit(127);
		}
	}
	for (i = 0; i < nproc; i++) {
		(void)waitpid(pids[i], &status, 0);
		if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
			fprintf(stderr, "child %d bad exit (status=%d)\n",
			    i, status);
			rc = 1;
		}
	}

	/* Verify the oracle. */
	if ((db = opendb(e, 0)) == NULL) {
		(void)e->close(e, 0);
		return (1);
	}
	if (verify_all(e, db, nproc, nthread, nkeys) != 0)
		rc = 1;
	(void)db->close(db, 0);

	/* db_verify: the database must be structurally sound.  verify()
	 * needs a handle that was never opened, so use a fresh one. */
	(void)e->close(e, 0);
	if ((e = openenv(home)) == NULL)
		return (1);
	if ((ret = db_create(&vdb, e, 0)) != 0) {
		e->err(e, ret, "db_create verify");
		(void)e->close(e, 0);
		return (1);
	}
	if ((ret = vdb->verify(vdb, DBFILE, NULL, NULL, 0)) != 0) {
		e->err(e, ret, "DB->verify");
		rc = 1;
		/* verify() closes the handle itself on all paths */
	} else
		printf("db_verify: OK\n");
	(void)e->close(e, 0);

	if (rc == 0)
		printf("hash_conc: PASS\n");
	else
		printf("hash_conc: FAIL\n");
	return (rc);
}

int
main(int argc, char **argv)
{
	int nproc, nthread;
	uint32_t nkeys;

	if (argc < 6) {
		fprintf(stderr, "usage: %s <home> build|child <n> <nthread> "
		    "<nkeys>\n", argv[0]);
		return (2);
	}
	g_home = argv[1];
	nthread = atoi(argv[4]);
	nkeys = (uint32_t)strtoul(argv[5], NULL, 10);

	if (strcmp(argv[2], "child") == 0) {
		int proc_id = atoi(argv[3]);
		return (child_main(argv[1], proc_id, nthread, nkeys));
	}
	if (strcmp(argv[2], "abort") == 0) {
		/* argv[3] = number of concurrent readers. */
		return (abort_main(argv[1], atoi(argv[3]), nkeys));
	}
	if (strcmp(argv[2], "notdurable") == 0) {
		/* DB_TXN_NOT_DURABLE: the split takes the LSN_NOT_LOGGED
		 * branch, so the cache fast path must NOT engage (DBC_LOGGING
		 * false) and master's locked metapath must keep it correct. */
		g_notdurable = 1;
		nproc = atoi(argv[3]);
		return (parent_main(argv[1], nproc, nthread, nkeys, argv[0]));
	}
	nproc = atoi(argv[3]);
	return (parent_main(argv[1], nproc, nthread, nkeys, argv[0]));
}

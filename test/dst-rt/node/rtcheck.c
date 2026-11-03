/*
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * test/dst-rt/node/rtcheck.c -- the invariant oracle (RFC 0024 §2.4, §2.7).
 *
 *   rtcheck --ack FILE [--ack FILE ...] --home DIR [--home DIR ...]
 *
 * It opens each env home with DB_RECOVER (so the database reflects what
 * survived the faults), reads the ACK logs, and enforces §2.4 rules 1-3 and 7:
 *
 *   1. Durability     -- every ACKed op is present with its value.
 *   2. No phantom     -- nothing FAILed is visible; UNKNOWN is allowed either
 *                        way; an ACKed del means the key is absent.
 *   3. Integrity      -- DB->verify passes; recovery succeeded.
 *   7. Error semantics-- recovery opened without a crash (a panic/exit here is
 *                        itself the failure).
 *
 * Rules 4-6 (HA convergence, single master, XA atomicity) live in fi_check.sh
 * / later phases.
 *
 * The ACK-log join (§2.7): member op lines carry the vhash; a per-txnid `txn`
 * line carries the outcome.  For each put/del member we look up its txnid's
 * outcome and apply the rule for that outcome.  The LAST op on a key wins
 * (replay order == log order == seq order).
 *
 * Exit 0 = PASS, 1 = one or more violations (each printed).
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "db.h"
#include "fi_acklog.h"		/* fi_fnv1a */

#define MAX_HOMES 8
#define MAX_ACKS  32

/* A resolved per-key expectation: the final op on this key and its outcome.
 * "Latest" across nodes is REPLICATION order, not append order: a write in a
 * higher master generation supersedes any write in a lower one, and within a
 * generation the higher commit LSN wins.  Single-node entries have gen 0 and a
 * monotonic LSN, so (gen, lsn) still orders them correctly. */
struct keyexp {
	char key[64];
	char op;		/* 'p' put, 'd' del */
	uint64_t vhash;		/* for put */
	char dbname[64];
	u_int32_t gen;		/* master generation of the winning op */
	u_int32_t lfile, loff;	/* commit LSN of the winning op */
	long lineno;		/* global append order, tiebreak for equal (gen,lsn) */
	uint64_t txnkey;	/* TXNKEY of the last op; resolved to outcome later */
	int outcome;		/* 0 ACK, 1 FAIL, 2 UNKNOWN */
	struct keyexp *next;
};

/* txnid -> outcome, built from the `txn` lines.  The key is scoped by the
 * ack-log file index (txnid resets to 0 in each rtload invocation, and a soak
 * passes one ack file per cycle), so cross-file txnids never collide. */
struct txnout { uint64_t key; int outcome; struct txnout *next; };

/* A put/del member op buffered until its `txn` line arrives, because the
 * ordering key (gen, LSN) is on the txn line, which follows the member ops. */
struct pending {
	uint64_t txnkey;
	char op;
	char dbname[64];
	char key[64];
	uint64_t vhash;
	long lineno;
	struct pending *next;
};

static struct keyexp *keys = NULL;
static struct txnout *txns = NULL;
static struct pending *pend = NULL;
static int violations = 0;
static long g_lineno = 0;	/* global append order across all ack files */
static int g_fileidx = 0;	/* which ack file we are parsing */
static long n_ack = 0, n_fail = 0, n_unknown = 0;  /* acked/failed/unknown looked up */

/* combine the current file index and a txnid into a collision-free key. */
#define TXNKEY(id) (((uint64_t)g_fileidx << 32) | (uint32_t)(id))

static int
outcode(const char *s)
{
	if (strcmp(s, "ACK") == 0) return (0);
	if (strcmp(s, "FAIL") == 0) return (1);
	return (2);	/* UNKNOWN or anything else */
}

static struct txnout *
txn_find(uint64_t id)
{
	struct txnout *t;
	for (t = txns; t != NULL; t = t->next)
		if (t->key == id) return (t);
	return (NULL);
}

static void
txn_set(uint64_t id, int oc)
{
	struct txnout *t = txn_find(id);
	if (t == NULL) {
		t = calloc(1, sizeof(*t));
		t->key = id; t->next = txns; txns = t;
	}
	t->outcome = oc;
}

static struct keyexp *
key_find(const char *dbname, const char *k)
{
	struct keyexp *e;
	for (e = keys; e != NULL; e = e->next)
		if (strcmp(e->key, k) == 0 && strcmp(e->dbname, dbname) == 0)
			return (e);
	return (NULL);
}

/* (gen, lfile, loff, lineno) strictly greater than a key's current winner? */
static int
order_gt(u_int32_t gen, u_int32_t lf, u_int32_t lo, long ln,
    const struct keyexp *e)
{
	if (gen != e->gen) return (gen > e->gen);
	if (lf != e->lfile) return (lf > e->lfile);
	if (lo != e->loff) return (lo > e->loff);
	return (ln >= e->lineno);	/* tiebreak: later append wins */
}

/* record an op on a key; the latest in (gen, LSN) replication order wins. */
static void
key_record(u_int32_t gen, u_int32_t lf, u_int32_t lo, long lineno,
    uint64_t txnkey, char op, const char *dbname, const char *k, uint64_t vh)
{
	struct keyexp *e = key_find(dbname, k);
	if (e == NULL) {
		e = calloc(1, sizeof(*e));
		snprintf(e->key, sizeof(e->key), "%s", k);
		snprintf(e->dbname, sizeof(e->dbname), "%s", dbname);
		e->gen = 0; e->lfile = 0; e->loff = 0; e->lineno = -1;
		e->next = keys; keys = e;
	}
	if (e->lineno == -1 || order_gt(gen, lf, lo, lineno, e)) {
		e->gen = gen; e->lfile = lf; e->loff = lo; e->lineno = lineno;
		e->op = op;
		e->vhash = vh;
		e->txnkey = txnkey;	/* resolved to an outcome after parsing */
	}
}

/* buffer a member op until its txn line supplies (gen, LSN). */
static void
pend_add(uint64_t txnkey, char op, const char *dbname, const char *k,
    uint64_t vh, long lineno)
{
	struct pending *p = calloc(1, sizeof(*p));
	p->txnkey = txnkey; p->op = op; p->vhash = vh; p->lineno = lineno;
	snprintf(p->dbname, sizeof(p->dbname), "%s", dbname);
	snprintf(p->key, sizeof(p->key), "%s", k);
	p->next = pend; pend = p;
}

/* on a txn line: finalize every buffered member op of this txn with the txn's
 * (gen, LSN), then drop them from the pending list. */
static void
pend_flush(uint64_t txnkey, u_int32_t gen, u_int32_t lf, u_int32_t lo)
{
	struct pending **pp = &pend, *p;
	while ((p = *pp) != NULL) {
		if (p->txnkey == txnkey) {
			key_record(gen, lf, lo, p->lineno, txnkey, p->op,
			    p->dbname, p->key, p->vhash);
			*pp = p->next; free(p);
		} else
			pp = &p->next;
	}
}

/* pass 1: parse all ack logs, recording member ops and txn outcomes. */
static int
parse_acklog(const char *path)
{
	FILE *fp = fopen(path, "r");
	char line[1024];
	if (fp == NULL) {
		fprintf(stderr, "rtcheck: cannot open ack log %s: %s\n",
		    path, strerror(errno));
		return (1);
	}
	while (fgets(line, sizeof(line), fp) != NULL) {
		long seq; u_int32_t txnid;
		char op[16], dbname[64], k[64], vhx[32], out[16], lsn[40], genf[40];
		int n = sscanf(line, "%ld %u %15s %63s %63s %31s %15s %39s %39s",
		    &seq, &txnid, op, dbname, k, vhx, out, lsn, genf);
		if (n < 6) continue;
		g_lineno++;	/* append order, tiebreak for equal (gen,LSN) */
		if (strcmp(op, "txn") == 0) {
			/* vhx is "-", out holds the outcome; lsn is "<file>/<off>"
			 * and genf is "gen=<n>" (both optional for single-node
			 * entries, which have no replication order). */
			u_int32_t gen = 0, lf = 0, lo = 0;
			const char *outcome = (n >= 7) ? out : vhx;
			if (n >= 8) sscanf(lsn, "%u/%u", &lf, &lo);
			if (n >= 9 && strncmp(genf, "gen=", 4) == 0)
				gen = (u_int32_t)strtoul(genf + 4, NULL, 10);
			txn_set(TXNKEY(txnid), outcode(outcome));
			pend_flush(TXNKEY(txnid), gen, lf, lo);
		} else if (strcmp(op, "put") == 0) {
			uint64_t vh = strtoull(vhx, NULL, 16);
			pend_add(TXNKEY(txnid), 'p', dbname, k, vh, g_lineno);
		} else if (strcmp(op, "del") == 0) {
			pend_add(TXNKEY(txnid), 'd', dbname, k, 0, g_lineno);
		}
	}
	fclose(fp);
	/*
	 * Any member op with no matching txn line (the process died between the
	 * PENDING put and the txn line) is UNKNOWN by definition; finalize it
	 * with gen/LSN 0 so it is recorded but never gates (rule 1 treats
	 * UNKNOWN as either-way).  Flushing with a sentinel txnkey would miss
	 * them, so record them directly here.
	 */
	{
		struct pending *p;
		while ((p = pend) != NULL) {
			key_record(0, 0, 0, p->lineno, p->txnkey, p->op,
			    p->dbname, p->key, p->vhash);
			/* no txn line -> txn_find returns NULL -> UNKNOWN later */
			pend = p->next; free(p);
		}
	}
	g_fileidx++;	/* next ack file: a fresh txnid namespace */
	return (0);
}

/* open a home with DB_RECOVER and return its env (rule 3/7). */
static DB_ENV *
open_recover(const char *home)
{
	DB_ENV *env;
	int ret;
	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "rtcheck: db_env_create: %s\n",
		    db_strerror(ret));
		return (NULL);
	}
	(void)env->set_cachesize(env, 0, 64 * 1024 * 1024, 1);
	ret = env->open(env, home, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER | DB_THREAD, 0644);
	if (ret != 0) {
		fprintf(stderr,
		    "VIOLATION (rule 3/7): recovery open of %s failed: %s\n",
		    home, db_strerror(ret));
		(void)env->close(env, 0);
		return (NULL);
	}
	return (env);
}

/* verify every db named in the expectations (rule 3). */
static void
verify_db(DB_ENV *env, const char *home, const char *dbname)
{
	DB *db;
	int ret;
	/* DB->verify requires a handle not opened; create a fresh one. */
	if ((ret = db_create(&db, env, 0)) != 0) {
		fprintf(stderr, "rtcheck: db_create for verify: %s\n",
		    db_strerror(ret));
		violations++;
		return;
	}
	ret = db->verify(db, dbname, NULL, NULL, 0);
	/* verify closes the handle itself (it must not be used after). */
	if (ret != 0) {
		fprintf(stderr,
		    "VIOLATION (rule 3): DB->verify %s in %s: %s\n",
		    dbname, home, db_strerror(ret));
		violations++;
	}
}

int
main(int argc, char *argv[])
{
	const char *homes[MAX_HOMES]; int nhomes = 0;
	const char *acks[MAX_ACKS]; int nacks = 0;
	int i;
	DB_ENV *env = NULL;
	DB *db = NULL;
	struct keyexp *e;

	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "--home") == 0 && i + 1 < argc) {
			if (nhomes < MAX_HOMES) homes[nhomes++] = argv[++i];
		} else if (strcmp(argv[i], "--ack") == 0 && i + 1 < argc) {
			if (nacks < MAX_ACKS) acks[nacks++] = argv[++i];
		} else {
			fprintf(stderr, "rtcheck: unknown arg %s\n", argv[i]);
			return (2);
		}
	}
	if (nhomes == 0 || nacks == 0) {
		fprintf(stderr,
		    "usage: rtcheck --ack FILE... --home DIR...\n");
		return (2);
	}

	for (i = 0; i < nacks; i++)
		if (parse_acklog(acks[i]) != 0)
			return (2);

	/* Resolve each key's outcome from its txn line now that all are read.
	 * (key_record stashed the op's TXNKEY in ->txnkey.) */
	for (e = keys; e != NULL; e = e->next) {
		struct txnout *t = txn_find(e->txnkey);
		e->outcome = (t != NULL) ? t->outcome : 2 /* UNKNOWN */;
	}

	/* Single-node oracle: one home drives rules 1-3; extra homes are only
	 * recovery/verify checked (HA convergence is fi_check.sh's job). */
	for (i = 0; i < nhomes; i++) {
		DB_ENV *e2 = open_recover(homes[i]);
		if (e2 == NULL) { violations++; continue; }
		if (i == 0) env = e2;
		else (void)e2->close(e2, 0);
	}
	if (env == NULL) {
		printf("FAIL: no home recovered\n");
		return (1);
	}

	/* Collect the set of db names and verify each (rule 3). */
	{
		char seen[16][64]; int nseen = 0, j;
		for (e = keys; e != NULL; e = e->next) {
			int dup = 0;
			for (j = 0; j < nseen; j++)
				if (strcmp(seen[j], e->dbname) == 0) dup = 1;
			if (!dup && nseen < 16)
				snprintf(seen[nseen++], 64, "%s", e->dbname);
		}
		for (j = 0; j < nseen; j++)
			verify_db(env, homes[0], seen[j]);
	}

	/* Rules 1 & 2: open the dbs read-only and check each key.  Open one
	 * handle per db name and keep it until the name changes; the keys list
	 * is grouped by db name because each db name is appended once. */
	{
	char cur_db[64] = "";
	DB *cur = NULL;
	for (e = keys; e != NULL; e = e->next) {
		DBT key, data;
		int ret;

		if (cur == NULL || strcmp(cur_db, e->dbname) != 0) {
			if (cur != NULL) { (void)cur->close(cur, 0); cur = NULL; }
			if ((ret = db_create(&db, env, 0)) != 0) {
				fprintf(stderr, "rtcheck: db_create: %s\n",
				    db_strerror(ret)); violations++; break;
			}
			ret = db->open(db, NULL, e->dbname, NULL, DB_UNKNOWN,
			    DB_RDONLY | DB_THREAD, 0);
			if (ret != 0) {
				fprintf(stderr,
				    "VIOLATION: cannot open %s read-only: %s\n",
				    e->dbname, db_strerror(ret));
				(void)db->close(db, 0); db = NULL;
				violations++; continue;
			}
			snprintf(cur_db, sizeof(cur_db), "%s", e->dbname);
			cur = db;
		}

		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		data.flags = DB_DBT_MALLOC;	/* let libdb allocate the value buf */
		key.data = e->key; key.size = (u_int32_t)strlen(e->key);
		ret = cur->get(cur, NULL, &key, &data, 0);

		if (e->outcome == 1) {		/* FAIL: must NOT be visible */
			n_fail++;
			/* Only a violation if THIS op's value is present.  A
			 * FAILed put whose key holds an earlier ACKed value is
			 * fine -- we verify the latest op, so a FAILed op is
			 * the latest only when nothing later touched the key. */
			if (e->op == 'p' && ret == 0 &&
			    fi_fnv1a(data.data, data.size) == e->vhash) {
				printf("VIOLATION (rule 2, phantom): FAILed "
				    "put of %s is visible\n", e->key);
				violations++;
			}
			if (ret == 0 && data.data != NULL) free(data.data);
			continue;
		}
		if (e->outcome == 2) {		/* UNKNOWN: either way is ok */
			n_unknown++;
			if (ret == 0 && data.data != NULL) free(data.data);
			continue;
		}

		/* ACK (rule 1): this op was acked and we are now looking it up. */
		n_ack++;
		if (e->op == 'p') {
			if (ret == DB_NOTFOUND) {
				printf("VIOLATION (rule 1, durability): ACKed "
				    "put of %s is MISSING after recovery\n",
				    e->key);
				violations++;
			} else if (ret != 0) {
				printf("VIOLATION (rule 1): get %s: %s\n",
				    e->key, db_strerror(ret));
				violations++;
			} else if (fi_fnv1a(data.data, data.size) != e->vhash) {
				printf("VIOLATION (rule 1, value): %s has the "
				    "wrong value after recovery\n", e->key);
				violations++;
			}
		} else {	/* ACKed del: key must be absent */
			if (ret == 0) {
				printf("VIOLATION (rule 2, phantom): ACKed del "
				    "of %s but key is still present\n", e->key);
				violations++;
			}
		}
		if (ret == 0 && data.data != NULL)
			free(data.data);	/* DB_DBT_MALLOC buffer */
	}
	if (cur != NULL) (void)cur->close(cur, 0);
	}

	(void)env->close(env, 0);

	/* Non-vacuity: a PASS over ZERO acked ops is the vacuous-green shape --
	 * the oracle looked up nothing, so it proves nothing.  Report the counts
	 * always, and FAIL VACUOUS when no acked op was checked.  (An empty ack
	 * log, or an ack log whose txns all FAILed/are UNKNOWN, lands here.)
	 * F2's HA scenarios rely on this guard. */
	if (violations == 0 && n_ack == 0) {
		printf("VACUOUS: 0 acked ops checked "
		    "(checked %ld acked, %ld failed, %ld unknown)\n",
		    n_ack, n_fail, n_unknown);
		return (1);
	}
	if (violations == 0) {
		printf("PASS (checked %ld acked, %ld failed, %ld unknown)\n",
		    n_ack, n_fail, n_unknown);
		return (0);
	}
	printf("FAIL: %d violation(s) "
	    "(checked %ld acked, %ld failed, %ld unknown)\n",
	    violations, n_ack, n_fail, n_unknown);
	return (1);
}

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

/* A resolved per-key expectation: the final op on this key and its outcome. */
struct keyexp {
	char key[64];
	char op;		/* 'p' put, 'd' del */
	uint64_t vhash;		/* for put */
	char dbname[64];
	long seq;		/* last op's seq, for "latest wins" */
	int outcome;		/* 0 ACK, 1 FAIL, 2 UNKNOWN */
	struct keyexp *next;
};

/* txnid -> outcome, built from the `txn` lines. */
struct txnout { u_int32_t txnid; int outcome; struct txnout *next; };

static struct keyexp *keys = NULL;
static struct txnout *txns = NULL;
static int violations = 0;

static int
outcode(const char *s)
{
	if (strcmp(s, "ACK") == 0) return (0);
	if (strcmp(s, "FAIL") == 0) return (1);
	return (2);	/* UNKNOWN or anything else */
}

static struct txnout *
txn_find(u_int32_t id)
{
	struct txnout *t;
	for (t = txns; t != NULL; t = t->next)
		if (t->txnid == id) return (t);
	return (NULL);
}

static void
txn_set(u_int32_t id, int oc)
{
	struct txnout *t = txn_find(id);
	if (t == NULL) {
		t = calloc(1, sizeof(*t));
		t->txnid = id; t->next = txns; txns = t;
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

/* record the latest op on a key (seq order). */
static void
key_record(long seq, u_int32_t txnid, char op, const char *dbname,
    const char *k, uint64_t vh)
{
	struct keyexp *e = key_find(dbname, k);
	if (e == NULL) {
		e = calloc(1, sizeof(*e));
		snprintf(e->key, sizeof(e->key), "%s", k);
		snprintf(e->dbname, sizeof(e->dbname), "%s", dbname);
		e->seq = -1;
		e->next = keys; keys = e;
	}
	if (seq >= e->seq) {
		e->seq = seq;
		e->op = op;
		e->vhash = vh;
		/* outcome is resolved after all txn lines are read; stash the
		 * txnid in vhash's unused high nibble?  No -- keep a parallel
		 * field. */
		e->outcome = (int)txnid;	/* temporary: holds txnid */
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
		char op[16], dbname[64], k[64], vhx[32], out[16];
		int n = sscanf(line, "%ld %u %15s %63s %63s %31s %15s",
		    &seq, &txnid, op, dbname, k, vhx, out);
		if (n < 6) continue;
		if (strcmp(op, "txn") == 0) {
			/* vhx field here is "-", out holds the outcome */
			txn_set(txnid, outcode(n >= 7 ? out : vhx));
		} else if (strcmp(op, "put") == 0) {
			uint64_t vh = strtoull(vhx, NULL, 16);
			key_record(seq, txnid, 'p', dbname, k, vh);
		} else if (strcmp(op, "del") == 0) {
			key_record(seq, txnid, 'd', dbname, k, 0);
		}
	}
	fclose(fp);
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

	/* Resolve each key's outcome from its txnid now that all txn lines are
	 * read.  (key_record stashed the txnid in ->outcome.) */
	for (e = keys; e != NULL; e = e->next) {
		struct txnout *t = txn_find((u_int32_t)e->outcome);
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
		key.data = e->key; key.size = (u_int32_t)strlen(e->key);
		ret = cur->get(cur, NULL, &key, &data, 0);

		if (e->outcome == 1) {		/* FAIL: must NOT be visible */
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
			continue;
		}
		if (e->outcome == 2)		/* UNKNOWN: either way is ok */
			continue;

		/* ACK (rule 1) */
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
	}
	if (cur != NULL) (void)cur->close(cur, 0);
	}

	(void)env->close(env, 0);

	if (violations == 0) { printf("PASS\n"); return (0); }
	printf("FAIL: %d violation(s)\n", violations);
	return (1);
}

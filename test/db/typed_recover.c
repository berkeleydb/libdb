/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_recover.c --
 *	RFC 0014 commit 9 test (spec §7.2 typed_recover, D5/C14): the typed
 *	build's db_recover recovers a typed environment WITH NO CODECS
 *	registered -- recovery opens the typed subdatabases but calls no
 *	comparator or constraint (the §3.5 hook is skipped for DB_AM_RECOVER
 *	handles).  An old or default build fails closed on the meta bit; that
 *	is typed_old_reader's job, not this test.
 *
 *	Flow:
 *	  1. Parent creates a txn env and a typed Btree primary (NOT NULL on a
 *	     data field) plus a UNIQUE secondary, inserts committed rows.
 *	  2. A forked child opens the env, commits more rows, then opens an
 *	     UNCOMMITTED txn with one more row and kill -9's itself mid-txn.
 *	  3. Parent runs the real db_recover UTILITY (argv path from the
 *	     runner) on the home -- it links no application code, so no codec
 *	     is registered; it must exit 0.
 *	  4. Parent reopens WITH the schema and full-scans against a reference
 *	     model: every committed row present, the uncommitted one absent,
 *	     NOT NULL holds, and the unique index and primary agree 1:1.
 *	One VERDICT line.
 *
 *	Usage: typed_recover <home> <db_recover-path>
 */
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

/*
 * Data record: field 0 = a 4-byte u32 "amount" (NOT NULL; 0xFFFFFFFF means
 * NULL); the key is a 4-byte u32 id.  The UNIQUE constraint is on the amount,
 * so no two rows may share an amount.  The data codec is non-MEMCMP (ordered
 * int) so a Btree open would install __typed_bt_compare if codecs were present
 * -- but the key here is left untyped (MEMCMP ids), and the UNIQUE secondary
 * carries the ordered-int codec.  During recovery none of this runs.
 */
static int
dfield(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	const u_int8_t *d = rec->data;
	(void)rc;
	if (rec->size < 4) return (DB_CODEC_MALFORMED);
	if (i != 0) return (DB_CODEC_MALFORMED);
	*pp = d; *lenp = 4;
	*isnull = (d[0] == 0xFF && d[1] == 0xFF && d[2] == 0xFF && d[3] == 0xFF);
	return (0);
}
static int
icmp(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	u_int32_t x, y;
	(void)c;
	if (al != 4 || bl != 4) return (0);
	x = ((u_int32_t)a[0]<<24)|((u_int32_t)a[1]<<16)|((u_int32_t)a[2]<<8)|a[3];
	y = ((u_int32_t)b[0]<<24)|((u_int32_t)b[1]<<16)|((u_int32_t)b[2]<<8)|b[3];
	return (x < y ? -1 : x > y ? 1 : 0);
}
static u_int32_t
ihash(const DB_CODEC *c, const u_int8_t *p, u_int32_t len)
{
	u_int32_t h = 2166136261u, i;
	(void)c;
	for (i = 0; i < len; i++) { h ^= p[i]; h *= 16777619u; }
	return (h);
}
/*
 * Big-endian u32 is already memcmp-ordered, so the amount codec is
 * MEMCMP+EQUALIMAGE: that satisfies the UNIQUE constraint (which needs an
 * EQUALIMAGE/MEMCMP codec or a sortkey) and keeps the test focused on
 * recovery, not on the generated comparator.
 */
static DB_CODEC intc = {
	"amt", 1,
	DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, icmp, NULL, ihash, NULL, NULL, NULL, NULL, 0
};
static const DB_CODEC *dtypes[1] = { &intc };
static DB_RECORD_CODEC drc = {
	"drec", 1, DB_CODEC_TOTAL, 1, dfield, NULL, dtypes, NULL
};
static DB_CONSTRAINT cons[2];
static DB *udbp;	/* UNIQUE secondary, caller-owned */

static void
fill_schema(DB_SCHEMA *s)
{
	memset(s, 0, sizeof(*s));
	s->type = DB_BTREE;
	s->data = &drc;
	memset(cons, 0, sizeof(cons));
	cons[0].kind = DB_CONSTRAINT_NOT_NULL;
	cons[0].side = DB_TYPED_DATA; cons[0].field = 0; cons[0].name = "nn_amt";
	cons[1].kind = DB_CONSTRAINT_UNIQUE;
	cons[1].side = DB_TYPED_DATA; cons[1].field = 0; cons[1].name = "uq_amt";
	cons[1].unique_db = udbp;
	s->constraints = cons; s->nconstraints = 2;
}

static void
be32(unsigned char *b, u_int32_t v) { b[0]=v>>24; b[1]=v>>16; b[2]=v>>8; b[3]=v; }

/* open env + typed primary (and its unique secondary).  create: 1 to create.
 * In a txn env both opens use DB_AUTO_COMMIT so the databases are
 * transactional (a non-txn secondary cannot take the primary's put txn). */
static int
open_all(DB_ENV **ep, DB **pp, const char *home, u_int32_t eflags, int create)
{
	DB_ENV *e;
	DB *p;
	DB_SCHEMA s;
	u_int32_t oflags;
	int ret;

	if ((ret = db_env_create(&e, 0)) != 0) return (ret);
	if ((ret = e->open(e, home, eflags, 0600)) != 0) return (ret);
	*ep = e;
	oflags = DB_AUTO_COMMIT | (create ? DB_CREATE : 0);

	/* UNIQUE secondary index: a plain non-DUP Btree, caller-owned. */
	if ((ret = db_create(&udbp, e, 0)) != 0) return (ret);
	if ((ret = udbp->open(udbp, NULL, "uq.db", "uidx", DB_BTREE,
	    oflags, 0600)) != 0) return (ret);

	if ((ret = db_create(&p, e, 0)) != 0) return (ret);
	fill_schema(&s);
	if ((ret = db_set_schema(p, &s, 0)) != 0) return (ret);
	if ((ret = p->open(p, NULL, "prim.db", "p", DB_BTREE,
	    oflags, 0600)) != 0) return (ret);
	*pp = p;
	return (0);
}

static int
put_row(DB_ENV *e, DB *p, u_int32_t id, u_int32_t amt, int commit)
{
	DB_TXN *t;
	DBT k, d;
	unsigned char kb[4], db[4];
	int ret;

	if ((ret = e->txn_begin(e, NULL, &t, 0)) != 0) return (ret);
	be32(kb, id); be32(db, amt);
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = kb; k.size = 4; d.data = db; d.size = 4;
	ret = p->put(p, t, &k, &d, 0);
	if (ret != 0) { (void)t->abort(t); return (ret); }
	return (commit ? t->commit(t, 0) : 0 /* leave open; caller dies */);
}

/* The forked writer: commit two rows, then start an uncommitted txn with a
 * third row and kill -9 before committing. */
static int
writer(const char *home)
{
	DB_ENV *e; DB *p;
	if (open_all(&e, &p, home,
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0) != 0)
		_exit(2);
	if (put_row(e, p, 10, 100, 1) != 0) _exit(3);
	if (put_row(e, p, 11, 101, 1) != 0) _exit(3);
	/* Uncommitted txn with id=12/amt=102, then die mid-txn. */
	if (put_row(e, p, 12, 102, 0) != 0) _exit(3);
	/* Flush log so the committed rows are durable, but DO NOT commit the
	 * open txn.  kill -9 leaves it to be rolled back by recovery. */
	(void)e->log_flush(e, NULL);
	(void)raise(SIGKILL);
	_exit(0);
}

int
main(int argc, char *argv[])
{
	DB_ENV *e; DB *p; DBC *c;
	DBT k, d;
	pid_t pid;
	int status, ret, n, nprim, nidx;
	const char *home = (argc > 1) ? argv[1] : ".";
	const char *recov = (argc > 2) ? argv[2] : "db_recover";
	char cmd[2048];

	/* 1. Create the env and typed primary; seed committed rows. */
	if (open_all(&e, &p, home,
	    DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN, 1) != 0) {
		printf("VERDICT typed_recover FAIL create\n"); return (1); }
	CK(put_row(e, p, 1, 1, 1) == 0, "seed row id=1 amt=1");
	CK(put_row(e, p, 2, 2, 1) == 0, "seed row id=2 amt=2");
	/* a NOT NULL violation must still be refused before the crash. */
	{
		DB_TXN *t; DBT bk, bd; unsigned char kb[4], db[4];
		e->txn_begin(e, NULL, &t, 0);
		be32(kb, 99); memset(db, 0xFF, 4);
		memset(&bk,0,sizeof bk); memset(&bd,0,sizeof bd);
		bk.data=kb; bk.size=4; bd.data=db; bd.size=4;
		ret = p->put(p, t, &bk, &bd, 0);
		(void)t->abort(t);
		CK(ret == DB_CONSTRAINT_VIOLATION, "NOT NULL refused pre-crash");
	}
	(void)p->close(p, 0);
	(void)udbp->close(udbp, 0);
	(void)e->close(e, 0);

	/* 2. Fork the writer; it kill -9's itself mid-txn. */
	pid = fork();
	if (pid < 0) { printf("VERDICT typed_recover FAIL fork\n"); return (1); }
	if (pid == 0)
		return (writer(home));
	(void)waitpid(pid, &status, 0);
	CK(WIFSIGNALED(status) && WTERMSIG(status) == SIGKILL,
	    "writer died by SIGKILL mid-txn");

	/* 3. Run the REAL db_recover utility with NO codecs registered. */
	snprintf(cmd, sizeof(cmd), "%s -h '%s' 2>&1", recov, home);
	ret = system(cmd);
	CK(ret == 0, "db_recover (no codecs) exited 0");

	/* 4. Reopen WITH the schema; full scan + reference model.  db_recover
	 * removed the shared region files, so the env reopen needs DB_CREATE to
	 * recreate them; the on-disk databases persist and open create=0. */
	if ((ret = open_all(&e, &p, home,
	    DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN, 0)) != 0) {
		printf("VERDICT typed_recover FAIL reopen ret=%d (%s)\n",
		    ret, db_strerror(ret));
		return (1);
	}
	/* committed: ids {1,2,10,11}; uncommitted id 12 must be ABSENT. */
	nprim = 0;
	if (p->cursor(p, NULL, &c, 0) == 0) {
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		while (c->get(c, &k, &d, DB_NEXT) == 0) {
			const unsigned char *kp = k.data, *dp = d.data;
			u_int32_t id = ((u_int32_t)kp[0]<<24)|((u_int32_t)kp[1]<<16)|
			    ((u_int32_t)kp[2]<<8)|kp[3];
			/* NOT NULL must hold on every surviving row. */
			if (dp[0]==0xFF&&dp[1]==0xFF&&dp[2]==0xFF&&dp[3]==0xFF) {
				printf("  FAIL: NULL amount survived (id=%u)\n", id);
				nfail++;
			}
			if (id == 12) {
				printf("  FAIL: uncommitted id=12 survived\n");
				nfail++;
			}
			nprim++;
		}
		(void)c->close(c);
	}
	CK(nprim == 4, "exactly the 4 committed rows survived (ids 1,2,10,11)");

	/* unique index <-> primary bijection: same count, each primary row's
	 * amount present once in the index. */
	nidx = 0;
	if (udbp->cursor(udbp, NULL, &c, 0) == 0) {
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		while (c->get(c, &k, &d, DB_NEXT) == 0) nidx++;
		(void)c->close(c);
	}
	CK(nidx == nprim, "unique index and primary agree 1:1 after recovery");

	(void)p->close(p, 0);
	(void)udbp->close(udbp, 0);
	(void)e->close(e, 0);

	n = nfail;
	if (n == 0)
		printf("VERDICT typed_recover PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_recover FAIL %d of %d checks\n", n, ncase);
	return (n == 0 ? 0 : 1);
}

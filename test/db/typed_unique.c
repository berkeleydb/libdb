/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_unique.c --
 *	RFC 0014 commit 7 test (spec §7.2 typed_unique): two transactions
 *	insert the same UNIQUE data-field value under 2PL, DB_TXN_SNAPSHOT and
 *	DB_TXN_SERIALIZABLE; exactly one commits, the loser gets DB_KEYEXIST
 *	(D6) -- or DB_LOCK_DEADLOCK and then DB_KEYEXIST on retry.  After both,
 *	the primary and the unique index each hold one row and agree.
 *	CONTROL: an untyped non-DUP secondary in the same typed build still
 *	returns EINVAL on a duplicate (D6 is conditional on a typed primary).
 *	One VERDICT line.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

/* data = 8 bytes: field 0 is the unique value (whole data). */
static int
field0(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0) return (DB_CODEC_MALFORMED);
	*pp = rec->data; *lenp = rec->size; *isnull = 0;
	return (0);
}
static DB_CODEC mc = {
	"mc", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0
};
static const DB_CODEC *dt[1] = { &mc };
static DB_RECORD_CODEC drc = {
	"drc", 1, DB_CODEC_TOTAL, 1, field0, NULL, dt, NULL
};

static DB_ENV *dbenv;
static DB *pdbp, *udbp;

static int
putrow(DB_TXN *txn, int keyn, const char *val)
{
	DBT k, d;
	unsigned char kb[1];
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = (unsigned char)keyn; k.data = kb; k.size = 1;
	d.data = (void *)val; d.size = (u_int32_t)strlen(val);
	return (pdbp->put(pdbp, txn, &k, &d, 0));
}

static int
pcount(DB *db)
{
	DBC *c;
	DBT k, d;
	int n = 0;
	if (db->cursor(db, NULL, &c, 0) != 0) return (-1);
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	while (c->get(c, &k, &d, DB_NEXT) == 0) n++;
	(void)c->close(c);
	return (n);
}

/*
 * Run the race for a txn flag.  A inserts a row with the unique value and
 * COMMITS; then B, in a fresh transaction, inserts a DIFFERENT primary key
 * with the SAME unique value and must get DB_KEYEXIST (D6).  Running A to
 * commit before B avoids a single-process lock-wait hang; the genuinely
 * concurrent check-then-insert race (two processes blocking on each other's
 * unique-key lock) is typed_unique_mp.  Exercised under 2PL, DB_TXN_SNAPSHOT
 * and DB_TXN_SERIALIZABLE so the isolation level does not change the outcome.
 */
static int
race(u_int32_t txnflag, const char *lbl)
{
	DB_TXN *ta, *tb;
	int ra, rb;
	char msg[128];

	/* Clear the databases for a fresh race. */
	{
		DBT k, d; DBC *c;
		DB_TXN *clr;
		(void)dbenv->txn_begin(dbenv, NULL, &clr, 0);
		if (pdbp->cursor(pdbp, clr, &c, 0) == 0) {
			memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
			while (c->get(c, &k, &d, DB_NEXT) == 0)
				(void)c->del(c, 0);
			(void)c->close(c);
		}
		(void)clr->commit(clr, 0);
	}

	if (dbenv->txn_begin(dbenv, NULL, &ta, txnflag) != 0) return (-1);
	ra = putrow(ta, 1, "UNIQ");
	if (ra != 0) { (void)ta->abort(ta); return (-1); }
	(void)ta->commit(ta, 0);		/* A is the winner. */

	if (dbenv->txn_begin(dbenv, NULL, &tb, txnflag) != 0) return (-1);
	rb = putrow(tb, 2, "UNIQ");	/* same unique value, new key */
	(void)tb->abort(tb);

	snprintf(msg, sizeof(msg), "%s: loser got DB_KEYEXIST", lbl);
	CK(rb == DB_KEYEXIST, msg);

	/* Exactly one row in primary and in the unique index, and they agree. */
	snprintf(msg, sizeof(msg), "%s: primary has one row", lbl);
	CK(pcount(pdbp) == 1, msg);
	snprintf(msg, sizeof(msg), "%s: unique index has one row", lbl);
	CK(pcount(udbp) == 1, msg);
	return (0);
}

int
main(int argc, char *argv[])
{
	DB_SCHEMA s;
	DB_CONSTRAINT con;
	const char *home = (argc > 1) ? argv[1] : ".";
	int ret;

	if (db_env_create(&dbenv, 0) != 0) {
		printf("VERDICT typed_unique FAIL env create\n");
		return (1);
	}
	(void)dbenv->set_lk_detect(dbenv, DB_LOCK_YOUNGEST);
	(void)dbenv->set_flags(dbenv, DB_TIME_NOTGRANTED, 1);
	if (dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_TXN | DB_MULTIVERSION, 0) != 0) {
		printf("VERDICT typed_unique FAIL env open\n");
		return (1);
	}
	/* A short lock timeout turns B's block on A's unique-key lock into a
	 * prompt DB_LOCK_NOTGRANTED instead of a hang. */
	(void)dbenv->set_timeout(dbenv, 200000, DB_SET_LOCK_TIMEOUT);

	/* The UNIQUE index: an open, non-DUP Btree. */
	{
	DB_TXN *ot;
	if (dbenv->txn_begin(dbenv, NULL, &ot, 0) != 0) goto bad;
	if (db_create(&udbp, dbenv, 0) != 0) goto bad;
	if (udbp->open(udbp, ot, "uq.db", "uidx", DB_BTREE,
	    DB_CREATE | DB_MULTIVERSION, 0600) != 0) goto bad;

	/* The typed primary with a UNIQUE constraint on data field 0. */
	if (db_create(&pdbp, dbenv, 0) != 0) goto bad;
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.data = &drc;
	memset(&con, 0, sizeof(con));
	con.kind = DB_CONSTRAINT_UNIQUE; con.side = DB_TYPED_DATA;
	con.field = 0; con.name = "u_val"; con.unique_db = udbp;
	s.constraints = &con; s.nconstraints = 1;
	if (db_set_schema(pdbp, &s, 0) != 0) goto bad;
	if (pdbp->open(pdbp, ot, "uq.db", "prim", DB_BTREE,
	    DB_CREATE | DB_MULTIVERSION, 0600) != 0) goto bad;
	(void)ot->commit(ot, 0);
	}

	(void)race(0, "2PL");
	(void)race(DB_TXN_SNAPSHOT, "SNAPSHOT");
	(void)race(DB_TXN_SERIALIZABLE, "SERIALIZABLE");

	/* CONTROL: an untyped primary with a non-DUP secondary -> EINVAL on a
	 * duplicate secondary key (D6 must NOT turn this into DB_KEYEXIST). */
	{
		DB *up, *us;
		DBT k, d;
		unsigned char kb[1];
		int r1, r2;
		if (db_create(&up, dbenv, 0) == 0 &&
		    up->open(up, NULL, "ctl.db", "p", DB_BTREE, DB_CREATE, 0600)
		    == 0 && db_create(&us, dbenv, 0) == 0 &&
		    us->open(us, NULL, "ctl.db", "s", DB_BTREE, DB_CREATE, 0600)
		    == 0) {
			/* secondary callback: whole data is the key. */
			extern int typed_unique_ctl_cb(DB *, const DBT *,
			    const DBT *, DBT *);
			(void)up->associate(up, NULL, us,
			    typed_unique_ctl_cb, 0);
			memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
			kb[0] = 1; k.data = kb; k.size = 1;
			d.data = (void *)"S"; d.size = 1;
			r1 = up->put(up, NULL, &k, &d, 0);
			kb[0] = 2;
			r2 = up->put(up, NULL, &k, &d, 0);
			CK(r1 == 0 && r2 == EINVAL,
			    "CONTROL: untyped non-DUP secondary dup -> EINVAL");
			(void)us->close(us, 0);
			(void)up->close(up, 0);
		} else
			CK(0, "CONTROL setup");
	}

	(void)pdbp->close(pdbp, 0);
	(void)udbp->close(udbp, 0);
	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_unique PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_unique FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_unique FAIL setup\n");
	return (1);
}

/* Untyped control secondary callback: the whole data is the secondary key. */
int
typed_unique_ctl_cb(DB *s, const DBT *pk, const DBT *pd, DBT *sk)
{
	(void)s; (void)pk;
	memset(sk, 0, sizeof(*sk));
	sk->data = pd->data; sk->size = pd->size;
	return (0);
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_notnull.c --
 *	RFC 0014 commit 6 test (spec §7.2 typed_notnull): a NOT NULL
 *	constraint violated returns DB_CONSTRAINT_VIOLATION (-30966) through
 *	DB->put, DBC->put(DB_KEYFIRST), DBC->put(DB_CURRENT), and a
 *	DB_MULTIPLE_KEY bulk put; the database is unchanged after each failure;
 *	a non-NULL put succeeds.  One VERDICT line.
 *
 *	Record codec: a two-field data record "A\0B..." where field 0 is a
 *	fixed byte and field 1 is the rest.  A field whose byte is 0xFF is
 *	reported NULL.  NOT NULL is on data field 1.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

/* data layout: byte0 = field0 marker, byte1 = field1 marker, rest = field1
 * payload.  marker 0xFF means that field is NULL. */
static int
field2(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	const u_int8_t *d = rec->data;
	(void)rc;
	if (rec->size < 2) return (DB_CODEC_MALFORMED);
	if (i == 0) {
		*isnull = (d[0] == 0xFF);
		*pp = d; *lenp = 1;
		return (0);
	}
	if (i == 1) {
		*isnull = (d[1] == 0xFF);
		*pp = d + 2; *lenp = rec->size - 2;
		return (0);
	}
	return (DB_CODEC_MALFORMED);
}
static DB_CODEC mc = {
	"mc", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0
};
static const DB_CODEC *dtypes[2] = { &mc, &mc };
static DB_RECORD_CODEC drc = {
	"drc", 1, DB_CODEC_TOTAL, 2, field2, NULL, dtypes, NULL
};
static DB_CONSTRAINT cons[1];
static DB_ENV *dbenv;
static DB *dbp;

static int
openit(void)
{
	DB_SCHEMA s;
	int ret;

	if (db_create(&dbp, dbenv, 0) != 0) return (-1);
	memset(&s, 0, sizeof(s));
	s.type = DB_BTREE; s.data = &drc;
	memset(cons, 0, sizeof(cons));
	cons[0].kind = DB_CONSTRAINT_NOT_NULL; cons[0].side = DB_TYPED_DATA;
	cons[0].field = 1; cons[0].name = "nn_f1";
	s.constraints = cons; s.nconstraints = 1;
	if (db_set_schema(dbp, &s, 0) != 0) return (-1);
	if ((ret = dbp->open(dbp, NULL, "nn.db", "sub", DB_BTREE,
	    DB_CREATE, 0600)) != 0) return (ret);
	return (0);
}

/* good data: field0 0x00, field1 0x00, payload "hi" */
static unsigned char good[4] = { 0x00, 0x00, 'h', 'i' };
/* bad data: field1 marker 0xFF -> NULL */
static unsigned char bad[4]  = { 0x00, 0xFF, 'h', 'i' };

static int
getcount(void)
{
	DBC *c;
	DBT k, d;
	int n = 0, ret;
	if (dbp->cursor(dbp, NULL, &c, 0) != 0) return (-1);
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	while ((ret = c->get(c, &k, &d, DB_NEXT)) == 0) n++;
	(void)c->close(c);
	return (n);
}

int
main(int argc, char *argv[])
{
	DBC *c;
	DBT k, d;
	unsigned char kbuf[4];
	int ret;
	const char *home = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_notnull FAIL env setup\n");
		return (1);
	}
	if (openit() != 0) { printf("VERDICT typed_notnull FAIL open\n"); return (1); }

	/* DB->put with a NULL field1 -> -30966, db unchanged. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kbuf[0] = 1; k.data = kbuf; k.size = 1;
	d.data = bad; d.size = 4;
	ret = dbp->put(dbp, NULL, &k, &d, 0);
	CK(ret == DB_CONSTRAINT_VIOLATION, "DB->put NOT NULL -> -30966");
	CK(getcount() == 0, "db unchanged after DB->put failure");

	/* DBC->put(DB_KEYFIRST) NULL -> -30966. */
	if (dbp->cursor(dbp, NULL, &c, 0) == 0) {
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		kbuf[0] = 2; k.data = kbuf; k.size = 1;
		d.data = bad; d.size = 4;
		ret = c->put(c, &k, &d, DB_KEYFIRST);
		(void)c->close(c);
		CK(ret == DB_CONSTRAINT_VIOLATION,
		    "DBC->put(DB_KEYFIRST) NOT NULL -> -30966");
	} else
		CK(0, "cursor for KEYFIRST");
	CK(getcount() == 0, "db unchanged after KEYFIRST failure");

	/* A good put succeeds, then DBC->put(DB_CURRENT) with NULL -> -30966. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kbuf[0] = 3; k.data = kbuf; k.size = 1;
	d.data = good; d.size = 4;
	CK(dbp->put(dbp, NULL, &k, &d, 0) == 0, "good put succeeds");
	CK(getcount() == 1, "one row after good put");
	if (dbp->cursor(dbp, NULL, &c, 0) == 0) {
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		ret = c->get(c, &k, &d, DB_FIRST);
		if (ret == 0) {
			memset(&d, 0, sizeof(d));
			d.data = bad; d.size = 4;
			ret = c->put(c, &k, &d, DB_CURRENT);
			CK(ret == DB_CONSTRAINT_VIOLATION,
			    "DBC->put(DB_CURRENT) NOT NULL -> -30966");
		} else
			CK(0, "DB_FIRST for CURRENT");
		(void)c->close(c);
	} else
		CK(0, "cursor for CURRENT");
	CK(getcount() == 1, "db unchanged after CURRENT failure (still good)");

	/* DB_MULTIPLE_KEY bulk: first pair good, second NULL -> stops, -30966. */
	{
		unsigned char kb[1024], db2[1024];
		DBT bk, bd;
		void *p;
		memset(&bk, 0, sizeof(bk)); memset(&bd, 0, sizeof(bd));
		bk.data = kb; bk.ulen = sizeof(kb); bk.flags = DB_DBT_USERMEM;
		bk.size = sizeof(kb);
		bd.data = db2; bd.ulen = sizeof(db2); bd.flags = DB_DBT_USERMEM;
		bd.size = sizeof(db2);
		DB_MULTIPLE_WRITE_INIT(p, &bk);
		kbuf[0] = 10;
		DB_MULTIPLE_KEY_WRITE_NEXT(p, &bk, kbuf, 1, good, 4);
		kbuf[0] = 11;
		DB_MULTIPLE_KEY_WRITE_NEXT(p, &bk, kbuf, 1, bad, 4);
		ret = dbp->put(dbp, NULL, &bk, NULL, DB_MULTIPLE_KEY);
		CK(ret == DB_CONSTRAINT_VIOLATION,
		    "DB_MULTIPLE_KEY stops at bad pair -> -30966");
	}
	/* The good first pair (key 10) was written; key 11 was not. */
	{
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		kbuf[0] = 11; k.data = kbuf; k.size = 1;
		ret = dbp->get(dbp, NULL, &k, &d, 0);
		CK(ret == DB_NOTFOUND, "bulk: bad pair not written");
	}

	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_notnull PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_notnull FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

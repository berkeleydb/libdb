/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_unique_nulls.c --
 *	RFC 0014 commit 7 test (spec §7.2 typed_unique_nulls):
 *	  NULLS DISTINCT (default): two NULL-field rows both commit, and the
 *	    unique index gets no entry for them (DB_DONOTINDEX);
 *	  NULLS_NOT_DISTINCT: the second NULL-field row -> DB_KEYEXIST;
 *	  a non-NULL value whose bytes equal the NULL image (0x00) does NOT
 *	    collide with a NULL row (non-NULL images are 0x01-prefixed).
 *	One VERDICT line.
 *
 *	data byte 0 is a NULL marker (0xFF = NULL), byte 1.. the field value.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

/* field 0: NULL if byte0==0xFF, else bytes [1..). */
static int
field0(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	const u_int8_t *d = rec->data;
	(void)rc;
	if (i != 0 || rec->size < 1) return (DB_CODEC_MALFORMED);
	*isnull = (d[0] == 0xFF);
	*pp = d + 1; *lenp = rec->size - 1;
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

static int
put(DB *p, int keyn, const void *data, u_int32_t len)
{
	DBT k, d;
	unsigned char kb[1];
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = (unsigned char)keyn; k.data = kb; k.size = 1;
	d.data = (void *)data; d.size = len;
	return (p->put(p, NULL, &k, &d, 0));
}

static int
build(DB **udbpp, DB **pdbpp, const char *file, u_int32_t conflags)
{
	DB *u, *p;
	DB_SCHEMA s;
	static DB_CONSTRAINT con;

	if (db_create(&u, dbenv, 0) != 0) return (-1);
	if (u->open(u, NULL, file, "uidx", DB_BTREE, DB_CREATE, 0600) != 0)
		return (-1);
	if (db_create(&p, dbenv, 0) != 0) return (-1);
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.data = &drc;
	memset(&con, 0, sizeof(con));
	con.kind = DB_CONSTRAINT_UNIQUE; con.side = DB_TYPED_DATA;
	con.field = 0; con.name = "u_val"; con.unique_db = u;
	con.flags = conflags;
	s.constraints = &con; s.nconstraints = 1;
	if (db_set_schema(p, &s, 0) != 0) return (-1);
	if (p->open(p, NULL, file, "prim", DB_BTREE, DB_CREATE, 0600) != 0)
		return (-1);
	*udbpp = u; *pdbpp = p;
	return (0);
}

int
main(int argc, char *argv[])
{
	DB *u, *p;
	const char *home = (argc > 1) ? argv[1] : ".";
	unsigned char nullrow[1], valrow[2], zerorow[1];
	int ret;

	nullrow[0] = 0xFF;			/* NULL field */
	valrow[0] = 0x00; valrow[1] = 'X';	/* non-NULL "X" */
	zerorow[0] = 0x00;			/* non-NULL, EMPTY field */

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_unique_nulls FAIL env setup\n");
		return (1);
	}

	/* ---- NULLS DISTINCT (default): two NULL rows both commit, no index
	 * entries for them. ---- */
	if (build(&u, &p, "nd.db", 0) != 0) goto bad;
	CK(put(p, 1, nullrow, 1) == 0, "NULLS DISTINCT: first NULL row OK");
	CK(put(p, 2, nullrow, 1) == 0, "NULLS DISTINCT: second NULL row OK");
	CK(pcount(p) == 2, "NULLS DISTINCT: both NULL rows in primary");
	CK(pcount(u) == 0, "NULLS DISTINCT: no index entry for NULLs");
	/* A non-NULL value indexes normally. */
	CK(put(p, 3, valrow, 2) == 0, "NULLS DISTINCT: non-NULL row OK");
	CK(pcount(u) == 1, "NULLS DISTINCT: non-NULL row indexed");
	(void)p->close(p, 0); (void)u->close(u, 0);

	/* ---- NULLS_NOT_DISTINCT: second NULL -> DB_KEYEXIST; a non-NULL
	 * single 0x00 byte does NOT collide with the NULL image. ---- */
	if (build(&u, &p, "nnd.db", DB_CONSTRAINT_NULLS_NOT_DISTINCT) != 0)
		goto bad;
	CK(put(p, 1, nullrow, 1) == 0, "NULLS_NOT_DISTINCT: first NULL OK");
	ret = put(p, 2, nullrow, 1);
	CK(ret == DB_KEYEXIST, "NULLS_NOT_DISTINCT: second NULL -> DB_KEYEXIST");
	/* zerorow is non-NULL with an EMPTY field; its index key is the single
	 * byte 0x01, which must NOT equal the NULL image 0x00.  Drop the 0x01
	 * prefix (the sabotage) and this key becomes 0x00 == the NULL image. */
	CK(put(p, 3, zerorow, 1) == 0,
	    "non-NULL empty value does not collide with the NULL image");
	(void)p->close(p, 0); (void)u->close(u, 0);

	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_unique_nulls PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_unique_nulls FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_unique_nulls FAIL setup\n");
	return (1);
}

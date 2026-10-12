/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_maxlen.c --
 *	RFC 0014 commit 6 test (spec §7.2 typed_maxlen): a MAXLEN constraint
 *	accepts a field of exactly maxlen bytes, rejects maxlen+1 with
 *	DB_CONSTRAINT_VIOLATION (-30966), and the same for DB_TYPED_WHOLE at
 *	the whole-DBT boundary.  One VERDICT line.
 *
 *	data field 0 = whole data DBT (a single-field record over the bytes).
 *	Two constraints: MAXLEN 4 on field 0, and MAXLEN 8 on DB_TYPED_WHOLE.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

/* Single-field record: field 0 is the whole DBT (its real length). */
static int
field1(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0) return (DB_CODEC_MALFORMED);
	*pp = rec->data;
	*lenp = rec->size;	/* field 0 == whole record */
	*isnull = 0;
	return (0);
}
static DB_CODEC mc = {
	"mc", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0
};
static const DB_CODEC *dt[1] = { &mc };
static DB_RECORD_CODEC drc = {
	"drc", 1, DB_CODEC_TOTAL, 1, field1, NULL, dt, NULL
};
static DB_CONSTRAINT cons[2];
static DB_ENV *dbenv;
static DB *dbp;

static int
put(int keyn, const void *data, u_int32_t len)
{
	DBT k, d;
	unsigned char kb[4];
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = (unsigned char)keyn; k.data = kb; k.size = 1;
	d.data = (void *)data; d.size = len;
	return (dbp->put(dbp, NULL, &k, &d, 0));
}

int
main(int argc, char *argv[])
{
	DB_SCHEMA s;
	const char *home = (argc > 1) ? argv[1] : ".";
	static unsigned char b16[16] = {0};

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_maxlen FAIL env setup\n");
		return (1);
	}
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.data = &drc;
	memset(cons, 0, sizeof(cons));
	cons[0].kind = DB_CONSTRAINT_MAXLEN; cons[0].side = DB_TYPED_DATA;
	cons[0].field = 0; cons[0].maxlen = 4; cons[0].name = "ml_f0";
	cons[1].kind = DB_CONSTRAINT_MAXLEN; cons[1].side = DB_TYPED_DATA;
	cons[1].field = DB_TYPED_WHOLE; cons[1].maxlen = 8; cons[1].name = "ml_whole";
	s.constraints = cons; s.nconstraints = 2;
	if (db_set_schema(dbp, &s, 0) != 0) goto bad;
	if (dbp->open(dbp, NULL, "ml.db", "sub", DB_BTREE, DB_CREATE, 0600) != 0)
		goto bad;

	/* field 0 == whole record.  cons[0] MAXLEN 4, cons[1] WHOLE MAXLEN 8. */
	/* 4 bytes: field 0 len 4 == maxlen OK, whole 4 <= 8 OK. */
	CK(put(1, b16, 4) == 0, "field len == maxlen (4) succeeds");
	/* 5 bytes: field 0 len 5 > maxlen 4 -> -30966 (field boundary). */
	CK(put(2, b16, 5) == DB_CONSTRAINT_VIOLATION,
	    "field maxlen+1 (5) -> -30966");
	/* whole DBT: 8 bytes -> field 0 len 8 > 4, so this would also fail the
	 * field constraint.  To isolate the WHOLE boundary, drop cons[0] to a
	 * larger field maxlen first.  Reopen with field maxlen 16. */
	(void)dbp->close(dbp, 0);
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.data = &drc;
	memset(cons, 0, sizeof(cons));
	cons[0].kind = DB_CONSTRAINT_MAXLEN; cons[0].side = DB_TYPED_DATA;
	cons[0].field = DB_TYPED_WHOLE; cons[0].maxlen = 8; cons[0].name = "ml_whole";
	s.constraints = cons; s.nconstraints = 1;
	if (db_set_schema(dbp, &s, 0) != 0) goto bad;
	if (dbp->open(dbp, NULL, "ml.db", "sub2", DB_BTREE, DB_CREATE, 0600) != 0)
		goto bad;
	/* whole DBT: exactly 8 bytes OK. */
	CK(put(3, b16, 8) == 0, "whole DBT == maxlen (8) succeeds");
	/* whole DBT: 9 bytes -> -30966 (the boundary the sabotage targets). */
	CK(put(4, b16, 9) == DB_CONSTRAINT_VIOLATION,
	    "whole DBT maxlen+1 (9) -> -30966");

	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_maxlen PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_maxlen FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_maxlen FAIL setup\n");
	return (1);
}

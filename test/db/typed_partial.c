/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_partial.c --
 *	RFC 0014 commit 6 test (spec §7.2 typed_partial): a put with
 *	DB_DBT_PARTIAL on the key or the data is refused EINVAL, and the
 *	record is unchanged.  One VERDICT line.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

static int
field1(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
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
	"drc", 1, DB_CODEC_TOTAL, 1, field1, NULL, dt, NULL
};
static DB_ENV *dbenv;
static DB *dbp;

static int
count(void)
{
	DBC *c;
	DBT k, d;
	int n = 0;
	if (dbp->cursor(dbp, NULL, &c, 0) != 0) return (-1);
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	while (c->get(c, &k, &d, DB_NEXT) == 0) n++;
	(void)c->close(c);
	return (n);
}

int
main(int argc, char *argv[])
{
	DB_SCHEMA s;
	DBT k, d;
	unsigned char kb[8], db2[8];
	int ret;
	const char *home = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_partial FAIL env setup\n");
		return (1);
	}
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.data = &drc;
	if (db_set_schema(dbp, &s, 0) != 0) goto bad;
	if (dbp->open(dbp, NULL, "pp.db", "sub", DB_BTREE, DB_CREATE, 0600) != 0)
		goto bad;

	memset(kb, 0, sizeof(kb)); memset(db2, 0, sizeof(db2));

	/* DB_DBT_PARTIAL on data -> EINVAL. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = 1; k.data = kb; k.size = 1;
	d.data = db2; d.size = 4; d.flags = DB_DBT_PARTIAL; d.doff = 0; d.dlen = 4;
	ret = dbp->put(dbp, NULL, &k, &d, 0);
	CK(ret == EINVAL, "DB_DBT_PARTIAL on data -> EINVAL");

	/* DB_DBT_PARTIAL on key -> EINVAL. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = 2; k.data = kb; k.size = 1;
	k.flags = DB_DBT_PARTIAL; k.doff = 0; k.dlen = 1;
	d.data = db2; d.size = 4;
	ret = dbp->put(dbp, NULL, &k, &d, 0);
	CK(ret == EINVAL, "DB_DBT_PARTIAL on key -> EINVAL");

	CK(count() == 0, "no record written by either partial put");

	/* A non-partial put still works. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = 3; k.data = kb; k.size = 1;
	d.data = db2; d.size = 4;
	CK(dbp->put(dbp, NULL, &k, &d, 0) == 0, "non-partial put succeeds");
	CK(count() == 1, "one record after non-partial put");

	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_partial PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_partial FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_partial FAIL setup\n");
	return (1);
}

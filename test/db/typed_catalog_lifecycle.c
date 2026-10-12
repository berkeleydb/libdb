/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_catalog_lifecycle.c --
 *	RFC 0014 commit 5 test (spec §7.2 typed_catalog_lifecycle): the
 *	catalog record exists after create, is deleted by DB->remove, moves
 *	with DB->rename, is absent after an ABORTED create, and survives
 *	DB->truncate.  The record presence is checked by opening __db_typed
 *	directly and get()-ing the key.  One VERDICT line.
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
static const DB_CODEC *types1[1] = { &mc };
static DB_RECORD_CODEC krc = {
	"krc", 1, DB_CODEC_TOTAL, 1, field0, NULL, types1, NULL
};
static DB_ENV *dbenv;

static void
schema(DB_SCHEMA *s) { memset(s, 0, sizeof(*s)); s->type = DB_BTREE; s->key = &krc; }

/* 1 if __db_typed in <file> has a record for <sub>, 0 if not, -1 on error. */
static int
catalog_has(const char *file, const char *sub)
{
	DB *cdbp;
	DBT k, d;
	int ret, rc;

	if (db_create(&cdbp, dbenv, 0) != 0) return (-1);
	ret = cdbp->open(cdbp, NULL, file, "__db_typed", DB_BTREE,
	    DB_RDONLY, 0);
	if (ret != 0) { (void)cdbp->close(cdbp, 0); return (ret == ENOENT ? 0 : -1); }
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = (void *)sub; k.size = (u_int32_t)strlen(sub);
	rc = cdbp->get(cdbp, NULL, &k, &d, 0);
	(void)cdbp->close(cdbp, 0);
	if (rc == 0) return (1);
	if (rc == DB_NOTFOUND) return (0);
	return (-1);
}

static int
create_typed(const char *file, const char *sub, DB_TXN *txn)
{
	DB *dbp;
	DB_SCHEMA s;
	int ret;

	if (db_create(&dbp, dbenv, 0) != 0) return (-1);
	schema(&s);
	if (db_set_schema(dbp, &s, 0) != 0) { (void)dbp->close(dbp, 0); return (-1); }
	ret = dbp->open(dbp, txn, file, sub, DB_BTREE, DB_CREATE, 0600);
	(void)dbp->close(dbp, txn == NULL ? 0 : DB_NOSYNC);
	return (ret);
}

int
main(int argc, char *argv[])
{
	DB *dbp;
	DB_TXN *txn;
	DB_SCHEMA s;
	int ret;
	const char *home = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 || dbenv->open(dbenv, home,
	    DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_TXN, 0) != 0) {
		printf("VERDICT typed_catalog_lifecycle FAIL env setup\n");
		return (1);
	}

	/* 1. Create -> catalog has the record. */
	CK(create_typed("lc.db", "sub", NULL) == 0, "create typed subdatabase");
	CK(catalog_has("lc.db", "sub") == 1, "catalog has record after create");

	/* 2. DB->remove deletes it. */
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	ret = dbp->remove(dbp, "lc.db", "sub", 0);	/* closes/frees dbp */
	CK(ret == 0, "DB->remove the typed subdatabase");
	CK(catalog_has("lc.db", "sub") == 0, "catalog record gone after remove");

	/* 3. DB->rename moves it, and the new name opens with the schema. */
	CK(create_typed("lc.db", "sub2", NULL) == 0, "re-create sub2");
	CK(catalog_has("lc.db", "sub2") == 1, "catalog has sub2");
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	ret = dbp->rename(dbp, "lc.db", "sub2", "sub3", 0);  /* frees dbp */
	CK(ret == 0, "DB->rename sub2 -> sub3");
	CK(catalog_has("lc.db", "sub2") == 0, "old name gone from catalog");
	CK(catalog_has("lc.db", "sub3") == 1, "new name in catalog");
	if (db_create(&dbp, dbenv, 0) == 0) {
		schema(&s);
		if (db_set_schema(dbp, &s, 0) == 0)
			ret = dbp->open(dbp, NULL, "lc.db", "sub3", DB_BTREE,
			    0, 0);
		else
			ret = -1;
		CK(ret == 0, "renamed sub3 opens with the schema");
		(void)dbp->close(dbp, 0);
	} else
		CK(0, "renamed sub3 opens (db_create failed)");

	/* 4. Aborted create leaves neither subdatabase nor record. */
	if (dbenv->txn_begin(dbenv, NULL, &txn, 0) != 0) goto bad;
	ret = create_typed("lc.db", "subA", txn);
	CK(ret == 0, "create subA in a txn");
	(void)txn->abort(txn);
	CK(catalog_has("lc.db", "subA") == 0,
	    "aborted create leaves no catalog record");

	/* 5. DB->truncate keeps the record. */
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	schema(&s);
	if (db_set_schema(dbp, &s, 0) == 0 &&
	    dbp->open(dbp, NULL, "lc.db", "sub3", DB_BTREE, 0, 0) == 0) {
		u_int32_t cnt;
		ret = dbp->truncate(dbp, NULL, &cnt, 0);
		CK(ret == 0, "DB->truncate the typed subdatabase");
	} else
		CK(0, "open sub3 for truncate");
	(void)dbp->close(dbp, 0);
	CK(catalog_has("lc.db", "sub3") == 1, "catalog record survives truncate");

	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_catalog_lifecycle PASS all %d checks\n",
		    ncase);
	else
		printf("VERDICT typed_catalog_lifecycle FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_catalog_lifecycle FAIL setup\n");
	return (1);
}

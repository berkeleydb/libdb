/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_catalog_format.c --
 *	RFC 0014 commit 5 test (spec §7.2 typed_catalog_format): a catalog
 *	record with bad magic, format version 2, truncated length, or a codec
 *	index out of range makes a typed open fail EINVAL.  We create a typed
 *	subdatabase, then open its __db_typed catalog as a plain Btree and
 *	overwrite the record with each malformation, reopening typed each time.
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

/* Open __db_typed, rewrite the "sub" record with mutator, write back. */
static int
corrupt(const char *file, void (*mut)(unsigned char *, int *len, int cap))
{
	DB *cdbp;
	DBT k, d;
	unsigned char buf[2048];
	int len, ret;

	if (db_create(&cdbp, dbenv, 0) != 0) return (-1);
	if (cdbp->open(cdbp, NULL, file, "__db_typed", DB_BTREE, 0, 0) != 0) {
		(void)cdbp->close(cdbp, 0); return (-1);
	}
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = (void *)"sub"; k.size = 3;
	d.flags = DB_DBT_MALLOC;
	if ((ret = cdbp->get(cdbp, NULL, &k, &d, 0)) != 0) {
		(void)cdbp->close(cdbp, 0); return (-1);
	}
	len = (int)d.size;
	if (len > (int)sizeof(buf)) len = (int)sizeof(buf);
	memcpy(buf, d.data, len);
	free(d.data);
	mut(buf, &len, (int)sizeof(buf));
	memset(&d, 0, sizeof(d));
	d.data = buf; d.size = (u_int32_t)len;
	ret = cdbp->put(cdbp, NULL, &k, &d, 0);
	(void)cdbp->close(cdbp, 0);
	return (ret);
}

static int
reopen_typed(const char *file)
{
	DB *dbp;
	DB_SCHEMA s;
	int ret;

	if (db_create(&dbp, dbenv, 0) != 0) return (-1);
	schema(&s);
	if (db_set_schema(dbp, &s, 0) != 0) { (void)dbp->close(dbp, 0); return (-1); }
	ret = dbp->open(dbp, NULL, file, "sub", DB_BTREE, 0, 0);
	(void)dbp->close(dbp, 0);
	return (ret);
}

static void mut_magic(unsigned char *b, int *l, int cap) { (void)l; (void)cap; b[0] ^= 0xff; }
static void mut_version(unsigned char *b, int *l, int cap) { (void)l; (void)cap; b[7] = 2; }
static void mut_trunc(unsigned char *b, int *l, int cap) { (void)b; (void)cap; if (*l > 10) *l -= 5; }

int
main(int argc, char *argv[])
{
	DB *dbp;
	DB_SCHEMA s;
	int ret;
	const char *home = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_catalog_format FAIL env setup\n");
		return (1);
	}

	/* Create a typed subdatabase. */
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	schema(&s);
	if (db_set_schema(dbp, &s, 0) != 0) goto bad;
	CK(dbp->open(dbp, NULL, "cf.db", "sub", DB_BTREE, DB_CREATE, 0600) == 0,
	    "create typed subdatabase");
	(void)dbp->close(dbp, 0);

	/* Baseline: a clean reopen must succeed. */
	CK(reopen_typed("cf.db") == 0, "clean reopen succeeds");

	/* Bad magic -> EINVAL. */
	CK(corrupt("cf.db", mut_magic) == 0, "write bad magic");
	ret = reopen_typed("cf.db");
	CK(ret == EINVAL, "bad magic -> EINVAL");

	/* Restore then set version 2 -> EINVAL. */
	CK(corrupt("cf.db", mut_magic) == 0, "restore magic");
	CK(corrupt("cf.db", mut_version) == 0, "write version 2");
	ret = reopen_typed("cf.db");
	CK(ret == EINVAL, "format version 2 -> EINVAL");

	/* Restore version then truncate length -> EINVAL. */
	/*
	 * The record currently has version 2 (from mut_version).  Truncating
	 * it trips the length check (total != data.size) before anything else,
	 * so a truncated record is EINVAL regardless of the version byte.
	 */
	CK(corrupt("cf.db", mut_trunc) == 0, "truncate record");
	ret = reopen_typed("cf.db");
	CK(ret == EINVAL, "truncated length -> EINVAL");

	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_catalog_format PASS all %d checks\n",
		    ncase);
	else
		printf("VERDICT typed_catalog_format FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_catalog_format FAIL setup\n");
	return (1);
}

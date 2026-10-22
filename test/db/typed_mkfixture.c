/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_mkfixture.c --
 *	Generate the on-disk fixtures for typed_old_reader (RFC 0014 §7.2):
 *	a Btree and a Hash typed subdatabase, each with the BTM_TYPED /
 *	DB_HASH_TYPED meta bit set (commit 4).  Catalog records are added by
 *	commit 5.  Built and run only in a --enable-typed build; the resulting
 *	files are committed under test/db/fixtures/ and read by the DEFAULT
 *	build, which must refuse them (BDB0055).
 *
 * Usage: typed_mkfixture <outdir>   writes typed_bt.db and typed_hash.db
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

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

static int
one(DB_ENV *dbenv, const char *file, DBTYPE t)
{
	DB *dbp;
	DB_SCHEMA s;
	DBT k, d;
	int ret;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0)
		return (ret);
	memset(&s, 0, sizeof(s));
	s.type = t; s.key = &krc;
	if ((ret = db_set_schema(dbp, &s, 0)) != 0) {
		(void)dbp->close(dbp, 0);
		return (ret);
	}
	if ((ret = dbp->open(dbp, NULL, file, "typedsub", t,
	    DB_CREATE, 0600)) != 0) {
		(void)dbp->close(dbp, 0);
		return (ret);
	}
	/* One record, so the file is non-trivial. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = (void *)"key"; k.size = 3;
	d.data = (void *)"val"; d.size = 3;
	(void)dbp->put(dbp, NULL, &k, &d, 0);
	return (dbp->close(dbp, 0));
}

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	char path[1024];
	const char *out;
	int ret;

	out = (argc > 1) ? argv[1] : ".";

	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->open(dbenv, out,
	    DB_CREATE | DB_INIT_MPOOL, 0)) != 0) {
		fprintf(stderr, "env: %s\n", db_strerror(ret));
		return (1);
	}
	if ((ret = one(dbenv, "typed_bt.db", DB_BTREE)) != 0) {
		fprintf(stderr, "bt: %s\n", db_strerror(ret));
		return (1);
	}
	if ((ret = one(dbenv, "typed_hash.db", DB_HASH)) != 0) {
		fprintf(stderr, "hash: %s\n", db_strerror(ret));
		return (1);
	}
	(void)dbenv->close(dbenv, 0);
	(void)snprintf(path, sizeof(path), "%s/typed_bt.db", out);
	printf("wrote %s and typed_hash.db\n", out);
	return (0);
}

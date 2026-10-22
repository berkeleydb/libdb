/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_rdonly_noschema.c --
 *	RFC 0014 commit 5 test (spec §7.2 typed_rdonly_noschema): a typed
 *	subdatabase opened with NO schema is allowed only read-only (§4.1
 *	row 3).  Create a typed subdatabase with a schema and one row; close;
 *	reopen schema-less DB_RDONLY (must open and read the row); reopen
 *	schema-less read-write (must EINVAL "db_set_schema required to open
 *	for writing").  The db_dump/db_load/db_verify D8 behaviour is tested
 *	separately by typed_util (commit 12).
 *
 *	Typed tier.  One VERDICT line.
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

int
main(int argc, char *argv[])
{
	DB *dbp;
	DB_SCHEMA s;
	DBT k, d;
	int ret;
	const char *home = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_rdonly_noschema FAIL env setup\n");
		return (1);
	}

	/* Create a typed subdatabase with one row. */
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.key = &krc;
	if (db_set_schema(dbp, &s, 0) != 0) goto bad;
	ret = dbp->open(dbp, NULL, "rd.db", "sub", DB_BTREE, DB_CREATE, 0600);
	CK(ret == 0, "create typed subdatabase");
	if (ret == 0) {
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		k.data = (void *)"abc"; k.size = 3;
		d.data = (void *)"xyz"; d.size = 3;
		CK(dbp->put(dbp, NULL, &k, &d, 0) == 0, "put one row");
	}
	(void)dbp->close(dbp, 0);

	/* Schema-less DB_RDONLY: must open and read the row. */
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	ret = dbp->open(dbp, NULL, "rd.db", "sub", DB_UNKNOWN, DB_RDONLY, 0);
	CK(ret == 0, "schema-less DB_RDONLY opens");
	if (ret == 0) {
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		k.data = (void *)"abc"; k.size = 3;
		CK(dbp->get(dbp, NULL, &k, &d, 0) == 0 && d.size == 3 &&
		    memcmp(d.data, "xyz", 3) == 0, "schema-less read sees row");
	} else
		CK(0, "schema-less read sees row (open failed)");
	(void)dbp->close(dbp, 0);

	/* Schema-less read-write: must EINVAL. */
	if (db_create(&dbp, dbenv, 0) != 0) goto bad;
	ret = dbp->open(dbp, NULL, "rd.db", "sub", DB_UNKNOWN, 0, 0);
	CK(ret == EINVAL, "schema-less read-write -> EINVAL");
	(void)dbp->close(dbp, 0);

	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_rdonly_noschema PASS all %d checks\n",
		    ncase);
	else
		printf("VERDICT typed_rdonly_noschema FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_rdonly_noschema FAIL setup\n");
	return (1);
}

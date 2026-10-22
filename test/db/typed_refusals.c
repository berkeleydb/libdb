/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_refusals.c --
 *	RFC 0014 commit 4 test (spec §7.2 typed_refusals): the Phase 1
 *	refusals.  Each case asserts an EXPECTED non-zero return; a wrong code
 *	is a FAIL.  One VERDICT line.
 *
 *	C6 (fname/dname NULL, named in-memory) is refused in __db_open_arg;
 *	Heap/Queue/Recno and DB_DUP-before-schema and set_bt_compare-before-
 *	schema in db_set_schema; DB_DUP-after-schema, compression and a
 *	callback overridden after db_set_schema in __typed_open; partitioning
 *	keeps the existing ENOENT/BDB0637 (no new code).
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;

#define	CK(cond, msg) do {						\
	ncase++;							\
	if (!(cond)) { printf("  FAIL [%d]: %s\n", ncase, msg); nfail++; }\
	else printf("  ok   [%d]: %s\n", ncase, msg);			\
} while (0)

static const char *HOME;

/* A MEMCMP key codec and a one-field record codec over it. */
static DB_CODEC mc = {
	"mc", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0
};
static const DB_CODEC *types1[1] = { &mc };

static int
field0(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0) return (DB_CODEC_MALFORMED);
	*pp = rec->data; *lenp = rec->size; *isnull = 0;
	return (0);
}
static DB_RECORD_CODEC krc = {
	"krc", 1, DB_CODEC_TOTAL, 1, field0, NULL, types1, NULL
};

static void
mkschema(DB_SCHEMA *s, DBTYPE t)
{
	memset(s, 0, sizeof(*s));
	s->type = t;
	s->key = &krc;
}

static DB_ENV *dbenv;

static DB *
newdb(void)
{
	DB *dbp;
	if (db_create(&dbp, dbenv, 0) != 0) return (NULL);
	return (dbp);
}

int
main(int argc, char *argv[])
{
	DB *dbp;
	DB_SCHEMA s;
	int ret;

	HOME = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_refusals FAIL env setup\n");
		return (1);
	}

	/* ---- C6: fname == NULL (in-memory) -> EINVAL at open. ---- */
	dbp = newdb(); mkschema(&s, DB_BTREE);
	CK(db_set_schema(dbp, &s, 0) == 0, "schema set (fname NULL case)");
	ret = dbp->open(dbp, NULL, NULL, "subonly", DB_BTREE, DB_CREATE, 0);
	CK(ret == EINVAL, "fname==NULL -> EINVAL");
	(void)dbp->close(dbp, 0);

	/* ---- C6: dname == NULL (one-per-file) -> EINVAL at open. ---- */
	dbp = newdb(); mkschema(&s, DB_BTREE);
	CK(db_set_schema(dbp, &s, 0) == 0, "schema set (dname NULL case)");
	ret = dbp->open(dbp, NULL, "fileonly.db", NULL, DB_BTREE,
	    DB_CREATE, 0);
	CK(ret == EINVAL, "dname==NULL -> EINVAL");
	(void)dbp->close(dbp, 0);

	/* ---- C6: named in-memory (fname NULL, dname given) -> EINVAL. ---- */
	dbp = newdb(); mkschema(&s, DB_BTREE);
	CK(db_set_schema(dbp, &s, 0) == 0, "schema set (named in-mem case)");
	ret = dbp->open(dbp, NULL, NULL, "inmem", DB_BTREE, DB_CREATE, 0);
	CK(ret == EINVAL, "named in-memory -> EINVAL");
	(void)dbp->close(dbp, 0);

	/* ---- Heap/Queue/Recno schema -> EINVAL at db_set_schema. ---- */
	dbp = newdb(); mkschema(&s, DB_QUEUE);
	CK(db_set_schema(dbp, &s, 0) == EINVAL, "DB_QUEUE schema -> EINVAL");
	(void)dbp->close(dbp, 0);
	dbp = newdb(); mkschema(&s, DB_RECNO);
	CK(db_set_schema(dbp, &s, 0) == EINVAL, "DB_RECNO schema -> EINVAL");
	(void)dbp->close(dbp, 0);
	dbp = newdb(); mkschema(&s, DB_HEAP);
	CK(db_set_schema(dbp, &s, 0) == EINVAL, "DB_HEAP schema -> EINVAL");
	(void)dbp->close(dbp, 0);

	/* ---- DB_DUP set BEFORE db_set_schema -> EINVAL (rule 8). ---- */
	dbp = newdb();
	(void)dbp->set_flags(dbp, DB_DUP);
	mkschema(&s, DB_BTREE);
	CK(db_set_schema(dbp, &s, 0) == EINVAL,
	    "DB_DUP before set_schema -> EINVAL");
	(void)dbp->close(dbp, 0);

	/* ---- DB_DUP set AFTER db_set_schema -> EINVAL at open. ---- */
	dbp = newdb(); mkschema(&s, DB_BTREE);
	CK(db_set_schema(dbp, &s, 0) == 0, "schema set (dup-after case)");
	(void)dbp->set_flags(dbp, DB_DUP);
	ret = dbp->open(dbp, NULL, "dupafter.db", "sub", DB_BTREE,
	    DB_CREATE, 0);
	CK(ret == EINVAL, "DB_DUP after set_schema -> EINVAL at open");
	(void)dbp->close(dbp, 0);

	/* ---- set_bt_compress (compression) -> EINVAL at open. ---- */
	dbp = newdb(); mkschema(&s, DB_BTREE);
	CK(db_set_schema(dbp, &s, 0) == 0, "schema set (compress case)");
	ret = dbp->set_bt_compress(dbp, NULL, NULL);
	if (ret == 0) {
		ret = dbp->open(dbp, NULL, "compress.db", "sub", DB_BTREE,
		    DB_CREATE, 0);
		CK(ret == EINVAL, "compression -> EINVAL at open");
	} else
		CK(0, "set_bt_compress unavailable");
	(void)dbp->close(dbp, 0);

	/* ---- set_bt_compare AFTER db_set_schema -> EINVAL at open. ---- */
	dbp = newdb(); mkschema(&s, DB_BTREE);
	CK(db_set_schema(dbp, &s, 0) == 0, "schema set (bt_compare case)");
	ret = dbp->set_bt_compare(dbp,
	    (int (*)(DB *, const DBT *, const DBT *))field0);
	if (ret == 0) {
		ret = dbp->open(dbp, NULL, "cmpafter.db", "sub", DB_BTREE,
		    DB_CREATE, 0);
		CK(ret == EINVAL,
		    "set_bt_compare after set_schema -> EINVAL at open");
	} else
		CK(0, "set_bt_compare unavailable");
	(void)dbp->close(dbp, 0);

	/* ---- Create a typed subdatabase, then a non-typed one in it, then
	 * open a pre-existing BTM_COMPRESS subdatabase as typed -> EINVAL. ---
	 * Build a compressed subdatabase (untyped) first, then try to open it
	 * with a schema: __typed_open must refuse it because the open never
	 * sets DB2_AM_TYPED_FILE (no catalog yet in commit 4, so a pre-
	 * existing compressed subdatabase that is NOT typed still refuses the
	 * schema at the compression check once DB_IS_COMPRESSED is true). */
	dbp = newdb();
	ret = dbp->set_bt_compress(dbp, NULL, NULL);
	if (ret == 0)
		ret = dbp->open(dbp, NULL, "precompress.db", "sub", DB_BTREE,
		    DB_CREATE, 0);
	(void)dbp->close(dbp, 0);
	dbp = newdb(); mkschema(&s, DB_BTREE);
	if (db_set_schema(dbp, &s, 0) == 0) {
		ret = dbp->open(dbp, NULL, "precompress.db", "sub", DB_BTREE,
		    0, 0);
		CK(ret == EINVAL,
		    "pre-existing BTM_COMPRESS subdb opened typed -> EINVAL");
	} else
		CK(0, "could not set schema for precompress case");
	(void)dbp->close(dbp, 0);

	/* ---- Partitioning keeps the existing ENOENT/BDB0637. ----
	 * A partitioned handle opened with a subdatabase name is refused by
	 * __db_open (db_open.c BDB0637 -> ENOENT), with no typed code. */
	dbp = newdb();
	{
		DBT keys[1];
		memset(keys, 0, sizeof(keys));
		keys[0].data = (void *)"m"; keys[0].size = 1;
		ret = dbp->set_partition(dbp, 2, keys, NULL);
		if (ret == 0) {
			ret = dbp->open(dbp, NULL, "part.db", "sub", DB_BTREE,
			    DB_CREATE, 0);
			CK(ret == ENOENT,
			    "partitioned subdatabase -> ENOENT (existing)");
		} else
			CK(0, "set_partition unavailable");
	}
	(void)dbp->close(dbp, 0);

	(void)dbenv->close(dbenv, 0);

	if (nfail == 0)
		printf("VERDICT typed_refusals PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_refusals FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

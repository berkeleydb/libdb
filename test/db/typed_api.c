/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_api.c --
 *	RFC 0014 commit 3 test: db_set_schema validation (spec §3.4), the
 *	fingerprint self-checks (§5.1), the deep copy of db_get_schema (§1.5),
 *	and the after-open refusal.
 *
 * Every assertion is checked against an EXPECTED return code; a wrong-but-
 * nonzero code is a FAIL, not a pass, so the sabotage arms (which turn one
 * refusal into acceptance) are caught.  One VERDICT line; absence is failure.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail;
static int ncase;

#define	CK(cond, msg) do {						\
	ncase++;							\
	if (!(cond)) {							\
		printf("  FAIL [%d]: %s\n", ncase, msg);		\
		nfail++;						\
	} else								\
		printf("  ok   [%d]: %s\n", ncase, msg);		\
} while (0)

/* A simple MEMCMP codec: no compare, bytes are the order. */
static DB_CODEC memcmp_codec = {
	"memcmp", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0
};

/* Reverse-order int codec with golden vectors in compare order. */
static int
rev_cmp(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	int x, y;
	(void)c;
	x = (al == 4) ? (int)((a[0]<<24)|(a[1]<<16)|(a[2]<<8)|a[3]) : 0;
	y = (bl == 4) ? (int)((b[0]<<24)|(b[1]<<16)|(b[2]<<8)|b[3]) : 0;
	/* reverse: larger value sorts first */
	return (x < y) ? 1 : (x > y) ? -1 : 0;
}
static u_int8_t v10[4] = {0,0,0,10};
static u_int8_t v05[4] = {0,0,0,5};
static u_int8_t v01[4] = {0,0,0,1};
/* In rev order: 10 < 5 < 1, so vectors must be listed 10,5,1. */
static DBT rev_vecs[3];
static DB_CODEC rev_codec;

/* A hash so revint passes rule 5 (hash!=NULL): equal values hash equal. */
static u_int32_t
rev_hash(const DB_CODEC *c, const u_int8_t *a, u_int32_t al)
{
	(void)c;
	return (al == 4) ? (u_int32_t)((a[0]<<24)|(a[1]<<16)|(a[2]<<8)|a[3])
	    : 0;
}

static void
init_rev(void)
{
	memset(rev_vecs, 0, sizeof(rev_vecs));
	rev_vecs[0].data = v10; rev_vecs[0].size = 4;
	rev_vecs[1].data = v05; rev_vecs[1].size = 4;
	rev_vecs[2].data = v01; rev_vecs[2].size = 4;
	memset(&rev_codec, 0, sizeof(rev_codec));
	rev_codec.name = "revint";
	rev_codec.version = 1;
	rev_codec.flags = DB_CODEC_TOTAL;
	rev_codec.compare = rev_cmp;
	rev_codec.hash = rev_hash;
	rev_codec.vectors = rev_vecs;
	rev_codec.nvectors = 3;
}

static DB *
newdb(DB_ENV *dbenv)
{
	DB *dbp;
	if (db_create(&dbp, dbenv, 0) != 0)
		return (NULL);
	return (dbp);
}

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	DB *dbp;
	const DB_CODEC *types1[1];
	DB_RECORD_CODEC rc, rc_data;
	DB_SCHEMA schema;
	const DB_SCHEMA *got;
	int ret;

	(void)argc; (void)argv;
	init_rev();

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, NULL, DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE,
	    0) != 0) {
		printf("VERDICT typed_api FAIL env setup\n");
		return (1);
	}

	/* ---- A valid schema is accepted. ---- */
	types1[0] = &memcmp_codec;
	memset(&rc, 0, sizeof(rc));
	rc.name = "k"; rc.version = 1; rc.flags = DB_CODEC_TOTAL;
	rc.nfields = 1; rc.types = types1;
	/* field() can be any non-NULL here (validation only checks != NULL). */
	rc.field = (int (*)(const DB_RECORD_CODEC *, const DBT *, u_int32_t,
	    const u_int8_t **, u_int32_t *, int *))(void *)newdb;
	memset(&schema, 0, sizeof(schema));
	schema.type = DB_BTREE; schema.key = &rc;
	dbp = newdb(dbenv);
	ret = db_set_schema(dbp, &schema, 0);
	CK(ret == 0, "valid schema accepted");
	(void)dbp->close(dbp, 0);

	/* ---- flags != 0 -> EINVAL (rule 1). ---- */
	dbp = newdb(dbenv);
	CK(db_set_schema(dbp, &schema, 1) == EINVAL, "flags!=0 -> EINVAL");
	(void)dbp->close(dbp, 0);

	/* ---- schema->flags != 0 -> EINVAL (rule 1). ---- */
	dbp = newdb(dbenv);
	schema.flags = 1;
	CK(db_set_schema(dbp, &schema, 0) == EINVAL,
	    "schema.flags!=0 -> EINVAL");
	schema.flags = 0;
	(void)dbp->close(dbp, 0);

	/* ---- type != BTREE/HASH -> EINVAL (rule 2). ---- */
	dbp = newdb(dbenv);
	schema.type = DB_QUEUE;
	CK(db_set_schema(dbp, &schema, 0) == EINVAL, "bad type -> EINVAL");
	schema.type = DB_BTREE;
	(void)dbp->close(dbp, 0);

	/* ---- key and data both NULL -> EINVAL (rule 3). ---- */
	dbp = newdb(dbenv);
	schema.key = NULL; schema.data = NULL;
	CK(db_set_schema(dbp, &schema, 0) == EINVAL,
	    "no typed side -> EINVAL");
	schema.key = &rc;
	(void)dbp->close(dbp, 0);

	/* ---- record codec with a NULL field() -> EINVAL (rule 4). ---- */
	dbp = newdb(dbenv);
	{
		DB_RECORD_CODEC bad = rc;
		bad.field = NULL;
		schema.key = &bad;
		CK(db_set_schema(dbp, &schema, 0) == EINVAL,
		    "NULL field() -> EINVAL");
		schema.key = &rc;
	}
	(void)dbp->close(dbp, 0);

	/* ---- codec with no compare and not MEMCMP -> EINVAL (rule 5). ----
	 * This is sabotage arm (a): deleting the compare==NULL=>MEMCMP rule
	 * makes this accepted. */
	dbp = newdb(dbenv);
	{
		DB_CODEC nocmp = memcmp_codec;
		const DB_CODEC *t[1];
		DB_RECORD_CODEC r2 = rc;
		static DBT asc[3];
		memset(asc, 0, sizeof(asc));
		asc[0].data = v01; asc[0].size = 4;	/* ascending memcmp */
		asc[1].data = v05; asc[1].size = 4;	/* order, so only the */
		asc[2].data = v10; asc[2].size = 4;	/* missing compare is */
		nocmp.flags = DB_CODEC_TOTAL;		/* the fault. */
		nocmp.compare = NULL;
		nocmp.hash = rev_hash;		/* has a hash */
		nocmp.vectors = asc;
		nocmp.nvectors = 3;
		t[0] = &nocmp; r2.types = t;
		schema.key = &r2;
		ret = db_set_schema(dbp, &schema, 0);
		CK(ret == EINVAL, "no-compare non-MEMCMP codec -> EINVAL");
		schema.key = &rc;
	}
	(void)dbp->close(dbp, 0);

	/* ---- unordered vectors -> EINVAL (§5.1 self-check). ----
	 * Sabotage arm (b): deleting the vector-order self-check accepts this.
	 * revint sorts 10<5<1; list them 1,5,10 (ascending) which is NOT in
	 * compare (descending) order. */
	dbp = newdb(dbenv);
	{
		DBT bad_vecs[3];
		DB_CODEC badc = rev_codec;
		const DB_CODEC *t[1];
		DB_RECORD_CODEC r2 = rc;
		memset(bad_vecs, 0, sizeof(bad_vecs));
		bad_vecs[0].data = v01; bad_vecs[0].size = 4;
		bad_vecs[1].data = v05; bad_vecs[1].size = 4;
		bad_vecs[2].data = v10; bad_vecs[2].size = 4;
		badc.vectors = bad_vecs;
		t[0] = &badc; r2.types = t;
		schema.key = &r2;
		ret = db_set_schema(dbp, &schema, 0);
		CK(ret == EINVAL, "unordered vectors -> EINVAL");
		schema.key = &rc;
	}
	(void)dbp->close(dbp, 0);

	/* ---- false MEMCMP claim -> EINVAL (§5.1). ----
	 * revint's compare is descending, memcmp is ascending; claiming MEMCMP
	 * must be caught. */
	dbp = newdb(dbenv);
	{
		DB_CODEC badc = rev_codec;
		const DB_CODEC *t[1];
		DB_RECORD_CODEC r2 = rc;
		badc.flags = DB_CODEC_MEMCMP | DB_CODEC_TOTAL;
		t[0] = &badc; r2.types = t;
		schema.key = &r2;
		ret = db_set_schema(dbp, &schema, 0);
		CK(ret == EINVAL, "false MEMCMP -> EINVAL");
		schema.key = &rc;
	}
	(void)dbp->close(dbp, 0);

	/* ---- false EQUALIMAGE claim -> EINVAL (§5.1). ----
	 * A codec whose compare says two DIFFERENT byte strings are equal, but
	 * which claims EQUALIMAGE (compare==0 iff byte-identical). */
	dbp = newdb(dbenv);
	{
		static u_int8_t a2[2] = {1, 0};
		static u_int8_t b2[2] = {1, 9};	/* different bytes */
		static DBT eqv[2];
		DB_CODEC badc;
		const DB_CODEC *t[1];
		DB_RECORD_CODEC r2 = rc;
		memset(eqv, 0, sizeof(eqv));
		eqv[0].data = a2; eqv[0].size = 2;
		eqv[1].data = b2; eqv[1].size = 2;
		memset(&badc, 0, sizeof(badc));
		badc.name = "eqlie"; badc.version = 1;
		badc.flags = DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL;
		/* compare only looks at byte 0, so a2 and b2 compare equal. */
		badc.compare = NULL;	/* must set below */
		badc.vectors = eqv; badc.nvectors = 2;
		{
			extern int typed_api_eqlie_cmp(const DB_CODEC *,
			    const u_int8_t *, u_int32_t,
			    const u_int8_t *, u_int32_t);
			badc.compare = (int (*)(const DB_CODEC *,
			    const u_int8_t *, u_int32_t,
			    const u_int8_t *, u_int32_t))typed_api_eqlie_cmp;
		}
		t[0] = &badc; r2.types = t;
		schema.key = &r2;
		ret = db_set_schema(dbp, &schema, 0);
		CK(ret == EINVAL, "false EQUALIMAGE -> EINVAL");
		schema.key = &rc;
	}
	(void)dbp->close(dbp, 0);

	/* ---- deep copy: scribble caller arrays after the call. ----
	 * Sabotage arm (c): a shallow copy that keeps the caller's types[]
	 * pointer sees the scribble. */
	dbp = newdb(dbenv);
	{
		const DB_CODEC *t[1];
		DB_RECORD_CODEC r2;
		memset(&r2, 0, sizeof(r2));
		r2.name = "deepk"; r2.version = 1; r2.flags = DB_CODEC_TOTAL;
		r2.nfields = 1; r2.field = rc.field;
		t[0] = &memcmp_codec; r2.types = t;
		schema.key = &r2;
		ret = db_set_schema(dbp, &schema, 0);
		CK(ret == 0, "deep-copy schema accepted");
		/* Scribble the caller's record codec and types array. */
		memset(&r2, 0xAB, sizeof(r2));
		t[0] = NULL;
		got = NULL;
		ret = db_get_schema(dbp, &got);
		CK(ret == 0 && got != NULL, "db_get_schema returns copy");
		if (got != NULL && got->key != NULL) {
			CK(got->key->name != NULL &&
			    strcmp(got->key->name, "deepk") == 0,
			    "deep copy: record codec name survives scribble");
			CK(got->key->nfields == 1 &&
			    got->key->types != NULL &&
			    got->key->types[0] == &memcmp_codec,
			    "deep copy: types[] survives scribble");
		} else {
			CK(0, "deep copy: schema missing");
			CK(0, "deep copy: schema missing");
		}
		schema.key = &rc;
	}
	(void)dbp->close(dbp, 0);

	/* ---- db_get_schema with no schema -> EINVAL. ---- */
	dbp = newdb(dbenv);
	got = NULL;
	CK(db_get_schema(dbp, &got) == EINVAL, "get with no schema -> EINVAL");
	(void)dbp->close(dbp, 0);

	/* ---- db_set_schema after open -> EINVAL. ---- */
	dbp = newdb(dbenv);
	schema.key = &rc;
	ret = dbp->open(dbp, NULL, NULL, "sub", DB_BTREE, DB_CREATE, 0);
	if (ret == 0) {
		CK(db_set_schema(dbp, &schema, 0) == EINVAL,
		    "set_schema after open -> EINVAL");
	} else {
		/* open of an in-memory db with no file -- fall back. */
		CK(0, "could not open to test after-open refusal");
	}
	(void)dbp->close(dbp, 0);

	(void)dbenv->close(dbenv, 0);

	if (nfail == 0)
		printf("VERDICT typed_api PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_api FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

/* A compare that only inspects byte 0 -- makes two different strings equal. */
int
typed_api_eqlie_cmp(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	(void)c; (void)al; (void)bl;
	return (a[0] < b[0]) ? -1 : (a[0] > b[0]) ? 1 : 0;
}

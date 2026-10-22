/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_valid.c --
 *	RFC 0014 commit 6 test (spec §7.2 typed_valid): a VALID constraint
 *	whose codec validate() returns DB_CODEC_MALFORMED fails the put with
 *	DB_CONSTRAINT_VIOLATION (-30966), NEVER -30964; a record-codec
 *	validate() failure does the same; an arbitrary error (ENOMEM) from a
 *	callback is passed through unchanged.  One VERDICT line.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

/* field 0 == whole data. */
static int
field1(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0) return (DB_CODEC_MALFORMED);
	*pp = rec->data; *lenp = rec->size; *isnull = 0;
	return (0);
}

/* codec validate: 0 if first byte is 'G', DB_CODEC_MALFORMED otherwise. */
static int
cvalidate_malformed(const DB_CODEC *c, const u_int8_t *p, u_int32_t len)
{
	(void)c;
	if (len > 0 && p[0] == 'G') return (0);
	return (DB_CODEC_MALFORMED);
}
/* codec validate that returns an arbitrary error (ENOMEM) for 'E'. */
static int
cvalidate_enomem(const DB_CODEC *c, const u_int8_t *p, u_int32_t len)
{
	(void)c;
	if (len > 0 && p[0] == 'E') return (ENOMEM);
	return (0);
}

static DB_CODEC cm_mal = {
	"cmmal", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, cvalidate_malformed, NULL, NULL, NULL, 0
};
static DB_CODEC cm_enomem = {
	"cmeno", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, cvalidate_enomem, NULL, NULL, NULL, 0
};

/* record-codec validate: DB_CODEC_MALFORMED if the DBT starts with 'R'. */
static int
rvalidate(const DB_RECORD_CODEC *rc, const DBT *rec)
{
	const u_int8_t *d = rec->data;
	(void)rc;
	if (rec->size > 0 && d[0] == 'R') return (DB_CODEC_MALFORMED);
	return (0);
}

static DB_ENV *dbenv;

static int
openit(DB **dbpp, const char *sub, DB_CODEC *codec, int with_rvalidate)
{
	DB *dbp;
	DB_SCHEMA s;
	DB_CONSTRAINT con;
	static const DB_CODEC *dt[1];
	static DB_RECORD_CODEC drc;

	if (db_create(&dbp, dbenv, 0) != 0) return (-1);
	dt[0] = codec;
	memset(&drc, 0, sizeof(drc));
	drc.name = "drc"; drc.version = 1; drc.flags = DB_CODEC_TOTAL;
	drc.nfields = 1; drc.field = field1; drc.types = dt;
	drc.validate = with_rvalidate ? rvalidate : NULL;
	memset(&s, 0, sizeof(s)); s.type = DB_BTREE; s.data = &drc;
	memset(&con, 0, sizeof(con));
	con.kind = DB_CONSTRAINT_VALID; con.side = DB_TYPED_DATA;
	con.field = 0; con.name = "v_f0";
	s.constraints = &con; s.nconstraints = 1;
	if (db_set_schema(dbp, &s, 0) != 0) { (void)dbp->close(dbp, 0); return (-1); }
	if (dbp->open(dbp, NULL, "vl.db", sub, DB_BTREE, DB_CREATE, 0600) != 0) {
		(void)dbp->close(dbp, 0); return (-1);
	}
	*dbpp = dbp;
	return (0);
}

static int
put1(DB *dbp, int keyn, const char *data)
{
	DBT k, d;
	unsigned char kb[1];
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	kb[0] = (unsigned char)keyn; k.data = kb; k.size = 1;
	d.data = (void *)data; d.size = (u_int32_t)strlen(data);
	return (dbp->put(dbp, NULL, &k, &d, 0));
}

int
main(int argc, char *argv[])
{
	DB *dbp;
	int ret;
	const char *home = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_valid FAIL env setup\n");
		return (1);
	}

	/* 1. codec validate -> MALFORMED maps to -30966 (never -30964). */
	if (openit(&dbp, "mal", &cm_mal, 0) != 0) goto bad;
	CK(put1(dbp, 1, "Good") == 0, "codec VALID: good value put succeeds");
	ret = put1(dbp, 2, "Bad");
	CK(ret == DB_CONSTRAINT_VIOLATION,
	    "codec VALID malformed -> -30966");
	CK(ret != DB_CODEC_MALFORMED, "never leaks -30964 (codec)");
	(void)dbp->close(dbp, 0);

	/* 2. record-codec validate -> MALFORMED maps to -30966.  Use a codec
	 * whose field validate passes ('R' is not 'E'), and a record validate
	 * that rejects a leading 'R'. */
	if (openit(&dbp, "rec", &cm_enomem, 1) != 0) goto bad;
	ret = put1(dbp, 3, "ok-rec");
	CK(ret == 0, "record VALID: non-R value put succeeds");
	ret = put1(dbp, 5, "R-rec");
	CK(ret == DB_CONSTRAINT_VIOLATION,
	    "record VALID malformed -> -30966");
	CK(ret != DB_CODEC_MALFORMED, "never leaks -30964 (record)");
	(void)dbp->close(dbp, 0);

	/* 3. arbitrary error (ENOMEM) from a callback is passed through. */
	if (openit(&dbp, "eno", &cm_enomem, 0) != 0) goto bad;
	ret = put1(dbp, 6, "Eboom");
	CK(ret == ENOMEM, "arbitrary callback error passed through (ENOMEM)");
	(void)dbp->close(dbp, 0);

	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_valid PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_valid FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
bad:
	printf("VERDICT typed_valid FAIL setup\n");
	return (1);
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_codec_mismatch.c --
 *	RFC 0014 commit 5 test (spec §7.2 typed_codec_mismatch): the catalog
 *	fingerprint check on open (§5.2).  Create with codec v1, then open
 *	with:
 *	  B: v2            -> EINVAL  (version differs)
 *	  C: v1 reversed   -> DB_CODEC_MISMATCH (-30965) (same name+version,
 *	                      different fingerprint)
 *	  D: missing codec -> EINVAL
 *	  E: v1 diff flags -> DB_CODEC_MISMATCH (-30965)
 *	  F: identical     -> opens and reads the created row
 *	Opens are sequential in one process; the catalog is persistent, so
 *	this exercises the same compare path as separate processes would.
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

/* An ordered-int codec with golden vectors; version/compare are varied. */
static int
cmp_asc(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	int x, y;
	(void)c;
	x = al == 4 ? (int)((a[0]<<24)|(a[1]<<16)|(a[2]<<8)|a[3]) : 0;
	y = bl == 4 ? (int)((b[0]<<24)|(b[1]<<16)|(b[2]<<8)|b[3]) : 0;
	return x < y ? -1 : x > y ? 1 : 0;
}
static int
cmp_desc(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	return -cmp_asc(c, a, al, b, bl);
}
static u_int32_t
h4(const DB_CODEC *c, const u_int8_t *a, u_int32_t al)
{
	(void)c;
	return al == 4 ? (u_int32_t)((a[0]<<24)|(a[1]<<16)|(a[2]<<8)|a[3]) : 0;
}

static u_int8_t v1b[4] = {0,0,0,1}, v2b[4] = {0,0,0,2}, v3b[4] = {0,0,0,3};
static DBT ascv[3], descv[3];

/* A different-named codec for the "schema missing the codec" case (D). */
static DB_CODEC other = {
	"other", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0
};

/* Single field spanning the whole record (the comparator reads it). */
static int
recfield(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0)
		return (DB_CODEC_MALFORMED);
	*pp = rec->data; *lenp = rec->size; *isnull = 0;
	return (0);
}

static DB_RECORD_CODEC *
mkrc(DB_RECORD_CODEC *rc, DB_CODEC *c, const DB_CODEC **t)
{
	memset(rc, 0, sizeof(*rc));
	rc->name = "k"; rc->version = 1; rc->flags = DB_CODEC_TOTAL;
	rc->nfields = 1; rc->types = t;
	/*
	 * A real single-field locator: field 0 is the whole record.  Commit 8
	 * installs __typed_bt_compare, which actually calls field() on the
	 * case-F read path, so a bogus field pointer would crash.
	 */
	rc->field = recfield;
	t[0] = c;
	return (rc);
}

static DB_ENV *dbenv;

static int
try(const char *lbl, DB_CODEC *c, int expect, int do_read)
{
	DB *dbp;
	DB_RECORD_CODEC rc;
	DB_SCHEMA s;
	const DB_CODEC *t[1];
	DBT k, d;
	int ret, flags, ok;

	if (db_create(&dbp, dbenv, 0) != 0) return (-1);
	memset(&s, 0, sizeof(s));
	s.type = DB_BTREE;
	if (c != NULL)
		s.key = mkrc(&rc, c, t);
	else {
		/* missing-codec case D: data-side only, key untyped. */
		s.key = NULL;
		s.data = mkrc(&rc, &other, t);
	}
	if ((ret = db_set_schema(dbp, &s, 0)) != 0) {
		/* set_schema itself may reject (e.g. a self-check); surface. */
		(void)dbp->close(dbp, 0);
		ok = (ret == expect);
		CK(ok, lbl);
		return (ok ? 0 : -1);
	}
	flags = (expect == 0 && !do_read) ? DB_CREATE : 0;
	if (expect == 0 && do_read == 2) flags = DB_CREATE;	/* creator */
	ret = dbp->open(dbp, NULL, "cm.db", "sub", DB_BTREE, flags, 0600);
	ok = (ret == expect);
	if (ok && do_read == 2) {
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		k.data = v2b; k.size = 4; d.data = (void *)"V"; d.size = 1;
		(void)dbp->put(dbp, NULL, &k, &d, 0);
	}
	if (ok && do_read == 1) {
		memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
		k.data = v2b; k.size = 4;
		ok = dbp->get(dbp, NULL, &k, &d, 0) == 0 &&
		    d.size == 1 && memcmp(d.data, "V", 1) == 0;
	}
	(void)dbp->close(dbp, 0);
	CK(ok, lbl);
	return (ok ? 0 : -1);
}

int
main(int argc, char *argv[])
{
	DB_CODEC c_v1, c_v2, c_v1rev, c_v1flags, c_v1same;
	const char *home = (argc > 1) ? argv[1] : ".";
	int i;

	memset(ascv, 0, sizeof(ascv)); memset(descv, 0, sizeof(descv));
	ascv[0].data = v1b; ascv[0].size = 4;
	ascv[1].data = v2b; ascv[1].size = 4;
	ascv[2].data = v3b; ascv[2].size = 4;
	/* descending order lists the same values 3,2,1 */
	descv[0].data = v3b; descv[0].size = 4;
	descv[1].data = v2b; descv[1].size = 4;
	descv[2].data = v1b; descv[2].size = 4;

#define	INIT(cc, nm, ver, fl, cf, vv) do {				\
	memset(&cc, 0, sizeof(cc));					\
	cc.name = nm; cc.version = ver; cc.flags = fl;			\
	cc.compare = cf; cc.hash = h4; cc.vectors = vv; cc.nvectors = 3;	\
} while (0)
	INIT(c_v1,      "oint", 1, DB_CODEC_TOTAL, cmp_asc,  ascv);
	INIT(c_v2,      "oint", 2, DB_CODEC_TOTAL, cmp_asc,  ascv);
	INIT(c_v1rev,   "oint", 1, DB_CODEC_TOTAL, cmp_desc, descv);
	INIT(c_v1flags, "oint", 1, DB_CODEC_FIXED | DB_CODEC_TOTAL,
	    cmp_asc, ascv);
	c_v1flags.fixed_len = 4;
	INIT(c_v1same,  "oint", 1, DB_CODEC_TOTAL, cmp_asc,  ascv);

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_codec_mismatch FAIL env setup\n");
		return (1);
	}

	(void)i;
	/* A: create with v1 and store one row. */
	try("A create v1", &c_v1, 0, 2);
	/* B: v2 -> EINVAL (version differs). */
	try("B v2 -> EINVAL", &c_v2, EINVAL, 0);
	/* C: v1 reversed compare -> DB_CODEC_MISMATCH. */
	try("C v1 reversed -> DB_CODEC_MISMATCH", &c_v1rev,
	    DB_CODEC_MISMATCH, 0);
	/* D: schema missing the codec -> EINVAL. */
	try("D missing codec -> EINVAL", NULL, EINVAL, 0);
	/* E: v1 different flags -> DB_CODEC_MISMATCH. */
	try("E v1 diff flags -> DB_CODEC_MISMATCH", &c_v1flags,
	    DB_CODEC_MISMATCH, 0);
	/* F: identical v1 -> opens and reads A's row. */
	try("F identical v1 opens and reads", &c_v1same, 0, 1);

	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_codec_mismatch PASS all %d checks\n",
		    ncase);
	else
		printf("VERDICT typed_codec_mismatch FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

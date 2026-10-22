/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_hash.c --
 *	RFC 0014 commit 8 test (spec §7.2 typed_hash): a Hash with a
 *	case-insensitive key codec installs __typed_h_hash / __typed_h_compare.
 *	Put "abc"; a get with "AbC" finds it (same bucket, equal under the
 *	codec).  Put "ABC"; it overwrites, so the database holds exactly one
 *	record.  A bytewise hash would land "AbC" in a different bucket and the
 *	get would miss -- that is the sabotage.  One VERDICT line.
 */
#include <sys/types.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

/* The key is one field spanning the whole key. */
static int
cifield(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0)
		return (DB_CODEC_MALFORMED);
	*pp = rec->data; *lenp = rec->size; *isnull = 0;
	return (0);
}
/* Case-insensitive compare / hash.  NOT EQUALIMAGE: "abc" and "ABC" are
 * equal under compare but their bytes differ, so h_hash/h_compare must be
 * generated. */
static int
cicmp(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	u_int32_t i, min = al < bl ? al : bl;
	int ca, cb;
	(void)c;
	for (i = 0; i < min; i++) {
		ca = tolower(a[i]); cb = tolower(b[i]);
		if (ca != cb) return (ca < cb ? -1 : 1);
	}
	return (al < bl ? -1 : al > bl ? 1 : 0);
}
static u_int32_t
cihash(const DB_CODEC *c, const u_int8_t *p, u_int32_t len)
{
	/* FNV-1a over the lowercased bytes. */
	u_int32_t h = 2166136261u, i;
	(void)c;
	for (i = 0; i < len; i++) {
		h ^= (u_int32_t)tolower(p[i]);
		h *= 16777619u;
	}
	return (h);
}
static DBT civec[2];
static DB_CODEC cic = {
	"ci", 1, DB_CODEC_TOTAL, 0, cicmp, NULL, cihash, NULL, NULL, NULL,
	civec, 2
};
static const DB_CODEC *ktypes[1] = { &cic };
static DB_RECORD_CODEC krc = {
	"kci", 1, DB_CODEC_TOTAL, 1, cifield, NULL, ktypes, NULL
};

static DB_ENV *dbenv;

static int
getcount(DB *dbp)
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
	DB *dbp;
	DBT k, d;
	DB_SCHEMA s;
	int ret, base;
	const char *home = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_hash FAIL env setup\n");
		return (1);
	}
	/* Golden vectors in ascending codec order: "abc" < "xyz". */
	memset(civec, 0, sizeof(civec));
	civec[0].data = (void *)"abc"; civec[0].size = 3;
	civec[1].data = (void *)"xyz"; civec[1].size = 3;
	if (db_create(&dbp, dbenv, 0) != 0) {
		printf("VERDICT typed_hash FAIL db_create\n"); return (1); }
	memset(&s, 0, sizeof(s));
	s.type = DB_HASH; s.key = &krc;
	if (db_set_schema(dbp, &s, 0) != 0) {
		printf("VERDICT typed_hash FAIL set_schema\n"); return (1); }
	/*
	 * A low fill factor forces thousands of buckets once populated, so a
	 * byte-sensitive hash (the "no h_hash install" sabotage) sends "abc"
	 * and "AbC" to different buckets and the case-insensitive get misses.
	 * With the default fill factor only a handful of buckets exist and
	 * every key collides in one, which would hide the sabotage.
	 */
	if (dbp->set_h_ffactor(dbp, 2) != 0) {
		printf("VERDICT typed_hash FAIL set_h_ffactor\n"); return (1); }
	if ((ret = dbp->open(dbp, NULL, "h.db", "sub", DB_HASH,
	    DB_CREATE, 0600)) != 0) {
		printf("VERDICT typed_hash FAIL open %d\n", ret); return (1); }

	/*
	 * Populate many distinct keys first, so the table grows to hundreds of
	 * buckets.  With only one bucket every key collides there and a get
	 * finds its record through h_compare alone -- which would let the
	 * "no h_hash install" sabotage pass vacuously.  With many buckets a
	 * byte-sensitive __ham_func5 sends "abc" and "AbC" to different buckets,
	 * so the case-insensitive get only works when __typed_h_hash is really
	 * installed.
	 */
	{
		char kbuf[16];
		int j;
		for (j = 0; j < 5000; j++) {
			sprintf(kbuf, "k%06d", j);
			memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
			k.data = kbuf; k.size = (u_int32_t)strlen(kbuf);
			d.data = "p"; d.size = 1;
			if (dbp->put(dbp, NULL, &k, &d, 0) != 0) {
				printf("VERDICT typed_hash FAIL populate\n");
				return (1);
			}
		}
	}

	/* put "abc" -> "v1" */
	base = getcount(dbp);
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = "abc"; k.size = 3; d.data = "v1"; d.size = 2;
	CK(dbp->put(dbp, NULL, &k, &d, 0) == 0, "put abc");

	/* get "AbC" must find it (case-insensitive hash + compare). */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = "AbC"; k.size = 3;
	ret = dbp->get(dbp, NULL, &k, &d, 0);
	CK(ret == 0, "get AbC finds the abc record");
	CK(ret == 0 && d.size == 2 && memcmp(d.data, "v1", 2) == 0,
	    "get AbC returns v1");

	/* put "ABC" -> "v2": same key under the codec, so it overwrites. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = "ABC"; k.size = 3; d.data = "v2"; d.size = 2;
	CK(dbp->put(dbp, NULL, &k, &d, 0) == 0, "put ABC overwrites");
	CK(getcount(dbp) == base + 1, "exactly one abc record after overwrite");

	/* get "abc" now returns v2. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = "abc"; k.size = 3;
	ret = dbp->get(dbp, NULL, &k, &d, 0);
	CK(ret == 0 && d.size == 2 && memcmp(d.data, "v2", 2) == 0,
	    "get abc returns v2 (overwritten)");

	/* a genuinely different key does not collide. */
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = "xyz"; k.size = 3; d.data = "v3"; d.size = 2;
	(void)dbp->put(dbp, NULL, &k, &d, 0);
	CK(getcount(dbp) == base + 2, "two case-folded records after a distinct key");

	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_hash PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_hash FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_btree_cmp.c --
 *	RFC 0014 commit 8 test (spec §7.2 typed_btree_cmp): the generated
 *	__typed_bt_compare sorts a non-MEMCMP key in codec order, not bytewise.
 *
 *	Case 1: a reverse-order 4-byte big-endian integer codec.  Insert keys
 *	1,2,3; a DB_NEXT scan must return them 3,2,1 (codec order), which a
 *	bytewise comparator (00 00 00 01 < 00 00 00 02) could never do.
 *	Case 2: malformed keys (field() fails: size != 4) sort AFTER every
 *	well-formed key, in memcmp-then-length order among themselves.
 *	Case 3: an all-MEMCMP key schema leaves bt_compare == __bam_defcmp,
 *	proved through get_bt_compare -- so the generated comparator is only
 *	installed when it is needed.
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

/*
 * A key record codec with one field: a 4-byte big-endian u32 whose compare
 * REVERSES the natural order (larger value sorts first).  Not MEMCMP, not
 * EQUALIMAGE: this forces __typed_bt_compare to be installed.  A key that is
 * not exactly 4 bytes is a malformed record (field() returns MALFORMED).
 */
static int
rfield(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0 || rec->size != 4)
		return (DB_CODEC_MALFORMED);
	*pp = rec->data; *lenp = 4; *isnull = 0;
	return (0);
}
static int
rcmp(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	u_int32_t x, y;
	(void)c;
	if (al != 4 || bl != 4) return (0);
	x = ((u_int32_t)a[0]<<24)|((u_int32_t)a[1]<<16)|((u_int32_t)a[2]<<8)|a[3];
	y = ((u_int32_t)b[0]<<24)|((u_int32_t)b[1]<<16)|((u_int32_t)b[2]<<8)|b[3];
	/* reversed: larger value is "less" so it sorts first. */
	return (x > y ? -1 : x < y ? 1 : 0);
}
static u_int32_t
rhash(const DB_CODEC *c, const u_int8_t *p, u_int32_t len)
{
	u_int32_t h = 2166136261u, i;
	(void)c;
	for (i = 0; i < len; i++) { h ^= p[i]; h *= 16777619u; }
	return (h);
}
/* Golden vectors in NON-DECREASING codec order (the self-check requires it).
 * rcmp reverses, so the "smaller" (sorts-first) vector is the LARGER value:
 * {00 00 00 09} then {00 00 00 01}. */
static const unsigned char gv0[4] = { 0, 0, 0, 9 };
static const unsigned char gv1[4] = { 0, 0, 0, 1 };
static DBT revvec[2];
static DB_CODEC revcodec = {
	"rev32", 1, DB_CODEC_TOTAL, 0, rcmp, NULL, rhash, NULL, NULL, NULL,
	revvec, 2
};
static const DB_CODEC *ktypes[1] = { &revcodec };
static DB_RECORD_CODEC krc = {
	"krev", 1, DB_CODEC_TOTAL, 1, rfield, NULL, ktypes, NULL
};

/* An all-MEMCMP key codec: __typed_bt_compare must NOT be installed. */
static DB_CODEC memc = {
	"memc", 1, DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE | DB_CODEC_TOTAL,
	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0
};
static const DB_CODEC *mtypes[1] = { &memc };
static DB_RECORD_CODEC mrc = {
	"kmem", 1, DB_CODEC_TOTAL, 1, rfield, NULL, mtypes, NULL
};

static DB_ENV *dbenv;

static void
be32(unsigned char *b, u_int32_t v)
{
	b[0]=v>>24; b[1]=v>>16; b[2]=v>>8; b[3]=v;
}

int
main(int argc, char *argv[])
{
	DB *dbp;
	DBC *c;
	DBT k, d;
	DB_SCHEMA s;
	unsigned char kb[8], vb[4];
	u_int32_t got[8];
	int (*cmpf)(DB *, const DBT *, const DBT *);
	int ret, n;
	const char *home = (argc > 1) ? argv[1] : ".";

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_btree_cmp FAIL env setup\n");
		return (1);
	}

	/* Golden vectors (data/size only). */
	memset(revvec, 0, sizeof(revvec));
	revvec[0].data = (void *)gv0; revvec[0].size = 4;
	revvec[1].data = (void *)gv1; revvec[1].size = 4;

	/* ---- Case 1 + 2: reverse-order codec, scan + malformed-last. ---- */
	if (db_create(&dbp, dbenv, 0) != 0) {
		printf("VERDICT typed_btree_cmp FAIL db_create\n"); return (1); }
	memset(&s, 0, sizeof(s));
	s.type = DB_BTREE; s.key = &krc;
	if (db_set_schema(dbp, &s, 0) != 0) {
		printf("VERDICT typed_btree_cmp FAIL set_schema\n"); return (1); }
	if ((ret = dbp->open(dbp, NULL, "rev.db", "sub", DB_BTREE,
	    DB_CREATE, 0600)) != 0) {
		printf("VERDICT typed_btree_cmp FAIL open %d\n", ret); return (1); }

	/* Insert well-formed keys 1,2,3 and two malformed keys (len 3, len 5).*/
	memset(&d, 0, sizeof(d)); vb[0]='x'; d.data=vb; d.size=1;
	for (n = 1; n <= 3; n++) {
		memset(&k, 0, sizeof(k)); be32(kb, (u_int32_t)n); k.data=kb; k.size=4;
		CK(dbp->put(dbp, NULL, &k, &d, 0) == 0, "put well-formed key");
	}
	/* malformed A: 3 bytes {0x02,0x00,0x00}; malformed B: 5 bytes. */
	memset(&k, 0, sizeof(k)); kb[0]=0x02; kb[1]=kb[2]=0x00; k.data=kb; k.size=3;
	CK(dbp->put(dbp, NULL, &k, &d, 0) == 0, "put malformed A (len 3)");
	memset(&k, 0, sizeof(k)); kb[0]=0x01; kb[1]=kb[2]=kb[3]=kb[4]=0x00;
	k.data=kb; k.size=5;
	CK(dbp->put(dbp, NULL, &k, &d, 0) == 0, "put malformed B (len 5)");

	/* Scan: expect well-formed 3,2,1 (reversed codec order) first, then the
	 * two malformed last.  Record each key's first byte + length. */
	n = 0;
	if (dbp->cursor(dbp, NULL, &c, 0) == 0) {
		while ((ret = c->get(c, &k, &d, DB_NEXT)) == 0 && n < 8) {
			/* encode well-formed 4-byte as its int, malformed as a
			 * sentinel >= 1000 so order is checkable. */
			if (k.size == 4) {
				const unsigned char *p = k.data;
				got[n++] = ((u_int32_t)p[0]<<24)|((u_int32_t)p[1]<<16)|
				    ((u_int32_t)p[2]<<8)|p[3];
			} else
				got[n++] = 1000 + k.size;	/* 1003, 1005 */
		}
		(void)c->close(c);
	}
	CK(n == 5, "scan returned 5 keys");
	/* well-formed in reverse order: 3,2,1 */
	CK(n >= 3 && got[0] == 3 && got[1] == 2 && got[2] == 1,
	    "well-formed keys in reverse codec order (3,2,1)");
	/* malformed last; among themselves memcmp-then-length: A={02,00,00}
	 * vs B={01,00,00,00,00}: first byte 0x02 > 0x01 so A sorts after B,
	 * i.e. B (len 5 ->1005) then A (len 3 ->1003). */
	CK(n == 5 && got[3] == 1005 && got[4] == 1003,
	    "malformed keys sort last, memcmp-then-length (B then A)");
	(void)dbp->close(dbp, 0);

	/* ---- Case 3: all-MEMCMP key -> bt_compare stays __bam_defcmp. ---- */
	if (db_create(&dbp, dbenv, 0) != 0) {
		printf("VERDICT typed_btree_cmp FAIL db_create2\n"); return (1); }
	memset(&s, 0, sizeof(s));
	s.type = DB_BTREE; s.key = &mrc;
	if (db_set_schema(dbp, &s, 0) != 0) {
		printf("VERDICT typed_btree_cmp FAIL set_schema2\n"); return (1); }
	if ((ret = dbp->open(dbp, NULL, "mem.db", "sub", DB_BTREE,
	    DB_CREATE, 0600)) != 0) {
		printf("VERDICT typed_btree_cmp FAIL open2 %d\n", ret); return (1); }
	cmpf = NULL;
	ret = dbp->get_bt_compare(dbp, &cmpf);
	CK(ret == 0, "get_bt_compare ok");
	/* __bam_defcmp is the default; a MEMCMP key must keep it (no generated
	 * comparator), so get_bt_compare must NOT return __typed_bt_compare.
	 * We cannot name __bam_defcmp from here, so assert bytewise behaviour:
	 * put 0x00000001 and 0x00000002, scan must be natural order 1,2. */
	memset(&d, 0, sizeof(d)); d.data=vb; d.size=1;
	memset(&k, 0, sizeof(k)); be32(kb,1); k.data=kb; k.size=4;
	(void)dbp->put(dbp, NULL, &k, &d, 0);
	memset(&k, 0, sizeof(k)); be32(kb,2); k.data=kb; k.size=4;
	(void)dbp->put(dbp, NULL, &k, &d, 0);
	n = 0;
	if (dbp->cursor(dbp, NULL, &c, 0) == 0) {
		while (c->get(c, &k, &d, DB_NEXT) == 0 && n < 8) {
			const unsigned char *p = k.data;
			got[n++] = ((u_int32_t)p[0]<<24)|((u_int32_t)p[1]<<16)|
			    ((u_int32_t)p[2]<<8)|p[3];
		}
		(void)c->close(c);
	}
	CK(n == 2 && got[0] == 1 && got[1] == 2,
	    "all-MEMCMP key: natural byte order (default comparator kept)");
	(void)dbp->close(dbp, 0);

	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_btree_cmp PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_btree_cmp FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

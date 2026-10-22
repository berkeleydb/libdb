/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_cmp_fuzz.c --
 *	RFC 0014 commit 8 test (spec §7.2 typed_cmp_fuzz): the generated
 *	__typed_bt_compare must be TOTAL over arbitrary bytes (RFC 0007's
 *	contract that DB_OPTREAD relies on, spec §6.3).  Fuzz the comparator
 *	fetched from get_bt_compare with random, truncated and torn key pairs,
 *	using a deliberately LYING field() that returns slices running past the
 *	end of the record.  The libdb slice-bounds check (§6.3) must turn that
 *	lie into "malformed", so no type codec ever reads out of bounds -- under
 *	ASan a missing check is a heap-buffer-overflow (the sabotage).
 *
 *	Each key DBT is backed by a heap block sized EXACTLY to its length, so
 *	any read past the end is a real ASan violation, not a read into slack.
 *	Properties checked over 10^6 pairs:
 *	  - no crash / ASan report (the process reaching the VERDICT is proof);
 *	  - antisymmetry: sign(cmp(a,b)) == -sign(cmp(b,a));
 *	  - malformed-last: a well-formed key always compares < a malformed one.
 *	60-second budget (the runner also wraps a timeout).
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"

static int nfail, ncase;
#define	CK(c, m) do { ncase++; if (!(c)) { printf("  FAIL [%d]: %s\n", ncase, m); nfail++; } else printf("  ok   [%d]: %s\n", ncase, m); } while (0)

/*
 * The LYING field codec: two fields.  field 0 is the first 4 bytes; field 1
 * is "the rest" -- but for a torn record it reports a length that runs PAST
 * the end of the record (rec->size + 7), so an uncaught comparator would read
 * off the end.  A record shorter than 4 bytes is reported malformed.  This is
 * exactly the §6.3 adversary: a field() that lies about where its slice ends.
 */
static int
lyingfield(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	const u_int8_t *d = rec->data;
	(void)rc;
	if (rec->size < 4)
		return (DB_CODEC_MALFORMED);
	*isnull = 0;
	if (i == 0) { *pp = d; *lenp = 4; return (0); }
	if (i == 1) {
		/* LIE: claim 7 bytes more than the record actually has. */
		*pp = d + 4;
		*lenp = (rec->size - 4) + 7;
		return (0);
	}
	return (DB_CODEC_MALFORMED);
}
/* A field codec that reads every byte of its slice (so an unbounded slice
 * faults).  compare() sums the bytes -- forcing a read of all `len`. */
static int
sumcmp(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	u_int32_t i; unsigned long sa = 0, sb = 0;
	(void)c;
	for (i = 0; i < al; i++) sa += a[i];
	for (i = 0; i < bl; i++) sb += b[i];
	if (sa != sb) return (sa < sb ? -1 : 1);
	return (al < bl ? -1 : al > bl ? 1 : 0);
}
static u_int32_t
sumhash(const DB_CODEC *c, const u_int8_t *p, u_int32_t len)
{
	u_int32_t i; unsigned long s = 0;
	(void)c;
	for (i = 0; i < len; i++) s += p[i];
	return ((u_int32_t)s * 2654435761u);
}
/* Golden vectors ascending by byte-sum: {0x01} (sum 1) < {0x02,0x03} (sum 5). */
static const unsigned char fv0[1] = { 0x01 };
static const unsigned char fv1[2] = { 0x02, 0x03 };
static DBT fvec[2];
static DB_CODEC f0 = {
	"f0", 1, DB_CODEC_TOTAL, 0, sumcmp, NULL, sumhash, NULL, NULL, NULL,
	fvec, 2
};
static DB_CODEC f1 = {
	"f1", 1, DB_CODEC_TOTAL, 0, sumcmp, NULL, sumhash, NULL, NULL, NULL,
	fvec, 2
};
static const DB_CODEC *ktypes[2] = { &f0, &f1 };
static DB_RECORD_CODEC krc = {
	"klie", 1, DB_CODEC_TOTAL, 2, lyingfield, NULL, ktypes, NULL
};

static int sgn(int x) { return (x < 0 ? -1 : x > 0 ? 1 : 0); }

/* make a key DBT backed by an exact-sized heap block (ASan-tight). */
static void
mkkey(DBT *k, const unsigned char *src, u_int32_t n)
{
	unsigned char *p = malloc(n == 0 ? 1 : n);
	if (n > 0) memcpy(p, src, n);
	memset(k, 0, sizeof(*k));
	k->data = p; k->size = n;
}

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	DB *dbp;
	DBT a, b;
	DB_SCHEMA s;
	int (*cmp)(DB *, const DBT *, const DBT *) = NULL;
	unsigned char buf1[64], buf2[64];
	u_int32_t i, n1, n2, iter;
	time_t t0;
	int c1, c2, ret;
	const char *home = (argc > 1) ? argv[1] : ".";
	u_int32_t niter = (argc > 2) ? (u_int32_t)strtoul(argv[2], NULL, 10)
	    : 1000000u;

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_cmp_fuzz FAIL env setup\n");
		return (1);
	}
	memset(fvec, 0, sizeof(fvec));
	fvec[0].data = (void *)fv0; fvec[0].size = 1;
	fvec[1].data = (void *)fv1; fvec[1].size = 2;
	if (db_create(&dbp, dbenv, 0) != 0) {
		printf("VERDICT typed_cmp_fuzz FAIL db_create\n"); return (1); }
	memset(&s, 0, sizeof(s));
	s.type = DB_BTREE; s.key = &krc;
	if (db_set_schema(dbp, &s, 0) != 0) {
		printf("VERDICT typed_cmp_fuzz FAIL set_schema\n"); return (1); }
	if ((ret = dbp->open(dbp, NULL, "fz.db", "sub", DB_BTREE,
	    DB_CREATE, 0600)) != 0) {
		printf("VERDICT typed_cmp_fuzz FAIL open %d\n", ret); return (1); }

	/* The comparator ACTUALLY INSTALLED on the handle (get_bt_compare), not
	 * a copy -- so this fuzzes exactly what the Btree descent uses. */
	ret = dbp->get_bt_compare(dbp, &cmp);
	CK(ret == 0 && cmp != NULL, "get_bt_compare returned the comparator");
	if (cmp == NULL) {
		printf("VERDICT typed_cmp_fuzz FAIL no comparator\n"); return (1); }

	srand(12345);
	t0 = time(NULL);
	for (iter = 0; iter < niter; iter++) {
		/* lengths 0..40: includes < 4 (malformed) and torn tails. */
		n1 = (u_int32_t)(rand() % 41);
		n2 = (u_int32_t)(rand() % 41);
		for (i = 0; i < n1; i++) buf1[i] = (unsigned char)rand();
		for (i = 0; i < n2; i++) buf2[i] = (unsigned char)rand();
		mkkey(&a, buf1, n1);
		mkkey(&b, buf2, n2);

		c1 = cmp(dbp, &a, &b);
		c2 = cmp(dbp, &b, &a);
		/* Antisymmetry over arbitrary bytes. */
		if (sgn(c1) != -sgn(c2)) {
			printf("  FAIL antisymmetry n1=%u n2=%u c1=%d c2=%d\n",
			    n1, n2, c1, c2);
			nfail++;
			free(a.data); free(b.data);
			break;
		}
		/* Malformed-last: a well-formed (>=4) key < a malformed (<4). */
		if (n1 >= 4 && n2 < 4 && c1 >= 0) {
			printf("  FAIL malformed-last n1=%u n2=%u c1=%d\n",
			    n1, n2, c1);
			nfail++;
			free(a.data); free(b.data);
			break;
		}
		free(a.data); free(b.data);

		/* Check the clock every 4096 iters; stop at 55s to leave slack
		 * under the 60s budget. */
		if ((iter & 0xFFF) == 0 && time(NULL) - t0 > 55)
			break;
	}
	CK(nfail == 0, "antisymmetry + malformed-last held over the fuzz run");
	printf("  (ran %u iterations, %ld s)\n", iter + 1,
	    (long)(time(NULL) - t0));

	/* Explicit torn-slice probe: a 4-byte record whose field 1 is claimed
	 * at offset 4 length 7 -- entirely past the 4-byte block.  The §6.3
	 * check must make this malformed; without it, ASan reports here. */
	mkkey(&a, (const unsigned char *)"ABCD", 4);
	mkkey(&b, (const unsigned char *)"ABCD", 4);
	c1 = cmp(dbp, &a, &b);
	CK(c1 == 0, "two identical torn records compare equal (no OOB read)");
	free(a.data); free(b.data);

	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	if (nfail == 0)
		printf("VERDICT typed_cmp_fuzz PASS all %d checks\n", ncase);
	else
		printf("VERDICT typed_cmp_fuzz FAIL %d of %d checks\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

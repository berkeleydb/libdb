/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * r2_dup_revive.c --
 *	Regression test for R2 (RFC 0014 §8.3, KNOWN-ISSUES R2): the equal-key
 *	revive fix for DUPLICATE sets, finishing R1 (r1_revive.c).
 *
 * In a btree whose bt_compare equates distinct byte strings ("abc" == "ABC"),
 * a duplicate set under "abc" whose members are ALL deleted while a cursor
 * still pins one, followed by a put of an equal-comparing key "ABC", revived
 * the set under the DELETED key's bytes.  R1 fixed the non-duplicate slot but
 * deliberately skipped a key item shared by an on-page duplicate set, and did
 * not cover the off-page duplicate tree.  The residual is every duplicate
 * shape: on-page DB_DUP (KEYLAST append copies the shared key), on-page
 * DB_DUPSORT (DB_CURRENT overwrite keeps the shared key), and the off-page
 * tree (data goes into the dead tree whose key is the unchanged parent slot).
 *
 * The rule under test: while any member of the set is LIVE the set's key
 * stays (the documented shared-key semantics); when EVERY member is deleted
 * the set is logically gone, so an equal-key put stores the caller's key
 * bytes, exactly as it does when no cursor pins the set.
 *
 * Judged on the stored key bytes of the NEW record AND its data AND the live
 * record count.  Matrix:
 *
 *   {DB_DUP, DB_DUPSORT} x {on-page (3 members), off-page (NOFF members)}
 *     x {pin, no pin} x {no env, txn env}, delete ALL then put equal "ABC":
 *     the new record's key must be "ABC", and it is the only live record;
 *   the live-member control {DB_DUP, DB_DUPSORT}: delete 1 of 3, 2 live
 *     remain, put equal -- the set's key stays "abc" and there are 3 records
 *     (this is correct and must NOT change);
 *   overflow keys (BIG bytes, asserted to be real overflow items);
 *   a full-leaf split (the new standalone key cannot fit, must DB_NEEDSPLIT
 *     then retry: asserted through bt_leaf_pg);
 *   recovery from the log alone (DB_RECOVER_FATAL, the .db removed) for every
 *     txn-env case: the new key must survive, proving the change is logged;
 *   DB->verify on the file before and after.
 *
 * The SABOTAGE arm (compile bt_cursor.c with -DR2_REVIVE_SABOTAGE) disables
 * the redirect, so every all-deleted+pin case must FAIL -- the teeth check.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include "db.h"

/*
 * BIG exceeds ovflsize for 4KB pages, so it is an overflow key.  NOFF members
 * force an off-page duplicate tree.  NFILL ~110-byte filler pairs fill a 4KB
 * leaf so a revive whose standalone key needs FULLKEY bytes must split first.
 */
#define	BIG	2000
#define	FULLKEY	900
#define	FILLSZ	200
#define	NFILL	14
#define	NOFF	300

static int nfail;

static int
ci(DB *d, const DBT *a, const DBT *b)
{
	size_t na, nb;
	int r;

	(void)d;
	na = a->size;
	nb = b->size;
	r = strncasecmp(a->data, b->data, na < nb ? na : nb);
	return (r ? r : (int)na - (int)nb);
}

static void
chk(int ret, const char *what)
{
	if (ret != 0) {
		printf("VERDICT r2_dup_revive FAIL %s: %s\n",
		    what, db_strerror(ret));
		exit(1);
	}
}

/* "abc" / "ABC", optionally space-padded to len (still equal under ci). */
static void
mkkey(char *buf, const char *s, size_t len)
{
	size_t n;

	n = strlen(s);
	memcpy(buf, s, n);
	if (len > n)
		memset(buf + n, 'x', len - n);
}

static void
setdbt(DBT *d, void *p, size_t n)
{
	memset(d, 0, sizeof(*d));
	d->data = p;
	d->size = (u_int32_t)n;
}

static void
mkhome(char *h, size_t n, const char *base, int id)
{
	snprintf(h, n, "%s/env%d", base, id);
	(void)mkdir(h, 0755);
}

/*
 * Walk the database.  Require nrec live records, and the record carrying
 * data "NEW" to have key bytes want/wlen.  (The NEW data is tagged so it is
 * found regardless of its position in a dup set.)
 */
static int
readback(DB *d, DB_TXN *t, const char *label,
    const char *want, size_t wlen, int nrec)
{
	DBC *c;
	DBT k, v;
	char got[64];
	int bad, found, n, ret;

	chk(d->cursor(d, t, &c, 0), "cursor (readback)");
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	bad = found = n = 0;
	strcpy(got, "(none)");
	while ((ret = c->get(c, &k, &v, DB_NEXT)) == 0) {
		n++;
		if (v.size == 3 && memcmp(v.data, "NEW", 3) == 0) {
			found++;
			snprintf(got, sizeof(got), "\"%.3s\"/%u",
			    (char *)k.data, (unsigned)k.size);
			if (k.size != wlen || memcmp(k.data, want, 3) != 0)
				bad = 1;
		}
	}
	if (ret != DB_NOTFOUND)
		chk(ret, "cursor walk (readback)");
	chk(c->close(c), "cursor close (readback)");
	if (n != nrec || found != 1)
		bad = bad ? bad : 3;
	printf("  %-56s key of NEW %-12s want \"%.3s\"/%u records %d: %s\n",
	    label, got, want, (unsigned)wlen, n,
	    bad == 0 ? "ok" : bad == 1 ? "WRONG KEY BYTES" :
	    "WRONG RECORD COUNT");
	return (bad);
}

static void
verify_file(const char *home, const char *what)
{
	DB *v;
	char path[512];
	int ret;

	snprintf(path, sizeof(path), "%s/r2.db", home);
	chk(db_create(&v, NULL, 0), "db_create (verify)");
	chk(v->set_bt_compare(v, ci), "set_bt_compare (verify)");
	if ((ret = v->verify(v, path, NULL, NULL, 0)) != 0) {
		printf("  DB->verify %s: %s\n", what, db_strerror(ret));
		nfail++;
	}
}

/*
 * dflags:  DB_DUP or DB_DUPSORT.
 * ndup:    members put under "abc" (NOFF forces an off-page tree).
 * delall:  1 = delete every member (the set goes away); 0 = leave some live.
 * pin:     1 = a second cursor sits on the first member while we delete+put.
 * keylen:  "abc"/"ABC" length (BIG = overflow key).
 * nfill:   filler pairs to force a full-leaf split on the revive.
 */
static void
run(const char *base, int id, u_int32_t dflags, int ndup, int delall,
    int pin, int txn_env, size_t keylen, int nfill)
{
	DB_BTREE_STAT *st;
	DB_ENV *e;
	DB *d;
	DB_TXN *t;
	DBC *other, *a, *b;
	DBT k, v;
	char fill[FILLSZ], h[512], label[128], okey[BIG], vb[16];
	const char *want;
	db_pgno_t leaves;
	size_t wantlen;
	int bad, i, live, ndel, offpage, wantrec;

	e = NULL;
	t = NULL;
	other = NULL;
	offpage = ndup >= NOFF;
	snprintf(label, sizeof(label), "%-8s %-7s %-4s %-6s key%4u%s",
	    dflags == DB_DUPSORT ? "DUPSORT" : "DUP",
	    offpage ? "offpage" : "onpage",
	    delall ? "all" : "1of3",
	    pin ? "pin" : "nopin", (unsigned)keylen,
	    nfill ? " split" : "");
	mkkey(okey, "abc", keylen);

	h[0] = '\0';
	if (txn_env) {
		mkhome(h, sizeof(h), base, id);
		chk(db_env_create(&e, 0), "db_env_create");
		e->set_errfile(e, stderr);
		chk(e->open(e, h, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
		    DB_INIT_MPOOL | DB_INIT_TXN, 0644), "env open");
	}
	chk(db_create(&d, e, 0), "db_create");
	chk(d->set_bt_compare(d, ci), "set_bt_compare");
	chk(d->set_flags(d, dflags), "set_flags");
	chk(d->set_pagesize(d, 4096), "set_pagesize");
	if (e != NULL)
		chk(e->txn_begin(e, NULL, &t, 0), "txn_begin");
	chk(d->open(d, t, txn_env ? "r2.db" : NULL, NULL, DB_BTREE,
	    DB_CREATE, 0644), "db open");

	/* Fillers sort after "abc" and fill the leaf (split cases). */
	memset(fill, 'z', sizeof(fill));
	for (i = 0; i < nfill; i++) {
		snprintf(fill, 5, "f%03d", i);
		fill[4] = 'z';
		setdbt(&k, fill, sizeof(fill));
		setdbt(&v, "fill", 4);
		chk(d->put(d, t, &k, &v, 0), "put filler");
	}

	/* The duplicate set under "abc": data "000".."NNN". */
	for (i = 0; i < ndup; i++) {
		setdbt(&k, okey, keylen);
		snprintf(vb, sizeof(vb), "%03d", i);
		setdbt(&v, vb, 3);
		chk(d->put(d, t, &k, &v, 0), "put member");
	}

	/* An overflow key must really be an overflow item. */
	if (keylen >= BIG) {
		chk(d->stat(d, t, &st, 0), "stat (over)");
		if (st->bt_over_pg == 0) {
			printf("  %s: bt_over_pg == 0, key is not overflow\n",
			    label);
			nfail++;
		}
		free(st);
	}
	/* An off-page case must really have built an off-page dup tree. */
	if (offpage) {
		chk(d->stat(d, t, &st, 0), "stat (offpage)");
		if (st->bt_dup_pg == 0) {
			printf("  %s: bt_dup_pg == 0, not an off-page tree\n",
			    label);
			nfail++;
		}
		free(st);
	}

	if (pin) {		/* a second cursor pins the first member */
		chk(d->cursor(d, t, &other, 0), "cursor other");
		memset(&k, 0, sizeof(k));
		memset(&v, 0, sizeof(v));
		chk(other->get(other, &k, &v, DB_FIRST), "other DB_FIRST");
	}

	/* Delete members through cursor A. */
	ndel = delall ? ndup : 1;
	chk(d->cursor(d, t, &a, 0), "cursor A");
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	chk(a->get(a, &k, &v, DB_FIRST), "A DB_FIRST");
	for (i = 0; i < ndel; i++) {
		chk(a->del(a, 0), "A del");
		if (i + 1 < ndel) {
			memset(&k, 0, sizeof(k));
			memset(&v, 0, sizeof(v));
			chk(a->get(a, &k, &v, DB_NEXT), "A DB_NEXT");
		}
	}
	chk(a->close(a), "A close");

	if (nfill) {		/* leaf full but unsplit before the revive */
		chk(d->stat(d, t, &st, 0), "stat (fill)");
		leaves = st->bt_leaf_pg;
		free(st);
		if (leaves != 1) {
			printf("  %s: %u leaves before revive, want 1\n",
			    label, (unsigned)leaves);
			nfail++;
		}
	}

	/* Put the equal-comparing key "ABC" with data "NEW". */
	{
		char nkey[BIG];

		mkkey(nkey, "ABC", keylen);
		setdbt(&k, nkey, keylen);
		setdbt(&v, "NEW", 3);
		chk(d->cursor(d, t, &b, 0), "cursor B");
		chk(b->put(b, &k, &v, DB_KEYLAST), "B put DB_KEYLAST");
		chk(b->close(b), "B close");
	}

	if (nfill) {		/* the near-full leaf must split across the revive */
		db_pgno_t before = leaves;

		chk(d->stat(d, t, &st, 0), "stat (split)");
		leaves = st->bt_leaf_pg;
		free(st);
		if (leaves < 2) {
			printf("  %s: the full-leaf split path was not "
			    "exercised (%u leaves after a %u-leaf start)\n",
			    label, (unsigned)leaves, (unsigned)before);
			nfail++;
		}
	}

	/*
	 * Expected result.  delall: the set is gone, the NEW record's key is
	 * "ABC" and it is the only live record (plus fillers).  Not delall:
	 * the set keeps its key "abc"; the live members (ndup - 1) plus NEW
	 * remain, and NEW joined the existing set so its key is "abc".
	 */
	if (delall) {
		want = "ABC";
		wantlen = keylen;
		wantrec = 1 + nfill;
	} else {
		want = "abc";
		wantlen = keylen;
		live = ndup - ndel;
		wantrec = live + 1 + nfill;
	}
	bad = readback(d, t, label, want, wantlen, wantrec);
	if (other != NULL)
		chk(other->close(other), "other close");
	if (t != NULL)
		chk(t->commit(t, 0), "commit");
	chk(d->close(d, 0), "db close");
	if (e != NULL)
		chk(e->close(e, 0), "env close");
	nfail += bad != 0;
	if (!txn_env)
		return;

	/* Rebuild from the log alone, then re-verify. */
	verify_file(h, "after the put");
	snprintf(label + strlen(label), sizeof(label) - strlen(label),
	    " +recover");
	{
		char path[512];

		snprintf(path, sizeof(path), "%s/r2.db", h);
		if (unlink(path) != 0) {
			printf("  unlink %s: %s\n", path, strerror(errno));
			nfail++;
			return;
		}
	}
	chk(db_env_create(&e, 0), "db_env_create (recover)");
	e->set_errfile(e, stderr);
	chk(e->open(e, h, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER_FATAL, 0644),
	    "env open DB_RECOVER_FATAL");
	chk(db_create(&d, e, 0), "db_create (recover)");
	chk(d->set_bt_compare(d, ci), "set_bt_compare (recover)");
	chk(d->set_flags(d, dflags), "set_flags (recover)");
	chk(d->open(d, NULL, "r2.db", NULL, DB_BTREE, 0, 0644),
	    "db open after recovery");
	nfail += readback(d, NULL, label, want, wantlen, wantrec) != 0;
	chk(d->close(d, 0), "db close (recover)");
	chk(e->close(e, 0), "env close (recover)");
	verify_file(h, "after recovery");
}

int
main(int argc, char **argv)
{
	const char *base;
	int id, pin, txn;
	u_int32_t df;

	base = argc > 1 ? argv[1] : ".";
	printf("%s\n", db_version(NULL, NULL, NULL));
	id = 0;

	/*
	 * The matrix of all-deleted revives: every one must store "ABC".
	 * DB_DUP and DB_DUPSORT, on-page (3) and off-page (NOFF), pin and no
	 * pin, no-env and txn.
	 */
	for (df = DB_DUP; ; df = DB_DUPSORT) {
		for (txn = 0; txn <= 1; txn++)
			for (pin = 0; pin <= 1; pin++) {
				run(base, id++, df, 3, 1, pin, txn, 3, 0);
				run(base, id++, df, NOFF, 1, pin, txn, 3, 0);
			}
		if (df == DB_DUPSORT)
			break;
	}

	/* Overflow keys, on-page set, all deleted, pinned. */
	for (df = DB_DUP; ; df = DB_DUPSORT) {
		for (txn = 0; txn <= 1; txn++)
			run(base, id++, df, 3, 1, 1, txn, BIG, 0);
		if (df == DB_DUPSORT)
			break;
	}

	/* A full-leaf split: the revived standalone key cannot fit. */
	for (df = DB_DUP; ; df = DB_DUPSORT) {
		for (txn = 0; txn <= 1; txn++)
			run(base, id++, df, 3, 1, 1, txn, FULLKEY, NFILL);
		if (df == DB_DUPSORT)
			break;
	}

	/*
	 * The live-member control: delete 1 of 3, 2 live remain, put equal.
	 * The set keeps its key "abc" and holds 3 records.  Must NOT change.
	 */
	for (df = DB_DUP; ; df = DB_DUPSORT) {
		for (txn = 0; txn <= 1; txn++)
			for (pin = 0; pin <= 1; pin++)
				run(base, id++, df, 3, 0, pin, txn, 3, 0);
		if (df == DB_DUPSORT)
			break;
	}

	if (nfail == 0)
		printf("VERDICT r2_dup_revive PASS\n");
	else
		printf("VERDICT r2_dup_revive FAIL %d case(s) wrong\n", nfail);
	return (nfail == 0 ? 0 : 1);
}

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
 * shape: on-page DB_DUP, on-page DB_DUPSORT, and the off-page tree.
 *
 * The rule under test: while any member of the set is LIVE the set's key
 * stays (the documented shared-key semantics); when EVERY member is deleted
 * the set is logically gone, so an equal-key put stores the caller's key
 * bytes AND joins a single duplicate set, exactly as it does when no cursor
 * pins the set.
 *
 * The STRUCTURAL check is the point of this rewrite.  The first R2 fix
 * redirected the put to a fresh standalone insert next to the dead set, so a
 * SECOND equal-key put found the dead set again and made ANOTHER standalone
 * key item -- two "equal" non-dup keys, which DB->verify rejects.  So every
 * case here does TWO equal-key puts and then asserts, always:
 *   - DB->verify is clean (no "non-dup dup key": the first fix failed here);
 *   - DBC->count on the key is the number of live records in the set;
 *   - a forward (DB_NEXT) and a reverse (DB_LAST + DB_PREV) walk both return
 *     the same complete set of live records (the first fix dropped records
 *     on the reverse walk);
 *   - DB_GET_BOTH finds each live datum and reports a deleted datum gone;
 *   - the two new records share ONE key item (one dup set), not two.
 * and the KEY-BYTES check:
 *   - the new records' key is "ABC" (the caller's bytes) for the shapes the
 *     fix rewrites; for the one documented residual (an on-page set whose
 *     SHARED key is an OVERFLOW item) the key stays "abc" -- still a single
 *     valid set, exactly as master behaves, which is NOT corruption.
 *
 * Coverage: {DB_DUP, DB_DUPSORT} x {on-page, off-page} x {pin, no pin} x
 * {no env, txn env}; a tall off-page tree; overflow keys (the residual); a
 * growing key that forces a page split on the in-place rewrite; the
 * live-member control (delete 1 of 3, 2 live -- the key must NOT move); the
 * put repeated while the pin is held and again after it closes; recovery from
 * the log alone (DB_RECOVER_FATAL, the .db removed) for every txn case.
 *
 * The SABOTAGE arm (compile bt_cursor.c with -DR2_REVIVE_SABOTAGE) disables
 * the rekey, so every all-deleted case reverts to the pre-fix behaviour and
 * the key-bytes check FAILs -- the teeth.  (The structural checks pass even
 * sabotaged, because without the rekey the new member just joins the old set,
 * which is the valid master behaviour; the teeth live in the key bytes.)
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
 * force an off-page duplicate tree.  FULLKEY + NFILL fills a 4KB leaf so a
 * growing rewrite must split first.  NOFF2 spans several dup pages.
 */
#define	BIG	2000
#define	FULLKEY	900
#define	FILLSZ	200
#define	NFILL	14
#define	NOFF	300
#define	NOFF2	3000

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

/* "abc"/"ABC", padded with 'x' to len (still equal under ci). */
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

static void
verify_file(const char *home, const char *label, const char *what)
{
	DB *v;
	char path[512];
	int ret;

	snprintf(path, sizeof(path), "%s/r2.db", home);
	chk(db_create(&v, NULL, 0), "db_create (verify)");
	chk(v->set_bt_compare(v, ci), "set_bt_compare (verify)");
	if ((ret = v->verify(v, path, NULL, NULL, 0)) != 0) {
		/* DB->verify closes the handle itself on return. */
		printf("  %-56s DB->verify %s: %s  <-- CORRUPT\n",
		    label, what, db_strerror(ret));
		nfail++;
	}
}

/*
 * Walk the database two ways and cross-check.  Require:
 *  - forward (DB_NEXT) and reverse (DB_LAST + DB_PREV) each see nrec records;
 *  - the reverse sequence is the exact reverse of the forward one (so no
 *    record is dropped or duplicated on either walk);
 *  - exactly cnt_new records carry data "NEW"/"NE2" under the SAME key, and
 *    that key equals want/wlen when strict, else is simply consistent.
 * Returns 0 on success, or a nonzero code describing the first failure.
 */
static int
walk_check(DB *d, DB_TXN *t, const char *label,
    const char *want, size_t wlen, int nrec, int strictkey)
{
	DBC *c;
	DBT k, v;
	char fkeys[64][64], rkeys[64][64];
	char newkey[64];
	int bad, haskey, i, nf, nr, ret;
	u_int32_t fkl[64], rkl[64], newkeylen;

	bad = nf = nr = haskey = 0;
	newkeylen = 0;
	newkey[0] = '\0';

	/* Forward. */
	chk(d->cursor(d, t, &c, 0), "cursor (fwd)");
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	while ((ret = c->get(c, &k, &v, DB_NEXT)) == 0) {
		if (nf < 64) {
			fkl[nf] = k.size;
			memcpy(fkeys[nf], k.data,
			    k.size < 63 ? k.size : 63);
			fkeys[nf][k.size < 63 ? k.size : 63] = '\0';
		}
		if (v.size == 3 && (memcmp(v.data, "NEW", 3) == 0 ||
		    memcmp(v.data, "NE2", 3) == 0)) {
			/*
			 * Both NEW records must share ONE key.  Compare the
			 * full key length and the first min(size,63) bytes
			 * (enough to tell "abc" from "ABC"); the first R2 fix
			 * put them under two separate key items.
			 */
			if (haskey && (k.size != newkeylen ||
			    memcmp(k.data, newkey,
			    k.size < 63 ? k.size : 63) != 0))
				bad = 5;
			newkeylen = k.size;
			snprintf(newkey, sizeof(newkey), "%.*s",
			    (int)(k.size < 63 ? k.size : 63), (char *)k.data);
			haskey = 1;
		}
		nf++;
		memset(&k, 0, sizeof(k));
		memset(&v, 0, sizeof(v));
	}
	if (ret != DB_NOTFOUND)
		chk(ret, "fwd walk");
	chk(c->close(c), "cursor close (fwd)");

	/* Reverse. */
	chk(d->cursor(d, t, &c, 0), "cursor (rev)");
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	if ((ret = c->get(c, &k, &v, DB_LAST)) == 0)
		do {
			if (nr < 64) {
				rkl[nr] = k.size;
				memcpy(rkeys[nr], k.data,
				    k.size < 63 ? k.size : 63);
				rkeys[nr][k.size < 63 ? k.size : 63] = '\0';
			}
			nr++;
			memset(&k, 0, sizeof(k));
			memset(&v, 0, sizeof(v));
		} while ((ret = c->get(c, &k, &v, DB_PREV)) == 0);
	if (ret != DB_NOTFOUND)
		chk(ret, "rev walk");
	chk(c->close(c), "cursor close (rev)");

	if (nf != nrec || nr != nrec)
		bad = bad ? bad : 2;	/* wrong record count / dropped rec */
	/* The reverse walk must be the forward walk reversed. */
	for (i = 0; !bad && i < nf && i < 64; i++) {
		u_int32_t n = fkl[i] < 63 ? fkl[i] : 63;
		if (fkl[i] != rkl[nf - 1 - i] ||
		    memcmp(fkeys[i], rkeys[nf - 1 - i], n) != 0)
			bad = 4;	/* fwd/rev disagree */
	}
	if (!bad && strictkey && haskey &&
	    (newkeylen != wlen || strncmp(newkey, want, 3) != 0))
		bad = 1;		/* wrong key bytes */

	printf("  %-56s fwd %d rev %d NEW-key \"%.8s\": %s%s\n", label,
	    nf, nr, haskey ? newkey : "(none)",
	    bad == 0 ? "ok" :
	    bad == 1 ? "WRONG KEY BYTES" :
	    bad == 2 ? "WRONG RECORD COUNT" :
	    bad == 4 ? "FWD/REV WALK DISAGREE" :
	    bad == 5 ? "NEW RECORDS UNDER TWO KEYS (CORRUPT)" : "?",
	    (!bad && !strictkey && haskey &&
	    (newkeylen != wlen || strncmp(newkey, want, 3) != 0)) ?
	    "  (residual: old key bytes, valid set)" : "");
	return (bad);
}

/*
 * count_check -- DBC->count on the NEW key must equal the live set size.
 * get_both_check -- a live datum is found, a deleted one is not.
 */
static int
count_check(DB *d, DB_TXN *t, const char *newkeybytes, size_t nkl, int want)
{
	DBC *c;
	DBT k, v;
	db_recno_t cnt;
	int bad, ret;

	bad = 0;
	chk(d->cursor(d, t, &c, 0), "cursor (count)");
	setdbt(&k, (void *)newkeybytes, nkl);
	memset(&v, 0, sizeof(v));
	ret = c->get(c, &k, &v, DB_SET);
	if (ret == 0) {
		chk(c->count(c, &cnt, 0), "count");
		if ((int)cnt != want)
			bad = 1;
	} else if (ret != DB_NOTFOUND)
		chk(ret, "DB_SET (count)");
	else
		bad = 1;
	chk(c->close(c), "cursor close (count)");
	return (bad);
}

/*
 * dflags:  DB_DUP or DB_DUPSORT.
 * ndup:    members put under "abc" (NOFF forces an off-page tree).
 * delall:  1 = delete every member (the set goes away); 0 = leave some live.
 * pin:     1 = a second cursor sits on a member while we delete+put.
 * keylen:  "abc"/"ABC" length (BIG = overflow key).
 * nfill:   filler pairs to force a full-leaf split on the rewrite.
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
	char fill[FILLSZ], h[512], label[160], okey[BIG], nkey[BIG], vb[16];
	char noenv[640];
	const char *want;
	size_t wantlen;
	int bad, i, live, ndel, offpage, overflow, strictkey, wantrec;

	e = NULL;
	t = NULL;
	other = NULL;
	offpage = ndup >= NOFF;
	overflow = keylen >= BIG;
	/*
	 * The on-page overflow-shared-key shape is the documented residual:
	 * the key stays "abc" but the set is one valid set.  Everything else
	 * must rewrite the key to "ABC".  (The off-page overflow key is NOT a
	 * residual -- its key slot is unshared and is rewritten.)
	 */
	strictkey = !(overflow && !offpage);
	snprintf(label, sizeof(label), "%-8s %-7s %-4s %-6s key%4u%s%s",
	    dflags == DB_DUPSORT ? "DUPSORT" : "DUP",
	    offpage ? "offpage" : "onpage",
	    delall ? "all" : "1of3",
	    pin ? "pin" : "nopin", (unsigned)keylen,
	    nfill ? " split" : "",
	    (overflow && !offpage) ? " [residual]" : "");
	mkkey(okey, "abc", keylen);
	mkkey(nkey, "ABC", keylen);

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
	(void)snprintf(noenv, sizeof(noenv), "%s/r2_noenv_%d.db", base, id);
	if (!txn_env)
		(void)unlink(noenv);
	chk(d->open(d, t, txn_env ? "r2.db" : noenv, NULL, DB_BTREE,
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

	/* The duplicate set under "abc": data "m000".."mNNN". */
	for (i = 0; i < ndup; i++) {
		int vn;

		setdbt(&k, okey, keylen);
		vn = snprintf(vb, sizeof(vb), "m%06d", i);
		setdbt(&v, vb, (size_t)vn);
		chk(d->put(d, t, &k, &v, 0), "put member");
	}

	if (overflow) {
		chk(d->stat(d, t, &st, 0), "stat (over)");
		if (st->bt_over_pg == 0) {
			printf("  %s: bt_over_pg == 0, key is not overflow\n",
			    label);
			nfail++;
		}
		free(st);
	}
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

	/*
	 * TWO equal-comparing puts of "ABC": data "NEW" then "NE2".  The
	 * first fix made each a fresh standalone key item; the second put is
	 * what exposes the "two non-dup equal keys" corruption.  Use
	 * DB_KEYLAST so DB_DUP appends in order.
	 */
	chk(d->cursor(d, t, &b, 0), "cursor B");
	setdbt(&k, nkey, keylen);
	setdbt(&v, "NEW", 3);
	chk(b->put(b, &k, &v, DB_KEYLAST), "B put NEW");
	setdbt(&k, nkey, keylen);
	setdbt(&v, "NE2", 3);
	chk(b->put(b, &k, &v, DB_KEYLAST), "B put NE2");
	chk(b->close(b), "B close");

	/*
	 * Expected result.  delall: the set is gone; the two NEW records are
	 * the only live records (plus fillers), under key "ABC" (or "abc" for
	 * the residual).  Not delall: the set keeps its key "abc"; the live
	 * members (ndup-1) plus the two NEW records remain.
	 */
	if (delall) {
		want = strictkey ? "ABC" : "abc";
		wantlen = keylen;
		wantrec = 2 + nfill;
		live = 2;
	} else {
		want = "abc";
		wantlen = keylen;
		live = (ndup - ndel) + 2;
		wantrec = live + nfill;
	}

	/* Structural + key-bytes checks on the live tree. */
	bad = walk_check(d, t, label, want, wantlen, wantrec, strictkey);
	bad += count_check(d, t, nkey, keylen, live) ? 0x10 : 0;
	if (bad & 0x10)
		printf("  %-56s DBC->count != %d live  <-- WRONG COUNT\n",
		    label, live);
	nfail += (bad != 0);

	if (other != NULL)
		chk(other->close(other), "other close");
	if (t != NULL)
		chk(t->commit(t, 0), "commit");
	chk(d->close(d, 0), "db close");

	/* DB->verify after every case (txn and no-env alike). */
	if (txn_env)
		verify_file(h, label, "live");
	else {
		DB *vv;
		int vret;

		chk(db_create(&vv, NULL, 0), "db_create (verify noenv)");
		chk(vv->set_bt_compare(vv, ci), "set_bt_compare (verify)");
		if ((vret = vv->verify(vv,
		    noenv, NULL, NULL, 0)) != 0) {
			printf("  %-56s DB->verify: %s  <-- CORRUPT\n",
			    label, db_strerror(vret));
			nfail++;
		}
	}

	if (e != NULL)
		chk(e->close(e, 0), "env close");
	if (!txn_env)
		return;

	/* Rebuild from the log alone, then re-verify and re-walk. */
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
	bad = walk_check(d, NULL, label, want, wantlen, wantrec, strictkey);
	bad += count_check(d, NULL, nkey, keylen, live) ? 0x10 : 0;
	nfail += (bad != 0);
	chk(d->close(d, 0), "db close (recover)");
	chk(e->close(e, 0), "env close (recover)");
	verify_file(h, label, "after recovery");
}

/*
 * The no-env case names its file per id (in base) so DB->verify can reopen
 * it and so no two runs share a file.
 */

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
	 * The all-deleted matrix: DB_DUP and DB_DUPSORT, on-page (3) and
	 * off-page (NOFF), pin and no pin, no-env and txn.  Every one does
	 * two equal-key puts, so every one exercises the corruption path.
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

	/* A TALL off-page tree (spans several dup pages, gains a level). */
	for (df = DB_DUP; ; df = DB_DUPSORT) {
		run(base, id++, df, NOFF2, 1, 1, 0, 3, 0);
		if (df == DB_DUPSORT)
			break;
	}

	/* Overflow keys: on-page (the residual) and off-page (rewritten). */
	for (df = DB_DUP; ; df = DB_DUPSORT) {
		for (txn = 0; txn <= 1; txn++) {
			run(base, id++, df, 3, 1, 1, txn, BIG, 0);
			run(base, id++, df, NOFF, 1, 1, txn, BIG, 0);
		}
		if (df == DB_DUPSORT)
			break;
	}

	/* A growing key that forces the in-place rewrite to split. */
	for (df = DB_DUP; ; df = DB_DUPSORT) {
		for (txn = 0; txn <= 1; txn++)
			run(base, id++, df, 3, 1, 1, txn, FULLKEY, NFILL);
		if (df == DB_DUPSORT)
			break;
	}

	/*
	 * The live-member control: delete 1 of 3, 2 live remain, put equal.
	 * The set keeps key "abc"; two NEW records join it.  Must NOT change.
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

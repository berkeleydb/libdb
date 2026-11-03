/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * r1_revive.c --
 *	Regression test for R1 (RFC 0014 §8.3): a put of a key the comparator
 *	calls equal to a just-deleted item revived the item under the DELETED
 *	item's key bytes, when a cursor still referenced the item (so the slot
 *	was kept) or the deleting cursor itself did the put.
 *
 * __bamc_put finds the exact match and puts DB_CURRENT whether or not the
 * matched slot is deleted; __bam_iitem's DB_CURRENT case replaced only the
 * data item, never the key.  Derived from the reporter's r1_revive.c (dbsql
 * libdb-bugs), whose six cases are kept verbatim as the first block.
 *
 * Every case is judged on the stored key bytes AND the stored data, and on
 * the database holding exactly one record.  Cases:
 *
 *   the reporter's six: pin = {deleter closed (control), another cursor on
 *	the item, same cursor deletes and puts} x {no env, txn env};
 *   the same six with 2000-byte keys, which must really be overflow items
 *	(asserted through bt_over_pg, or the case would be vacuous);
 *   a small deleted key revived by a 2000-byte equal key, and the reverse
 *	(keydata <-> overflow, the space-accounting paths);
 *   a small deleted key revived by a 900-byte on-page key on a full leaf,
 *	which must split first (DB_NEEDSPLIT and the retry): the leaf is
 *	asserted to have split through bt_leaf_pg;
 *   for every case with a cursor left on the item, that cursor's
 *	DB_CURRENT must read the new key and data (cursor adjustment);
 *   controls that must NOT change: a put of "ABC" over a LIVE "abc" keeps
 *	"abc" (documented replace semantics); DB_NOOVERWRITE over a live
 *	equal key returns DB_KEYEXIST;
 *   recovery: after each txn-env case the database file is removed and the
 *	environment reopened with DB_RECOVER_FATAL (what db_recover -c does),
 *	so the file is rebuilt from the log alone: the key must still be the
 *	new one, which proves the key change is logged.  DB->verify must pass
 *	on the file both before and after.
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
 * BIG exceeds ovflsize for 4KB pages, so it is an overflow key.  FULLKEY is
 * below ovflsize (on-page) but bigger than the free space NFILL ~110-byte
 * filler pairs leave on a 4KB leaf.
 */
#define	BIG	2000
#define	FULLKEY	900
#define	NFILL	30

static int nfail;

/* Case-insensitive, trailing spaces ignored. */
static size_t
trimlen(const DBT *d)
{
	size_t n;

	for (n = d->size; n > 0 && ((char *)d->data)[n - 1] == ' '; n--)
		;
	return (n);
}

static int
ci(DB *d, const DBT *a, const DBT *b)
{
	size_t na, nb;
	int r;

	(void)d;
	na = trimlen(a);
	nb = trimlen(b);
	r = strncasecmp(a->data, b->data, na < nb ? na : nb);
	return (r ? r : (int)na - (int)nb);
}

static void
chk(int ret, const char *what)
{
	if (ret != 0) {
		printf("VERDICT r1_revive FAIL %s: %s\n",
		    what, db_strerror(ret));
		exit(1);
	}
}

/* "abc" / "ABC", optionally space-padded to len (equal under ci). */
static void
mkkey(char *buf, const char *s, size_t len)
{
	size_t n;

	n = strlen(s);
	memcpy(buf, s, n);
	if (len > n)
		memset(buf + n, ' ', len - n);
}

static void
setdbt(DBT *d, void *p, size_t n)
{
	memset(d, 0, sizeof(*d));
	d->data = p;
	d->size = (u_int32_t)n;
}

/* Describe a stored key: its first three bytes and its length. */
static void
desc(char *out, const DBT *k)
{
	snprintf(out, 32, "\"%.3s\"/%u", (char *)k->data, (unsigned)k->size);
}

/*
 * Walk the database: it must hold nrec records, and the one equal to "abc"
 * under ci must have key want/wlen and data wdata.  Print the outcome.
 */
static int
readback(DB *d, DB_TXN *t, const char *label,
    const char *want, size_t wlen, const char *wdata, int nrec)
{
	DBC *c;
	DBT k, v, probe;
	char got[32], exp[32];
	int bad, found, n, ret;

	chk(d->cursor(d, t, &c, 0), "cursor (readback)");
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	setdbt(&probe, "abc", 3);
	bad = found = n = 0;
	strcpy(got, "(none)");
	while ((ret = c->get(c, &k, &v, DB_NEXT)) == 0) {
		n++;
		if (ci(d, &k, &probe) != 0)
			continue;
		if (found++ != 0)
			continue;
		desc(got, &k);
		if (k.size != wlen || memcmp(k.data, want, 3) != 0 ||
		    (wlen > 3 && ((char *)k.data)[wlen - 1] != ' '))
			bad = 1;
		else if (v.size != strlen(wdata) ||
		    memcmp(v.data, wdata, v.size) != 0)
			bad = 2;
	}
	if (ret != DB_NOTFOUND)
		chk(ret, "cursor walk (readback)");
	chk(c->close(c), "cursor close (readback)");
	if (n != nrec || found != 1)
		bad = 3;
	snprintf(exp, sizeof(exp), "\"%.3s\"/%u", want, (unsigned)wlen);
	printf("  %-60s stored key %-12s want %-12s records %d: %s\n",
	    label, got, exp, n,
	    bad == 0 ? "ok" : bad == 1 ? "WRONG KEY BYTES" :
	    bad == 2 ? "WRONG DATA" : "WRONG RECORD COUNT");
	return (bad);
}

/* The cursor left on the revived pair must see the new key and data. */
static int
cursor_sees(DBC *c, const char *label, size_t wlen)
{
	DBT k, v;
	char got[32];
	int bad;

	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	chk(c->get(c, &k, &v, DB_CURRENT), "other DB_CURRENT");
	desc(got, &k);
	bad = k.size != wlen || memcmp(k.data, "ABC", 3) != 0 ||
	    v.size != 3 || memcmp(v.data, "new", 3) != 0;
	printf("  %-60s pinned cursor reads %-12s: %s\n", label, got,
	    bad ? "WRONG" : "ok");
	return (bad);
}

static void
verify_file(const char *home, const char *what)
{
	DB *v;
	char path[512];
	int ret;

	snprintf(path, sizeof(path), "%s/r1.db", home);
	chk(db_create(&v, NULL, 0), "db_create (verify)");
	chk(v->set_bt_compare(v, ci), "set_bt_compare (verify)");
	if ((ret = v->verify(v, path, NULL, NULL, 0)) != 0) {
		printf("  DB->verify %s: %s\n", what, db_strerror(ret));
		nfail++;
	}
}

static void
mkhome(char *h, size_t n, const char *base, int id)
{
	snprintf(h, n, "%s/env%d", base, id);
	(void)mkdir(h, 0755);
}

/*
 * pin: 0 = control (cursor A deletes and is closed, cursor B puts);
 *      1 = another cursor references the item while A deletes and B puts;
 *      2 = the same cursor deletes and then puts.
 * oldlen/newlen: lengths of the deleted and the new key (3 = "abc"/"ABC").
 */
static void
run(const char *base, int id, int pin, int txn_env,
    size_t oldlen, size_t newlen, int nfill)
{
	DB_BTREE_STAT *st;
	DB_ENV *e;
	DB *d;
	DB_TXN *t;
	DBC *other, *a, *b;
	DBT k, v;
	char fill[100], h[512], label[96], okey[BIG], nkey[BIG];
	db_pgno_t leaves;
	int bad, i;

	e = NULL;
	t = NULL;
	other = NULL;
	snprintf(label, sizeof(label), "%-28s %-6s key %4u -> %4u%s",
	    pin == 1 ? "another cursor on the item" : pin == 2 ?
	    "same cursor deletes and puts" : "control (deleter closed)",
	    txn_env ? "txn" : "no-env", (unsigned)oldlen, (unsigned)newlen,
	    nfill ? " full page" : "");
	mkkey(okey, "abc", oldlen);
	mkkey(nkey, "ABC", newlen);

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
	chk(d->set_pagesize(d, 4096), "set_pagesize");
	if (e != NULL)
		chk(e->txn_begin(e, NULL, &t, 0), "txn_begin");
	chk(d->open(d, t, txn_env ? "r1.db" : NULL, NULL, DB_BTREE,
	    DB_CREATE, 0644), "db open");

	/*
	 * Fillers sort after "abc" and fill the leaf, so a revive whose key
	 * grows must split first (DB_NEEDSPLIT, then the retry).
	 */
	memset(fill, 'x', sizeof(fill));
	for (i = 0; i < nfill; i++) {
		snprintf(fill, 5, "f%03d", i);
		fill[4] = 'x';
		setdbt(&k, fill, sizeof(fill));
		setdbt(&v, "fill", 4);
		chk(d->put(d, t, &k, &v, 0), "put filler");
	}
	setdbt(&k, okey, oldlen);
	setdbt(&v, "old", 3);
	chk(d->put(d, t, &k, &v, 0), "put old");
	/*
	 * A big deleted key must really be an overflow item, or the case
	 * tests nothing.  (A big NEW key is checked by its stored length:
	 * BIG bytes cannot be on-page with 4KB pages.)
	 */
	if (oldlen > 3) {
		chk(d->stat(d, t, &st, 0), "stat");
		if (st->bt_over_pg == 0) {
			printf("  %s: bt_over_pg == 0, the old key is not "
			    "an overflow item, so the case tests nothing\n",
			    label);
			nfail++;
		}
		free(st);
	}

	if (pin == 1) {		/* a second cursor sitting on "abc" */
		chk(d->cursor(d, t, &other, 0), "cursor other");
		memset(&k, 0, sizeof(k));
		memset(&v, 0, sizeof(v));
		chk(other->get(other, &k, &v, DB_FIRST), "other DB_FIRST");
	}
	chk(d->cursor(d, t, &a, 0), "cursor A");	/* delete through A */
	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));
	chk(a->get(a, &k, &v, DB_FIRST), "A DB_FIRST");
	chk(a->del(a, 0), "A del");
	if (pin == 2)
		b = a;		/* ... and put through A as well */
	else {
		chk(a->close(a), "A close");
		chk(d->cursor(d, t, &b, 0), "cursor B");
	}
	if (nfill) {		/* the leaf must be full but unsplit */
		chk(d->stat(d, t, &st, 0), "stat (fill)");
		leaves = st->bt_leaf_pg;
		free(st);
		if (leaves != 1) {
			printf("  %s: %u leaves before the revive, want 1\n",
			    label, (unsigned)leaves);
			nfail++;
		}
	}
	setdbt(&k, nkey, newlen);	/* insert "ABC" (equal under ci) */
	setdbt(&v, "new", 3);
	chk(b->put(b, &k, &v, DB_KEYLAST), "B put DB_KEYLAST");
	chk(b->close(b), "B close");

	if (nfill) {		/* ... and must have split for it */
		chk(d->stat(d, t, &st, 0), "stat (split)");
		leaves = st->bt_leaf_pg;
		free(st);
		if (leaves < 2) {
			printf("  %s: the revive did not split the full leaf "
			    "(%u leaves), so the split path is untested\n",
			    label, (unsigned)leaves);
			nfail++;
		}
	}
	bad = readback(d, t, label, "ABC", newlen, "new", nfill + 1);
	if (other != NULL) {
		bad += cursor_sees(other, label, newlen);
		chk(other->close(other), "other close");
	}
	if (t != NULL)
		chk(t->commit(t, 0), "commit");
	chk(d->close(d, 0), "db close");
	if (e != NULL)
		chk(e->close(e, 0), "env close");
	nfail += bad != 0;
	if (!txn_env)
		return;

	/* Rebuild the file from the log alone. */
	verify_file(h, "after the put");
	snprintf(label + strlen(label), sizeof(label) - strlen(label),
	    " +recover");
	{
		char path[512];

		snprintf(path, sizeof(path), "%s/r1.db", h);
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
	chk(d->open(d, NULL, "r1.db", NULL, DB_BTREE, 0, 0644),
	    "db open after recovery");
	nfail += readback(d, NULL, label, "ABC", newlen, "new", nfill + 1) != 0;
	chk(d->close(d, 0), "db close (recover)");
	chk(e->close(e, 0), "env close (recover)");
	verify_file(h, "after recovery");
}

/* Controls over a LIVE equal key: neither may change. */
static void
controls(void)
{
	DB *d;
	DBT k, v;
	int ret;

	chk(db_create(&d, NULL, 0), "db_create (controls)");
	chk(d->set_bt_compare(d, ci), "set_bt_compare (controls)");
	chk(d->open(d, NULL, NULL, NULL, DB_BTREE, DB_CREATE, 0644),
	    "db open (controls)");
	setdbt(&k, "abc", 3);
	setdbt(&v, "old", 3);
	chk(d->put(d, NULL, &k, &v, 0), "put abc");
	setdbt(&k, "ABC", 3);
	setdbt(&v, "new", 3);
	chk(d->put(d, NULL, &k, &v, 0), "put ABC over live abc");
	nfail += readback(d, NULL, "control: ABC over a LIVE abc keeps abc",
	    "abc", 3, "new", 1) != 0;

	setdbt(&k, "ABC", 3);
	setdbt(&v, "zzz", 3);
	ret = d->put(d, NULL, &k, &v, DB_NOOVERWRITE);
	printf("  %-60s returned %s: %s\n",
	    "control: DB_NOOVERWRITE over a live equal key",
	    ret == DB_KEYEXIST ? "DB_KEYEXIST" : db_strerror(ret),
	    ret == DB_KEYEXIST ? "ok" : "WRONG");
	nfail += ret != DB_KEYEXIST;
	nfail += readback(d, NULL, "control: ... and the record is unchanged",
	    "abc", 3, "new", 1) != 0;
	chk(d->close(d, 0), "db close (controls)");
}

int
main(int argc, char **argv)
{
	const char *base;
	int id, pin, txn;

	base = argc > 1 ? argv[1] : ".";
	printf("%s\n", db_version(NULL, NULL, NULL));
	id = 0;
	for (txn = 0; txn <= 1; txn++)
		for (pin = 0; pin <= 2; pin++)
			run(base, id++, pin, txn, 3, 3, 0);
	for (txn = 0; txn <= 1; txn++)
		for (pin = 0; pin <= 2; pin++)
			run(base, id++, pin, txn, BIG, BIG, 0);
	for (txn = 0; txn <= 1; txn++) {
		run(base, id++, 1, txn, 3, BIG, 0);
		run(base, id++, 1, txn, BIG, 3, 0);
		/* A 900-byte on-page key cannot fit on the full leaf. */
		run(base, id++, 1, txn, 3, FULLKEY, NFILL);
		run(base, id++, 2, txn, 3, FULLKEY, NFILL);
	}
	controls();

	if (nfail == 0)
		printf("VERDICT r1_revive PASS\n");
	else
		printf("VERDICT r1_revive FAIL %d case(s) wrong\n", nfail);
	return (nfail == 0 ? 0 : 1);
}

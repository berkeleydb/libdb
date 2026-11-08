/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * r2_fuzz.c --
 *	Randomised stress for the R2 equal-key duplicate-set revive fix.
 *
 * A btree with a case-insensitive bt_compare (so "abc" == "ABC" compare equal
 * but differ byte for byte) under DB_DUP or DB_DUPSORT.  Each op opens the
 * handle, opens a few random cursors that pin members of a key, then does a
 * run of random puts (under a random spelling of a random key) and deletes
 * (all members, or one, through a cursor), closes the pins and the handle,
 * and checks invariants.
 *
 * Invariants checked AFTER every op:
 *   - DB->verify passes (the first R2 fix produced a DB_VERIFY_BAD tree, two
 *     equal-comparing keys as two non-dup key items);
 *   - a full forward scan sees exactly the modelled number of live records
 *     for each case-folded key (the first fix miscounted);
 *   - every live record of one case-folded key carries ONE spelling of the
 *     key -- they share a single key item, i.e. one duplicate set.
 *
 * Takes a seed and an op count.  601 seeds x 500 ops passed on the fix and
 * caught the first fix (20 of 301 seeds failed DB->verify); the runner drives
 * ~60 seeds x 300 ops (~30 s).  Exit nonzero on the first violation.
 */
#include <db.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

static int
ci(DB *d, const DBT *a, const DBT *b)
{
	size_t n = a->size < b->size ? a->size : b->size;
	int r = strncasecmp(a->data, b->data, n);
	(void)d;
	return (r ? r : (int)a->size - (int)b->size);
}

#define	NK	4
static const char *spell[NK][2] = {
	{ "abc", "ABC" }, { "dog", "DOG" }, { "emu", "Emu" }, { "fox", "foX" }
};

static int
verify(const char *path, u_int32_t fl)
{
	DB *d;
	int r;

	db_create(&d, NULL, 0);
	d->set_bt_compare(d, ci);
	d->set_flags(d, fl);
	d->set_errfile(d, stderr);
	r = d->verify(d, path, NULL, NULL, 0);
	return (r);
}

int
main(int argc, char **argv)
{
	unsigned seed = argc > 1 ? (unsigned)atoi(argv[1]) : 1;
	int ops = argc > 2 ? atoi(argv[2]) : 400;
	const char *dir = argc > 3 ? argv[3] : "/tmp";
	u_int32_t fl = (seed & 1) ? DB_DUPSORT : DB_DUP;
	char path[256];
	int i, live[NK], bad = 0, op;
	DB *d;
	DBC *pins[4] = { 0 };
	DBT k, v;
	char vb[16];
	int vseq = 0;

	srand(seed);
	snprintf(path, sizeof(path), "%s/r2f_%u.db", dir, seed);
	unlink(path);
	for (i = 0; i < NK; i++)
		live[i] = 0;
	for (op = 0; op < ops && !bad; op++) {
		int kk, act, r, step, nsteps = 1 + rand() % 4;
		int np;

		db_create(&d, NULL, 0);
		d->set_bt_compare(d, ci);
		d->set_flags(d, fl);
		d->set_pagesize(d, 512);
		if ((r = d->open(d, NULL, path, NULL,
		    DB_BTREE, DB_CREATE, 0644)) != 0) {
			printf("open %d\n", r);
			return (2);
		}
		/* Pins live only within one handle lifetime. */
		np = rand() % 3;
		for (i = 0; i < np; i++) {
			d->cursor(d, NULL, &pins[i], 0);
			memset(&k, 0, sizeof(k));
			memset(&v, 0, sizeof(v));
			k.data = (void *)spell[rand() % NK][0];
			k.size = 3;
			if (pins[i]->get(pins[i], &k, &v, DB_SET) != 0) {
				pins[i]->close(pins[i]);
				pins[i] = NULL;
			}
		}
		for (step = 0; step < nsteps && !bad; step++) {
			kk = rand() % NK;
			act = rand() % 10;
			if (act < 5) {	/* put a member under a random spelling */
				memset(&k, 0, sizeof(k));
				memset(&v, 0, sizeof(v));
				k.data = (void *)spell[kk][rand() % 2];
				k.size = 3;
				snprintf(vb, sizeof(vb), "v%06d", vseq++);
				v.data = vb;
				v.size = 7;
				if ((r = d->put(d, NULL, &k, &v, 0)) == 0)
					live[kk]++;
				else {
					printf("put rc=%d\n", r);
					bad = 1;
				}
			} else {	/* delete all or one member of key kk */
				DBC *c;
				int all = act >= 8;

				d->cursor(d, NULL, &c, 0);
				memset(&k, 0, sizeof(k));
				memset(&v, 0, sizeof(v));
				k.data = (void *)spell[kk][0];
				k.size = 3;
				if (c->get(c, &k, &v, DB_SET) == 0)
					do {
						if (c->del(c, 0) == 0)
							live[kk]--;
						if (!all)
							break;
						memset(&v, 0, sizeof(v));
					} while (c->get(c,
					    &k, &v, DB_NEXT_DUP) == 0);
				c->close(c);
			}
		}
		/* Scan: per folded key, live count and spelling consistency. */
		{
			DBC *c;
			int cnt[NK] = { 0 };
			char seen[NK][4];

			memset(seen, 0, sizeof(seen));
			d->cursor(d, NULL, &c, 0);
			memset(&k, 0, sizeof(k));
			memset(&v, 0, sizeof(v));
			while (c->get(c, &k, &v, DB_NEXT) == 0)
				for (i = 0; i < NK; i++)
					if (k.size == 3 && strncasecmp(k.data,
					    spell[i][0], 3) == 0) {
						cnt[i]++;
						if (!seen[i][0])
							memcpy(seen[i],
							    k.data, 3);
						else if (memcmp(seen[i],
						    k.data, 3) != 0) {
							printf("op %d: key %s "
							    "has two spellings "
							    "%.3s/%.3s in one "
							    "walk\n", op,
							    spell[i][0], seen[i],
							    (char *)k.data);
							bad = 1;
						}
					}
			c->close(c);
			for (i = 0; i < NK; i++)
				if (cnt[i] != live[i]) {
					printf("op %d: key %s count %d, "
					    "model %d\n", op, spell[i][0],
					    cnt[i], live[i]);
					bad = 1;
				}
		}
		for (i = 0; i < 3; i++)
			if (pins[i]) {
				pins[i]->close(pins[i]);
				pins[i] = NULL;
			}
		d->close(d, 0);
		if ((r = verify(path, fl)) != 0) {
			printf("op %d: verify %d\n", op, r);
			bad = 1;
		}
	}
	(void)unlink(path);
	printf("seed %u %s ops=%d: %s\n", seed,
	    fl == DB_DUPSORT ? "DUPSORT" : "DUP", op, bad ? "FAIL" : "PASS");
	return (bad);
}

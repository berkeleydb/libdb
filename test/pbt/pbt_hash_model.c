/*-
 * test/pbt/pbt_hash_model.c
 *	Stateful model test: a random put/del/get sequence on a real
 *	in-memory Berkeley DB DB_HASH database is compared, after every
 *	operation, against a simple in-test model (an array map).  This is
 *	the Tier-1 "model test" from the Hegel methodology: the highest-
 *	value first test for any data structure.
 *
 * The subject is a real DB_HASH (db_create + DB->open in-memory + put/
 * del/get).  The model is a plain key->value array with linear scan.
 * After each operation we assert both agree on presence and value for
 * the touched key.  DB_OVERWRITE_DUP is NOT used, so put replaces the
 * value for an existing key (the hash access method's default), matching
 * the model's overwrite semantics.
 *
 * Keys are drawn from a small pool of short byte strings so that
 * overwrites, deletes of present/absent keys, and re-inserts all occur
 * frequently -- that collision-heavy traffic is where map bugs hide.
 */

#include <string.h>

#include "db.h"

#include "pbt_common.h"

#if defined(PBT_HAVE_HEGEL)

#define POOL_KEYS 6	/* distinct keys the model/subject share */
#define MAX_VAL   32

struct model_ent {
	int      present;
	uint8_t  val[MAX_VAL];
	size_t   vlen;
};

/* Fixed key pool: index i -> a short, distinct key. */
static void
pool_key(int i, DBT *k, uint8_t buf[2])
{
	buf[0] = (uint8_t)('A' + i);
	buf[1] = (uint8_t)(0x40 + i);
	memset(k, 0, sizeof(*k));
	k->data = buf;
	k->size = 2;
}

static DB *
open_inmem_hash(void)
{
	DB *dbp = NULL;
	if (db_create(&dbp, NULL, 0) != 0)
		return (NULL);
	if (dbp->open(dbp, NULL, NULL, NULL, DB_HASH, DB_CREATE, 0600) != 0) {
		(void)dbp->close(dbp, 0);
		return (NULL);
	}
	return (dbp);
}

/* Assert the subject DB agrees with the model for key index ki. */
static int
check_key(DB *dbp, const struct model_ent *m, int ki)
{
	DBT k, out;
	uint8_t kbuf[2];
	int ret;

	pool_key(ki, &k, kbuf);
	memset(&out, 0, sizeof(out));
	ret = dbp->get(dbp, NULL, &k, &out, 0);
	if (m->present) {
		if (ret != 0)
			return (0);
		if (out.size != m->vlen)
			return (0);
		if (m->vlen != 0 && memcmp(out.data, m->val, m->vlen) != 0)
			return (0);
	} else {
		if (ret != DB_NOTFOUND)
			return (0);
	}
	return (1);
}

static void
prop_hash_matches_model(hegel_test_case *tc, void *u)
{
	DB *dbp;
	struct model_ent model[POOL_KEYS];
	int nops, i, ki, op;
	(void)u;

	dbp = open_inmem_hash();
	hegel_assume(dbp != NULL);
	memset(model, 0, sizeof(model));

	/* Draw a sequence length; let hegel explore long sequences. */
	nops = (int)hegel_draw_int(tc, hegel_integers(1, 200));

	for (i = 0; i < nops; i++) {
		DBT k;
		uint8_t kbuf[2];
		int ok;

		ki = (int)hegel_draw_int(tc, hegel_integers(0, POOL_KEYS - 1));
		op = (int)hegel_draw_int(tc, hegel_integers(0, 2)); /* 0 put 1 del 2 get */
		pool_key(ki, &k, kbuf);

		if (op == 0) {			/* put (overwrite) */
			DBT v;
			uint8_t vbuf[MAX_VAL];
			size_t vlen;
			int j;
			vlen = (size_t)hegel_draw_int(tc,
			    hegel_integers(0, MAX_VAL));
			for (j = 0; j < (int)vlen; j++)
				vbuf[j] = (uint8_t)hegel_draw_int(tc,
				    hegel_integers(0, 255));
			memset(&v, 0, sizeof(v));
			v.data = vbuf;
			v.size = (u_int32_t)vlen;
			PBT_CHECK(dbp->put(dbp, NULL, &k, &v, 0) == 0,
			    "put into DB_HASH failed");
			model[ki].present = 1;
			model[ki].vlen = vlen;
			if (vlen != 0)
				memcpy(model[ki].val, vbuf, vlen);
		} else if (op == 1) {		/* del */
			int ret = dbp->del(dbp, NULL, &k, 0);
			PBT_CHECK(ret == 0 || ret == DB_NOTFOUND,
			    "del returned an unexpected code");
			/* del must succeed iff key was present. */
			if (model[ki].present)
				PBT_CHECK(ret == 0,
				    "del of a present key did not succeed");
			else
				PBT_CHECK(ret == DB_NOTFOUND,
				    "del of an absent key was not DB_NOTFOUND");
			model[ki].present = 0;
			model[ki].vlen = 0;
		}
		/* op == 2 (get) is covered by the check below. */

		ok = check_key(dbp, &model[ki], ki);
		PBT_CHECK(ok, "DB_HASH disagrees with model after operation");
	}

	/* Final full agreement over the whole pool. */
	for (ki = 0; ki < POOL_KEYS; ki++)
		PBT_CHECK(check_key(dbp, &model[ki], ki),
		    "DB_HASH disagrees with model at end");

	(void)dbp->close(dbp, 0);
}

static const pbt_entry_t tests[] = {
	{ "hash_matches_model", prop_hash_matches_model, 200 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "hash_matches_model", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("hash_model", tests)

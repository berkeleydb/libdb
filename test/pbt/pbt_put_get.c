/*-
 * test/pbt/pbt_put_get.c
 *	End-to-end property test: put/get round-trip on a real in-memory
 *	Berkeley DB B-tree.  Exercises db_create + DB->open (in-memory,
 *	DB_CREATE, NULL filename -- the idiom from test/micro/source/
 *	b_inmem.c) + DB->put + DB->get across the full stack: btree,
 *	mpool, DBT plumbing.  All symbols are public (db.h) and linked.
 *
 * Property: for any key/value byte strings, storing then fetching the
 * key returns exactly the value that was stored (round-trip identity).
 * With DB_NOOVERWRITE we insert a fresh key each example, so the value
 * read back must byte-for-byte equal the value written.
 */

#include <string.h>

#include "db.h"

#include "pbt_common.h"

#if defined(PBT_HAVE_HEGEL)

/* Open a fresh in-memory B-tree.  Returns NULL on failure. */
static DB *
open_inmem_btree(void)
{
	DB *dbp = NULL;
	if (db_create(&dbp, NULL, 0) != 0)
		return (NULL);
	if (dbp->open(dbp, NULL, NULL, NULL, DB_BTREE, DB_CREATE, 0600) != 0) {
		(void)dbp->close(dbp, 0);
		return (NULL);
	}
	return (dbp);
}

static void
prop_put_get_roundtrip(hegel_test_case *tc, void *u)
{
	DB *dbp;
	DBT key, val, out;
	uint8_t *kbuf, *vbuf;
	size_t klen = 0, vlen = 0;
	int ret;
	(void)u;

	/* Non-empty key (BDB rejects zero-length keys); value may be empty. */
	kbuf = hegel_draw_bytes(tc, hegel_binary(1, 128), &klen);
	vbuf = hegel_draw_bytes(tc, hegel_binary(0, 512), &vlen);
	hegel_assume(kbuf != NULL);

	dbp = open_inmem_btree();
	hegel_assume(dbp != NULL);

	memset(&key, 0, sizeof(key));
	memset(&val, 0, sizeof(val));
	memset(&out, 0, sizeof(out));
	key.data = kbuf;
	key.size = (u_int32_t)klen;
	val.data = vbuf;
	val.size = (u_int32_t)vlen;

	ret = dbp->put(dbp, NULL, &key, &val, DB_NOOVERWRITE);
	hegel_assume(ret == 0);

	ret = dbp->get(dbp, NULL, &key, &out, 0);
	hegel_assume(ret == 0);

	/* Round-trip identity: same length and same bytes. */
	hegel_assume(out.size == val.size);
	hegel_assume(val.size == 0 || memcmp(out.data, val.data, val.size) == 0);

	(void)dbp->close(dbp, 0);
	free(kbuf);
	free(vbuf);
}

/* Absent key -> DB_NOTFOUND, never a spurious hit. */
static void
prop_get_missing_notfound(hegel_test_case *tc, void *u)
{
	DB *dbp;
	DBT key, out;
	uint8_t *kbuf;
	size_t klen = 0;
	int ret;
	(void)u;

	kbuf = hegel_draw_bytes(tc, hegel_binary(1, 128), &klen);
	hegel_assume(kbuf != NULL);

	dbp = open_inmem_btree();
	hegel_assume(dbp != NULL);

	memset(&key, 0, sizeof(key));
	memset(&out, 0, sizeof(out));
	key.data = kbuf;
	key.size = (u_int32_t)klen;

	ret = dbp->get(dbp, NULL, &key, &out, 0);
	hegel_assume(ret == DB_NOTFOUND);

	(void)dbp->close(dbp, 0);
	free(kbuf);
}

static const pbt_entry_t tests[] = {
	{ "put_get_roundtrip",   prop_put_get_roundtrip,   200 },
	{ "get_missing_notfound", prop_get_missing_notfound, 100 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "put_get_roundtrip",    NULL, 0 },
	{ "get_missing_notfound", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("put_get", tests)

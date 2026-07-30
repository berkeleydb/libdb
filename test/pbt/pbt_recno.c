/*-
 * test/pbt/pbt_recno.c
 *	End-to-end property tests for the recno (record-number) access
 *	method, src/btree/bt_recno.c, over a real in-memory DB_RECNO.
 *
 * Two contracts, both grounded in bt_recno.c and the recno design:
 *
 *   put_get_roundtrip -- appending a data item (DB_APPEND) assigns it the
 *	next record number, and getting that record number returns exactly
 *	the data stored (round-trip identity through the recno stack:
 *	__ram_append -> __ram_add -> __bam_iitem, and DB->get by record
 *	number).
 *
 *   renumber_contiguous -- with DB_RENUMBER set, deleting a record
 *	physically removes it and shifts every higher record down by one
 *	(bt_recno.c __ramc_del: "In renumbering recnos, ... adjust the
 *	counts, adjust the cursors"; __ram_ca(CA_DELETE)).  So after any
 *	sequence of N appends and D in-range deletes, the surviving records
 *	are numbered exactly 1..(N-D) with no gaps.  We verify this by
 *	walking the whole database with a cursor and DB_NEXT: the record
 *	numbers must come out 1, 2, 3, ... contiguously.
 *
 * All symbols used are public (db.h) and linked; this exercises the recno
 * access method the way the Tcl recno tests do, but with generated
 * insert/delete sequences and an explicit contiguity oracle.
 */

#include <string.h>

#include "db.h"

#include "pbt_common.h"

#if defined(PBT_HAVE_HEGEL)

/* Open a fresh in-memory DB_RECNO.  If renumber, set DB_RENUMBER first. */
static DB *
open_inmem_recno(int renumber)
{
	DB *dbp = NULL;

	if (db_create(&dbp, NULL, 0) != 0)
		return (NULL);
	if (renumber && dbp->set_flags(dbp, DB_RENUMBER) != 0) {
		(void)dbp->close(dbp, 0);
		return (NULL);
	}
	if (dbp->open(dbp, NULL, NULL, NULL, DB_RECNO, DB_CREATE, 0600) != 0) {
		(void)dbp->close(dbp, 0);
		return (NULL);
	}
	return (dbp);
}

/* Append one data item; return the assigned record number (0 on failure). */
static db_recno_t
recno_append(DB *dbp, const void *buf, size_t len)
{
	DBT key, data;
	db_recno_t rec = 0;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &rec;
	key.ulen = sizeof(rec);
	key.flags = DB_DBT_USERMEM;	/* DB_APPEND writes the recno here */
	data.data = (void *)buf;
	data.size = (u_int32_t)len;
	if (dbp->put(dbp, NULL, &key, &data, DB_APPEND) != 0)
		return (0);
	return (rec);
}

/*
 * P1: append/get round-trip -- a value appended at the returned record
 * number reads back byte-for-byte.
 */
static void
prop_put_get_roundtrip(hegel_test_case *tc, void *u)
{
	DB *dbp;
	DBT key, out;
	uint8_t *vbuf;
	size_t vlen = 0;
	db_recno_t rec;
	int ret;
	(void)u;

	/* Recno data is non-empty (a zero-length record is not stored). */
	vbuf = hegel_draw_bytes(tc, hegel_binary(1, 300), &vlen);
	hegel_assume(vbuf != NULL);

	dbp = open_inmem_recno(0);
	hegel_assume(dbp != NULL);

	rec = recno_append(dbp, vbuf, vlen);
	PBT_CHECK(rec != 0, "DB_APPEND did not assign a record number");

	memset(&key, 0, sizeof(key));
	memset(&out, 0, sizeof(out));
	key.data = &rec;
	key.size = sizeof(rec);
	ret = dbp->get(dbp, NULL, &key, &out, 0);
	PBT_CHECK(ret == 0, "get of appended record failed");
	PBT_CHECK(out.size == (u_int32_t)vlen, "appended record read back wrong size");
	PBT_CHECK(memcmp(out.data, vbuf, vlen) == 0,
	    "appended record read back wrong bytes");

	(void)dbp->close(dbp, 0);
	free(vbuf);
}

/*
 * P2: renumber contiguity -- after N appends and a set of deletes, the
 * live records are numbered exactly 1..M with no gaps.  We drive it with
 * a generated number of appends and a generated list of delete positions,
 * then verify by cursor walk.
 */
static void
prop_renumber_contiguous(hegel_test_case *tc, void *u)
{
	DB *dbp;
	DBC *dbc;
	DBT key, data;
	db_recno_t rec, walk, expected;
	int n, ndel, i, live, ret;
	uint8_t payload[4] = { 0xDE, 0xAD, 0xBE, 0xEF };
	(void)u;

	dbp = open_inmem_recno(1 /* DB_RENUMBER */);
	hegel_assume(dbp != NULL);

	/* Append n records (1..n). */
	n = (int)hegel_draw_int(tc, hegel_integers(1, 30));
	for (i = 0; i < n; i++)
		PBT_CHECK(recno_append(dbp, payload, sizeof(payload)) != 0,
		    "append during renumber setup failed");
	live = n;

	/* Delete ndel records, each at a currently-valid position. */
	ndel = (int)hegel_draw_int(tc, hegel_integers(0, n));
	for (i = 0; i < ndel && live > 0; i++) {
		rec = (db_recno_t)hegel_draw_int(tc, hegel_integers(1, live));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &rec;
		key.size = sizeof(rec);
		ret = dbp->del(dbp, NULL, &key, 0);
		PBT_CHECK(ret == 0, "del of an in-range record failed");
		live--;
	}

	/* Walk every live record; recnos must be 1, 2, 3, ... contiguously. */
	PBT_CHECK(dbp->cursor(dbp, NULL, &dbc, 0) == 0, "cursor open failed");
	expected = 1;
	for (;;) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		ret = dbc->get(dbc, &key, &data, DB_NEXT);
		if (ret == DB_NOTFOUND)
			break;
		PBT_CHECK(ret == 0, "cursor DB_NEXT failed");
		PBT_CHECK(key.size == sizeof(walk), "cursor recno key wrong size");
		memcpy(&walk, key.data, sizeof(walk));
		PBT_CHECK(walk == expected, "renumber left a gap or wrong order");
		expected++;
	}
	/* Exactly `live` records survived, numbered 1..live. */
	PBT_CHECK((int)(expected - 1) == live,
	    "surviving record count != N - deletes");

	(void)dbc->close(dbc);
	(void)dbp->close(dbp, 0);
}

static const pbt_entry_t tests[] = {
	{ "put_get_roundtrip",   prop_put_get_roundtrip,   200 },
	{ "renumber_contiguous", prop_renumber_contiguous, 200 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "put_get_roundtrip",   NULL, 0 },
	{ "renumber_contiguous", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("recno", tests)

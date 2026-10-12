/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_gen.c --
 *	RFC 0014 typed data and constraints: the generated bt_compare /
 *	h_hash / h_compare / associate callbacks, and db_record_fixed.
 *
 * Whole-file guarded on HAVE_TYPED (--enable-typed / meson -Dtyped=enabled),
 * the os_aio_uring.c pattern: the file is in every build's source list and
 * compiles to nothing unless the option is on.
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/btree.h"
#include "dbinc/hash.h"
#include "typed_int.h"

#ifdef HAVE_TYPED
/*
 * db_record_fixed_field --
 *	The field() callback db_record_fixed installs: field i is at
 *	(rec + off[i], len[i]).  Total and bounded by rec->size; a slice that
 *	runs off the end makes the record malformed.  off[]/len[] are the two
 *	app-owned arrays stashed after the DB_RECORD_CODEC via app_private
 *	(see db_record_fixed); isnull is always 0 for a fixed layout.
 */
struct __fixed_layout {
	const u_int32_t *off;
	const u_int32_t *len;
};

static int
db_record_fixed_field(rc, rec, i, pp, lenp, isnull)
	const DB_RECORD_CODEC *rc;
	const DBT *rec;
	u_int32_t i;
	const u_int8_t **pp;
	u_int32_t *lenp;
	int *isnull;
{
	const struct __fixed_layout *lay;
	u_int32_t off, len;

	lay = rc->app_private;
	if (i >= rc->nfields)
		return (DB_CODEC_MALFORMED);
	off = lay->off[i];
	len = lay->len[i];
	/* Overflow-safe: len > size || off > size - len. */
	if (len > rec->size || off > rec->size - len)
		return (DB_CODEC_MALFORMED);
	*pp = (const u_int8_t *)rec->data + off;
	*lenp = len;
	*isnull = 0;
	return (0);
}

/*
 * db_record_fixed --
 *	Fill an application-owned DB_RECORD_CODEC describing a fixed layout
 *	(spec §1.5): field i is (rec + off[i], len[i]).  off[]/len[]/types[]
 *	are referenced, not copied (same lifetime contract as any callback's
 *	data).  app_private holds a small heap block carrying off/len to
 *	field(); it is owned by the codec and freed by the application, like
 *	any app_private.
 */
int
db_record_fixed(rc, name, version, nfields, off, len, types)
	DB_RECORD_CODEC *rc;
	const char *name;
	u_int32_t version, nfields;
	const u_int32_t *off, *len;
	const DB_CODEC **types;
{
	struct __fixed_layout *lay;

	if (rc == NULL || name == NULL || off == NULL || len == NULL ||
	    types == NULL || nfields < 1 || nfields > 65535)
		return (EINVAL);

	/*
	 * app_private carries off[]/len[] to field().  __os_malloc(NULL)
	 * uses the global allocator; the block references the caller's off/len
	 * arrays (not copied), so both must outlive the codec, same contract
	 * as types[].
	 */
	if (__os_malloc(NULL, sizeof(*lay), &lay) != 0)
		return (ENOMEM);
	lay->off = off;
	lay->len = len;

	memset(rc, 0, sizeof(*rc));
	rc->name = name;
	rc->version = version;
	rc->flags = DB_CODEC_TOTAL;
	rc->nfields = nfields;
	rc->field = db_record_fixed_field;
	rc->validate = NULL;
	rc->types = types;
	rc->app_private = lay;
	return (0);
}

/*
 * __typed_walk_field --
 *	Locate field i of a record with the record codec, applying the one
 *	bounds check libdb can make (spec §6.3): the returned (p, len) slice
 *	must lie inside rec, whatever a lying field() claims.  Returns 0 and
 *	fills the returned (pp, lenp, isnull) on a well-formed field; returns
 *	non-zero (the codec's error, or DB_CODEC_MALFORMED if the slice runs
 *	off the end) to mean "treat the record as malformed".  This turns a
 *	lying field() into a misordering, not an out-of-bounds read by a type
 *	codec; it cannot stop field() itself from faulting -- only
 *	DB_CODEC_TOTAL does.
 */
static int
__typed_walk_field(rc, rec, i, pp, lenp, isnull)
	const DB_RECORD_CODEC *rc;
	const DBT *rec;
	u_int32_t i;
	const u_int8_t **pp;
	u_int32_t *lenp;
	int *isnull;
{
	const u_int8_t *d, *p;
	u_int32_t len, size;
	int ret;

	*pp = NULL; *lenp = 0; *isnull = 0;
	if ((ret = rc->field(rc, rec, i, pp, lenp, isnull)) != 0)
		return (ret);
	if (*isnull)
		return (0);
	/*
	 * Overflow-safe (spec §6.3): p within [d, d+size], len within the
	 * rest.  A field() that returns a slice outside rec makes the record
	 * malformed; the type codec never sees those bytes.
	 */
	d = rec->data;
	size = rec->size;
	p = *pp;
	len = *lenp;
	if (p < d || (size_t)(p - d) > size || len > size - (size_t)(p - d))
		return (DB_CODEC_MALFORMED);
	return (0);
}

/*
 * __typed_rec_cmp --
 *	Compare two typed records field by field with the key record codec
 *	(spec §3.6, the RFC's bounded-walk comparator).  Semantics:
 *	  - a record whose field() reports malformed sorts AFTER every
 *	    well-formed record;
 *	  - two malformed records compare by memcmp of their raw bytes, then
 *	    by length (a total order over arbitrary bytes);
 *	  - otherwise, field by field: NULL sorts before non-NULL; a MEMCMP
 *	    field compares by memcmp of the common prefix then shorter-is-less;
 *	    any other field compares with its codec's compare().
 *	Total over arbitrary bytes when every codec is DB_CODEC_TOTAL (the
 *	DB_OPTREAD contract, gated in __typed_open).
 */
static int
__typed_rec_cmp(rc, a, b)
	const DB_RECORD_CODEC *rc;
	const DBT *a, *b;
{
	const DB_CODEC *fc;
	const u_int8_t *pa, *pb;
	u_int32_t i, la, lb, min;
	int na, nb, ia, ib, c;

	for (i = 0; i < rc->nfields; i++) {
		na = __typed_walk_field(rc, a, i, &pa, &la, &ia);
		nb = __typed_walk_field(rc, b, i, &pb, &lb, &ib);
		/* Malformed sorts last; two malformed -> raw memcmp+length. */
		if (na != 0 || nb != 0) {
			if (na != 0 && nb != 0) {
				min = a->size < b->size ? a->size : b->size;
				if (min != 0 &&
				    (c = memcmp(a->data, b->data, min)) != 0)
					return (c);
				return (a->size < b->size ? -1 :
				    a->size > b->size ? 1 : 0);
			}
			return (na != 0 ? 1 : -1);
		}
		/* NULL sorts before non-NULL. */
		if (ia || ib) {
			if (ia && ib)
				continue;
			return (ia ? -1 : 1);
		}
		fc = rc->types[i];
		if (FLD_ISSET(fc->flags, DB_CODEC_MEMCMP) ||
		    fc->compare == NULL) {
			/* MEMCMP: common prefix, then shorter-is-less. */
			min = la < lb ? la : lb;
			if (min != 0 && (c = memcmp(pa, pb, min)) != 0)
				return (c);
			if (la != lb)
				return (la < lb ? -1 : 1);
		} else if ((c = fc->compare(fc, pa, la, pb, lb)) != 0)
			return (c);
	}
	return (0);
}

/*
 * __typed_bt_compare --
 *	The generated bt_compare installed on a non-MEMCMP typed Btree key
 *	(spec §3.6).  State comes from the handle's side-table slot; if the
 *	slot is 0 (an internal path cloned the pointer onto a fresh handle) it
 *	DIAGNOSTIC-asserts and falls back to __bam_defcmp's byte order, so it
 *	can misorder but cannot fault.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_bt_compare __P((DB *, const DBT *, const DBT *));
 * PUBLIC: #endif
 */
int
__typed_bt_compare(dbp, a, b)
	DB *dbp;
	const DBT *a, *b;
{
	TYPED_STATE *ts;

	if ((ts = __typed_state(dbp)) == NULL || ts->schema.key == NULL) {
		DB_ASSERT(dbp->env, ts != NULL && ts->schema.key != NULL);
		return (__bam_defcmp(dbp, a, b));
	}
	return (__typed_rec_cmp(ts->schema.key, a, b));
}

/*
 * __typed_h_hash --
 *	The generated h_hash installed on a non-EQUALIMAGE typed Hash key
 *	(spec §3.6).  Hashes each key field's bytes with its codec's hash()
 *	(or __ham_func5 of the raw bytes for an EQUALIMAGE/MEMCMP field that
 *	has no hash), then mixes the per-field results through __ham_func5 so
 *	the field split is part of the hash.  A malformed record falls back to
 *	hashing the raw bytes, matching the comparator's total order (a
 *	malformed record is still placed deterministically).
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: u_int32_t __typed_h_hash __P((DB *, const void *, u_int32_t));
 * PUBLIC: #endif
 */
u_int32_t
__typed_h_hash(dbp, key, len)
	DB *dbp;
	const void *key;
	u_int32_t len;
{
	TYPED_STATE *ts;
	const DB_RECORD_CODEC *rc;
	const DB_CODEC *fc;
	const u_int8_t *p;
	DBT rec;
	u_int32_t i, flen, h, fh;
	int isnull;

	if ((ts = __typed_state(dbp)) == NULL || ts->schema.key == NULL) {
		DB_ASSERT(dbp->env, ts != NULL && ts->schema.key != NULL);
		return (__ham_func5(dbp, key, len));
	}
	rc = ts->schema.key;
	memset(&rec, 0, sizeof(rec));
	rec.data = (void *)key;
	rec.size = len;

	h = 0;
	for (i = 0; i < rc->nfields; i++) {
		if (__typed_walk_field(rc, &rec, i, &p, &flen, &isnull) != 0)
			return (__ham_func5(dbp, key, len));
		if (isnull)
			fh = 0;
		else {
			fc = rc->types[i];
			fh = fc->hash != NULL ? fc->hash(fc, p, flen) :
			    __ham_func5(dbp, p, flen);
		}
		/* Mix the field result in; the index i keeps the split part
		 * of the hash, so {"ab",""} and {"a","b"} differ. */
		h = __ham_func5(dbp, &fh, sizeof(fh)) ^ (h * 16777619u + i);
	}
	return (h);
}

/*
 * __typed_h_compare --
 *	The generated h_compare installed with __typed_h_hash (spec §3.6):
 *	0 iff every key field's equal / compare()==0 holds.  Hash equality is
 *	not enough -- two distinct values can collide -- so the bucket chain
 *	needs this exact equality to find the record.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_h_compare __P((DB *, const DBT *, const DBT *));
 * PUBLIC: #endif
 */
int
__typed_h_compare(dbp, a, b)
	DB *dbp;
	const DBT *a, *b;
{
	TYPED_STATE *ts;

	if ((ts = __typed_state(dbp)) == NULL || ts->schema.key == NULL) {
		DB_ASSERT(dbp->env, ts != NULL && ts->schema.key != NULL);
		return (__bam_defcmp(dbp, a, b));
	}
	return (__typed_rec_cmp(ts->schema.key, a, b));
}

/*
 * __typed_unique_cb --
 *	The generated secondary-index callback for a UNIQUE constraint (spec
 *	§3.6).  Installed by __typed_open when it associates the constraint's
 *	unique_db to the primary.  Extracts the constrained data field and
 *	maps it to a secondary key:
 *	  - a NULL field with NULLS DISTINCT -> DB_DONOTINDEX (no index entry);
 *	  - a NULL field with NULLS_NOT_DISTINCT -> a one-byte 0x00 key (so the
 *	    second NULL collides), while every non-NULL image is prefixed 0x01
 *	    (so a non-NULL value can never equal the NULL image);
 *	  - EQUALIMAGE / MEMCMP -> the field bytes themselves (no copy, unless
 *	    a 0x01 prefix is needed for NULLS_NOT_DISTINCT);
 *	  - otherwise -> the codec's sortkey() output in __os_umalloc memory
 *	    flagged DB_DBT_APPMALLOC.
 *	A DB_CODEC_MALFORMED from the codec becomes DB_CONSTRAINT_VIOLATION,
 *	which fails the put through the existing callback-veto path.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_unique_cb __P((DB *, const DBT *, const DBT *, DBT *));
 * PUBLIC: #endif
 */
int
__typed_unique_cb(sdbp, pkey, pdata, skey)
	DB *sdbp;
	const DBT *pkey, *pdata;
	DBT *skey;
{
	ENV *env;
	DB *pdbp;
	TYPED_STATE *ts;
	const DB_CONSTRAINT *con;
	const DB_RECORD_CODEC *rc;
	const DB_CODEC *fc;
	const u_int8_t *p;
	u_int8_t *out;
	u_int32_t i, flen, need, nndist, prefixed;
	int isnull, ret;

	COMPQUIET(pkey, NULL);
	pdbp = sdbp->s_primary;
	if (pdbp == NULL || (ts = __typed_state(pdbp)) == NULL)
		return (EINVAL);
	env = sdbp->env;

	/* The constraint whose unique_db is this secondary. */
	con = NULL;
	for (i = 0; i < ts->schema.nconstraints; i++)
		if (ts->schema.constraints[i].kind == DB_CONSTRAINT_UNIQUE &&
		    ts->schema.constraints[i].unique_db == sdbp) {
			con = &ts->schema.constraints[i];
			break;
		}
	if (con == NULL)
		return (EINVAL);
	rc = ts->schema.data;
	if (rc == NULL)
		return (EINVAL);
	nndist = FLD_ISSET(con->flags, DB_CONSTRAINT_NULLS_NOT_DISTINCT);

	/* Locate the constrained field in the primary data. */
	p = NULL; flen = 0; isnull = 0;
	if ((ret = rc->field(rc, pdata, con->field, &p, &flen, &isnull)) != 0) {
		if (ret == DB_CODEC_MALFORMED)
			return (DB_CONSTRAINT_VIOLATION);
		return (ret);
	}

	memset(skey, 0, sizeof(*skey));

	if (isnull) {
		if (!nndist)
			return (DB_DONOTINDEX);	/* NULLS DISTINCT */
		/* NULLS_NOT_DISTINCT: a single 0x00-byte key, so two NULLs
		 * collide.  Non-NULL images are 0x01-prefixed below, so a
		 * non-NULL value can never equal this. */
		if ((ret = __os_umalloc(env, 1, &out)) != 0)
			return (ret);
		out[0] = 0x00;
		skey->data = out; skey->size = 1;
		F_SET(skey, DB_DBT_APPMALLOC);
		return (0);
	}

	fc = rc->types[con->field];
	prefixed = nndist ? 1 : 0;

	if (FLD_ISSET(fc->flags, DB_CODEC_EQUALIMAGE | DB_CODEC_MEMCMP)) {
		/* Field bytes are the key.  No copy unless a prefix is needed. */
		if (!prefixed) {
			skey->data = (void *)p;
			skey->size = flen;
			return (0);
		}
		if ((ret = __os_umalloc(env, flen + 1, &out)) != 0)
			return (ret);
		out[0] = 0x01;
		if (flen > 0)
			memcpy(out + 1, p, flen);
		skey->data = out; skey->size = flen + 1;
		F_SET(skey, DB_DBT_APPMALLOC);
		return (0);
	}

	/* sortkey(): two-call protocol, output in __os_umalloc memory. */
	need = 0;
	if ((ret = fc->sortkey(fc, p, flen, NULL, 0, &need)) != 0 &&
	    ret != DB_BUFFER_SMALL) {
		if (ret == DB_CODEC_MALFORMED)
			return (DB_CONSTRAINT_VIOLATION);
		return (ret);
	}
	if ((ret = __os_umalloc(env, need + prefixed, &out)) != 0)
		return (ret);
	if (prefixed)
		out[0] = 0x01;
	if (need > 0 && (ret = fc->sortkey(fc, p, flen, out + prefixed,
	    need, &need)) != 0) {
		__os_ufree(env, out);
		if (ret == DB_CODEC_MALFORMED)
			return (DB_CONSTRAINT_VIOLATION);
		return (ret);
	}
	skey->data = out; skey->size = need + prefixed;
	F_SET(skey, DB_DBT_APPMALLOC);
	return (0);
}
#endif /* HAVE_TYPED */

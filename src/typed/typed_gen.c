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

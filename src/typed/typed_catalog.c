/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_catalog.c --
 *	RFC 0014 typed data and constraints: the __db_typed catalog (D3)
 *	and the codec fingerprint.
 *
 * Whole-file guarded on HAVE_TYPED (--enable-typed / meson -Dtyped=enabled),
 * the os_aio_uring.c pattern: the file is in every build's source list and
 * compiles to nothing unless the option is on.
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/db_am.h"
#include "dbinc/hmac.h"
#include "typed_int.h"

#ifdef HAVE_TYPED
static void __typed_sha_u32 __P((SHA1_CTX *, u_int32_t));

/*
 * __typed_sha_u32 --
 *	Feed a big-endian u32 to the running SHA-1, so a fingerprint is
 *	byte-order independent (the catalog is; the meta page is not, §4.2).
 */
static void
__typed_sha_u32(ctx, v)
	SHA1_CTX *ctx;
	u_int32_t v;
{
	unsigned char b[4];

	b[0] = (unsigned char)(v >> 24);
	b[1] = (unsigned char)(v >> 16);
	b[2] = (unsigned char)(v >> 8);
	b[3] = (unsigned char)v;
	__db_SHA1Update(ctx, b, sizeof(b));
}

/*
 * __typed_fingerprint --
 *	Compute the 20-byte SHA-1 fingerprint of one DB_CODEC over the byte
 *	stream of spec §5.1, and run the §5.1 self-checks against the codec's
 *	own golden vectors.  Returns EINVAL (naming the codec and the vector
 *	indices) if any self-check fails; the fingerprint proves only that two
 *	codecs agree on their vectors (the stated limit).
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_fingerprint __P((DB *, const DB_CODEC *, u_int8_t *));
 * PUBLIC: #endif
 */
int
__typed_fingerprint(dbp, c, out)
	DB *dbp;
	const DB_CODEC *c;
	u_int8_t *out;
{
	ENV *env;
	SHA1_CTX ctx;
	const DBT *vi, *vj;
	u_int32_t flags, i, j, h, maxneed, need;
	u_int8_t *sbuf;
	int cmp, cmp2, memc, memc2, ret;

	env = dbp->env;
	ret = 0;
	sbuf = NULL;

	/* C11 normalisation: MEMCMP implies EQUALIMAGE in the identity. */
	flags = c->flags;
	if (FLD_ISSET(flags, DB_CODEC_MEMCMP))
		flags |= DB_CODEC_EQUALIMAGE;

	__db_SHA1Init(&ctx);

	/* (1) name, version, flags, fixed_len, nvectors. */
	__db_SHA1Update(&ctx, (unsigned char *)c->name,
	    (size_t)strlen(c->name));
	__typed_sha_u32(&ctx, c->version);
	__typed_sha_u32(&ctx, flags);
	__typed_sha_u32(&ctx, c->fixed_len);
	__typed_sha_u32(&ctx, c->nvectors);

	/* (2) each vector: size then bytes. */
	for (i = 0; i < c->nvectors; i++) {
		vi = &c->vectors[i];
		__typed_sha_u32(&ctx, vi->size);
		if (vi->size > 0)
			__db_SHA1Update(&ctx,
			    (unsigned char *)vi->data, (size_t)vi->size);
	}

	/*
	 * (3) sign(compare(v_i,v_j)) for every ordered pair; (self-check)
	 * antisymmetry, non-decreasing order, MEMCMP/EQUALIMAGE claims.
	 */
	for (i = 0; i < c->nvectors; i++) {
		vi = &c->vectors[i];
		for (j = 0; j < c->nvectors; j++) {
			vj = &c->vectors[j];
			cmp = __typed_codec_cmp(c, vi, vj);
			__typed_sha_u32(&ctx, (u_int32_t)(cmp < 0 ? -1 :
			    (cmp > 0 ? 1 : 0)));
		}
	}
	for (i = 0; i < c->nvectors; i++) {
		vi = &c->vectors[i];
		for (j = i + 1; j < c->nvectors; j++) {
			vj = &c->vectors[j];
			cmp = __typed_codec_cmp(c, vi, vj);
			cmp2 = __typed_codec_cmp(c, vj, vi);
			/* antisymmetry */
			if ((cmp < 0 ? -1 : cmp > 0 ? 1 : 0) !=
			    -(cmp2 < 0 ? -1 : cmp2 > 0 ? 1 : 0)) {
				__db_errx(env, DB_STR("5353",
	    "typed codec %s: compare is not antisymmetric on vectors %lu,%lu"),
				    c->name, (u_long)i, (u_long)j);
				ret = EINVAL;
				goto done;
			}
			/* non-decreasing order (RFC's ordered list) */
			if (cmp > 0) {
				__db_errx(env, DB_STR("5354",
	    "typed codec %s: vectors %lu,%lu are not in compare order"),
				    c->name, (u_long)i, (u_long)j);
				ret = EINVAL;
				goto done;
			}
			/* MEMCMP: the default byte order must match compare. */
			if (FLD_ISSET(c->flags, DB_CODEC_MEMCMP)) {
				memc = __typed_memcmp_order(vi, vj);
				if ((cmp < 0 ? -1 : cmp > 0 ? 1 : 0) !=
				    (memc < 0 ? -1 : memc > 0 ? 1 : 0)) {
					__db_errx(env, DB_STR("5355",
	    "typed codec %s: MEMCMP claimed but order differs on %lu,%lu"),
					    c->name, (u_long)i, (u_long)j);
					ret = EINVAL;
					goto done;
				}
			}
			/* EQUALIMAGE: compare==0 iff byte-identical. */
			if (FLD_ISSET(c->flags, DB_CODEC_EQUALIMAGE)) {
				memc2 = __typed_memcmp_order(vi, vj);
				if ((cmp == 0) != (memc2 == 0)) {
					__db_errx(env, DB_STR("5356",
	    "typed codec %s: EQUALIMAGE claimed but violated on %lu,%lu"),
					    c->name, (u_long)i, (u_long)j);
					ret = EINVAL;
					goto done;
				}
			}
		}
	}

	/*
	 * (4) equal(v_i,v_j) for i<j, and the self-check that hash agrees with
	 * equal.  equal defaults to compare()==0.
	 */
	for (i = 0; i < c->nvectors; i++) {
		vi = &c->vectors[i];
		for (j = i + 1; j < c->nvectors; j++) {
			int eq;

			vj = &c->vectors[j];
			if (c->equal != NULL)
				eq = c->equal(c, (const u_int8_t *)vi->data,
				    vi->size, (const u_int8_t *)vj->data,
				    vj->size) != 0;
			else
				eq = __typed_codec_cmp(c, vi, vj) == 0;
			__typed_sha_u32(&ctx, (u_int32_t)eq);
			if (eq && c->hash != NULL) {
				u_int32_t hi, hj;

				hi = c->hash(c, (const u_int8_t *)vi->data,
				    vi->size);
				hj = c->hash(c, (const u_int8_t *)vj->data,
				    vj->size);
				if (hi != hj) {
					__db_errx(env, DB_STR("5357",
	    "typed codec %s: hash differs on equal vectors %lu,%lu"),
					    c->name, (u_long)i, (u_long)j);
					ret = EINVAL;
					goto done;
				}
			}
		}
	}

	/* (5) per vector: hash, validate==0, and the sortkey output. */
	maxneed = 0;
	for (i = 0; i < c->nvectors; i++) {
		vi = &c->vectors[i];
		if (c->hash != NULL) {
			h = c->hash(c, (const u_int8_t *)vi->data, vi->size);
			__typed_sha_u32(&ctx, h);
		}
		if (c->validate != NULL)
			__typed_sha_u32(&ctx, (u_int32_t)(c->validate(c,
			    (const u_int8_t *)vi->data, vi->size) == 0));
		if (c->sortkey != NULL) {
			need = 0;
			(void)c->sortkey(c, (const u_int8_t *)vi->data,
			    vi->size, NULL, 0, &need);
			if (need > maxneed)
				maxneed = need;
		}
	}
	if (c->sortkey != NULL && maxneed > 0 &&
	    (ret = __os_malloc(env, maxneed, &sbuf)) != 0)
		goto done;
	if (c->sortkey != NULL) {
		for (i = 0; i < c->nvectors; i++) {
			vi = &c->vectors[i];
			need = 0;
			if ((ret = c->sortkey(c, (const u_int8_t *)vi->data,
			    vi->size, sbuf, maxneed, &need)) != 0) {
				__db_errx(env, DB_STR("5358",
	    "typed codec %s: sortkey failed on vector %lu"),
				    c->name, (u_long)i);
				ret = EINVAL;
				goto done;
			}
			__typed_sha_u32(&ctx, need);
			if (need > 0)
				__db_SHA1Update(&ctx, sbuf, (size_t)need);
		}
	}

	__db_SHA1Final(out, &ctx);

done:	if (sbuf != NULL)
		__os_free(env, sbuf);
	return (ret);
}

/*
 * __typed_memcmp_order --
 *	The default bytewise order: memcmp over the shorter length, then
 *	shorter-is-less.  Used for MEMCMP codecs with no compare and for the
 *	MEMCMP/EQUALIMAGE self-checks.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_memcmp_order __P((const DBT *, const DBT *));
 * PUBLIC: #endif
 */
int
__typed_memcmp_order(a, b)
	const DBT *a, *b;
{
	u_int32_t n;
	int r;

	n = a->size < b->size ? a->size : b->size;
	if (n > 0 && (r = memcmp(a->data, b->data, (size_t)n)) != 0)
		return (r);
	if (a->size < b->size)
		return (-1);
	if (a->size > b->size)
		return (1);
	return (0);
}

/*
 * __typed_codec_cmp --
 *	Compare two DBT values with a codec: its compare() if set, else the
 *	default bytewise order (a MEMCMP codec may leave compare NULL).
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_codec_cmp __P((const DB_CODEC *, const DBT *,
 * PUBLIC:     const DBT *));
 * PUBLIC: #endif
 */
int
__typed_codec_cmp(c, a, b)
	const DB_CODEC *c;
	const DBT *a, *b;
{
	if (c->compare != NULL)
		return (c->compare(c, (const u_int8_t *)a->data, a->size,
		    (const u_int8_t *)b->data, b->size));
	return (__typed_memcmp_order(a, b));
}

/*
 * The __db_typed catalog (spec §4.2-§4.3, §5.2).  One record per typed
 * subdatabase, keyed by the subdatabase name, in the same file (D3, C6).
 * All integers big-endian so the catalog is byte-order independent.
 */
#define	TYPED_CAT_MAGIC		0x44425459U	/* "DBTY" */
#define	TYPED_CAT_VERSION	1U
#define	TYPED_CAT_SUBDB		"__db_typed"

/* Big-endian put/get into a byte cursor. */
static void
cput32(u_int8_t **pp, u_int32_t v)
{
	(*pp)[0] = (u_int8_t)(v >> 24); (*pp)[1] = (u_int8_t)(v >> 16);
	(*pp)[2] = (u_int8_t)(v >> 8);  (*pp)[3] = (u_int8_t)v;
	*pp += 4;
}
static void
cput16(u_int8_t **pp, u_int32_t v)
{
	(*pp)[0] = (u_int8_t)(v >> 8); (*pp)[1] = (u_int8_t)v; *pp += 2;
}
static void
cput8(u_int8_t **pp, u_int32_t v)
{
	(*pp)[0] = (u_int8_t)v; *pp += 1;
}
static void
cputs(u_int8_t **pp, const char *s)
{
	size_t n = strlen(s);
	cput8(pp, (u_int32_t)n);
	memcpy(*pp, s, n);
	*pp += n;
}

/* Bounded big-endian get; sets *okp = 0 on short read. */
static u_int32_t
cget(const u_int8_t **pp, const u_int8_t *end, int width, int *okp)
{
	u_int32_t v = 0;
	int i;
	if (*pp + width > end) { *okp = 0; return (0); }
	for (i = 0; i < width; i++)
		v = (v << 8) | (*pp)[i];
	*pp += width;
	return (v);
}

/*
 * __typed_codec_index --
 *	The position of codec c in ts->codecs[], the catalog's codec order.
 */
static u_int32_t
__typed_codec_index(TYPED_STATE *ts, const DB_CODEC *c)
{
	u_int32_t i;
	for (i = 0; i < ts->ncodecs; i++)
		if (ts->codecs[i] == c)
			return (i);
	return (0);	/* collect_codecs guarantees c is present */
}

/*
 * __typed_catalog_encode --
 *	Build the §4.2 record for ts into a freshly malloc'd buffer.
 */
static int
__typed_catalog_encode(dbp, ts, out, lenp)
	DB *dbp;
	TYPED_STATE *ts;
	u_int8_t **out;
	u_int32_t *lenp;
{
	ENV *env;
	const DB_RECORD_CODEC *rc;
	const DB_CONSTRAINT *con;
	u_int8_t *buf, *p;
	size_t sz;
	u_int32_t i, s, f;
	int ret;

	env = dbp->env;

	/* Upper bound on the size. */
	sz = 64;
	for (i = 0; i < ts->ncodecs; i++)
		sz += 1 + strlen(ts->codecs[i]->name) + 12 + 20;
	for (s = 0; s < 2; s++) {
		rc = (s == 0) ? ts->schema.key : ts->schema.data;
		if (rc != NULL)
			sz += 1 + 1 + strlen(rc->name) + 12 +
			    (size_t)rc->nfields * 2;
		else
			sz += 1;
	}
	sz += 2;
	for (i = 0; i < ts->schema.nconstraints; i++)
		sz += 20 + 1 + strlen(ts->schema.constraints[i].name);

	if ((ret = __os_malloc(env, sz, &buf)) != 0)
		return (ret);
	p = buf;

	cput32(&p, TYPED_CAT_MAGIC);
	cput32(&p, TYPED_CAT_VERSION);
	cput32(&p, 0);			/* total length, patched below */
	cput32(&p, (u_int32_t)ts->schema.type);
	cput16(&p, ts->ncodecs);
	for (i = 0; i < ts->ncodecs; i++) {
		const DB_CODEC *c = ts->codecs[i];
		f = c->flags;
		if (FLD_ISSET(f, DB_CODEC_MEMCMP))
			f |= DB_CODEC_EQUALIMAGE;	/* C11 */
		cputs(&p, c->name);
		cput32(&p, c->version);
		cput32(&p, f);
		cput32(&p, c->fixed_len);
		memcpy(p, ts->fp[i], 20); p += 20;
	}
	for (s = 0; s < 2; s++) {
		rc = (s == 0) ? ts->schema.key : ts->schema.data;
		if (rc == NULL) { cput8(&p, 0); continue; }
		cput8(&p, 1);
		cputs(&p, rc->name);
		cput32(&p, rc->version);
		cput32(&p, rc->flags);
		cput32(&p, rc->nfields);
		for (i = 0; i < rc->nfields; i++)
			cput16(&p, __typed_codec_index(ts, rc->types[i]));
	}
	cput16(&p, ts->schema.nconstraints);
	for (i = 0; i < ts->schema.nconstraints; i++) {
		con = &ts->schema.constraints[i];
		cput32(&p, con->kind);
		cput32(&p, con->side);
		cput32(&p, con->field);
		cput32(&p, con->maxlen);
		cput32(&p, con->flags);
		cputs(&p, con->name);
	}

	*lenp = (u_int32_t)(p - buf);
	/* Patch the total length at offset 8. */
	buf[8] = (u_int8_t)(*lenp >> 24); buf[9] = (u_int8_t)(*lenp >> 16);
	buf[10] = (u_int8_t)(*lenp >> 8); buf[11] = (u_int8_t)*lenp;
	*out = buf;
	return (0);
}

/*
 * __typed_catalog_open --
 *	Open (optionally creating) the __db_typed subdatabase in fname.  The
 *	handle is a plain Btree -- it has no schema and no typed bit, so
 *	__typed_open does not recurse into it.
 */
static int
__typed_catalog_open(dbp, ip, txn, fname, create, cdbpp)
	DB *dbp;
	DB_THREAD_INFO *ip;
	DB_TXN *txn;
	const char *fname;
	int create;
	DB **cdbpp;
{
	DB *cdbp;
	u_int32_t oflags;
	int ret;

	*cdbpp = NULL;
	if ((ret = __db_create_internal(&cdbp, dbp->env, 0)) != 0)
		return (ret);
	if (F_ISSET(dbp, DB_AM_NOT_DURABLE) &&
	    (ret = __db_set_flags(cdbp, DB_TXN_NOT_DURABLE)) != 0) {
		(void)__db_close(cdbp, txn, DB_NOSYNC);
		return (ret);
	}
	oflags = create ? DB_CREATE : 0;
	if (F_ISSET(dbp, DB_AM_RDONLY))
		oflags |= DB_RDONLY;
	if ((ret = __db_open(cdbp, ip, txn, fname, TYPED_CAT_SUBDB,
	    DB_BTREE, oflags, 0, PGNO_BASE_MD)) != 0) {
		(void)__db_close(cdbp, txn, DB_NOSYNC);
		return (ret);
	}
	*cdbpp = cdbp;
	return (0);
}

/*
 * __typed_catalog_write --
 *	Write (overwrite) the catalog record for a newly created typed
 *	subdatabase, in the caller's txn (spec §4.3).
 */
int
__typed_catalog_write(dbp, ip, txn, fname, dname, ts)
	DB *dbp;
	DB_THREAD_INFO *ip;
	DB_TXN *txn;
	const char *fname, *dname;
	TYPED_STATE *ts;
{
	DB *cdbp;
	DBT key, data;
	u_int8_t *rec;
	u_int32_t reclen;
	int ret, t_ret;

	if ((ret = __typed_catalog_encode(dbp, ts, &rec, &reclen)) != 0)
		return (ret);
	if ((ret = __typed_catalog_open(dbp, ip, txn, fname, 1, &cdbp)) != 0) {
		__os_free(dbp->env, rec);
		return (ret);
	}
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)dname; key.size = (u_int32_t)strlen(dname);
	data.data = rec; data.size = reclen;
	ret = __db_put(cdbp, ip, txn, &key, &data, 0);
	__os_free(dbp->env, rec);
	if ((t_ret = __db_close(cdbp, txn, DB_NOSYNC)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/*
 * __typed_catalog_check --
 *	Parse the catalog record for an existing typed subdatabase and compare
 *	it against the passed schema and fingerprints (spec §5.2).
 */
int
__typed_catalog_check(dbp, ip, txn, fname, dname, ts)
	DB *dbp;
	DB_THREAD_INFO *ip;
	DB_TXN *txn;
	const char *fname, *dname;
	TYPED_STATE *ts;
{
	ENV *env;
	DB *cdbp;
	DBT key, data;
	const u_int8_t *p, *end;
	const DB_RECORD_CODEC *rc;
	const DB_CONSTRAINT *con;
	u_int32_t magic, ver, total, type, ncodecs, i, j, s, present;
	u_int32_t nf, nconstraints, idx;
	int ret, t_ret, ok;
	char namebuf[256];

	env = dbp->env;
	if ((ret = __typed_catalog_open(dbp, ip, txn, fname, 0, &cdbp)) != 0) {
		/* No __db_typed subdatabase at all: no catalog record. */
		if (ret == ENOENT) {
			__db_errx(env, DB_STR("5364",
		    "typed database %s: no catalog record"), dname);
			return (EINVAL);
		}
		return (ret);
	}
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)dname; key.size = (u_int32_t)strlen(dname);
	if ((ret = __db_get(cdbp, ip, txn, &key, &data, 0)) != 0) {
		if (ret == DB_NOTFOUND) {
			__db_errx(env, DB_STR("5365",
		    "typed database %s: no catalog record"), dname);
			ret = EINVAL;
		}
		goto done;
	}

	p = data.data;
	end = p + data.size;
	ok = 1;
#define	CORRUPT()	do {						\
	__db_errx(env, DB_STR("5366",					\
	    "typed database %s: corrupt catalog record"), dname);	\
	ret = EINVAL; goto done;					\
} while (0)
#define	MISMATCH(msg)	do {						\
	__db_errx(env, "%s", msg);					\
	ret = EINVAL; goto done;					\
} while (0)

	magic = cget(&p, end, 4, &ok);
	ver = cget(&p, end, 4, &ok);
	total = cget(&p, end, 4, &ok);
	type = cget(&p, end, 4, &ok);
	ncodecs = cget(&p, end, 2, &ok);
	if (!ok || magic != TYPED_CAT_MAGIC)
		CORRUPT();
	if (ver > TYPED_CAT_VERSION) {
		__db_errx(env, DB_STR("5367",
	    "typed database %s: catalog format %lu not supported"),
		    dname, (u_long)ver);
		ret = EINVAL; goto done;
	}
	if (total != data.size)
		CORRUPT();
	if (type != (u_int32_t)ts->schema.type)
		MISMATCH("typed database: DBTYPE differs from the catalog record");
	if (ncodecs != ts->ncodecs)
		MISMATCH("typed database: codec set differs from the catalog");

	/* Compare each codec by position (collect order is deterministic). */
	for (i = 0; i < ncodecs; i++) {
		u_int32_t namelen, cver, cflags, cfixed, sflags;
		const DB_CODEC *sc;

		namelen = cget(&p, end, 1, &ok);
		if (!ok || namelen > 255 || p + namelen > end)
			CORRUPT();
		memcpy(namebuf, p, namelen); namebuf[namelen] = '\0';
		p += namelen;
		cver = cget(&p, end, 4, &ok);
		cflags = cget(&p, end, 4, &ok);
		cfixed = cget(&p, end, 4, &ok);
		if (!ok || p + 20 > end)
			CORRUPT();
		sc = ts->codecs[i];
		sflags = sc->flags;
		if (FLD_ISSET(sflags, DB_CODEC_MEMCMP))
			sflags |= DB_CODEC_EQUALIMAGE;
		if (strcmp(namebuf, sc->name) != 0)
			MISMATCH("typed database: codec set differs from the catalog");
		if (cver != sc->version) {
			__db_errx(env, DB_STR("5368",
		    "typed database requires codec %s v%lu, got v%lu"),
			    sc->name, (u_long)cver, (u_long)sc->version);
			ret = EINVAL; goto done;
		}
		if (cflags != sflags || cfixed != sc->fixed_len ||
		    memcmp(p, ts->fp[i], 20) != 0) {
			__db_errx(env, DB_STR("5369",
	    "codec %s v%lu behaves differently from the codec this database was created with"),
			    sc->name, (u_long)sc->version);
			ret = DB_CODEC_MISMATCH; goto done;
		}
		p += 20;
	}

	/* key then data record codec. */
	for (s = 0; s < 2; s++) {
		u_int32_t namelen, rver, rflags;

		rc = (s == 0) ? ts->schema.key : ts->schema.data;
		present = cget(&p, end, 1, &ok);
		if (!ok)
			CORRUPT();
		if (present == 0) {
			if (rc != NULL)
				MISMATCH("typed database: schema differs from the catalog record");
			continue;
		}
		if (rc == NULL)
			MISMATCH("typed database: schema differs from the catalog record");
		namelen = cget(&p, end, 1, &ok);
		if (!ok || namelen > 255 || p + namelen > end)
			CORRUPT();
		memcpy(namebuf, p, namelen); namebuf[namelen] = '\0';
		p += namelen;
		rver = cget(&p, end, 4, &ok);
		rflags = cget(&p, end, 4, &ok);
		nf = cget(&p, end, 4, &ok);
		if (!ok)
			CORRUPT();
		if (strcmp(namebuf, rc->name) != 0 || rver != rc->version ||
		    rflags != rc->flags || nf != rc->nfields)
			MISMATCH("typed database: schema differs from the catalog record");
		for (j = 0; j < nf; j++) {
			idx = cget(&p, end, 2, &ok);
			if (!ok)
				CORRUPT();
			if (idx != __typed_codec_index(ts, rc->types[j]))
				MISMATCH("typed database: schema differs from the catalog record");
		}
	}

	nconstraints = cget(&p, end, 2, &ok);
	if (!ok)
		CORRUPT();
	if (nconstraints != ts->schema.nconstraints)
		MISMATCH("typed database: schema differs from the catalog record");
	for (i = 0; i < nconstraints; i++) {
		u_int32_t kind, side, field, maxlen, cflags, namelen;

		con = &ts->schema.constraints[i];
		kind = cget(&p, end, 4, &ok);
		side = cget(&p, end, 4, &ok);
		field = cget(&p, end, 4, &ok);
		maxlen = cget(&p, end, 4, &ok);
		cflags = cget(&p, end, 4, &ok);
		namelen = cget(&p, end, 1, &ok);
		if (!ok || namelen > 255 || p + namelen > end)
			CORRUPT();
		memcpy(namebuf, p, namelen); namebuf[namelen] = '\0';
		p += namelen;
		if (kind != con->kind || side != con->side ||
		    field != con->field || maxlen != con->maxlen ||
		    cflags != con->flags || strcmp(namebuf, con->name) != 0)
			MISMATCH("typed database: schema differs from the catalog record");
	}
	ret = 0;
#undef	CORRUPT
#undef	MISMATCH

done:	if ((t_ret = __db_close(cdbp, txn, DB_NOSYNC)) != 0 && ret == 0)
		ret = t_ret;
	COMPQUIET(total, 0);
	return (ret);
}

/*
 * __typed_catalog_remove --
 *	Delete the catalog record for a typed subdatabase being removed
 *	(spec §4.3).  A missing record (or no __db_typed at all) is not an
 *	error.  Called from __db_subdb_remove after MU_REMOVE, in the txn.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_catalog_remove __P((DB *, DB_THREAD_INFO *, DB_TXN *,
 * PUBLIC:     const char *, const char *));
 * PUBLIC: #endif
 */
int
__typed_catalog_remove(dbp, ip, txn, fname, dname)
	DB *dbp;
	DB_THREAD_INFO *ip;
	DB_TXN *txn;
	const char *fname, *dname;
{
	DB *cdbp;
	DBT key;
	int ret, t_ret;

	if ((ret = __typed_catalog_open(dbp, ip, txn, fname, 0, &cdbp)) != 0)
		return (ret == ENOENT ? 0 : ret);
	memset(&key, 0, sizeof(key));
	key.data = (void *)dname; key.size = (u_int32_t)strlen(dname);
	ret = __db_del(cdbp, ip, txn, &key, 0);
	if (ret == DB_NOTFOUND)
		ret = 0;
	if ((t_ret = __db_close(cdbp, txn, DB_NOSYNC)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/*
 * __typed_catalog_rename --
 *	Move the catalog record from oldname to newname (spec §4.3).  A
 *	missing old record is not an error.  Called from __db_subdb_rename
 *	after MU_RENAME, in the txn.
 *
 * PUBLIC: #ifdef HAVE_TYPED
 * PUBLIC: int __typed_catalog_rename __P((DB *, DB_THREAD_INFO *, DB_TXN *,
 * PUBLIC:     const char *, const char *, const char *));
 * PUBLIC: #endif
 */
int
__typed_catalog_rename(dbp, ip, txn, fname, oldname, newname)
	DB *dbp;
	DB_THREAD_INFO *ip;
	DB_TXN *txn;
	const char *fname, *oldname, *newname;
{
	ENV *env;
	DB *cdbp;
	DBT key, data, nkey;
	int ret, t_ret;

	env = dbp->env;
	if ((ret = __typed_catalog_open(dbp, ip, txn, fname, 0, &cdbp)) != 0)
		return (ret == ENOENT ? 0 : ret);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	memset(&nkey, 0, sizeof(nkey));
	key.data = (void *)oldname; key.size = (u_int32_t)strlen(oldname);
	data.flags = DB_DBT_MALLOC;
	if ((ret = __db_get(cdbp, ip, txn, &key, &data, 0)) != 0) {
		if (ret == DB_NOTFOUND)
			ret = 0;
		goto done;
	}
	nkey.data = (void *)newname; nkey.size = (u_int32_t)strlen(newname);
	if ((ret = __db_put(cdbp, ip, txn, &nkey, &data, 0)) != 0)
		goto done;
	ret = __db_del(cdbp, ip, txn, &key, 0);
	if (ret == DB_NOTFOUND)
		ret = 0;

done:	if (data.data != NULL)
		__os_ufree(env, data.data);
	if ((t_ret = __db_close(cdbp, txn, DB_NOSYNC)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}
#endif /* HAVE_TYPED */

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
#endif /* HAVE_TYPED */

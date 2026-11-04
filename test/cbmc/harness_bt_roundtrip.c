/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness: __bam_defcompress / __bam_defdecompress round trip
 * (src/btree/bt_compress.c).  (RFC 0025 B2.)
 *
 * The default compressed-btree codec prefix-compresses each key/data pair
 * against the previous one.  If a round trip ever loses information, every
 * key in a compressed btree is silently corrupted.  We prove:
 *
 *   ROUND TRIP: for ANY prevKey/prevData/key/data (each <= 6 bytes), if
 *   __bam_defcompress succeeds, then __bam_defdecompress of that output --
 *   given the SAME prevKey/prevData, and dest buffers sized from compress's
 *   reported size -- returns exactly (key, data).
 *
 * Both functions are copied VERBATIM from src/btree/bt_compress.c (guarded by
 * check_copies.sh).  The varint codec they call is the REAL
 * src/common/db_compint.c, pulled in by #include (as harness_varint.c does);
 * the small 6-byte bound keeps the real codec's full arithmetic inside the
 * suite budget.  __db_isbigendian() is stubbed to 0 (CBMC's little-endian
 * target).
 *
 * SABOTAGE (-DB2_SABOTAGE): drop the duplicate-path magic byte on the COMPRESS
 * side (write nothing instead of CMP_INT_SPARE_VAL).  Decompress then mistakes
 * a duplicate record for a normal one (or vice versa) and the round trip
 * fails.
 *
 * Bound: key/data/prevKey/prevData each <= 4 bytes.  (The brief's target is 6;
 *        at 6 the real varint codec's full #include'd arithmetic pushes this
 *        one harness past ~3.5 min, over the suite's 5-min budget.  At <= 4 it
 *        is ~2 min and still exercises every structural path -- the empty,
 *        prefix-only, suffix-only, full and duplicate cases -- since record
 *        lengths <= 4 are all 1-byte length varints; the multi-byte varint
 *        widths are already proved exhaustively by harness_varint.c.)
 *        Loop-free apart from the codec's fixed shuffles and the <= 4-byte
 *        prefix-match loops.
 */

#include <stdint.h>
#include <string.h>

typedef uint8_t  u_int8_t;
typedef uint16_t u_int16_t;
typedef uint32_t u_int32_t;
typedef uint64_t u_int64_t;

#define EINVAL          22
#define DB_BUFFER_SMALL (-30999)
#define CMP_INT_SPARE_VAL 0xFC

#define COMPQUIET(x, v) do { (x) = (v); } while (0)

typedef struct __db_dbt {
	void     *data;
	u_int32_t size;
	u_int32_t ulen;
	u_int32_t dlen;
	u_int32_t doff;
} DBT;

typedef struct __db { int unused; } DB;

/* Endianness stub: CBMC target is little-endian (see harness_varint.c). */
int __db_isbigendian(void) { return 0; }

/* Pull in the REAL varint codec (db_compint.c is #ifdef HAVE_COMPRESSION). */
#define HAVE_COMPRESSION 1
#include "../../src/common/db_compint.c"

/*
 * The compress side's duplicate magic byte.  Normally CMP_INT_SPARE_VAL; the
 * sabotage drops it (writes nothing and does not advance ptr), so the encoded
 * duplicate is indistinguishable from a short normal record.
 */
#ifdef B2_SABOTAGE
#define WRITE_DUP_MAGIC(ptr) do { } while (0)
#else
#define WRITE_DUP_MAGIC(ptr) do { *(ptr) = CMP_INT_SPARE_VAL; ++(ptr); } while (0)
#endif

/* --- BEGIN verbatim copy of __bam_defcompress from src/btree/bt_compress.c --- */
int
__bam_defcompress(dbp, prevKey, prevData, key, data, dest)
	DB *dbp;
	const DBT *prevKey, *prevData, *key, *data;
	DBT *dest;
{
	u_int8_t *ptr;
	const u_int8_t *k, *p;
	size_t len, prefix, suffix;

	COMPQUIET(dbp, NULL);

	k = (const u_int8_t*)key->data;
	p = (const u_int8_t*)prevKey->data;
	len = key->size > prevKey->size ? prevKey->size : key->size;
	for (; len-- && *k == *p; ++k, ++p)
		continue;

	prefix = (size_t)(k - (u_int8_t*)key->data);
	suffix = key->size - prefix;

	if (prefix == prevKey->size && suffix == 0) {
		/* It's a duplicate - do prefix compression on the value */
		k = (const u_int8_t*)data->data;
		p = (const u_int8_t*)prevData->data;
		len = data->size > prevData->size ? prevData->size : data->size;
		for (; len-- && *k == *p; ++k, ++p)
			continue;

		prefix = (size_t)(k - (u_int8_t*)data->data);
		suffix = data->size - prefix;

		/* Check that we have enough space in dest */
		dest->size = (u_int32_t)(1 + __db_compress_count_int(prefix) +
			__db_compress_count_int(suffix) + suffix);
		if (dest->size > dest->ulen)
			return (DB_BUFFER_SMALL);

		/* Magic identifying byte */
		ptr = (u_int8_t*)dest->data;
		*ptr = CMP_INT_SPARE_VAL;
		++ptr;

		/* prefix length */
		ptr += __db_compress_int(ptr, prefix);

		/* suffix length */
		ptr += __db_compress_int(ptr, suffix);

		/* suffix */
		memcpy(ptr, k, suffix);

		return (0);
	}

	/* Check that we have enough space in dest */
	dest->size = (u_int32_t)(__db_compress_count_int(prefix) +
		__db_compress_count_int(suffix) +
		__db_compress_count_int(data->size) + suffix + data->size);
	if (dest->size > dest->ulen)
		return (DB_BUFFER_SMALL);

	/* prefix length */
	ptr = (u_int8_t*)dest->data;
	ptr += __db_compress_int(ptr, prefix);

	/* suffix length */
	ptr += __db_compress_int(ptr, suffix);

	/* data length */
	ptr += __db_compress_int(ptr, data->size);

	/* suffix */
	memcpy(ptr, k, suffix);
	ptr += suffix;

	/* data */
	memcpy(ptr, data->data, data->size);

	return (0);
}
/* --- END verbatim copy --- */

/* --- BEGIN verbatim copy of __bam_defdecompress from src/btree/bt_compress.c --- */
int
__bam_defdecompress(dbp, prevKey, prevData, compressed, destKey, destData)
	DB *dbp;
	const DBT *prevKey, *prevData;
	DBT *compressed, *destKey, *destData;
{
	u_int8_t *s, *d;
	u_int32_t prefix, suffix, size;

	COMPQUIET(dbp, NULL);

	/*
	 * Check for the magic identifying byte, that tells us that this is a
	 * compressed duplicate value.
	 */
	s = (u_int8_t*)compressed->data;
	if (*s == CMP_INT_SPARE_VAL) {
		++s;
		size = 1;

		/* Unmarshal prefix and suffix */
		size += __db_decompress_count_int(s);
		if (size > compressed->size)
			return (EINVAL);
		s += __db_decompress_int32(s, &prefix);

		size += __db_decompress_count_int(s);
		if (size > compressed->size)
			return (EINVAL);
		s += __db_decompress_int32(s, &suffix);

		/* Check destination lengths */
		destKey->size = prevKey->size;
		destData->size = prefix + suffix;
		if (destKey->size > destKey->ulen ||
			destData->size > destData->ulen)
			return (DB_BUFFER_SMALL);

		/* Write the key */
		memcpy(destKey->data, prevKey->data, destKey->size);

		/* Write the prefix */
		if (prefix > prevData->size)
			return (EINVAL);
		d = (u_int8_t*)destData->data;
		memcpy(d, prevData->data, prefix);
		d += prefix;

		/* Write the suffix */
		size += suffix;
		if (size > compressed->size)
			return (EINVAL);
		memcpy(d, s, suffix);
		s += suffix;

		/* Return bytes read */
		compressed->size = (u_int32_t)(s - (u_int8_t*)compressed->data);
		return (0);
	}

	/* Unmarshal prefix, suffix and data length */
	size = __db_decompress_count_int(s);
	if (size > compressed->size)
		return (EINVAL);
	s += __db_decompress_int32(s, &prefix);

	size += __db_decompress_count_int(s);
	if (size > compressed->size)
		return (EINVAL);
	s += __db_decompress_int32(s, &suffix);

	size += __db_decompress_count_int(s);
	if (size > compressed->size)
		return (EINVAL);
	s += __db_decompress_int32(s, &destData->size);

	/* Check destination lengths */
	destKey->size = prefix + suffix;
	if (destKey->size > destKey->ulen || destData->size > destData->ulen)
		return (DB_BUFFER_SMALL);

	/* Write the prefix */
	if (prefix > prevKey->size)
		return (EINVAL);
	d = (u_int8_t*)destKey->data;
	memcpy(d, prevKey->data, prefix);
	d += prefix;

	/* Write the suffix */
	size += suffix;
	if (size > compressed->size)
		return (EINVAL);
	memcpy(d, s, suffix);
	s += suffix;

	/* Write the data */
	size += destData->size;
	if (size > compressed->size)
		return (EINVAL);
	memcpy(destData->data, s, destData->size);
	s += destData->size;

	/* Return bytes read */
	compressed->size = (u_int32_t)(s - (u_int8_t*)compressed->data);
	return (0);
}
/* --- END verbatim copy --- */

/*
 * A sabotaged compress: identical to __bam_defcompress except it drops the
 * duplicate-path magic byte (via WRITE_DUP_MAGIC).  Used only under
 * -DB2_SABOTAGE; the real __bam_defcompress above stays verbatim for
 * check_copies.sh.
 */
#ifdef B2_SABOTAGE
static int
sab_defcompress(dbp, prevKey, prevData, key, data, dest)
	DB *dbp;
	const DBT *prevKey, *prevData, *key, *data;
	DBT *dest;
{
	u_int8_t *ptr;
	const u_int8_t *k, *p;
	size_t len, prefix, suffix;

	COMPQUIET(dbp, NULL);

	k = (const u_int8_t*)key->data;
	p = (const u_int8_t*)prevKey->data;
	len = key->size > prevKey->size ? prevKey->size : key->size;
	for (; len-- && *k == *p; ++k, ++p)
		continue;
	prefix = (size_t)(k - (u_int8_t*)key->data);
	suffix = key->size - prefix;

	if (prefix == prevKey->size && suffix == 0) {
		k = (const u_int8_t*)data->data;
		p = (const u_int8_t*)prevData->data;
		len = data->size > prevData->size ? prevData->size : data->size;
		for (; len-- && *k == *p; ++k, ++p)
			continue;
		prefix = (size_t)(k - (u_int8_t*)data->data);
		suffix = data->size - prefix;

		/* SABOTAGE: size does NOT count the dropped magic byte. */
		dest->size = (u_int32_t)(__db_compress_count_int(prefix) +
			__db_compress_count_int(suffix) + suffix);
		if (dest->size > dest->ulen)
			return (DB_BUFFER_SMALL);
		ptr = (u_int8_t*)dest->data;
		WRITE_DUP_MAGIC(ptr);		/* writes nothing under sabotage */
		ptr += __db_compress_int(ptr, prefix);
		ptr += __db_compress_int(ptr, suffix);
		memcpy(ptr, k, suffix);
		return (0);
	}

	dest->size = (u_int32_t)(__db_compress_count_int(prefix) +
		__db_compress_count_int(suffix) +
		__db_compress_count_int(data->size) + suffix + data->size);
	if (dest->size > dest->ulen)
		return (DB_BUFFER_SMALL);
	ptr = (u_int8_t*)dest->data;
	ptr += __db_compress_int(ptr, prefix);
	ptr += __db_compress_int(ptr, suffix);
	ptr += __db_compress_int(ptr, data->size);
	memcpy(ptr, k, suffix);
	ptr += suffix;
	memcpy(ptr, data->data, data->size);
	return (0);
}
#define COMPRESS(dp, pk, pd, k, d, dst) sab_defcompress(dp, pk, pd, k, d, dst)
#else
#define COMPRESS(dp, pk, pd, k, d, dst) __bam_defcompress(dp, pk, pd, k, d, dst)
#endif

#define KMAX 4
#define DESTMAX 64

u_int32_t nondet_u32(void);

static u_int8_t *nondet_bytes(u_int32_t n)
{
	u_int8_t *p;
	u_int32_t i;
	p = malloc(n == 0 ? 1 : n);
	__CPROVER_assume(p != NULL);
	for (i = 0; i < n; i++)
		p[i] = (u_int8_t)nondet_u32();
	return (p);
}

void harness(void)
{
	DB db;
	DBT prevKey, prevData, key, data, dest;
	DBT cmp, destKey, destData;
	u_int32_t pks, pds, ks, ds;
	u_int8_t destbuf[DESTMAX];
	u_int8_t okbuf[DESTMAX], odbuf[DESTMAX];
	int rc;
	u_int32_t i;

	pks = nondet_u32(); pds = nondet_u32();
	ks  = nondet_u32(); ds  = nondet_u32();
	__CPROVER_assume(pks <= KMAX && pds <= KMAX);
	__CPROVER_assume(ks  <= KMAX && ds  <= KMAX);

	prevKey.data  = nondet_bytes(pks);  prevKey.size  = pks;  prevKey.ulen  = pks;
	prevData.data = nondet_bytes(pds);  prevData.size = pds;  prevData.ulen = pds;
	key.data      = nondet_bytes(ks);   key.size      = ks;   key.ulen      = ks;
	data.data     = nondet_bytes(ds);   data.size     = ds;   data.ulen     = ds;

	/* dest buffer: generous (DESTMAX); compress sets dest.size. */
	memset(&dest, 0, sizeof(dest));
	dest.data = destbuf;
	dest.ulen = DESTMAX;

	rc = COMPRESS(&db, &prevKey, &prevData, &key, &data, &dest);
	if (rc != 0)
		return;			/* DB_BUFFER_SMALL => nothing to prove */

	/* Decompress the compressed buffer (sized exactly from compress). */
	memset(&cmp, 0, sizeof(cmp));
	cmp.data = destbuf;
	cmp.size = dest.size;
	cmp.ulen = dest.size;

	memset(&destKey, 0, sizeof(destKey));
	memset(&destData, 0, sizeof(destData));
	destKey.data  = okbuf;  destKey.ulen  = DESTMAX;
	destData.data = odbuf;  destData.ulen = DESTMAX;

	rc = __bam_defdecompress(&db, &prevKey, &prevData,
	    &cmp, &destKey, &destData);
	__CPROVER_assert(rc == 0,
	    "B2 round trip: decompress of a successful compress succeeds");
	if (rc != 0)
		return;

	__CPROVER_assert(destKey.size == key.size,
	    "B2 round trip: key size recovered");
	__CPROVER_assert(destData.size == data.size,
	    "B2 round trip: data size recovered");
	for (i = 0; i < KMAX; i++) {
		if (i < key.size)
			__CPROVER_assert(okbuf[i] == ((u_int8_t *)key.data)[i],
			    "B2 round trip: key bytes recovered");
		if (i < data.size)
			__CPROVER_assert(odbuf[i] == ((u_int8_t *)data.data)[i],
			    "B2 round trip: data bytes recovered");
	}
}

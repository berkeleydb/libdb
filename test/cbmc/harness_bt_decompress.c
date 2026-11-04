/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness: __bam_defdecompress (src/btree/bt_compress.c) -- the default
 * btree compressed-key/data decoder.  (RFC 0025 target B1.)
 *
 * A compressed btree's pages are a TRUST BOUNDARY: they come from disk, so a
 * corrupt or hostile file reaches this decoder.  It must never read outside
 * the compressed buffer [compressed->data, compressed->data + compressed->size)
 * nor outside prevKey->data / prevData->data, and never write past
 * destKey->ulen / destData->ulen.
 *
 *   SAFETY PROPERTY (checked by CBMC's --bounds-check + --pointer-check):
 *     for ANY compressed bytes of size 0..COMPMAX, ANY prevKey/prevData of
 *     size 0..PREVMAX, and dest buffers with nondet ulen 0..DESTMAX, no read
 *     or write ever leaves its object.
 *
 * Each buffer is allocated at EXACTLY its claimed size (malloc(size)), so any
 * byte read past compressed->size -- or past prevKey/prevData->size, or any
 * byte written past dest ulen -- is an out-of-object access CBMC reports.
 *
 * What is modelled / stubbed:
 *   - DBT / DB structs: mirror src/dbinc/db.in (only the fields the function
 *     touches: data, size, ulen).
 *   - COMPQUIET: a no-op, as in db_int.h.
 *   - the varint decoders __db_decompress_count_int / __db_decompress_int32.
 *     Their full arithmetic (the 256-entry size table, the 9-way byte
 *     shuffle) is proved separately by harness_varint.c against the REAL
 *     src/common/db_compint.c -- re-exploring it here blows the SAT formula
 *     past the 5-minute suite budget.  What matters for THIS proof is the
 *     memory footprint each helper touches, so the models below read
 *     EXACTLY the real bytes: __db_decompress_count_int reads buf[0] and
 *     returns a width 1..9; __db_decompress_int32 reads buf[0..width-1] and
 *     returns width.  The over-read B1 proves is precisely that
 *     __bam_defdecompress calls these BEFORE checking the buffer is long
 *     enough, so faithfully reproducing their read span is what the proof
 *     needs.  (The widths come from the real size table, abstracted to a
 *     nondet 1..9, which is a superset of the real behaviour.)
 *
 * The decoder body is copied VERBATIM from src/btree/bt_compress.c between the
 * BEGIN/END markers below (guarded by check_copies.sh).
 *
 * Bound: compressed <= COMPMAX (16) bytes, prevKey/prevData <= PREVMAX (8)
 *        bytes, dest ulen <= DESTMAX (16).  The function is loop-free.
 *
 * ===========================================================================
 * REAL BUG (expected): __bam_defdecompress reads each varint's first byte via
 *   __db_decompress_count_int(s)  BEFORE checking  size > compressed->size,
 * and reads *s (the magic byte) with no size check at all when size == 0.
 * On a truncated/corrupt compressed item this over-reads past
 * compressed->size.  The harness proves it (VERIFICATION FAILED on the real
 * code); -DB1_FIXED builds the fixed variant that verifies.  See README.md
 * "BUG FOUND".
 * ===========================================================================
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uint8_t  u_int8_t;
typedef uint16_t u_int16_t;
typedef uint32_t u_int32_t;
typedef uint64_t u_int64_t;
typedef uint32_t db_pgno_t;

#define EINVAL          22
#define DB_BUFFER_SMALL (-30999)

/* CMP_INT_SPARE_VAL: db_int.h value (the duplicate-value magic byte). */
#define CMP_INT_SPARE_VAL 0xFC

#define COMPQUIET(x, v) do { (x) = (v); } while (0)

/* DBT: only the fields __bam_defdecompress touches, at the real layout. */
typedef struct __db_dbt {
	void     *data;
	u_int32_t size;
	u_int32_t ulen;
	u_int32_t dlen;
	u_int32_t doff;
} DBT;

typedef struct __db { int unused; } DB;

/*
 * Varint read-footprint models.  See the header comment: these read EXACTLY
 * the bytes the real db_compint.c decoders read (buf[0] for the count; the
 * leading `width` bytes for the int32), so CBMC sees the identical
 * out-of-buffer accesses, while the real arithmetic is verified by
 * harness_varint.c.  The width is the REAL size table from db_compint.c
 * (__db_marshaled_int_size), so count_int and int32 agree on the width for
 * the same leading byte, exactly as the real code does.
 */
static const u_int8_t varint_size_tab[256] = {
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
	2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
	3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
	4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,6,7,8,9,0xFF,0xFF,0xFF,0xFF
};

static u_int32_t varint_width(const u_int8_t *buf)
{
	u_int8_t c = buf[0];          /* the real count_int reads buf[0] */
	u_int32_t w = varint_size_tab[c];
	/* 0xFF table entries are spare/magic bytes; the caller never reaches
	 * them on the non-magic paths, but clamp so the read loop is bounded. */
	if (w == 0xFF)
		w = 1;
	return (w);
}

static u_int32_t __db_decompress_count_int(const u_int8_t *buf)
{
	return (varint_width(buf));
}

static int __db_decompress_int32(const u_int8_t *buf, u_int32_t *i)
{
	u_int32_t w = varint_width(buf);
	u_int32_t k;
	u_int32_t acc = 0;
	/* The real int32 decoder reads the leading `w` bytes of buf. */
	for (k = 0; k < w; k++)
		acc = (acc << 1) ^ buf[k];
	*i = acc;
	return ((int)w);
}

/* ------------------------------------------------------------------------ */
#ifdef B1_FIXED
/*
 * The proposed fix: check that the varint's declared width fits BEFORE
 * reading its bytes, and that there is at least one byte before reading *s.
 * This mirrors fix/bt-decompress-bounds.  __db_decompress_count_int reads
 * s[0] (one byte), so a safe pre-check is "the next byte index (size) is
 * still < compressed->size" before each count_int, and compressed->size >= 1
 * before *s.
 */
static int
b1_fixed_defdecompress(dbp, prevKey, prevData, compressed, destKey, destData)
	DB *dbp;
	const DBT *prevKey, *prevData;
	DBT *compressed, *destKey, *destData;
{
	u_int8_t *s, *d;
	u_int32_t prefix, suffix, size;

	COMPQUIET(dbp, NULL);

	/* Need at least the magic/first byte. */
	if (compressed->size < 1)
		return (EINVAL);

	s = (u_int8_t*)compressed->data;
	if (*s == CMP_INT_SPARE_VAL) {
		++s;
		size = 1;

		/* Before reading the count byte at offset `size`, it must be
		 * on-buffer. */
		if (size >= compressed->size)
			return (EINVAL);
		size += __db_decompress_count_int(s);
		if (size > compressed->size)
			return (EINVAL);
		s += __db_decompress_int32(s, &prefix);

		if (size >= compressed->size)
			return (EINVAL);
		size += __db_decompress_count_int(s);
		if (size > compressed->size)
			return (EINVAL);
		s += __db_decompress_int32(s, &suffix);

		destKey->size = prevKey->size;
		destData->size = prefix + suffix;
		if (destKey->size > destKey->ulen ||
			destData->size > destData->ulen)
			return (DB_BUFFER_SMALL);

		memcpy(destKey->data, prevKey->data, destKey->size);

		if (prefix > prevData->size)
			return (EINVAL);
		d = (u_int8_t*)destData->data;
		memcpy(d, prevData->data, prefix);
		d += prefix;

		size += suffix;
		if (size > compressed->size)
			return (EINVAL);
		memcpy(d, s, suffix);
		s += suffix;

		compressed->size = (u_int32_t)(s - (u_int8_t*)compressed->data);
		return (0);
	}

	if (size >= compressed->size)
		return (EINVAL);
	size = __db_decompress_count_int(s);
	if (size > compressed->size)
		return (EINVAL);
	s += __db_decompress_int32(s, &prefix);

	if (size >= compressed->size)
		return (EINVAL);
	size += __db_decompress_count_int(s);
	if (size > compressed->size)
		return (EINVAL);
	s += __db_decompress_int32(s, &suffix);

	if (size >= compressed->size)
		return (EINVAL);
	size += __db_decompress_count_int(s);
	if (size > compressed->size)
		return (EINVAL);
	s += __db_decompress_int32(s, &destData->size);

	destKey->size = prefix + suffix;
	if (destKey->size > destKey->ulen || destData->size > destData->ulen)
		return (DB_BUFFER_SMALL);

	if (prefix > prevKey->size)
		return (EINVAL);
	d = (u_int8_t*)destKey->data;
	memcpy(d, prevKey->data, prefix);
	d += prefix;

	size += suffix;
	if (size > compressed->size)
		return (EINVAL);
	memcpy(d, s, suffix);
	s += suffix;

	size += destData->size;
	if (size > compressed->size)
		return (EINVAL);
	memcpy(destData->data, s, destData->size);
	s += destData->size;

	compressed->size = (u_int32_t)(s - (u_int8_t*)compressed->data);
	return (0);
}
#else
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
#endif /* B1_FIXED */

#define COMPMAX 16
#define PREVMAX 8
#define DESTMAX 16

u_int32_t nondet_u32(void);

/*
 * Allocate a buffer of EXACTLY `n` bytes.  CBMC's malloc gives a fresh object
 * of exactly n bytes whose contents are nondeterministic, so (a) we need no
 * fill loop, and (b) any read of byte >= n -- including byte 0 of a 0-byte
 * object, the size==0 case -- is an out-of-object access CBMC reports.  We do
 * NOT return NULL for n==0: a NULL dereference is a different (modelling)
 * failure; we want the clean "read past compressed->size" over-read, so a
 * 0-size-but-valid object is the faithful model of a truncated item.
 */
static u_int8_t *exact_buf(u_int32_t n)
{
	u_int8_t *p = malloc(n);
	__CPROVER_assume(p != NULL);
	return (p);
}

void harness(void)
{
	DB db;
	DBT prevKey, prevData, compressed, destKey, destData;
	u_int32_t csize, pksize, pdsize, dkulen, ddulen;

	csize  = nondet_u32();
	pksize = nondet_u32();
	pdsize = nondet_u32();
	dkulen = nondet_u32();
	ddulen = nondet_u32();
	__CPROVER_assume(csize  <= COMPMAX);
	__CPROVER_assume(pksize <= PREVMAX);
	__CPROVER_assume(pdsize <= PREVMAX);
	__CPROVER_assume(dkulen <= DESTMAX);
	__CPROVER_assume(ddulen <= DESTMAX);

	compressed.data = exact_buf(csize);
	compressed.size = csize;
	compressed.ulen = csize;

	prevKey.data = exact_buf(pksize);
	prevKey.size = pksize;
	prevKey.ulen = pksize;

	prevData.data = exact_buf(pdsize);
	prevData.size = pdsize;
	prevData.ulen = pdsize;

	/* dest buffers are allocated at EXACTLY ulen; a write past ulen is an
	 * over-write CBMC reports. */
	destKey.data = exact_buf(dkulen);
	destKey.size = 0;
	destKey.ulen = dkulen;

	destData.data = exact_buf(ddulen);
	destData.size = 0;
	destData.ulen = ddulen;

#ifdef B1_FIXED
	(void)b1_fixed_defdecompress(&db, &prevKey, &prevData,
	    &compressed, &destKey, &destData);
#else
	(void)__bam_defdecompress(&db, &prevKey, &prevData,
	    &compressed, &destKey, &destData);
#endif
}

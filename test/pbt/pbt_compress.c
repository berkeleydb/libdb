/*-
 * test/pbt/pbt_compress.c
 *	Property-based test for the btree default prefix-compression codec:
 *	__bam_defcompress / __bam_defdecompress (src/btree/bt_compress.c),
 *	built only when HAVE_COMPRESSION is defined (the default).
 *
 * Contract from the source:
 *   __bam_defcompress(dbp, prevKey, prevData, key, data, dest)
 *	writes into `dest` a compressed encoding of (key, data) expressed
 *	relative to the preceding (prevKey, prevData): it stores the length
 *	of the common prefix key shares with prevKey, then the differing
 *	suffix, then the data.  dbp is COMPQUIET'd, so NULL is safe.
 *   __bam_defdecompress(dbp, prevKey, prevData, compressed, destKey, destData)
 *	reverses it: it reconstructs (key, data) from the same preceding
 *	(prevKey, prevData) and the compressed bytes.
 *
 * Property (round-trip identity): for any preceding pair (prevKey,
 * prevData) and any current pair (key, data),
 *	decompress(compress(prev, cur)) == cur
 * byte-for-byte.  This is the invariant the whole compressed-btree
 * on-disk format rests on -- if it failed, a compressed database could
 * not be read back.  It is falsifiable against any prefix-length or
 * memcpy bug in either half of the codec.
 *
 * Both routines are exported from libdb (PUBLIC: prototypes in
 * bt_compress.c; verified reachable via nm).  We declare them locally.
 */

#include <string.h>

#include "db.h"

#include "pbt_common.h"

/* Exported by libdb (PUBLIC: prototypes in src/btree/bt_compress.c). */
extern int __bam_defcompress(DB *, const DBT *, const DBT *,
    const DBT *, const DBT *, DBT *);
extern int __bam_defdecompress(DB *, const DBT *, const DBT *, DBT *,
    DBT *, DBT *);

#if defined(PBT_HAVE_HEGEL)

/* Set a DBT to reference `n` bytes at `p`. */
static void
set_dbt(DBT *d, void *p, size_t n)
{
	memset(d, 0, sizeof(*d));
	d->data = p;
	d->size = (u_int32_t)n;
}

/* Set a DBT to reference an output buffer of capacity `cap` at `p`. */
static void
set_out(DBT *d, void *p, size_t cap)
{
	memset(d, 0, sizeof(*d));
	d->data = p;
	d->ulen = (u_int32_t)cap;
}

/*
 * P1: decompress(compress(prev, cur)) reproduces cur exactly.
 *
 * We generate a preceding key/data and a current key/data, all as
 * arbitrary byte strings (keys non-empty since a btree key is; data may
 * be empty).  Broad sizes deliberately include the case where `key`
 * shares a long prefix with `prevKey` (the interesting compression path)
 * and the case where they diverge at byte 0 (no prefix).
 */
static void
prop_compress_roundtrip(hegel_test_case *tc, void *u)
{
	uint8_t *pk, *pd, *k, *dta;
	size_t pklen = 0, pdlen = 0, klen = 0, dlen = 0;
	uint8_t enc[2048];
	uint8_t outk[1024], outd[1024];
	DBT prevKey, prevData, key, data, dest, dkey, ddata;
	int ret;
	(void)u;

	pk = hegel_draw_bytes(tc, hegel_binary(1, 200), &pklen);
	pd = hegel_draw_bytes(tc, hegel_binary(0, 200), &pdlen);
	k  = hegel_draw_bytes(tc, hegel_binary(1, 200), &klen);
	dta = hegel_draw_bytes(tc, hegel_binary(0, 200), &dlen);
	hegel_assume(pk != NULL && k != NULL);

	set_dbt(&prevKey, pk, pklen);
	set_dbt(&prevData, pd, pdlen);
	set_dbt(&key, k, klen);
	set_dbt(&data, dta, dlen);
	set_out(&dest, enc, sizeof(enc));

	ret = __bam_defcompress(NULL, &prevKey, &prevData, &key, &data, &dest);
	/* enc[] is generously sized; a full buffer is not the property here. */
	hegel_assume(ret == 0);

	/* dest.size now holds the encoded length; feed it back to decompress. */
	set_out(&dkey, outk, sizeof(outk));
	set_out(&ddata, outd, sizeof(outd));
	ret = __bam_defdecompress(NULL, &prevKey, &prevData, &dest,
	    &dkey, &ddata);
	hegel_assume(ret == 0);

	/* Round-trip identity for both key and data. */
	hegel_assume(dkey.size == key.size);
	hegel_assume(key.size == 0 || memcmp(dkey.data, key.data, key.size) == 0);
	hegel_assume(ddata.size == data.size);
	hegel_assume(data.size == 0 ||
	    memcmp(ddata.data, data.data, data.size) == 0);

	free(pk);
	free(pd);
	free(k);
	free(dta);
}

static const pbt_entry_t tests[] = {
	{ "compress_roundtrip", prop_compress_roundtrip, 600 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "compress_roundtrip", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("compress", tests)

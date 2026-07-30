/*-
 * test/pbt/pbt_compint.c
 *	Property-based tests for the integer-compression (varint) codec in
 *	src/common/db_compint.c: __db_compress_int / __db_decompress_int /
 *	__db_compress_count_int / __db_decompress_count_int /
 *	__db_decompress_int32.
 *
 * This codec is the on-disk length/offset encoding used by the btree
 * prefix-compression path (src/btree/bt_compress.c) -- it is only built
 * when HAVE_COMPRESSION is defined (the default: dist/configure.ac
 * db_cv_build_compression=yes -> AC_DEFINE(HAVE_COMPRESSION)).  The
 * source's own header comment gives the format table:
 *
 *   First byte  | Next  | Maximum value
 *   [0 xxxxxxx] | 0     | 2^7 - 1
 *   [10 xxxxxx] | 1     | 2^14 + 2^7 - 1
 *   ...
 *   [11111 011] | 8     | 2^64 + ... + 2^7 - 1
 *
 * and states: "this compression algorithm depends on big-endian order".
 * The high bits of the first byte are a self-describing length prefix, so
 * the encoding is *order-preserving*: this is what lets the codec store
 * lengths/offsets that must sort correctly.
 *
 * Contracts exercised (all grounded in the source, not restated from it):
 *   roundtrip        -- __db_decompress_int(__db_compress_int(i)) == i for
 *                       every u_int64_t i (the codec is a bijection).
 *   count_agrees     -- __db_compress_count_int(i) equals the byte count
 *                       __db_compress_int actually writes, AND equals
 *                       __db_decompress_count_int over the produced bytes,
 *                       AND equals the length __db_decompress_int reports.
 *   order_preserving -- a <= b  ==>  memcmp(enc(a), enc(b)) has the same
 *                       sign (comparing over the shorter length then by
 *                       length): the encoded bytes sort like the integers.
 *   int32_matches    -- for i <= UINT32_MAX, __db_decompress_int32 yields
 *                       the same value (and length) as __db_decompress_int.
 *
 * These are all exported from libdb (PUBLIC: prototypes in db_compint.c;
 * verified reachable via nm on the built library).  We declare them
 * locally to avoid dragging in db_int.h.
 */

#include "db.h"

#include "pbt_common.h"

/* Exported by libdb (PUBLIC: prototypes in src/common/db_compint.c). */
extern u_int32_t __db_compress_count_int(u_int64_t);
extern int __db_compress_int(u_int8_t *, u_int64_t);
extern u_int32_t __db_decompress_count_int(const u_int8_t *);
extern int __db_decompress_int(const u_int8_t *, u_int64_t *);
extern int __db_decompress_int32(const u_int8_t *, u_int32_t *);

#if defined(PBT_HAVE_HEGEL)

#include <string.h>

/* Max encoded size is 9 bytes (the [11111 011] 9-byte form). */
#define CMP_MAXLEN 9

/*
 * Draw a u_int64_t.  hegel_integers works over int64_t, so we draw a
 * signed value and reinterpret its bits -- this reaches the full unsigned
 * range including values above INT64_MAX (the 9-byte encodings).
 */
static u_int64_t
draw_u64(hegel_test_case *tc)
{
	return ((u_int64_t)hegel_draw_int(tc, hegel_integers(INT64_MIN, INT64_MAX)));
}

/* P1: encode then decode is the identity over the full 64-bit range. */
static void
prop_roundtrip(hegel_test_case *tc, void *u)
{
	u_int8_t buf[CMP_MAXLEN];
	u_int64_t v, out;
	int n;
	(void)u;

	v = draw_u64(tc);
	n = __db_compress_int(buf, v);
	PBT_CHECK(n >= 1 && n <= CMP_MAXLEN,
	    "__db_compress_int returned out-of-range length");
	(void)__db_decompress_int(buf, &out);
	PBT_CHECK(out == v, "varint roundtrip: decompress(compress(v)) != v");
}

/*
 * P2: the three "how many bytes" views agree with the encoder:
 *   count_int(v) == bytes written == decompress_count_int(buf)
 *                == length reported by decompress_int.
 */
static void
prop_count_agrees(hegel_test_case *tc, void *u)
{
	u_int8_t buf[CMP_MAXLEN];
	u_int64_t v, out;
	u_int32_t predicted, back;
	int written, read_len;
	(void)u;

	v = draw_u64(tc);
	predicted = __db_compress_count_int(v);
	written = __db_compress_int(buf, v);
	back = __db_decompress_count_int(buf);
	read_len = __db_decompress_int(buf, &out);

	PBT_CHECK((u_int32_t)written == predicted,
	    "__db_compress_int wrote a different byte count than count_int");
	PBT_CHECK(back == predicted,
	    "__db_decompress_count_int disagrees with count_int");
	PBT_CHECK((u_int32_t)read_len == predicted,
	    "__db_decompress_int length disagrees with count_int");
}

/*
 * P3: the encoding is order-preserving under unsigned byte comparison.
 * We encode two values, then compare the encodings the way a sorted store
 * would -- lexicographically over the common prefix, ties broken by the
 * longer encoding being greater (a longer varint always encodes a larger
 * value).  The sign of that comparison must match the sign of (a - b).
 */
static void
prop_order_preserving(hegel_test_case *tc, void *u)
{
	u_int8_t ba[CMP_MAXLEN], bb[CMP_MAXLEN];
	u_int64_t a, b;
	int la, lb, mn, cmp;
	(void)u;

	a = draw_u64(tc);
	b = draw_u64(tc);
	la = __db_compress_int(ba, a);
	lb = __db_compress_int(bb, b);

	mn = la < lb ? la : lb;
	cmp = memcmp(ba, bb, (size_t)mn);
	if (cmp == 0)
		cmp = (la > lb) - (la < lb);	/* longer enc => larger value */

	if (a < b)
		PBT_CHECK(cmp < 0, "varint encoding not order-preserving (a<b)");
	else if (a > b)
		PBT_CHECK(cmp > 0, "varint encoding not order-preserving (a>b)");
	else
		PBT_CHECK(cmp == 0, "varint encoding not order-preserving (a==b)");
}

/*
 * P4: for values that fit in 32 bits, the 32-bit decoder agrees with the
 * 64-bit decoder on both the value and the number of bytes consumed.
 */
static void
prop_int32_matches(hegel_test_case *tc, void *u)
{
	u_int8_t buf[CMP_MAXLEN];
	u_int64_t v, out64;
	u_int32_t out32;
	int n32, n64;
	(void)u;

	v = (u_int64_t)(u_int32_t)hegel_draw_int(tc,
	    hegel_integers(0, 0xFFFFFFFFLL));
	(void)__db_compress_int(buf, v);
	n32 = __db_decompress_int32(buf, &out32);
	n64 = __db_decompress_int(buf, &out64);

	PBT_CHECK((u_int64_t)out32 == out64,
	    "int32 decoder value disagrees with 64-bit decoder");
	PBT_CHECK(out32 == (u_int32_t)v,
	    "int32 decoder value != original");
	PBT_CHECK(n32 == n64,
	    "int32 decoder byte count disagrees with 64-bit decoder");
}

static const pbt_entry_t tests[] = {
	{ "roundtrip",        prop_roundtrip,        800 },
	{ "count_agrees",     prop_count_agrees,     800 },
	{ "order_preserving", prop_order_preserving, 800 },
	{ "int32_matches",    prop_int32_matches,    500 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "roundtrip",        NULL, 0 },
	{ "count_agrees",     NULL, 0 },
	{ "order_preserving", NULL, 0 },
	{ "int32_matches",    NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("compint", tests)

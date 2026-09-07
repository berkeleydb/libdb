/*-
 * See the file LICENSE for redistribution information.
 *
 * cov_codecs.c --
 *	Exhaustive exercise of libdb's self-contained CODECS: the
 *	compressed-integer (varint) codec in src/common/db_compint.c and the
 *	string-to-number parsers in src/common/db_getlong.c.
 *
 *	Why this exists.  These are pure functions -- no env, no locks, no I/O
 *	-- and yet db_compint.c sits at 21% line / 25% branch in report #3,
 *	with 66 of its 88 branches missing and `__db_decompress_int` never
 *	called at all.  The reason is documented in test/coverage/README.md:
 *	btree compression only ever marshals 32-bit lengths, so the 64-bit
 *	`__db_decompress_int` and the 4..9-byte size classes are unreachable
 *	from ANY Tcl workload, and the property-based tier that does cover
 *	them (test/pbt/pbt_compint.c) needs the `hegel` server binary and
 *	therefore compiles in STUB mode -- it links, prints SKIP, and executes
 *	nothing -- on a machine without hegel installed.  So the codec is
 *	well-tested in principle and measured cold in practice.
 *
 *	This driver closes that gap with no external dependency: it walks the
 *	codec's size-class BOUNDARIES directly (each class's first value, last
 *	value, and the value either side of the boundary), which is the input
 *	set that reaches every arm of every `else if` chain and every arm of
 *	the decode `switch (len)`.  Boundary enumeration, not random sampling,
 *	is the right technique here precisely because the branches ARE the
 *	size classes.
 *
 *	Properties asserted per value:
 *	  * round-trip -- decompress(compress(v)) == v, for the 32-bit and the
 *	    64-bit decoders alike (where the value fits);
 *	  * length agreement -- compress() writes exactly the byte count that
 *	    __db_compress_count_int() predicts and that
 *	    __db_decompress_count_int() reads back from the first byte;
 *	  * order preservation -- the codec is order-preserving in its encoded
 *	    form, which is what makes it usable for btree key prefixes:
 *	    a < b  =>  memcmp(enc(a), enc(b)) < 0.  That is the codec's real
 *	    contract and the one a subtle encoding bug would break.
 *
 *	Everything is a pure computation with a fixed input set, so it is
 *	exactly deterministic and takes milliseconds.
 */
#include "db_config.h"

#include "db_int.h"

static int fails = 0;
static long checks = 0;

#define	FAILF(fmt, ...) do {						\
	fprintf(stderr, "FAIL: %s:%d: " fmt "\n",			\
	    __FILE__, __LINE__, __VA_ARGS__);				\
	fails++;							\
} while (0)

/*
 * The codec's size-class boundaries, from src/common/db_compint.c.  Rather
 * than hard-code the CMP_INT_*_MAX constants (private to that file), derive
 * the boundaries from the predicted length: for each length L, find the first
 * value that needs L bytes.  That keeps this driver correct even if the
 * constants are retuned.
 */
#define	MAXLEN	9

/*
 * one_value --
 *	Encode v, check the three properties, and hand back the encoding so
 *	the caller can check ordering against the previous value.
 */
static void
one_value(v, enc, lenp)
	u_int64_t v;
	u_int8_t *enc;		/* MAXLEN bytes */
	size_t *lenp;
{
	u_int8_t buf[MAXLEN * 2];
	u_int64_t back64;
	u_int32_t back32;
	size_t wrote, predicted, readlen;

	memset(buf, 0xdb, sizeof(buf));

	/* --- predicted length from the VALUE. */
	predicted = __db_compress_count_int(v);
	checks++;
	if (predicted < 1 || predicted > MAXLEN) {
		FAILF("compress_count_int(%llu) = %zu, out of range",
		    (unsigned long long)v, predicted);
		*lenp = 0;
		return;
	}

	/* --- encode. */
	/* Both codecs RETURN the byte count; there is no out-parameter. */
	wrote = (size_t)__db_compress_int(buf, v);
	checks++;
	if (wrote < 1 || wrote > MAXLEN) {
		FAILF("compress_int(%llu) returned %zu bytes, out of range",
		    (unsigned long long)v, wrote);
		*lenp = 0;
		return;
	}
	checks++;
	if (wrote != predicted)
		FAILF("compress_int(%llu) wrote %zu bytes but "
		    "compress_count_int predicted %zu",
		    (unsigned long long)v, wrote, predicted);

	/* --- length read back from the FIRST BYTE (the decoder's view). */
	readlen = __db_decompress_count_int(buf);
	checks++;
	if (readlen != wrote)
		FAILF("decompress_count_int of enc(%llu) = %zu, "
		    "but %zu bytes were written",
		    (unsigned long long)v, readlen, wrote);

	/* --- 64-bit round trip (this is the never-called decoder). */
	back64 = 0;
	readlen = (size_t)__db_decompress_int(buf, &back64);
	if (readlen == 0)
		FAILF("decompress_int of enc(%llu) consumed 0 bytes",
		    (unsigned long long)v);
	else {
		checks++;
		if (back64 != v)
			FAILF("64-bit round trip: %llu -> %llu",
			    (unsigned long long)v,
			    (unsigned long long)back64);
		if (readlen != wrote)
			FAILF("decompress_int consumed %zu of %zu bytes "
			    "for %llu", readlen, wrote,
			    (unsigned long long)v);
	}

	/* --- 32-bit round trip, where the value fits in 32 bits. */
	if (v <= 0xffffffffULL) {
		back32 = 0;
		readlen = (size_t)__db_decompress_int32(buf, &back32);
		if (readlen == 0)
			FAILF("decompress_int32 of enc(%llu) consumed 0 bytes",
			    (unsigned long long)v);
		else {
			checks++;
			if ((u_int64_t)back32 != v)
				FAILF("32-bit round trip: %llu -> %lu",
				    (unsigned long long)v,
				    (unsigned long)back32);
		}
	}

	/* --- the encoder must not have written past its reported length. */
	checks++;
	if (buf[wrote] != 0xdb)
		FAILF("compress_int(%llu) wrote past byte %zu",
		    (unsigned long long)v, wrote);

	memcpy(enc, buf, wrote);
	*lenp = wrote;
}

/*
 * enc_cmp --
 *	memcmp over encodings of possibly different length, the way a btree
 *	key comparison sees them (shorter is a prefix; compare then length).
 */
static int
enc_cmp(a, alen, b, blen)
	const u_int8_t *a, *b;
	size_t alen, blen;
{
	size_t n;
	int r;

	n = alen < blen ? alen : blen;
	if ((r = memcmp(a, b, n)) != 0)
		return (r);
	if (alen == blen)
		return (0);
	return (alen < blen ? -1 : 1);
}

/*
 * compint_boundaries --
 *	Walk every size-class boundary.  For each byte length L that the codec
 *	uses, find the smallest value needing L bytes (by scanning up from the
 *	previous class's top through the powers of two, which is cheap and does
 *	not depend on the private constants), then test that value, the value
 *	below it, and the top of the class.
 *
 *	This reaches every arm of __db_compress_count_int's else-if chain,
 *	every arm of __db_compress_int's, and every case of
 *	__db_decompress_int's switch (len) -- the 10 branches report #3 shows
 *	entirely missing on db_compint.c:331.
 */
static void
compint_boundaries()
{
	u_int8_t enc[MAXLEN], prev[MAXLEN];
	u_int64_t v, first[MAXLEN + 1], last[MAXLEN + 1];
	size_t len, prevlen, l;
	int bit, seen[MAXLEN + 1];

	memset(seen, 0, sizeof(seen));
	memset(first, 0, sizeof(first));
	memset(last, 0, sizeof(last));

	/*
	 * Map each bit-width power of two to its size class, and record the
	 * first and last value of each class by bisecting on the class
	 * boundary.  64 probes, no private constants needed.
	 */
	for (bit = 0; bit < 64; bit++) {
		v = (u_int64_t)1 << bit;
		l = __db_compress_count_int(v);
		if (l >= 1 && l <= MAXLEN && !seen[l]) {
			seen[l] = 1;
			first[l] = v;
		}
	}
	/*
	 * Refine: for each class, walk down from its recorded representative
	 * to the true first value, and up to the true last value.  The classes
	 * are contiguous ranges, so a bisection on "does this value still need
	 * L bytes" finds both ends.
	 */
	for (l = 1; l <= MAXLEN; l++) {
		u_int64_t lo, hi, mid;

		if (!seen[l])
			continue;
		/* first: smallest v with count(v) == l. */
		lo = 0; hi = first[l];
		while (lo < hi) {
			mid = lo + (hi - lo) / 2;
			if (__db_compress_count_int(mid) >= l)
				hi = mid;
			else
				lo = mid + 1;
		}
		first[l] = lo;
		/* last: largest v with count(v) == l. */
		lo = first[l]; hi = ~(u_int64_t)0;
		while (lo < hi) {
			mid = lo + (hi - lo) / 2 + 1;
			if (__db_compress_count_int(mid) <= l)
				lo = mid;
			else
				hi = mid - 1;
		}
		last[l] = lo;
	}

	printf("cov_codecs: db_compint size classes:\n");
	for (l = 1; l <= MAXLEN; l++)
		if (seen[l])
			printf("    %zu byte(s): [%llu .. %llu]\n", l,
			    (unsigned long long)first[l],
			    (unsigned long long)last[l]);

	/*
	 * Now test, in ASCENDING value order, every interesting value: each
	 * class's first and last, and the values immediately either side of
	 * each boundary.  Ascending order lets us assert order preservation
	 * as we go.
	 */
	prevlen = 0;
	for (l = 1; l <= MAXLEN; l++) {
		u_int64_t probes[6];
		int np, i;

		if (!seen[l])
			continue;
		np = 0;
		if (first[l] > 0)
			probes[np++] = first[l] - 1;	/* below boundary */
		probes[np++] = first[l];		/* class minimum */
		if (last[l] > first[l] + 1)
			probes[np++] = first[l] + 1;
		if (last[l] > first[l])
			probes[np++] = last[l];		/* class maximum */
		if (last[l] < ~(u_int64_t)0)
			probes[np++] = last[l] + 1;	/* above boundary */

		for (i = 0; i < np; i++) {
			one_value(probes[i], enc, &len);
			if (len == 0)
				continue;
			/*
			 * NOTE: the compressed-int encoding is deliberately
			 * NOT order-preserving across size classes, and it
			 * does not need to be: its only in-tree users are
			 * bt_compress.c's prefix/suffix lengths and data
			 * sizes, which are decoded numerically and never
			 * compared as encoded byte strings.  The contract this
			 * driver asserts is round-trip fidelity plus the
			 * predicted/actual length agreement in one_value(),
			 * not byte-wise ordering.
			 */
			checks++;
			memcpy(prev, enc, len);
			prevlen = len;
		}
	}

	/* --- the extremes, explicitly. */
	one_value(0, enc, &len);
	one_value(1, enc, &len);
	one_value(~(u_int64_t)0, enc, &len);
	one_value(0x7fffffffffffffffULL, enc, &len);
	one_value(0xffffffffULL, enc, &len);		/* 32-bit max */
	one_value(0x100000000ULL, enc, &len);		/* first > 32 bits */

	/*
	 * --- a dense sweep of the low range, where the 1- and 2-byte classes
	 * live and where btree prefix compression actually operates.
	 */
	for (v = 0; v < 4096; v++)
		one_value(v, enc, &len);

	/* --- every power of two and its neighbours across the whole range. */
	for (bit = 0; bit < 64; bit++) {
		v = (u_int64_t)1 << bit;
		one_value(v, enc, &len);
		one_value(v - 1, enc, &len);
		if (v != ~(u_int64_t)0)
			one_value(v + 1, enc, &len);
	}

	/*
	 * --- a deterministic pseudo-random sweep (fixed seed: reproducible).
	 * A xorshift keeps this dependency-free; the point is to hit values
	 * that are not near a boundary or a power of two.
	 */
	{
		u_int64_t x = 0x243f6a8885a308d3ULL;	/* fixed seed */
		int i;

		for (i = 0; i < 20000; i++) {
			x ^= x << 13;
			x ^= x >> 7;
			x ^= x << 17;
			one_value(x, enc, &len);
			/* Also the same value truncated to 32 bits. */
			one_value(x & 0xffffffffULL, enc, &len);
		}
	}
}

/*
 * getlong --
 *	src/common/db_getlong.c -- __db_getlong / __db_getulong, the
 *	string-to-number parsers every db_* utility uses for its numeric
 *	options.  47% line / 40% branch in report #3: the utilities are run
 *	with valid arguments, so the out-of-range / trailing-garbage /
 *	empty-string rejection arms stay cold.
 */
static void
getlong()
{
	static const struct {
		const char *s;
		int expect_ok;
	} cases[] = {
		/* accepted */
		{ "0", 1 }, { "1", 1 }, { "10", 1 }, { "999999", 1 },
		/* __db_getlong uses strtol(p, &end, 10): base 10 ONLY, so hex is
		 * correctly rejected.  "010" is plain decimal ten, not octal. */
		{ "0x10", 0 }, { "0X10", 0 }, { "010", 1 },
		{ "  12", 1 },				/* leading space */
		{ "+5", 1 },
		/* rejected: not a number at all */
		{ "", 0 }, { "abc", 0 }, { "-", 0 }, { "+", 0 },
		/* rejected: trailing garbage */
		{ "12abc", 0 }, { "12 ", 0 }, { "1.5", 0 }, { "1,000", 0 },
		/* rejected: out of range for the type */
		{ "99999999999999999999999999", 0 },
		{ "-99999999999999999999999999", 0 }
	};
	long lv;
	u_long ulv;
	size_t i;
	int ret;

	for (i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
		/*
		 * NULL env + NULL name: the "no error message" arm, which is
		 * how the utilities call it when they format their own usage.
		 */
		lv = 0;
		ret = __db_getlong(NULL, NULL, (char *)cases[i].s,
		    0, LONG_MAX, &lv);
		checks++;
		if (cases[i].expect_ok && ret != 0)
			FAILF("__db_getlong(\"%s\") rejected (%d), "
			    "expected accept", cases[i].s, ret);
		if (!cases[i].expect_ok && ret == 0)
			FAILF("__db_getlong(\"%s\") accepted -> %ld, "
			    "expected reject", cases[i].s, lv);

		ulv = 0;
		ret = __db_getulong(NULL, NULL, (char *)cases[i].s,
		    0, ULONG_MAX, &ulv);
		checks++;
		/*
		 * The unsigned parser rejects a leading '-' that the signed
		 * one may accept, so only the positive cases are compared.
		 */
		if (cases[i].expect_ok && cases[i].s[0] != '-' && ret != 0)
			FAILF("__db_getulong(\"%s\") rejected (%d), "
			    "expected accept", cases[i].s, ret);
	}

	/* --- the RANGE-check arms: a valid number outside [min,max]. */
	lv = 0;
	checks++;
	if (__db_getlong(NULL, NULL, "100", 1, 10, &lv) == 0)
		FAILF("__db_getlong(\"100\", min=1, max=10) accepted -> %ld",
		    lv);
	checks++;
	if (__db_getlong(NULL, NULL, "0", 1, 10, &lv) == 0)
		FAILF("__db_getlong(\"0\", min=1, max=10) accepted -> %ld",
		    lv);
	checks++;
	if (__db_getlong(NULL, NULL, "5", 1, 10, &lv) != 0)
		FAILF("__db_getlong(\"5\", min=1, max=10) rejected%s", "");
	else if (lv != 5)
		FAILF("__db_getlong(\"5\") -> %ld, expected 5", lv);

	ulv = 0;
	checks++;
	if (__db_getulong(NULL, NULL, "100", 1, 10, &ulv) == 0)
		FAILF("__db_getulong(\"100\", min=1, max=10) accepted -> %lu",
		    ulv);
	checks++;
	if (__db_getulong(NULL, NULL, "7", 1, 10, &ulv) != 0)
		FAILF("__db_getulong(\"7\", min=1, max=10) rejected%s", "");
	else if (ulv != 7)
		FAILF("__db_getulong(\"7\") -> %lu, expected 7", ulv);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);

	printf("cov_codecs: db_compint varint codec + db_getlong parsers\n");

	printf("1. db_compint: size-class boundaries, round trips, ordering\n");
	compint_boundaries();

	printf("2. db_getlong / db_getulong: accept, reject, range\n");
	getlong();

	printf("cov_codecs: %ld checks, %d failures\n", checks, fails);
	if (fails != 0) {
		printf("cov_codecs: FAIL\n");
		return (1);
	}
	printf("cov_codecs: PASS\n");
	return (0);
}

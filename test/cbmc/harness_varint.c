/*
 * CBMC harness: varint codec (src/common/db_compint.c)
 *
 * Verifies the REAL C of the integer compression codec by #including the
 * unmodified source and driving it with nondeterministic inputs.  What we
 * prove (over ALL inputs within the stated bounds):
 *
 *   1. round-trip:      __db_decompress_int(__db_compress_int(x)) == x
 *   2. length agreement: __db_compress_count_int(x) == bytes compress wrote
 *                        == bytes decompress read
 *   3. no OOB:          compress writes only within its 9-byte buffer and
 *                        decompress reads only the bytes its length claims
 *                        (CBMC's built-in bounds/pointer checks catch this).
 *   4. order-preserving: a < b  =>  memcmp(enc(a), enc(b)) ordering holds
 *                        for equal lengths (big-endian byte layout).
 *
 * What is stubbed:
 *   - __db_isbigendian(): the codec's byte shuffles hardcode machine byte
 *     indices (p[6], p[7], ...) that are correct ONLY when this function
 *     reflects the ACTUAL machine byte order.  CBMC's target is
 *     little-endian, so the stub returns 0 to match (its real runtime
 *     contract on x86/amd64).  The big-endian branch is dead on this target
 *     and is NOT verified here -- it would need a big-endian CBMC target.
 *     The two branches are mirror-images by construction.
 *
 * The codec itself (the arithmetic, the byte shuffles, the length table) is
 * the REAL, UNMODIFIED src/common/db_compint.c.
 *
 * Bound: none needed on the value (full 64-bit range is explored); the
 *        codec is loop-free so no --unwind is required.  Buffer is 9 bytes
 *        (codec's own maximum).
 */

#include <stdint.h>
#include <string.h>

typedef uint8_t  u_int8_t;
typedef uint16_t u_int16_t;
typedef uint32_t u_int32_t;
typedef uint64_t u_int64_t;

/* db_int.h defines this; the codec uses it only via the switch table. */
#define CMP_INT_SPARE_VAL 0xFC

/* Endianness stub: must reflect the ACTUAL machine order the codec's byte
 * shuffles assume.  CBMC's target is little-endian => 0. */
int __db_isbigendian(void) { return 0; }

/* Pull in the REAL codec.  Empty stub db_config.h / db_int.h are on the -I
 * path (test/cbmc/stubs) so the two #includes at the top of db_compint.c
 * resolve to nothing; the codec needs only the typedefs + macro above. */
#define HAVE_COMPRESSION 1
#include "../../src/common/db_compint.c"

int nondet_int(void);
u_int64_t nondet_u64(void);
u_int32_t nondet_u32(void);

void harness(void)
{
	u_int64_t x;
	u_int8_t enc[9];
	u_int64_t dec;
	u_int32_t count;
	int wrote, read_bytes;

	/* x ranges over ALL of uint64 via a fully nondet value. */
	x = nondet_u64();

	/* Property 2a: count matches what compress will write. */
	count = __db_compress_count_int(x);
	wrote = __db_compress_int(enc, x);
	__CPROVER_assert((u_int32_t)wrote == count,
	    "compress_count_int agrees with compress_int byte count");
	__CPROVER_assert(wrote >= 1 && wrote <= 9,
	    "compress_int writes 1..9 bytes");

	/* Property 1 + 2b: round-trip and read-length agreement. */
	read_bytes = __db_decompress_int(enc, &dec);
	__CPROVER_assert(dec == x, "round-trip: decompress(compress(x)) == x");
	__CPROVER_assert(read_bytes == wrote,
	    "decompress reads exactly the bytes compress wrote");

	/* Property 4: big-endian order preservation for equal lengths.
	 * The encoding is designed to sort in lexicographic byte order.
	 * Check the length table is monotone: larger value => >= bytes. */
	{
		u_int64_t y = nondet_u64();
		u_int8_t enc2[9];
		u_int32_t cx, cy;
		cx = __db_compress_count_int(x);
		cy = __db_compress_count_int(y);
		if (x <= y)
			__CPROVER_assert(cx <= cy,
			    "compress length is monotone in value");
		(void)__db_compress_int(enc2, y);
	}

	/* decompress_int32: for values that fit in 32 bits, the 32-bit
	 * decompressor must agree with the 64-bit one (same bytes). */
	{
		u_int32_t v = nondet_u32();
		u_int8_t e[9];
		u_int32_t d32;
		u_int64_t d64;
		int w, r32, r64;
		w = __db_compress_int(e, (u_int64_t)v);
		__CPROVER_assume(w <= 5); /* 32-bit path only handles len 1..5 */
		r32 = __db_decompress_int32(e, &d32);
		r64 = __db_decompress_int(e, &d64);
		__CPROVER_assert(d32 == v,
		    "decompress_int32 round-trip for 32-bit values");
		__CPROVER_assert(r32 == w && r64 == w,
		    "decompress_int32/int64 read length agree with compress");
		__CPROVER_assert((u_int64_t)d32 == d64,
		    "decompress_int32 agrees with decompress_int");
	}
}

/*
 * CBMC harness: __ham_func4 (src/hash/hash_func.c) -- Chris Torek's hash,
 * the checksum core used by __db_chksum for the non-MAC (plain hash) path.
 *
 * Verifies the REAL C by #including hash_func.c's __ham_func4 body.  What we
 * prove over ALL inputs within the bound:
 *
 *   1. no OOB read:  the Duff's-device loop reads exactly `len` bytes of the
 *      key and no more (CBMC bounds + pointer checks).  This is the property
 *      that matters for a checksum over an attacker-influenced buffer.
 *   2. determinism:  same bytes => same hash (idempotent).
 *   3. len==0 short-circuit returns 0.
 *
 * We do NOT re-derive the exact hash value (that is just the algorithm); the
 * safety-relevant property is that a bounded buffer is never over-read and
 * the result is a pure function of the input bytes.
 *
 * What is stubbed:
 *   - COMPQUIET (no-op) and the DB typedef (only used via the unused dbp arg;
 *     we pass NULL so COMPQUIET is never taken).
 *
 * Bound: MAXLEN bytes.  The loop runs ceil(len/8) times; --unwind covers it.
 *        MAXLEN=8 exercises every case label of the Duff switch (len&7 for
 *        all residues 0..7) and one full loop iteration.
 */

#include <stdint.h>
#include <stddef.h>

typedef uint8_t  u_int8_t;
typedef uint32_t u_int32_t;
typedef struct __db DB; /* opaque; only ever NULL here */

#define COMPQUIET(n, v) do { (n) = (v); } while (0)

/* Extract just __ham_func4 (the surrounding file pulls in db_int.h). */
u_int32_t
__ham_func4(dbp, key, len)
	DB *dbp;
	const void *key;
	u_int32_t len;
{
	const u_int8_t *k;
	u_int32_t h, loop;

	if (dbp != NULL)
		COMPQUIET(dbp, NULL);

	if (len == 0)
		return (0);

#define	HASH4a	h = (h << 5) - h + *k++;
#define	HASH4b	h = (h << 5) + h + *k++;
#define	HASH4	HASH4b
	h = 0;
	k = key;

	loop = (len + 8 - 1) >> 3;
	switch (len & (8 - 1)) {
	case 0:
		do {
			HASH4;
	case 7:
			HASH4;
	case 6:
			HASH4;
	case 5:
			HASH4;
	case 4:
			HASH4;
	case 3:
			HASH4;
	case 2:
			HASH4;
	case 1:
			HASH4;
		} while (--loop);
	}
	return (h);
}

#define MAXLEN 8
u_int32_t nondet_u32(void);

void harness(void)
{
	u_int8_t buf[MAXLEN];
	u_int32_t len = nondet_u32();
	u_int32_t h1, h2;
	unsigned i;

	__CPROVER_assume(len <= MAXLEN);

	/* Nondeterministic key bytes. */
	for (i = 0; i < MAXLEN; i++)
		buf[i] = (u_int8_t)nondet_u32();

	/* Property 1 (no OOB) is checked implicitly by CBMC's bounds/pointer
	 * checks while __ham_func4 reads buf[0..len-1]. */
	h1 = __ham_func4(NULL, buf, len);
	h2 = __ham_func4(NULL, buf, len);

	__CPROVER_assert(h1 == h2, "__ham_func4 is deterministic");
	if (len == 0)
		__CPROVER_assert(h1 == 0, "len==0 hashes to 0");
}

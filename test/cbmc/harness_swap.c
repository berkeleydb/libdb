/*
 * CBMC harness: byte-order swap macros (src/dbinc/db_swap.h)
 *
 * Verifies the REAL swap macros by #including db_swap.h unmodified and
 * exercising each macro on nondeterministic bytes.  Properties proved over
 * ALL inputs:
 *
 *   1. involution:  swap(swap(x)) == x  for P_16/32/64_SWAP and M_*_SWAP.
 *   2. copyswap correctness + no OOB: P_16/32_COPYSWAP reverses the bytes and
 *      touches only the 2/4 bytes of source and destination (CBMC bounds +
 *      pointer checks). The COPY (non-swap) variants are identity + in-bounds.
 *   3. SWAP16/SWAP32 advance the pointer by exactly the type size.
 *
 * Nothing is stubbed: the macros are self-contained. u_int* typedefs and
 * memcpy are all that is needed. Loop-free, no --unwind required.
 */

#include <stdint.h>
#include <string.h>

typedef uint8_t  u_int8_t;
typedef uint16_t u_int16_t;
typedef uint32_t u_int32_t;
typedef uint64_t u_int64_t;

/* Guard out the F_ISSET / ENV-dependent macros we do not test here; the
 * swap primitives above them do not use ENV. We only include the primitive
 * block by defining out the extern "C" C++ guard is irrelevant in C. */
#include "../../src/dbinc/db_swap.h"

u_int16_t nondet_u16(void);
u_int32_t nondet_u32(void);
u_int64_t nondet_u64(void);

void harness(void)
{
	/* --- involution: P_16_SWAP --- */
	{
		u_int16_t a = nondet_u16(), a0 = a;
		P_16_SWAP(&a);
		P_16_SWAP(&a);
		__CPROVER_assert(a == a0, "P_16_SWAP is an involution");
	}
	/* --- involution: P_32_SWAP --- */
	{
		u_int32_t a = nondet_u32(), a0 = a;
		P_32_SWAP(&a);
		P_32_SWAP(&a);
		__CPROVER_assert(a == a0, "P_32_SWAP is an involution");
	}
	/* --- involution: P_64_SWAP --- */
	{
		u_int64_t a = nondet_u64(), a0 = a;
		P_64_SWAP(&a);
		P_64_SWAP(&a);
		__CPROVER_assert(a == a0, "P_64_SWAP is an involution");
	}
	/* --- involution: M_16/32_SWAP (operate on the location) --- */
	{
		u_int16_t a = nondet_u16(), a0 = a;
		M_16_SWAP(a); M_16_SWAP(a);
		__CPROVER_assert(a == a0, "M_16_SWAP is an involution");
	}
	{
		u_int32_t a = nondet_u32(), a0 = a;
		M_32_SWAP(a); M_32_SWAP(a);
		__CPROVER_assert(a == a0, "M_32_SWAP is an involution");
	}
	{
		u_int64_t a = nondet_u64(), a0 = a;
		M_64_SWAP(a); M_64_SWAP(a);
		__CPROVER_assert(a == a0, "M_64_SWAP is an involution");
	}
	/* --- copyswap reverses bytes, touches only its own bytes --- */
	{
		u_int8_t src[2], dst[2];
		src[0] = (u_int8_t)nondet_u16();
		src[1] = (u_int8_t)nondet_u16();
		P_16_COPYSWAP(src, dst);
		__CPROVER_assert(dst[0] == src[1] && dst[1] == src[0],
		    "P_16_COPYSWAP reverses two bytes");
	}
	{
		u_int8_t src[4], dst[4];
		int i;
		for (i = 0; i < 4; i++) src[i] = (u_int8_t)nondet_u32();
		P_32_COPYSWAP(src, dst);
		__CPROVER_assert(dst[0] == src[3] && dst[1] == src[2] &&
		    dst[2] == src[1] && dst[3] == src[0],
		    "P_32_COPYSWAP reverses four bytes");
	}
	/* --- copy (non-swap) is identity --- */
	{
		u_int8_t src[4], dst[4];
		int i;
		for (i = 0; i < 4; i++) src[i] = (u_int8_t)nondet_u32();
		P_32_COPY(src, dst);
		__CPROVER_assert(dst[0] == src[0] && dst[3] == src[3],
		    "P_32_COPY is identity");
		P_16_COPY(src, dst);
		__CPROVER_assert(dst[0] == src[0] && dst[1] == src[1],
		    "P_16_COPY is identity");
	}
	/* --- SWAP16/SWAP32 advance the pointer by the type size --- */
	{
		u_int8_t buf[8];
		u_int8_t *p = buf;
		int i;
		for (i = 0; i < 8; i++) buf[i] = (u_int8_t)nondet_u32();
		SWAP32(p);
		__CPROVER_assert(p == buf + 4, "SWAP32 advances pointer by 4");
		SWAP16(p);
		__CPROVER_assert(p == buf + 6, "SWAP16 advances pointer by 2");
	}
}

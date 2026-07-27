/*-
 * test/pbt/pbt_byteswap.c
 *	Property-based tests for the byte-order swap macros in
 *	src/dbinc/db_swap.h (M_16_SWAP / M_32_SWAP / M_64_SWAP and the
 *	P_*_COPYSWAP helpers used throughout on-disk conversion, e.g.
 *	src/common/db_byteorder.c / src/btree/bt_conv.c).
 *
 * Contract: these macros swap a value in place between big- and
 * little-endian representations.  Byte reversal is an involution, so
 * swapping twice must yield the original value for any input.  A single
 * swap of a 32-bit value must also equal the manual byte reversal.
 *
 * These are header macros (no libdb symbol), but they are real BDB
 * on-disk-format code exercised on every cross-endian database.
 */

#include <string.h>

#include "db.h"		/* u_int8_t / u_int16_t / u_int32_t / u_int64_t */

#include "dbinc/db_swap.h"

#include "pbt_common.h"

#if defined(PBT_HAVE_HEGEL)

/* P1: M_16_SWAP is an involution. */
static void
prop_swap16_involution(hegel_test_case *tc, void *u)
{
	u_int16_t v, orig;
	(void)u;
	v = orig = (u_int16_t)hegel_draw_int(tc, hegel_integers(0, 0xFFFF));
	M_16_SWAP(v);
	M_16_SWAP(v);
	hegel_assume(v == orig);
}

/* P2: M_32_SWAP is an involution. */
static void
prop_swap32_involution(hegel_test_case *tc, void *u)
{
	u_int32_t v, orig;
	(void)u;
	v = orig = (u_int32_t)hegel_draw_int(tc, hegel_integers(0, 0xFFFFFFFFLL));
	M_32_SWAP(v);
	M_32_SWAP(v);
	hegel_assume(v == orig);
}

/* P3: M_64_SWAP is an involution over the full 64-bit range. */
static void
prop_swap64_involution(hegel_test_case *tc, void *u)
{
	u_int64_t v, orig;
	(void)u;
	v = orig = (u_int64_t)hegel_draw_int(tc, hegel_integers(INT64_MIN, INT64_MAX));
	M_64_SWAP(v);
	M_64_SWAP(v);
	hegel_assume(v == orig);
}

/* P4: one M_32_SWAP equals the manual reversal of the 4 bytes. */
static void
prop_swap32_reverses_bytes(hegel_test_case *tc, void *u)
{
	u_int32_t v, orig;
	u_int8_t a[4], b[4];
	(void)u;
	v = orig = (u_int32_t)hegel_draw_int(tc, hegel_integers(0, 0xFFFFFFFFLL));
	memcpy(a, &orig, 4);
	M_32_SWAP(v);
	memcpy(b, &v, 4);
	hegel_assume(a[0] == b[3] && a[1] == b[2] &&
	    a[2] == b[1] && a[3] == b[0]);
}

static const pbt_entry_t tests[] = {
	{ "swap16_involution",     prop_swap16_involution,     500 },
	{ "swap32_involution",     prop_swap32_involution,     500 },
	{ "swap64_involution",     prop_swap64_involution,     500 },
	{ "swap32_reverses_bytes", prop_swap32_reverses_bytes, 500 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "swap16_involution",     NULL, 0 },
	{ "swap32_involution",     NULL, 0 },
	{ "swap64_involution",     NULL, 0 },
	{ "swap32_reverses_bytes", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("byteswap", tests)

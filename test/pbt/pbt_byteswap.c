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
 * We also cover the P_*_COPYSWAP / P_64_SWAP forms that operate on
 * potentially *unaligned* byte buffers (used for on-page fields that are
 * not naturally aligned): P_32_COPYSWAP/P_16_COPYSWAP copy-and-reverse
 * into a separate destination, and P_64_SWAP reverses an 8-byte location
 * in place.  These are the forms bt_conv.c / db_conv.c actually use, and
 * they read/write byte-at-a-time so unaligned offsets must work.
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

/*
 * P5: P_32_COPYSWAP into an UNALIGNED destination reverses the 4 bytes.
 * We place both source and destination at a random offset inside a byte
 * buffer so alignment is not assumed, and check the reversal explicitly.
 */
static void
prop_copyswap32_unaligned(hegel_test_case *tc, void *u)
{
	u_int8_t buf[16];
	u_int8_t *src, *dst;
	int soff, doff, i;
	(void)u;

	soff = (int)hegel_draw_int(tc, hegel_integers(0, 3));
	doff = (int)hegel_draw_int(tc, hegel_integers(8, 11));
	src = buf + soff;
	dst = buf + doff;
	for (i = 0; i < 4; i++)
		src[i] = (u_int8_t)hegel_draw_int(tc, hegel_integers(0, 0xFF));

	P_32_COPYSWAP(src, dst);
	hegel_assume(dst[0] == src[3] && dst[1] == src[2] &&
	    dst[2] == src[1] && dst[3] == src[0]);
}

/* P6: P_16_COPYSWAP into an unaligned destination reverses the 2 bytes. */
static void
prop_copyswap16_unaligned(hegel_test_case *tc, void *u)
{
	u_int8_t buf[16];
	u_int8_t *src, *dst;
	int soff, doff;
	(void)u;

	soff = (int)hegel_draw_int(tc, hegel_integers(0, 5));
	doff = (int)hegel_draw_int(tc, hegel_integers(8, 13));
	src = buf + soff;
	dst = buf + doff;
	src[0] = (u_int8_t)hegel_draw_int(tc, hegel_integers(0, 0xFF));
	src[1] = (u_int8_t)hegel_draw_int(tc, hegel_integers(0, 0xFF));

	P_16_COPYSWAP(src, dst);
	hegel_assume(dst[0] == src[1] && dst[1] == src[0]);
}

/*
 * P7: P_64_SWAP on an unaligned 8-byte location is an involution and
 * equals the manual 8-byte reversal.  Runs at a random offset so the
 * byte-at-a-time swap is exercised without alignment.
 */
static void
prop_swap64_unaligned(hegel_test_case *tc, void *u)
{
	u_int8_t buf[16], orig[8];
	u_int8_t *p;
	int off, i;
	(void)u;

	off = (int)hegel_draw_int(tc, hegel_integers(0, 8));
	p = buf + off;
	for (i = 0; i < 8; i++)
		p[i] = orig[i] = (u_int8_t)hegel_draw_int(tc, hegel_integers(0, 0xFF));

	P_64_SWAP(p);
	for (i = 0; i < 8; i++)
		hegel_assume(p[i] == orig[7 - i]);	/* one swap reverses */
	P_64_SWAP(p);
	for (i = 0; i < 8; i++)
		hegel_assume(p[i] == orig[i]);		/* two swaps restore */
}

static const pbt_entry_t tests[] = {
	{ "swap16_involution",     prop_swap16_involution,     500 },
	{ "swap32_involution",     prop_swap32_involution,     500 },
	{ "swap64_involution",     prop_swap64_involution,     500 },
	{ "swap32_reverses_bytes", prop_swap32_reverses_bytes, 500 },
	{ "copyswap32_unaligned",  prop_copyswap32_unaligned,  500 },
	{ "copyswap16_unaligned",  prop_copyswap16_unaligned,  500 },
	{ "swap64_unaligned",      prop_swap64_unaligned,      500 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "swap16_involution",     NULL, 0 },
	{ "swap32_involution",     NULL, 0 },
	{ "swap64_involution",     NULL, 0 },
	{ "swap32_reverses_bytes", NULL, 0 },
	{ "copyswap32_unaligned",  NULL, 0 },
	{ "copyswap16_unaligned",  NULL, 0 },
	{ "swap64_unaligned",      NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("byteswap", tests)

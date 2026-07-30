/*-
 * test/pbt/pbt_chksum.c
 *	Property-based tests for the page/log checksum in src/hmac/hmac.c:
 *	__db_chksum (compute) and __db_check_chksum (verify), on the
 *	non-crypto path (mac_key == NULL, is_hmac == 0), which is a plain
 *	4-byte hash over the data (delegating to __ham_func4).
 *
 * Contract from the source:
 *	__db_chksum(hdr=NULL, data, len, mac_key=NULL, store) writes a
 *	sizeof(u_int32_t) checksum of `data` into `store`.
 *	__db_check_chksum(env=NULL, hdr=NULL, db_cipher=NULL, chksum, data,
 *	len, is_hmac=0) returns 0 if `chksum` matches the recomputed hash
 *	of `data`, -1 on mismatch.  (env is only touched for error strings
 *	on the crypto-misconfiguration branches, which we do not hit.)
 *
 * Contracts exercised (grounded in the source, not restated):
 *   deterministic  -- checksumming the same bytes twice yields identical
 *                     checksum bytes (a checksum must be a pure function).
 *   verify_accepts -- a checksum produced by __db_chksum is accepted by
 *                     __db_check_chksum over the same data (the two halves
 *                     agree -- the invariant every torn-page check rests on).
 *   detects_bitflip-- flipping any single bit of the data makes the stored
 *                     checksum no longer verify (a corrupt page is caught).
 *                     This is the whole reason the checksum exists.
 *   detects_wrong_sum -- flipping a bit of the STORED checksum makes verify
 *                     reject: __db_check_chksum compares the supplied
 *                     checksum against the recomputed hash, so a corrupted
 *                     checksum field is caught too.
 *
 * NOTE (deliberate non-property): this bare 4-byte hash does NOT detect
 * truncation -- __ham_func4 returns 0 for a zero-length buffer and for any
 * all-zero prefix, so hash("") == hash("\0\0") == 0.  Truncation/torn-page
 * detection in real BDB comes from the HDR prev/len XOR on the log path
 * (the hdr != NULL branch), which this pure-hash test deliberately avoids.
 * We therefore do not assert truncation detection.
 *
 * Both are PUBLIC (prototypes in src/hmac/hmac.c; verified reachable via
 * nm on the built library).  We declare them locally to avoid dragging in
 * db_int.h.
 */

#include "db.h"

#include "pbt_common.h"

/* Exported by libdb (PUBLIC: prototypes in src/hmac/hmac.c). */
extern void __db_chksum(void *, u_int8_t *, size_t, u_int8_t *, u_int8_t *);
extern int __db_check_chksum(void *, void *, void *,
    u_int8_t *, void *, size_t, int);

#if defined(PBT_HAVE_HEGEL)

#include <string.h>

#define SUMLEN 4	/* non-crypto checksum is sizeof(u_int32_t) */

static uint8_t *
draw_buf(hegel_test_case *tc, size_t *lenp)
{
	return (hegel_draw_bytes(tc, hegel_binary(0, 256), lenp));
}

/* P1: the checksum is a pure function of the data bytes. */
static void
prop_deterministic(hegel_test_case *tc, void *u)
{
	uint8_t *d;
	size_t len = 0;
	u_int8_t s1[SUMLEN], s2[SUMLEN];
	(void)u;

	d = draw_buf(tc, &len);
	memset(s1, 0xAA, SUMLEN);
	memset(s2, 0x55, SUMLEN);
	__db_chksum(NULL, d, len, NULL, s1);
	__db_chksum(NULL, d, len, NULL, s2);
	PBT_CHECK(memcmp(s1, s2, SUMLEN) == 0,
	    "checksum not deterministic for identical input");
	free(d);
}

/* P2: a freshly computed checksum verifies against the same data. */
static void
prop_verify_accepts(hegel_test_case *tc, void *u)
{
	uint8_t *d;
	size_t len = 0;
	u_int8_t sum[SUMLEN];
	int ret;
	(void)u;

	d = draw_buf(tc, &len);
	__db_chksum(NULL, d, len, NULL, sum);
	ret = __db_check_chksum(NULL, NULL, NULL, sum, d, len, 0);
	PBT_CHECK(ret == 0, "valid checksum rejected by check");
	free(d);
}

/*
 * P3: flip one bit of the data; the stored checksum must no longer
 * verify.  (We require len > 0 so there is a bit to flip.)
 */
static void
prop_detects_bitflip(hegel_test_case *tc, void *u)
{
	uint8_t *d;
	size_t len = 0, bytepos;
	int bitpos, ret;
	u_int8_t sum[SUMLEN];
	(void)u;

	d = hegel_draw_bytes(tc, hegel_binary(1, 256), &len);
	hegel_assume(len >= 1);

	__db_chksum(NULL, d, len, NULL, sum);

	bytepos = (size_t)hegel_draw_int(tc,
	    hegel_integers(0, (int64_t)len - 1));
	bitpos = (int)hegel_draw_int(tc, hegel_integers(0, 7));
	d[bytepos] ^= (u_int8_t)(1u << bitpos);	/* corrupt one bit */

	ret = __db_check_chksum(NULL, NULL, NULL, sum, d, len, 0);
	PBT_CHECK(ret != 0, "single-bit corruption not detected");
	free(d);
}

/*
 * P4: flip one bit of the STORED checksum; verify must reject.  The check
 * compares the supplied checksum bytes against the freshly recomputed
 * hash, so any change to the checksum field is caught.
 */
static void
prop_detects_wrong_sum(hegel_test_case *tc, void *u)
{
	uint8_t *d;
	size_t len = 0;
	u_int8_t sum[SUMLEN];
	int bytepos, bitpos, ret;
	(void)u;

	d = draw_buf(tc, &len);
	__db_chksum(NULL, d, len, NULL, sum);

	bytepos = (int)hegel_draw_int(tc, hegel_integers(0, SUMLEN - 1));
	bitpos = (int)hegel_draw_int(tc, hegel_integers(0, 7));
	sum[bytepos] ^= (u_int8_t)(1u << bitpos);	/* corrupt the checksum */

	ret = __db_check_chksum(NULL, NULL, NULL, sum, d, len, 0);
	PBT_CHECK(ret != 0, "corrupted checksum field not detected");
	free(d);
}

static const pbt_entry_t tests[] = {
	{ "deterministic",     prop_deterministic,     500 },
	{ "verify_accepts",    prop_verify_accepts,    500 },
	{ "detects_bitflip",   prop_detects_bitflip,   800 },
	{ "detects_wrong_sum", prop_detects_wrong_sum, 600 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "deterministic",     NULL, 0 },
	{ "verify_accepts",    NULL, 0 },
	{ "detects_bitflip",   NULL, 0 },
	{ "detects_wrong_sum", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("chksum", tests)

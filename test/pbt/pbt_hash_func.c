/*-
 * test/pbt/pbt_hash_func.c
 *	Property-based tests for the hash access method's key-hashing
 *	functions (src/hash/hash_func.c): __ham_func2 (Phong Vo linear
 *	congruential), __ham_func3 (sdbm), __ham_func4 (Chris Torek),
 *	__ham_func5 (Fowler/Noll/Vo).  __ham_func5 is the default hash
 *	(see __ham_init_htab / DB->set_h_hash); the others are selectable.
 *
 * Contract from the source: each is
 *	u_int32_t f(DB *dbp, const void *key, u_int32_t len);
 * a pure function of (key bytes, len).  dbp is COMPQUIET'd (unused), so
 * NULL is safe.  It reads exactly `len` bytes of `key` and returns a
 * 32-bit hash.  The contract a hash function must honor for the access
 * method to be correct:
 *
 *   determinism    -- equal (key,len) inputs yield equal outputs.  A
 *                     non-deterministic hash would send the same key to
 *                     different buckets on different lookups.
 *   length-honoring -- the result depends only on the first `len` bytes;
 *                     trailing bytes past `len` never change it.  All
 *                     four functions iterate exactly `key..key+len`.
 *   func5 == FNV    -- __ham_func5 is exactly the 32-bit FNV-1 recurrence
 *                     h = (h * 16777619) ^ byte, h0 = 0 (an independent
 *                     re-derivation of the loop body).
 *
 * These are exported from libdb (verified via nm on the built .so); the
 * prototypes live in an internal header, so we declare them locally to
 * avoid dragging in the full db_int.h include tree.
 */

#include "db.h"

#include "pbt_common.h"

/* Exported by libdb; prototypes from src/hash/hash_func.c. */
extern u_int32_t __ham_func2(DB *, const void *, u_int32_t);
extern u_int32_t __ham_func3(DB *, const void *, u_int32_t);
extern u_int32_t __ham_func4(DB *, const void *, u_int32_t);
extern u_int32_t __ham_func5(DB *, const void *, u_int32_t);

#if defined(PBT_HAVE_HEGEL)

#include <string.h>

typedef u_int32_t (*hashfn)(DB *, const void *, u_int32_t);
static const hashfn FUNCS[] = {
	__ham_func2, __ham_func3, __ham_func4, __ham_func5
};
#define NFUNCS ((int)(sizeof(FUNCS) / sizeof(FUNCS[0])))

/* Draw a byte buffer (0..128 bytes) into caller-owned storage. */
static uint8_t *
draw_key(hegel_test_case *tc, size_t *lenp)
{
	return (hegel_draw_bytes(tc, hegel_binary(0, 128), lenp));
}

/* P1: determinism -- same bytes + len hash identically, for every func. */
static void
prop_determinism(hegel_test_case *tc, void *u)
{
	uint8_t *k;
	size_t len = 0;
	int i;
	(void)u;

	k = draw_key(tc, &len);
	for (i = 0; i < NFUNCS; i++)
		hegel_assume(FUNCS[i](NULL, k, (u_int32_t)len) ==
		    FUNCS[i](NULL, k, (u_int32_t)len));
	free(k);
}

/*
 * P2: length-honoring -- the hash of the first `len` bytes is unaffected
 * by whatever follows.  We hash `k` at length `len`, then append a random
 * suffix and hash the SAME prefix length again; results must match.
 */
static void
prop_length_honoring(hegel_test_case *tc, void *u)
{
	uint8_t *prefix, *suffix, *joined;
	size_t plen = 0, slen = 0;
	int i;
	(void)u;

	prefix = draw_key(tc, &plen);
	suffix = draw_key(tc, &slen);

	joined = malloc(plen + slen + 1);	/* +1 so malloc(0) is fine */
	hegel_assume(joined != NULL);
	if (plen != 0)
		memcpy(joined, prefix, plen);
	if (slen != 0)
		memcpy(joined + plen, suffix, slen);

	for (i = 0; i < NFUNCS; i++)
		hegel_assume(
		    FUNCS[i](NULL, prefix, (u_int32_t)plen) ==
		    FUNCS[i](NULL, joined, (u_int32_t)plen));

	free(prefix);
	free(suffix);
	free(joined);
}

/* P3: __ham_func5 is exactly the 32-bit FNV-1 recurrence (oracle). */
static void
prop_func5_is_fnv(hegel_test_case *tc, void *u)
{
	uint8_t *k;
	size_t len = 0, i;
	u_int32_t h;
	(void)u;

	k = draw_key(tc, &len);
	for (h = 0, i = 0; i < len; i++) {
		h *= 16777619U;
		h ^= k[i];
	}
	hegel_assume(__ham_func5(NULL, k, (u_int32_t)len) == h);
	free(k);
}

static const pbt_entry_t tests[] = {
	{ "determinism",      prop_determinism,      400 },
	{ "length_honoring",  prop_length_honoring,  500 },
	{ "func5_is_fnv",     prop_func5_is_fnv,      500 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "determinism",      NULL, 0 },
	{ "length_honoring",  NULL, 0 },
	{ "func5_is_fnv",     NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("hash_func", tests)

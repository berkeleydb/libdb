/*-
 * test/pbt/pbt_defpfx.c
 *	Property-based tests for __bam_defpfx() -- Berkeley DB's default
 *	B-tree prefix routine (src/btree/bt_compare.c), used during
 *	page splits (bt_split.c) and compaction (bt_compact.c) to pick the
 *	shortest key that still separates two neighbours.
 *
 * Contract from the source (read directly):
 *	cnt = 1; walk min(a->size,b->size) bytes.  On the first differing
 *	byte at 0-based index i, return i+1 (the count including that byte).
 *	If they match over the common length:
 *		a->size < b->size  -> a->size + 1
 *		b->size < a->size  -> b->size + 1
 *		equal sizes        -> b->size          (== a->size)
 *	COMPQUIET(dbp) so NULL is safe.
 *
 * The value __bam_defpfx returns is the number of leading bytes of the
 * LARGER key that must be retained for it to still sort strictly after
 * the smaller key -- i.e. a prefix length.  The invariants that make it
 * a correct separator:
 *   bounded        -- pfx <= min(la,lb)+1 always; when the longer key is
 *                     non-empty, 1 <= pfx <= max(la,lb); two empty keys
 *                     give pfx == 0 (nothing to separate).
 *   locates_diff   -- if a and b differ within the common prefix, pfx-1
 *                     is exactly the index of the first differing byte,
 *                     and bytes [0, pfx-1) are equal in a and b.
 *   symmetric      -- __bam_defpfx(a,b) == __bam_defpfx(b,a) (the routine
 *                     is order-insensitive; it only measures shared bytes).
 *   separates      -- consistency with __bam_defcmp: the pfx-length prefix
 *                     of the strictly-greater key already compares greater
 *                     than the smaller key (so truncating to pfx preserves
 *                     the separation the split relies on).
 *
 * __bam_defpfx / __bam_defcmp are exported from libdb (verified via nm);
 * prototypes live in an internal header, declared locally here.
 */

#include "db.h"

#include "pbt_common.h"

/* Exported by libdb; prototypes from src/btree/bt_compare.c. */
extern size_t __bam_defpfx(DB *, const DBT *, const DBT *);
extern int __bam_defcmp(DB *, const DBT *, const DBT *);

#if defined(PBT_HAVE_HEGEL)

#include <string.h>

static uint8_t *
draw_dbt(hegel_test_case *tc, DBT *d)
{
	size_t len = 0;
	uint8_t *p = hegel_draw_bytes(tc, hegel_binary(0, 64), &len);
	memset(d, 0, sizeof(*d));
	d->data = p;
	d->size = (u_int32_t)len;
	return (p);
}

/*
 * P1: bounds.  pfx <= min(la,lb)+1 always.  When the longer key is
 * non-empty, 1 <= pfx <= max(la,lb).  The one degenerate case is two
 * EMPTY keys: the source returns b->size == 0 (there is nothing to
 * separate), so pfx == 0 there -- asserted explicitly rather than
 * excluded, since it is a real (and easily-broken) boundary.
 */
static void
prop_bounded(hegel_test_case *tc, void *u)
{
	DBT a, b;
	uint8_t *pa, *pb;
	size_t pfx, mn, mx;
	(void)u;
	pa = draw_dbt(tc, &a);
	pb = draw_dbt(tc, &b);
	mn = a.size < b.size ? a.size : b.size;
	mx = a.size > b.size ? a.size : b.size;

	pfx = __bam_defpfx(NULL, &a, &b);
	PBT_CHECK(pfx <= mn + 1, "prefix length > min(len)+1");
	if (mx > 0) {
		PBT_CHECK(pfx >= 1, "prefix length < 1 for non-empty keys");
		PBT_CHECK(pfx <= mx, "prefix length > max(len)");
	} else
		PBT_CHECK(pfx == 0, "prefix of two empty keys != 0");
	free(pa);
	free(pb);
}

/*
 * P2: when a and b differ inside the common prefix, pfx-1 indexes the
 * first differing byte and everything before it is equal in both keys.
 */
static void
prop_locates_diff(hegel_test_case *tc, void *u)
{
	DBT a, b;
	uint8_t *pa, *pb;
	size_t pfx, mn, i, first;
	int differ_in_common;
	(void)u;
	pa = draw_dbt(tc, &a);
	pb = draw_dbt(tc, &b);
	mn = a.size < b.size ? a.size : b.size;

	/* Find the first differing byte in the common region (if any). */
	differ_in_common = 0;
	first = mn;
	for (i = 0; i < mn; i++)
		if (((uint8_t *)a.data)[i] != ((uint8_t *)b.data)[i]) {
			differ_in_common = 1;
			first = i;
			break;
		}

	pfx = __bam_defpfx(NULL, &a, &b);
	if (differ_in_common) {
		PBT_CHECK(pfx == first + 1,
		    "prefix does not point at first differing byte");
		/* bytes strictly before the divergence are equal */
		for (i = 0; i + 1 < pfx; i++)
			PBT_CHECK(((uint8_t *)a.data)[i] ==
			    ((uint8_t *)b.data)[i],
			    "prefix bytes differ before divergence");
	}
	free(pa);
	free(pb);
}

/* P3: prefix length is symmetric in its two arguments. */
static void
prop_symmetric(hegel_test_case *tc, void *u)
{
	DBT a, b;
	uint8_t *pa, *pb;
	(void)u;
	pa = draw_dbt(tc, &a);
	pb = draw_dbt(tc, &b);
	PBT_CHECK(__bam_defpfx(NULL, &a, &b) ==
	    __bam_defpfx(NULL, &b, &a), "prefix length not symmetric");
	free(pa);
	free(pb);
}

/*
 * P4: the prefix separates.  For two DISTINCT keys, truncate the strictly
 * greater one to `pfx` bytes; that truncated prefix must still compare
 * strictly greater than the smaller full key under __bam_defcmp.  This is
 * the property a split/compaction relies on when it stores only the
 * prefix as the separator.
 */
static void
prop_separates(hegel_test_case *tc, void *u)
{
	DBT a, b, hi, trunc;
	uint8_t *pa, *pb;
	const DBT *lo, *big;
	size_t pfx;
	(void)u;
	pa = draw_dbt(tc, &a);
	pb = draw_dbt(tc, &b);

	/* Only meaningful for distinct keys. */
	hegel_assume(__bam_defcmp(NULL, &a, &b) != 0);
	if (__bam_defcmp(NULL, &a, &b) < 0) {
		lo = &a; big = &b;
	} else {
		lo = &b; big = &a;
	}
	hi = *big;

	pfx = __bam_defpfx(NULL, &a, &b);
	PBT_CHECK(pfx <= hi.size, "prefix exceeds the larger key length");

	memset(&trunc, 0, sizeof(trunc));
	trunc.data = hi.data;
	trunc.size = (u_int32_t)pfx;

	/* Truncated separator still sorts strictly after the smaller key. */
	PBT_CHECK(__bam_defcmp(NULL, &trunc, lo) > 0,
	    "truncated prefix does not separate the keys");
	free(pa);
	free(pb);
}

static const pbt_entry_t tests[] = {
	{ "bounded",      prop_bounded,      500 },
	{ "locates_diff", prop_locates_diff, 700 },
	{ "symmetric",    prop_symmetric,    500 },
	{ "separates",    prop_separates,    800 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "bounded",      NULL, 0 },
	{ "locates_diff", NULL, 0 },
	{ "symmetric",    NULL, 0 },
	{ "separates",    NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("defpfx", tests)

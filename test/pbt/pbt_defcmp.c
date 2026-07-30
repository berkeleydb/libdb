/*-
 * test/pbt/pbt_defcmp.c
 *	Property-based tests for __bam_defcmp() -- Berkeley DB's default
 *	B-tree/duplicate key comparison routine (src/btree/bt_compare.c,
 *	the comparator used whenever the application sets none).
 *
 * Contract from the source:
 *	Returns < 0 if a < b, = 0 if a == b, > 0 if a > b, comparing byte
 *	strings lexicographically with the shorter-is-less tiebreak.  The
 *	dbp argument is COMPQUIET'd (unused), so NULL is safe.
 *
 * __bam_defcmp returns *raw* differences (not clamped to -1/0/1), so we
 * assert only sign relations: it is a total order over byte strings --
 * reflexive, antisymmetric, and transitive -- and it agrees in sign with
 * a memcmp-with-length-tiebreak oracle.
 *
 * __bam_defcmp is exported from libdb (verified via nm on the built .so);
 * its prototype lives in an internal header, so we declare it locally to
 * avoid dragging in the full db_int.h include tree.
 */

#include "db.h"

#include "pbt_common.h"

/* Exported by libdb; prototype from src/btree/bt_compare.c. */
extern int __bam_defcmp(DB *dbp, const DBT *a, const DBT *b);

#if defined(PBT_HAVE_HEGEL)

#include <string.h>

static int
sign(long v)
{
	return (v < 0 ? -1 : (v > 0 ? 1 : 0));
}

/* Fill a DBT from a hegel-drawn byte buffer (0..64 bytes). */
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

/* Independent oracle: lexicographic memcmp with shorter-is-less. */
static int
oracle_cmp(const DBT *a, const DBT *b)
{
	size_t n = a->size < b->size ? a->size : b->size;
	int c = (n == 0) ? 0 : memcmp(a->data, b->data, n);
	if (c != 0)
		return (c < 0 ? -1 : 1);
	if (a->size == b->size)
		return (0);
	return (a->size < b->size ? -1 : 1);
}

/* P1: reflexive -- comparing a value with itself is 0. */
static void
prop_reflexive(hegel_test_case *tc, void *u)
{
	DBT a;
	uint8_t *pa;
	(void)u;
	pa = draw_dbt(tc, &a);
	PBT_CHECK(__bam_defcmp(NULL, &a, &a) == 0,
	    "__bam_defcmp(a, a) != 0 (not reflexive)");
	free(pa);
}

/* P2: antisymmetric in sign -- sign(cmp(a,b)) == -sign(cmp(b,a)). */
static void
prop_antisymmetric(hegel_test_case *tc, void *u)
{
	DBT a, b;
	uint8_t *pa, *pb;
	(void)u;
	pa = draw_dbt(tc, &a);
	pb = draw_dbt(tc, &b);
	PBT_CHECK(sign(__bam_defcmp(NULL, &a, &b)) ==
	    -sign(__bam_defcmp(NULL, &b, &a)),
	    "__bam_defcmp not antisymmetric in sign");
	free(pa);
	free(pb);
}

/* P3: transitive in sign over three byte strings. */
static void
prop_transitive(hegel_test_case *tc, void *u)
{
	DBT a, b, c;
	uint8_t *pa, *pb, *pc;
	int ab, bc, ac;
	(void)u;
	pa = draw_dbt(tc, &a);
	pb = draw_dbt(tc, &b);
	pc = draw_dbt(tc, &c);
	ab = sign(__bam_defcmp(NULL, &a, &b));
	bc = sign(__bam_defcmp(NULL, &b, &c));
	ac = sign(__bam_defcmp(NULL, &a, &c));
	if (ab <= 0 && bc <= 0)
		PBT_CHECK(ac <= 0, "__bam_defcmp not transitive (a<=b<=c)");
	if (ab >= 0 && bc >= 0)
		PBT_CHECK(ac >= 0, "__bam_defcmp not transitive (a>=b>=c)");
	free(pa);
	free(pb);
	free(pc);
}

/* P4: agrees in sign with an independent memcmp oracle. */
static void
prop_matches_oracle(hegel_test_case *tc, void *u)
{
	DBT a, b;
	uint8_t *pa, *pb;
	(void)u;
	pa = draw_dbt(tc, &a);
	pb = draw_dbt(tc, &b);
	PBT_CHECK(sign(__bam_defcmp(NULL, &a, &b)) == oracle_cmp(&a, &b),
	    "__bam_defcmp disagrees in sign with memcmp oracle");
	free(pa);
	free(pb);
}

static const pbt_entry_t tests[] = {
	{ "reflexive",      prop_reflexive,      300 },
	{ "antisymmetric",  prop_antisymmetric,  400 },
	{ "transitive",     prop_transitive,     600 },
	{ "matches_oracle", prop_matches_oracle, 500 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "reflexive",      NULL, 0 },
	{ "antisymmetric",  NULL, 0 },
	{ "transitive",     NULL, 0 },
	{ "matches_oracle", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("defcmp", tests)

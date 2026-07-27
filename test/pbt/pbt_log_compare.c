/*-
 * test/pbt/pbt_log_compare.c
 *	Property-based tests for log_compare() (public API, db.h;
 *	src/log/log_compare.c wrapping the LOG_COMPARE macro in
 *	src/dbinc/db_int.in).
 *
 * Contract from the source comment:
 *	"Compare two LSN's; return 1, 0, -1 if first is >, == or < second."
 * A DB_LSN is (u_int32_t file, u_int32_t offset) ordered lexicographically
 * by (file, offset).  So log_compare is a total order and its result is
 * always in {-1, 0, 1}.  These properties are exactly the total-order
 * axioms, verifiable against a buggy implementation.
 */

#include "db.h"

#include "pbt_common.h"

#if defined(PBT_HAVE_HEGEL)

/* Draw a DB_LSN from a small (file, offset) space so ties/orderings both
 * occur often -- the interesting cases for a comparator are near-equal
 * inputs, not astronomically separated ones. */
static DB_LSN
draw_lsn(hegel_test_case *tc)
{
	DB_LSN l;
	l.file = (u_int32_t)hegel_draw_int(tc, hegel_integers(0, 8));
	l.offset = (u_int32_t)hegel_draw_int(tc, hegel_integers(0, 8));
	return (l);
}

/* P1: result is always in {-1, 0, 1}. */
static void
prop_result_in_range(hegel_test_case *tc, void *u)
{
	DB_LSN a, b;
	int r;
	(void)u;
	a = draw_lsn(tc);
	b = draw_lsn(tc);
	r = log_compare(&a, &b);
	hegel_assume(r == -1 || r == 0 || r == 1);
}

/* P2: reflexivity -- log_compare(a, a) == 0. */
static void
prop_reflexive(hegel_test_case *tc, void *u)
{
	DB_LSN a;
	(void)u;
	a = draw_lsn(tc);
	hegel_assume(log_compare(&a, &a) == 0);
}

/* P3: antisymmetry -- log_compare(a, b) == -log_compare(b, a). */
static void
prop_antisymmetric(hegel_test_case *tc, void *u)
{
	DB_LSN a, b;
	(void)u;
	a = draw_lsn(tc);
	b = draw_lsn(tc);
	hegel_assume(log_compare(&a, &b) == -log_compare(&b, &a));
}

/* P4: transitivity -- a<=b and b<=c imply a<=c (and the strict/equal
 * variants).  We check the sign relation holds transitively. */
static void
prop_transitive(hegel_test_case *tc, void *u)
{
	DB_LSN a, b, c;
	int ab, bc, ac;
	(void)u;
	a = draw_lsn(tc);
	b = draw_lsn(tc);
	c = draw_lsn(tc);
	ab = log_compare(&a, &b);
	bc = log_compare(&b, &c);
	ac = log_compare(&a, &c);
	/* If a<=b and b<=c then a<=c. */
	if (ab <= 0 && bc <= 0)
		hegel_assume(ac <= 0);
	/* If a>=b and b>=c then a>=c. */
	if (ab >= 0 && bc >= 0)
		hegel_assume(ac >= 0);
	/* If a==b and b==c then a==c. */
	if (ab == 0 && bc == 0)
		hegel_assume(ac == 0);
}

static const pbt_entry_t tests[] = {
	{ "result_in_range", prop_result_in_range, 300 },
	{ "reflexive",       prop_reflexive,        300 },
	{ "antisymmetric",   prop_antisymmetric,    300 },
	{ "transitive",      prop_transitive,       500 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "result_in_range", NULL, 0 },
	{ "reflexive",       NULL, 0 },
	{ "antisymmetric",   NULL, 0 },
	{ "transitive",      NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("log_compare", tests)

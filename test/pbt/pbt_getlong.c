/*-
 * test/pbt/pbt_getlong.c
 *	Property-based tests for the numeric-argument parsers in
 *	src/common/db_getlong.c: __db_getlong / __db_getulong.
 *
 * These convert a textual argument (from utilities / config) into a
 * long / u_long, bounded to [min,max], returning:
 *	0	 success, *storep set
 *	EINVAL	 empty string, or trailing garbage after the digits
 *	ERANGE	 strtol/strtoul overflow, or value outside [min,max]
 * (source: the four early-return branches).  This is pure string->number
 * logic -- exactly the kind of thing the tcl suite never exercises
 * directly (db_getlong.c sits at ~47% line coverage in the tcl COV run),
 * so the PBT tier closes that gap.
 *
 * Contracts exercised (grounded in the branch structure, not restated):
 *   roundtrip_in_range   -- a value formatted with %ld then parsed back
 *                           within a covering [min,max] returns 0 and the
 *                           same value (the codec is round-trip exact).
 *   rejects_out_of_range -- an in-range integer, parsed against a window
 *                           that EXCLUDES it, returns ERANGE and leaves
 *                           *storep untouched (never a false success).
 *   rejects_trailing     -- digits followed by a non-digit, non-newline
 *                           byte are rejected EINVAL (the end[0] check).
 *   robust_no_crash      -- arbitrary bytes never crash and never return
 *                           a code outside {0, EINVAL, ERANGE}; on success
 *                           *storep lands inside [min,max].
 *   getulong_zero_is_max -- __db_getulong treats max==0 as "no upper
 *                           bound" (documented in the source), so a huge
 *                           value passes with max==0 but is rejected with
 *                           a small explicit max.
 *
 * Both are PUBLIC (prototypes in db_getlong.c; verified reachable via nm
 * on the built library).  Passing dbenv == NULL routes diagnostics to
 * stderr and takes the pure branch (no env required).
 */

#include "db.h"

#include "pbt_common.h"

/* Exported by libdb (PUBLIC: prototypes in src/common/db_getlong.c). */
extern int __db_getlong(DB_ENV *, const char *, char *, long, long, long *);
extern int __db_getulong(DB_ENV *, const char *, char *,
    u_long, u_long, u_long *);

#if defined(PBT_HAVE_HEGEL)

#include <errno.h>
#include <limits.h>
#include <string.h>

/*
 * All these tests pass dbenv == NULL, which sends the error messages to
 * stderr on rejection.  That is deliberate (it exercises the NULL-dbenv
 * branch), but to keep the run readable we silence stderr for the batch.
 */
static void
quiet_stderr(void)
{
	static int done = 0;
	if (!done) {
		(void)freopen("/dev/null", "w", stderr);
		done = 1;
	}
}

/* P1: format then parse within a covering window is the identity. */
static void
prop_roundtrip_in_range(hegel_test_case *tc, void *u)
{
	char buf[32];
	long v, min, max, out;
	int ret;
	(void)u;
	quiet_stderr();

	/* Draw within a range that fits `long` on any platform (>= 32-bit). */
	v = (long)hegel_draw_int(tc, hegel_integers(-2000000000L, 2000000000L));
	(void)snprintf(buf, sizeof(buf), "%ld", v);

	/* A window that always contains v. */
	min = v > LONG_MIN + 100 ? v - 100 : LONG_MIN;
	max = v < LONG_MAX - 100 ? v + 100 : LONG_MAX;

	out = v + 1;	/* poison, so we detect a stale/unset store */
	ret = __db_getlong(NULL, "pbt", buf, min, max, &out);
	PBT_CHECK(ret == 0, "in-range value rejected");
	PBT_CHECK(out == v, "parsed value != formatted value");
}

/*
 * P2: an in-range integer parsed against a window that EXCLUDES it must
 * be rejected ERANGE -- the parser never reports a false success and
 * never writes *storep on the range-reject path.
 */
static void
prop_rejects_out_of_range(hegel_test_case *tc, void *u)
{
	char buf[32];
	long v, min, max, sentinel, out;
	int ret, below;
	(void)u;
	quiet_stderr();

	v = (long)hegel_draw_int(tc, hegel_integers(-1000000000L, 1000000000L));
	(void)snprintf(buf, sizeof(buf), "%ld", v);
	below = (int)hegel_draw_bool(tc, hegel_booleans());

	if (below) {			/* window strictly above v */
		min = v + 1;
		max = v + 1000;
	} else {			/* window strictly below v */
		min = v - 1000;
		max = v - 1;
	}

	sentinel = v ^ 0x5A5A5A5AL;
	out = sentinel;
	ret = __db_getlong(NULL, "pbt", buf, min, max, &out);
	PBT_CHECK(ret == ERANGE, "out-of-window value not rejected ERANGE");
	PBT_CHECK(out == sentinel, "store written on range-reject path");
}

/*
 * P3: valid digits followed by a byte that is neither '\0' nor '\n' are
 * rejected EINVAL (the `end[0] != '\0' && end[0] != '\n'` guard).  A
 * trailing '\n' is explicitly allowed, so we assert that separately.
 */
static void
prop_rejects_trailing(hegel_test_case *tc, void *u)
{
	char buf[40];
	long v, out;
	int ret, junk;
	static const char junks[] = "abZ!/.:xq_";
	(void)u;
	quiet_stderr();

	v = (long)hegel_draw_int(tc, hegel_integers(0, 1000000L));
	junk = (int)hegel_draw_int(tc,
	    hegel_integers(0, (int64_t)sizeof(junks) - 2));

	/* "<digits><junkchar>" must be EINVAL. */
	(void)snprintf(buf, sizeof(buf), "%ld%c", v, junks[junk]);
	out = -1;
	ret = __db_getlong(NULL, "pbt", buf, LONG_MIN, LONG_MAX, &out);
	PBT_CHECK(ret == EINVAL, "trailing junk not rejected EINVAL");

	/* "<digits>\n" must still succeed (newline is tolerated). */
	(void)snprintf(buf, sizeof(buf), "%ld\n", v);
	out = -1;
	ret = __db_getlong(NULL, "pbt", buf, LONG_MIN, LONG_MAX, &out);
	PBT_CHECK(ret == 0 && out == v, "trailing newline not tolerated");
}

/*
 * P4: robustness -- arbitrary NUL-terminated bytes never crash, always
 * return a code in {0, EINVAL, ERANGE}, and on success land in [min,max].
 */
static void
prop_robust_no_crash(hegel_test_case *tc, void *u)
{
	uint8_t *raw;
	char *s;
	size_t len = 0, i;
	long min, max, out;
	int ret;
	(void)u;
	quiet_stderr();

	raw = hegel_draw_bytes(tc, hegel_binary(0, 64), &len);
	s = malloc(len + 1);
	hegel_assume(s != NULL);
	for (i = 0; i < len; i++)		/* NUL-free so it's one C string */
		s[i] = raw[i] == '\0' ? ' ' : (char)raw[i];
	s[len] = '\0';

	min = (long)hegel_draw_int(tc, hegel_integers(-100000L, 0));
	max = (long)hegel_draw_int(tc, hegel_integers(0, 100000L));

	out = 0x1234;
	ret = __db_getlong(NULL, "pbt", s, min, max, &out);
	PBT_CHECK(ret == 0 || ret == EINVAL || ret == ERANGE,
	    "return code outside {0, EINVAL, ERANGE}");
	if (ret == 0)
		PBT_CHECK(out >= min && out <= max,
		    "success stored value outside [min,max]");

	free(s);
	free(raw);
}

/*
 * P5: __db_getulong documents that max == 0 means "no upper bound"
 * (ULONG_MAX substitute).  So a large value passes with max==0 but is
 * rejected ERANGE with a small explicit max.
 */
static void
prop_getulong_zero_is_max(hegel_test_case *tc, void *u)
{
	char buf[32];
	u_long v, out;
	int ret;
	(void)u;
	quiet_stderr();

	/* A value comfortably larger than the small explicit cap below. */
	v = (u_long)hegel_draw_int(tc, hegel_integers(1000000L, 3000000000L));
	(void)snprintf(buf, sizeof(buf), "%lu", v);

	out = 0;
	ret = __db_getulong(NULL, "pbt", buf, 0, 0, &out);	/* max==0 */
	PBT_CHECK(ret == 0 && out == v, "max==0 did not mean unbounded");

	out = 0;
	ret = __db_getulong(NULL, "pbt", buf, 0, 1000, &out);	/* real cap */
	PBT_CHECK(ret == ERANGE, "value above explicit max not rejected");
}

static const pbt_entry_t tests[] = {
	{ "roundtrip_in_range",   prop_roundtrip_in_range,   600 },
	{ "rejects_out_of_range", prop_rejects_out_of_range, 600 },
	{ "rejects_trailing",     prop_rejects_trailing,     600 },
	{ "robust_no_crash",      prop_robust_no_crash,      800 },
	{ "getulong_zero_is_max", prop_getulong_zero_is_max, 500 },
	{ NULL, NULL, 0 }
};

#else

static const pbt_entry_t tests[] = {
	{ "roundtrip_in_range",   NULL, 0 },
	{ "rejects_out_of_range", NULL, 0 },
	{ "rejects_trailing",     NULL, 0 },
	{ "robust_no_crash",      NULL, 0 },
	{ "getulong_zero_is_max", NULL, 0 },
	{ NULL, NULL, 0 }
};

#endif

PBT_MAIN("getlong", tests)

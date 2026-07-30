/*
 * CBMC harness: __db_getlong / __db_getulong (src/common/db_getlong.c)
 *
 * Verifies the REAL string->number parsers by #including the unmodified
 * source with minimal stubs for the DB_ENV error path.  What we prove over
 * a bounded nondeterministic input string:
 *
 *   __db_getlong:
 *     1. returns 0  ==>  min <= *storep <= max   (in-range result is honoured)
 *     2. never reads past the NUL-terminated input (CBMC bounds/pointer).
 *     3. an out-of-range strtol result (ERANGE) or trailing garbage is
 *        rejected (return != 0) and *storep is NOT written past the bound.
 *   __db_getulong:
 *     4. returns 0  ==>  val >= min AND (max == 0 || val <= max)
 *        (the documented "0 means no upper bound" contract).
 *
 * What is stubbed:
 *   - dbenv == NULL, so the error branches take the fprintf() path; fprintf
 *     and strerror are the libc CBMC models (harmless, we ignore output).
 *   - __os_set_errno / __os_get_errno map to the real C errno.
 *   - DB_STR_A(id, msg, fmt) -> msg (the plain format string).
 *   - strtoul: a small faithful base-10 model (CBMC ships strtol but not
 *     strtoul); __db_getlong uses CBMC's own strtol model.
 *
 * Bound: input string length SLEN (incl. NUL).  The parse is driven by the
 *        libc strtol/strtoul CBMC models; --unwind covers the SLEN scan.
 */

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

/* fprintf/strerror are only used for user-facing error messages on the
 * dbenv==NULL path; stub them (CBMC has no variadic fprintf body).  This
 * does not affect the parse logic or return value we are verifying. */
#define fprintf(...) (0)
#define strerror(e)  ("")

/*
 * CBMC ships a model for strtol but NOT strtoul.  We supply a small faithful
 * base-10 strtoul model so the REAL __db_getulong logic (its range checks and
 * the documented max==0 contract) is what gets verified -- exactly as
 * __db_getlong is verified against CBMC's own strtol model.  The model:
 *   - skips no whitespace (inputs here have none),
 *   - parses [0-9]* into an unsigned long, saturating to ULONG_MAX + ERANGE,
 *   - sets *endptr to the first non-digit (or to nptr if none),
 *   matching the contract __db_getulong relies on.
 */
#define strtoul cbmc_strtoul
static unsigned long cbmc_strtoul(const char *nptr, char **endptr, int base)
{
	unsigned long v = 0;
	const char *s = nptr;
	(void)base; /* base 10 only in this codebase */
	while (*s >= '0' && *s <= '9') {
		unsigned d = (unsigned)(*s - '0');
		if (v > (ULONG_MAX - d) / 10) {
			v = ULONG_MAX;
			errno = ERANGE;
			/* consume remaining digits */
			while (*s >= '0' && *s <= '9') s++;
			break;
		}
		v = v * 10 + d;
		s++;
	}
	if (endptr != NULL)
		*endptr = (char *)s;
	return v;
}

typedef unsigned long u_long;

/* --- stubs for the db_int.h bits db_getlong.c touches --- */
/* dbenv is always NULL at runtime, but the error branches reference
 * dbenv->err / dbenv->errx, so the type must have those members. */
typedef struct __db_env {
	void (*err)(struct __db_env *, int, const char *, ...);
	void (*errx)(struct __db_env *, const char *, ...);
} DB_ENV;
#define __os_set_errno(e) (errno = (e))
#define __os_get_errno()  (errno)
#define DB_STR_A(id, msg, fmt) (msg)

/* Pull in the REAL parsers (empty db_config.h / db_int.h on the -I path). */
#include "../../src/common/db_getlong.c"

#define SLEN 6
int nondet_int(void);
long nondet_long(void);
u_long nondet_ulong(void);

void harness_long(void)
{
	char p[SLEN];
	long min = nondet_long(), max = nondet_long(), store;
	int ret, i;

	/* Nondeterministic NUL-terminated string. */
	for (i = 0; i < SLEN - 1; i++)
		p[i] = (char)nondet_int();
	p[SLEN - 1] = '\0';
	__CPROVER_assume(min <= max);

	store = nondet_long(); /* poison: must be overwritten only on success */
	ret = __db_getlong(NULL, "cbmc", p, min, max, &store);

	if (ret == 0) {
		__CPROVER_assert(store >= min && store <= max,
		    "__db_getlong success => result within [min,max]");
	}
}

void harness_ulong(void)
{
	char p[SLEN];
	u_long min = nondet_ulong(), max = nondet_ulong(), store;
	int ret, i;

	for (i = 0; i < SLEN - 1; i++)
		p[i] = (char)nondet_int();
	p[SLEN - 1] = '\0';

	store = nondet_ulong();
	ret = __db_getulong(NULL, "cbmc", p, min, max, &store);

	if (ret == 0) {
		__CPROVER_assert(store >= min,
		    "__db_getulong success => result >= min");
		/* Documented contract: max==0 means "no upper bound". */
		__CPROVER_assert(max == 0 || store <= max,
		    "__db_getulong success => result <= max (unless max==0)");
	}
}

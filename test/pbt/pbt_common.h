/*-
 * Property-based testing (Hegel) scaffolding for libdb.
 *
 * test/pbt/pbt_common.h
 *	Shared runner for hegel-c property tests.  Each pbt_*.c file
 *	includes this, defines its properties as hegel test functions,
 *	lists them in a pbt_entry_t[] table, and closes with PBT_MAIN().
 *
 *	GRACEFUL STUB MODE: when the build was NOT configured with
 *	-Dhegel=enabled (macro PBT_HAVE_HEGEL undefined), this header
 *	supplies stub types + generators so every pbt_*.c still compiles
 *	and links against libdb.  main() then prints SKIP and exits 0, so
 *	CI without the `hegel` binary stays green while still proving the
 *	libdb symbols under test are reachable.
 *
 *	The real API mirrored here is from hegel-c (https://github.com/
 *	gburd/hegel-c): hegel_session_new/free, hegel_run_test returning
 *	hegel_results, HEGEL_DEFAULT_SETTINGS, and the hegel_draw_int,
 *	hegel_integers, hegel_binary, hegel_assume, and hegel_fail surface.
 *	Properties assert with PBT_CHECK (-> hegel_fail); hegel_assume is a
 *	precondition filter only (see PBT_CHECK below).
 */

#ifndef LIBDB_PBT_COMMON_H
#define LIBDB_PBT_COMMON_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * PBT_CHECK(cond, msg) -- the property-assertion helper.  A property must
 * *fail* (not merely skip) when violated.  In hegel, hegel_assume(false)
 * marks a case INVALID (skipped) -- it is a PRECONDITION filter, never an
 * assertion.  The failure signal is hegel_fail(), which marks the case
 * INTERESTING and lets the server shrink a minimal counterexample.  Use
 * PBT_CHECK for every actual property; reserve hegel_assume() for genuine
 * domain preconditions.  In stub mode both are no-ops (bodies unreached).
 */
#define PBT_CHECK(cond, msg) do { if (!(cond)) hegel_fail(msg); } while (0)

#if defined(PBT_HAVE_HEGEL)

#include "hegel/hegel.h"
#include "hegel/generators.h"

/*
 * A property-based test entry.  name appears in the OK/FAIL output;
 * fn is the hegel-c test function; max_examples caps the run (0 -> 100).
 */
typedef struct pbt_entry {
	const char    *name;
	hegel_test_fn  fn;
	int            max_examples;
} pbt_entry_t;

/*
 * pbt_run_all -- run every entry in `tests` against a single hegel
 * session.  Returns 0 if all pass, 1 if any fail (or the session cannot
 * start, which usually means the `hegel` server binary is not on PATH).
 */
static inline int
pbt_run_all(const char *suite_name, const pbt_entry_t *tests)
{
	hegel_session *s;
	const pbt_entry_t *e;
	int failures = 0;

	s = hegel_session_new();
	if (s == NULL) {
		fprintf(stderr, "[%s] FAIL: cannot start hegel session\n",
		    suite_name);
		fprintf(stderr, "  hint: install hegel-core (pip install "
		    "hegel-core) or set HEGEL_SERVER_COMMAND\n");
		return (1);
	}

	for (e = tests; e->name != NULL; e++) {
		hegel_settings settings = HEGEL_DEFAULT_SETTINGS;
		hegel_results r;

		settings.max_examples =
		    e->max_examples > 0 ? e->max_examples : 100;
		r = hegel_run_test(s, e->fn, NULL, &settings);
		if (r.passed)
			printf("  [PBT] %s/%s OK (%d valid examples)\n",
			    suite_name, e->name, r.valid_test_cases);
		else {
			printf("  [PBT] %s/%s FAIL: %s\n",
			    suite_name, e->name,
			    r.error != NULL ? r.error : "property violated");
			failures++;
		}
		hegel_results_free(&r);
	}
	hegel_session_free(s);
	return (failures == 0 ? 0 : 1);
}

#define PBT_MAIN(SUITE, TESTS)						\
	int main(int argc, char *argv[]) {				\
		(void)argc; (void)argv;					\
		return (pbt_run_all((SUITE), (TESTS)));			\
	}

#else /* !PBT_HAVE_HEGEL -- stub mode */

/*
 * Stub mode: tests print SKIP and exit 0.  Minimal type/generator
 * definitions let the pbt_*.c bodies compile without #ifdef clutter;
 * the property bodies are unreachable here but still type-check and,
 * critically, still reference the libdb symbols under test so the
 * linker proves them reachable.
 */
typedef int hegel_test_case;
typedef struct hegel_generator hegel_generator;
typedef void (*hegel_test_fn)(hegel_test_case *tc, void *user_data);

typedef struct pbt_entry {
	const char    *name;
	hegel_test_fn  fn;
	int            max_examples;
} pbt_entry_t;

static inline int
pbt_run_all(const char *suite_name, const pbt_entry_t *tests)
{
	const pbt_entry_t *e;
	int n = 0;

	for (e = tests; e->name != NULL; e++)
		n++;
	printf("  [PBT] %s SKIP (built without -Dhegel=enabled); "
	    "%d properties unverified\n", suite_name, n);
	return (0);
}

#define PBT_MAIN(SUITE, TESTS)						\
	int main(int argc, char *argv[]) {				\
		(void)argc; (void)argv;					\
		return (pbt_run_all((SUITE), (TESTS)));			\
	}

/* Stub generators/draws mirroring the hegel-c signatures. */
static inline int64_t
hegel_draw_int(hegel_test_case *tc, hegel_generator *gen)
{ (void)tc; (void)gen; return (0); }
static inline int
hegel_draw_bool(hegel_test_case *tc, hegel_generator *gen)
{ (void)tc; (void)gen; return (0); }
static inline uint8_t *
hegel_draw_bytes(hegel_test_case *tc, hegel_generator *gen, size_t *len)
{ (void)tc; (void)gen; if (len != NULL) *len = 0; return (NULL); }
static inline hegel_generator *
hegel_integers(int64_t lo, int64_t hi)
{ (void)lo; (void)hi; return (NULL); }
static inline hegel_generator *
hegel_booleans(void)
{ return (NULL); }
static inline hegel_generator *
hegel_binary(size_t lo, size_t hi)
{ (void)lo; (void)hi; return (NULL); }
static inline void
hegel_assume(int cond)
{ (void)cond; }
static inline void
hegel_fail(const char *msg)
{ (void)msg; }
static inline void
hegel_note(const char *msg)
{ (void)msg; }

#endif /* PBT_HAVE_HEGEL */

#endif /* LIBDB_PBT_COMMON_H */

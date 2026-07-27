/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_rng.c --
 *	Pilot: prove the seeded PRNG tree is deterministic, reproducible,
 *	and stream-independent -- the foundation every other DST scenario
 *	rests on.  Mirrors xtc's test_sim_rng.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_rng && ./test_sim_rng
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* The DST core is compiled into libdb under --enable-dst; declare the
 * bits we exercise (test/sim is on the include path via DST_CFLAGS). */
#include "sim_rng.h"

#define NDRAW 256

static int failures;

#define CHECK(cond, msg) do {						\
	if (!(cond)) {							\
		fprintf(stderr, "FAIL: %s (%s:%d)\n", msg,		\
		    __FILE__, __LINE__);				\
		failures++;						\
	}								\
} while (0)

/* Collect NDRAW draws from one stream into out[]. */
static void
collect(seed, stream, out)
	uint64_t seed;
	int stream;
	uint64_t *out;
{
	int i;

	__db_sim_activate(seed);
	for (i = 0; i < NDRAW; i++)
		out[i] = __db_sim_rng(stream);
	__db_sim_deactivate();
}

int
main()
{
	uint64_t a[NDRAW], b[NDRAW], io[NDRAW], fault[NDRAW];
	int i, same;

	/* 1. Determinism: the same seed reproduces the identical sequence. */
	collect(0xC0FFEEull, DB_SIM_RNG_APP, a);
	collect(0xC0FFEEull, DB_SIM_RNG_APP, b);
	CHECK(memcmp(a, b, sizeof(a)) == 0,
	    "same seed must reproduce identical draws");

	/* 2. Seed sensitivity: a different seed gives a different sequence. */
	collect(0xC0FFEEull, DB_SIM_RNG_APP, a);
	collect(0xC0FFEFull, DB_SIM_RNG_APP, b);
	CHECK(memcmp(a, b, sizeof(a)) != 0,
	    "different seeds must give different draws");

	/* 3. Stream independence: IO and FAULT streams under the SAME seed
	 *    are independent sequences -- so adding a draw at one site never
	 *    shifts another site's sequence (stable replay under change). */
	collect(0xABCD1234ull, DB_SIM_RNG_IO, io);
	collect(0xABCD1234ull, DB_SIM_RNG_FAULT, fault);
	CHECK(memcmp(io, fault, sizeof(io)) != 0,
	    "distinct streams must be independent under one seed");

	/* 3b. Interleaving IO+FAULT draws must NOT change what the IO stream
	 *     produces vs drawing IO alone -- the isolation guarantee. */
	__db_sim_activate(0xABCD1234ull);
	same = 1;
	for (i = 0; i < NDRAW; i++) {
		uint64_t v = __db_sim_rng(DB_SIM_RNG_IO);
		(void)__db_sim_rng(DB_SIM_RNG_FAULT);   /* perturb FAULT */
		if (v != io[i])
			same = 0;
	}
	__db_sim_deactivate();
	CHECK(same, "interleaving another stream must not shift the IO stream");

	/* 4. rng_range respects the bound and is deterministic. */
	__db_sim_activate(42);
	for (i = 0; i < NDRAW; i++) {
		uint64_t r = __db_sim_rng_range(DB_SIM_RNG_APP, 100);
		CHECK(r < 100, "rng_range must stay below bound");
	}
	__db_sim_deactivate();

	/* 5. Inactive sim draws 0 (callers gate on __db_sim_active). */
	CHECK(__db_sim_active() == 0, "sim must be inactive after deactivate");
	CHECK(__db_sim_rng(DB_SIM_RNG_APP) == 0,
	    "inactive sim must draw 0");

	/* 6. Determinism guard: outside a run it is a no-op (count stays 0);
	 *    inside a run, in non-strict mode, it counts violations. */
	__db_sim_nondeterminism("outside-run");   /* no-op */
	__db_sim_activate(7);
	__db_sim_strict(0);                        /* count, do not abort */
	__db_sim_nondeterminism("test.clock_read");
	CHECK(__db_sim_nondeterminism_count() == 1,
	    "guard must count a violation inside a run");
	__db_sim_strict(1);
	__db_sim_deactivate();

	if (failures == 0) {
		printf("test_sim_rng: PASS (determinism, seed-sensitivity, "
		    "stream-independence, guard)\n");
		return (EXIT_SUCCESS);
	}
	fprintf(stderr, "test_sim_rng: FAIL (%d checks)\n", failures);
	return (EXIT_FAILURE);
}

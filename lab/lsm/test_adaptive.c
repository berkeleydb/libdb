/*-
 * libdb LSM prototype: adaptive controller tests.
 *
 * Drives the controller with an Amethyst-style phase-shifting workload and
 * asserts that (a) the structure axis spawns/collapses correctly, (b) the
 * per-segment policy axis converges to the workload-appropriate layout, and
 * (c) cooldowns prevent oscillation. Deterministic (virtual tick clock).
 *
 * Build/run:  cc -O2 -Wall adaptive.c test_adaptive.c -o t && ./t
 */
#include <stdio.h>
#include <stdlib.h>
#include "adaptive.h"

static int failures = 0;

#define CHECK(cond, msg) do {						\
	if (cond) {							\
		printf("  PASS: %s\n", msg);				\
	} else {							\
		printf("  FAIL: %s\n", msg);				\
		failures++;						\
	}								\
} while (0)

static const char *
mode_name(adc_mode m)
{
	return m == ADC_SINGLE ? "SINGLE" :
	       m == ADC_HYBRID ? "HYBRID" : "MULTILEVEL";
}

/* Structure axis: write-heavy spawns to MULTILEVEL, idle collapses to SINGLE. */
static void
test_structure(void)
{
	adc_structure s;
	uint64_t t, tick = 0;
	int switches = 0;
	adc_mode prev;

	printf("test_structure (phase-shifting):\n");
	adc_structure_init(&s, /*window*/ 1000, /*cooldown*/ 200);
	prev = s.mode;

	/* Phase A: write-heavy for 1500 ticks (2 writes/tick). */
	for (t = 0; t < 1500; t++, tick++) {
		adc_structure_record_write(&s, tick);
		adc_structure_record_write(&s, tick);
		if (adc_structure_eval(&s, tick) != prev) {
			switches++;
			prev = s.mode;
		}
	}
	printf("    after write-heavy phase: mode=%s\n", mode_name(s.mode));
	CHECK(s.mode == ADC_MULTILEVEL, "write-heavy workload spawns to MULTILEVEL");

	/* Phase B: idle/read-only for 3000 ticks (lets the write window drain). */
	for (t = 0; t < 3000; t++, tick++) {
		adc_structure_record_read(&s, tick);
		if (adc_structure_eval(&s, tick) != prev) {
			switches++;
			prev = s.mode;
		}
	}
	printf("    after idle phase: mode=%s, total switches=%d\n",
	    mode_name(s.mode), switches);
	CHECK(s.mode == ADC_SINGLE, "idle workload collapses to SINGLE");
	/* SINGLE->HYBRID->MULTILEVEL then back = 4 transitions total. */
	CHECK(switches == 4, "exactly 4 structure transitions (no oscillation)");
}

/* Anti-flap: an alternating signal must not switch faster than the cooldown. */
static void
test_cooldown(void)
{
	adc_structure s;
	uint64_t tick;
	int switches = 0;
	adc_mode prev;

	printf("test_cooldown (anti-flap):\n");
	adc_structure_init(&s, 1000, 200);
	prev = s.mode;
	/* Sustained heavy writes for 2000 ticks; eval every tick. Once it
	 * reaches MULTILEVEL it must stay (no flapping back and forth). */
	for (tick = 0; tick < 2000; tick++) {
		adc_structure_record_write(&s, tick);
		adc_structure_record_write(&s, tick);
		if (adc_structure_eval(&s, tick) != prev) {
			switches++;
			prev = s.mode;
		}
	}
	printf("    sustained load: mode=%s, switches=%d\n",
	    mode_name(s.mode), switches);
	CHECK(s.mode == ADC_MULTILEVEL && switches == 2,
	    "sustained load reaches MULTILEVEL in 2 steps and stays");
}

/* Policy axis: per-segment leveled<->tiered by read/write hotness. */
static void
test_segment(void)
{
	adc_segment hot_w, hot_r, balanced;
	uint64_t t, tick = 0;

	printf("test_segment (per-segment policy):\n");
	adc_segment_init(&hot_w, 1000, 200, ADC_LEVELED);
	adc_segment_init(&hot_r, 1000, 200, ADC_TIERED);
	adc_segment_init(&balanced, 1000, 200, ADC_LEVELED);

	for (t = 0; t < 1500; t++, tick++) {
		/* write-hot segment */
		adc_segment_record_write(&hot_w, tick);
		adc_segment_record_write(&hot_w, tick);
		adc_segment_eval(&hot_w, tick);
		/* read-hot segment */
		adc_segment_record_read(&hot_r, tick);
		adc_segment_record_read(&hot_r, tick);
		adc_segment_eval(&hot_r, tick);
		/* balanced segment: equal reads and writes */
		adc_segment_record_read(&balanced, tick);
		adc_segment_record_write(&balanced, tick);
		adc_segment_eval(&balanced, tick);
	}
	printf("    hot_w=%s hot_r=%s balanced=%s\n",
	    hot_w.policy == ADC_TIERED ? "TIERED" : "LEVELED",
	    hot_r.policy == ADC_TIERED ? "TIERED" : "LEVELED",
	    balanced.policy == ADC_TIERED ? "TIERED" : "LEVELED");
	CHECK(hot_w.policy == ADC_TIERED, "write-hot segment becomes TIERED");
	CHECK(hot_r.policy == ADC_LEVELED, "read-hot segment becomes LEVELED");
	CHECK(balanced.policy == ADC_LEVELED,
	    "balanced segment keeps its policy (no churn)");
}

int
main(void)
{
	printf("== libdb adaptive LSM controller prototype ==\n");
	test_structure();
	test_cooldown();
	test_segment();
	if (failures == 0) {
		printf("\nALL TESTS PASSED\n");
		return (0);
	}
	printf("\n%d TEST(S) FAILED\n", failures);
	return (1);
}

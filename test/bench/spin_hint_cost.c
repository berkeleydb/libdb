/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 *
 * spin_hint_cost -- nanoseconds per spin-wait hint instruction.
 *
 * Basis for the MUTEX_PAUSE choice in src/dbinc/mutex_int.h.  On AArch64 it
 * times nop, yield and isb; on x86 nop and pause.  Best of 5 x 20M iterations.
 *     cc -O2 spin_hint_cost.c -o spin_hint_cost && ./spin_hint_cost
 */
#include <stdio.h>
#include <time.h>

static double
now(void)
{
	struct timespec t;

	clock_gettime(CLOCK_MONOTONIC, &t);
	return (t.tv_sec * 1e9 + t.tv_nsec);
}

#define	N	20000000L
#define	RUN(name, insn) do {						\
	double best = 1e18, a, d;					\
	long i;								\
	int r;								\
	for (r = 0; r < 5; r++) {					\
		a = now();						\
		for (i = 0; i < N; i++)					\
			__asm__ volatile (insn ::: "memory");		\
		if ((d = (now() - a) / N) < best)			\
			best = d;					\
	}								\
	printf("%-8s %.2f ns\n", name, best);				\
} while (0)

int
main(void)
{
#if defined(__aarch64__)
	RUN("nop", "nop");
	RUN("yield", "yield");
	RUN("isb", "isb");
#elif defined(__x86_64__) || defined(__i386__)
	RUN("nop", "nop");
	RUN("pause", "pause");
#endif
	return (0);
}

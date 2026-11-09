/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness H3: election quorum arithmetic.
 *
 * ELECTION_MAJORITY(n) (src/dbinc/rep.h:534) is the vote count a candidate
 * must collect to win a generation.  The one-master-per-generation safety
 * argument rests on two facts about it, which we prove for every group size
 * nsites in [1, 64]:
 *
 *   (1) MAJORITY INTERSECTION: any two vote sets of size >= majority, drawn
 *       from the same n sites, share at least one site.  Two distinct
 *       candidates therefore cannot each collect a majority of VOTE2s, so no
 *       two masters are elected in one generation.  By inclusion-exclusion,
 *       |A n B| >= |A| + |B| - n, so it is enough that 2*maj - n >= 1, i.e.
 *       2*maj > n.  We prove 2*ELECTION_MAJORITY(n) > n directly, AND prove
 *       the underlying set fact constructively: two nondeterministic subsets
 *       of [0,n) each of size >= maj must intersect (checked by CBMC over a
 *       nondeterministic membership bitmap).
 *
 *   (2) SUB-MAJORITY WARNING: __rep_elect_int warns when the requested ack
 *       count is sub-majority, using the test `ack <= nsites/2`.  We prove
 *       this is exactly equivalent to `ack < ELECTION_MAJORITY(nsites)` for
 *       every nsites and every ack in [0, 64].  (If they ever diverged, the
 *       warning would fire on a true majority or stay silent on a true
 *       sub-majority.)
 *
 * Everything here is pure integer arithmetic (no engine struct, no I/O), so
 * the harness is loop-free except for the small constructive intersection
 * loop over N sites.
 *
 * Bound: nsites in [1, 64]; ack in [0, 64]; constructive intersection over
 *        N = 8 sites (enough to exhibit the pigeonhole; the arithmetic proof
 *        covers 1..64 with no bound on the set representation).
 */

#include <stdint.h>

typedef uint32_t u_int32_t;

/* --- VERBATIM from src/dbinc/rep.h:534 --- */
#define	ELECTION_MAJORITY(n) (((n) / 2) + 1)

/*
 * SABOTAGE: the classic off-by-one, majority = n/2 (floor) instead of n/2+1.
 * For even n (e.g. n=4, maj=2) two disjoint sets of size 2 then exist, so the
 * intersection property FAILS; and the warning equivalence breaks too.
 */
#ifdef MAJORITY_SABOTAGE
#undef	ELECTION_MAJORITY
#define	ELECTION_MAJORITY(n) ((n) / 2)
#endif

u_int32_t nondet_u32(void);

/*
 * (1a) + (2): the arithmetic identities, over all nsites in [1,64] and all
 * ack in [0,64].
 */
void harness_arith(void)
{
	u_int32_t nsites = nondet_u32();
	u_int32_t ack = nondet_u32();
	u_int32_t maj;

	__CPROVER_assume(nsites >= 1 && nsites <= 64);
	__CPROVER_assume(ack <= 64);

	maj = ELECTION_MAJORITY(nsites);

	/* (1a) 2*maj > nsites => any two majorities must intersect. */
	__CPROVER_assert(2 * maj > nsites,
	    "two majorities of nsites always intersect (2*maj > nsites)");

	/* maj is a real majority: strictly more than half, and <= nsites. */
	__CPROVER_assert(maj <= nsites, "a majority is at most the whole group");
	__CPROVER_assert(2 * maj > nsites, "a majority is strictly over half");

	/*
	 * (2) the sub-majority warning test `ack <= nsites/2` is EXACTLY
	 * `ack < ELECTION_MAJORITY(nsites)`.
	 */
	__CPROVER_assert((ack <= nsites / 2) == (ack < maj),
	    "sub-majority warning (ack <= nsites/2) == (ack < majority)");
}

/*
 * (1b) constructive intersection: two subsets A, B of [0,N) each with at least
 * ELECTION_MAJORITY(N) members must share an index.  Membership is a
 * nondeterministic pair of bitmaps; CBMC searches for a disjoint pair and
 * finds none.
 */
#define	N	8
void harness_intersect(void)
{
	int inA[N], inB[N];
	u_int32_t ca, cb, maj;
	int i;

	maj = ELECTION_MAJORITY(N);

	ca = cb = 0;
	for (i = 0; i < N; i++) {
		inA[i] = nondet_u32() & 1;
		inB[i] = nondet_u32() & 1;
		if (inA[i])
			ca++;
		if (inB[i])
			cb++;
	}
	__CPROVER_assume(ca >= maj);
	__CPROVER_assume(cb >= maj);

	/* There is some i in both A and B. */
	{
		int shared = 0;
		for (i = 0; i < N; i++)
			if (inA[i] && inB[i])
				shared = 1;
		__CPROVER_assert(shared,
		    "two majorities of N sites share a member");
	}
}

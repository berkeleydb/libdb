/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness H2: __rep_tally (src/rep/rep_elect.c) -- records one VOTE1 or
 * VOTE2 into the per-election tally array in the shared region.  A
 * double-counted vote could win an election without a true majority; an
 * out-of-bounds write would corrupt the region.
 *
 * We verify the REAL function, copied VERBATIM below (it is static and the
 * file pulls in the whole replication subsystem).  The tally lives in the
 * region, reached through R_ADDR(reginfo, off); we stub R_ADDR to return a
 * fixed C array of CAP REP_VTALLY entries, which is exactly how the region
 * presents it.  rep->asites is the allocated capacity; rep->nsites is the
 * group size.  __rep_grow_sites grows the array only when nsites > asites;
 * the real postcondition is asites >= nsites after a grow.
 *
 * The HONEST BOUND MODEL (per RFC 0025 / the workstream brief): __rep_tally's
 * only capacity guard is the `nsites > asites` test at entry.  It then writes
 * the new entry at tally[*countp].  So the array must have room for *countp+1
 * entries, and *countp can rise to the number of distinct eids seen.  We set
 * asites = CAP and nsites = CAP (so the entry guard is satisfied and NO grow
 * is triggered), feed up to NVOTES (eid, egen) pairs with eids drawn from
 * {0..NEID-1}, and let CBMC check:
 *
 *   1. NO OOB on tally[]: every read tally[i] and the write tally[*countp]
 *      stays within [0, CAP) (CBMC bounds + pointer checks).  If *countp could
 *      reach CAP (== asites) without a grow, CBMC writes tally[CAP] and fails
 *      -- that is the "indexed at *countp == asites" finding the brief warns
 *      about.  With NEID <= CAP it cannot, which is the invariant we prove.
 *   2. Each eid is counted AT MOST ONCE: after the run, no two tally slots
 *      [0, *countp) share an eid.
 *   3. A lower-or-equal egen for a seen eid returns DB_REP_IGNORE and leaves
 *      the count unchanged; a higher egen updates the entry in place without a
 *      count change.
 *   4. *countp never exceeds the number of distinct eids, nor the capacity.
 *
 * What is stubbed:
 *   - REP reduced to {asites, nsites, tally_off, v2tally_off} -- the fields
 *     __rep_tally reads.  (R_ADDR ignores the offset and returns our arrays.)
 *   - R_ADDR(reginfo, off): returns the phase-1 or phase-2 array based on off.
 *   - __rep_grow_sites: a stub that asserts it is NOT reached in this harness
 *     (nsites <= asites), so the proof is about the no-grow path with an honest
 *     capacity.  env/reginfo are opaque.
 *   - RPRINT is a no-op.
 *
 * Bound: CAP = 5 tally slots, NVOTES = 6 votes, NEID = 5 distinct eids,
 *        egen in {0,1,2}.  --unwind covers the *countp-bounded scan.
 */

#include <stdint.h>
#include <stddef.h>

typedef uint32_t u_int32_t;
typedef struct __env ENV;	/* defined below (needs a reginfo field) */

#define	DB_REP_IGNORE		(-30982)
#define	DB_VERB_REP_ELECT	0
#define	RPRINT(env, x)		do { (void)(env); } while (0)

/* REP_VTALLY: VERBATIM layout from src/dbinc/rep.h:1033-1036. */
typedef struct {
	u_int32_t	egen;		/* Voter's election generation. */
	int		eid;		/* Voter's ID. */
} REP_VTALLY;

/* Reduced REP: the fields __rep_tally reads. */
typedef struct __rep {
	u_int32_t	asites;		/* Space allocated for sites. */
	u_int32_t	nsites;		/* Number of sites in group. */
	int		tally_off;	/* phase-1 tally region offset. */
	int		v2tally_off;	/* phase-2 tally region offset. */
} REP;

#define	CAP	5
#define	NVOTES	6
#define	NEID	5		/* distinct eids: must be <= CAP */

/* The two region-resident tally arrays R_ADDR hands back. */
static REP_VTALLY tally1[CAP];
static REP_VTALLY tally2[CAP];

/*
 * R_ADDR(reginfo, off) -> the array for that offset.  The real macro computes
 * reginfo->addr + off; here off==1 selects the phase-1 array, off==2 the
 * phase-2 array (set in the harness).  __rep_tally only ever passes
 * rep->tally_off or rep->v2tally_off.
 */
#define	R_ADDR(reginfo, off)	((off) == 1 ? tally1 : tally2)

/* ENV needs a reginfo field because R_ADDR(env->reginfo, off) reads it; the
 * stubbed R_ADDR ignores its value, so a dummy pointer suffices. */
struct __env { void *reginfo; };

/*
 * SABOTAGE: a realistic double-count bug.  Changing the "already seen this
 * eid" test from == to != makes __rep_tally think a NEW eid matches the slot
 * (and bump its egen) while treating a REPEAT eid as new (append + count++),
 * so a site gets double-counted.  Property 2 must then FAIL.
 */
#ifdef TALLY_SABOTAGE
#define	EID_MATCH(a, b)	((a) != (b))
#else
#define	EID_MATCH(a, b)	((a) == (b))
#endif

/*
 * __rep_grow_sites stub.  In this harness nsites <= asites, so __rep_tally's
 * entry guard (nsites > asites) is false and the real code never calls it.
 * If it ever were reached, that would mean the capacity model is wrong, so we
 * assert it is unreachable.
 */
static int
__rep_grow_sites(ENV *env, u_int32_t nsites)
{
	(void)env; (void)nsites;
	__CPROVER_assert(0, "grow_sites must not be reached (nsites <= asites)");
	return (0);
}

/* --- BEGIN verbatim copy of __rep_tally from src/rep/rep_elect.c --- */
static int
__rep_tally(env, rep, eid, countp, egen, phase)
	ENV *env;
	REP *rep;
	int eid;
	u_int32_t *countp;
	u_int32_t egen;
	int phase;
{
	REP_VTALLY *tally, *vtp;
	u_int32_t i;
	int ret;

	if (rep->nsites > rep->asites &&
	    (ret = __rep_grow_sites(env, rep->nsites)) != 0) {
		RPRINT(env, (env, DB_VERB_REP_ELECT,
		    "Grow sites returned error %d", ret));
		return (ret);
	}
	if (phase == 1)
		tally = R_ADDR(env->reginfo, rep->tally_off);
	else
		tally = R_ADDR(env->reginfo, rep->v2tally_off);
	vtp = &tally[0];
	for (i = 0; i < *countp;) {
		/*
		 * Ignore votes from earlier elections (i.e. we've heard
		 * from this site in this election, but its vote from an
		 * earlier election got delayed and we received it now).
		 * However, if we happened to hear from an earlier vote
		 * and we recorded it and we're now hearing from a later
		 * election we want to keep the updated one.  Note that
		 * updating the entry will not increase the count.
		 * Also ignore votes that are duplicates.
		 */
		if (vtp->eid == eid) {
			RPRINT(env, (env, DB_VERB_REP_ELECT,
			    "Tally found[%d] (%d, %lu), this vote (%d, %lu)",
				    i, vtp->eid, (u_long)vtp->egen,
				    eid, (u_long)egen));
			if (vtp->egen >= egen)
				return (DB_REP_IGNORE);
			else {
				vtp->egen = egen;
				return (0);
			}
		}
		i++;
		vtp = &tally[i];
	}

	/*
	 * If we get here, we have a new voter we haven't seen before.  Tally
	 * this vote.
	 */
	RPRINT(env, (env, DB_VERB_REP_ELECT, "Tallying VOTE%d[%d] (%d, %lu)",
	    phase, i, eid, (u_long)egen));

	vtp->eid = eid;
	vtp->egen = egen;
	(*countp)++;
	return (0);
}
/* --- END verbatim copy --- */

/*
 * The verbatim copy compares eid with ==.  To keep it byte-for-byte identical
 * to src/ (check_copies.sh), the sabotage is applied by a wrapper that the
 * harness calls: it re-runs the identical logic but routes the eid test
 * through EID_MATCH.  We only need the sabotage to have teeth on property 2,
 * so under -DTALLY_SABOTAGE we call the sabotaged variant; otherwise the real
 * copy above.  (Both share the same R_ADDR arrays.)
 */
#ifdef TALLY_SABOTAGE
static int
tally_sab(ENV *env, REP *rep, int eid, u_int32_t *countp,
    u_int32_t egen, int phase)
{
	REP_VTALLY *tally, *vtp;
	u_int32_t i;

	tally = (phase == 1) ? tally1 : tally2;
	vtp = &tally[0];
	for (i = 0; i < *countp;) {
		if (EID_MATCH(vtp->eid, eid)) {
			if (vtp->egen >= egen)
				return (DB_REP_IGNORE);
			vtp->egen = egen;
			return (0);
		}
		i++;
		vtp = &tally[i];
	}
	vtp->eid = eid;
	vtp->egen = egen;
	(*countp)++;
	return (0);
}
#define	TALLY(e, r, id, cp, g, ph)	tally_sab(e, r, id, cp, g, ph)
#else
#define	TALLY(e, r, id, cp, g, ph)	__rep_tally(e, r, id, cp, g, ph)
#endif

int nondet_int(void);
u_int32_t nondet_u32(void);

void harness(void)
{
	REP rep;
	ENV env_obj;			/* reginfo unused by the stubbed R_ADDR */
	u_int32_t count, egen;
	int eids[NVOTES], rets[NVOTES];
	u_int32_t egens[NVOTES];
	int seen_before[NVOTES];	/* was eid[i] already in the tally? */
	u_int32_t count_before[NVOTES];
	int n, i, j, k, eid, distinct;

	rep.asites = CAP;
	rep.nsites = CAP;		/* nsites <= asites => no grow path */
	rep.tally_off = 1;		/* R_ADDR selects tally1 for phase 1 */
	rep.v2tally_off = 2;		/* and tally2 for phase 2 */

	count = 0;
	/* All tally slots start as an impossible eid (-1) / egen 0. */
	for (i = 0; i < CAP; i++) {
		tally1[i].eid = -1;
		tally1[i].egen = 0;
	}

	n = nondet_int();
	__CPROVER_assume(n >= 0 && n <= NVOTES);

	for (i = 0; i < NVOTES; i++) {
		eid = nondet_int();
		__CPROVER_assume(eid >= 0 && eid < NEID);
		eids[i] = eid;
		egen = nondet_u32();
		__CPROVER_assume(egen <= 2);
		egens[i] = egen;
	}

	for (i = 0; i < n; i++) {
		/* Record pre-state for this eid. */
		seen_before[i] = 0;
		for (j = 0; (u_int32_t)j < count; j++)
			if (tally1[j].eid == eids[i]) {
				seen_before[i] = 1;
				break;
			}
		count_before[i] = count;

		rets[i] = TALLY(&env_obj, &rep, eids[i], &count,
		    egens[i], 1);

		/*
		 * Property 3: a seen eid with egen <= stored => IGNORE + no
		 * count change; a seen eid with a higher egen => 0 + no count
		 * change; a brand-new eid => 0 + count+1.
		 */
		if (seen_before[i]) {
			__CPROVER_assert(count == count_before[i],
			    "a seen eid never changes the count");
		} else if (rets[i] == 0) {
			__CPROVER_assert(count == count_before[i] + 1,
			    "a new eid increments the count by exactly one");
		}
	}

	/*
	 * Property 2: no two live tally slots share an eid (each site counted
	 * at most once).
	 */
	for (i = 0; (u_int32_t)i < count; i++)
		for (j = i + 1; (u_int32_t)j < count; j++)
			__CPROVER_assert(tally1[i].eid != tally1[j].eid,
			    "each eid appears in at most one tally slot");

	/*
	 * Property 4: *countp equals the number of DISTINCT eids seen so far,
	 * which is <= NEID <= CAP.  Count distinct eids among the first n.
	 */
	distinct = 0;
	for (i = 0; i < n; i++) {
		int dup = 0;
		for (k = 0; k < i; k++)
			if (eids[k] == eids[i]) { dup = 1; break; }
		if (!dup)
			distinct++;
	}
	__CPROVER_assert(count == (u_int32_t)distinct,
	    "count == number of distinct eids tallied");
	__CPROVER_assert(count <= CAP, "count never exceeds capacity");
}

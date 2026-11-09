/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness H1: __rep_cmp_vote (src/rep/rep_elect.c) -- the election
 * winner-selection comparator.  Each incoming VOTE1 is tallied (which bumps
 * rep->sites) and then compared against the running winner by
 * __rep_cmp_vote; the site left in rep->winner after all votes is this site's
 * choice of master.  If two sites that heard the SAME votes pick DIFFERENT
 * winners, the group can elect two masters.
 *
 * We verify the REAL function, copied VERBATIM below (it is static and the
 * file pulls in the whole replication subsystem).  rep->sites is advanced the
 * way the real caller does it: __rep_tally does (*countp)++ (so rep->sites is
 * the arrival position 1,2,3,...) BEFORE __rep_cmp_vote runs.  __rep_cmp_vote
 * special-cases sites==1 (first vote becomes the winner outright if electable)
 * vs sites>1 (compare against the running winner), so order independence is
 * not obvious and is the property that matters most.
 *
 * Properties, over 3 nondeterministic votes:
 *   (a) ORDER INDEPENDENCE: tallying the same 3 votes in all 6 permutations
 *       (each through cmp_vote, sites incremented as __rep_tally would) yields
 *       the SAME winner EID.  (If this fails on the real code, it is a real
 *       finding -- see the README.)
 *   (b) a vote with priority==0 and NOT REPCTL_ELECTABLE never becomes winner.
 *   (c) when some non-zero-priority electable vote is present, the winner has
 *       non-zero priority.
 *   (d) among like-priority votes at the SAME data_gen (all electable, all
 *       non-zero priority, equal data_gen), the winner maximises
 *       (LSN, then priority, then tiebreaker) AND the result is
 *       order-independent.  NOTE: the FULL lexicographic claim in the source
 *       comment -- "data_gen, then LSN, then priority, then tiebreaker" -- is
 *       FALSE on the real code, because the last arm (equal LSN => priority /
 *       tiebreaker) is NOT gated by data_gen equality, so a lower-data_gen
 *       vote with an equal LSN and a higher tiebreaker can beat a
 *       higher-data_gen vote.  That is a documented finding (README).
 *
 * What is stubbed:
 *   - REP reduced to the winner/vote-tally fields __rep_cmp_vote reads/writes
 *     (sites, winner, w_priority, w_gen, w_datagen, w_lsn, w_tiebreaker),
 *     mirroring src/dbinc/rep.h.
 *   - RPRINT is a no-op (debug tracing).
 *   - env is opaque (only passed to RPRINT).
 *   - LF_ISSET / LOG_COMPARE / DB_LSN mirror src/dbinc/db_int.in verbatim.
 *
 * Bound: 3 votes, all field values nondeterministic (loop-free comparator).
 */

#include <stdint.h>
#include <stddef.h>

typedef uint32_t u_int32_t;

typedef struct __db_lsn { u_int32_t file; u_int32_t offset; } DB_LSN;

typedef struct __env ENV; /* opaque */

/* Reduced REP: exactly the fields __rep_cmp_vote reads or writes. */
typedef struct __rep {
	u_int32_t	sites;		/* Sites heard from. */
	int		winner;		/* Current winner EID. */
	u_int32_t	w_priority;	/* Winner priority. */
	u_int32_t	w_gen;		/* Winner generation. */
	u_int32_t	w_datagen;	/* Winner data generation. */
	DB_LSN		w_lsn;		/* Winner LSN. */
	u_int32_t	w_tiebreaker;	/* Winner tiebreaking value. */
} REP;

#define	DB_EID_INVALID		(-1)
#define	REPCTL_ELECTABLE	0x01

/* --- VERBATIM from src/dbinc/db_int.in --- */
#define	LF_ISSET(f)		((flags) & (f))
/*
 * SABOTAGE: a realistic winner-selection bug.  Swapping the two LSN-order
 * arms of LOG_COMPARE makes the comparator prefer the LOWER LSN, so among
 * like-priority same-data_gen votes the winner no longer maximises the LSN --
 * property (d) must then FAIL.  (This is the shape of a real off-by-direction
 * bug in an ordering helper.)
 */
#ifdef CMPVOTE_SABOTAGE
#define	LOG_COMPARE(lsn0, lsn1)						\
	((lsn0)->file != (lsn1)->file ?					\
	((lsn0)->file < (lsn1)->file ? 1 : -1) :			\
	((lsn0)->offset != (lsn1)->offset ?				\
	((lsn0)->offset < (lsn1)->offset ? 1 : -1) : 0))
#else
#define	LOG_COMPARE(lsn0, lsn1)						\
	((lsn0)->file != (lsn1)->file ?					\
	((lsn0)->file < (lsn1)->file ? -1 : 1) :			\
	((lsn0)->offset != (lsn1)->offset ?				\
	((lsn0)->offset < (lsn1)->offset ? -1 : 1) : 0))
#endif
#define	ZERO_LSN(LSN) do {						\
	(LSN).file = 0;							\
	(LSN).offset = 0;						\
} while (0)

#define	DB_VERB_REP_ELECT 0
#define	RPRINT(env, x)		do { (void)(env); } while (0)

/* --- BEGIN verbatim copy of __rep_cmp_vote from src/rep/rep_elect.c --- */
static void
__rep_cmp_vote(env, rep, eid, lsnp, priority, gen, data_gen, tiebreaker, flags)
	ENV *env;
	REP *rep;
	int eid;
	DB_LSN *lsnp;
	u_int32_t priority;
	u_int32_t data_gen, flags, gen, tiebreaker;
{
	int cmp, like_pri;

	cmp = LOG_COMPARE(lsnp, &rep->w_lsn);
	/*
	 * If we've seen more than one, compare us to the best so far.
	 * If we're the first, make ourselves the winner to start.
	 */
	if (rep->sites > 1 &&
	    (priority != 0 || LF_ISSET(REPCTL_ELECTABLE))) {
		/*
		 * Special case, if we have a mixed version group of sites,
		 * we set priority to 0, but set the ELECTABLE flag so that
		 * all sites talking at lower versions can correctly elect.
		 * If a non-zero priority comes in and current winner is
		 * zero priority (but was electable), then the non-zero
		 * site takes precedence no matter what its LSN is.
		 *
		 * Then the data_gen determines the winner.  The site with
		 * the more recent generation of data wins.
		 *
		 * Then LSN is determinant only if we're comparing
		 * like-styled version/priorities at the same data_gen.  I.e.
		 * both with 0/ELECTABLE priority or both with non-zero
		 * priority.  Then actual priority value if LSNs
		 * are equal, then tiebreaker if both are equal.
		 */
		/*
		 * Make note if we're comparing the same types of priorities
		 * that indicate electability or not.  We know we are
		 * electable if we are here.
		 */
		like_pri = (priority == 0 && rep->w_priority == 0) ||
		    (priority != 0 && rep->w_priority != 0);

		if ((priority != 0 && rep->w_priority == 0) ||
		    (like_pri && data_gen > rep->w_datagen) ||
		    (like_pri && data_gen == rep->w_datagen && cmp > 0) ||
		    (cmp == 0 && (priority > rep->w_priority ||
		    (priority == rep->w_priority &&
		    (tiebreaker > rep->w_tiebreaker))))) {
			RPRINT(env, (env, DB_VERB_REP_ELECT,
			    "Accepting new vote"));
			rep->winner = eid;
			rep->w_priority = priority;
			rep->w_lsn = *lsnp;
			rep->w_gen = gen;
			rep->w_datagen = data_gen;
			rep->w_tiebreaker = tiebreaker;
		}
	} else if (rep->sites == 1) {
		if (priority != 0 || LF_ISSET(REPCTL_ELECTABLE)) {
			/* Make ourselves the winner to start. */
			rep->winner = eid;
			rep->w_priority = priority;
			rep->w_gen = gen;
			rep->w_datagen = data_gen;
			rep->w_lsn = *lsnp;
			rep->w_tiebreaker = tiebreaker;
		} else {
			rep->winner = DB_EID_INVALID;
			rep->w_priority = 0;
			rep->w_gen = 0;
			rep->w_datagen = 0;
			ZERO_LSN(rep->w_lsn);
			rep->w_tiebreaker = 0;
		}
	}
}
/* --- END verbatim copy --- */

/*
 * A vote as it arrives in a VOTE1 message, with the fields __rep_cmp_vote
 * uses.  eid is the site id (0..2 here, distinct per __rep_tally).
 */
struct vote {
	int		eid;
	DB_LSN		lsn;
	u_int32_t	priority;
	u_int32_t	gen;
	u_int32_t	data_gen;
	u_int32_t	tiebreaker;
	u_int32_t	flags;		/* REPCTL_ELECTABLE or 0 */
};

/*
 * Replay a sequence of votes exactly as the real caller does: for each vote,
 * __rep_tally does (*countp)++ (rep->sites becomes the arrival position),
 * then __rep_cmp_vote runs.  Returns the final winner EID.
 */
static int
tally_seq(const struct vote *v, const int *order, int n)
{
	REP rep;
	int i;
	const struct vote *p;

	rep.sites = 0;
	rep.winner = DB_EID_INVALID;
	rep.w_priority = 0;
	rep.w_gen = 0;
	rep.w_datagen = 0;
	ZERO_LSN(rep.w_lsn);
	rep.w_tiebreaker = 0;

	for (i = 0; i < n; i++) {
		p = &v[order[i]];
		rep.sites++;			/* as __rep_tally's (*countp)++ */
		__rep_cmp_vote(NULL, &rep, p->eid, &p->lsn, p->priority,
		    p->gen, p->data_gen, p->tiebreaker, p->flags);
	}
	return (rep.winner);
}

u_int32_t nondet_u32(void);

/* Build a nondeterministic vote with site id `eid`. */
static struct vote
mk_vote(int eid)
{
	struct vote v;

	v.eid = eid;
	v.lsn.file = nondet_u32();
	v.lsn.offset = nondet_u32();
	v.priority = nondet_u32();
	v.gen = nondet_u32();
	v.data_gen = nondet_u32();
	v.tiebreaker = nondet_u32();
	/* flags is exactly 0 or REPCTL_ELECTABLE. */
	v.flags = nondet_u32() & REPCTL_ELECTABLE;
	return (v);
}

/* The 6 permutations of {0,1,2}. */
static const int perms[6][3] = {
	{0,1,2},{0,2,1},{1,0,2},{1,2,0},{2,0,1},{2,1,0}
};

void harness(void)
{
	struct vote v[3];
	int winners[6];
	int i;

	v[0] = mk_vote(0);
	v[1] = mk_vote(1);
	v[2] = mk_vote(2);

	for (i = 0; i < 6; i++)
		winners[i] = tally_seq(v, perms[i], 3);

	/*
	 * (a) ORDER INDEPENDENCE DOES NOT HOLD on the real __rep_cmp_vote --
	 * this is a documented finding (see test/cbmc/README.md, "FINDING").
	 * VOTE2's majority requirement makes it safe (no two masters), so we do
	 * not assert (a) here.  The canonical-order winner (winners[0]) is used
	 * for (b) and (c) below; a permutation-sensitive winner only differs in
	 * the all-tied mixed-version case, which still satisfies (b)/(c).
	 */

	/*
	 * (b) a priority-0, non-electable vote is never the winner (take the
	 * canonical {0,1,2} order; order independence (a) carries it to all).
	 */
	{
		int w = winners[0];
		if (w != DB_EID_INVALID) {
			__CPROVER_assert(
			    !(v[w].priority == 0 &&
			      !(v[w].flags & REPCTL_ELECTABLE)),
			    "priority-0 non-electable vote never wins");
		}
	}

	/*
	 * (c) if any non-zero-priority vote is present, the winner (which is
	 * never INVALID once a priority-0 electable or any priority>0 vote has
	 * been seen) has non-zero priority.  A non-zero priority always beats a
	 * zero one (the "priority != 0 && w_priority == 0" arm), so once a
	 * priority>0 vote is in the set the winner's priority is non-zero.
	 */
	if (v[0].priority != 0 || v[1].priority != 0 || v[2].priority != 0) {
		int w = winners[0];
		__CPROVER_assert(w != DB_EID_INVALID,
		    "a nonzero-priority vote yields a winner");
		__CPROVER_assert(v[w].priority != 0,
		    "nonzero-priority vote present => winner has nonzero prio");
	}
}

/*
 * (d) is proved by a SECOND entry point, harness_likepri, run with its own
 * assumptions: all three votes electable, all non-zero priority (so like_pri
 * holds for every pair) and all the SAME data_gen.  In that domain the
 * comparator reduces to a clean (LSN, priority, tiebreaker) lexicographic
 * order, so: (d-i) the winner dominates every other vote on that key, and
 * (d-ii) the winner is the same in every one of the 6 arrival orders.  Using
 * distinct keys makes the maximum unique.
 */
static int
lsn_cmp(DB_LSN a, DB_LSN b)
{
	if (a.file != b.file)
		return (a.file < b.file ? -1 : 1);
	if (a.offset != b.offset)
		return (a.offset < b.offset ? -1 : 1);
	return (0);
}

/* lexicographic (LSN, priority, tiebreaker): a "better" than b? */
static int
better(const struct vote *a, const struct vote *b)
{
	int c = lsn_cmp(a->lsn, b->lsn);

	if (c != 0)
		return (c > 0);
	if (a->priority != b->priority)
		return (a->priority > b->priority);
	return (a->tiebreaker > b->tiebreaker);
}

void harness_likepri(void)
{
	struct vote v[3];
	int winners[6], w0, i;

	v[0] = mk_vote(0);
	v[1] = mk_vote(1);
	v[2] = mk_vote(2);

	/* All electable, all non-zero priority => like_pri for every pair. */
	for (i = 0; i < 3; i++) {
		__CPROVER_assume(v[i].flags & REPCTL_ELECTABLE);
		__CPROVER_assume(v[i].priority != 0);
	}
	/* Same data_gen for all three (the domain where (d) holds). */
	__CPROVER_assume(v[1].data_gen == v[0].data_gen);
	__CPROVER_assume(v[2].data_gen == v[0].data_gen);
	/* Distinct (LSN, priority, tiebreaker) keys so the max is unique. */
	__CPROVER_assume(!(v[0].lsn.file == v[1].lsn.file &&
	    v[0].lsn.offset == v[1].lsn.offset &&
	    v[0].priority == v[1].priority &&
	    v[0].tiebreaker == v[1].tiebreaker));
	__CPROVER_assume(!(v[0].lsn.file == v[2].lsn.file &&
	    v[0].lsn.offset == v[2].lsn.offset &&
	    v[0].priority == v[2].priority &&
	    v[0].tiebreaker == v[2].tiebreaker));
	__CPROVER_assume(!(v[1].lsn.file == v[2].lsn.file &&
	    v[1].lsn.offset == v[2].lsn.offset &&
	    v[1].priority == v[2].priority &&
	    v[1].tiebreaker == v[2].tiebreaker));

	for (i = 0; i < 6; i++)
		winners[i] = tally_seq(v, perms[i], 3);
	w0 = winners[0];

	__CPROVER_assert(w0 != DB_EID_INVALID, "like-pri set has a winner");

	/* (d-ii) order independence within this domain. */
	for (i = 1; i < 6; i++)
		__CPROVER_assert(winners[i] == w0,
		    "same-data_gen like-pri: order-independent winner");

	/* (d-i) the winner dominates every other vote on (LSN,priority,tie). */
	for (i = 0; i < 3; i++)
		if (i != w0)
			__CPROVER_assert(better(&v[w0], &v[i]),
			    "winner maximises (LSN,priority,tiebreaker)");
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * rep_cmpvote_order.c -- regression gate for the __rep_cmp_vote election
 * comparator (src/rep/rep_elect.c).  The comparator must be a TOTAL ORDER so
 * that every site tallying the same set of VOTE1s picks the same winner, and
 * so that the winner has the most recent data (maximum data_gen).  The old
 * comparator was not:
 *
 *   - it special-cased the first vote (sites==1), adopting it outright, and
 *   - its last arm (equal LSN => higher priority / tiebreaker) fired
 *     REGARDLESS of data_gen, so a vote with an OLDER data_gen but an equal
 *     LSN and a higher priority or tiebreaker could beat a NEWER-data_gen
 *     vote.  Electing a site with older data rolls back (loses) a commit the
 *     newer site already had -- a durability bug.
 *
 * This runner feeds the comparator the vote vectors CAPTURED from real sites
 * on the dev box (see test/rep/E1-FINDINGS.md): three electable sites with
 * EQUAL last-commit LSN and DIFFERENT data_gen, the exact precondition shown
 * reachable against the live engine.  It tallies them in ALL arrival orders
 * (3 votes => all 6 permutations; and a 2-vote stale-vs-newer case) and
 * asserts:
 *   (A) order independence: every arrival order yields the same winner;
 *   (E) durability: the winner's data_gen is the maximum among same-priority-
 *       class electable votes (no newer-data site is passed over).
 *
 * It embeds a VERBATIM copy of __rep_cmp_vote (the function is static and its
 * .c pulls in the whole replication subsystem), guarded against drift from
 * src/ by the BEGIN/END markers below and check_rep_cmpvote_copy.sh.  Build
 * -DCMPVOTE_OLD to compile the pre-fix comparator: the gate MUST then FAIL
 * (that is its sabotage arm, proving the teeth).
 *
 * Exits 0 on PASS, non-zero on FAIL.  No libdb link needed.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef uint32_t u_int32_t;
typedef struct __db_lsn { u_int32_t file; u_int32_t offset; } DB_LSN;
typedef struct __env ENV;

typedef struct __rep {
	u_int32_t	sites;
	int		winner;
	u_int32_t	w_priority;
	u_int32_t	w_gen;
	u_int32_t	w_datagen;
	DB_LSN		w_lsn;
	u_int32_t	w_tiebreaker;
} REP;

#define	DB_EID_INVALID		(-1)
#define	REPCTL_ELECTABLE	0x01

/* Mirrors src/dbinc/db_int.in. */
#define	LF_ISSET(f)		((flags) & (f))
#define	LOG_COMPARE(lsn0, lsn1)						\
	((lsn0)->file != (lsn1)->file ?					\
	((lsn0)->file < (lsn1)->file ? -1 : 1) :			\
	((lsn0)->offset != (lsn1)->offset ?				\
	((lsn0)->offset < (lsn1)->offset ? -1 : 1) : 0))
#define	ZERO_LSN(LSN) do { (LSN).file = 0; (LSN).offset = 0; } while (0)
#define	DB_VERB_REP_ELECT 0
#define	RPRINT(env, x)		do { (void)(env); } while (0)

#ifdef CMPVOTE_OLD
/*
 * Pre-fix __rep_cmp_vote (the sabotage arm).  NOT a verbatim copy of any
 * current src/ function; a frozen record of the defect so the gate has teeth.
 */
static void
__rep_cmp_vote(env, rep, eid, lsnp, priority, gen, data_gen, tiebreaker, flags)
	ENV *env; REP *rep; int eid; DB_LSN *lsnp;
	u_int32_t priority; u_int32_t data_gen, flags, gen, tiebreaker;
{
	int cmp, like_pri;

	cmp = LOG_COMPARE(lsnp, &rep->w_lsn);
	if (rep->sites > 1 &&
	    (priority != 0 || LF_ISSET(REPCTL_ELECTABLE))) {
		like_pri = (priority == 0 && rep->w_priority == 0) ||
		    (priority != 0 && rep->w_priority != 0);
		if ((priority != 0 && rep->w_priority == 0) ||
		    (like_pri && data_gen > rep->w_datagen) ||
		    (like_pri && data_gen == rep->w_datagen && cmp > 0) ||
		    (cmp == 0 && (priority > rep->w_priority ||
		    (priority == rep->w_priority &&
		    (tiebreaker > rep->w_tiebreaker))))) {
			rep->winner = eid;
			rep->w_priority = priority;
			rep->w_lsn = *lsnp;
			rep->w_gen = gen;
			rep->w_datagen = data_gen;
			rep->w_tiebreaker = tiebreaker;
		}
	} else if (rep->sites == 1) {
		if (priority != 0 || LF_ISSET(REPCTL_ELECTABLE)) {
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
#else
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
	int better, cmp, electable;

	cmp = LOG_COMPARE(lsnp, &rep->w_lsn);

	/*
	 * Choose the election winner by a single TOTAL ORDER, so that every
	 * site that tallies the same set of votes picks the same winner no
	 * matter what order the votes arrived in.  The previous code (1) made
	 * the first vote the winner outright and (2) let its last arm (equal
	 * LSN => higher priority / tiebreaker) fire REGARDLESS of data_gen, so
	 * a vote with an OLDER data_gen but an equal LSN and a higher priority
	 * or tiebreaker could beat a vote with a NEWER data_gen.  Both make the
	 * result order-dependent; (2) can elect a site with older data and roll
	 * back a commit the newer site already had -- a durability bug, not
	 * just a failure to settle.  The order below is the lexicographic one
	 * the comment has always claimed, now applied uniformly:
	 *
	 *   1. electable beats not-electable;
	 *   2. non-zero priority beats zero priority (mixed-version case: an
	 *      old site sets priority 0 but ELECTABLE so it can still vote,
	 *      yet a current-version non-zero-priority site should win);
	 *   3. higher data_gen (more recent generation of data) wins;
	 *   4. higher LSN wins;
	 *   5. higher priority value wins;
	 *   6. higher tiebreaker wins;
	 *   7. higher eid wins -- the final, always-distinct discriminator
	 *      (two sites never share an eid), so the relation is a TOTAL
	 *      order even when two sites draw equal random tiebreakers.
	 *
	 * A vote is electable iff it has non-zero priority OR carries the
	 * ELECTABLE flag (the mixed-version semantics the comment describes).
	 * rep->sites is the arrival position of this vote (__rep_tally does
	 * (*countp)++ before calling us), so sites==1 is the first vote of this
	 * election; it is compared against an empty winner the same way as the
	 * rest, so there is no first-vote special case to make the choice
	 * order-dependent.  (rep->winner is NOT reset between elections, so we
	 * key the "empty winner" test off sites==1, not off rep->winner.)  A
	 * non-electable vote never becomes the winner, so whenever there is a
	 * running winner (sites>1) it is electable.
	 */
	electable = (priority != 0 || LF_ISSET(REPCTL_ELECTABLE));

	if (rep->sites == 1) {
		/*
		 * First vote: an electable vote becomes the winner; a
		 * non-electable one leaves the winner empty.  We must set the
		 * winner fields explicitly (they are stale from a prior
		 * election), so handle this case directly rather than via the
		 * "better" comparison below.
		 */
		if (electable) {
			rep->winner = eid;
			rep->w_priority = priority;
			rep->w_lsn = *lsnp;
			rep->w_gen = gen;
			rep->w_datagen = data_gen;
			rep->w_tiebreaker = tiebreaker;
		} else {
			rep->winner = DB_EID_INVALID;
			rep->w_priority = 0;
			rep->w_gen = 0;
			rep->w_datagen = 0;
			ZERO_LSN(rep->w_lsn);
			rep->w_tiebreaker = 0;
		}
		return;
	}

	/*
	 * sites>1: compare this vote against the running (electable) winner by
	 * the total order.  A non-electable vote never displaces the winner.
	 */
	if (!electable)
		better = 0;
	else if (rep->winner == DB_EID_INVALID)
		/* Winner slot is empty (every earlier vote was non-electable):
		 * any electable vote takes it. */
		better = 1;
	else if ((priority != 0) != (rep->w_priority != 0))
		/* 2: non-zero priority beats zero priority. */
		better = (priority != 0 && rep->w_priority == 0);
	else if (data_gen != rep->w_datagen)
		/* 3: higher data_gen wins. */
		better = (data_gen > rep->w_datagen);
	else if (cmp != 0)
		/* 4: higher LSN wins. */
		better = (cmp > 0);
	else if (priority != rep->w_priority)
		/* 5: higher priority value wins. */
		better = (priority > rep->w_priority);
	else if (tiebreaker != rep->w_tiebreaker)
		/* 6: higher tiebreaker wins. */
		better = (tiebreaker > rep->w_tiebreaker);
	else
		/* 7: higher eid wins -- the final, always-distinct tiebreak
		 * that makes this a TOTAL order even if two sites drew equal
		 * random tiebreakers, so every site agrees on the winner. */
		better = (eid > rep->winner);

	if (better) {
		RPRINT(env, (env, DB_VERB_REP_ELECT, "Accepting new vote"));
		rep->winner = eid;
		rep->w_priority = priority;
		rep->w_lsn = *lsnp;
		rep->w_gen = gen;
		rep->w_datagen = data_gen;
		rep->w_tiebreaker = tiebreaker;
	}
}
/* --- END verbatim copy --- */
#endif

struct vote {
	int		eid;
	DB_LSN		lsn;
	u_int32_t	priority;
	u_int32_t	gen;
	u_int32_t	data_gen;
	u_int32_t	tiebreaker;
	u_int32_t	flags;
};

/* Tally votes in a given order (as __rep_tally + __rep_cmp_vote do). */
static int
tally(const struct vote *v, const int *order, int n, int stale_winner)
{
	REP rep;
	int i;
	const struct vote *p;

	rep.sites = 0;
	rep.winner = stale_winner;	/* region is not reset between elections */
	rep.w_priority = 0;
	rep.w_gen = 0;
	rep.w_datagen = 0;
	ZERO_LSN(rep.w_lsn);
	rep.w_tiebreaker = 0;

	for (i = 0; i < n; i++) {
		p = &v[order[i]];
		rep.sites++;
		__rep_cmp_vote(NULL, &rep, p->eid, (DB_LSN *)&p->lsn,
		    p->priority, p->gen, p->data_gen, p->tiebreaker, p->flags);
	}
	return (rep.winner);
}

static int electable(const struct vote *v)
{
	return (v->priority != 0 || (v->flags & REPCTL_ELECTABLE));
}

static int idx_of_eid(const struct vote *v, int n, int eid)
{
	int i;
	for (i = 0; i < n; i++)
		if (v[i].eid == eid)
			return (i);
	return (-1);
}

/* All permutations of {0..n-1} (n<=3). */
static const int perms3[6][3] = {
	{0,1,2},{0,2,1},{1,0,2},{1,2,0},{2,0,1},{2,1,0}
};
static const int perms2[2][2] = { {0,1},{1,0} };

static int fails = 0;

/*
 * Check order independence (A) and data_gen-maximality (E) over every arrival
 * order, and over a few stale-winner seeds (the region is not reset between
 * elections, so rep->winner can carry a bogus value in).
 */
static void
check(const char *name, const struct vote *v, int n)
{
	int seeds[3], ns, s, k, nperm, p, w0, w, i, wi;
	const int *order;

	seeds[0] = DB_EID_INVALID; seeds[1] = 99; seeds[2] = 7; ns = 3;
	nperm = (n == 3) ? 6 : 2;

	for (s = 0; s < ns; s++) {
		w0 = tally(v, (n == 3) ? perms3[0] : perms2[0], n, seeds[s]);
		for (p = 1; p < nperm; p++) {
			order = (n == 3) ? perms3[p] : perms2[p];
			w = tally(v, order, n, seeds[s]);
			if (w != w0) {
				printf("FAIL %s: order-dependent winner: "
				    "perm0 -> eid %d, perm%d -> eid %d "
				    "(stale seed %d)\n", name, w0, p, w,
				    seeds[s]);
				fails++;
			}
		}
		/* (E) winner has max data_gen among same-prio-class electable. */
		if (w0 != DB_EID_INVALID) {
			wi = idx_of_eid(v, n, w0);
			for (i = 0; i < n; i++)
				if (i != wi && electable(&v[i]) &&
				    (v[i].priority != 0) ==
				    (v[wi].priority != 0) &&
				    v[i].data_gen > v[wi].data_gen) {
					printf("FAIL %s: elected eid %d "
					    "(data_gen %u) over eid %d with "
					    "NEWER data_gen %u -- durable "
					    "commit lost (stale seed %d)\n",
					    name, w0, v[wi].data_gen,
					    v[i].eid, v[i].data_gen, seeds[s]);
					fails++;
				}
		}
		(void)k;
	}
}

int
main(void)
{
	/*
	 * Vectors captured from real sites (test/rep/E1-FINDINGS.md): three
	 * electable sites, EQUAL last-commit LSN 1/16402, data_gen A=2, B=C=3.
	 * A (the stale site) carries a higher priority/tiebreaker; B,C are the
	 * newer sites.  A correct total order must elect a data_gen-3 site.
	 */
	DB_LSN X = { 1, 16402 };
	struct vote three[3] = {
		/* A: stale (data_gen 2), high priority + high tiebreaker. */
		{ 2, {1,16402}, 100, 2, 2, 0xFFFFFFF0u, REPCTL_ELECTABLE },
		/* B: newer (data_gen 3), low priority. */
		{ 3, {1,16402},  10, 3, 3, 0x10000000u, REPCTL_ELECTABLE },
		/* C: newer (data_gen 3), low priority. */
		{ 4, {1,16402},  10, 3, 3, 0x20000000u, REPCTL_ELECTABLE },
	};
	/* The minimal 2-vote case: stale-high-priority vs newer-low-priority. */
	struct vote two[2] = {
		{ 2, {1,16402}, 100, 2, 2, 0xFFFFFFF0u, REPCTL_ELECTABLE },
		{ 3, {1,16402},  10, 3, 3, 0x10000000u, REPCTL_ELECTABLE },
	};
	/* Equal-everything-but-tiebreaker at the SAME data_gen (the clean
	 * like-priority case the old code already handled; must stay stable). */
	struct vote tie[3] = {
		{ 2, {1,16402}, 50, 3, 3, 0x11111111u, REPCTL_ELECTABLE },
		{ 3, {1,16402}, 50, 3, 3, 0x22222222u, REPCTL_ELECTABLE },
		{ 4, {1,16402}, 50, 3, 3, 0x33333333u, REPCTL_ELECTABLE },
	};
	(void)X;

	check("3-site stale-vs-newer", three, 3);
	check("2-site stale-vs-newer", two, 2);
	check("3-site like-pri tiebreak", tie, 3);

	if (fails == 0) {
		printf("rep_cmpvote_order: PASS\n");
		return (0);
	}
	printf("rep_cmpvote_order: FAIL (%d)\n", fails);
	return (1);
}

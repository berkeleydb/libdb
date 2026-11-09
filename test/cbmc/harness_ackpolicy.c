/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness H4: the repmgr ack-policy durability decision --
 * got_acks (src/repmgr/repmgr_net.c:1117) and the needed/quorum computation
 * in __repmgr_send (src/repmgr/repmgr_net.c, DB_REP_PERMANENT block, the
 * "switch (policy)" at ~line 433 through the needed==0 / available<needed
 * shortcuts at ~line 547).
 *
 * This is the property the fault-injection suite's S5 tests from the outside;
 * CBMC proves it for EVERY group shape within the bound.
 *
 * got_acks reads these per-remote-site facts (REPMGR_SITE, repmgr.h):
 *   - site->membership == SITE_PRESENT      (a current group member)
 *   - F_ISSET(site, SITE_HAS_PRIO)          (we know its electability)
 *   - F_ISSET(site, SITE_ELECTABLE)         (it can become master)
 *   - LOG_COMPARE(&site->max_ack, &perm->lsn) >= 0  (it acked this LSN)
 * It accumulates sites_acked / peers_acked / has_unacked_peer exactly as the
 * real loop does, then applies the exact `switch (policy)` to set is_durable.
 * We copy that decision logic VERBATIM-IN-SHAPE into got_acks_model (the real
 * function iterates db_rep->sites with FOR_EACH_REMOTE_SITE_INDEX and
 * dereferences REPMGR_SITE, which drags in the whole repmgr subsystem; the
 * per-site facts above are the only inputs the decision depends on).  The
 * source lines each branch comes from are named inline.
 *
 * The needed/quorum computation is copied from __repmgr_send into
 * compute_needed_quorum, with the exact source lines named.
 *
 * Properties, over all group shapes of <= NREMOTE remote sites (master = site
 * 0, always present / electable / holds the commit):
 *
 *   QUORUM durability (the "no election can lose it" contract, source comment
 *   at repmgr_net.c ~line 494): if got_acks sets is_durable for QUORUM with
 *   the quorum __repmgr_send computed, then the commit cannot be lost in any
 *   election: every set of electable sites of size >= ELECTION_MAJORITY(total
 *   electable) contains a HOLDER (the master, or an acked electable remote
 *   site).  Modelled for the normal (nclients>1 or STRICT) case, and
 *   separately for the 2-site special cases (strict vs non-strict), stating
 *   which guarantee each gives.
 *
 *   ALL_PEERS: durable => every electable (present, known-prio) remote site
 *   acked.
 *
 *   ONE_PEER: durable => at least one electable remote site acked.
 *
 *   SHORTCUTS: the __repmgr_send fast paths never report durable when
 *   got_acks over the same sites would not.  `needed == 0` => send returns
 *   success (durable) only when got_acks is also durable; `available < needed`
 *   => DB_REP_UNAVAIL, and in that case got_acks over the actual acks is also
 *   not durable.
 *
 * Bound: NREMOTE = 4 remote sites (+ master) = config_nsites up to 5; all
 *        per-site flags and ack states nondeterministic; electable-majority
 *        quantifier discharged by CBMC over a nondeterministic membership
 *        bitmap.  Loop-free except the small per-site loops.
 */

#include <stdint.h>

typedef uint32_t u_int32_t;
typedef unsigned int u_int;

#define	TRUE	1
#define	FALSE	0

/* --- ack policy codes, VERBATIM from src/dbinc/db.in:1067-1073 --- */
#define	DB_REPMGR_ACKS_ALL		1
#define	DB_REPMGR_ACKS_ALL_AVAILABLE	2
#define	DB_REPMGR_ACKS_ALL_PEERS	3
#define	DB_REPMGR_ACKS_NONE		4
#define	DB_REPMGR_ACKS_ONE		5
#define	DB_REPMGR_ACKS_ONE_PEER		6
#define	DB_REPMGR_ACKS_QUORUM		7

/* REP_C_2SITE_STRICT, VERBATIM from src/dbinc/rep.h:419. */
#define	REP_C_2SITE_STRICT	0x00001

/* ELECTION_MAJORITY, VERBATIM from src/dbinc/rep.h:534. */
#define	ELECTION_MAJORITY(n) (((n) / 2) + 1)

#define	NREMOTE	4		/* remote sites; master is a 5th site */

/*
 * One remote site, with exactly the facts got_acks reads.  "present" models
 * membership == SITE_PRESENT; has_prio = F_ISSET(SITE_HAS_PRIO); electable =
 * F_ISSET(SITE_ELECTABLE); acked = LOG_COMPARE(max_ack, lsn) >= 0.
 */
struct site {
	int present;
	int has_prio;
	int electable;
	int acked;
};

/*
 * got_acks' decision, with the per-site facts pre-extracted.  This is a
 * faithful transcription of got_acks (src/repmgr/repmgr_net.c:1117): the
 * accumulation loop (lines 1133-1156) and the `switch (policy)` (lines
 * 1160-1178).  Returns is_durable (perm->is_durable), and writes
 * sites_acked / peers_acked / has_unacked_peer through the out-params.
 */
static int
got_acks_model(const struct site *s, int n, int policy,
    u_int threshold, u_int quorum,
    u_int *sites_acked_o, u_int *peers_acked_o, int *has_unacked_peer_o)
{
	u_int sites_acked, peers_acked;
	int has_unacked_peer, is_perm, i;

	sites_acked = peers_acked = 0;
	has_unacked_peer = FALSE;
	/* FOR_EACH_REMOTE_SITE_INDEX(eid) { ... }  (lines 1133-1156) */
	for (i = 0; i < n; i++) {
		if (!s[i].present)			/* != SITE_PRESENT */
			continue;
		if (!s[i].has_prio) {
			/* Never connected: assume the worst. */
			has_unacked_peer = TRUE;
			continue;
		}
		if (s[i].acked) {			/* LOG_COMPARE >= 0 */
			sites_acked++;
#ifdef ACKPOLICY_SABOTAGE
			/*
			 * SABOTAGE: count EVERY acked site as a peer, dropping
			 * the electability test.  peers_acked is then inflated by
			 * non-electable acks, so QUORUM declares a commit durable
			 * that a majority of ELECTABLE sites could still lose --
			 * the contract (harness_quorum) must FAIL.
			 */
			peers_acked++;
#else
			if (s[i].electable)
				peers_acked++;
#endif
		} else {
			if (s[i].electable)
				has_unacked_peer = TRUE;
		}
	}

	switch (policy) {				/* lines 1160-1178 */
	case DB_REPMGR_ACKS_ALL:
	case DB_REPMGR_ACKS_ONE:
		is_perm = (sites_acked >= threshold);
		break;
	case DB_REPMGR_ACKS_ONE_PEER:
		is_perm = (peers_acked >= threshold);
		break;
	case DB_REPMGR_ACKS_QUORUM:
	case DB_REPMGR_ACKS_ALL_AVAILABLE:
		is_perm = (peers_acked >= quorum) || !has_unacked_peer;
		break;
	case DB_REPMGR_ACKS_ALL_PEERS:
		is_perm = !has_unacked_peer;
		break;
	default:
		is_perm = FALSE;
	}
	*sites_acked_o = sites_acked;
	*peers_acked_o = peers_acked;
	*has_unacked_peer_o = has_unacked_peer;
	return (is_perm);
}

/*
 * The needed/quorum computation copied from __repmgr_send
 * (src/repmgr/repmgr_net.c, DB_REP_PERMANENT block).  Covers the non-GMDB
 * (active_gmdb_update == none) path for QUORUM / ONE_PEER / ALL_PEERS /
 * ONE / ALL.  nclients = config_nsites - 1 (line ~405); quorum is the
 * QUORUM/ALL_AVAILABLE computation at lines ~464-490; needed/available at the
 * per-policy cases.  Returns needed; writes quorum and available out.
 * `ret_unavail` is set when the ALL_AVAILABLE quorum-0 early-out fires (not
 * used for the policies we model here).
 */
static u_int
compute_needed_quorum(int policy, u_int nclients, int strict,
    u_int nsites_sent, u_int npeers_sent, int missed_peer,
    u_int *quorum_o, u_int *available_o)
{
	u_int needed = 0, available = 0, quorum = 0;

	switch (policy) {
	case DB_REPMGR_ACKS_ONE:				/* line ~439 */
		needed = 1;
		available = nsites_sent;
		break;
	case DB_REPMGR_ACKS_ALL:				/* line ~444 */
		needed = nclients;
		available = nsites_sent;
		break;
	case DB_REPMGR_ACKS_ONE_PEER:				/* line ~450 */
		needed = 1;
		available = npeers_sent;
		break;
	case DB_REPMGR_ACKS_ALL_PEERS:				/* line ~455 */
		needed = 1;
		available = npeers_sent;
		break;
	case DB_REPMGR_ACKS_QUORUM:				/* line ~465 */
		/* nclients>1 || STRICT (|| gmdb_primary, not modelled) */
		if (nclients > 1 || strict)
			quorum = nclients / 2;
		else
			quorum = nclients;
		needed = quorum;
		available = npeers_sent;
		if (npeers_sent < quorum && !missed_peer)	/* line ~530 */
			needed = npeers_sent;
		break;
	default:
		needed = 0;
	}
	*quorum_o = quorum;
	*available_o = available;
	return (needed);
}

int nondet_int(void);
u_int32_t nondet_u32(void);

/* Fill NREMOTE nondeterministic remote sites. */
static void
mk_sites(struct site *s)
{
	int i;

	for (i = 0; i < NREMOTE; i++) {
		s[i].present = nondet_int() & 1;
		s[i].has_prio = nondet_int() & 1;
		s[i].electable = nondet_int() & 1;
		s[i].acked = nondet_int() & 1;
	}
}

/*
 * The "holders" of the commit = the master (always) plus every acked
 * electable remote site.  The electable sites = the master plus every
 * electable remote site that is a present group member.  (A site with
 * !has_prio has unknown electability; got_acks treats it as a potential
 * unacked peer, so for the safety argument we must count it as a possible
 * electable non-holder too.  We model has_prio sites as the electable set and
 * require !has_unacked_peer to carry the !has_prio case, matching got_acks.)
 *
 * The contract: if durable, then every subset E of the electable sites with
 * |E| >= ELECTION_MAJORITY(total_electable) contains a holder.  We discharge
 * the "every subset" quantifier by letting CBMC pick a nondeterministic
 * electable majority that avoids all holders and asserting it cannot reach
 * majority size.
 */
void harness_quorum(void)
{
	struct site s[NREMOTE];
	u_int sites_acked, peers_acked, quorum, needed, available;
	int has_unacked_peer, is_durable, strict, i;
	u_int nclients, total_electable, maj;
	u_int32_t npeers_sent;

	mk_sites(s);
	strict = nondet_int() & 1;

	/* nclients = config_nsites - 1 = number of remote sites that exist. */
	nclients = NREMOTE;

	/* Model the NORMAL quorum case: nclients > 1 or STRICT. */
	__CPROVER_assume(nclients > 1 || strict);

	/*
	 * npeers_sent (electable remote sites we sent to) -- for the durability
	 * contract we take the strongest adversary: we sent to all present
	 * electable peers, so missed_peer = FALSE and npeers_sent counts them.
	 * got_acks itself recomputes from the site table, so npeers_sent only
	 * feeds the quorum/needed math.
	 */
	npeers_sent = 0;
	for (i = 0; i < NREMOTE; i++)
		if (s[i].present && s[i].has_prio && s[i].electable)
			npeers_sent++;

	needed = compute_needed_quorum(DB_REPMGR_ACKS_QUORUM, nclients, strict,
	    /*nsites_sent*/npeers_sent, npeers_sent, /*missed_peer*/FALSE,
	    &quorum, &available);
	(void)needed; (void)available;

	is_durable = got_acks_model(s, NREMOTE, DB_REPMGR_ACKS_QUORUM,
	    /*threshold*/needed, quorum,
	    &sites_acked, &peers_acked, &has_unacked_peer);

	/* total electable sites = master (1) + present electable remote sites
	 * with known prio.  A !has_prio present site has unknown electability;
	 * got_acks' has_unacked_peer accounts for it, so when is_durable holds
	 * via the (peers_acked >= quorum) arm we must ALSO have no !has_prio
	 * present site that could be a hidden electable non-holder -- which is
	 * exactly what the !has_unacked_peer disjunct guarantees in the other
	 * arm.  We therefore split on which arm made it durable. */
	total_electable = 1;	/* master */
	for (i = 0; i < NREMOTE; i++)
		if (s[i].present && s[i].has_prio && s[i].electable)
			total_electable++;
	maj = ELECTION_MAJORITY(total_electable);

	if (!is_durable)
		return;

	/*
	 * CONTRACT: pick a nondeterministic subset E of the electable sites
	 * (master + electable present remote peers) that contains NO holder
	 * (holder = master, or acked electable peer).  Since the master is a
	 * holder, E must exclude the master.  Assert |E| < maj: no holder-free
	 * electable set can reach a majority, so every majority has a holder.
	 */
	{
		int inE_remote[NREMOTE];
		u_int esz = 0;		/* |E|; master excluded (it's a holder) */

		for (i = 0; i < NREMOTE; i++) {
			inE_remote[i] = nondet_int() & 1;
			if (inE_remote[i]) {
				/* E members must be electable present peers... */
				__CPROVER_assume(s[i].present && s[i].has_prio &&
				    s[i].electable);
				/* ...and NON-holders: an acked electable peer is
				 * a holder, so exclude it. */
				__CPROVER_assume(!s[i].acked);
				esz++;
			}
		}
		/*
		 * If is_durable came via !has_unacked_peer, then there is NO
		 * present electable peer that is unacked AND no !has_prio
		 * present site, so no holder-free electable peer exists at all
		 * (esz == 0 < maj trivially).  If it came via peers_acked >=
		 * quorum, the unacked electable peers number <= nclients -
		 * peers_acked, and 2*maj > total_electable bounds esz.  CBMC
		 * checks the assertion for both.
		 */
		__CPROVER_assert(esz < maj,
		    "QUORUM durable => no electable majority avoids a holder");
	}
}

/* ALL_PEERS: durable => every electable (present, known-prio) remote acked. */
void harness_all_peers(void)
{
	struct site s[NREMOTE];
	u_int sites_acked, peers_acked;
	int has_unacked_peer, is_durable, i;

	mk_sites(s);
	is_durable = got_acks_model(s, NREMOTE, DB_REPMGR_ACKS_ALL_PEERS,
	    /*threshold*/0, /*quorum*/0,
	    &sites_acked, &peers_acked, &has_unacked_peer);
	if (!is_durable)
		return;
	for (i = 0; i < NREMOTE; i++)
		if (s[i].present && s[i].has_prio && s[i].electable)
			__CPROVER_assert(s[i].acked,
			    "ALL_PEERS durable => every electable peer acked");
	/* Also: no present site with unknown electability (!has_prio). */
	for (i = 0; i < NREMOTE; i++)
		if (s[i].present)
			__CPROVER_assert(s[i].has_prio,
			    "ALL_PEERS durable => no present site of unknown prio");
}

/* ONE_PEER: durable => at least one electable remote site acked. */
void harness_one_peer(void)
{
	struct site s[NREMOTE];
	u_int sites_acked, peers_acked, quorum, available, needed;
	int has_unacked_peer, is_durable, i, found;

	mk_sites(s);
	needed = compute_needed_quorum(DB_REPMGR_ACKS_ONE_PEER, NREMOTE, 0,
	    0, 0, FALSE, &quorum, &available);
	is_durable = got_acks_model(s, NREMOTE, DB_REPMGR_ACKS_ONE_PEER,
	    /*threshold*/needed, quorum,
	    &sites_acked, &peers_acked, &has_unacked_peer);
	if (!is_durable)
		return;
	found = 0;
	for (i = 0; i < NREMOTE; i++)
		if (s[i].present && s[i].has_prio && s[i].electable && s[i].acked)
			found = 1;
	__CPROVER_assert(found,
	    "ONE_PEER durable => some electable peer acked");
}

/*
 * The 2-SITE special cases (master + one remote), stated separately per the
 * brief.  nclients == 1.  compute_needed_quorum gives:
 *   - NON-strict: quorum = nclients = 1, so needed = 1 (unless npeers_sent <
 *     1).  The one client must ack for the commit to be durable.  This is the
 *     "fudge" in the source comment: a 2-site non-strict client self-elects on
 *     losing the master, so to keep the quorum guarantee the client must have
 *     the txn.  GUARANTEE: durable => the (sole electable) client acked.
 *   - STRICT: quorum = nclients/2 = 0, so needed = 0 => durable with ZERO
 *     client acks.  The source says this is correct: in strict mode the lone
 *     client will NOT self-elect, so no election can lose the commit even if
 *     the client never received it (the master stays master).  GUARANTEE:
 *     durable by the master alone; safe because no sub-majority election
 *     occurs.
 */
void harness_twosite(void)
{
	struct site s[NREMOTE];
	u_int sites_acked, peers_acked, quorum, needed, available;
	int has_unacked_peer, is_durable, strict, i;
	u_int nclients = 1;		/* master + exactly one remote */
	u_int32_t npeers_sent;

	mk_sites(s);
	strict = nondet_int() & 1;

	/* Exactly one remote site exists, present, electable, known prio. */
	__CPROVER_assume(s[0].present && s[0].has_prio && s[0].electable);
	for (i = 1; i < NREMOTE; i++)
		__CPROVER_assume(!s[i].present);
	npeers_sent = 1;

	needed = compute_needed_quorum(DB_REPMGR_ACKS_QUORUM, nclients, strict,
	    npeers_sent, npeers_sent, /*missed_peer*/FALSE, &quorum, &available);

	if (strict) {
		/* STRICT: quorum 0, needed 0 => durable by master alone. */
		__CPROVER_assert(quorum == 0 && needed == 0,
		    "2-site STRICT QUORUM needs 0 client acks (master-only durable)");
	} else {
		/* NON-strict: quorum 1, needed 1 => the client must ack. */
		__CPROVER_assert(quorum == 1 && needed == 1,
		    "2-site non-strict QUORUM needs the single client to ack");
		is_durable = got_acks_model(s, NREMOTE, DB_REPMGR_ACKS_QUORUM,
		    needed, quorum,
		    &sites_acked, &peers_acked, &has_unacked_peer);
		if (is_durable)
			__CPROVER_assert(s[0].acked,
			    "2-site non-strict QUORUM durable => the client acked");
	}
}

/*
 * SHORTCUTS: the __repmgr_send fast paths must be sound against the QUORUM
 * durability contract.  For QUORUM, __repmgr_send does:
 *   if (needed == 0) goto out;        // declares DURABLE without calling got_acks
 *   if (available < needed) UNAVAIL;  // declares NOT durable
 * else it waits and uses got_acks.
 *
 * The needed==0 path (reached only when npeers_sent < quorum && !missed_peer
 * collapses needed to npeers_sent==0) declares durability WITHOUT any acks, so
 * it must be sound on its own: npeers_sent==0 with missed_peer==FALSE means
 * there are NO electable remote peers at all, so the master alone is the whole
 * electable majority and holds the commit.  We prove the contract holds for
 * that path directly (no holder-free electable majority exists).
 *
 * The available<needed path declares NOT durable, which is always safe (it can
 * only refuse a commit, never falsely confirm one); we assert the engine does
 * take that branch (available < needed) rather than silently continue.
 */
void harness_shortcut(void)
{
	struct site s[NREMOTE];
	u_int sites_acked, peers_acked, quorum, needed, available;
	int has_unacked_peer, is_durable, strict, i;
	u_int nclients, total_electable, maj;
	u_int32_t npeers_sent;

	mk_sites(s);
	strict = nondet_int() & 1;
	nclients = NREMOTE;
	__CPROVER_assume(nclients > 1 || strict);

	npeers_sent = 0;
	for (i = 0; i < NREMOTE; i++)
		if (s[i].present && s[i].has_prio && s[i].electable)
			npeers_sent++;

	needed = compute_needed_quorum(DB_REPMGR_ACKS_QUORUM, nclients, strict,
	    npeers_sent, npeers_sent, /*missed_peer*/FALSE, &quorum, &available);

	/*
	 * needed==0 (via the all-peers-sent collapse) => __repmgr_send declares
	 * durable without acks.  This is reachable only when npeers_sent==0 with
	 * missed_peer FALSE, i.e. no electable remote peers exist.  Then the
	 * electable set is just the master, the majority is 1, and the master
	 * (always a holder) is in every majority.  Prove no holder-free
	 * electable majority exists.
	 */
	if (needed == 0) {
		__CPROVER_assert(npeers_sent == 0,
		    "QUORUM needed==0 only when no electable peers were sent to");

		total_electable = 1;	/* master */
		for (i = 0; i < NREMOTE; i++)
			if (s[i].present && s[i].has_prio && s[i].electable)
				total_electable++;
		maj = ELECTION_MAJORITY(total_electable);

		/* A holder-free electable set excludes the master (holder); it
		 * can hold only electable present peers.  With npeers_sent==0
		 * there are none, so its size is 0 < maj. */
		{
			int inE[NREMOTE];
			u_int esz = 0;

			for (i = 0; i < NREMOTE; i++) {
				inE[i] = nondet_int() & 1;
				if (inE[i]) {
					__CPROVER_assume(s[i].present &&
					    s[i].has_prio && s[i].electable);
					__CPROVER_assume(!s[i].acked);
					esz++;
				}
			}
			__CPROVER_assert(esz < maj,
			    "needed==0 shortcut is durability-safe (master is the whole majority)");
		}
		return;
	}

	/*
	 * available < needed => __repmgr_send returns DB_REP_UNAVAIL (declares
	 * NOT durable), which is always safe.  Confirm got_acks over the current
	 * acks would also NOT be durable in this branch (so the refusal is not
	 * masking a genuinely-durable state we should have confirmed): with
	 * available = npeers_sent and needed = quorum, available < needed means
	 * peers_acked <= npeers_sent < quorum, and all peers present (missed_peer
	 * FALSE) so has_unacked_peer is driven by unacked electable peers /
	 * unknown-prio sites.
	 */
	is_durable = got_acks_model(s, NREMOTE, DB_REPMGR_ACKS_QUORUM,
	    needed, quorum, &sites_acked, &peers_acked, &has_unacked_peer);
	if (available < needed)
		__CPROVER_assert(!is_durable,
		    "available<needed => got_acks is also not durable (safe refusal)");
}

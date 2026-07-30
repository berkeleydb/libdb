/*-
 * Deterministic Simulation Testing (DST) for libdb -- v2 multi-process seam.
 *
 * sim_sched.h --
 *	The v2 scaffold: the seeded kill-point / yield-point seam for the
 *	deterministic multi-process layer.  v1 (single-process fault +
 *	crash/recovery) is documented in DESIGN.md; the v2 design + the
 *	failchk-recovery pilot are in DST-V2-DESIGN.md.  Read those first.
 *
 *	This header owns the DB_SIM_RNG_SCHED stream (reserved but never
 *	drawn in v1, so consuming it here does NOT shift any v1 draw
 *	ordering -- a v1 scenario replays byte-identically with or without
 *	v2 present).
 *
 *	Everything is header-only, dependency-free (stdint), and compiles to
 *	nothing when a caller does not include it.  The library itself is
 *	NOT changed by this header; the yield-point macro below is the seam
 *	phase 2 will plant into real lock/latch/commit sites under
 *	#ifdef HAVE_DST, but phase 1 (the failchk pilot) only draws the
 *	kill point -- it does not control interleaving (see DST-V2-DESIGN.md
 *	sec.3 for the honest deterministic/nondeterministic boundary).
 */

#ifndef _DB_SIM_SCHED_H_
#define _DB_SIM_SCHED_H_

#include <stdint.h>

#include "sim_rng.h"

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * __db_sim_sched_killpoint --
 *	Draw the seeded operation boundary at which the victim process
 *	should stop and let itself be killed, in [0, nsteps).  Drawn from
 *	the reserved DB_SIM_RNG_SCHED stream so it never perturbs v1 seeds.
 *	Returns 0 when sim is inactive (caller must have activated the seed).
 */
static __inline uint64_t
__db_sim_sched_killpoint(nsteps)
	uint64_t nsteps;
{
	if (nsteps == 0)
		return (0);
	return (__db_sim_rng_range(DB_SIM_RNG_SCHED, nsteps));
}

/*
 * __db_sim_sched_yield (PHASE 2 STUB -- documented, intentionally inert) --
 *	The yield-point seam the deterministic scheduler (approach (a) in
 *	DST-V2-DESIGN.md) will plant at BDB's real lock/latch/commit sites
 *	under #ifdef HAVE_DST.  A planted call would block the calling
 *	process on its per-process coordination primitive until the seeded
 *	harness ready-queue releases it, making cross-process interleaving
 *	deterministic at yield-point granularity.
 *
 *	Phase 1 (the failchk pilot) does NOT plant these -- it seeds only
 *	the kill point, not the interleaving.  This stub exists so the
 *	call-site shape is fixed now (a stable seam, like SCHED being
 *	reserved) and phase 2 is a mechanical planting, not a redesign.
 *	It draws one SCHED value per site so the scheduler's decision
 *	stream is already carved out; with no scheduler wired it just
 *	advances the (otherwise unused) SCHED stream and returns.
 */
static __inline void
__db_sim_sched_yield(site)
	int site;
{
	(void)site;
	if (__db_sim_active())
		(void)__db_sim_rng(DB_SIM_RNG_SCHED);
}

#if defined(__cplusplus)
}
#endif

#endif /* !_DB_SIM_SCHED_H_ */

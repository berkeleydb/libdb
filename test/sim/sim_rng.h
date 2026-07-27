/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_rng.h --
 *	Seeded per-stream PRNG tree + activation state.  Adapted from the
 *	xtc DST core (src/evt/sim.c), pared to what a single-process libdb
 *	fault+crash harness needs.  Dependency-free (stdint + stdatomic).
 *
 *	The PRNG is a per-stream splitmix64: one root seed splits into
 *	independent sub-streams so a draw at one decision site (IO, fault,
 *	buggify, app workload) never perturbs another site's sequence --
 *	the FoundationDB discipline that keeps a replay stable when a new
 *	draw site is added.
 *
 *	Everything here compiles to a couple of relaxed atomic loads when
 *	sim is inactive, and the whole translation unit is only linked
 *	into the library when configured --enable-dst (HAVE_DST).  A
 *	production build never sees it.
 */

#ifndef _DB_SIM_RNG_H_
#define _DB_SIM_RNG_H_

#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * Well-known PRNG streams.  Each independent decision site owns a
 * stream so adding a draw in one site cannot shift another's sequence.
 * Keep SCHED reserved for the future deterministic scheduler (v2) so
 * v1 draw ordering does not move when it lands.
 */
enum db_sim_stream {
	DB_SIM_RNG_IO      = 0,   /* simulated I/O latency / fault toggles */
	DB_SIM_RNG_FAULT   = 1,   /* generic fault-injection toggles */
	DB_SIM_RNG_BUGGIFY = 2,   /* buggify per-run activation coins */
	DB_SIM_RNG_APP     = 3,   /* application/test workload draws */
	DB_SIM_RNG_SCHED   = 4,   /* RESERVED: deterministic scheduler (v2) */
	DB_SIM_RNG_NSTREAMS
};

/* 1 while a deterministic simulation run is active.  Hot-path call
 * sites branch on this to decide between seeded and normal behaviour. */
int      __db_sim_active __P((void));

/* Activate sim with a root seed (resets every stream + the guard
 * count).  Idempotent re-activation re-seeds.  Test-only. */
void     __db_sim_activate __P((uint64_t));
void     __db_sim_deactivate __P((void));
uint64_t __db_sim_seed __P((void));

/* Draw the next 64-bit value from stream `s`, or a uniform value in
 * [0, bound).  Deterministic given the activation seed.  Return 0 when
 * sim is inactive (callers gate on __db_sim_active()). */
uint64_t __db_sim_rng __P((int));
uint64_t __db_sim_rng_range __P((int, uint64_t));

/*
 * Determinism guard.  A sim-reachable primitive that would break seed
 * replay (a real clock read, an unseeded rand(), a raw pid) calls
 * __db_sim_nondeterminism(what).  Outside a sim run it is a no-op; under
 * sim it records the violation and, in strict mode (default), aborts
 * naming the source.  The count is queryable so a harness can assert 0
 * to PROVE a run was fully deterministic.
 */
void     __db_sim_nondeterminism __P((const char *));
void     __db_sim_strict __P((int));
int      __db_sim_nondeterminism_count __P((void));

#if defined(__cplusplus)
}
#endif

#endif /* !_DB_SIM_RNG_H_ */

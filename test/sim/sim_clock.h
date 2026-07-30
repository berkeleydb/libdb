/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_clock.h --
 *	Clock-skew / time-jump fault class.  Under sim, when armed, the
 *	__os_gettime seam applies a seeded per-read skew to the clock BDB
 *	reads -- a fixed per-run offset, optional per-read jitter, and an
 *	occasional discrete JUMP (forward and, the dangerous one, BACKWARD).
 *	This exercises every decision that reads the clock: lock/txn timeout
 *	deadlines (deadline = now + timeout; later now2 >= deadline), the
 *	deadlock detector's expiry scan, checkpoint scheduling and the
 *	replication lease/election timers.
 *
 *	FoundationDB-style: a process's clock can read ahead/behind "true"
 *	time and can jump.  The correctness concern is code that assumes
 *	monotonic time -- a backward jump can make a timeout never fire
 *	(hang) or fire instantly (premature abort).
 *
 *	This header is OWNED by the clock-skew work; keep the shared headers
 *	(sim_fault.h / sim_rng.h) untouched.  The skew draws on its own
 *	dedicated PRNG stream so arming it never perturbs the IO/FAULT/APP
 *	streams (same discipline as every other fault knob).
 *
 *	Compilation model: like the rest of DST, this whole surface is only
 *	reachable under --enable-dst (HAVE_DST).  The __os_gettime hook is
 *	#ifdef HAVE_DST and is a single relaxed load (__db_sim_active) in the
 *	common no-sim case, so an --enable-dst library that is NOT running a
 *	sim reads the real clock at ~zero cost, and a production (DST-off)
 *	build never sees any of this.
 */

#ifndef _DB_SIM_CLOCK_H_
#define _DB_SIM_CLOCK_H_

#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * Arm the clock-skew fault.  Drawn from the dedicated CLOCK stream so
 * enabling it never shifts another site's draws.
 *
 *   offset_ns  a FIXED per-run offset applied to EVERY read (models a
 *              clock that reads steadily ahead/behind true time); may be
 *              negative.  If 0, a seeded offset in [-1s, +1s] is drawn.
 *   jitter_ns  bound on a per-read uniform jitter in [-jitter, +jitter]
 *              (models a jumpy, imprecise clock); 0 => no jitter.
 *   jump_ns    magnitude of an occasional discrete jump; 0 => no jumps.
 *   jump_pct   per-1000 probability that a given read takes a jump.  A
 *              jump is forward or BACKWARD (seeded coin) by up to jump_ns.
 *
 * All bounded + seeded: same seed => same skew sequence => replayable.
 */
void __db_sim_clock_enable
    __P((int64_t offset_ns, int64_t jitter_ns, int64_t jump_ns, unsigned jump_pct));
void __db_sim_clock_disable __P((void));
int  __db_sim_clock_armed __P((void));

/*
 * Bound the disturbance to the first `n` clock reads, after which the skew
 * self-disables (the clock "settles" back to true time).  This models a
 * TRANSIENT jump -- a real clock-skew event has a finite duration; a
 * correct deadline-based timeout must recover and fire once the clock
 * resumes advancing.  0 (the default) => the skew never settles (a
 * persistent-adversary clock).  Deterministic: `n` reads is `n` reads.
 */
void __db_sim_clock_settle_after __P((unsigned n));

/*
 * Apply the armed skew to a just-read clock value.  `sec`/`nsec` are the
 * real reading; on return they hold the skewed reading.  A no-op (leaves
 * the value untouched) unless a sim armed the knob.  Counts a skew
 * "firing" via the fault-activation counter so a swarm can prove coverage.
 *
 * `monotonic` is passed through for diagnostics/future use; the skew is
 * applied to both clock domains because a skewed process clock skews every
 * reading (that is exactly the FDB model, and it is what makes a "monotonic"
 * read that in libdb actually falls through to CLOCK_REALTIME dangerous).
 */
void __db_sim_clock_skew __P((time_t *sec, long *nsec, int monotonic));

/* How many times the skew has fired this run (diagnostic / coverage). */
unsigned long __db_sim_clock_fire_count __P((void));

#if defined(__cplusplus)
}
#endif

#endif /* !_DB_SIM_CLOCK_H_ */

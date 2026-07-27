/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_fault.h --
 *	Fault-injection surface: seeded fault toggles, buggify, the
 *	simulated-I/O fault knobs and the write-back-cache crash model,
 *	plus the ZERO-COST macros the __os_* hooks use.
 *
 *	Adapted from xtc_sim.h / xtc_dst_inject.h.
 *
 *	Compilation model:
 *	  - HAVE_DST OFF (production / default): the DST_* macros below are
 *	    defined to constants / no-ops in db_int.h's stub (see the
 *	    !HAVE_DST branch), so the __os_* hooks vanish and none of these
 *	    symbols are referenced.  This whole header is only included by
 *	    sim TUs and by the __os_* hooks under #ifdef HAVE_DST.
 *	  - HAVE_DST ON: the sim core TU (sim_core.c) is linked into the
 *	    library and the hooks call these functions, each gated first on
 *	    __db_sim_active() (a single relaxed load) so an --enable-dst
 *	    library that is NOT running a sim behaves exactly like stock.
 */

#ifndef _DB_SIM_FAULT_H_
#define _DB_SIM_FAULT_H_

#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

/* Seeded fault toggle: 1 with probability pct_per_1000/1000 from the
 * FAULT stream; 0 when sim inactive.  Same seed => same fault schedule. */
int      __db_sim_fault __P((unsigned));

/* Buggify: once-per-run cached coin per named site (BUGGIFY stream). */
void     __db_sim_buggify_enable __P((unsigned));
void     __db_sim_buggify_disable __P((void));
int      __db_sim_buggify __P((const char *));

/* Simulated I/O faults: seeded latency + short-transfer/EIO toggle. */
void     __db_sim_io_faults_enable __P((int64_t, int64_t, unsigned));
void     __db_sim_io_faults_disable __P((void));
int      __db_sim_io_faults_active __P((void));
int64_t  __db_sim_io_latency __P((void));
int      __db_sim_io_should_fault __P((void));

/* Disk-full (ENOSPC): a seeded coin fails a whole write, nothing
 * persists. */
void     __db_sim_io_enospc_enable __P((unsigned));
int      __db_sim_io_enospc __P((void));

/* Torn write (persist a seeded prefix, report full) + corrupt read
 * (bit-flip one returned byte).  Both leave latent bad data a checksum
 * must catch. */
void     __db_sim_io_corrupt_enable __P((unsigned));
void     __db_sim_io_corrupt_disable __P((void));
int      __db_sim_io_torn_prefix __P((int));
int      __db_sim_io_flip_byte __P((int));

/*
 * Write-back cache crash model (the ack-before-fsync durability
 * catcher).  Keyed by a caller-chosen 64-bit file key (libdb hashes the
 * DB_FH name so the frontier survives close/reopen).  A write records
 * the written high-water; fsync promotes written -> durable; a crash
 * loses everything past the last fsync.  A recovery test truncates the
 * file to __db_sim_io_durable_end(key).
 */
void     __db_sim_wb_enable __P((int));
int      __db_sim_wb_active __P((void));
void     __db_sim_wb_wrote __P((uint64_t, uint64_t));
void     __db_sim_wb_synced __P((uint64_t));
uint64_t __db_sim_wb_written_end __P((uint64_t));
uint64_t __db_sim_io_durable_end __P((uint64_t));

/*
 * Buggify branch macro for planting a legal-but-pessimal path in real
 * library code:  if (DB_SIM_BUGGIFY("log.flush.tiny")) { ...slow... }
 * A single relaxed load + short-scan in an --enable-dst build; compiles
 * to constant 0 when HAVE_DST is off (see db_int.h stub).
 */
#define DB_SIM_BUGGIFY(name)  __db_sim_buggify(name)

#if defined(__cplusplus)
}
#endif

#endif /* !_DB_SIM_FAULT_H_ */

/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_buggify.h --
 *	FoundationDB-style BUGGIFY: plant named "buggify points" at real
 *	library sites that, under sim, MAY take a LEGAL-BUT-PESSIMAL path so
 *	the engine's rare/slow paths (page splits, aggressive eviction, log
 *	flushes, forced checkpoints, deadlock-detector runs) run CONSTANTLY
 *	under test instead of once in a blue moon.
 *
 *	The key property, copied verbatim from FoundationDB: a buggify point
 *	NEVER changes correctness.  Every choice it forces is one the engine
 *	is already free to make -- split a page a little early, evict a warm
 *	buffer, fsync now instead of later, checkpoint before the byte
 *	threshold, run the deadlock detector eagerly.  It only changes
 *	TIMING / SIZING / PATH, never a result.  If turning a buggify point
 *	ON makes an invariant fail, either the "pessimal" choice was not
 *	actually legal (fix the point) or the engine mishandles a legal-but-
 *	rare path (a REAL bug -- record the seed, do not paper over it).
 *
 *	Mechanism.  DB_BUGGIFY("name") is a per-run cached coin drawn from
 *	the dedicated DB_SIM_RNG_BUGGIFY stream (see sim_rng.h): the FIRST
 *	time a named point is reached in a run it flips the coin (1 with the
 *	enabled probability) and caches the result, so every later reach of
 *	the same name in that run agrees and the whole run replays from its
 *	seed.  Drawn from its own stream so enabling buggify never perturbs
 *	the IO / FAULT / APP schedules.
 *
 *	Coverage.  The core counts, per named point, how many times it was
 *	REACHED and whether its coin ACTIVATED this run.  A swarm folds these
 *	across a seed sweep and reports per-point activation %.  A point
 *	reached on many seeds but NEVER activated is a coverage gap: its
 *	pessimal path never actually ran, so it tested nothing.
 *
 *	Compilation model (ZERO production overhead).  Every planted site in
 *	src/ is wrapped in `#ifdef HAVE_DST`, and this header is included by
 *	those TUs ONLY under that guard.  When HAVE_DST is off (production /
 *	default) DB_BUGGIFY is never referenced -- no symbol, no branch, no
 *	cost.  When HAVE_DST is on but no sim run is active, DB_BUGGIFY is a
 *	couple of relaxed atomic loads that return 0.  Verify with:
 *	    nm libdb-*.so | grep -c '__db_sim_buggify'   # 0 in an OFF build
 *
 *	The primitive (the coin + the per-name cache + the coverage counters)
 *	lives in sim_core.c's buggify section, declared in sim_fault.h; this
 *	header adds the DB_BUGGIFY macro, the point-name catalog, and the
 *	coverage-query API a swarm/test uses.
 */

#ifndef _DB_SIM_BUGGIFY_H_
#define _DB_SIM_BUGGIFY_H_

#include "sim_fault.h"		/* __db_sim_buggify + enable/disable */

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * DB_BUGGIFY(name) --
 *	1 iff the buggify point `name` is activated for this run (take the
 *	pessimal path); 0 otherwise.  Under HAVE_DST only; a planted src
 *	site is itself `#ifdef HAVE_DST`, so an OFF build never sees this.
 *
 *	    #ifdef HAVE_DST
 *	    if (DB_BUGGIFY(BUGGIFY_LOG_FLUSH_NOW))
 *	            LF_SET(DB_FLUSH);    // legal: an extra flush is always OK
 *	    #endif
 */
#define DB_BUGGIFY(name)	__db_sim_buggify(name)

/*
 * The planted-point catalog.  Every DB_BUGGIFY name used in src/ is named
 * here so the set of points is discoverable in one place and a test can
 * assert a specific point activated.  Keep in sync with the DESIGN.md
 * "buggify point catalog" table.  Format: <subsystem>.<what>.
 */
#define BUGGIFY_BT_SPLIT_EARLY		"bt.split_early"    /* bt_put.c */
#define BUGGIFY_HASH_EXPAND_EARLY	"hash.expand_early" /* hash_page.c */
#define BUGGIFY_MP_ALLOC_AGGRESSIVE	"mp.alloc_aggressive" /* mp_alloc.c */
#define BUGGIFY_MP_EVICT_COLD		"mp.evict_cold"     /* mp_fput.c */
#define BUGGIFY_LOG_FLUSH_NOW		"log.flush_now"     /* log_put.c */
#define BUGGIFY_LOG_NEWFILE_EARLY	"log.newfile_early" /* log_put.c */
#define BUGGIFY_TXN_CHKPT_FORCE		"txn.chkpt_force"   /* txn_chkpt.c */
#define BUGGIFY_LOCK_DD_NOW		"lock.dd_now"       /* lock.c (post-op) */
#define BUGGIFY_LOCK_DD_WAIT_NOW	"lock.dd_wait_now"  /* lock.c (pre-wait) */

/*
 * The catalog as a NULL-terminated array so a swarm/test can iterate the
 * points it EXPECTS and flag any it planted but never reached.  Static so
 * each TU that needs it gets its own copy (bounded, tiny).
 */
static const char *const db_buggify_catalog[] = {
	BUGGIFY_BT_SPLIT_EARLY,
	BUGGIFY_HASH_EXPAND_EARLY,
	BUGGIFY_MP_ALLOC_AGGRESSIVE,
	BUGGIFY_MP_EVICT_COLD,
	BUGGIFY_LOG_FLUSH_NOW,
	BUGGIFY_LOG_NEWFILE_EARLY,
	BUGGIFY_TXN_CHKPT_FORCE,
	BUGGIFY_LOCK_DD_NOW,
	BUGGIFY_LOCK_DD_WAIT_NOW,
	NULL
};

/*
 * Buggify coverage-query API (implemented in sim_core.c).  A point enters
 * the table the first time it is reached this run.
 */
int          __db_sim_buggify_npoints __P((void));
const char  *__db_sim_buggify_point_name __P((int));
unsigned long __db_sim_buggify_point_reached __P((int));
int          __db_sim_buggify_point_activated __P((int));
/* Cached decision for a name without reaching it; -1 if not yet reached. */
int          __db_sim_buggify_decided __P((const char *));

#if defined(__cplusplus)
}
#endif

#endif /* !_DB_SIM_BUGGIFY_H_ */

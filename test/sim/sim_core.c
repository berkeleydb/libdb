/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_core.c --
 *	The DST core: seeded per-stream PRNG tree, determinism guard,
 *	seeded fault toggles, buggify, and the simulated-I/O fault knobs
 *	(latency / EIO / short transfer / torn write / corrupt read /
 *	ENOSPC + the write-back-cache durable-frontier crash model).
 *
 *	Adapted from the xtc DST core (src/evt/sim.c, src/io/io_sim.c),
 *	reduced to the single-process libdb fault+crash axis and rewritten
 *	to libdb conventions.  All state is process-global; hot-path
 *	accessors are relaxed atomic loads so a NON-active sim run (and,
 *	when HAVE_DST is off, the whole absence of this TU) costs nothing.
 *
 *	This whole file is compiled into the library ONLY when configured
 *	--enable-dst (guarded by HAVE_DST in Makefile).  When off, none of
 *	these symbols exist and the __os_* hooks (guarded by the same
 *	macro) compile to the stock code path.
 */

#include "db_config.h"

#include "db_int.h"
#include "sim_rng.h"
#include "sim_fault.h"
#include "sim_clock.h"
#include "sim_buggify.h"		/* buggify coverage-query API (owned there) */

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

/* ---- activation + PRNG tree ---- */

static _Atomic int g_sim_active;
static uint64_t    g_sim_seed;
static uint64_t    g_sim_stream[DB_SIM_RNG_NSTREAMS];

/* splitmix64: the standard finalizer used to derive independent streams
 * from a seed.  0x9E3779B97F4A7C15 is the golden-ratio increment. */
static uint64_t
splitmix64(sp)
	uint64_t *sp;
{
	uint64_t z = (*sp += 0x9E3779B97F4A7C15ull);
	z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
	z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
	return (z ^ (z >> 31));
}

int
__db_sim_active()
{
	return (atomic_load_explicit(&g_sim_active, memory_order_relaxed));
}

uint64_t
__db_sim_seed()
{
	return (g_sim_seed);
}

/* ---- determinism guard ---- */

static _Atomic int g_nondet_count;
static _Atomic int g_nondet_strict = 1;   /* abort on violation by default */

void
__db_sim_nondeterminism(what)
	const char *what;
{
	if (!__db_sim_active())
		return;
	atomic_fetch_add_explicit(&g_nondet_count, 1, memory_order_relaxed);
	if (atomic_load_explicit(&g_nondet_strict, memory_order_relaxed)) {
		fprintf(stderr, "libdb DST determinism violation: %s called on "
		    "a sim-reachable path (breaks seed replay)\n",
		    what != NULL ? what : "(unknown)");
		abort();
	}
}

void
__db_sim_strict(on)
	int on;
{
	atomic_store_explicit(&g_nondet_strict, on ? 1 : 0,
	    memory_order_relaxed);
}

int
__db_sim_nondeterminism_count()
{
	return (atomic_load_explicit(&g_nondet_count, memory_order_relaxed));
}

/* ---- io fault knobs (defined early: deactivate resets them) ---- */

static _Atomic int     g_io_faults_on;
static _Atomic int64_t g_io_lat_min_ns;
static _Atomic int64_t g_io_lat_max_ns;
static _Atomic int     g_io_fault_pct;    /* per-1000 short/EIO probability */
static _Atomic int     g_io_enospc_pct;   /* per-1000 write ENOSPC */
static _Atomic int     g_io_corrupt_pct;  /* per-1000 torn-write/corrupt-read */
static _Atomic int     g_io_corrupt_on;

/* ---- fault-activation coverage counters (FoundationDB-style) ----
 * Each class counts how many times the fault actually FIRED this run, so
 * a swarm can report per-fault activation.  Relaxed atomics: a count is a
 * diagnostic, not a control path. */
static _Atomic unsigned long g_fc[DB_SIM_FC_NCLASSES];

static void
fc_hit(cls)
	int cls;
{
	if (cls >= 0 && cls < DB_SIM_FC_NCLASSES)
		atomic_fetch_add_explicit(&g_fc[cls], 1, memory_order_relaxed);
}

unsigned long
__db_sim_fault_count(cls)
	int cls;
{
	if (cls < 0 || cls >= DB_SIM_FC_NCLASSES)
		return (0);
	return (atomic_load_explicit(&g_fc[cls], memory_order_relaxed));
}

void
__db_sim_fault_count_reset()
{
	int i;
	for (i = 0; i < DB_SIM_FC_NCLASSES; i++)
		atomic_store_explicit(&g_fc[i], 0, memory_order_relaxed);
}

const char *
__db_sim_fault_class_name(cls)
	int cls;
{
	switch (cls) {
	case DB_SIM_FC_TORN:     return ("torn");
	case DB_SIM_FC_ENOSPC:   return ("enospc");
	case DB_SIM_FC_CORRUPT:  return ("corrupt");
	case DB_SIM_FC_STALE:    return ("stale");
	case DB_SIM_FC_LATENCY:  return ("latency");
	case DB_SIM_FC_SHORTEIO: return ("shorteio");
	case DB_SIM_FC_CLOCK:    return ("clock");
	default:                 return ("?");
	}
}

void
__db_sim_activate(seed)
	uint64_t seed;
{
	int i;
	uint64_t s = seed != 0 ? seed : 0x9E3779B97F4A7C15ull;

	g_sim_seed = s;
	/* Derive each stream's state from the root seed so the streams are
	 * independent yet fully determined by the seed. */
	for (i = 0; i < DB_SIM_RNG_NSTREAMS; i++) {
		uint64_t t = s + (uint64_t)(i + 1) * 0x9E3779B97F4A7C15ull;
		g_sim_stream[i] = splitmix64(&t);
	}
	atomic_store_explicit(&g_nondet_count, 0, memory_order_relaxed);
	__db_sim_fault_count_reset();
	atomic_store_explicit(&g_sim_active, 1, memory_order_release);
}

void
__db_sim_deactivate()
{
	atomic_store_explicit(&g_sim_active, 0, memory_order_release);
	/* Reset io fault knobs so one run's config never leaks into the
	 * next in-process run. */
	atomic_store_explicit(&g_io_faults_on, 0, memory_order_release);
	atomic_store_explicit(&g_io_fault_pct, 0, memory_order_release);
	atomic_store_explicit(&g_io_enospc_pct, 0, memory_order_release);
	atomic_store_explicit(&g_io_corrupt_pct, 0, memory_order_release);
	atomic_store_explicit(&g_io_corrupt_on, 0, memory_order_release);
	__db_sim_io_stale_enable(0);
	__db_sim_wb_enable(0);
	__db_sim_clock_disable();
	__db_sim_reccrash_enable(0);
}

uint64_t
__db_sim_rng(s)
	int s;
{
	if (!__db_sim_active())
		return (0);
	if (s < 0 || s >= DB_SIM_RNG_NSTREAMS)
		s = DB_SIM_RNG_APP;
	return (splitmix64(&g_sim_stream[s]));
}

uint64_t
__db_sim_rng_range(s, bound)
	int s;
	uint64_t bound;
{
	if (bound == 0)
		return (0);
	/* Modulo bias over a 64-bit draw with a small bound is negligible
	 * and -- crucially -- deterministic. */
	return (__db_sim_rng(s) % bound);
}

/* ---- seeded fault toggle ---- */

int
__db_sim_fault(pct_per_1000)
	unsigned pct_per_1000;
{
	if (!__db_sim_active())
		return (0);
	if (pct_per_1000 == 0)
		return (0);
	if (pct_per_1000 >= 1000)
		return (1);
	return (__db_sim_rng_range(DB_SIM_RNG_FAULT, 1000) < pct_per_1000);
}

/* ---- buggify (per-run cached coin + coverage counters) ----
 *
 * A buggify point (see sim_buggify.h, DB_BUGGIFY) is a named site in real
 * library code that, under sim, MAY take a legal-but-pessimal path -- the
 * coin is flipped ONCE per run per name and cached, so every reach of a
 * name agrees and the whole run replays (the FoundationDB discipline).
 *
 * Coverage: per name we count how many times the point was REACHED and,
 * separately, whether its coin ACTIVATED (came up 1) this run.  A swarm
 * folds these across a seed sweep: a point reached-but-never-activated
 * across many seeds is a coverage gap (its pessimal path never ran).
 */
#define DB_SIM_BUG_MAX 64
static char          g_bug_name[DB_SIM_BUG_MAX][48];
static signed char   g_bug_decided[DB_SIM_BUG_MAX];   /* 0/1 */
static unsigned long g_bug_reached[DB_SIM_BUG_MAX];   /* reaches this run */
static _Atomic int   g_bug_n;
static _Atomic int   g_bug_on;
static _Atomic int   g_bug_pct;

void
__db_sim_buggify_enable(pct_per_1000)
	unsigned pct_per_1000;
{
	int i;
	for (i = 0; i < DB_SIM_BUG_MAX; i++) {
		g_bug_name[i][0] = '\0';
		g_bug_decided[i] = 0;
		g_bug_reached[i] = 0;
	}
	atomic_store_explicit(&g_bug_n, 0, memory_order_relaxed);
	atomic_store_explicit(&g_bug_pct, (int)pct_per_1000,
	    memory_order_relaxed);
	atomic_store_explicit(&g_bug_on, 1, memory_order_release);
}

void
__db_sim_buggify_disable()
{
	atomic_store_explicit(&g_bug_on, 0, memory_order_release);
	atomic_store_explicit(&g_bug_n, 0, memory_order_relaxed);
}

/*
 * Once-per-run decision for buggify point `name`: 1 to take the pessimal
 * path, 0 otherwise.  Decided on first reach (seeded coin from the
 * BUGGIFY stream) and cached, so every reach of the same name in a run
 * agrees and the whole run replays.  Each reach bumps the name's reach
 * count for coverage reporting.
 *
 * ponytail: single global array + linear scan (bounded 64 names, called
 * from tests not a hot library path), a hash map if the site count grows.
 */
int
__db_sim_buggify(name)
	const char *name;
{
	int i, n, pct, decision;

	if (name == NULL)
		return (0);
	if (!atomic_load_explicit(&g_bug_on, memory_order_acquire))
		return (0);
	if (!__db_sim_active())
		return (0);

	n = atomic_load_explicit(&g_bug_n, memory_order_relaxed);
	for (i = 0; i < n; i++)
		if (strncmp(g_bug_name[i], name, sizeof(g_bug_name[0])) == 0) {
			g_bug_reached[i]++;
			return (g_bug_decided[i]);
		}

	if (n >= DB_SIM_BUG_MAX)
		return (0);                 /* table full: no buggify */
	pct = atomic_load_explicit(&g_bug_pct, memory_order_relaxed);
	if (pct == 0)
		decision = 0;
	else if (pct >= 1000)
		decision = 1;
	else
		decision =
		    (int)__db_sim_rng_range(DB_SIM_RNG_BUGGIFY, 1000) < pct;
	(void)strncpy(g_bug_name[n], name, sizeof(g_bug_name[0]) - 1);
	g_bug_name[n][sizeof(g_bug_name[0]) - 1] = '\0';
	g_bug_decided[n] = (signed char)decision;
	g_bug_reached[n] = 1;
	atomic_store_explicit(&g_bug_n, n + 1, memory_order_relaxed);
	return (decision);
}

/*
 * Buggify coverage query (for the swarm / coverage report).  A name is
 * only in the table once it has been reached at least once this run.
 */
int
__db_sim_buggify_npoints()
{
	return (atomic_load_explicit(&g_bug_n, memory_order_relaxed));
}

const char *
__db_sim_buggify_point_name(idx)
	int idx;
{
	if (idx < 0 || idx >= atomic_load_explicit(&g_bug_n, memory_order_relaxed))
		return (NULL);
	return (g_bug_name[idx]);
}

unsigned long
__db_sim_buggify_point_reached(idx)
	int idx;
{
	if (idx < 0 || idx >= atomic_load_explicit(&g_bug_n, memory_order_relaxed))
		return (0);
	return (g_bug_reached[idx]);
}

int
__db_sim_buggify_point_activated(idx)
	int idx;
{
	if (idx < 0 || idx >= atomic_load_explicit(&g_bug_n, memory_order_relaxed))
		return (0);
	return (g_bug_decided[idx]);
}

/*
 * Query the cached decision for a name WITHOUT reaching it (does not bump
 * the reach count and does not decide an unseen point).  -1 if the name
 * has not been reached yet this run.  Lets a test assert a specific point
 * activated on a given seed.
 */
int
__db_sim_buggify_decided(name)
	const char *name;
{
	int i, n;

	if (name == NULL)
		return (-1);
	n = atomic_load_explicit(&g_bug_n, memory_order_relaxed);
	for (i = 0; i < n; i++)
		if (strncmp(g_bug_name[i], name, sizeof(g_bug_name[0])) == 0)
			return (g_bug_decided[i]);
	return (-1);
}

/* ---- simulated I/O faults ---- */

void
__db_sim_io_faults_enable(lat_min_ns, lat_max_ns, fault_pct_per_1000)
	int64_t lat_min_ns, lat_max_ns;
	unsigned fault_pct_per_1000;
{
	if (lat_min_ns < 0)
		lat_min_ns = 0;
	if (lat_max_ns < lat_min_ns)
		lat_max_ns = lat_min_ns;
	atomic_store_explicit(&g_io_lat_min_ns, lat_min_ns,
	    memory_order_relaxed);
	atomic_store_explicit(&g_io_lat_max_ns, lat_max_ns,
	    memory_order_relaxed);
	atomic_store_explicit(&g_io_fault_pct, (int)fault_pct_per_1000,
	    memory_order_relaxed);
	atomic_store_explicit(&g_io_faults_on, 1, memory_order_release);
}

void
__db_sim_io_faults_disable()
{
	atomic_store_explicit(&g_io_faults_on, 0, memory_order_release);
}

int
__db_sim_io_faults_active()
{
	return (atomic_load_explicit(&g_io_faults_on, memory_order_acquire));
}

int64_t
__db_sim_io_latency()
{
	int64_t lo, hi;

	if (!__db_sim_io_faults_active() || !__db_sim_active())
		return (0);
	lo = atomic_load_explicit(&g_io_lat_min_ns, memory_order_relaxed);
	hi = atomic_load_explicit(&g_io_lat_max_ns, memory_order_relaxed);
	if (hi <= lo)
		return (lo);
	return (lo + (int64_t)__db_sim_rng_range(DB_SIM_RNG_IO,
	    (uint64_t)(hi - lo + 1)));
}

int
__db_sim_io_should_fault()
{
	int pct;

	if (!__db_sim_io_faults_active() || !__db_sim_active())
		return (0);
	pct = atomic_load_explicit(&g_io_fault_pct, memory_order_relaxed);
	if (pct == 0)
		return (0);
	if (pct >= 1000) {
		fc_hit(DB_SIM_FC_SHORTEIO);
		return (1);
	}
	if ((int)__db_sim_rng_range(DB_SIM_RNG_IO, 1000) < pct) {
		fc_hit(DB_SIM_FC_SHORTEIO);
		return (1);
	}
	return (0);
}

void
__db_sim_io_enospc_enable(pct_per_1000)
	unsigned pct_per_1000;
{
	if (pct_per_1000 > 1000)
		pct_per_1000 = 1000;
	atomic_store_explicit(&g_io_enospc_pct, (int)pct_per_1000,
	    memory_order_release);
}

int
__db_sim_io_enospc()
{
	int pct = atomic_load_explicit(&g_io_enospc_pct, memory_order_acquire);

	if (pct <= 0 || !__db_sim_active())
		return (0);
	if ((int)__db_sim_rng_range(DB_SIM_RNG_IO, 1000) < pct) {
		fc_hit(DB_SIM_FC_ENOSPC);
		return (1);
	}
	return (0);
}

/* ---- torn write / corrupt read ---- */

void
__db_sim_io_corrupt_enable(corrupt_pct_per_1000)
	unsigned corrupt_pct_per_1000;
{
	atomic_store_explicit(&g_io_corrupt_pct, (int)corrupt_pct_per_1000,
	    memory_order_relaxed);
	atomic_store_explicit(&g_io_corrupt_on, 1, memory_order_release);
}

void
__db_sim_io_corrupt_disable()
{
	atomic_store_explicit(&g_io_corrupt_on, 0, memory_order_release);
}

static int
io_should_corrupt()
{
	int pct;

	if (!atomic_load_explicit(&g_io_corrupt_on, memory_order_acquire) ||
	    !__db_sim_active())
		return (0);
	pct = atomic_load_explicit(&g_io_corrupt_pct, memory_order_relaxed);
	if (pct == 0)
		return (0);
	if (pct >= 1000)
		return (1);
	return ((int)__db_sim_rng_range(DB_SIM_RNG_IO, 1000) < pct);
}

int
__db_sim_io_torn_prefix(full_len)
	int full_len;
{
	if (full_len < 2 || !io_should_corrupt())
		return (full_len);
	fc_hit(DB_SIM_FC_TORN);
	/* Persist a strict prefix in [1, full_len-1] -- a torn write always
	 * loses at least the last byte -- but report full success. */
	return (1 + (int)__db_sim_rng_range(DB_SIM_RNG_IO,
	    (uint64_t)(full_len - 1)));
}

int
__db_sim_io_flip_byte(len)
	int len;
{
	if (len <= 0 || !io_should_corrupt())
		return (-1);
	fc_hit(DB_SIM_FC_CORRUPT);
	return ((int)__db_sim_rng_range(DB_SIM_RNG_IO, (uint64_t)len));
}

/* ---- write-back cache crash model (THE durability catcher) ----
 *
 * The problem it closes: the sim writes to a REAL file, so bytes reach
 * the file on write() regardless of whether fsync was called -- meaning
 * a crash-recovery test that trusts the writer cannot catch a writer
 * that ACKs a commit WITHOUT fsyncing it.  This models the disk
 * honestly: a write lands in a volatile per-file cache (tracked as the
 * WRITTEN high-water offset); only fsync promotes the written extent to
 * DURABLE; a crash loses everything past the last fsync.
 *
 * Keyed by the libdb file id (a hash of the DB_FH name, since libdb
 * re-opens fds), so the frontier survives close/reopen the way a real
 * disk would.  A recovery test asks __db_sim_io_durable_end(key) for the
 * true post-crash durable frontier to truncate to.
 *
 * ponytail: bounded fixed table, O(n) scan; a WAL crash test tracks one
 * or two files.  Upgrade to a hash keyed table if a scenario needs many.
 */
#define DB_SIM_WB_FILES 16
#define DB_SIM_WB_NAMELEN 256
struct sim_wb_ent {
	uint64_t key;
	uint64_t written_end;
	uint64_t durable_end;
	char     name[DB_SIM_WB_NAMELEN];   /* path, for crash truncation */
	int      used;
};
static struct sim_wb_ent g_wb[DB_SIM_WB_FILES];
static _Atomic int g_wb_on;
/*
 * When set, a freshly-tracked file's durable frontier is SEEDED from its
 * current on-disk size at first touch (the bytes already on disk when
 * THIS process started are treated as durable).  Correct ONLY for a
 * process that inherited genuinely-durable files from a PRIOR process
 * (e.g. a recovery process opening files a crashed workload already
 * truncated to its durable frontier).  It is WRONG for a process that
 * creates/pre-extends its own files (a pre-extended-but-unsynced log
 * would be miscounted as durable), so it is OFF by default and the
 * single-process crash tests never set it -- only the crash-DURING-
 * recovery harness does, via __db_sim_wb_enable(DB_SIM_WB_SEED_ONDISK).
 */
static _Atomic int g_wb_seed_ondisk;

void
__db_sim_wb_enable(on)
	int on;
{
	if (on)
		memset(g_wb, 0, sizeof(g_wb));
	atomic_store_explicit(&g_wb_seed_ondisk,
	    on == DB_SIM_WB_SEED_ONDISK ? 1 : 0, memory_order_release);
	atomic_store_explicit(&g_wb_on, on ? 1 : 0, memory_order_release);
}

int
__db_sim_wb_active()
{
	return (atomic_load_explicit(&g_wb_on, memory_order_acquire));
}

static struct sim_wb_ent *
wb_slot(key)
	uint64_t key;
{
	int k, free_k = -1;

	for (k = 0; k < DB_SIM_WB_FILES; k++) {
		if (g_wb[k].used && g_wb[k].key == key)
			return (&g_wb[k]);
		if (free_k < 0 && !g_wb[k].used)
			free_k = k;
	}
	if (free_k >= 0) {
		g_wb[free_k].used = 1;
		g_wb[free_k].key = key;
		g_wb[free_k].written_end = 0;
		g_wb[free_k].durable_end = 0;
		g_wb[free_k].name[0] = '\0';
		return (&g_wb[free_k]);
	}
	return (NULL);   /* table full: this file is not tracked (bounded) */
}

/*
 * __db_sim_wb_note_name --
 *	Record the on-disk path for a tracked file so a crash can truncate
 *	it to the durable frontier.  Called from the write hook (which has
 *	the DB_FH name).  A no-op unless the write-back model is armed.
 */
void
__db_sim_wb_note_name(key, name)
	uint64_t key;
	const char *name;
{
	struct sim_wb_ent *e;
	struct stat sb;

	if (!__db_sim_wb_active() || name == NULL)
		return;
	e = wb_slot(key);
	if (e != NULL && e->name[0] == '\0') {
		(void)strncpy(e->name, name, DB_SIM_WB_NAMELEN - 1);
		e->name[DB_SIM_WB_NAMELEN - 1] = '\0';
		/*
		 * Seed the durable frontier from the file's CURRENT on-disk
		 * size ONLY when explicitly requested (a recovery process that
		 * inherited durable files from a crashed workload).  Bytes
		 * already on disk when this process began tracking are then
		 * durable; without this a crash-during-recovery would drop
		 * bytes that were ALREADY durable before recovery started,
		 * which no real power loss does.  OFF for a self-creating
		 * process (a pre-extended-but-unsynced log must NOT be counted
		 * durable -- that is the ack-before-fsync bug we catch).
		 */
		if (atomic_load_explicit(&g_wb_seed_ondisk,
		    memory_order_acquire) &&
		    e->durable_end == 0 && e->written_end == 0 &&
		    stat(name, &sb) == 0 && sb.st_size > 0) {
			e->durable_end = (uint64_t)sb.st_size;
			e->written_end = (uint64_t)sb.st_size;
		}
	}
}

void
__db_sim_wb_wrote(key, end_off)
	uint64_t key, end_off;
{
	struct sim_wb_ent *e;

	if (!__db_sim_wb_active())
		return;
	e = wb_slot(key);
	if (e != NULL && end_off > e->written_end)
		e->written_end = end_off;
}

void
__db_sim_wb_synced(key)
	uint64_t key;
{
	struct sim_wb_ent *e;

	if (!__db_sim_wb_active())
		return;
	e = wb_slot(key);
	if (e != NULL)
		e->durable_end = e->written_end;
}

uint64_t
__db_sim_wb_written_end(key)
	uint64_t key;
{
	struct sim_wb_ent *e;

	if (!__db_sim_wb_active())
		return (0);
	e = wb_slot(key);
	return (e != NULL ? e->written_end : 0);
}

uint64_t
__db_sim_io_durable_end(key)
	uint64_t key;
{
	struct sim_wb_ent *e;

	if (!__db_sim_wb_active())
		return (0);
	e = wb_slot(key);
	return (e != NULL ? e->durable_end : 0);
}

/*
 * __db_sim_wb_crash --
 *	Model a power loss: every tracked file's bytes past its last fsync
 *	(durable frontier) never reached the platter, so truncate the real
 *	file back to durable_end.  This is what makes an ACK-before-fsync
 *	bug detectable -- a commit whose log was written but not fsync'd is
 *	dropped here, exactly as a real disk would drop it on power loss.
 *	Returns the number of files truncated.  Called by a crash-recovery
 *	test in-process at the crash boundary, before recovery.
 *
 *	ponytail: truncate() by path (files are closed at crash); an
 *	ftruncate on a live fd if a scenario needs it while the file is open.
 */
int
__db_sim_wb_crash()
{
	int k, n = 0;

	if (!__db_sim_wb_active())
		return (0);
	for (k = 0; k < DB_SIM_WB_FILES; k++) {
		if (!g_wb[k].used || g_wb[k].name[0] == '\0')
			continue;
		/* Only shrink: durable_end <= written_end always, and we
		 * never grow a file (that would fabricate bytes). */
		if (truncate(g_wb[k].name, (off_t)g_wb[k].durable_end) == 0)
			n++;
		/* After the crash the volatile cache is gone: written == durable. */
		g_wb[k].written_end = g_wb[k].durable_end;
	}
	return (n);
}

/* ---- crash-DURING-recovery model ----
 *
 * Recovery (__db_apprec) does all its page writes and its recovery
 * checkpoint through the SAME __os_io/__os_fsync seam the write hooks
 * already sit on.  This model arms a per-run counter: when a target N > 0
 * is set, the Nth recovery-phase I/O op crashes the process (truncate to
 * the durable frontier, exactly like the workload crash, then _exit(42)).
 * That lets a scenario crash a recovery pass PARTWAY -- after some redo
 * pages are applied but before the recovery checkpoint is durable -- and
 * prove the NEXT recovery re-converges (recovery is idempotent +
 * interruptible, BDB's most important correctness property).
 *
 * ticks() reports how many recovery I/O ops have run this process, so a
 * harness runs recovery once uncrashed to learn the full-recovery I/O
 * count, then sweeps crash points 1..that.  Deterministic: the same seed
 * drives the same workload => the same recovery => the same I/O sequence
 * => the same crash point for a given N.
 */
static _Atomic unsigned long g_reccrash_target;   /* 0 = disarmed */
static _Atomic unsigned long g_reccrash_ticks;

void
__db_sim_reccrash_enable(target)
	unsigned long target;
{
	atomic_store_explicit(&g_reccrash_ticks, 0, memory_order_relaxed);
	atomic_store_explicit(&g_reccrash_target, target,
	    memory_order_release);
}

unsigned long
__db_sim_reccrash_ticks()
{
	return (atomic_load_explicit(&g_reccrash_ticks, memory_order_relaxed));
}

void
__db_sim_reccrash_tick()
{
	unsigned long tgt, n;

	tgt = atomic_load_explicit(&g_reccrash_target, memory_order_acquire);
	if (!__db_sim_active())
		return;
	/* Count this recovery-phase I/O op. */
	n = atomic_fetch_add_explicit(&g_reccrash_ticks, 1,
	    memory_order_relaxed) + 1;
	if (tgt == 0 || n < tgt)
		return;
	/* Reached the seeded crash point: drop every byte written but not
	 * yet fsync'd (recovery's own un-durable work AND any workload tail),
	 * then die abruptly like a power loss.  Disarm first so wb_crash's
	 * own I/O (it uses truncate, not the hooks, but be safe) cannot
	 * re-enter. */
	atomic_store_explicit(&g_reccrash_target, 0, memory_order_release);
	(void)__db_sim_wb_crash();
	fflush(NULL);
	_exit(42);
}

/* ---- latency (consumed by the __os_io hooks) ----
 *
 * A seeded per-I/O latency in ns.  In the single-process v1 pilots there
 * is no scheduler to REORDER against, so latency does not change any
 * invariant; but wiring it as a real (tiny, capped) sleep makes the knob
 * genuinely CONSUMED and lets a scenario model a slow disk.  Off by
 * default (lat_max == 0 => 0ns => no sleep).
 *
 * ponytail: a bounded nanosleep; the load-bearing use (completion-order
 * interleaving) is a v2 async-path item, this just makes the knob real.
 */
void
__db_sim_io_sleep_latency()
{
	int64_t ns;
	struct timespec ts;

	ns = __db_sim_io_latency();
	if (ns <= 0)
		return;
	fc_hit(DB_SIM_FC_LATENCY);
	/* Cap so an over-large seeded value cannot wedge a test. */
	if (ns > 2000000)
		ns = 2000000;                 /* 2ms cap */
	ts.tv_sec = 0;
	ts.tv_nsec = ns;
	(void)nanosleep(&ts, NULL);
}

/* ---- stale-read model (superseded-write ring) ----
 *
 * Catches recovery/cache code that returns a well-formed but OUT-OF-DATE
 * version of a block (skipping an LSN/version check).  On a write we
 * snapshot the CURRENT (about-to-be-superseded) bytes at (fkey,off) into
 * a ring; on a seeded stale-read coin at a matching (fkey,off) we return
 * that prior version instead of the fresh bytes.  Both bytes are
 * well-formed -- the fault is that the reader accepted the old one.
 * Adapted from xtc's __xtc_sim_io_stale_*.
 *
 * ponytail: fixed ring, O(n) newest-match scan (bounded); a scenario
 * touches a handful of hot blocks.
 */
#define DB_SIM_STALE_RING   32
#define DB_SIM_STALE_MAXLEN 512
struct sim_stale_ent {
	uint64_t fkey;
	uint64_t off;
	int      len;
	unsigned char buf[DB_SIM_STALE_MAXLEN];
};
static struct sim_stale_ent g_stale[DB_SIM_STALE_RING];
static _Atomic int g_stale_head;
static _Atomic int g_stale_pct;

void
__db_sim_io_stale_enable(pct_per_1000)
	unsigned pct_per_1000;
{
	if (pct_per_1000 > 1000)
		pct_per_1000 = 1000;
	memset(g_stale, 0, sizeof(g_stale));
	atomic_store_explicit(&g_stale_head, 0, memory_order_relaxed);
	atomic_store_explicit(&g_stale_pct, (int)pct_per_1000,
	    memory_order_release);
}

int
__db_sim_io_stale_armed()
{
	return (atomic_load_explicit(&g_stale_pct, memory_order_acquire) > 0);
}

void
__db_sim_io_stale_record(fkey, off, buf, len)
	uint64_t fkey, off;
	const void *buf;
	int len;
{
	int h, cp;

	if (atomic_load_explicit(&g_stale_pct, memory_order_acquire) <= 0 ||
	    !__db_sim_active() || buf == NULL || len <= 0)
		return;
	cp = len < DB_SIM_STALE_MAXLEN ? len : DB_SIM_STALE_MAXLEN;
	h = atomic_load_explicit(&g_stale_head, memory_order_relaxed);
	g_stale[h].fkey = fkey;
	g_stale[h].off = off;
	g_stale[h].len = cp;
	memcpy(g_stale[h].buf, buf, (size_t)cp);
	atomic_store_explicit(&g_stale_head, (h + 1) % DB_SIM_STALE_RING,
	    memory_order_relaxed);
}

int
__db_sim_io_stale_read(fkey, off, buf, len)
	uint64_t fkey, off;
	void *buf;
	int len;
{
	int i, h, pct;

	pct = atomic_load_explicit(&g_stale_pct, memory_order_acquire);
	if (pct <= 0 || !__db_sim_active() || buf == NULL || len <= 0)
		return (0);
	if ((int)__db_sim_rng_range(DB_SIM_RNG_IO, 1000) >= pct)
		return (0);
	/* Newest matching prior version wins. */
	h = atomic_load_explicit(&g_stale_head, memory_order_relaxed);
	for (i = 0; i < DB_SIM_STALE_RING; i++) {
		int k = (h - 1 - i + DB_SIM_STALE_RING) % DB_SIM_STALE_RING;
		if (g_stale[k].len > 0 && g_stale[k].fkey == fkey &&
		    g_stale[k].off == off) {
			int cp = g_stale[k].len < len ? g_stale[k].len : len;
			memcpy(buf, g_stale[k].buf, (size_t)cp);
			fc_hit(DB_SIM_FC_STALE);
			return (1);
		}
	}
	return (0);
}

/* ---- clock-skew / time-jump fault (sim_clock.h) ----
 *
 * A seeded skew applied at the __os_gettime seam.  Three components, all
 * bounded and drawn from the dedicated CLOCK stream so arming this never
 * shifts another site's draws:
 *   - g_clk_offset: a FIXED per-run offset (set once at arm), applied to
 *     every read -- a clock that reads steadily ahead/behind true time;
 *   - g_clk_jitter: a per-read uniform jitter in [-j, +j] -- an imprecise,
 *     jumpy clock;
 *   - g_clk_jump*:  an occasional discrete jump, forward or BACKWARD, by
 *     up to g_clk_jump_ns, with per-1000 probability g_clk_jump_pct.  The
 *     backward jump is the dangerous one: it can make a deadline (now +
 *     timeout, compared later against a smaller now) never be reached.
 *
 * The skew is applied to a signed nanosecond accumulator then clamped so
 * the result never goes negative (a negative tv_sec would be nonsense to
 * the caller and is not what "clock reads earlier" means).  Determinism:
 * the whole sequence of skews is a pure function of the seed.
 */
static _Atomic int     g_clk_on;
static _Atomic int64_t g_clk_offset;     /* fixed per-run offset (ns) */
static _Atomic int64_t g_clk_jitter;     /* per-read jitter bound (ns) */
static _Atomic int64_t g_clk_jump_ns;    /* jump magnitude (ns) */
static _Atomic int     g_clk_jump_pct;   /* per-1000 jump probability */
static _Atomic unsigned long g_clk_fires;
static _Atomic long    g_clk_settle;     /* skew reads left; <0 => never */

#define NS_PER_S 1000000000LL

void
__db_sim_clock_enable(offset_ns, jitter_ns, jump_ns, jump_pct)
	int64_t offset_ns, jitter_ns, jump_ns;
	unsigned jump_pct;
{
	int64_t off = offset_ns;

	/* If no fixed offset requested, draw a seeded one in [-1s, +1s] so a
	 * bare enable still models a clock that is off by a constant. */
	if (off == 0)
		off = (int64_t)__db_sim_rng_range(DB_SIM_RNG_CLOCK,
		    (uint64_t)(2 * NS_PER_S + 1)) - NS_PER_S;
	atomic_store_explicit(&g_clk_offset, off, memory_order_relaxed);
	atomic_store_explicit(&g_clk_jitter,
	    jitter_ns < 0 ? -jitter_ns : jitter_ns, memory_order_relaxed);
	atomic_store_explicit(&g_clk_jump_ns,
	    jump_ns < 0 ? -jump_ns : jump_ns, memory_order_relaxed);
	atomic_store_explicit(&g_clk_jump_pct,
	    jump_pct > 1000 ? 1000 : (int)jump_pct, memory_order_relaxed);
	atomic_store_explicit(&g_clk_fires, 0, memory_order_relaxed);
	atomic_store_explicit(&g_clk_settle, -1, memory_order_relaxed);
	atomic_store_explicit(&g_clk_on, 1, memory_order_release);
}

void
__db_sim_clock_settle_after(n)
	unsigned n;
{
	atomic_store_explicit(&g_clk_settle, (long)n, memory_order_relaxed);
}

void
__db_sim_clock_disable()
{
	atomic_store_explicit(&g_clk_on, 0, memory_order_release);
	atomic_store_explicit(&g_clk_offset, 0, memory_order_relaxed);
	atomic_store_explicit(&g_clk_jitter, 0, memory_order_relaxed);
	atomic_store_explicit(&g_clk_jump_ns, 0, memory_order_relaxed);
	atomic_store_explicit(&g_clk_jump_pct, 0, memory_order_relaxed);
	atomic_store_explicit(&g_clk_settle, -1, memory_order_relaxed);
}

int
__db_sim_clock_armed()
{
	return (atomic_load_explicit(&g_clk_on, memory_order_acquire) &&
	    __db_sim_active());
}

unsigned long
__db_sim_clock_fire_count()
{
	return (atomic_load_explicit(&g_clk_fires, memory_order_relaxed));
}

void
__db_sim_clock_skew(sec, nsec, monotonic)
	time_t *sec;
	long *nsec;
	int monotonic;
{
	int64_t total, jit, jbound, jmag;
	int jpct;
	long left;

	COMPQUIET(monotonic, 0);
	if (sec == NULL || nsec == NULL || !__db_sim_clock_armed())
		return;

	/* Transient-jump budget: after `settle` skewed reads the clock
	 * returns to true time (a bounded disturbance that recovers). */
	left = atomic_load_explicit(&g_clk_settle, memory_order_relaxed);
	if (left == 0)
		return;                     /* settled: real clock */
	if (left > 0)
		atomic_store_explicit(&g_clk_settle, left - 1,
		    memory_order_relaxed);

	/* real reading as a signed ns accumulator */
	total = (int64_t)*sec * NS_PER_S + (int64_t)*nsec;

	/* fixed per-run offset */
	total += atomic_load_explicit(&g_clk_offset, memory_order_relaxed);

	/* per-read jitter in [-bound, +bound] */
	jbound = atomic_load_explicit(&g_clk_jitter, memory_order_relaxed);
	if (jbound > 0) {
		jit = (int64_t)__db_sim_rng_range(DB_SIM_RNG_CLOCK,
		    (uint64_t)(2 * jbound + 1)) - jbound;
		total += jit;
	}

	/* occasional discrete jump, forward or BACKWARD */
	jmag = atomic_load_explicit(&g_clk_jump_ns, memory_order_relaxed);
	jpct = atomic_load_explicit(&g_clk_jump_pct, memory_order_relaxed);
	if (jmag > 0 && jpct > 0 &&
	    (int)__db_sim_rng_range(DB_SIM_RNG_CLOCK, 1000) < jpct) {
		/* seeded coin: 0 => backward, 1 => forward */
		int64_t j = (int64_t)__db_sim_rng_range(DB_SIM_RNG_CLOCK,
		    (uint64_t)jmag + 1);
		if (__db_sim_rng_range(DB_SIM_RNG_CLOCK, 2) == 0)
			total -= j;                 /* BACKWARD (dangerous) */
		else
			total += j;                 /* forward */
	}

	/* Clamp: a clock never reads before the epoch here. */
	if (total < 0)
		total = 0;

	*sec = (time_t)(total / NS_PER_S);
	*nsec = (long)(total % NS_PER_S);
	fc_hit(DB_SIM_FC_CLOCK);
	atomic_fetch_add_explicit(&g_clk_fires, 1, memory_order_relaxed);
}

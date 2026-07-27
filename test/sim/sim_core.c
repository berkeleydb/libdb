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

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
	__db_sim_wb_enable(0);
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

/* ---- buggify (per-run cached coin) ---- */

#define DB_SIM_BUG_MAX 64
static char        g_bug_name[DB_SIM_BUG_MAX][48];
static signed char g_bug_decided[DB_SIM_BUG_MAX];   /* 0/1 */
static _Atomic int g_bug_n;
static _Atomic int g_bug_on;
static _Atomic int g_bug_pct;

void
__db_sim_buggify_enable(pct_per_1000)
	unsigned pct_per_1000;
{
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
 * agrees and the whole run replays.
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
		if (strncmp(g_bug_name[i], name, sizeof(g_bug_name[0])) == 0)
			return (g_bug_decided[i]);

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
	atomic_store_explicit(&g_bug_n, n + 1, memory_order_relaxed);
	return (decision);
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
	if (pct >= 1000)
		return (1);
	return ((int)__db_sim_rng_range(DB_SIM_RNG_IO, 1000) < pct);
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
	return ((int)__db_sim_rng_range(DB_SIM_RNG_IO, 1000) < pct);
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
struct sim_wb_ent {
	uint64_t key;
	uint64_t written_end;
	uint64_t durable_end;
	int      used;
};
static struct sim_wb_ent g_wb[DB_SIM_WB_FILES];
static _Atomic int g_wb_on;

void
__db_sim_wb_enable(on)
	int on;
{
	if (on)
		memset(g_wb, 0, sizeof(g_wb));
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
		return (&g_wb[free_k]);
	}
	return (NULL);   /* table full: this file is not tracked (bounded) */
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

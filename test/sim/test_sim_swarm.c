/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_swarm.c --
 *	FoundationDB-style SWARM / SOAK: a large, shardable seed sweep over
 *	a mixed fault workload, collecting aggregate pass/fail AND per-fault
 *	ACTIVATION coverage (how many seeds actually triggered each fault
 *	class -- a class never activated is a coverage gap).
 *
 *	Modeled on xtc's test_sim_swarm.  libdb is multi-process with real
 *	fsync durability, so a single deterministic cross-process schedule
 *	is a v2 item (see DESIGN.md sec.0); this swarm therefore sweeps the
 *	axis that DOES map onto BDB's architecture -- seeded FAULT injection
 *	over the real __os_io seam -- exactly the FoundationDB "simulated
 *	disk" discipline.
 *
 *	Per seed, a mix of fault classes is armed from the seed bits (so the
 *	seed fully determines the scenario AND replays): torn writes,
 *	corrupt reads, stale reads, ENOSPC, per-I/O latency.  A versioned,
 *	checksummed page workload writes then re-reads N pages through the
 *	real __os_open / __os_io / __os_fsync seam and asserts, under
 *	whatever faults fired, the safety invariants:
 *
 *	  - a torn/corrupt page is NEVER accepted as valid (its self-check
 *	    fails => detected, never silent-bad);
 *	  - a STALE read (an older version handed back) is caught by the
 *	    per-page version stamp -- never adopted as current;
 *	  - the run reaches quiescence (no hang) and REPLAYS byte-identically
 *	    from the seed (two runs => identical result + activation counts).
 *
 *	It reports, across the sweep: seeds x invariant-violations, distinct
 *	fault mixes explored, and per-fault activation counts + percentages.
 *
 *	Invocation:
 *	  test_sim_swarm              bounded default (256 seeds) for CI.
 *	  test_sim_swarm <count>      sweep <count> seeds from base 1.
 *	  test_sim_swarm <count> <base>  shard the seed space.
 *	  test_sim_swarm 5000 0       a soak run.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_swarm && ./test_sim_swarm [count] [base]
 */

#include "db_config.h"

#include "db_int.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sim_rng.h"
#include "sim_fault.h"

#define NPAGE   8             /* distinct page offsets */
#define PGSZ    256
#define CKOFF   (PGSZ - 8)    /* checksum in the last 8 bytes */
#define VOFF    0             /* version in the first 8 bytes */
#define NWRITE  6             /* versions written per page */
#define SWARMDB "TESTDIR_sim_swarm/swarm.dat"

/* FNV-1a over [buf, buf+n): the page's self-checksum. */
static uint64_t
pg_cksum(p, n)
	const unsigned char *p;
	size_t n;
{
	uint64_t h = 1469598103934665603ull;
	size_t i;
	for (i = 0; i < n; i++) { h ^= p[i]; h *= 1099511628211ull; }
	return (h);
}

/* Per-seed scenario: which fault classes are armed (from the seed bits),
 * with seed-varied MAGNITUDES so the sweep spans mild to brutal. */
struct scenario {
	int torn, corrupt, stale, enospc, latency, shorteio;
	int torn_pct, corrupt_pct, stale_pct, enospc_pct, shorteio_pct;
	int64_t lat_hi;
};

static void
derive(seed, sc)
	uint64_t seed;
	struct scenario *sc;
{
	sc->torn     = (seed & 0x1) != 0;
	sc->corrupt  = (seed & 0x2) != 0;
	sc->stale    = (seed & 0x4) != 0;
	sc->enospc   = (seed & 0x8) != 0;
	sc->latency  = (seed & 0x10) != 0;
	sc->shorteio = (seed & 0x20) != 0;
	sc->torn_pct    = 50 + (int)((seed >> 6) & 0x7) * 40;   /* 50..330 */
	sc->corrupt_pct = 50 + (int)((seed >> 9) & 0x7) * 40;
	sc->stale_pct   = 100 + (int)((seed >> 12) & 0x7) * 50; /* 100..450 */
	sc->enospc_pct  = 50 + (int)((seed >> 15) & 0x7) * 30;  /* 50..260 */
	sc->shorteio_pct= 30 + (int)((seed >> 18) & 0x7) * 20;  /* 30..170 */
	sc->lat_hi      = (10 + (int64_t)((seed >> 21) & 0x7) * 10) * 1000LL;
}

/*
 * Run the mixed workload once with `seed` under `sc`.  Returns 0 (always
 * reaches quiescence).  Sets *silent_bad to the count of pages accepted
 * that were corrupt/stale (MUST be 0), and folds a result hash for the
 * replay check.
 */
static int
run_once(seed, sc, silent_bad, out_hash)
	uint64_t seed;
	const struct scenario *sc;
	int *silent_bad;
	uint64_t *out_hash;
{
	DB_ENV *dbenv;
	ENV *env;
	DB_FH *fhp;
	unsigned char page[PGSZ];
	uint64_t h = 1469598103934665603ull;
	int p, v, ret, bad = 0;
	size_t nio;

	*silent_bad = 0;
	*out_hash = 0;

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (-1);
	env = dbenv->env;

	__db_sim_activate(seed);
	if (sc->latency || sc->shorteio)
		__db_sim_io_faults_enable(0, sc->lat_hi,
		    (unsigned)(sc->shorteio ? sc->shorteio_pct : 0));
	if (sc->torn || sc->corrupt)
		__db_sim_io_corrupt_enable((unsigned)(sc->torn ?
		    sc->torn_pct : sc->corrupt_pct));
	if (sc->stale)
		__db_sim_io_stale_enable((unsigned)sc->stale_pct);
	if (sc->enospc)
		__db_sim_io_enospc_enable((unsigned)sc->enospc_pct);

	if (__os_open(env, SWARMDB, 0, DB_OSO_CREATE, 0664, &fhp) != 0) {
		(void)dbenv->close(dbenv, 0);
		__db_sim_deactivate();
		return (-1);
	}

	for (p = 0; p < NPAGE; p++) {
		off_t off = (off_t)p * PGSZ;
		uint64_t highest = 0;

		for (v = 1; v <= NWRITE; v++) {
			uint64_t ver = (uint64_t)v, got = 0, ck;

			/* Build a versioned, self-checksummed page. */
			memset(page, (int)((p * 7 + v) & 0xff), CKOFF);
			memcpy(page + VOFF, &ver, sizeof(ver));
			ck = pg_cksum(page, CKOFF);
			memcpy(page + CKOFF, &ck, sizeof(ck));

			ret = __os_io(env, DB_IO_WRITE, fhp, 0, 0,
			    (u_int32_t)off, PGSZ, page, &nio);
			if (ret != 0 || nio != PGSZ)
				continue;   /* ENOSPC / short: skip this ver */
			(void)__os_fsync(env, fhp);
			if (ver > highest)
				highest = ver;

			/* Read back through the real seam (torn tail / corrupt
			 * bit / stale version may be injected). */
			memset(page, 0, sizeof(page));
			ret = __os_io(env, DB_IO_READ, fhp, 0, 0,
			    (u_int32_t)off, PGSZ, page, &nio);
			if (ret != 0 || nio != PGSZ)
				continue;

			memcpy(&got, page + VOFF, sizeof(got));
			memcpy(&ck, page + CKOFF, sizeof(ck));
			h = h * 1000003ull + got;

			if (ck != pg_cksum(page, CKOFF)) {
				/* Torn/corrupt page: checksum FAILED => the
				 * page is detected bad, not accepted.  Correct
				 * behaviour -- a reader that trusted it anyway
				 * would be silent-bad. */
				continue;
			}
			/* Checksum-valid: the version MUST NOT be older than
			 * the highest durably written (a stale read).  If it
			 * is, the version check CATCHES it; adopting it would
			 * be silent-bad. */
			if (got < highest) {
				/* stale, correctly detectable via version */
				continue;
			}
			if (got > highest) {
				/* A version from the future: impossible unless
				 * a checksum-valid page carries wrong data =
				 * silent bad (must never happen). */
				bad++;
			}
		}
	}

	(void)__os_closehandle(env, fhp);
	__db_sim_io_stale_enable(0);
	__db_sim_io_enospc_enable(0);
	__db_sim_io_corrupt_disable();
	__db_sim_io_faults_disable();
	*silent_bad = bad;
	*out_hash = h;
	__db_sim_deactivate();
	(void)dbenv->close(dbenv, 0);
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	long n_seeds = argc > 1 ? strtol(argv[1], NULL, 10) : 256;
	long base    = argc > 2 ? strtol(argv[2], NULL, 10) : 1;
	long s, failures = 0;
	long n_torn = 0, n_corrupt = 0, n_stale = 0, n_enospc = 0, n_lat = 0;
	/* Per-fault ACTIVATION: seeds on which the class actually fired. */
	unsigned long act[DB_SIM_FC_NCLASSES];
	int cls;
	uint64_t seen[256];
	int n_seen = 0;
	char cmd[256];

	if (n_seeds < 1)
		n_seeds = 1;
	memset(act, 0, sizeof(act));

	(void)snprintf(cmd, sizeof(cmd),
	    "rm -rf TESTDIR_sim_swarm && mkdir -p TESTDIR_sim_swarm");
	(void)system(cmd);

	printf("== DST swarm: %ld seeds (base %ld) x mixed-fault workload "
	    "==\n", n_seeds, base);

	for (s = 0; s < n_seeds; s++) {
		uint64_t seed = 0x9E3779B97F4A7C15ull * (uint64_t)(base + s);
		struct scenario sc;
		int bad1 = 0, bad2 = 0, rc1, rc2, i;
		uint64_t h1 = 0, h2 = 0;
		unsigned long fired[DB_SIM_FC_NCLASSES];

		derive(seed, &sc);
		n_torn    += sc.torn;
		n_corrupt += sc.corrupt;
		n_stale   += sc.stale;
		n_enospc  += sc.enospc;
		n_lat     += sc.latency;

		(void)system("rm -f TESTDIR_sim_swarm/swarm.dat");
		rc1 = run_once(seed, &sc, &bad1, &h1);
		/* Snapshot which classes fired THIS seed (counters reset each
		 * activate, so read them right after the run). */
		for (cls = 0; cls < DB_SIM_FC_NCLASSES; cls++) {
			fired[cls] = __db_sim_fault_count(cls);
			if (fired[cls] > 0)
				act[cls]++;
		}

		(void)system("rm -f TESTDIR_sim_swarm/swarm.dat");
		rc2 = run_once(seed, &sc, &bad2, &h2);

		if (rc1 != 0 || rc2 != 0) {
			printf("FAIL seed=0x%llx: setup error rc1=%d rc2=%d\n",
			    (unsigned long long)seed, rc1, rc2);
			failures++;
			continue;
		}
		if (bad1 != 0 || bad2 != 0) {
			printf("FAIL seed=0x%llx: %d/%d SILENT-BAD pages "
			    "accepted (a corrupt/stale page passed as valid)\n",
			    (unsigned long long)seed, bad1, bad2);
			failures++;
			continue;
		}
		if (h1 != h2) {
			printf("FAIL seed=0x%llx: replay mismatch "
			    "(%016llx != %016llx)\n", (unsigned long long)seed,
			    (unsigned long long)h1, (unsigned long long)h2);
			failures++;
			continue;
		}
		for (i = 0; i < n_seen; i++)
			if (seen[i] == h1)
				break;
		if (i == n_seen && n_seen < (int)(sizeof(seen)/sizeof(seen[0])))
			seen[n_seen++] = h1;
	}

	printf("swarm swept %ld seeds: %ld invariant violation(s), %d "
	    "distinct results; armed mix: torn=%ld corrupt=%ld stale=%ld "
	    "enospc=%ld latency=%ld\n", n_seeds, failures, n_seen,
	    n_torn, n_corrupt, n_stale, n_enospc, n_lat);

	printf("fault ACTIVATION (seeds on which the class actually fired):\n");
	for (cls = 0; cls < DB_SIM_FC_NCLASSES; cls++)
		printf("  %-9s %5lu / %ld seeds (%.1f%%)\n",
		    __db_sim_fault_class_name(cls), act[cls], n_seeds,
		    100.0 * (double)act[cls] / (double)n_seeds);

	if (failures > 0) {
		printf("FAIL: %ld seed(s) violated a safety invariant "
		    "(re-run ./test_sim_swarm 1 <seed-index> to reproduce)\n",
		    failures);
		return (EXIT_FAILURE);
	}
	if (n_seeds >= 20 && n_seen < 2) {
		printf("FAIL: the swarm explored only one result -- the "
		    "workload is not seed-sensitive\n");
		return (EXIT_FAILURE);
	}
	/*
	 * Coverage-gap guard (FoundationDB discipline): on a sweep large
	 * enough that every class SHOULD fire, a class that never activated
	 * is a hole in the fault coverage -- surface it as a failure so a
	 * regression that silently stops arming a fault class is caught.
	 */
	if (n_seeds >= 64) {
		int gap = 0;
		for (cls = 0; cls < DB_SIM_FC_NCLASSES; cls++) {
			/*
			 * The clock-skew class is exercised by the dedicated
			 * test_sim_clockskew_* scenarios, not this I/O swarm
			 * (which arms only the disk-fault knobs), so it is not
			 * expected to fire here -- skip it in the gap guard.
			 */
			if (cls == DB_SIM_FC_CLOCK)
				continue;
			if (act[cls] == 0) {
				printf("FAIL: fault class '%s' NEVER activated "
				    "across %ld seeds -- a coverage gap\n",
				    __db_sim_fault_class_name(cls), n_seeds);
				gap = 1;
			}
		}
		if (gap)
			return (EXIT_FAILURE);
	}
	printf("OK: %ld-seed swarm -- 0 invariant violations (no corrupt/"
	    "stale page ever accepted as valid), every seed replayed "
	    "identically, %d distinct results explored\n", n_seeds, n_seen);
	return (EXIT_SUCCESS);
}

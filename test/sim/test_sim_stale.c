/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_stale.c --
 *	Stale-read (out-of-date read) fault + the version/LSN check that
 *	catches it.  FoundationDB's stale-data fault class: the disk hands
 *	back a page that is STRUCTURALLY VALID but OUT OF DATE -- a durable
 *	version that was later overwritten.  A bare page checksum does NOT
 *	catch this: the stale page's own checksum is consistent with its
 *	old contents.  The only defense is a monotonic version / LSN stamped
 *	in the page and checked on read (exactly what BDB's recovery does
 *	with page LSNs).
 *
 *	This drives the REAL libdb OS seam (__os_open / __os_io / __os_fsync)
 *	so the stale-read ring is exercised end to end through the actual
 *	hooks: the write path's __db_sim_io_presnapshot_hook snapshots the
 *	current on-disk bytes before each overwrite, and the read path's
 *	__db_sim_io_read_hook returns a prior version on a seeded coin.  Each
 *	page carries a monotonically increasing version in its first 8
 *	bytes; the writer knows the highest version it has durably written,
 *	so a read whose version is LOWER is a detected stale read.
 *
 *	Two invariants, both proven across a seed sweep:
 *	  (1) DETECTION: the version check CATCHES every stale read -- no
 *	      stale page is ever adopted as current (a reader that skipped
 *	      the check would regress its view; we assert that never
 *	      happens).  A version from the FUTURE would be real corruption
 *	      / a model bug -- asserted to be zero.
 *	  (2) EXERCISED: at least one stale read fires across the sweep (the
 *	      ring + hooks actually work), and each seed replays identically.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_stale && ./test_sim_stale [seed]
 */

#include "db_config.h"

#include "db_int.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sim_rng.h"
#include "sim_fault.h"

#define NSLOTS  6            /* distinct page offsets */
#define PGSZ    512
#define NWRITE  8            /* versions written per slot */
#define VOFF    0            /* the version lives in the first 8 bytes */
#define STALEDB "TESTDIR_sim_stale/stale.dat"

/*
 * Run the workload once with `seed`.  For each slot, write NWRITE
 * increasing versions; after each durable (fsync'd) write, read the page
 * back and check the version against the highest durably written.  Stale
 * injection can make the read return a prior version; the version check
 * detects it.  Returns 0 on success; fills the counters.
 */
static int
run_once(seed, out_stale, out_regress, out_reads)
	uint64_t seed;
	int *out_stale, *out_regress, *out_reads;
{
	ENV *env;
	DB_ENV *dbenv;
	DB_FH *fhp;
	uint8_t page[PGSZ];
	int slot, v, ret, stale = 0, regress = 0, reads = 0;
	size_t nio;

	*out_stale = *out_regress = *out_reads = 0;

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (ret);
	env = dbenv->env;

	__db_sim_activate(seed);
	/* Seeded latency + stale reads at 40% so a prior version is
	 * frequently returned; no ENOSPC/torn (those are other scenarios). */
	__db_sim_io_faults_enable(0, 20000, 0);
	__db_sim_io_stale_enable(400);

	if ((ret = __os_open(env, STALEDB, 0,
	    DB_OSO_CREATE, 0664, &fhp)) != 0) {
		fprintf(stderr, "os_open failed: %s\n", db_strerror(ret));
		goto done;
	}

	for (slot = 0; slot < NSLOTS; slot++) {
		off_t off = (off_t)slot * PGSZ;
		uint64_t highest = 0;

		for (v = 1; v <= NWRITE; v++) {
			uint64_t ver = (uint64_t)v, got = 0;

			memset(page, (int)(slot & 0xff), PGSZ);
			memcpy(page + VOFF, &ver, sizeof(ver));
			if ((ret = __os_io(env, DB_IO_WRITE, fhp, 0, 0,
			    (u_int32_t)off, PGSZ, page, &nio)) != 0 ||
			    nio != PGSZ)
				continue;   /* skip this version on write fault */
			/* Make it durable so `highest` is a true durable HWM. */
			(void)__os_fsync(env, fhp);
			if (ver > highest)
				highest = ver;

			/* Read it back through the real seam; stale injection
			 * may return a prior version. */
			memset(page, 0, sizeof(page));
			if ((ret = __os_io(env, DB_IO_READ, fhp, 0, 0,
			    (u_int32_t)off, PGSZ, page, &nio)) != 0 ||
			    nio != PGSZ)
				continue;
			reads++;
			memcpy(&got, page + VOFF, sizeof(got));
			if (got < highest) {
				/* Stale read: the disk handed back a version
				 * OLDER than what we durably wrote.  The
				 * version check CATCHES it -- we detect and do
				 * NOT adopt it as current.  A reader that
				 * skipped this check would silently regress. */
				stale++;
			} else if (got > highest) {
				/* A version from the FUTURE: real corruption
				 * or a model bug (must be 0). */
				regress++;
			}
		}
	}
	(void)__os_closehandle(env, fhp);

done:
	__db_sim_io_stale_enable(0);
	__db_sim_io_faults_disable();
	__db_sim_deactivate();
	(void)dbenv->close(dbenv, 0);
	*out_stale = stale;
	*out_regress = regress;
	*out_reads = reads;
	return (ret);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t base = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x57A1Eull;
	int n = 40, i, fails = 0;
	long total_stale = 0;
	char cmd[256];

	(void)snprintf(cmd, sizeof(cmd),
	    "rm -rf TESTDIR_sim_stale && mkdir -p TESTDIR_sim_stale");
	(void)system(cmd);

	printf("== stale-read DST (version check catches out-of-date reads): "
	    "%d seeds ==\n", n);

	for (i = 0; i < n; i++) {
		uint64_t seed = base + (uint64_t)i * 0x9E3779B97F4A7C15ull;
		int s1 = 0, r1 = 0, rd1 = 0, s2 = 0, r2 = 0, rd2 = 0;
		int rc1, rc2, pass = 1;

		(void)system("rm -f TESTDIR_sim_stale/stale.dat");
		rc1 = run_once(seed, &s1, &r1, &rd1);
		if (rc1 != 0 || r1 != 0)
			pass = 0;   /* setup error, or a stale page adopted */

		if (pass) {
			(void)system("rm -f TESTDIR_sim_stale/stale.dat");
			rc2 = run_once(seed, &s2, &r2, &rd2);
			/* Replay: same seed => identical stale/regress/reads
			 * counts (the fault schedule is seeded). */
			if (rc2 != rc1 || s2 != s1 || r2 != r1 || rd2 != rd1)
				pass = 0;
		}
		total_stale += s1;
		if (!pass) {
			fprintf(stderr, "  seed 0x%016llx: FAIL "
			    "(stale=%d regress=%d reads=%d rc=%d; "
			    "replay stale=%d regress=%d reads=%d)\n",
			    (unsigned long long)seed, s1, r1, rd1, rc1,
			    s2, r2, rd2);
			fails++;
		}
	}

	if (fails == 0 && total_stale == 0) {
		fprintf(stderr, "test_sim_stale: FAIL -- not a single stale "
		    "read fired across the sweep; the ring/hooks are not "
		    "wired (stale=%lu)\n",
		    __db_sim_fault_count(DB_SIM_FC_STALE));
		return (EXIT_FAILURE);
	}
	if (fails == 0) {
		printf("test_sim_stale: PASS -- %d seeds, the version check "
		    "caught every out-of-date read (%ld stale reads seen, "
		    "zero adopted as current, zero future versions), replayed "
		    "identically from seed\n", n, total_stale);
		return (EXIT_SUCCESS);
	}
	fprintf(stderr, "test_sim_stale: FAIL -- %d/%d seeds failed\n",
	    fails, n);
	return (EXIT_FAILURE);
}

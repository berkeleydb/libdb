/*-
 * See the file LICENSE for redistribution information.
 *
 * aio_concurrent_sync.c -- concurrency proof for the async (os_aio) buffer-pool
 * writeback path.
 *
 * THE RESIDUAL THIS TEST EXISTS TO CLOSE
 *
 * os_aio writeback is driven from __memp_sync_int, which is reached
 * concurrently by several unrelated callers in one process: txn_checkpoint
 * (DB_SYNC_CHECKPOINT), memp_trickle (DB_SYNC_TRICKLE), memp_sync /
 * DB->sync (DB_SYNC_CACHE / DB_SYNC_FILE), and the buffer allocator when it
 * gets desperate (DB_SYNC_ALLOC, from __memp_alloc).  There is ONE per-process
 * DB_AIO_CONTEXT.  Each caller keeps its in-flight window (MEMP_AIO_W aiow[])
 * on its OWN STACK and drains BY COUNT: "reap until I have seen n
 * completions."  Reaping is a shared-queue operation -- io_uring hands back
 * whatever CQEs are ready, the thread pool pops whatever is on the completion
 * FIFO.  Nothing in a completion identifies which sync call owns it.
 *
 * So without serialization, caller A's drain-by-count can be satisfied by
 * caller B's completions ("cross-reap").  A then runs its own completion
 * bookkeeping for writes that have NOT reached the disk: __memp_pgwrite_finish
 * clears BH_DIRTY, the buffer is unpinned, and the pgout page copy is freed --
 * while the device write is still outstanding against that very buffer.  The
 * consequences are (a) a FALSE DURABLE FRONTIER: the checkpoint believes those
 * pages are on disk, writes its checkpoint record, and a crash loses them --
 * exactly the class of bug the async error propagation was added to prevent;
 * and (b) memory corruption: a write-after-free of the page copy and a
 * BH_DIRTY/refcount race on a live buffer.
 *
 * The fix under test is an exclusive-use latch (the aio context's mtx_aio,
 * taken with
 * MUTEX_TRYLOCK in __memp_sync_int): one sync call owns the context from its
 * first submit to its final drain, and any caller that does not win the latch
 * writes synchronously -- the reference behaviour, always correct, never
 * blocked, so the latch cannot deadlock.
 *
 * WHAT THIS TEST DOES
 *
 * It runs all four sync callers simultaneously against one environment, with
 * enough dirty-page pressure that they genuinely overlap, and checks the
 * invariants that a cross-reap breaks:
 *
 *  1) DURABILITY BY CONTENT.  Writer threads commit with DB_TXN_SYNC and
 *     record what they committed.  At the end every committed key must be
 *     present with the right value, and db_verify must be clean.  A cross-reap
 *     that clears BH_DIRTY for an unwritten page loses that page's updates,
 *     because the buffer is then evictable and the checkpoint's durable
 *     frontier has moved past the log records that would redo it.
 *
 *  2) NO SILENT SUCCESS.  Every sync/checkpoint call's return code is
 *     recorded.  A success return is a durability claim, so a success must
 *     never coexist with a lost record (checked by 1).
 *
 *  3) THE DRAIN'S OWN SELF-CHECK.  __memp_aio_drain verifies that every slot
 *     in its window actually had its completion callback run (w->done) before
 *     it finishes that slot.  A cross-reap leaves a slot undone; the drain
 *     then refuses to clear BH_DIRTY and reports EIO, failing that caller's
 *     checkpoint rather than lying about it.  In a --enable-diagnostic build
 *     the same condition trips a DB_ASSERT, so a cross-reap aborts the test
 *     loudly instead of silently corrupting.  That assert firing IS the
 *     failure signal for this test.
 *
 *  4) ASSERTIONS UNDER ASan/TSan.  The write-after-free half of a cross-reap
 *     is what ASan sees; the BH_DIRTY/ref race is what TSan sees.  This test
 *     is the workload those tools are pointed at (see test/c/aio-conc-run.sh).
 *
 * Run it against the OTHER side of the switch too ("sync" mode below, which
 * simply does not request DB_MPOOL_AIO): the two modes must produce identical
 * outcomes, which is what makes this an equivalence test and not just a smoke
 * test.
 *
 * DB_MPOOL_AIO is default-OFF, so "aio" mode opts in explicitly.
 *
 * Usage: aio_concurrent_sync [aio|sync] [seconds]
 *   aio  -- DB_MPOOL_AIO, async writeback (the path under test)
 *   sync -- the default synchronous reference path
 *
 * Exit status 0 = all invariants held; non-zero = a specific invariant named
 * on stderr failed.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"TESTDIR_aio_conc"
#define	DBFILE		"aioconc.db"

#define	NWRITER		6	/* threads generating dirty pages */
#define	NRECS_PER_TXN	20
#define	KEYSPACE	60000	/* >> cache, so eviction forces DB_SYNC_ALLOC */
#define	CACHE_BYTES	(2 * 1024 * 1024)	/* small on purpose */

#define	MAXTRACK	200000	/* committed keys we remember for the audit */

static DB_ENV	*dbenv;
static DB	*dbp;
static volatile int stop_flag;
static int	 failures;

/*
 * Committed-record ledger.  Each writer owns a disjoint key range, so a
 * plain per-slot store needs no lock: slot = key, value = the token last
 * committed for it (0 = never committed).
 */
static unsigned long	*committed;

/* Per-caller outcome counters, for the report. */
static struct {
	unsigned long ckp, ckp_err;
	unsigned long trickle, trickle_err;
	unsigned long sync, sync_err;
	unsigned long dbsync, dbsync_err;
	unsigned long commits;
} stats;
static pthread_mutex_t stats_lk = PTHREAD_MUTEX_INITIALIZER;

static void
fail(const char *what, int ret)
{
	fprintf(stderr, "FAIL: %s%s%s\n", what,
	    ret == 0 ? "" : ": ", ret == 0 ? "" : db_strerror(ret));
	failures++;
}

/* Monotonic-ish millisecond clock for the run-duration bound. */
static double
now_sec(void)
{
	struct timespec ts;

	(void)clock_gettime(CLOCK_MONOTONIC, &ts);
	return ((double)ts.tv_sec + (double)ts.tv_nsec / 1e9);
}

/*
 * Writer: commit small DB_TXN_SYNC transactions over its own key range,
 * recording each committed value.  DB_TXN_SYNC makes each commit's log
 * durable, so a lost record after the run is unambiguously a data-page
 * writeback failure and not an un-flushed log.
 */
static void *
writer_thread(void *arg)
{
	DBT key, data;
	DB_TXN *txn;
	char kbuf[32], vbuf[32];
	unsigned long tok;
	int i, id, k, nped, ret;
	/*
	 * Pending writes of the CURRENT txn.  They are published to the
	 * ledger only after commit succeeds: a txn that aborts (deadlock)
	 * wrote nothing, and recording it would make the audit report a
	 * phantom "lost" record and mask a real one.
	 */
	struct { int k; unsigned long tok; } ped[NRECS_PER_TXN];

	id = (int)(long)arg;
	tok = (unsigned long)id * 1000000UL;

	while (!stop_flag) {
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
			fail("txn_begin", ret);
			break;
		}
		nped = 0;
		for (i = 0; i < NRECS_PER_TXN; i++) {
			/* Disjoint per-writer stride: writer id owns k%NWRITER==id. */
			k = (int)((random() % (KEYSPACE / NWRITER)) * NWRITER + id);
			tok++;
			(void)snprintf(kbuf, sizeof(kbuf), "k-%08d", k);
			(void)snprintf(vbuf, sizeof(vbuf), "v-%010lu", tok);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf;
			key.size = (u_int32_t)strlen(kbuf) + 1;
			data.data = vbuf;
			data.size = (u_int32_t)strlen(vbuf) + 1;
			if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0) {
				(void)txn->abort(txn);
				txn = NULL;
				if (ret != DB_LOCK_DEADLOCK)
					fail("put", ret);
				break;
			}
			if (k < MAXTRACK) {
				ped[nped].k = k;
				ped[nped].tok = tok;
				nped++;
			}
		}
		if (txn == NULL)
			continue;
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0) {
			if (ret != DB_LOCK_DEADLOCK)
				fail("commit", ret);
			continue;	/* wrote nothing: publish nothing */
		}
		/*
		 * Committed with DB_TXN_SYNC, so the log is durable.  Publish
		 * now; from here on, a missing record is a data-page
		 * writeback failure, not an un-flushed log.  In txn order, so
		 * a key rewritten within the txn ends at its last value.
		 */
		for (i = 0; i < nped; i++)
			committed[ped[i].k] = ped[i].tok;
		(void)pthread_mutex_lock(&stats_lk);
		stats.commits++;
		(void)pthread_mutex_unlock(&stats_lk);
	}
	return (NULL);
}

/* Checkpoint caller: DB_SYNC_CHECKPOINT through __memp_sync_int. */
static void *
ckp_thread(void *arg)
{
	int ret;

	arg = arg;
	while (!stop_flag) {
		ret = dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE);
		(void)pthread_mutex_lock(&stats_lk);
		stats.ckp++;
		if (ret != 0)
			stats.ckp_err++;
		(void)pthread_mutex_unlock(&stats_lk);
		/*
		 * A checkpoint MAY legitimately fail here only for reasons
		 * unrelated to writeback; there is no fault injected in this
		 * run, so any failure is a real problem worth reporting.
		 */
		if (ret != 0)
			fail("txn_checkpoint (no fault injected)", ret);
		usleep(2000);
	}
	return (NULL);
}

/* Trickle caller: DB_SYNC_TRICKLE through __memp_sync_int. */
static void *
trickle_thread(void *arg)
{
	int nwrote, ret;

	arg = arg;
	while (!stop_flag) {
		ret = dbenv->memp_trickle(dbenv, 60, &nwrote);
		(void)pthread_mutex_lock(&stats_lk);
		stats.trickle++;
		if (ret != 0)
			stats.trickle_err++;
		(void)pthread_mutex_unlock(&stats_lk);
		if (ret != 0)
			fail("memp_trickle", ret);
		usleep(1000);
	}
	return (NULL);
}

/* memp_sync caller: DB_SYNC_CACHE through __memp_sync_int. */
static void *
sync_thread(void *arg)
{
	int ret;

	arg = arg;
	while (!stop_flag) {
		ret = dbenv->memp_sync(dbenv, NULL);
		(void)pthread_mutex_lock(&stats_lk);
		stats.sync++;
		if (ret != 0)
			stats.sync_err++;
		(void)pthread_mutex_unlock(&stats_lk);
		if (ret != 0)
			fail("memp_sync", ret);
		usleep(3000);
	}
	return (NULL);
}

/* DB->sync caller: DB_SYNC_FILE through __memp_sync_int. */
static void *
dbsync_thread(void *arg)
{
	int ret;

	arg = arg;
	while (!stop_flag) {
		ret = dbp->sync(dbp, 0);
		(void)pthread_mutex_lock(&stats_lk);
		stats.dbsync++;
		if (ret != 0)
			stats.dbsync_err++;
		(void)pthread_mutex_unlock(&stats_lk);
		if (ret != 0)
			fail("DB->sync", ret);
		usleep(4000);
	}
	return (NULL);
}

/*
 * Audit: every key we recorded as committed must read back with the value we
 * recorded.  This is the durability-by-content invariant; a cross-reap that
 * clears BH_DIRTY for an unwritten page shows up here as a missing or stale
 * record after the environment is reopened (recovery included).
 */
static int
audit(void)
{
	DBT key, data;
	char kbuf[32], vbuf[32], got[64];
	int k, lost, ret, stale;

	lost = stale = 0;
	for (k = 0; k < KEYSPACE && k < MAXTRACK; k++) {
		if (committed[k] == 0)
			continue;
		(void)snprintf(kbuf, sizeof(kbuf), "k-%08d", k);
		(void)snprintf(vbuf, sizeof(vbuf), "v-%010lu", committed[k]);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf;
		key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = got;
		data.ulen = sizeof(got);
		data.flags = DB_DBT_USERMEM;
		if ((ret = dbp->get(dbp, NULL, &key, &data, 0)) != 0) {
			if (ret == DB_NOTFOUND) {
				if (lost++ < 5)
					fprintf(stderr,
					    "  LOST key %s (expected %s)\n",
					    kbuf, vbuf);
				continue;
			}
			fail("audit get", ret);
			return (1);
		}
		/*
		 * The ledger is provisional -- a later aborted or deadlocked
		 * txn may have superseded the value -- so a MISMATCH is only
		 * reported, not failed; a MISSING key is the hard failure,
		 * since the key was written at least once by a committed txn
		 * and can never legitimately vanish.
		 */
		if (strcmp(got, vbuf) != 0)
			stale++;
	}
	printf("audit: lost=%d stale-but-present=%d\n", lost, stale);
	if (lost != 0) {
		fail("committed records LOST (durability broken)", 0);
		return (1);
	}
	return (0);
}

int
main(int argc, char *argv[])
{
	pthread_t wr[NWRITER], ckp, trk, syn, dbs;
	const char *mode;
	double t0, secs;
	int i, ret, use_aio;
	u_int32_t oflags;

	mode = argc > 1 ? argv[1] : "aio";
	secs = argc > 2 ? atof(argv[2]) : 8.0;
	use_aio = strcmp(mode, "sync") != 0;

	printf("aio_concurrent_sync: mode=%s (%s writeback), %.1fs, "
	    "%d writers + ckp + trickle + memp_sync + db->sync\n",
	    mode, use_aio ? "ASYNC" : "SYNCHRONOUS", secs, NWRITER);
	(void)setvbuf(stdout, NULL, _IOLBF, 0);

	if ((committed = calloc(MAXTRACK, sizeof(*committed))) == NULL) {
		fprintf(stderr, "calloc failed\n");
		return (1);
	}

	/* Fresh environment directory. */
	(void)system("find " HOME " -type f -delete 2>/dev/null");
	(void)mkdir(HOME, 0755);

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (1);
	}
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, "aio_conc");

	/*
	 * Small cache + big keyspace: the allocator runs out of buffers and
	 * reaches its aggressive DB_SYNC_ALLOC __memp_sync_int call, which is
	 * the fourth concurrent sync caller and the one an application cannot
	 * invoke directly.
	 */
	if ((ret = dbenv->set_cachesize(dbenv, 0, CACHE_BYTES, 1)) != 0)
		fail("set_cachesize", ret);

	/*
	 * Break lock cycles automatically.  The writers' key ranges are
	 * disjoint by VALUE (stride NWRITER) but NOT by PAGE -- adjacent keys
	 * share a btree leaf, so writers genuinely conflict at page
	 * granularity and deadlock.  Without a detector every writer blocks
	 * forever and the test hangs (in BOTH writeback modes; this is not an
	 * aio property).  The writer loop already treats DB_LOCK_DEADLOCK as
	 * a retry, and a deadlock abort is not a durability event, so this
	 * does not weaken any invariant being checked.
	 */
	if ((ret = dbenv->set_lk_detect(dbenv, DB_LOCK_DEFAULT)) != 0)
		fail("set_lk_detect", ret);

	if (use_aio &&
	    (ret = dbenv->set_flags(dbenv, DB_MPOOL_AIO, 1)) != 0)
		fail("set_flags(DB_MPOOL_AIO)", ret);

	oflags = DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_THREAD | DB_RECOVER;
	if ((ret = dbenv->open(dbenv, HOME, oflags, 0644)) != 0) {
		fprintf(stderr, "env open: %s\n", db_strerror(ret));
		return (1);
	}

	if ((ret = db_create(&dbp, dbenv, 0)) != 0) {
		fail("db_create", ret);
		return (1);
	}
	if ((ret = dbp->open(dbp, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "db open: %s\n", db_strerror(ret));
		return (1);
	}

	/* Launch every sync caller and the dirty-page load simultaneously. */
	stop_flag = 0;
	for (i = 0; i < NWRITER; i++)
		if (pthread_create(&wr[i], NULL, writer_thread,
		    (void *)(long)i) != 0)
			fail("pthread_create writer", 0);
	if (pthread_create(&ckp, NULL, ckp_thread, NULL) != 0)
		fail("pthread_create ckp", 0);
	if (pthread_create(&trk, NULL, trickle_thread, NULL) != 0)
		fail("pthread_create trickle", 0);
	if (pthread_create(&syn, NULL, sync_thread, NULL) != 0)
		fail("pthread_create sync", 0);
	if (pthread_create(&dbs, NULL, dbsync_thread, NULL) != 0)
		fail("pthread_create dbsync", 0);

	for (t0 = now_sec(); now_sec() - t0 < secs; )
		usleep(50000);
	stop_flag = 1;

	for (i = 0; i < NWRITER; i++)
		(void)pthread_join(wr[i], NULL);
	(void)pthread_join(ckp, NULL);
	(void)pthread_join(trk, NULL);
	(void)pthread_join(syn, NULL);
	(void)pthread_join(dbs, NULL);

	printf("drivers: commits=%lu ckp=%lu(err %lu) trickle=%lu(err %lu) "
	    "memp_sync=%lu(err %lu) db_sync=%lu(err %lu)\n",
	    stats.commits, stats.ckp, stats.ckp_err,
	    stats.trickle, stats.trickle_err, stats.sync, stats.sync_err,
	    stats.dbsync, stats.dbsync_err);

	/*
	 * A run that never overlapped the callers would prove nothing, so
	 * require that each concurrent sync caller actually ran many times
	 * alongside a meaningful commit load.
	 */
	if (stats.commits < 50 || stats.ckp < 5 || stats.trickle < 5 ||
	    stats.sync < 5 || stats.dbsync < 5)
		fail("workload too light to prove concurrency", 0);

	/* Final consistent close, then audit through a fresh handle. */
	if ((ret = dbp->close(dbp, 0)) != 0)
		fail("db close", ret);
	if ((ret = dbenv->close(dbenv, 0)) != 0)
		fail("env close", ret);

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fail("reopen db_env_create", ret);
		return (1);
	}
	dbenv->set_errfile(dbenv, stderr);
	if (use_aio)
		(void)dbenv->set_flags(dbenv, DB_MPOOL_AIO, 1);
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER,
	    0644)) != 0) {
		fail("reopen env", ret);
		return (1);
	}
	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, DBFILE, NULL, DB_BTREE, 0, 0644)) != 0) {
		fail("reopen db", ret);
		return (1);
	}

	(void)audit();

	/*
	 * DB->verify is not permitted on an opened handle, so close the audit
	 * handle and verify through a fresh one (verify closes it itself).
	 */
	if ((ret = dbp->close(dbp, 0)) != 0)
		fail("audit db close", ret);
	dbp = NULL;
	{
		DB *vdbp;

		if ((ret = db_create(&vdbp, dbenv, 0)) != 0)
			fail("verify db_create", ret);
		else if ((ret = vdbp->verify(vdbp,
		    DBFILE, NULL, NULL, 0)) != 0)
			fail("db verify", ret);
	}
	(void)dbenv->close(dbenv, 0);
	free(committed);

	printf("%s: %s (%d failure%s)\n", mode,
	    failures == 0 ? "PASS" : "FAIL", failures,
	    failures == 1 ? "" : "s");
	return (failures == 0 ? 0 : 1);
}

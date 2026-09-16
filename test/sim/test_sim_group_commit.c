/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_group_commit.c --
 *	The durability gate for GROUP COMMIT, i.e. for the leader/followers
 *	batching of commit-record flushes in __log_flush_int.
 *
 *	The existing capstone (test_sim_crash_recover) proves the durability
 *	contract for a SINGLE committer.  A single committer never enters the
 *	group-commit path at all: it finds lp->in_flush == 0, flushes for
 *	itself, and returns.  The follower path -- where a committer parks on
 *	lp->commits, is woken by another thread's fsync, and returns success
 *	on the strength of that fsync -- only runs when commits overlap.  That
 *	path is where an acknowledged-but-not-durable commit could come from,
 *	so it needs a crash test of its own with real concurrency.
 *
 *	What this proves: with NTHREAD threads committing DB_TXN_SYNC
 *	concurrently, EVERY transaction whose commit() returned success is
 *	present after crash recovery.  Not "most", and not "the ones we
 *	happened to look at" -- each thread records a commit in a shared
 *	acknowledged-set ONLY after its commit() returned 0, the crash drops
 *	every byte that was written but never fsync'd, and the parent then
 *	requires every record in that set to be readable after recovery.
 *
 *	Why the acknowledged-set is trustworthy across the crash: it lives in
 *	an mmap'd MAP_SHARED file, so the parent reads exactly the entries the
 *	child had recorded at the moment it died, with no dependence on the
 *	child flushing anything.  A commit is recorded after the ack, so if
 *	anything the set UNDER-states what was acknowledged; it can never
 *	claim an ack that did not happen.  Recording after the ack is also
 *	what makes the test one-sided in the safe direction: the failure it
 *	can produce is a real lost commit, never a spurious one.
 *
 *	The write-back model is what gives the test teeth.  The sim writes to
 *	a real file, so bytes reach the file on pwrite whether or not anyone
 *	fsync'd.  At the crash boundary __db_sim_wb_crash() truncates each
 *	tracked file back to its durable frontier (its last fsync), so a
 *	commit that was acked without being fsync'd is genuinely gone --
 *	exactly as a power loss would lose it.  Without that, a test like
 *	this cannot fail no matter how broken the flush protocol is.
 *
 *	Crash points: the child crashes after the Nth acknowledged commit,
 *	swept over many values of N and many seeds, so the crash lands at
 *	many different points in the interleaving of leaders and followers --
 *	including mid-flush, with followers parked on the queue.
 *
 *	PLANTED BUG (DB_DST_INJECT_BUG=1, NODURABLE): __log_flush_int skips
 *	the log fsync but still acks.  The durable frontier then never
 *	advances, the crash drops the acked commits, and this test must
 *	report them lost.  If it does not, the test is not actually watching
 *	the thing it claims to watch, and the injected build fails it as a
 *	coverage hole.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_group_commit && ./test_sim_group_commit [seed] [ncommit]
 */

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"
#include "sim_inject.h"

#define	HOME	"TESTDIR_sim_group_commit"
#define	DBFILE	"gc.db"
#define	ACKFILE	HOME "/acked.map"

#define	NTHREAD	8		/* concurrent committers */
#define	MAXACK	4096		/* capacity of the acknowledged-set */

/*
 * The acknowledged-set, in shared memory so it survives the child's death.
 *
 * A slot is claimed with an atomic fetch-add on n, and only AFTER commit()
 * has returned 0.  count is the number of slots published.  The parent reads
 * the first count entries.
 */
struct ackset {
	unsigned int n;			/* slots claimed (atomic) */
	unsigned int crash_after;	/* crash once this many are acked */
	int	     key[MAXACK];	/* the acked record ids */
};

static struct ackset *acks;
static DB_ENV *env;
static DB *db;
static volatile int stop;

/*
 * Deterministic value for record i.  The key is the id so the parent can look
 * it up; the value is derived from the id alone (NOT from the RNG stream),
 * because with several threads racing there is no deterministic order of
 * draws, and the parent must be able to recompute the expected value for any
 * id it finds in the acknowledged-set.
 */
static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	(void)snprintf(kbuf, 32, "key-%08d", i);
	(void)snprintf(vbuf, 32, "val-%08d-%08x", i, (unsigned)(i * 2654435761u));
}

/*
 * Record that record `id` was acknowledged durable.  Called only after
 * commit() returned 0.  Returns the 1-based ack ordinal.
 */
static unsigned int
ack(id)
	int id;
{
	unsigned int slot;

	slot = __sync_fetch_and_add(&acks->n, 1);
	if (slot < MAXACK) {
		acks->key[slot] = id;
		/*
		 * Publish the key before any later reader (the parent, after
		 * the crash) can conclude from n that the slot is valid.
		 */
		__sync_synchronize();
	}
	return (slot + 1);
}

struct targ {
	pthread_t th;
	int	  id;			/* thread index */
};

static void *
committer(arg)
	void *arg;
{
	struct targ *t = arg;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned int nacked;
	int i, id, ret;

	/*
	 * Each thread owns a disjoint slice of the id space (id = i*NTHREAD +
	 * tid), so two threads never write the same key and the test measures
	 * durability rather than lock contention.
	 */
	for (i = 0; !stop; i++) {
		id = i * NTHREAD + t->id;
		mkrec(id, kbuf, vbuf);

		if ((ret = env->txn_begin(env, NULL, &txn, DB_TXN_SYNC)) != 0)
			break;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			break;
		}
		/*
		 * THE contract: after this returns 0, this record must be on
		 * stable storage.  Everything after this point may be lost by
		 * the crash except this record.
		 */
		if ((ret = txn->commit(txn, 0)) != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			break;
		}

		nacked = ack(id);

		/*
		 * CRASH (power loss) once enough commits have been
		 * acknowledged: drop every byte written but not fsync'd, then
		 * die abruptly -- no clean close, no checkpoint, other
		 * committers still mid-flight (some of them parked on
		 * lp->commits as followers, which is the state this test
		 * exists to crash in).
		 */
		if (nacked >= acks->crash_after) {
			__db_sim_wb_crash();
			fflush(NULL);
			_exit(42);
			/* NOTREACHED */
		}
	}
	return (NULL);
}

static int
run_child(seed, crash_after)
	uint64_t seed;
	unsigned int crash_after;
{
	struct targ t[NTHREAD];
	int i, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	acks->crash_after = crash_after;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0664)) != 0)
		return (ret);
	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0664)) != 0)
		return (ret);

	for (i = 0; i < NTHREAD; i++) {
		t[i].id = i;
		if ((ret = pthread_create(&t[i].th, NULL, committer, &t[i]))
		    != 0)
			return (ret);
	}
	/*
	 * A committer thread crashes the process; this join only returns if
	 * the workload gave up early (a setup error), which the parent
	 * reports as a failed run rather than a pass.
	 */
	for (i = 0; i < NTHREAD; i++)
		(void)pthread_join(t[i].th, NULL);
	return (1);
}

/*
 * Recover, then require every acknowledged record to be present and correct.
 * Returns 0 if the durability contract held.
 */
static int
verify_after_recovery(nchecked, missing_out)
	unsigned int *nchecked;
	int *missing_out;
{
	DB *vdb;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned int i, n;
	int ret, missing = 0, mismatch = 0;

	*nchecked = 0;
	*missing_out = 0;

	/* ALWAYS recover before touching the tree. */
	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0664))
	    != 0) {
		fprintf(stderr, "recover open failed: %s\n", db_strerror(ret));
		return (ret);
	}
	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "reopen failed: %s\n", db_strerror(ret));
		return (ret);
	}

	n = acks->n > MAXACK ? MAXACK : acks->n;
	for (i = 0; i < n; i++) {
		mkrec(acks->key[i], kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "LOST ACKED COMMIT %s: %s\n",
			    kbuf, db_strerror(ret));
			missing++;
		} else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0) {
			fprintf(stderr, "WRONG value for acked commit %s\n",
			    kbuf);
			mismatch++;
		}
	}
	*nchecked = n;
	(void)db->close(db, 0);

	/* Verify a FRESH handle (verify needs the db closed). */
	if ((ret = db_create(&vdb, env, 0)) != 0)
		return (ret);
	if ((ret = vdb->verify(vdb, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "db->verify FAILED: %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (ret);
	}
	(void)env->close(env, 0);

	*missing_out = missing;
	return (missing != 0 || mismatch != 0 ? 1 : 0);
}

/* Map the shared acknowledged-set.  Zeroed, MAP_SHARED, survives the child. */
static int
map_ackset()
{
	int fd;

	if ((fd = open(ACKFILE, O_RDWR | O_CREAT | O_TRUNC, 0664)) < 0) {
		perror("open ackfile");
		return (1);
	}
	if (ftruncate(fd, (off_t)sizeof(struct ackset)) != 0) {
		perror("ftruncate ackfile");
		(void)close(fd);
		return (1);
	}
	acks = mmap(NULL, sizeof(struct ackset), PROT_READ | PROT_WRITE,
	    MAP_SHARED, fd, 0);
	(void)close(fd);
	if (acks == MAP_FAILED) {
		perror("mmap ackfile");
		return (1);
	}
	memset(acks, 0, sizeof(*acks));
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xDB5EEDull;
	unsigned int crash_after = argc > 2 ?
	    (unsigned int)atoi(argv[2]) : 64;
	unsigned int nchecked;
	pid_t pid;
	int status, ret, missing;
	char cmd[256];

	if (crash_after < 1)
		crash_after = 1;
	if (crash_after > MAXACK)
		crash_after = MAXACK;

	/*
	 * Fresh env dir each run.  find -delete rather than a recursive
	 * remove: same effect, and it cannot be pointed at the wrong tree by
	 * an empty variable.
	 */
	(void)snprintf(cmd, sizeof(cmd),
	    "mkdir -p %s && find %s -mindepth 1 -delete", HOME, HOME);
	(void)system(cmd);

	if (map_ackset() != 0)
		return (EXIT_FAILURE);

	if ((pid = fork()) < 0) {
		perror("fork");
		return (EXIT_FAILURE);
	}
	if (pid == 0)
		exit(run_child(seed, crash_after) == 0 ? 0 : 1);

	if (waitpid(pid, &status, 0) < 0) {
		perror("waitpid");
		return (EXIT_FAILURE);
	}
	if (!(WIFEXITED(status) && WEXITSTATUS(status) == 42)) {
		fprintf(stderr, "child did not reach the crash point "
		    "(status %d) -- setup failed\n", status);
		return (EXIT_FAILURE);
	}
	if (acks->n == 0) {
		fprintf(stderr, "no commits were acknowledged -- the test "
		    "did not exercise anything\n");
		return (EXIT_FAILURE);
	}

	ret = verify_after_recovery(&nchecked, &missing);

#if DB_DST_BUG(1)
	/*
	 * NODURABLE invariant: with the fsync skipped, at least one
	 * acknowledged commit MUST be lost.  If everything survived, this
	 * test is not watching the flush protocol at all -- report the
	 * coverage hole rather than a pass.
	 */
	if (ret == 0 && missing == 0) {
		fprintf(stderr, "test_sim_group_commit: DID NOT CATCH "
		    "NODURABLE -- all %u acked commits survived despite the "
		    "skipped fsync (seed 0x%llx crash_after=%u)\n",
		    nchecked, (unsigned long long)seed, crash_after);
		return (EXIT_FAILURE);
	}
	printf("test_sim_group_commit: CAUGHT NODURABLE -- %d of %u acked "
	    "commits lost because the log fsync was skipped "
	    "(seed 0x%llx crash_after=%u)\n",
	    missing, nchecked, (unsigned long long)seed, crash_after);
	return (EXIT_SUCCESS);
#else
	if (ret == 0) {
		printf("test_sim_group_commit: PASS -- all %u acked commits "
		    "survived the crash across %d concurrent committers, DB "
		    "verifies clean (seed 0x%llx crash_after=%u)\n",
		    nchecked, NTHREAD, (unsigned long long)seed, crash_after);
		return (EXIT_SUCCESS);
	}
	fprintf(stderr, "test_sim_group_commit: FAIL -- %d of %u acked "
	    "commits did not survive (seed 0x%llx crash_after=%u)\n",
	    missing, nchecked, (unsigned long long)seed, crash_after);
	return (EXIT_FAILURE);
#endif
}

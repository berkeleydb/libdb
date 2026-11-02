/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_gc_fault.c --
 *	Adversarial correctness of GROUP COMMIT's flush-path error handling:
 *	a leader's fsync fails (EIO), or the leader stalls, while followers
 *	are parked on lp->commits.  The claim under test (review packet
 *	B/mp-log-os.md 2.1 and the code in __log_flush_int): the leader's
 *	error reaches every follower -- NO follower may return success for a
 *	flush that failed, and no follower may return success while the
 *	leader's fsync has not yet completed.
 *
 *	How the engine is supposed to behave, read from __log_flush_int:
 *	  - On fsync failure the leader unwinds WITHOUT advancing lp->s_lsn
 *	    (the `lp->s_lsn = f_lsn' assignment is PAST the fsync), so at the
 *	    `done:' wake pass LOG_COMPARE(&lp->s_lsn, &commit->lsn) > 0 is
 *	    FALSE for every follower whose record needed that flush.  The
 *	    first such follower is handed the DB_COMMIT_FLUSH baton (flushes
 *	    for itself on waking); the rest stay queued.  None is woken as a
 *	    success.  A follower woken without coverage re-tests
 *	    ALREADY_FLUSHED and, finding its LSN NOT durable, flushes for
 *	    itself rather than returning 0.
 *	  - The leader's own __log_flush_commit sees ret != 0 and (non-master)
 *	    force-aborts its commit record; __txn_commit then returns the
 *	    error, so the leader's own commit() returns non-zero.
 *
 *	This test turns that into an assertion by combining two mechanisms:
 *	  (a) a user fsync hook (db_env_set_func_fsync) that, on a chosen log
 *	      fsync, returns EIO (mode "eio") or sleeps then succeeds (mode
 *	      "stall"); and
 *	  (b) the DST write-back crash model: a fsync that returns EIO does
 *	      NOT promote the file's durable frontier (os_fsync.c only
 *	      promotes when ret == 0), so __db_sim_wb_crash() will drop every
 *	      byte that was written but whose fsync failed.
 *	Each thread records a commit in a MAP_SHARED acknowledged-set ONLY
 *	after commit() returned 0.  After the crash the parent recovers and
 *	requires every acked record present.  If a follower wrongly returned
 *	0 for the EIO'd flush, that record is acked-but-not-durable, the crash
 *	drops it, and recovery loses it -- a FAIL, which is exactly the broken
 *	group-commit error-propagation this test exists to catch.
 *
 *	EIO (req 4): the invariant is one-sided and strong -- every acked
 *	commit must survive, AND the EIO must actually have fired (else the
 *	test proves nothing).
 *
 *	STALL (req 5): the fsync hook blocks for a bounded interval on a
 *	chosen flush while followers wait, setting a shared "leader in fsync"
 *	flag for the duration.  A follower that returns 0 while that flag is
 *	set returned success before the fsync it depends on completed -- a
 *	FAIL.  The whole run is timeout-guarded by the runner, so a follower
 *	that hangs forever shows up as a timeout, not a wedged machine.  No
 *	crash is taken in stall mode; after the stall every commit is durable
 *	and must survive a clean reopen.
 *
 *	PLANTED BUG (DB_DST_INJECT_BUG=1, NODURABLE): with the fsync skipped
 *	but the commit acked, the EIO mode must lose acked commits -- the
 *	control that proves the test watches the flush path.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_gc_fault
 *	    ./test_sim_gc_fault eio   [seed] [crash_after]
 *	    ./test_sim_gc_fault stall [seed]
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
#include <time.h>
#include <unistd.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"
#include "sim_inject.h"

#define	HOME	"TESTDIR_sim_gc_fault"
#define	DBFILE	"gcf.db"
#define	ACKFILE	HOME "/acked.map"

#define	NTHREAD	8
#define	MAXACK	8192

/* Shared state, in mmap'd memory so it survives the child's death. */
struct ackset {
	unsigned int n;			/* acked slots claimed (atomic) */
	unsigned int crash_after;	/* eio mode: crash after this many acks */
	volatile int leader_in_fsync;	/* stall mode: a leader is blocked in fsync */
	volatile int stall_ack_during;	/* stall mode: an ack happened while blocked */
	/* fsync-fault coordination, shared so the PARENT can verify it fired. */
	int	     mode_eio;		/* 1 = EIO, 0 = stall */
	unsigned     fault_after;	/* fire on this post-arm log fsync */
	volatile int armed;		/* 1 once the workload has opened the env */
	volatile unsigned fsync_n;	/* post-arm log fsyncs counted */
	volatile int fault_fired;	/* the fault actually fired */
	int	     key[MAXACK];
};

static struct ackset *acks;
static DB_ENV *env;
static DB *db;
static volatile int stop;
static int	   g_log_fd = -1;	/* fd of the log file (first synced post-arm) */

/*
 * The fsync hook.  db_env_set_func_fsync installs this as DB_GLOBAL(j_fsync);
 * os_fsync.c calls it with the raw fd and treats a non-zero return as the
 * sync error.  We perturb only the LOG file's fsync, and only AFTER the
 * workload has armed the fault (so recovery/creation fsyncs during env open
 * are never faulted).  Every other fsync is a real fsync.
 */
static int
fsync_hook(fd)
	int fd;
{
	unsigned n;

	/* Not armed yet (env open / recovery): pass every fsync through. */
	if (acks == NULL || !acks->armed)
		return (fsync(fd) == 0 ? 0 : errno);

	/*
	 * Learn the log fd lazily: the first fd fsync'd AFTER arming is a
	 * committing thread's log flush.  (This workload takes no checkpoint
	 * during the measured window, so no data-file fsync intervenes.)
	 */
	if (g_log_fd == -1)
		g_log_fd = fd;

	if (fd == g_log_fd) {
		n = __sync_add_and_fetch(&acks->fsync_n, 1);
		/*
		 * EIO mode: once we reach the fault point, this and EVERY
		 * later log fsync fails -- the disk has died.  No durable
		 * frontier can advance past the fault, so wb_crash drops every
		 * record that was acked on the strength of a flush that never
		 * actually synced.  A committer that still returns success
		 * after this point is the defect this test hunts.
		 */
		if (acks->mode_eio && n >= acks->fault_after) {
			acks->fault_fired = 1;
			errno = EIO;
			return (EIO);
		}
		if (!acks->mode_eio && n == acks->fault_after) {
			/*
			 * Stall: hold the leader in fsync for a bounded
			 * interval with the flag raised, so any follower that
			 * returns 0 during the window is a defect.
			 */
			acks->fault_fired = 1;
			acks->leader_in_fsync = 1;
			__sync_synchronize();
			usleep(300000);		/* 300 ms */
			__sync_synchronize();
			acks->leader_in_fsync = 0;
			/* fall through to a real fsync below */
		}
	}
	/* Perform the real fsync. */
	return (fsync(fd) == 0 ? 0 : errno);
}

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	(void)snprintf(kbuf, 32, "key-%08d", i);
	(void)snprintf(vbuf, 32, "val-%08d-%08x", i, (unsigned)(i * 2654435761u));
}

static unsigned int
ack(id)
	int id;
{
	unsigned int slot;

	/*
	 * STALL invariant check, done at the ack (which is strictly AFTER
	 * commit() returned 0): if the leader is in its stalled fsync right
	 * now, this committer returned success while the flush it depends on
	 * has not completed.  Record it as a violation.
	 */
	if (!acks->mode_eio && acks->leader_in_fsync)
		acks->stall_ack_during = 1;

	slot = __sync_fetch_and_add(&acks->n, 1);
	if (slot < MAXACK) {
		acks->key[slot] = id;
		__sync_synchronize();
	}
	return (slot + 1);
}

struct targ { pthread_t th; int id; };

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
		ret = txn->commit(txn, 0);
		if (ret != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			/*
			 * A failed commit is the CORRECT outcome once the disk
			 * has died: the error reached the caller and the record
			 * is NOT acked.  Crash now (EIO mode) so wb_crash drops
			 * the un-synced tail and the parent checks that every
			 * ALREADY-acked commit still survives -- i.e. no earlier
			 * follower returned success for a flush that never synced.
			 */
			if (acks->mode_eio && acks->fault_fired) {
				__db_sim_wb_crash();
				fflush(NULL);
				_exit(42);
			}
			break;
		}

		nacked = ack(id);

		/*
		 * EIO mode: as soon as the persistent fsync failure has begun,
		 * crash.  Any record acked at or after this point that was
		 * covered only by the failing flush is dropped by wb_crash and
		 * must be absent after recovery -- if it is present-and-acked
		 * the group-commit error path let a follower lie.  We crash
		 * right after an ack so the acked set is published first.
		 */
		if (acks->mode_eio &&
		    (acks->fault_fired || nacked >= acks->crash_after)) {
			__db_sim_wb_crash();
			fflush(NULL);
			_exit(42);
		}
		/*
		 * Stall mode: run a bounded number of commits, enough that the
		 * fault_after fsync happens with followers parked, then stop.
		 */
		if (!acks->mode_eio && nacked >= 400)
			stop = 1;
	}
	return (NULL);
}

static int
open_env(flags)
	u_int32_t flags;
{
	int ret;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, flags, 0664)) != 0)
		return (ret);
	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	return (db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0664));
}

static int
run_child(seed)
	uint64_t seed;
{
	struct targ t[NTHREAD];
	int i, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = open_env(DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD)) != 0) {
		fprintf(stderr, "open: %s\n", db_strerror(ret));
		return (ret);
	}

	/*
	 * Arm the fault only now: the env is open and the next log fsync will
	 * be a committer's flush, never a creation/recovery fsync.
	 */
	acks->armed = 1;
	__sync_synchronize();

	for (i = 0; i < NTHREAD; i++) {
		t[i].id = i;
		if ((ret = pthread_create(&t[i].th, NULL, committer, &t[i]))
		    != 0)
			return (ret);
	}
	for (i = 0; i < NTHREAD; i++)
		(void)pthread_join(t[i].th, NULL);

	/* Stall mode reaches here (no crash); close cleanly. */
	(void)db->close(db, 0);
	(void)env->close(env, 0);
	return (0);
}

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

	*nchecked = 0; *missing_out = 0;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0664))
	    != 0) {
		fprintf(stderr, "recover open: %s\n", db_strerror(ret));
		return (ret);
	}
	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "reopen: %s\n", db_strerror(ret));
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
			fprintf(stderr, "WRONG value for %s\n", kbuf);
			mismatch++;
		}
	}
	*nchecked = n;
	(void)db->close(db, 0);

	if ((ret = db_create(&vdb, env, 0)) != 0)
		return (ret);
	if ((ret = vdb->verify(vdb, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "verify: %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (ret);
	}
	(void)env->close(env, 0);
	*missing_out = missing;
	return (missing != 0 || mismatch != 0 ? 1 : 0);
}

static int
map_ackset()
{
	int fd;

	if ((fd = open(ACKFILE, O_RDWR | O_CREAT | O_TRUNC, 0664)) < 0) {
		perror("open ackfile"); return (1);
	}
	if (ftruncate(fd, (off_t)sizeof(struct ackset)) != 0) {
		perror("ftruncate"); (void)close(fd); return (1);
	}
	acks = mmap(NULL, sizeof(struct ackset), PROT_READ | PROT_WRITE,
	    MAP_SHARED, fd, 0);
	(void)close(fd);
	if (acks == MAP_FAILED) { perror("mmap"); return (1); }
	memset(acks, 0, sizeof(*acks));
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	const char *modearg = argc > 1 ? argv[1] : "eio";
	uint64_t seed = argc > 2 ? strtoull(argv[2], NULL, 0) : 0xFA017ull;
	unsigned int crash_after = argc > 3 ? (unsigned)atoi(argv[3]) : 48;
	unsigned int nchecked;
	pid_t pid;
	int status, ret, missing, mode_eio;
	char cmd[256];

	mode_eio = strcmp(modearg, "eio") == 0;
	if (crash_after < 1) crash_after = 1;
	if (crash_after > MAXACK) crash_after = MAXACK;

	(void)snprintf(cmd, sizeof(cmd),
	    "mkdir -p %s && find %s -mindepth 1 -delete", HOME, HOME);
	(void)system(cmd);

	if (map_ackset() != 0)
		return (EXIT_FAILURE);
	acks->crash_after = crash_after;
	acks->mode_eio = mode_eio;
	/*
	 * The fault fires on a middle post-arm fsync so a batch of followers is
	 * parked behind it.  Derive it from the seed within a small range so
	 * repeated seeds land the fault at different points in the flush stream.
	 */
	acks->fault_after = (unsigned)(16 + (seed % 16));

	/* Install the fsync hook BEFORE any env is created. */
	if ((ret = db_env_set_func_fsync(fsync_hook)) != 0) {
		fprintf(stderr, "set_func_fsync: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}

	if ((pid = fork()) < 0) { perror("fork"); return (EXIT_FAILURE); }
	if (pid == 0)
		exit(run_child(seed) == 0 ? (mode_eio ? 1 : 0) : 1);

	if (waitpid(pid, &status, 0) < 0) {
		perror("waitpid"); return (EXIT_FAILURE);
	}

	if (mode_eio) {
		if (!(WIFEXITED(status) && WEXITSTATUS(status) == 42)) {
			fprintf(stderr, "eio: child did not reach the crash "
			    "point (status %d) -- the EIO may have stopped "
			    "every committer before crash_after; raising "
			    "fault_after would help\n", status);
			/*
			 * Not reaching the crash is a setup problem, not a
			 * durability result.  But we still must have fired the
			 * fault and seen at least one failed commit.
			 */
		}
		if (!acks->fault_fired || acks->fsync_n < acks->fault_after) {
			fprintf(stderr, "eio: the EIO fsync never fired "
			    "(saw %u fsyncs, needed %u, fired=%d) -- test proves "
			    "nothing\n", acks->fsync_n, acks->fault_after,
			    acks->fault_fired);
			return (EXIT_FAILURE);
		}
		if (acks->n == 0) {
			fprintf(stderr, "eio: no commit was acked\n");
			return (EXIT_FAILURE);
		}

		/* Disarm so recovery's own fsyncs are not faulted. */
		acks->armed = 0;
		ret = verify_after_recovery(&nchecked, &missing);
#if DB_DST_BUG(1)
		if (ret == 0 && missing == 0) {
			fprintf(stderr, "test_sim_gc_fault[eio]: DID NOT CATCH "
			    "NODURABLE -- all %u acked survived the skipped "
			    "fsync\n", nchecked);
			return (EXIT_FAILURE);
		}
		printf("test_sim_gc_fault[eio]: CAUGHT NODURABLE -- %d of %u "
		    "acked lost\n", missing, nchecked);
		return (EXIT_SUCCESS);
#else
		if (ret == 0) {
			printf("test_sim_gc_fault[eio]: PASS -- EIO fired on "
			    "fsync #%u, %u acked commits ALL survived crash "
			    "recovery; no follower returned success for the "
			    "failed flush (seed 0x%llx)\n",
			    acks->fault_after, nchecked,
			    (unsigned long long)seed);
			return (EXIT_SUCCESS);
		}
		fprintf(stderr, "test_sim_gc_fault[eio]: FAIL -- %d of %u "
		    "acked commits LOST: a follower returned success for a "
		    "flush the leader's EIO never made durable (seed 0x%llx)\n",
		    missing, nchecked, (unsigned long long)seed);
		return (EXIT_FAILURE);
#endif
	} else {
		/* STALL mode: no crash; child exits 0. */
		if (!(WIFEXITED(status) && WEXITSTATUS(status) == 0)) {
			fprintf(stderr, "stall: child failed (status %d)\n",
			    status);
			return (EXIT_FAILURE);
		}
		if (!acks->fault_fired || acks->fsync_n < acks->fault_after) {
			fprintf(stderr, "stall: the stalled fsync never fired "
			    "(saw %u, needed %u, fired=%d) -- raise the "
			    "workload\n", acks->fsync_n, acks->fault_after,
			    acks->fault_fired);
			return (EXIT_FAILURE);
		}
		if (acks->stall_ack_during) {
			fprintf(stderr, "test_sim_gc_fault[stall]: FAIL -- a "
			    "follower returned commit success while the "
			    "leader was still blocked in its fsync (seed "
			    "0x%llx)\n", (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		printf("test_sim_gc_fault[stall]: PASS -- leader stalled 300ms "
		    "in fsync on #%u, %u followers all waited for the fsync to "
		    "complete before returning success, none hung (seed "
		    "0x%llx)\n", acks->fault_after, acks->n,
		    (unsigned long long)seed);
		return (EXIT_SUCCESS);
	}
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 *
 * test_mutex_mpstress -- multi-PROCESS mutual-exclusion / suspend-path check
 * for the hybrid mutex, through the public DB_ENV->mutex_* API.
 *
 *   test_mutex_mpstress HOME NPROC SECONDS SPINS
 *   test_mutex_mpstress HOME worker SPINS        (internal: a re-exec'd worker)
 *
 * Why a separate test from test_mutex_stress: that one is threads only.  On
 * macOS a PTHREAD_PROCESS_SHARED condition variable binds to the virtual
 * ADDRESS of the mutex it is first waited with, and libdb maps the mutex
 * region at a different address in every process, so a second process's
 * pthread_cond_wait() on the same shared mutex returns EINVAL and libdb PANICs
 * (DB_RUNRECOVERY, then a hang).  A threads-only test never contends across
 * processes, so it never sees this.  This test forks NPROC INDEPENDENT
 * processes (fork + exec, each with a fresh libpthread and its own region
 * mapping) that all contend on ONE db_mutex_t.
 *
 * The leader opens a shared (NOT DB_PRIVATE) environment, allocates one mutex,
 * publishes its index in a small mmap'd coordination file, and re-exec's the
 * workers.  Every process -- leader included -- then repeatedly takes that one
 * mutex and does a non-atomic read-modify-write of a shared counter plus an
 * "occupant" check.  Low SPINS drives contended acquires into
 * __db_hybrid_mutex_suspend, the condvar path that fails on macOS.
 *
 * A mutex_lock that returns non-zero (the PANIC) is a hard FAIL; a wedged
 * environment shows up as no progress, caught by the leader's 10s watchdog.
 *
 * Prints one line:  VERDICT mutex_mpstress PASS|FAIL ...
 */
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/wait.h>

#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <db.h>

/* Shared coordination block, mmap'd from a file so re-exec'd workers see it. */
struct coord {
	db_mutex_t mtx;			/* the one contended mutex (region index) */
	int go;				/* leader -> workers: env is ready */
	int quit;			/* leader -> workers: stop now */
	char pad0[64];
	volatile int occupant;		/* who is inside the mutex (plain stores) */
	char pad1[64];
	volatile unsigned long counter;	/* non-atomic RMW inside the mutex */
	char pad2[64];
	volatile unsigned long progress[64];	/* per-process liveness, [0..NPROC) */
	volatile int violation;		/* 1 mutual-exclusion, 2 lock, 3 unlock */
	volatile int lock_failed;	/* a mutex_lock/unlock returned non-zero */
};

#define COORD_LEN ((size_t)sizeof(struct coord))

static struct coord *
map_coord(const char *home, int create)
{
	char path[4096];
	struct coord *c;
	int fd;

	(void)snprintf(path, sizeof(path), "%s/mpstress.coord", home);
	fd = open(path, create ? (O_RDWR | O_CREAT) : O_RDWR, 0644);
	if (fd < 0)
		return (NULL);
	if (create && ftruncate(fd, (off_t)COORD_LEN) != 0) {
		(void)close(fd);
		return (NULL);
	}
	c = mmap(NULL, COORD_LEN, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	(void)close(fd);
	return (c == MAP_FAILED ? NULL : c);
}

/* One process's contention loop against the shared mutex. */
static int
hammer(DB_ENV *dbenv, struct coord *c, int id, int watch, int secs)
{
	unsigned long n = 0;
	time_t end = time(NULL) + (watch ? secs + 5 : secs + 8);

	while (!c->quit && time(NULL) < end) {
		int ret = dbenv->mutex_lock(dbenv, c->mtx);
		if (ret != 0) {
			/*
			 * The macOS bug surfaces here: the hybrid suspend path
			 * PANICs, mutex_lock returns DB_RUNRECOVERY, and the
			 * environment is dead.  Record it for the verdict.
			 */
			c->lock_failed = ret;
			return (ret);
		}
		if (c->occupant != 0)
			c->violation = 1;
		c->occupant = id + 1;
		c->counter = c->counter + 1;
		if (c->occupant != id + 1)
			c->violation = 1;
		c->occupant = 0;
		if ((ret = dbenv->mutex_unlock(dbenv, c->mtx)) != 0) {
			c->lock_failed = ret;
			return (ret);
		}
		c->progress[id] = ++n;
	}
	return (0);
}

/* Re-exec'd worker: open the same env, read the published mutex, hammer it. */
static int
run_worker(const char *home, int spins)
{
	DB_ENV *dbenv;
	struct coord *c;
	int ret, id, i;

	if ((c = map_coord(home, 0)) == NULL) {
		fprintf(stderr, "worker: cannot map coord\n");
		return (2);
	}
	/* Wait for the leader to finish env setup and publish the mutex. */
	for (i = 0; i < 20000 && !c->go; i++)
		usleep(1000);
	if (!c->go) {
		fprintf(stderr, "worker: leader never signalled go\n");
		return (2);
	}
	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (spins > 0 && (ret = dbenv->mutex_set_tas_spins(dbenv,
	    (u_int32_t)spins)) != 0) ||
	    (ret = dbenv->open(dbenv, home,
	    DB_INIT_MPOOL | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "worker: env open: %s\n", db_strerror(ret));
		return (2);
	}
	/* Pick a per-process id slot from the shared counter of joiners. */
	id = 1 + (int)(getpid() % 40);		/* leader is id 0 */

	ret = hammer(dbenv, c, id, 0, 30);
	(void)dbenv->close(dbenv, 0);
	return (ret == 0 ? 0 : 1);
}

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	struct coord *c;
	pid_t kids[64];
	unsigned long last, now;
	int nproc, secs, spins, ret, i, stalled, hung, nkids;
	char self[4096], sspins[32];
	time_t end;

	/* Internal re-exec form: HOME worker SPINS */
	if (argc == 4 && strcmp(argv[2], "worker") == 0)
		return (run_worker(argv[1], atoi(argv[3])));

	if (argc != 5) {
		fprintf(stderr,
		    "usage: %s HOME NPROC SECONDS SPINS\n", argv[0]);
		return (2);
	}
	nproc = atoi(argv[2]);
	secs = atoi(argv[3]);
	spins = atoi(argv[4]);
	if (nproc < 2 || nproc > 40)
		nproc = 2;

	if ((c = map_coord(argv[1], 1)) == NULL) {
		printf("VERDICT mutex_mpstress FAIL cannot map coord file\n");
		return (1);
	}
	memset(c, 0, COORD_LEN);

	/* Leader opens the shared environment and allocates the one mutex. */
	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (spins > 0 && (ret = dbenv->mutex_set_tas_spins(dbenv,
	    (u_int32_t)spins)) != 0) ||
	    (ret = dbenv->open(dbenv, argv[1],
	    DB_CREATE | DB_INIT_MPOOL | DB_THREAD, 0644)) != 0 ||
	    (ret = dbenv->mutex_alloc(dbenv, 0, &c->mtx)) != 0) {
		printf("VERDICT mutex_mpstress FAIL setup %s\n",
		    db_strerror(ret));
		return (1);
	}

	/* Re-exec NPROC-1 independent worker processes. */
	(void)snprintf(sspins, sizeof(sspins), "%d", spins);
	ret = (int)readlink("/proc/self/exe", self, sizeof(self) - 1);
	if (ret > 0)
		self[ret] = '\0';
	else
		(void)snprintf(self, sizeof(self), "%s", argv[0]);
	nkids = 0;
	for (i = 0; i < nproc - 1 && i < (int)(sizeof(kids)/sizeof(kids[0])); i++) {
		pid_t p = fork();
		if (p == 0) {
			execl(self, self, argv[1], "worker", sspins,
			    (char *)NULL);
			execl(argv[0], argv[0], argv[1], "worker", sspins,
			    (char *)NULL);
			fprintf(stderr, "exec worker failed\n");
			_exit(2);
		}
		if (p > 0)
			kids[nkids++] = p;
	}

	c->go = 1;			/* workers may now open and hammer */

	/* Leader also hammers, and runs the stall watchdog. */
	end = time(NULL) + secs;
	last = 0;
	stalled = hung = 0;
	{
		/* Short bursts between watchdog checks. */
		time_t next;
		while (time(NULL) < end && !c->lock_failed) {
			next = time(NULL) + 1;
			while (time(NULL) < next && !c->lock_failed) {
				if (dbenv->mutex_lock(dbenv, c->mtx) != 0) {
					c->lock_failed = 1;
					break;
				}
				if (c->occupant != 0)
					c->violation = 1;
				c->occupant = 1;	/* leader id 0 -> 1 */
				c->counter = c->counter + 1;
				if (c->occupant != 1)
					c->violation = 1;
				c->occupant = 0;
				(void)dbenv->mutex_unlock(dbenv, c->mtx);
				c->progress[0]++;
			}
			for (now = 0, i = 0; i < 64; i++)
				now += c->progress[i];
			if (now == last) {
				if (++stalled >= 10) { hung = 1; break; }
			} else
				stalled = 0;
			last = now;
		}
	}

	c->quit = 1;
	for (i = 0; i < nkids; i++) {
		int st;
		(void)waitpid(kids[i], &st, 0);
		if (WIFEXITED(st) && WEXITSTATUS(st) != 0)
			c->lock_failed = c->lock_failed ? c->lock_failed : -1;
		if (WIFSIGNALED(st))
			c->lock_failed = c->lock_failed ? c->lock_failed : -2;
	}

	if (c->lock_failed) {
		printf("VERDICT mutex_mpstress FAIL mutex_lock failed across "
		    "processes (err=%d) -- macOS condvar EINVAL / PANIC? "
		    "nproc=%d spins=%d counter=%lu\n",
		    c->lock_failed, nproc, spins, c->counter);
		fflush(stdout);
		return (1);
	}
	if (hung) {
		printf("VERDICT mutex_mpstress FAIL hang: no progress for 10s "
		    "(nproc=%d spins=%d total=%lu) -- wedged environment?\n",
		    nproc, spins, last);
		fflush(stdout);
		return (1);
	}
	if (c->violation != 0) {
		printf("VERDICT mutex_mpstress FAIL mutual-exclusion "
		    "violation=%d nproc=%d spins=%d\n",
		    c->violation, nproc, spins);
		return (1);
	}
	printf("VERDICT mutex_mpstress PASS nproc=%d spins=%d secs=%d "
	    "counter=%lu\n", nproc, spins, secs, c->counter);
	(void)dbenv->mutex_free(dbenv, c->mtx);
	(void)dbenv->close(dbenv, 0);
	return (0);
}

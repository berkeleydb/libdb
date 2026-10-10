/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * f5_mpmix -- the MIXED-SWITCH multi-process check for F5.
 *
 * The patch's own comment claims that two processes DISAGREEING about
 * DB_LOCK_REFRESH_LOCK_MUTEX is safe, because "both means leave the same
 * postcondition".  f5_mproc cannot test that claim: it forks children that
 * inherit one environment, and __lock_refresh_lock_mutex() caches its getenv
 * in a process-local static which fork also copies.  So every child in
 * f5_mproc necessarily agrees with the parent.
 *
 * Here each worker is fork + EXECV of this same binary, so its getenv is read
 * fresh in a fresh address space, and the parity of its index decides whether
 * DB_LOCK_REFRESH_LOCK_MUTEX is in its environ.  Even workers run the NEW
 * conditional-lock path; odd workers run the OLD destroy+init path -- on the
 * SAME region, contending on the SAME lock objects, where one process's waiter
 * blocks on the very DB_MUTEX the other process's granter unlocks.
 *
 * Progress lives in a FILE-backed mmap (MAP_SHARED on a real file), not
 * MAP_ANON, because an exec'd child does not inherit anonymous mappings.
 *
 * Judged on forward progress per worker, like f5_mproc: a wedged worker is one
 * that stops advancing for STALL_LIMIT consecutive seconds while the clock
 * keeps running.  rc is not the verdict; the VERDICT line is.
 *
 *   f5_mpmix <nprocs> <secs>            (parent)
 *   f5_mpmix -c <idx> <secs> <progfile> (worker, used internally)
 */
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#include <db.h>

#define	NOBJ		4
#define	MAXP		64
#define	STALL_LIMIT	5

struct shared {
	volatile uint64_t progress[MAXP];
	volatile uint64_t aborts[MAXP];
	volatile int	  refresh[MAXP];	/* what the worker SAW */
};

static const char *home;

static uint64_t
now_ms(void)
{
	struct timeval tv;
	(void)gettimeofday(&tv, NULL);
	return (uint64_t)tv.tv_sec * 1000 + (uint64_t)tv.tv_usec / 1000;
}

static struct shared *
map_prog(const char *path, int create)
{
	struct shared *s;
	int fd;

	fd = open(path, create ? (O_RDWR | O_CREAT | O_TRUNC) : O_RDWR, 0644);
	if (fd < 0) { perror("open progfile"); exit(1); }
	if (create && ftruncate(fd, (off_t)sizeof(*s)) != 0) {
		perror("ftruncate"); exit(1);
	}
	s = mmap(NULL, sizeof(*s), PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (s == MAP_FAILED) { perror("mmap"); exit(1); }
	(void)close(fd);
	return (s);
}

static void
worker_alarm(int sig)
{
	static const char m[] = "  worker wedged (own alarm fired)\n";
	ssize_t n = write(2, m, sizeof(m) - 1);
	(void)sig; (void)n;
	_exit(3);
}

static int
worker(int idx, int secs, const char *progfile)
{
	struct shared *sh = map_prog(progfile, 0);
	DB_ENV *env;
	DBT obj;
	DB_LOCK l1, l2;
	uint32_t key, k1, k2;
	uint64_t rng, deadline;
	u_int32_t locker;
	int ret, round;

	(void)signal(SIGALRM, worker_alarm);
	(void)alarm((unsigned)(secs + 60));

	sh->refresh[idx] = getenv("DB_LOCK_REFRESH_LOCK_MUTEX") != NULL;

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "w%d db_env_create: %s\n", idx,
		    db_strerror(ret));
		return (1);
	}
	env->set_errfile(env, stderr);
	(void)env->set_lk_detect(env, DB_LOCK_YOUNGEST);
	/* ATTACH, do not create: the region must genuinely be shared. */
	if ((ret = env->open(env, home,
	    DB_INIT_LOCK | DB_INIT_MPOOL | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "w%d env open: %s\n", idx, db_strerror(ret));
		return (1);
	}
	if ((ret = env->lock_id(env, &locker)) != 0) {
		fprintf(stderr, "w%d lock_id: %s\n", idx, db_strerror(ret));
		return (1);
	}

	rng = 0x9E3779B97F4A7C15ULL * (uint64_t)(idx + 1) + 31;
	memset(&obj, 0, sizeof(obj));
	obj.size = sizeof(key);
	obj.data = &key;
	deadline = now_ms() + (uint64_t)secs * 1000;

	for (round = 0;; round++) {
		if ((round & 0xFF) == 0 && now_ms() >= deadline)
			break;
		rng ^= rng >> 12; rng ^= rng << 25; rng ^= rng >> 27;
		k1 = (uint32_t)((rng * 0x2545F4914F6CDD1DULL >> 32) % NOBJ);
		rng ^= rng >> 12; rng ^= rng << 25; rng ^= rng >> 27;
		k2 = (uint32_t)((rng * 0x2545F4914F6CDD1DULL >> 32) % NOBJ);
		if (k1 == k2)
			k2 = (k2 + 1) % NOBJ;

		key = k1;
		ret = env->lock_get(env, locker, 0, &obj, DB_LOCK_WRITE, &l1);
		if (ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED) {
			sh->aborts[idx]++; sh->progress[idx]++; continue;
		}
		if (ret != 0) {
			fprintf(stderr, "w%d get1: %s\n", idx,
			    db_strerror(ret));
			return (1);
		}
		key = k2;
		ret = env->lock_get(env, locker, 0, &obj, DB_LOCK_WRITE, &l2);
		if (ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED) {
			sh->aborts[idx]++;
			(void)env->lock_put(env, &l1);
			sh->progress[idx]++;
			continue;
		}
		if (ret != 0) {
			fprintf(stderr, "w%d get2: %s\n", idx,
			    db_strerror(ret));
			(void)env->lock_put(env, &l1);
			return (1);
		}
		(void)env->lock_put(env, &l2);
		(void)env->lock_put(env, &l1);
		sh->progress[idx]++;
	}
	(void)env->lock_id_free(env, locker);
	(void)env->close(env, 0);
	return (0);
}

int
main(int argc, char **argv)
{
	struct shared *sh;
	DB_ENV *env;
	pid_t pid[MAXP];
	uint64_t snap[MAXP], totp = 0, tota = 0, newp = 0, oldp = 0;
	int consec[MAXP];
	char progfile[512], sidx[16], ssecs[16];
	int np, secs, i, s, ret, stalled = 0, blips = 0, bad = 0, status;
	int nnew = 0, nold = 0;

	if ((home = getenv("F5_HOME")) == NULL)
		home = "F5MIXDIR";

	if (argc >= 5 && strcmp(argv[1], "-c") == 0)
		return (worker(atoi(argv[2]), atoi(argv[3]), argv[4]));

	np = argc > 1 ? atoi(argv[1]) : 8;
	secs = argc > 2 ? atoi(argv[2]) : 20;
	if (np < 2) np = 2;
	if (np > MAXP) np = MAXP;

	(void)snprintf(progfile, sizeof(progfile), "%s/.f5mixprog", home);

	/* Parent creates the region the workers will attach to. */
	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (1);
	}
	env->set_errfile(env, stderr);
	(void)env->set_lk_detect(env, DB_LOCK_YOUNGEST);
	(void)env->set_lk_max_lockers(env, 40000);
	(void)env->set_lk_max_objects(env, 40000);
	(void)env->set_lk_max_locks(env, 40000);
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_MPOOL | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "parent env open %s: %s\n", home,
		    db_strerror(ret));
		return (1);
	}

	sh = map_prog(progfile, 1);
	memset(sh, 0, sizeof(*sh));
	memset(consec, 0, sizeof(consec));

	printf("# f5_mpmix nprocs=%d secs=%d home=%s\n", np, secs, home);
	printf("# even idx -> NEW (conditional lock);"
	    " odd idx -> OLD (DB_LOCK_REFRESH_LOCK_MUTEX=1)\n");

	for (i = 0; i < np; i++) {
		(void)snprintf(sidx, sizeof(sidx), "%d", i);
		(void)snprintf(ssecs, sizeof(ssecs), "%d", secs);
		if ((pid[i] = fork()) == 0) {
			/* exec, so getenv is read fresh, not inherited. */
			if (i % 2 == 0)
				(void)unsetenv("DB_LOCK_REFRESH_LOCK_MUTEX");
			else
				(void)setenv("DB_LOCK_REFRESH_LOCK_MUTEX",
				    "1", 1);
			execl(argv[0], argv[0], "-c", sidx, ssecs, progfile,
			    (char *)NULL);
			perror("execl");
			_exit(127);
		}
		if (pid[i] < 0) { perror("fork"); return (1); }
	}

	for (s = 0; s < secs; s++) {
		for (i = 0; i < np; i++)
			snap[i] = sh->progress[i];
		sleep(1);
		if (s < 2)
			continue;	/* exec + attach needs a moment */
		for (i = 0; i < np; i++)
			if (sh->progress[i] == snap[i]) {
				if (++consec[i] == STALL_LIMIT) {
					fprintf(stderr, "  worker %d NO "
					    "progress for %d CONSECUTIVE "
					    "seconds (stuck at %llu, "
					    "refresh=%d) -- wedged\n", i,
					    consec[i],
					    (unsigned long long)snap[i],
					    sh->refresh[i]);
					stalled++;
				}
			} else {
				if (consec[i] != 0) blips++;
				consec[i] = 0;
			}
	}

	for (i = 0; i < np; i++) {
		if (waitpid(pid[i], &status, 0) < 0) {
			perror("waitpid"); bad++; continue;
		}
		if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
			fprintf(stderr, "  worker %d exited badly "
			    "(status=0x%x, refresh=%d)\n", i, status,
			    sh->refresh[i]);
			bad++;
		}
		totp += sh->progress[i];
		tota += sh->aborts[i];
		if (sh->refresh[i]) { nold++; oldp += sh->progress[i]; }
		else { nnew++; newp += sh->progress[i]; }
	}
	(void)env->close(env, 0);

	printf("  workers on NEW path: %d (progress %llu)\n", nnew,
	    (unsigned long long)newp);
	printf("  workers on OLD path: %d (progress %llu)\n", nold,
	    (unsigned long long)oldp);

	/*
	 * Three ways this test can be vacuous, each reported as FAIL rather
	 * than quietly passing: no aborts (the F5 branch never ran), all
	 * workers on one path (not actually mixed), or a worker that never
	 * made any progress at all (never attached).
	 */
	if (tota == 0)
		printf("  VERDICT f5_mpmix FAIL VACUOUS 0 aborts, so the "
		    "non-HELD lock-free path never ran (progress=%llu)\n",
		    (unsigned long long)totp);
	else if (nnew == 0 || nold == 0)
		printf("  VERDICT f5_mpmix FAIL VACUOUS not mixed: "
		    "nnew=%d nold=%d\n", nnew, nold);
	else if (stalled != 0 || bad != 0)
		printf("  VERDICT f5_mpmix FAIL %d wedged worker(s) "
		    "(>=%d consecutive idle seconds), %d bad exit(s); "
		    "progress=%llu aborts=%llu\n", stalled, STALL_LIMIT, bad,
		    (unsigned long long)totp, (unsigned long long)tota);
	else
		printf("  VERDICT f5_mpmix PASS %d NEW-path + %d OLD-path "
		    "processes shared ONE region for %d s, contending on the "
		    "same objects, with no worker idle for %d consecutive "
		    "seconds; progress=%llu aborts=%llu (aborts>0 => "
		    "cross-process block/grant/abort DID occur; %d transient "
		    "idle second(s))\n", nnew, nold, secs, STALL_LIMIT,
		    (unsigned long long)totp, (unsigned long long)tota, blips);

	return ((tota != 0 && nnew != 0 && nold != 0 &&
	    stalled == 0 && bad == 0) ? 0 : 1);
}

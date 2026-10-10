/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * pshared_attach_probe.c -- B3. does a PTHREAD_PROCESS_SHARED mutex initialised by a
 * process that has since EXITED remain lockable by a later process that maps
 * the same FILE?
 *
 * libdb's environment regions are file-backed mmaps (__db.00N).  A loader
 * process creates the region, pthread_mutex_init()s every mutex in it with
 * PTHREAD_PROCESS_SHARED, and exits.  A second process then opens the region
 * and locks those mutexes.  On Linux a pshared mutex is just bytes in the
 * mapping, so this works.  FreeBSD's libthr (since 11.0) stores a pshared
 * mutex as an OFF-PAGE object keyed by the backing VM object, created by
 * _umtx_op(UMTX_SHM_CREAT) at init time.
 *
 * Arms:
 *   A  init in a child that EXITS, lock in the parent (what libdb does)
 *   B  init in a child that stays ALIVE, lock in the parent (control)
 *   C  init and lock in the same process (control)
 *
 *   D  init in a child that EXITS while a third mapping is held throughout
 *
 * Output: one RESULT line per arm with the pthread_mutex_lock return value.
 * Measured on FreeBSD 14.5, kern.ipc.umtx_vnode_persistent=0: A=EINVAL,
 * B/C/D=ok.  With umtx_vnode_persistent=1 all four are ok.  Linux: all ok.
 * Build: cc -O2 -pthread pshared_attach_probe.c -o pa [-DPATH=\"/dir/f\"]
 */
#include <sys/mman.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef PATH
#define	PATH	"/tmp/pshared_attach.bin"
#endif

static pthread_mutex_t *
map_it(int create)
{
	void *p;
	int fd;

	if ((fd = open(PATH, O_RDWR | (create ? O_CREAT | O_TRUNC : 0), 0600)) < 0) {
		perror("open"); exit(2);
	}
	if (create && ftruncate(fd, 4096) != 0) {
		perror("ftruncate"); exit(2);
	}
	p = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (p == MAP_FAILED) {
		perror("mmap"); exit(2);
	}
	(void)close(fd);
	return ((pthread_mutex_t *)p);
}

static int
init_it(pthread_mutex_t *m)
{
	pthread_mutexattr_t a;
	int ret;

	(void)pthread_mutexattr_init(&a);
	(void)pthread_mutexattr_setpshared(&a, PTHREAD_PROCESS_SHARED);
	ret = pthread_mutex_init(m, &a);
	(void)pthread_mutexattr_destroy(&a);
	return (ret);
}

static void
report(const char *arm, pthread_mutex_t *m)
{
	int l, u;

	l = pthread_mutex_lock(m);
	u = (l == 0) ? pthread_mutex_unlock(m) : -1;
	printf("RESULT arm=%s lock=%d(%s) unlock=%d\n",
	    arm, l, l ? strerror(l) : "ok", u);
}

int
main(void)
{
	pthread_mutex_t *m;
	pid_t kid;
	int pfd[2], st;
	char c;

	/* A: initialiser exits before the attacher locks. */
	if ((kid = fork()) == 0) {
		m = map_it(1);
		if (init_it(m) != 0)
			_exit(3);
		_exit(0);
	}
	(void)waitpid(kid, &st, 0);
	m = map_it(0);
	report("A_init_proc_exited", m);
	(void)munmap(m, 4096);

	/* B: initialiser is still alive when the attacher locks. */
	if (pipe(pfd) != 0) { perror("pipe"); return (2); }
	if ((kid = fork()) == 0) {
		m = map_it(1);
		if (init_it(m) != 0)
			_exit(3);
		(void)write(pfd[1], "x", 1);
		pause();
		_exit(0);
	}
	(void)read(pfd[0], &c, 1);
	m = map_it(0);
	report("B_init_proc_alive", m);
	(void)kill(kid, SIGKILL);
	(void)waitpid(kid, &st, 0);
	(void)munmap(m, 4096);

	/* C: same process. */
	m = map_it(1);
	(void)init_it(m);
	report("C_same_proc", m);
	(void)pthread_mutex_destroy(m);
	(void)munmap(m, 4096);

	/* D: initialiser exits, but the parent keeps the file MAPPED throughout. */
	{
		pthread_mutex_t *hold;
		hold = map_it(1);
		if ((kid = fork()) == 0) {
			m = map_it(0);
			if (init_it(m) != 0)
				_exit(3);
			_exit(0);
		}
		(void)waitpid(kid, &st, 0);
		m = map_it(0);
		report("D_init_exited_parent_held_map", m);
		(void)munmap(m, 4096);
		(void)munmap(hold, 4096);
	}

	(void)unlink(PATH);
	return (0);
}

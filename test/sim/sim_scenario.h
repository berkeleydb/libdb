/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_scenario.h --
 *	Shared helpers for the DST crash/recover scenarios so each
 *	test_sim_*.c is just its workload + invariant, not env boilerplate.
 *
 *	The crash discipline (see .agents/concurrent-btree-corruption.md and
 *	DESIGN.md): a child forks, runs a seeded workload, arms the
 *	write-back durable-frontier model, and at the crash boundary calls
 *	__db_sim_wb_crash() to drop every byte written-but-not-fsync'd (a
 *	real power loss) before an abrupt _exit.  The parent ALWAYS runs
 *	DB_RECOVER before verify.
 */

#ifndef _DB_SIM_SCENARIO_H_
#define _DB_SIM_SCENARIO_H_

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"
#include "sim_inject.h"

/*
 * Optional small buffer-pool cache (bytes) for the recovery child, so a
 * scenario can force recovery to EVICT + write dirty pages DURING the
 * redo pass (making an armed crash point land on a real redo page write,
 * not only the recovery checkpoint).  A scenario overrides it by defining
 * SIM_RECOVER_CACHE before including this header; 0 (default) = engine
 * default cache.
 */
#ifndef SIM_RECOVER_CACHE
#define SIM_RECOVER_CACHE 0
#endif

/* Wipe and recreate a scratch env dir (known path we own). */
static int
sim_fresh_home(home)
	const char *home;
{
	char cmd[512];
	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    home, home);
	return (system(cmd));
}

/*
 * sim_run_crash_child --
 *	Fork; the child calls populate(seed) (which arms the write-back
 *	model, does the workload, and at its crash boundary calls
 *	__db_sim_wb_crash() then _exit(42)); the parent waits.  Returns 0
 *	iff the child reached the crash point (exit 42).
 */
typedef int (*sim_populate_fn) __P((uint64_t));

static int
sim_run_crash_child(seed, populate)
	uint64_t seed;
	sim_populate_fn populate;
{
	pid_t pid;
	int status;

	if ((pid = fork()) < 0) {
		perror("fork");
		return (-1);
	}
	if (pid == 0)
		exit(populate(seed) == 0 ? 0 : 1);   /* only on setup error */
	if (waitpid(pid, &status, 0) < 0) {
		perror("waitpid");
		return (-1);
	}
	if (WIFEXITED(status) && WEXITSTATUS(status) == 42)
		return (0);
	fprintf(stderr, "child did not reach the crash point (status %d)\n",
	    status);
	return (-1);
}

/* Open an env for recovery (always DB_RECOVER before verify). */
static int
sim_env_recover(home, envp)
	const char *home;
	DB_ENV **envp;
{
	DB_ENV *env;
	int ret;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0664))
	    != 0) {
		fprintf(stderr, "recover open failed: %s\n", db_strerror(ret));
		return (ret);
	}
	*envp = env;
	return (0);
}

/*
 * sim_recover_child --
 *	Fork; the child runs a full DB_RECOVER pass on `home` with the
 *	seed active and a crash armed on the `crash_at`-th recovery I/O op
 *	(0 => run recovery to completion, no crash).  On a completed
 *	recovery the child exits 0 and writes the number of recovery I/O
 *	ops it performed to *ticksp (via a pipe); on a crash it _exit(42)s
 *	mid-pass.  Returns:
 *	   1  child crashed mid-recovery (armed and hit the crash point);
 *	   0  recovery completed (child exit 0); *ticksp = full I/O count;
 *	  -1  fork/child error.
 *	The crash truncates the write-back frontier, so the on-disk files
 *	after a crash reflect exactly what recovery had made durable --
 *	the next recovery must re-converge.
 */
static int
sim_recover_child(seed, home, crash_at, ticksp)
	uint64_t seed;
	const char *home;
	unsigned long crash_at;
	unsigned long *ticksp;
{
	pid_t pid;
	int status, fds[2];
	unsigned long ticks = 0;

	if (ticksp != NULL)
		*ticksp = 0;
	if (pipe(fds) != 0) {
		perror("pipe");
		return (-1);
	}
	if ((pid = fork()) < 0) {
		perror("fork");
		(void)close(fds[0]);
		(void)close(fds[1]);
		return (-1);
	}
	if (pid == 0) {
		DB_ENV *env;
		unsigned long t;
		int ret;

		(void)close(fds[0]);
		__db_sim_activate(seed);
		/* Seed durable frontiers from the inherited on-disk files:
		 * this recovery process opens files a crashed workload already
		 * truncated to its durable frontier, so those bytes ARE
		 * durable.  A crash mid-recovery then drops only what THIS
		 * recovery wrote-but-did-not-fsync. */
		__db_sim_wb_enable(DB_SIM_WB_SEED_ONDISK);
		__db_sim_reccrash_enable(crash_at);
		ret = db_env_create(&env, 0);
		/* A small cache makes recovery EVICT + write dirty pages
		 * DURING the redo pass (not just flush at close), so an
		 * armed crash point can land on a genuine redo page write,
		 * not only the recovery checkpoint.  Best-effort: ignore a
		 * set failure (env still recovers, just with more cache). */
		if (ret == 0 && SIM_RECOVER_CACHE != 0)
			(void)env->set_cachesize(env, 0, SIM_RECOVER_CACHE, 1);
		if (ret == 0)
			ret = env->open(env, home, DB_CREATE | DB_INIT_LOCK |
			    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN |
			    DB_RECOVER, 0664);
		/* Recovery completed without hitting the crash point.  Read
		 * the tick count BEFORE disarming (enable resets it). */
		t = __db_sim_reccrash_ticks();
		__db_sim_reccrash_enable(0);
		if (ret == 0)
			(void)env->close(env, 0);
		(void)write(fds[1], &t, sizeof(t));
		(void)close(fds[1]);
		__db_sim_deactivate();
		_exit(ret == 0 ? 0 : 2);
	}
	(void)close(fds[1]);
	(void)read(fds[0], &ticks, sizeof(ticks));
	(void)close(fds[0]);
	if (waitpid(pid, &status, 0) < 0) {
		perror("waitpid");
		return (-1);
	}
	if (ticksp != NULL)
		*ticksp = ticks;
	if (WIFEXITED(status) && WEXITSTATUS(status) == 42)
		return (1);              /* crashed mid-recovery */
	if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
		return (0);              /* recovery completed */
	fprintf(stderr, "recover child unexpected status %d (crash_at=%lu)\n",
	    status, crash_at);
	return (-1);
}

/* Crash the write-back model then _exit like a power loss. */
#define SIM_CRASH_EXIT() do {						\
	__db_sim_wb_crash();						\
	fflush(NULL);							\
	_exit(42);							\
} while (0)

#endif /* !_DB_SIM_SCENARIO_H_ */

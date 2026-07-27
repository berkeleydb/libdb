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

/* Crash the write-back model then _exit like a power loss. */
#define SIM_CRASH_EXIT() do {						\
	__db_sim_wb_crash();						\
	fflush(NULL);							\
	_exit(42);							\
} while (0)

#endif /* !_DB_SIM_SCENARIO_H_ */

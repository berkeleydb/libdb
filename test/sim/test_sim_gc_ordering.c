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
 * test_sim_gc_ordering.c --
 *	Adversarial ORDERING check for group commit (req 2): after recovery,
 *	no committed transaction may depend on an uncommitted one.
 *
 *	The workload is a set of read-modify-write chains.  NKEY counters
 *	start at 0.  NTHREAD threads each repeatedly pick a counter, read its
 *	current value V under a DB_TXN_SYNC transaction, write V+1, and commit.
 *	Because every increment of a counter is produced by exactly one
 *	committed transaction, the value stored for a counter is a running
 *	ledger of how many committed RMW steps that counter has seen -- a
 *	causal chain where writing V+1 DEPENDS on the commit that wrote V.
 *
 *	A thread records (key, newval) in a MAP_SHARED acknowledged-set ONLY
 *	after commit() returned 0.  At a seeded crash point a thread power-cuts
 *	the process (__db_sim_wb_crash drops un-fsync'd bytes) and _exit(42)s.
 *
 *	After recovery the parent reads every counter and checks the ordering
 *	invariant for each key K:
 *	  1. recovered[K] <= max_acked[K]       -- the surviving value never
 *	     exceeds the highest value any thread was TOLD committed.  A
 *	     recovered value past the highest ack would mean a committed
 *	     write (V+1) survived whose predecessor commit (V) was never
 *	     acknowledged durable -- a committed txn depending on a
 *	     not-durably-committed one.
 *	  2. recovered[K] was an acked value (or 0): because each counter
 *	     value is produced by one committed txn, a surviving value V>0
 *	     that no thread ever acked would be a phantom write.
 *	Both are one-sided in the safe direction (the ack set can only
 *	under-state what committed, never over-state it), so a violation is a
 *	real ordering defect, never a false alarm.
 *
 *	This is the chain the single-record durability gate
 *	(test_sim_group_commit) does not exercise: there every record is
 *	independent, so a lost commit shows up as a missing key but never as a
 *	broken dependency.  Here a lost predecessor makes its successor an
 *	ordering violation.
 *
 *	PLANTED BUG (DB_DST_INJECT_BUG=1, NODURABLE): with the fsync skipped,
 *	the crash drops acked writes, so some counter's surviving value falls
 *	BELOW an acked value whose successor also survived -- caught as a lost
 *	acked commit (recovered[K] < some acked value that a surviving higher
 *	value depends on).  The control proves the chain has teeth.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_gc_ordering && ./test_sim_gc_ordering [seed] [crash_after]
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

#define	HOME	"TESTDIR_sim_gc_ordering"
#define	DBFILE	"gco.db"
#define	ACKFILE	HOME "/acked.map"

#define	NTHREAD	8
#define	NKEY	16		/* counters contended by all threads */
#define	MAXACK	65536

struct ackset {
	unsigned int n;			/* acks published (atomic) */
	unsigned int crash_after;
	/* Per-key high-water of acked values, so the parent knows the bound. */
	int	     max_acked[NKEY];
	/*
	 * A compact record of every acked (key,val).  We only need, per key,
	 * the SET of acked values to check invariant 2; a bitmap keyed by the
	 * value would be ideal but values are unbounded, so we keep the max
	 * (invariant 1) plus the full list for invariant 2 up to MAXACK.
	 */
	unsigned short ack_key[MAXACK];
	int	       ack_val[MAXACK];
};

static struct ackset *acks;
static DB_ENV *env;
static DB *db;
static volatile int stop;

static void
keyname(k, buf)
	int k;
	char *buf;
{
	(void)snprintf(buf, 32, "ctr-%04d", k);
}

/* Record that counter k reached value v (committed).  Called after commit()==0. */
static unsigned int
ack(k, v)
	int k, v;
{
	unsigned int slot;
	int old;

	/* Track per-key high-water (atomic max via CAS loop). */
	do {
		old = acks->max_acked[k];
		if (v <= old)
			break;
	} while (!__sync_bool_compare_and_swap(&acks->max_acked[k], old, v));

	slot = __sync_fetch_and_add(&acks->n, 1);
	if (slot < MAXACK) {
		acks->ack_key[slot] = (unsigned short)k;
		acks->ack_val[slot] = v;
		__sync_synchronize();
	}
	return (slot + 1);
}

struct targ { pthread_t th; int id; uint64_t seed; };

static void *
worker(arg)
	void *arg;
{
	struct targ *t = arg;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32];
	int v, nv, k, ret;
	unsigned int nacked;
	uint64_t r = t->seed ^ (0x9E3779B97F4A7C15ull * (uint64_t)(t->id + 1));

	while (!stop) {
		/* Pick a counter (per-thread PRNG; the DB holds the truth). */
		r = r * 6364136223846793005ull + 1442695040888963407ull;
		k = (int)((r >> 33) % NKEY);
		keyname(k, kbuf);

		if ((ret = env->txn_begin(env, NULL, &txn, DB_TXN_SYNC)) != 0)
			break;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;

		/* READ current value (0 if absent). */
		if ((ret = db->get(db, txn, &key, &data, 0)) == 0)
			v = *(int *)data.data;
		else if (ret == DB_NOTFOUND)
			v = 0;
		else {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			break;
		}

		/* MODIFY. */
		nv = v + 1;
		memset(&data, 0, sizeof(data));
		data.data = &nv; data.size = sizeof(nv);

		/* WRITE. */
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			break;
		}

		/* COMMIT (durable). */
		if ((ret = txn->commit(txn, 0)) != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			break;
		}

		/* This counter's value nv is now committed-durable. */
		nacked = ack(k, nv);

		if (nacked >= acks->crash_after) {
			__db_sim_wb_crash();
			fflush(NULL);
			_exit(42);
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
		t[i].id = i; t[i].seed = seed;
		if ((ret = pthread_create(&t[i].th, NULL, worker, &t[i])) != 0)
			return (ret);
	}
	for (i = 0; i < NTHREAD; i++)
		(void)pthread_join(t[i].th, NULL);
	return (1);	/* only reached on a setup error (no crash taken) */
}

static int
value_was_acked(k, v)
	int k, v;
{
	unsigned int i, n;

	if (v == 0)
		return (1);	/* initial value, trivially consistent */
	n = acks->n > MAXACK ? MAXACK : acks->n;
	for (i = 0; i < n; i++)
		if ((int)acks->ack_key[i] == k && acks->ack_val[i] == v)
			return (1);
	return (0);
}

static int
verify_after_recovery(viol_out)
	int *viol_out;
{
	DB *vdb;
	DBT key, data;
	char kbuf[32];
	int k, rv, ret, viol = 0;

	*viol_out = 0;

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

	for (k = 0; k < NKEY; k++) {
		keyname(k, kbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if ((ret = db->get(db, NULL, &key, &data, 0)) == 0)
			rv = *(int *)data.data;
		else if (ret == DB_NOTFOUND)
			rv = 0;
		else {
			fprintf(stderr, "get %s: %s\n", kbuf, db_strerror(ret));
			(void)db->close(db, 0); (void)env->close(env, 0);
			return (ret);
		}

		/* Invariant 1: never past the highest acknowledged value. */
		if (rv > acks->max_acked[k]) {
			fprintf(stderr, "ORDERING VIOLATION key %s: recovered "
			    "%d > max acked %d -- a committed write survived "
			    "whose predecessor commit was never acknowledged\n",
			    kbuf, rv, acks->max_acked[k]);
			viol++;
		}
		/* Invariant 2: the surviving value was actually acked. */
		else if (!value_was_acked(k, rv)) {
			fprintf(stderr, "ORDERING VIOLATION key %s: recovered "
			    "%d was never acknowledged committed -- phantom "
			    "write\n", kbuf, rv);
			viol++;
		}
	}
	(void)db->close(db, 0);

	if ((ret = db_create(&vdb, env, 0)) != 0)
		return (ret);
	if ((ret = vdb->verify(vdb, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "verify: %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (ret);
	}
	(void)env->close(env, 0);

	*viol_out = viol;
	return (viol != 0 ? 1 : 0);
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
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x0DEE11ull;
	unsigned int crash_after = argc > 2 ? (unsigned)atoi(argv[2]) : 256;
	pid_t pid;
	int status, ret, viol;
	char cmd[256];

	if (crash_after < 1) crash_after = 1;
	if (crash_after > MAXACK) crash_after = MAXACK;

	(void)snprintf(cmd, sizeof(cmd),
	    "mkdir -p %s && find %s -mindepth 1 -delete", HOME, HOME);
	(void)system(cmd);

	if (map_ackset() != 0)
		return (EXIT_FAILURE);

	if ((pid = fork()) < 0) { perror("fork"); return (EXIT_FAILURE); }
	if (pid == 0)
		exit(run_child(seed, crash_after) == 0 ? 0 : 1);

	if (waitpid(pid, &status, 0) < 0) {
		perror("waitpid"); return (EXIT_FAILURE);
	}
	if (!(WIFEXITED(status) && WEXITSTATUS(status) == 42)) {
		fprintf(stderr, "child did not reach the crash point "
		    "(status %d)\n", status);
		return (EXIT_FAILURE);
	}
	if (acks->n == 0) {
		fprintf(stderr, "no commit acked\n");
		return (EXIT_FAILURE);
	}

	ret = verify_after_recovery(&viol);

#if DB_DST_BUG(1)
	if (ret == 0 && viol == 0) {
		fprintf(stderr, "test_sim_gc_ordering: DID NOT CATCH NODURABLE "
		    "(seed 0x%llx)\n", (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_gc_ordering: CAUGHT NODURABLE -- %d ordering "
	    "violations after the skipped fsync (seed 0x%llx)\n",
	    viol, (unsigned long long)seed);
	return (EXIT_SUCCESS);
#else
	if (ret == 0) {
		printf("test_sim_gc_ordering: PASS -- %u RMW commits across "
		    "%d counters, %d threads; after crash recovery no counter "
		    "exceeds its acked high-water and every surviving value "
		    "was acknowledged (seed 0x%llx crash_after=%u)\n",
		    acks->n, NKEY, NTHREAD, (unsigned long long)seed,
		    crash_after);
		return (EXIT_SUCCESS);
	}
	fprintf(stderr, "test_sim_gc_ordering: FAIL -- %d ordering violations "
	    "(seed 0x%llx crash_after=%u)\n", viol,
	    (unsigned long long)seed, crash_after);
	return (EXIT_FAILURE);
#endif
}

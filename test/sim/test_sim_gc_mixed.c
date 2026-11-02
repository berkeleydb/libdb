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
 * test_sim_gc_mixed.c --
 *	Adversarial MIXED-DURABILITY check for group commit (req 3): with
 *	DB_TXN_SYNC, DB_TXN_WRITE_NOSYNC and DB_TXN_NOSYNC committers
 *	interleaved, a SYNC commit must make every EARLIER-LSN commit durable
 *	-- the fsync a SYNC committer drives covers the whole log prefix up to
 *	its own commit record, which includes the NOSYNC/WRITE_NOSYNC records
 *	that were appended before it.
 *
 *	Each committer requests a commit token (DB_TXN->set_commit_token),
 *	which carries the commit record's LSN (file/offset, network order, at
 *	token bytes [12..20)).  After commit() returns 0 the thread records
 *	(id, mode, lsn) in a MAP_SHARED acknowledged-set.  At a seeded crash
 *	point a thread power-cuts (__db_sim_wb_crash drops un-fsync'd bytes)
 *	and _exit(42)s.
 *
 *	After recovery the parent computes L = the largest LSN among all ACKED
 *	SYNC commits, and asserts:
 *
 *	  INVARIANT: every acked commit of ANY mode whose LSN <= L is present
 *	  after recovery.
 *
 *	That is exactly "a SYNC commit makes every earlier-LSN commit
 *	durable": the SYNC commit at LSN L fsync'd the log through L, so every
 *	record at an LSN <= L -- including NOSYNC ones -- is on stable
 *	storage.  A NOSYNC/WRITE_NOSYNC commit with an LSN > L may legally be
 *	absent (it was never covered by a durable fsync), so those are NOT
 *	required and their absence is not a failure.
 *
 *	The ack set is one-sided safe: a commit is recorded AFTER its ack, so
 *	it can only under-state what was acknowledged, never claim an ack that
 *	did not happen; a violation is therefore a real lost durable commit.
 *
 *	PLANTED BUG (DB_DST_INJECT_BUG=1, NODURABLE): the SYNC fsync is
 *	skipped, so L's fsync never happens, the crash drops the covered
 *	prefix, and some acked commit with LSN <= L is lost -- caught.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_gc_mixed && ./test_sim_gc_mixed [seed] [crash_after]
 */

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <arpa/inet.h>
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

#define	HOME	"TESTDIR_sim_gc_mixed"
#define	DBFILE	"gcm.db"
#define	ACKFILE	HOME "/acked.map"

#define	NTHREAD	9		/* 3 SYNC, 3 WRITE_NOSYNC, 3 NOSYNC */
#define	MAXACK	65536

#define	M_SYNC	0
#define	M_WRNOSYNC 1
#define	M_NOSYNC 2

struct ackrec {
	int	  id;
	u_int32_t lsn_file;
	u_int32_t lsn_off;
	int	  mode;
};

struct ackset {
	unsigned int n;
	unsigned int crash_after;
	struct ackrec rec[MAXACK];
};

static struct ackset *acks;
static DB_ENV *env;
static DB *db;
static volatile int stop;

static void
mkrec(id, kbuf, vbuf)
	int id;
	char *kbuf, *vbuf;
{
	(void)snprintf(kbuf, 32, "k-%08d", id);
	(void)snprintf(vbuf, 32, "v-%08d-%08x", id, (unsigned)(id * 2654435761u));
}

/* LSN compare: 1 if a < b. */
static int
lsn_lt(af, ao, bf, bo)
	u_int32_t af, ao, bf, bo;
{
	return (af < bf || (af == bf && ao < bo));
}

static unsigned int
ack(id, mode, tok)
	int id, mode;
	DB_TXN_TOKEN *tok;
{
	unsigned int slot;
	u_int32_t f, o;

	/* Extract the commit LSN from the token (network order). */
	memcpy(&f, tok->buf + 12, 4); f = ntohl(f);
	memcpy(&o, tok->buf + 16, 4); o = ntohl(o);

	slot = __sync_fetch_and_add(&acks->n, 1);
	if (slot < MAXACK) {
		acks->rec[slot].id = id;
		acks->rec[slot].lsn_file = f;
		acks->rec[slot].lsn_off = o;
		acks->rec[slot].mode = mode;
		__sync_synchronize();
	}
	return (slot + 1);
}

struct targ { pthread_t th; int id; int mode; };

static void *
committer(arg)
	void *arg;
{
	struct targ *t = arg;
	DB_TXN *txn;
	DB_TXN_TOKEN token;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned int nacked;
	u_int32_t cflag;
	int i, id, ret;

	cflag = t->mode == M_SYNC ? DB_TXN_SYNC :
	    t->mode == M_WRNOSYNC ? DB_TXN_WRITE_NOSYNC : DB_TXN_NOSYNC;

	for (i = 0; !stop; i++) {
		id = i * NTHREAD + t->id;
		mkrec(id, kbuf, vbuf);

		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			break;
		if ((ret = txn->set_commit_token(txn, &token)) != 0) {
			(void)txn->abort(txn);
			break;
		}
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
		if ((ret = txn->commit(txn, cflag)) != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			break;
		}

		nacked = ack(id, t->mode, &token);

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
		t[i].id = i;
		t[i].mode = i % 3;	/* 0 SYNC, 1 WRITE_NOSYNC, 2 NOSYNC */
		if ((ret = pthread_create(&t[i].th, NULL, committer, &t[i]))
		    != 0)
			return (ret);
	}
	for (i = 0; i < NTHREAD; i++)
		(void)pthread_join(t[i].th, NULL);
	return (1);
}

static int
verify_after_recovery(nreq, missing_out)
	unsigned int *nreq;
	int *missing_out;
{
	DB *vdb;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned int i, n;
	u_int32_t Lf, Lo;
	int ret, missing = 0, mismatch = 0, have_sync = 0;

	*nreq = 0; *missing_out = 0;

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

	/* L = largest LSN among acked SYNC commits. */
	Lf = Lo = 0;
	for (i = 0; i < n; i++)
		if (acks->rec[i].mode == M_SYNC) {
			if (!have_sync ||
			    lsn_lt(Lf, Lo, acks->rec[i].lsn_file,
			    acks->rec[i].lsn_off)) {
				Lf = acks->rec[i].lsn_file;
				Lo = acks->rec[i].lsn_off;
			}
			have_sync = 1;
		}

	if (!have_sync) {
		fprintf(stderr, "no SYNC commit was acked -- nothing to "
		    "anchor the mixed-durability invariant\n");
		(void)db->close(db, 0); (void)env->close(env, 0);
		return (2);
	}

	/*
	 * Every acked commit (ANY mode) with LSN <= L must be present: the
	 * SYNC fsync at L made the whole prefix durable.
	 */
	for (i = 0; i < n; i++) {
		if (lsn_lt(Lf, Lo, acks->rec[i].lsn_file, acks->rec[i].lsn_off))
			continue;	/* LSN > L: not required to survive */
		(*nreq)++;
		mkrec(acks->rec[i].id, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "LOST commit %s (mode %d, lsn %u/%u) "
			    "<= SYNC frontier %u/%u: a SYNC commit failed to "
			    "make an earlier commit durable\n",
			    kbuf, acks->rec[i].mode, acks->rec[i].lsn_file,
			    acks->rec[i].lsn_off, Lf, Lo);
			missing++;
		} else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0) {
			fprintf(stderr, "WRONG value for %s\n", kbuf);
			mismatch++;
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
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x31AEDull;
	unsigned int crash_after = argc > 2 ? (unsigned)atoi(argv[2]) : 128;
	unsigned int nreq;
	pid_t pid;
	int status, ret, missing;
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
		fprintf(stderr, "child did not reach crash (status %d)\n",
		    status);
		return (EXIT_FAILURE);
	}
	if (acks->n == 0) {
		fprintf(stderr, "no commit acked\n");
		return (EXIT_FAILURE);
	}

	ret = verify_after_recovery(&nreq, &missing);
	if (ret == 2) {
		/* No SYNC anchor; a weak run, not a verdict. Re-run a seed. */
		fprintf(stderr, "retry with a seed that lands a SYNC ack "
		    "before the crash\n");
		return (EXIT_FAILURE);
	}

#if DB_DST_BUG(1)
	if (ret == 0 && missing == 0) {
		fprintf(stderr, "test_sim_gc_mixed: DID NOT CATCH NODURABLE "
		    "(seed 0x%llx)\n", (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_gc_mixed: CAUGHT NODURABLE -- %d of %u "
	    "prefix-covered commits lost (seed 0x%llx)\n",
	    missing, nreq, (unsigned long long)seed);
	return (EXIT_SUCCESS);
#else
	if (ret == 0) {
		printf("test_sim_gc_mixed: PASS -- %u commits at/below the "
		    "SYNC durable frontier (mixed SYNC/WRITE_NOSYNC/NOSYNC) "
		    "all survived crash recovery; a SYNC commit made every "
		    "earlier-LSN commit durable (seed 0x%llx crash_after=%u)\n",
		    nreq, (unsigned long long)seed, crash_after);
		return (EXIT_SUCCESS);
	}
	fprintf(stderr, "test_sim_gc_mixed: FAIL -- %d of %u prefix-covered "
	    "commits LOST (seed 0x%llx crash_after=%u)\n",
	    missing, nreq, (unsigned long long)seed, crash_after);
	return (EXIT_FAILURE);
#endif
}

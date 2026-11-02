/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * gc_multiproc.c --
 *	Adversarial durability of MULTI-PROCESS group commit (req 6): several
 *	real OS processes attach ONE shared (non-DB_PRIVATE) environment and
 *	commit DB_TXN_SYNC concurrently, so their flushes batch through the
 *	cross-process leader/follower queue (lp->commits, each waiter blocking
 *	on a PTHREAD_PROCESS_SHARED mtx_txnwait).  A coordinator SIGKILLs the
 *	whole group at a point where committers are mid-flight -- a real power
 *	cut, no clean shutdown, some processes parked as followers on another
 *	process's fsync.  After recovery every transaction whose commit()
 *	returned success (recorded in a MAP_SHARED oracle AFTER the ack) must
 *	be present.
 *
 *	This is the axis the single-process gate (test_sim_group_commit, which
 *	uses pthreads in one process) does not reach: there every mtx_txnwait
 *	is a process-local pthread mutex; here it is a real cross-process
 *	shared mutex, and the leader whose fsync wakes a follower is a
 *	DIFFERENT PROCESS.  A broken cross-process wake (a follower in process
 *	B woken-for-success by a flush in process A that did not cover it)
 *	shows up as a lost acked commit after recovery.
 *
 *	Unlike the DST scenarios this uses a REAL kill and REAL fsync
 *	durability (no write-back truncation model), so it is the hardware
 *	confirmation that the software model's verdict holds on a real disk:
 *	a SIGKILL loses exactly the un-fsync'd tail, and every acked commit
 *	is on stable storage.
 *
 *	Roles (one binary):
 *	    gc_multiproc <home> setup <nproc>        -- create env+db, fork kids
 *	    gc_multiproc <home> child <id> <nproc>   -- exec'd committer
 *	    gc_multiproc <home> verify <nproc>       -- recover + check oracle
 *	The runner (run_gc_multiproc.sh) drives setup, waits, SIGKILLs the
 *	group, then runs verify.  Here `setup' forks the children and is the
 *	one the runner kills; `verify' is a fresh process after the kill.
 *
 *	Exit: setup never returns normally (it is killed); verify returns 0 if
 *	every acked commit survived, non-zero on a lost commit.
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
#include <unistd.h>

#include "db.h"

#define	DBFILE	"gcmp.db"
#define	ACKNAME	"acked.map"
#define	MAXACK	200000

struct ackset {
	unsigned int n;			/* acks published (atomic fetch-add) */
	int	     key[MAXACK];
};

static struct ackset *
map_ackset(home, create)
	const char *home;
	int create;
{
	struct ackset *a;
	char path[512];
	int fd, oflag;

	(void)snprintf(path, sizeof(path), "%s/%s", home, ACKNAME);
	oflag = O_RDWR | (create ? (O_CREAT | O_TRUNC) : 0);
	if ((fd = open(path, oflag, 0664)) < 0) {
		perror("open ackmap");
		return (NULL);
	}
	if (create && ftruncate(fd, (off_t)sizeof(struct ackset)) != 0) {
		perror("ftruncate ackmap"); (void)close(fd); return (NULL);
	}
	a = mmap(NULL, sizeof(struct ackset), PROT_READ | PROT_WRITE,
	    MAP_SHARED, fd, 0);
	(void)close(fd);
	if (a == MAP_FAILED) { perror("mmap ackmap"); return (NULL); }
	if (create)
		memset(a, 0, sizeof(*a));
	return (a);
}

static void
mkrec(id, kbuf, vbuf)
	int id;
	char *kbuf, *vbuf;
{
	(void)snprintf(kbuf, 32, "key-%08d", id);
	(void)snprintf(vbuf, 32, "val-%08d-%08x", id, (unsigned)(id * 2654435761u));
}

static int
open_env(home, env, create)
	const char *home;
	DB_ENV **env;
	u_int32_t create;
{
	int ret;

	if ((ret = db_env_create(env, 0)) != 0)
		return (ret);
	return ((*env)->open(*env, home, create | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0664));
}

/* A committer process: attach the shared env, commit SYNC forever, ack. */
static int
child(home, id, nproc)
	const char *home;
	int id, nproc;
{
	struct ackset *acks;
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, rid, ret;
	unsigned int slot;

	if ((acks = map_ackset(home, 0)) == NULL)
		return (1);
	if ((ret = open_env(home, &env, 0)) != 0) {
		fprintf(stderr, "child %d env: %s\n", id, db_strerror(ret));
		return (1);
	}
	if ((ret = db_create(&db, env, 0)) != 0)
		return (1);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_AUTO_COMMIT | DB_THREAD, 0664)) != 0) {
		fprintf(stderr, "child %d db: %s\n", id, db_strerror(ret));
		return (1);
	}

	for (i = 0; ; i++) {
		rid = i * nproc + id;		/* disjoint id slice */
		mkrec(rid, kbuf, vbuf);
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
		if ((ret = txn->commit(txn, 0)) != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			break;
		}
		/* Record the ack AFTER commit() returned 0. */
		slot = __sync_fetch_and_add(&acks->n, 1);
		if (slot < MAXACK) {
			acks->key[slot] = rid;
			__sync_synchronize();
		} else
			break;		/* oracle full; stop before overrun */
	}
	/* Normally unreachable: the coordinator SIGKILLs us. */
	(void)db->close(db, 0);
	(void)env->close(env, 0);
	return (0);
}

static int
setup(home, self, nproc)
	const char *home, *self;
	int nproc;
{
	struct ackset *acks;
	DB_ENV *env;
	DB *db;
	pid_t pid;
	char idbuf[16], npbuf[16];
	int i, ret;

	if ((acks = map_ackset(home, 1)) == NULL)
		return (1);

	/* Create the env + db, then close so children open the shared region. */
	if ((ret = open_env(home, &env, DB_CREATE)) != 0) {
		fprintf(stderr, "setup env: %s\n", db_strerror(ret));
		return (1);
	}
	if ((ret = db_create(&db, env, 0)) != 0)
		return (1);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "setup db: %s\n", db_strerror(ret));
		return (1);
	}
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	(void)snprintf(npbuf, sizeof(npbuf), "%d", nproc);
	for (i = 0; i < nproc; i++) {
		if ((pid = fork()) == 0) {
			(void)snprintf(idbuf, sizeof(idbuf), "%d", i);
			execl(self, self, home, "child", idbuf, npbuf,
			    (char *)NULL);
			perror("execl");
			_exit(127);
		}
		if (pid < 0) {
			perror("fork");
			return (1);
		}
	}
	/*
	 * Wait to be killed.  The runner SIGKILLs this process group after a
	 * fixed interval, which kills us and all the children mid-commit.  If
	 * we are somehow not killed, exit so the runner does not hang.
	 */
	for (i = 0; i < 600; i++)		/* up to 60 s */
		usleep(100000);
	return (0);
}

static int
verify(home, nproc)
	const char *home;
	int nproc;
{
	struct ackset *acks;
	DB_ENV *env;
	DB *db, *vdb;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned int i, n;
	int ret, missing = 0, mismatch = 0;

	(void)nproc;
	if ((acks = map_ackset(home, 0)) == NULL)
		return (1);

	/* ALWAYS recover a crashed txn env before touching the tree. */
	if ((ret = open_env(home, &env, DB_CREATE | DB_RECOVER)) != 0) {
		fprintf(stderr, "recover: %s\n", db_strerror(ret));
		return (1);
	}
	if ((ret = db_create(&db, env, 0)) != 0)
		return (1);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "reopen: %s\n", db_strerror(ret));
		return (1);
	}

	n = acks->n > MAXACK ? MAXACK : acks->n;
	for (i = 0; i < n; i++) {
		mkrec(acks->key[i], kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		/* DB_THREAD env: ask the library to allocate the result. */
		data.flags = DB_DBT_MALLOC;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "LOST ACKED COMMIT %s: %s\n",
			    kbuf, db_strerror(ret));
			missing++;
		} else {
			if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0) {
				fprintf(stderr, "WRONG value for %s\n", kbuf);
				mismatch++;
			}
			free(data.data);
		}
	}
	(void)db->close(db, 0);

	if ((ret = db_create(&vdb, env, 0)) != 0)
		return (1);
	if ((ret = vdb->verify(vdb, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "verify: %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (1);
	}
	(void)env->close(env, 0);

	if (missing == 0 && mismatch == 0) {
		printf("gc_multiproc: PASS -- %u acked commits across %d "
		    "processes ALL survived SIGKILL + recovery\n", n, nproc);
		return (0);
	}
	fprintf(stderr, "gc_multiproc: FAIL -- %d lost, %d wrong of %u acked\n",
	    missing, mismatch, n);
	return (1);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	const char *home, *self;
	int nproc;

	if (argc < 3) {
		fprintf(stderr, "usage: %s <home> setup|child|verify ...\n",
		    argv[0]);
		return (2);
	}
	self = argv[0];
	home = argv[1];

	if (strcmp(argv[2], "child") == 0 && argc >= 5)
		return (child(home, atoi(argv[3]), atoi(argv[4])));
	if (strcmp(argv[2], "verify") == 0 && argc >= 4)
		return (verify(home, atoi(argv[3])));
	if (strcmp(argv[2], "setup") == 0 && argc >= 4) {
		nproc = atoi(argv[3]);
		return (setup(home, self, nproc));
	}
	fprintf(stderr, "bad role/args\n");
	return (2);
}

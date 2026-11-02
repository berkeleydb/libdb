/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * hash_split_leak --
 *	A split or a bucket merge of a legacy P_HASH_UNSORTED hash page must
 *	release the page pins and the bucket lock it took when sorting the
 *	page fails.
 *
 *	__ham_split_page and __ham_merge_pages pin a bucket page and
 *	write-lock it, then sort it when it is still in the pre-4.6 unsorted
 *	format.  __ham_sort_page_cursor can fail: it logs the sort first
 *	(__ham_splitdata_log, which allocates).  Both callers returned that
 *	error directly instead of jumping to their cleanup, so the page stayed
 *	pinned and the bucket stayed locked until the environment closed.  The
 *	next writer to that bucket in another transaction blocked on the
 *	leaked lock, and DB->close reported pages still pinned.
 *
 *	The fixture is built as test/db/hash_unsorted_cmp.c does: a
 *	current-format Hash file whose bucket pages have their type byte set
 *	back to P_HASH_UNSORTED and whose metadata version is set back to 8
 *	(4.5.20).  Opening it in a transactional environment and inserting
 *	enough records splits those buckets.
 *
 *	Fault injection (an --enable-faultinject build) fails the Kth
 *	allocation for every K up to the workload's allocation count.  For
 *	every K whose failure lands inside the insert loop, the test aborts the
 *	transaction and then requires that
 *	  1. a second transaction can write every bucket (no leaked lock: a
 *	     lock-timeout of 2 s turns a leak into DB_LOCK_NOTGRANTED), and
 *	  2. the mpool has no pinned pages after the handles close.
 *	Without --enable-faultinject the test runs the workload once and checks
 *	only that the unsorted pages were split (a build sanity check).
 *
 * usage: hash_split_leak <home>
 * Exit 0 = PASS.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include "db.h"

#ifdef HAVE_FI
#include "fi_alloc.h"
#endif

#define	PAGESIZE	512
#define	NRECS		40		/* Fixture: a few buckets. */
#define	NGROW		600		/* Inserts after the downgrade: splits. */
#define	PAGE_TYPE_OFF	25
#define	META_VERSION_OFF 16
#define	P_HASH_UNSORTED_T 2
#define	P_HASH_T	13
#define	HASH_VERSION_45	8

static int
make_fixture(const char *home)
{
	DB *dbp;
	DBT key, data;
	char path[512], kbuf[32], vbuf[32];
	struct stat sb;
	off_t off;
	u_int32_t pgsz, version;
	u_int8_t type;
	int fd, i, converted;

	(void)snprintf(path, sizeof(path), "%s/h.db", home);
	(void)unlink(path);
	if (db_create(&dbp, NULL, 0) != 0 ||
	    dbp->set_pagesize(dbp, PAGESIZE) != 0 ||
	    dbp->set_h_ffactor(dbp, 2) != 0 ||
	    dbp->open(dbp, NULL, path, NULL, DB_HASH, DB_CREATE, 0644) != 0)
		return (-1);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	for (i = 0; i < NRECS; i++) {
		key.size = (u_int32_t)snprintf(kbuf, sizeof(kbuf), "key%06d", i);
		key.data = kbuf;
		data.size = (u_int32_t)snprintf(vbuf, sizeof(vbuf), "v%d", i);
		data.data = vbuf;
		if (dbp->put(dbp, NULL, &key, &data, 0) != 0)
			return (-1);
	}
	if (dbp->close(dbp, 0) != 0)
		return (-1);

	if ((fd = open(path, O_RDWR)) < 0 || fstat(fd, &sb) != 0 ||
	    pread(fd, &pgsz, sizeof(pgsz), 20) != (ssize_t)sizeof(pgsz))
		return (-1);
	version = HASH_VERSION_45;
	if (pwrite(fd, &version, sizeof(version), META_VERSION_OFF) !=
	    (ssize_t)sizeof(version))
		return (-1);
	converted = 0;
	for (off = 0; off < sb.st_size; off += (off_t)pgsz) {
		if (pread(fd, &type, 1, off + PAGE_TYPE_OFF) != 1)
			return (-1);
		if (type != P_HASH_T)
			continue;
		type = P_HASH_UNSORTED_T;
		if (pwrite(fd, &type, 1, off + PAGE_TYPE_OFF) != 1)
			return (-1);
		converted++;
	}
	(void)close(fd);
	return (converted);
}

static int
count_unsorted(const char *home)
{
	char path[512];
	struct stat sb;
	off_t off;
	u_int32_t pgsz;
	u_int8_t type;
	int fd, n;

	(void)snprintf(path, sizeof(path), "%s/h.db", home);
	if ((fd = open(path, O_RDONLY)) < 0 || fstat(fd, &sb) != 0 ||
	    pread(fd, &pgsz, sizeof(pgsz), 20) != (ssize_t)sizeof(pgsz))
		return (-1);
	for (n = 0, off = 0; off < sb.st_size; off += (off_t)pgsz)
		if (pread(fd, &type, 1, off + PAGE_TYPE_OFF) == 1 &&
		    type == P_HASH_UNSORTED_T)
			n++;
	(void)close(fd);
	return (n);
}

/*
 * workload --
 *	Open the legacy file in a transactional env and grow it (splits sort
 *	each unsorted bucket first).  Arm fault injection at K (K = 0: off).
 *	Returns 0 = pass, 1 = leak detected, 2 = setup error.
 */
static int
workload(const char *home, long k, int *fired)
{
	DB_ENV *env;
	DB *dbp;
	DB_TXN *txn;
	DB_MPOOL_STAT *gsp;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, rc;

	*fired = 0;
	rc = 0;
	if ((ret = db_env_create(&env, 0)) != 0)
		return (2);
	env->set_errfile(env, NULL);
	(void)env->set_cachesize(env, 0, 8 * 1024 * 1024, 1);
	(void)env->set_timeout(env, 2000000, DB_SET_LOCK_TIMEOUT);
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN | DB_PRIVATE |
	    DB_THREAD, 0)) != 0) {
		fprintf(stderr, "env open: %s\n", db_strerror(ret));
		return (2);
	}
	if ((ret = db_create(&dbp, env, 0)) != 0 || (ret = dbp->open(dbp,
	    NULL, "h.db", NULL, DB_HASH, DB_AUTO_COMMIT, 0)) != 0) {
		fprintf(stderr, "db open: %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (2);
	}
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

	/* Transaction 1: grow the table with injection armed. */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (2);
#ifdef HAVE_FI
	if (k > 0)
		__db_fi_arm(k);
#else
	(void)k;
#endif
	for (i = 0; i < NGROW; i++) {
		key.size =
		    (u_int32_t)snprintf(kbuf, sizeof(kbuf), "grow%06d", i);
		key.data = kbuf;
		data.size = (u_int32_t)snprintf(vbuf, sizeof(vbuf), "g%d", i);
		data.data = vbuf;
		if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0)
			break;
	}
#ifdef HAVE_FI
	*fired = __db_fi_fired();
	__db_fi_disarm();
#endif
	/*
	 * An injected failure in the commit itself is an allowed outcome: the
	 * commit returns an error and the transaction is resolved either way.
	 * What must never happen is a leaked lock or pin, checked below.
	 */
	if (ret != 0)
		(void)txn->abort(txn);
	else
		(void)txn->commit(txn, 0);

	/*
	 * Transaction 2: write a key into every bucket.  A lock leaked by
	 * transaction 1's failed split is still held by its locker, so this
	 * gets DB_LOCK_NOTGRANTED after the 2 s lock timeout.
	 */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0) {
		fprintf(stderr, "K=%ld: txn 2 begin: %s\n", k, db_strerror(ret));
		return (2);
	}
	for (i = 0; i < NGROW; i++) {
		key.size =
		    (u_int32_t)snprintf(kbuf, sizeof(kbuf), "probe%06d", i);
		key.data = kbuf;
		data.size = 1;
		data.data = "p";
		if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0) {
			if (ret == DB_LOCK_NOTGRANTED ||
			    ret == DB_LOCK_DEADLOCK) {
				fprintf(stderr,
				    "K=%ld: probe put blocked (%s): a bucket "
				    "lock leaked\n", k, db_strerror(ret));
				rc = 1;
			} else {
				fprintf(stderr, "K=%ld: probe put: %s\n",
				    k, db_strerror(ret));
				rc = 2;
			}
			break;
		}
	}
	if (rc == 0)
		(void)txn->commit(txn, 0);
	else
		(void)txn->abort(txn);

	/* No page may stay pinned once the cursor work is done. */
	if (env->memp_stat(env, &gsp, NULL, 0) == 0) {
		/* st_page_* do not count pins; use the db close check below. */
		free(gsp);
	}
	ret = dbp->close(dbp, 0);
	if (ret != 0 && rc == 0) {
		fprintf(stderr, "K=%ld: DB->close: %s (pinned page?)\n",
		    k, db_strerror(ret));
		rc = 1;
	}
	if ((ret = env->close(env, 0)) != 0 && rc == 0) {
		fprintf(stderr, "K=%ld: DB_ENV->close: %s\n",
		    k, db_strerror(ret));
		rc = 1;
	}
	return (rc);
}

static int
fresh(const char *home)
{
	char cmd[600];
	int n;

	(void)snprintf(cmd, sizeof(cmd),
	    "find '%s' -mindepth 1 -delete 2>/dev/null; mkdir -p '%s'",
	    home, home);
	if (system(cmd) != 0)
		return (-1);
	if ((n = make_fixture(home)) <= 0) {
		fprintf(stderr, "fixture: %d unsorted pages\n", n);
		return (-1);
	}
	return (n);
}

int
main(int argc, char *argv[])
{
	const char *home;
	long k, kmax;
	int fired, n, rc, status, fails, hits;
	pid_t pid;

	if (argc != 2) {
		fprintf(stderr, "usage: hash_split_leak <home>\n");
		return (2);
	}
	home = argv[1];

	/* Baseline: no injection.  The splits must sort every bucket. */
	if ((n = fresh(home)) < 0)
		return (2);
	if ((rc = workload(home, 0, &fired)) != 0) {
		printf("VERDICT hash_split_leak FAIL baseline workload rc=%d\n",
		    rc);
		return (1);
	}
	if (count_unsorted(home) >= n) {
		printf("VERDICT hash_split_leak FAIL fixture: %d unsorted pages "
		    "and none was split, so the test exercised nothing\n", n);
		return (1);
	}
#ifndef HAVE_FI
	printf("VERDICT hash_split_leak PASS baseline only (%d unsorted "
	    "pages sorted by splits); build with --enable-faultinject for the "
	    "error-path sweep\n", n);
	return (0);
#else
	kmax = getenv("HSL_MAXK") != NULL ? atol(getenv("HSL_MAXK")) : 4000;
	fails = hits = 0;
	for (k = 1; k <= kmax; k++) {
		if (fresh(home) < 0)
			return (2);
		/* Each K in a child, so a hang or crash cannot stop the sweep. */
		if ((pid = fork()) == 0) {
			alarm(60);
			rc = workload(home, k, &fired);
			_exit(rc == 0 && !fired ? 3 : rc);
		}
		if (waitpid(pid, &status, 0) != pid)
			return (2);
		if (WIFSIGNALED(status)) {
			fprintf(stderr, "K=%ld: child killed by signal %d\n",
			    k, WTERMSIG(status));
			fails++;
			continue;
		}
		rc = WEXITSTATUS(status);
		if (rc == 3)		/* K beyond the workload: done. */
			break;
		hits++;
		if (rc == 1)
			fails++;
		else if (rc == 2) {
			fprintf(stderr, "K=%ld: setup error\n", k);
			fails++;
		}
	}
	if (fails != 0) {
		printf("VERDICT hash_split_leak FAIL %d of %d injected "
		    "failures leaked a lock or a pin, or crashed\n",
		    fails, hits);
		return (1);
	}
	printf("VERDICT hash_split_leak PASS %d injected allocation failures, "
	    "0 leaked locks or pins (%d unsorted pages)\n", hits, n);
	return (0);
#endif
}

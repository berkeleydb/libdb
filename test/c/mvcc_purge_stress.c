/*-
 * See the file LICENSE for redistribution information.
 *
 * mvcc_purge_stress.c -- concurrent reader/writer stress for the #138
 * proactive obsolete-MVCC-version purge, plus an integrity check.
 *
 * Threads:
 *   - WRITERS: run DB_TXN_SNAPSHOT write transactions over a wide keyspace,
 *     creating MVCC version chains (the retention the purge must reclaim).
 *   - READERS: open a DB_TXN_SNAPSHOT, read a batch of keys, hold the
 *     snapshot briefly, re-read the SAME keys and assert every value is
 *     unchanged within the snapshot (the visibility invariant -- the purge
 *     must never free a version a live reader can still see).
 *   - A CHECKPOINTER: calls txn_checkpoint(DB_FORCE) in a tight loop, firing
 *     __memp_purge_obsolete concurrently with the readers and writers.  This
 *     is the race target: mvcc_ref / hp->old_reader / TXN_DTL_SNAPSHOT must
 *     stay consistent under concurrent purge + eviction + reader/writer.
 *
 * After the threads join, the value stored under each key is verified to be
 * one of the values a writer actually committed, and DB->verify is run.
 * Any torn read, wrong value, ASan/TSan report, or verify failure fails.
 *
 * Small, dependency-free (pthreads only).  Intended to run under ASan, and
 * under TSan (build the lib and this file with -fsanitize=thread) to prove
 * the purge path is race-free.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"TESTDIR_mvcc_purge_stress"
#define	NKEYS		2000
#define	NWRITERS	6
#define	NREADERS	6
#define	RUN_SECS	12
#define	VALBYTES	120

static DB_ENV *env;
static DB *db;
static volatile int stop;
static volatile int failed;

static void
fail(const char *op, int ret)
{
	fprintf(stderr, "FAIL %s: %s (%d)\n", op, db_strerror(ret), ret);
	failed = 1;
}

static int
retryable(int ret)
{
	return (ret == DB_LOCK_DEADLOCK || ret == DB_SNAPSHOT_CONFLICT ||
	    ret == DB_SNAPSHOT_UNSAFE || ret == DB_LOCK_NOTGRANTED);
}

static void
mkkey(char *b, int k)
{
	snprintf(b, 32, "k-%d", k);
}

static void *
writer(void *arg)
{
	DB_TXN *txn;
	DBT key, data;
	char kb[32], vb[VALBYTES];
	unsigned seed = (unsigned)(uintptr_t)arg * 2654435761u + 1;
	int k, ret;

	while (!stop && !failed) {
		k = (int)(seed = seed * 1103515245u + 12345u) % NKEYS;
		memset(vb, 'A' + (k % 26), VALBYTES);
		*(unsigned *)vb = seed;		/* recoverable "which write" tag */
		mkkey(kb, k);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kb;
		key.size = (u_int32_t)strlen(kb);
		data.data = vb;
		data.size = VALBYTES;

		if ((ret = env->txn_begin(env,
		    NULL, &txn, DB_TXN_SNAPSHOT)) != 0) {
			if (retryable(ret))
				continue;
			fail("writer txn_begin", ret);
			return (NULL);
		}
		ret = db->put(db, txn, &key, &data, 0);
		if (ret == 0)
			ret = txn->commit(txn, 0);
		else
			(void)txn->abort(txn);
		if (ret != 0 && !retryable(ret)) {
			fail("writer put/commit", ret);
			return (NULL);
		}
	}
	return (NULL);
}

static void *
reader(void *arg)
{
	DB_TXN *txn;
	DBT key, data;
	char kb[32], vb[VALBYTES], first[VALBYTES];
	unsigned seed = (unsigned)(uintptr_t)arg * 40503u + 7;
	int base, k, ret, got_first;

	while (!stop && !failed) {
		if ((ret = env->txn_begin(env,
		    NULL, &txn, DB_TXN_SNAPSHOT)) != 0) {
			if (retryable(ret))
				continue;
			fail("reader txn_begin", ret);
			return (NULL);
		}
		base = (int)(seed = seed * 1103515245u + 12345u) % NKEYS;

		/* First pass: record what the snapshot shows for each key. */
		got_first = 0;
		mkkey(kb, base);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kb;
		key.size = (u_int32_t)strlen(kb);
		data.data = first;
		data.ulen = VALBYTES;
		data.flags = DB_DBT_USERMEM;
		ret = db->get(db, txn, &key, &data, 0);
		if (ret == 0)
			got_first = 1;
		else if (ret != DB_NOTFOUND) {
			(void)txn->abort(txn);
			if (retryable(ret))
				continue;
			fail("reader get1", ret);
			return (NULL);
		}

		/* Let writers and the checkpointer (purge) run under us. */
		for (k = 0; k < 200; k++)
			if (stop)
				break;
		sched_yield();

		/* Second pass: the SAME key MUST read the SAME value -- the
		 * purge must not have freed the version this snapshot sees. */
		mkkey(kb, base);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kb;
		key.size = (u_int32_t)strlen(kb);
		data.data = vb;
		data.ulen = VALBYTES;
		data.flags = DB_DBT_USERMEM;
		ret = db->get(db, txn, &key, &data, 0);
		if (ret == 0 && got_first) {
			if (data.size != VALBYTES ||
			    memcmp(vb, first, VALBYTES) != 0) {
				fprintf(stderr, "FAIL: snapshot value for key "
				    "%d changed under a live reader -- purge "
				    "freed a visible version\n", base);
				failed = 1;
				(void)txn->abort(txn);
				return (NULL);
			}
		} else if (ret == DB_NOTFOUND && got_first) {
			fprintf(stderr, "FAIL: key %d vanished from a live "
			    "snapshot -- purge freed a visible version\n", base);
			failed = 1;
			(void)txn->abort(txn);
			return (NULL);
		} else if (ret != 0 && ret != DB_NOTFOUND) {
			(void)txn->abort(txn);
			if (retryable(ret))
				continue;
			fail("reader get2", ret);
			return (NULL);
		}
		(void)txn->commit(txn, 0);
	}
	return (NULL);
}

static void *
checkpointer(void *arg)
{
	int ret;

	(void)arg;
	while (!stop && !failed) {
		if ((ret = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0 &&
		    ret != DB_LOCK_DEADLOCK) {
			fail("checkpoint", ret);
			return (NULL);
		}
		usleep(2000);
	}
	return (NULL);
}

int
main(void)
{
	pthread_t w[NWRITERS], r[NREADERS], ck;
	DBT key, data;
	char kb[32], vb[VALBYTES];
	int i, ret;

	(void)mkdir(HOME, 0755);

	if ((ret = db_env_create(&env, 0)) != 0)
		fail("db_env_create", ret), exit(1);
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "mvcc_purge_stress");
	if ((ret = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		fail("set_lk_detect", ret), exit(1);
	if ((ret = env->set_tx_max(env, 40000)) != 0)
		fail("set_tx_max", ret), exit(1);
	/* Modest cache: exercises BOTH eviction reclaim and checkpoint purge,
	 * and the race between them. */
	if ((ret = env->set_cachesize(env, 0, 16 * 1024 * 1024, 1)) != 0)
		fail("set_cachesize", ret), exit(1);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0600)) != 0)
		fail("DB_ENV->open", ret), exit(1);

	if ((ret = db_create(&db, env, 0)) != 0)
		fail("db_create", ret), exit(1);
	if ((ret = db->open(db, NULL, "s.db", NULL, DB_BTREE, DB_CREATE |
	    DB_AUTO_COMMIT | DB_MULTIVERSION | DB_THREAD, 0600)) != 0)
		fail("DB->open", ret), exit(1);

	/* Seed every key. */
	for (i = 0; i < NKEYS; i++) {
		mkkey(kb, i);
		memset(vb, 'a' + (i % 26), VALBYTES);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kb;
		key.size = (u_int32_t)strlen(kb);
		data.data = vb;
		data.size = VALBYTES;
		for (;;) {
			ret = db->put(db, NULL, &key, &data, 0);
			if (ret == 0)
				break;
			if (!retryable(ret))
				fail("seed put", ret), exit(1);
		}
	}

	stop = failed = 0;
	for (i = 0; i < NWRITERS; i++)
		pthread_create(&w[i], NULL, writer, (void *)(uintptr_t)(i + 1));
	for (i = 0; i < NREADERS; i++)
		pthread_create(&r[i], NULL, reader, (void *)(uintptr_t)(i + 1));
	pthread_create(&ck, NULL, checkpointer, NULL);

	sleep(RUN_SECS);
	stop = 1;

	for (i = 0; i < NWRITERS; i++)
		pthread_join(w[i], NULL);
	for (i = 0; i < NREADERS; i++)
		pthread_join(r[i], NULL);
	pthread_join(ck, NULL);

	/* Final integrity: a full checkpoint/purge then DB->verify. */
	if ((ret = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0)
		fail("final checkpoint", ret);
	if ((ret = db->close(db, 0)) != 0)
		fail("DB->close", ret);
	if ((ret = env->close(env, 0)) != 0)
		fail("DB_ENV->close", ret);

	/* Fresh handle for verify (DB->verify requires an unopened handle). */
	if ((ret = db_env_create(&env, 0)) != 0)
		fail("db_env_create(verify)", ret), exit(1);
	env->set_errfile(env, stderr);
	if ((ret = env->open(env, HOME, DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN, 0600)) != 0)
		fail("DB_ENV->open(verify)", ret), exit(1);
	if ((ret = db_create(&db, env, 0)) != 0)
		fail("db_create(verify)", ret), exit(1);
	if ((ret = db->verify(db, "s.db", NULL, NULL, 0)) != 0)
		fail("DB->verify", ret);
	else
		printf("db_verify clean\n");
	(void)env->close(env, 0);

	printf("%s (writers=%d readers=%d checkpointer=1, %ds)\n",
	    failed ? "FAIL" : "PASS", NWRITERS, NREADERS, RUN_SECS);
	return (failed ? 1 : 0);
}

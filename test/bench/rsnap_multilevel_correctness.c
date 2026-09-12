/*
 * rsnap_multilevel_correctness.c
 *
 * Correctness gate for the wired multi-level snapshot descent (ROADMAP #2).
 *
 * 1. Build a tall B-tree (small pages -> >=3 levels for a modest key count)
 *    and print the tree height so we KNOW the multi-level path is exercised.
 * 2. Verify every key reads back its expected value (single-threaded), which
 *    forces the snapshot descent to choose byte-identical children.
 * 3. Concurrent phase: N reader threads descend while a writer inserts/deletes
 *    keys (causing internal-page splits/merges, which invalidate cache slots).
 *    Every reader compares the returned value to the invariant-checkable
 *    expected value; ANY mismatch is a hard failure (a wrong child bug).
 * 4. Return an exit code; caller runs db_verify afterwards.
 *
 * Value encoding: key K present => value is the ASCII of "v<K>" padded; a
 * reader that gets a value must find it equals the encoding for the key it
 * asked for -- proving the descent landed on the right leaf/child.
 *
 *   cc -O2 -pthread rsnap_multilevel_correctness.c -I<build> \
 *       -L<build>/.libs -ldb-5.3 -o rr
 */
#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "db.h"

static DB_ENV *env;
static DB *db;
static volatile int stop;
static volatile int go;
static uint32_t g_nkeys;
static volatile long g_reads;
static volatile long g_mismatch;

static void
encode_val(uint32_t k, char *buf, size_t n)
{
	memset(buf, 0, n);
	snprintf(buf, n, "v%010u", k);
}

static void
fill_key(DBT *d, uint32_t *kb, uint32_t v)
{
	*kb = v;
	memset(d, 0, sizeof(*d));
	d->data = kb;
	d->size = sizeof(*kb);
}

/* Reader: pick random keys, read, and check the value matches the key. */
static void *
reader(void *a)
{
	unsigned seed = (unsigned)(uintptr_t)a * 2654435761u + 1;
	DBT key, data;
	uint32_t kb;
	char got[64], want[64];
	int ret;

	while (!go) { }
	while (!stop) {
		uint32_t k = (uint32_t)(rand_r(&seed) % g_nkeys);
		fill_key(&key, &kb, k);
		memset(&data, 0, sizeof(data));
		data.data = got;
		data.ulen = sizeof(got);
		data.flags = DB_DBT_USERMEM;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0) {
			encode_val(k, want, sizeof(want));
			if (memcmp(got, want, 12) != 0) {
				/* WRONG CHILD: returned another key's value. */
				fprintf(stderr,
				    "MISMATCH key=%u got='%.12s' want='%.12s'\n",
				    k, got, want);
				__sync_fetch_and_add(&g_mismatch, 1);
			}
		} else if (ret != DB_NOTFOUND) {
			fprintf(stderr, "reader get k=%u ret=%d\n", k, ret);
			__sync_fetch_and_add(&g_mismatch, 1);
		}
		__sync_fetch_and_add(&g_reads, 1);
	}
	return (NULL);
}

/* Writer: churn keys near the top of the range to force splits/merges. */
static void *
writer(void *a)
{
	unsigned seed = 0xdeadbeef;
	DBT key, data;
	uint32_t kb;
	char v[64];
	int ret;

	(void)a;
	while (!go) { }
	while (!stop) {
		uint32_t k = g_nkeys + (uint32_t)(rand_r(&seed) % g_nkeys);
		fill_key(&key, &kb, k);
		encode_val(k, v, sizeof(v));
		memset(&data, 0, sizeof(data));
		data.data = v;
		data.size = 20;
		/* Insert then delete to keep the working set churning. */
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0 &&
		    ret != DB_KEYEXIST)
			fprintf(stderr, "writer put k=%u ret=%d\n", k, ret);
		if (rand_r(&seed) & 1)
			(void)db->del(db, NULL, &key, 0);
	}
	return (NULL);
}

static u_int32_t
tree_height(void)
{
	DB_BTREE_STAT *st = NULL;
	u_int32_t levels = 0;
	if (db->stat(db, NULL, &st, 0) == 0 && st != NULL) {
		levels = st->bt_levels;
		free(st);
	}
	return (levels);
}

int
main(int argc, char **argv)
{
	DBT key, data;
	uint32_t kb, i;
	char v[64];
	int ret, nthreads, ai;
	pthread_t rt[256], wt;
	int secs;

	if (argc < 4) {
		fprintf(stderr,
		    "usage: %s <nkeys> <readers> <secs>\n", argv[0]);
		return (2);
	}
	g_nkeys = (uint32_t)atoi(argv[1]);
	nthreads = atoi(argv[2]);
	secs = atoi(argv[3]);

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "env_create %d\n", ret);
		return (1);
	}
	env->set_errfile(env, stderr);
	env->set_cachesize(env, 0, 256 * 1024 * 1024, 1);
	(void)env->set_flags(env, DB_TXN_NOSYNC, 1);	/* fast load; not the SUT */
	if ((ret = env->open(env, "./RRDB", DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_TXN | DB_INIT_LOG | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "env open");
		return (1);
	}
	if ((ret = db_create(&db, env, 0)) != 0) {
		env->err(env, ret, "db_create");
		return (1);
	}
	/* Tiny pages so a modest key count yields a >=3-level tree. */
	(void)db->set_pagesize(db, 512);
	if ((ret = db->open(db, NULL, "rr.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "db open");
		return (1);
	}

	/* Load [0, nkeys). */
	for (i = 0; i < g_nkeys; i++) {
		fill_key(&key, &kb, i);
		encode_val(i, v, sizeof(v));
		memset(&data, 0, sizeof(data));
		data.data = v;
		data.size = 20;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
			env->err(env, ret, "load");
			return (1);
		}
	}
	printf("# loaded %u keys, pagesize=512, tree_levels=%u\n",
	    g_nkeys, tree_height());
	if (tree_height() < 3) {
		fprintf(stderr, "FAIL: tree height <3, multi-level not exercised\n");
		return (1);
	}

	/* Phase 1: single-threaded full read-back check. */
	for (i = 0; i < g_nkeys; i++) {
		char got[64], want[64];
		fill_key(&key, &kb, i);
		memset(&data, 0, sizeof(data));
		data.data = got;
		data.ulen = sizeof(got);
		data.flags = DB_DBT_USERMEM;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "FAIL: readback miss key=%u ret=%d\n",
			    i, ret);
			return (1);
		}
		encode_val(i, want, sizeof(want));
		if (memcmp(got, want, 12) != 0) {
			fprintf(stderr, "FAIL: readback wrong key=%u got='%.12s'"
			    " want='%.12s'\n", i, got, want);
			return (1);
		}
	}
	printf("PASS phase1: single-threaded read-back of %u keys correct\n",
	    g_nkeys);

	/* Phase 2: concurrent readers + writer with splits. */
	stop = 0; go = 0; g_reads = 0; g_mismatch = 0;
	for (ai = 0; ai < nthreads; ai++)
		pthread_create(&rt[ai], NULL, reader,
		    (void *)(uintptr_t)(ai + 1));
	pthread_create(&wt, NULL, writer, NULL);
	go = 1;
	sleep(secs);
	stop = 1;
	for (ai = 0; ai < nthreads; ai++)
		pthread_join(rt[ai], NULL);
	pthread_join(wt, NULL);

	printf("phase2: %ld reads across %d readers, %ld mismatches, "
	    "final tree_levels=%u\n",
	    g_reads, nthreads, g_mismatch, tree_height());
	if (g_mismatch != 0) {
		fprintf(stderr, "FAIL: %ld wrong-child mismatches\n", g_mismatch);
		return (1);
	}
	printf("PASS phase2: concurrent readers never saw a wrong child\n");

	db->close(db, 0);
	env->close(env, 0);
	return (0);
}

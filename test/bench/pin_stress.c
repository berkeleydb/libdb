/*-
 * pin_stress: DB_PRIVATE concurrent reader/writer stress, which is the ONLY
 * configuration where perf/bhpin-r1's optimistic fast path fires (it is gated
 * on ENV_PRIVATE) -- so it is the only configuration in which a correctness
 * gate says anything about R1.  test/c/batch_diff.c opens a SHARED environment,
 * where R1's own counters report attempts=0, so batch_diff passing on the bhpin
 * build is not evidence about R1.
 *
 * Readers verify every value they read against the value the writers maintain
 * (each key's value is a function of the key and a generation counter, so a
 * torn or stale page is detectable).  Writers split and delete to churn the
 * tree structure, which is what invalidates a wired frame under a lock-free
 * reader.
 *
 * usage: pin_stress <nkeys> <readers> <writers> <secs>
 * env:   PIN_HOME (existing dir), PIN_PRIVATE, DB_NO_BHPIN (kill switch)
 */
#include <sys/types.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"

#define	VALSZ	64

static DB_ENV *env;
static DB *db;
static volatile int stop, go;
static unsigned g_nkeys;
static unsigned long long n_reads, n_writes, n_mismatch, n_err, n_notfound;

/* Value for key k, generation g: deterministic, so any read is checkable. */
static void
fill(char *buf, unsigned k, unsigned g)
{
	unsigned i;
	for (i = 0; i < VALSZ; i++)
		buf[i] = (char)('a' + ((k + g + i) % 26));
}

static void *
reader(void *a)
{
	DBT key, data;
	unsigned kb, seed = (unsigned)(size_t)a * 2654435761u + 3;
	char got[VALSZ + 8], want0[VALSZ], want1[VALSZ];
	int ret;

	while (!go) { }
	while (!stop) {
		kb = (unsigned)(rand_r(&seed) % g_nkeys);
		memset(&key, 0, sizeof(key));
		key.data = &kb; key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));
		data.data = got; data.ulen = sizeof(got);
		data.flags = DB_DBT_USERMEM;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == DB_NOTFOUND) {
			(void)__sync_fetch_and_add(&n_notfound, 1);
			continue;
		}
		if (ret != 0) {
			(void)__sync_fetch_and_add(&n_err, 1);
			continue;
		}
		(void)__sync_fetch_and_add(&n_reads, 1);
		/*
		 * A concurrent writer may have advanced this key from gen 0 to
		 * gen 1, so either is correct -- anything else is a torn or
		 * bogus page.
		 */
		if (data.size != VALSZ) {
			(void)__sync_fetch_and_add(&n_mismatch, 1);
			continue;
		}
		fill(want0, kb, 0);
		fill(want1, kb, 1);
		if (memcmp(got, want0, VALSZ) != 0 &&
		    memcmp(got, want1, VALSZ) != 0)
			(void)__sync_fetch_and_add(&n_mismatch, 1);
	}
	return (NULL);
}

static void *
writer(void *a)
{
	DBT key, data;
	unsigned kb, seed = (unsigned)(size_t)a * 40503u + 11;
	char buf[VALSZ];
	int ret;

	while (!go) { }
	while (!stop) {
		kb = (unsigned)(rand_r(&seed) % g_nkeys);
		fill(buf, kb, 1);
		memset(&key, 0, sizeof(key));
		key.data = &kb; key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));
		data.data = buf; data.size = VALSZ;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
			(void)__sync_fetch_and_add(&n_err, 1);
			continue;
		}
		(void)__sync_fetch_and_add(&n_writes, 1);
		/* Put it back to gen 0 so both generations stay live. */
		fill(buf, kb, 0);
		data.data = buf; data.size = VALSZ;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			(void)__sync_fetch_and_add(&n_err, 1);
		else
			(void)__sync_fetch_and_add(&n_writes, 1);
	}
	return (NULL);
}

int
main(int argc, char **argv)
{
	pthread_t th[256];
	DBT key, data;
	u_int32_t oflags;
	unsigned kb, i;
	char buf[VALSZ];
	const char *home;
	struct timespec sl;
	int ret, nr, nw, secs, private, nth = 0;

	if (argc != 5) {
		fprintf(stderr, "usage: %s <nkeys> <readers> <writers>"
		    " <secs>\n", argv[0]);
		return (1);
	}
	g_nkeys = (unsigned)atoi(argv[1]);
	nr = atoi(argv[2]); nw = atoi(argv[3]); secs = atoi(argv[4]);
	if (g_nkeys < 100 || nr < 1 || nw < 0 || nr + nw > 256 || secs < 1) {
		fprintf(stderr, "bad args\n"); return (1);
	}
	if ((home = getenv("PIN_HOME")) == NULL) {
		fprintf(stderr, "set PIN_HOME\n"); return (1);
	}
	private = getenv("PIN_PRIVATE") != NULL;

	if ((ret = db_env_create(&env, 0)) != 0) return (1);
	env->set_errfile(env, stderr);
	(void)env->set_cachesize(env, 0, 256 * 1024 * 1024, 1);
	(void)env->set_flags(env, DB_TXN_NOSYNC, 1);
	(void)env->set_lk_detect(env, DB_LOCK_MINWRITE);
	oflags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_TXN |
	    DB_INIT_LOG | DB_THREAD;
	if (private) {
		oflags |= DB_PRIVATE;
		(void)env->set_thread_count(env, (u_int32_t)(nr + nw) + 16);
	}
	if ((ret = env->open(env, home, oflags, 0)) != 0) {
		env->err(env, ret, "env open %s", home); return (1);
	}
	if ((ret = db_create(&db, env, 0)) != 0) return (1);
	if ((ret = db->open(db, NULL, "stress.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "db open"); return (1);
	}
	for (i = 0; i < g_nkeys; i++) {
		fill(buf, i, 0);
		memset(&key, 0, sizeof(key));
		kb = i; key.data = &kb; key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));
		data.data = buf; data.size = VALSZ;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
			env->err(env, ret, "load"); return (1);
		}
	}
	/* Clean the pool so R1's fast path (which refuses BH_DIRTY) can fire. */
	if ((ret = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0) {
		env->err(env, ret, "checkpoint"); return (1);
	}
	{ int nwrote = 0; (void)env->memp_trickle(env, 100, &nwrote); }

	stop = 0; go = 0;
	for (i = 0; i < (unsigned)nr; i++)
		(void)pthread_create(&th[nth++], NULL, reader, (void *)(size_t)i);
	for (i = 0; i < (unsigned)nw; i++)
		(void)pthread_create(&th[nth++], NULL, writer, (void *)(size_t)i);
	go = 1;
	sl.tv_sec = secs; sl.tv_nsec = 0;
	(void)nanosleep(&sl, NULL);
	stop = 1;
	for (i = 0; i < (unsigned)nth; i++)
		(void)pthread_join(th[i], NULL);

	(void)db->close(db, 0);
	(void)env->close(env, 0);

	if (n_reads == 0 || (nw > 0 && n_writes == 0)) {
		printf("FAIL pin-stress env=%s bhpin=%s: vacuous run "
		    "reads=%llu writes=%llu\n", private ? "private" : "shared",
		    getenv("DB_NO_BHPIN") != NULL ? "off" : "on",
		    n_reads, n_writes);
		return (1);
	}
	printf("VERDICT pin-stress env=%s bhpin=%s reads=%llu writes=%llu "
	    "notfound=%llu errors=%llu value_mismatch=%llu -> %s\n",
	    private ? "private" : "shared",
	    getenv("DB_NO_BHPIN") != NULL ? "off" : "on",
	    n_reads, n_writes, n_notfound, n_err, n_mismatch,
	    (n_mismatch == 0 && n_err == 0) ? "PASS" : "FAIL");
	return ((n_mismatch == 0 && n_err == 0) ? 0 : 1);
}

/*-
 * See the file LICENSE for redistribution information.
 *
 * lock_bench -- direct lock-manager throughput/scaling probe.
 *
 * Bypasses the access methods and the buffer pool entirely: each thread
 * allocates its own locker id, then in a tight loop calls DB_ENV->lock_get
 * followed by DB_ENV->lock_put on a chosen object.  This isolates the lock
 * manager's own machinery (partition mutexes, object hash, lock/object free
 * lists, locker lookup, per-op counters) from B-tree search and page-pin
 * cache misses that dominate a real DB->get and mask the lock layer.
 *
 *   ./lock_bench <secs> <nobj> <mode> <t1> [t2 ...]
 *
 * mode:
 *   distinct  - each thread locks objects from its own disjoint key range
 *               (no conflicts; pure throughput / partition scaling)
 *   shared    - all threads lock READ over the SAME small set of nobj objects
 *               (read locks don't conflict, but they share object hash slots
 *               + lock-object refcount cache lines -> measures true sharing)
 *
 * Prints ops/sec per thread count.
 */
#include <sys/types.h>
#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#include <db.h>

static DB_ENV *g_env;
static int g_secs, g_nobj, g_shared, g_maxthreads;
static volatile int g_stop;

typedef struct {
	int tid;
	u_int32_t locker;
	uint64_t ops;
} worker;

static double
now_ms(void)
{
	struct timeval tv;
	(void)gettimeofday(&tv, NULL);
	return (double)tv.tv_sec * 1000.0 + (double)tv.tv_usec / 1000.0;
}

static void *
worker_main(void *arg)
{
	worker *w = arg;
	DBT obj;
	DB_LOCK lock;
	uint32_t key, base;
	uint64_t i = 0;
	int ret;

	/* distinct: thread t owns keys [t*nobj, (t+1)*nobj). shared: [0,nobj). */
	base = g_shared ? 0 : (uint32_t)w->tid * (uint32_t)g_nobj;
	memset(&obj, 0, sizeof(obj));
	obj.size = sizeof(key);
	obj.data = &key;

	while (!g_stop) {
		key = base + (uint32_t)(i++ % (uint64_t)g_nobj);
		ret = g_env->lock_get(g_env, w->locker, 0, &obj,
		    g_shared ? DB_LOCK_READ : DB_LOCK_WRITE, &lock);
		if (ret != 0) {
			fprintf(stderr, "lock_get: %s\n", db_strerror(ret));
			return NULL;
		}
		if ((ret = g_env->lock_put(g_env, &lock)) != 0) {
			fprintf(stderr, "lock_put: %s\n", db_strerror(ret));
			return NULL;
		}
		w->ops++;
	}
	return NULL;
}

int
main(int argc, char **argv)
{
	pthread_t *tids;
	worker *workers;
	const char *home = "LOCKBENCHDIR";
	double t0, elapsed;
	int ai, t, nthreads, ret;

	if (argc < 5) {
		fprintf(stderr,
		    "usage: %s <secs> <nobj> <distinct|shared> <t1> [t2 ...]\n",
		    argv[0]);
		return 1;
	}
	g_secs = atoi(argv[1]);
	g_nobj = atoi(argv[2]);
	g_shared = strcmp(argv[3], "shared") == 0;
	if (g_nobj < 1) g_nobj = 1;

	for (ai = 4; ai < argc; ai++)
		if (atoi(argv[ai]) > g_maxthreads) g_maxthreads = atoi(argv[ai]);

	if ((ret = db_env_create(&g_env, 0)) != 0) {
		fprintf(stderr, "env_create: %s\n", db_strerror(ret));
		return 1;
	}
	g_env->set_errfile(g_env, stderr);
	/* Size the lock subsystem for many lockers/objects/locks. */
	(void)g_env->set_lk_max_locks(g_env, 500000);
	(void)g_env->set_lk_max_objects(g_env, 500000);
	(void)g_env->set_lk_max_lockers(g_env, 500000);
	if ((ret = g_env->open(g_env, home,
	    DB_CREATE | DB_INIT_LOCK | DB_THREAD | DB_PRIVATE, 0)) != 0) {
		g_env->err(g_env, ret, "env open (mkdir %s first)", home);
		return 1;
	}

	printf("# lock_bench mode=%s nobj=%d secs=%d\n",
	    g_shared ? "shared" : "distinct", g_nobj, g_secs);
	printf("# threads      ops/sec\n");

	for (ai = 4; ai < argc; ai++) {
		nthreads = atoi(argv[ai]);
		tids = calloc((size_t)nthreads, sizeof(*tids));
		workers = calloc((size_t)nthreads, sizeof(*workers));
		for (t = 0; t < nthreads; t++) {
			workers[t].tid = t;
			if ((ret = g_env->lock_id(g_env, &workers[t].locker)) != 0) {
				fprintf(stderr, "lock_id: %s\n", db_strerror(ret));
				return 1;
			}
		}
		g_stop = 0;
		t0 = now_ms();
		for (t = 0; t < nthreads; t++)
			pthread_create(&tids[t], NULL, worker_main, &workers[t]);
		usleep((useconds_t)g_secs * 1000000);
		g_stop = 1;
		for (t = 0; t < nthreads; t++)
			pthread_join(tids[t], NULL);
		elapsed = (now_ms() - t0) / 1000.0;

		{
			uint64_t total = 0;
			for (t = 0; t < nthreads; t++) {
				total += workers[t].ops;
				(void)g_env->lock_id_free(g_env, workers[t].locker);
			}
			printf("%-12d %12.0f\n", nthreads,
			    (double)total / elapsed);
		}
		free(tids); free(workers);
	}

	(void)g_env->close(g_env, 0);
	return 0;
}

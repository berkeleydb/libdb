/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * dtrace_drv -- a small durable-commit workload for the `bdb' probes.
 *
 * usage: dtrace_drv <home> [threads] [txns-per-thread]
 *
 * Each thread runs begin / put / commit (synchronous, so every commit asks
 * for a log flush), then the main thread checkpoints, so txn-*, db-put,
 * log-flush_*, lock-* and mpool-write all fire.  Used by
 * test/db/run_dtrace_probes.sh and by the worked example in
 * docs_src/guides/programmer_reference/program_perfmon.md.
 */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

static DB_ENV *dbenv;
static DB *dbp;
static int ntxn;

static void *
worker(void *arg)
{
	DBT key, data;
	DB_TXN *txn;
	char buf[32];
	int i, ret;

	for (i = 0; i < ntxn; i++) {
		(void)snprintf(buf, sizeof(buf), "%ld-%d", (long)arg, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = data.data = buf;
		key.size = data.size = (u_int32_t)strlen(buf);
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0 ||
		    (ret = dbp->put(dbp, txn, &key, &data, 0)) != 0 ||
		    (ret = txn->commit(txn, 0)) != 0) {
			fprintf(stderr, "dtrace_drv: %s\n", db_strerror(ret));
			exit(1);
		}
	}
	return (NULL);
}

int
main(int argc, char *argv[])
{
	pthread_t tid[64];
	long i;
	int nthr, ret;

	if (argc < 2) {
		fprintf(stderr, "usage: dtrace_drv <home> [threads] [txns]\n");
		return (2);
	}
	nthr = argc > 2 ? atoi(argv[2]) : 4;
	ntxn = argc > 3 ? atoi(argv[3]) : 500;
	if (nthr < 1 || nthr > 64 || ntxn < 1) {
		fprintf(stderr, "dtrace_drv: threads must be 1..64\n");
		return (2);
	}
	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->open(dbenv, argv[1], DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD |
	    DB_RECOVER, 0)) != 0 ||
	    (ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, "drv.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "dtrace_drv: open: %s\n", db_strerror(ret));
		return (1);
	}
	for (i = 0; i < nthr; i++)
		if (pthread_create(&tid[i], NULL, worker, (void *)i) != 0)
			return (1);
	for (i = 0; i < nthr; i++)
		(void)pthread_join(tid[i], NULL);
	if ((ret = dbenv->txn_checkpoint(dbenv, 0, 0, 0)) != 0 ||
	    (ret = dbp->close(dbp, 0)) != 0 ||
	    (ret = dbenv->close(dbenv, 0)) != 0) {
		fprintf(stderr, "dtrace_drv: close: %s\n", db_strerror(ret));
		return (1);
	}
	printf("VERDICT dtrace_drv PASS commits=%d\n", nthr * ntxn);
	return (0);
}

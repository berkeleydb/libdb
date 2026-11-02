/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * xrel_evict_probe -- the sequential-insert pattern of test_micro's b_load
 * (DB_PRIVATE mpool-only env, no transactions) with the cache sized by an
 * argument.  Prints throughput and the mpool counters that show how hard
 * __memp_alloc works: dirty evictions and hash buckets examined per allocation.
 *
 * usage: xrel_evict_probe <queue|btree> <records> <home> <cache_kb>
 * output: am cache_kb ops_per_s rw_evict buckets_per_alloc
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <db.h>
int main(int argc, char **argv) {
	DB_ENV *e; DB *d; DBT k, v; DB_MPOOL_STAT *s; DBTYPE t;
	char kb[10], vb[20]; db_recno_t rn; int i, n = atoi(argv[2]); u_int32_t ck = (u_int32_t)atoi(argv[4]);
	struct timeval a, b;
	t = strcmp(argv[1], "queue") == 0 ? DB_QUEUE : DB_BTREE;
	db_env_create(&e, 0);
	e->set_cachesize(e, ck / (1024 * 1024), (ck % (1024 * 1024)) * 1024, 1);
	e->open(e, argv[3], DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0);
	db_create(&d, e, 0);
	if (t == DB_QUEUE) d->set_re_len(d, 20);
	d->open(d, NULL, "p.db", NULL, t, DB_CREATE, 0644);
	memset(&k, 0, sizeof k); memset(&v, 0, sizeof v); memset(vb, 'v', sizeof vb); v.data = vb; v.size = 20;
	gettimeofday(&a, NULL);
	for (i = 0; i < n; i++) {
		if (t == DB_QUEUE) { k.data = &rn; k.ulen = sizeof rn; k.flags = DB_DBT_USERMEM; d->put(d, NULL, &k, &v, DB_APPEND); }
		else { snprintf(kb, sizeof kb, "%09d", i); k.data = kb; k.size = 10; d->put(d, NULL, &k, &v, 0); }
	}
	gettimeofday(&b, NULL);
	e->memp_stat(e, &s, NULL, 0);
	printf("%s\t%u\t%.0f\t%lu\t%.1f\n", argv[1], ck, n / ((b.tv_sec - a.tv_sec) + (b.tv_usec - a.tv_usec) / 1e6),
	    (u_long)s->st_rw_evict, s->st_alloc ? (double)s->st_alloc_buckets / s->st_alloc : 0);
	d->close(d, 0); e->close(e, 0); return 0;
}

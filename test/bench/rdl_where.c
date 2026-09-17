/*
 * rdl_where.c -- which page does a read descent lock, and how many times is
 * __db_lget even CALLED?
 *
 * A stat-based count (rdl_count.c) counts lock_get REQUESTS, so it cannot
 * distinguish "lget not called" from "lget called and short-circuited".  This
 * probe does one read of one key and then dumps the locks the transaction
 * holds, with page numbers, so the locked page can be compared against the
 * tree's leaf.  Run under the gdb script rdl_where.gdb to also count calls.
 *
 * Usage: rdl_where -h HOME [-m]   (-m = DB_MULTIVERSION db + snapshot txn)
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "db.h"

#define	CK(call) do {							\
	int _r = (call);						\
	if (_r != 0) {							\
		fprintf(stderr, "FAIL %s:%d %s: %s\n", __FILE__,	\
		    __LINE__, #call, db_strerror(_r));			\
		exit(1);						\
	}								\
} while (0)

int
main(int argc, char *argv[])
{
	DB *dbp;
	DB_ENV *dbenv;
	DB_TXN *txn;
	DBT key, data;
	DB_BTREE_STAT *bs;
	char *home = NULL;
	char kb[32], vb[64];
	u_int32_t dbflags, txnflags;
	int ch, i, mv = 0, ssi = 0;

	while ((ch = getopt(argc, argv, "h:ms")) != EOF)
		switch (ch) {
		case 'h': home = optarg; break;
		case 'm': mv = 1; break;
		case 's': mv = 1; ssi = 1; break;
		default: exit(2);
		}
	if (home == NULL) { fprintf(stderr, "-h HOME required\n"); exit(2); }

	CK(db_env_create(&dbenv, 0));
	CK(dbenv->set_cachesize(dbenv, 0, 64 * 1024 * 1024, 1));
	CK(dbenv->set_lk_max_locks(dbenv, 50000));
	CK(dbenv->set_lk_max_objects(dbenv, 50000));
	CK(dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN | DB_THREAD, 0644));

	dbflags = DB_CREATE | DB_AUTO_COMMIT | DB_THREAD;
	if (mv)
		dbflags |= DB_MULTIVERSION;
	CK(db_create(&dbp, dbenv, 0));
	CK(dbp->set_pagesize(dbp, 512));
	CK(dbp->open(dbp, NULL, "t.db", NULL, DB_BTREE, dbflags, 0644));

	CK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	for (i = 0; i < 20000; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		snprintf(kb, sizeof(kb), "k%08d", i);
		snprintf(vb, sizeof(vb), "v%060d", i);
		key.data = kb; key.size = (u_int32_t)strlen(kb) + 1;
		data.data = vb; data.size = (u_int32_t)strlen(vb) + 1;
		CK(dbp->put(dbp, txn, &key, &data, 0));
		if ((i % 2000) == 1999) {
			CK(txn->commit(txn, 0));
			CK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
		}
	}
	CK(txn->commit(txn, 0));

	CK(dbp->stat(dbp, NULL, &bs, 0));
	printf("RDLW levels=%u internal_pg=%u leaf_pg=%u\n",
	    bs->bt_levels, bs->bt_int_pg, bs->bt_leaf_pg);
	free(bs);

	txnflags = ssi ? DB_TXN_SERIALIZABLE : (mv ? DB_TXN_SNAPSHOT : 0);
	CK(dbenv->txn_begin(dbenv, NULL, &txn, txnflags));

	/* The single measured read.  gdb counts __db_lget calls across it. */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	snprintf(kb, sizeof(kb), "k%08d", 12345);
	key.data = kb; key.size = (u_int32_t)strlen(kb) + 1;
	data.flags = DB_DBT_MALLOC;
	printf("RDLW mode=%s begin_read\n",
	    ssi ? "serializable" : mv ? "snapshot" : "plain");
	fflush(stdout);
	CK(dbp->get(dbp, txn, &key, &data, 0));
	printf("RDLW end_read\n");
	fflush(stdout);
	free(data.data);

	/* Dump held locks WITH page numbers, before commit releases them. */
	CK(dbenv->lock_stat_print(dbenv, DB_STAT_LOCK_LOCKERS));

	CK(txn->commit(txn, 0));
	CK(dbp->close(dbp, 0));
	CK(dbenv->close(dbenv, 0));
	printf("RDLW done\n");
	return (0);
}

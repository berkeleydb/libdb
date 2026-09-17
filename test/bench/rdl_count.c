/*
 * rdl_count.c -- ground-truth counter for the read-descent lock question.
 *
 * Builds a btree deep enough to have interior levels, then does N point reads
 * under each isolation level and reports, per read:
 *	locks	= DB_LOCK_STAT.st_nrequests delta / N	(lock_get calls)
 *	pages	= DB_MPOOL_STAT cache_hit+miss delta / N	(memp_fget calls)
 *	sireads = DB_LOCK_STAT.st_nlocks (live) after the pass
 *
 * The point: if locks/read < levels, the per-level DB_LOCK_READ claim is false.
 *
 * Usage: rdl_count [-h HOME] [-n NREADS] [-k NKEYS] [-p PAGESIZE]
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

static int nkeys = 20000;
static int nreads = 2000;
static u_int32_t pgsz = 512;

static void
mkkey(char *buf, int i)
{
	snprintf(buf, 32, "k%08d", i);
}

/*
 * One measured pass.  txnflags: flags for txn_begin (0, DB_TXN_SNAPSHOT,
 * DB_TXN_SERIALIZABLE).  notxn: read outside any transaction.
 * cflags: cursor/get flags (DB_READ_UNCOMMITTED).
 */
static void
pass(DB_ENV *dbenv, DB *dbp, const char *label,
    int notxn, u_int32_t txnflags, u_int32_t getflags, int levels)
{
	DB_LOCK_STAT *ls0, *ls1;
	DB_MPOOL_STAT *ms0, *ms1;
	DB_TXN *txn;
	DBT key, data;
	char kb[32];
	int i, ret;
	double lpr, ppr;
	u_int32_t nreq0, nrel0, nreq1, nrel1, nlk1, nsi1;
	u_int64_t pg0, pg1;

	/*
	 * DB_STAT_CLEAR returns the accumulated values AND THEN zeroes them, so
	 * the pre-pass baseline is by construction zero -- do not subtract the
	 * returned (pre-clear) values, that underflows.  That bug produced
	 * 2147483647-per-read nonsense on the first run of this probe.
	 */
	CK(dbenv->lock_stat(dbenv, &ls0, DB_STAT_CLEAR));
	CK(dbenv->memp_stat(dbenv, &ms0, NULL, DB_STAT_CLEAR));
	nreq0 = nrel0 = 0;
	pg0 = 0;
	free(ls0); free(ms0);

	txn = NULL;
	if (!notxn)
		CK(dbenv->txn_begin(dbenv, NULL, &txn, txnflags));

	for (i = 0; i < nreads; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		mkkey(kb, (i * 7919) % nkeys);	/* scattered, all distinct */
		key.data = kb; key.size = (u_int32_t)strlen(kb) + 1;
		data.flags = DB_DBT_MALLOC;
		if ((ret = dbp->get(dbp, txn, &key, &data, getflags)) != 0) {
			fprintf(stderr, "FAIL get %s: %s\n", label,
			    db_strerror(ret));
			exit(1);
		}
		free(data.data);
	}

	/* Sample lock stats BEFORE commit: commit releases everything. */
	CK(dbenv->lock_stat(dbenv, &ls1, 0));
	CK(dbenv->memp_stat(dbenv, &ms1, NULL, 0));
	nreq1 = ls1->st_nrequests; nrel1 = ls1->st_nreleases;
	nlk1 = ls1->st_nlocks; nsi1 = ls1->st_nlockers;
	pg1 = (u_int64_t)ms1->st_cache_hit + ms1->st_cache_miss;
	free(ls1); free(ms1);

	if (txn != NULL)
		CK(txn->commit(txn, 0));

	lpr = (double)(nreq1 - nreq0) / nreads;
	ppr = (double)(pg1 - pg0) / nreads;
	printf("RDL %-22s levels=%d locks/read=%.3f pages/read=%.3f "
	    "held_at_end=%u lockers=%u releases/read=%.3f\n",
	    label, levels, lpr, ppr, nlk1, nsi1,
	    (double)(nrel1 - nrel0) / nreads);
	fflush(stdout);
}

int
main(int argc, char *argv[])
{
	DB *dbp, *dbmv;
	DB_ENV *dbenv;
	DB_TXN *txn;
	DBT key, data;
	DB_BTREE_STAT *bs;
	char *home = NULL;
	char kb[32], vb[64];
	int ch, i, levels, mvlevels;

	while ((ch = getopt(argc, argv, "h:n:k:p:")) != EOF)
		switch (ch) {
		case 'h': home = optarg; break;
		case 'n': nreads = atoi(optarg); break;
		case 'k': nkeys = atoi(optarg); break;
		case 'p': pgsz = (u_int32_t)atoi(optarg); break;
		default: exit(2);
		}
	if (home == NULL) { fprintf(stderr, "-h HOME required\n"); exit(2); }

	CK(db_env_create(&dbenv, 0));
	CK(dbenv->set_cachesize(dbenv, 0, 256 * 1024 * 1024, 1));
	CK(dbenv->set_lk_max_locks(dbenv, 200000));
	CK(dbenv->set_lk_max_objects(dbenv, 200000));
	CK(dbenv->set_lk_max_lockers(dbenv, 20000));
	CK(dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN | DB_THREAD, 0644));

	/* Plain (non-multiversion) database. */
	CK(db_create(&dbp, dbenv, 0));
	CK(dbp->set_pagesize(dbp, pgsz));
	CK(dbp->open(dbp, NULL, "plain.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644));

	/* DB_MULTIVERSION database (needed for SI/SSI read paths). */
	CK(db_create(&dbmv, dbenv, 0));
	CK(dbmv->set_pagesize(dbmv, pgsz));
	CK(dbmv->open(dbmv, NULL, "mv.db", NULL, DB_BTREE, DB_CREATE |
	    DB_AUTO_COMMIT | DB_THREAD | DB_MULTIVERSION, 0644));

	CK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	for (i = 0; i < nkeys; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		mkkey(kb, i);
		snprintf(vb, sizeof(vb), "v%060d", i);
		key.data = kb; key.size = (u_int32_t)strlen(kb) + 1;
		data.data = vb; data.size = (u_int32_t)strlen(vb) + 1;
		CK(dbp->put(dbp, txn, &key, &data, 0));
		CK(dbmv->put(dbmv, txn, &key, &data, 0));
		if ((i % 2000) == 1999) {
			CK(txn->commit(txn, 0));
			CK(dbenv->txn_begin(dbenv, NULL, &txn, 0));
		}
	}
	CK(txn->commit(txn, 0));

	CK(dbp->stat(dbp, NULL, &bs, 0));
	levels = (int)bs->bt_levels; free(bs);
	CK(dbmv->stat(dbmv, NULL, &bs, 0));
	mvlevels = (int)bs->bt_levels; free(bs);

	printf("RDL setup nkeys=%d pagesize=%u nreads=%d "
	    "plain_levels=%d mv_levels=%d\n",
	    nkeys, pgsz, nreads, levels, mvlevels);

	/* ---- the five isolation regimes ---- */
	pass(dbenv, dbp,  "no-txn",             1, 0, 0, levels);
	pass(dbenv, dbp,  "plain-txn",          0, 0, 0, levels);
	pass(dbenv, dbp,  "read-uncommitted",   0, DB_READ_UNCOMMITTED,
	    DB_READ_UNCOMMITTED, levels);
	pass(dbenv, dbp,  "read-committed",     0, DB_READ_COMMITTED, 0,
	    levels);
	pass(dbenv, dbmv, "MV-plain-txn",       0, 0, 0, mvlevels);
	pass(dbenv, dbmv, "MV-snapshot(SI)",    0, DB_TXN_SNAPSHOT, 0,
	    mvlevels);
	pass(dbenv, dbmv, "MV-serializable(SSI)", 0, DB_TXN_SERIALIZABLE, 0,
	    mvlevels);
	/* Non-multiversion DB but snapshot txn: MULTIVERSION(dbp) is false. */
	pass(dbenv, dbp,  "nonMV-snapshot",     0, DB_TXN_SNAPSHOT, 0, levels);
	pass(dbenv, dbp,  "nonMV-serializable", 0, DB_TXN_SERIALIZABLE, 0,
	    levels);

	CK(dbmv->close(dbmv, 0));
	CK(dbp->close(dbp, 0));
	CK(dbenv->close(dbenv, 0));
	printf("RDL done\n");
	return (0);
}

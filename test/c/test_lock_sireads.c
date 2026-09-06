/*-
 * See the file LICENSE for redistribution information.
 *
 * $Id$
 *
 * test_lock_sireads -- regression driver for GitHub issue #140.
 *
 * __lock_vec's DB_LOCK_PUT_READ handling builds the temporary DBT descriptor
 * array that becomes the replication commit lock list.  The array used to be
 * sized from sh_locker->nwrites, while the population loop skipped only
 * DB_LOCK_READ and DB_LOCK_READ_UNCOMMITTED.  DB_LOCK_SIREAD (the SSI
 * snapshot-read marker) matched neither of those tests nor IS_WRITELOCK, so a
 * retained SIREAD lock consumed a descriptor slot that was never allocated:
 *
 *   - a heap-buffer-overflow WRITE of sizeof(DBT) past the allocation, and
 *   - __lock_fix_list was then handed nwrites, truncating the list, so a
 *     SIREAD object (newly granted locks go to the HEAD of heldby, hence are
 *     visited first) could displace a modified page's write-lock object.  The
 *     replication apply path reacquires only the listed objects as write
 *     locks, so the omitted page could be changed under a client read lock.
 *
 * The trigger needs ONE transaction holding BOTH a write lock and a SIREAD
 * lock on distinct objects, reaching the objlist-building __lock_vec call:
 *
 *   DB_TXN_SNAPSHOT txn (=> SSI => SIREAD markers on reads) on a
 *   DB_MULTIVERSION btree, one put + one get on a DIFFERENT page, committed on
 *   a replication MASTER (__txn_commit passes an objlist to DB_LOCK_PUT_READ
 *   only for a logged top-level txn on a master).  A single-site master with a
 *   stub transport suffices -- no client is needed.
 *
 * Small page size + a fill phase put the two keys on different B-tree pages,
 * which is what makes the read take a distinct SIREAD lock.
 *
 * `--control` drops the read: one write lock, no SIREAD, no overflow.  It must
 * be clean both before and after the fix (it proves the trigger is the SIREAD).
 *
 * Build/run against an AddressSanitizer-instrumented libdb; the driver is
 * test/c/chk.locksireads.  Exit 0 = clean.
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <db.h>

#define	DATABASE	"sireads.db"
#define	PAGE_SIZE	512
#define	NRECORDS	2000
#define	MAX_RETRIES	20

static int
fail(const char *call, int ret)
{
	fprintf(stderr, "FAIL: %s: %s (%d)\n", call, db_strerror(ret), ret);
	return (ret == 0 ? EINVAL : ret);
}

static int
retryable(int ret)
{
	return (ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED ||
	    ret == DB_SNAPSHOT_CONFLICT || ret == DB_SNAPSHOT_UNSAFE);
}

/*
 * send_message --
 *	Stub replication transport.  Broadcasts are accepted and dropped; there
 *	is no client, so a targeted send would be a bug in this test.
 */
static int
send_message(DB_ENV *dbenv, const DBT *control, const DBT *rec,
    const DB_LSN *lsn, int eid, u_int32_t flags)
{
	(void)dbenv;
	(void)control;
	(void)rec;
	(void)lsn;
	(void)flags;

	return (eid == DB_EID_BROADCAST ? 0 : EIO);
}

static void
make_dbt(DBT *dbt, const char *s)
{
	memset(dbt, 0, sizeof(*dbt));
	dbt->data = (void *)s;
	dbt->size = (u_int32_t)strlen(s);
}

static int
put_value(DB *db, DB_TXN *txn, const char *k, const char *v)
{
	DBT key, val;

	make_dbt(&key, k);
	make_dbt(&val, v);
	return (db->put(db, txn, &key, &val, 0));
}

static int
get_value(DB *db, DB_TXN *txn, const char *k, DBT *val)
{
	DBT key;

	make_dbt(&key, k);
	memset(val, 0, sizeof(*val));
	return (db->get(db, txn, &key, val, 0));
}

static int
fill_database(DB *db)
{
	char key[32], val[32];
	int attempt, i, ret;

	for (i = 0; i < NRECORDS; i++) {
		(void)snprintf(key, sizeof(key), "account%06d", i);
		(void)snprintf(val, sizeof(val), "balance%06d", i);
		for (attempt = 0; attempt < MAX_RETRIES; attempt++) {
			if ((ret = put_value(db, NULL, key, val)) == 0)
				break;
			if (!retryable(ret))
				return (fail("DB->put (fill)", ret));
		}
		if (attempt == MAX_RETRIES)
			return (fail("DB->put (fill retries)", EBUSY));
	}
	return (0);
}

/*
 * run_transaction --
 *	One snapshot transaction: a write (WRITE lock) plus, unless this is the
 *	control run, a read of a key on another page (SIREAD marker).  The
 *	commit is where __lock_vec builds the commit lock list, with both locks
 *	still on the locker's heldby list.
 */
static int
run_transaction(DB_ENV *dbenv, DB *db, int with_read)
{
	DB_TXN *txn;
	DBT val;
	int attempt, ret, t_ret;

	for (attempt = 0; attempt < MAX_RETRIES; attempt++) {
		txn = NULL;
		if ((ret = dbenv->txn_begin(
		    dbenv, NULL, &txn, DB_TXN_SNAPSHOT)) != 0)
			return (fail("DB_ENV->txn_begin", ret));

		ret = put_value(db, txn, "journal000001", "transfer000001");
		if (ret == 0 && with_read)
			ret = get_value(db, txn, "account001554", &val);
		if (ret != 0) {
			if ((t_ret = txn->abort(txn)) != 0)
				return (fail("DB_TXN->abort", t_ret));
			if (retryable(ret))
				continue;
			return (fail("transaction operation", ret));
		}
		if ((ret = txn->commit(txn, 0)) == 0)
			return (0);
		if (!retryable(ret))
			return (fail("DB_TXN->commit", ret));
	}
	return (fail("transaction retries", EBUSY));
}

int
main(int argc, char *argv[])
{
	DB *db;
	DB_ENV *dbenv;
	int ret, t_ret, with_read;

	with_read = (argc > 1 && strcmp(argv[1], "--control") == 0) ? 0 : 1;
	printf("test_lock_sireads: %s (libdb %s)\n",
	    with_read ? "trigger (write + SIREAD)" : "control (write only)",
	    db_version(NULL, NULL, NULL));

	db = NULL;
	dbenv = NULL;
	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (fail("db_env_create", ret));
	if ((ret = dbenv->rep_set_transport(dbenv, 1, send_message)) != 0) {
		(void)fail("DB_ENV->rep_set_transport", ret);
		goto out;
	}
	if ((ret = dbenv->open(dbenv, ".", DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_REP | DB_INIT_TXN,
	    0600)) != 0) {
		(void)fail("DB_ENV->open", ret);
		goto out;
	}
	if ((ret = dbenv->rep_start(dbenv, NULL, DB_REP_MASTER)) != 0) {
		(void)fail("DB_ENV->rep_start", ret);
		goto out;
	}
	if ((ret = db_create(&db, dbenv, 0)) != 0) {
		(void)fail("db_create", ret);
		goto out;
	}
	if ((ret = db->set_pagesize(db, PAGE_SIZE)) != 0) {
		(void)fail("DB->set_pagesize", ret);
		goto out;
	}
	if ((ret = db->open(db, NULL, DATABASE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION, 0600)) != 0) {
		(void)fail("DB->open", ret);
		goto out;
	}
	if ((ret = fill_database(db)) != 0)
		goto out;
	if ((ret = run_transaction(dbenv, db, with_read)) != 0)
		goto out;
	printf("test_lock_sireads: PASS\n");

out:	if (db != NULL && (t_ret = db->close(db, 0)) != 0 && ret == 0)
		ret = fail("DB->close", t_ret);
	if (dbenv != NULL && (t_ret = dbenv->close(dbenv, 0)) != 0 && ret == 0)
		ret = fail("DB_ENV->close", t_ret);
	return (ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

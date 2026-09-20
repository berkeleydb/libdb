/*
 * See the file LICENSE for redistribution information.
 *
 * curadj_dup_partition.c --
 *	Regression test for the cursor-adjustment retry protocol on a
 *	DB_THREAD handle.
 *
 * WHAT BROKE
 *
 * __db_walk_cursors() runs a callback over every active cursor of every DB
 * handle sharing an adj_fileid.  Three callbacks cannot run to completion while
 * the queue mutex is held, because they call back into cursor alloc/free, which
 * takes that same mutex:
 *
 *	__bam_ca_dup_func	-> __bam_opd_cursor -> __db_cursor_int
 *	__bam_ca_undodup_func	-> __dbc_close
 *	__ham_chgpg_recover_func-> __dbc_close
 *
 * The protocol is that such a callback DROPS the mutex, does its work, and
 * returns DB_LOCK_NOTGRANTED to tell the walk "I released it, rescan".
 *
 * When the per-handle cursor queues were sharded into DB->cq_parts[] with a
 * mutex each, the walk changed to hold cq_parts[i].mutex -- but these three
 * callbacks were not updated and kept dropping dbp->mutex, the mutex the walk
 * had held BEFORE sharding and no longer holds at all.  So on a DB_THREAD
 * handle the callback unlocked a mutex this thread did not hold and entered
 * __db_cursor_int with the partition mutex still held, which then blocks
 * forever on that same non-recursive mutex.
 *
 * WHY IT WAS INVISIBLE
 *
 * Both dbp->mutex and cq_parts[].mutex are allocated only under DB_THREAD
 * (src/db/db.c).  Without DB_THREAD both are MUTEX_INVALID, so CQ_LOCK and
 * MUTEX_UNLOCK are no-ops and neither defect can manifest.  The TCL cursor
 * suites (recd013, test048, test053, test072/073, test086-088) open
 * non-threaded handles and all pass either way -- measured.  Hence this test:
 * DB_THREAD is the whole point of it.
 *
 * WHAT IT DOES
 *
 * Parks a second cursor on the exact record whose on-page duplicate set is
 * about to be moved into an off-page duplicate tree, then drives that
 * conversion.  __bam_dup_convert -> __bam_ca_dup -> __db_walk_cursors finds the
 * parked cursor, and __bam_ca_dup_func takes the drop-and-rescan path.
 *
 * Against the defect this HANGS (measured: deadlock in __db_cursor_int's
 * CQ_LOCK, reached from __bam_ca_dup_func via __bam_opd_cursor), so the test is
 * a hang test and its runner imposes a timeout.  It also checks the parked
 * cursor still addresses its original key afterwards and can still walk the
 * whole duplicate set, because the failure mode of a botched adjustment that
 * does NOT hang is a cursor silently pointing at the wrong row.
 *
 * NO LOCKING SUBSYSTEM ON PURPOSE.  Cursor adjustment is a property of
 * in-memory cursor positions and happens with or without the lock manager, but
 * with DB_INIT_LOCK the parked read cursor holds a read lock on the root page
 * while the inserts below want it for write -- a single-threaded self-block that
 * has nothing to do with what is under test.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	NDUPS	2000

static int
fail(const char *what, int ret)
{
	fprintf(stderr, "FAIL curadj_dup_partition: %s: %s\n",
	    what, db_strerror(ret));
	return (1);
}

int
main(int argc, char *argv[])
{
	DB *db;
	DBC *parked, *other;
	DBT key, data;
	DB_ENV *env;
	int i, ret;
	char vbuf[64];
	u_int32_t nreach;
	const char *dir;

	dir = argc > 1 ? argv[1] : "CURADJ_DUP_TESTDIR";

	if ((ret = db_env_create(&env, 0)) != 0)
		return (fail("db_env_create", ret));
	if ((ret = env->open(env, dir,
	    DB_CREATE | DB_INIT_MPOOL | DB_THREAD, 0644)) != 0)
		return (fail("env->open", ret));

	if ((ret = db_create(&db, env, 0)) != 0)
		return (fail("db_create", ret));
	if ((ret = db->set_flags(db, DB_DUP)) != 0)
		return (fail("set_flags(DB_DUP)", ret));
	/* Small pages so the dup set crosses the 25%-of-page conversion bar. */
	if ((ret = db->set_pagesize(db, 512)) != 0)
		return (fail("set_pagesize", ret));
	/*
	 * DB_THREAD is the point of this test: it is what allocates dbp->mutex
	 * and every cq_parts[].mutex.  Without it the mutexes are
	 * MUTEX_INVALID, CQ_LOCK is a no-op, and the bug cannot reproduce.
	 */
	if ((ret = db->open(db, NULL, "curadj_dup.db", NULL, DB_BTREE,
	    DB_CREATE | DB_THREAD, 0644)) != 0)
		return (fail("db->open", ret));

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)"K";
	key.size = 1;

	/* Seed one duplicate, so a cursor has something to park on. */
	memset(vbuf, 0, sizeof(vbuf));
	(void)snprintf(vbuf, sizeof(vbuf), "d%05d", 0);
	data.data = vbuf;
	data.size = 8;
	if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
		return (fail("put(seed)", ret));

	/*
	 * Park a cursor exactly on (K, first duplicate).  This is the cursor
	 * __bam_ca_dup_func must find, give an off-page-dup cursor to, and
	 * rescan for.
	 */
	if ((ret = db->cursor(db, NULL, &parked, 0)) != 0)
		return (fail("db->cursor(parked)", ret));
	if ((ret = parked->get(parked, &key, &data, DB_SET)) != 0)
		return (fail("parked DB_SET", ret));

	/* A second live cursor, so the active queue is not a single element. */
	if ((ret = db->cursor(db, NULL, &other, 0)) != 0)
		return (fail("db->cursor(other)", ret));
	if ((ret = other->get(other, &key, &data, DB_SET)) != 0)
		return (fail("other DB_SET", ret));

	/*
	 * Pile on duplicates until the on-page set is converted to an off-page
	 * duplicate tree.  That conversion is __bam_dup_convert, which calls
	 * __bam_ca_dup -> __db_walk_cursors -> __bam_ca_dup_func.  Against the
	 * defect this loop never returns.
	 */
	for (i = 1; i < NDUPS; i++) {
		(void)snprintf(vbuf, sizeof(vbuf), "d%05d", i);
		data.data = vbuf;
		data.size = 8;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			return (fail("put(dup)", ret));
	}

	/*
	 * The adjustment must have kept the parked cursor on the same logical
	 * record.  A silently mis-adjusted cursor does not crash; it reads the
	 * wrong row, so check the key explicitly.
	 */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	if ((ret = parked->get(parked, &key, &data, DB_CURRENT)) != 0 &&
	    ret != DB_KEYEMPTY)
		return (fail("parked DB_CURRENT after conversion", ret));
	if (ret == 0 && (key.size != 1 || memcmp(key.data, "K", 1) != 0)) {
		fprintf(stderr, "FAIL curadj_dup_partition: parked cursor left "
		    "its key after the off-page dup conversion\n");
		return (1);
	}

	/* It must also still be able to walk the whole duplicate set. */
	for (nreach = 1;
	    parked->get(parked, &key, &data, DB_NEXT_DUP) == 0; nreach++)
		;
	if (nreach != NDUPS) {
		fprintf(stderr, "FAIL curadj_dup_partition: parked cursor "
		    "reached %lu of %d duplicates\n",
		    (unsigned long)nreach, NDUPS);
		return (1);
	}

	if ((ret = other->close(other)) != 0)
		return (fail("other->close", ret));
	if ((ret = parked->close(parked)) != 0)
		return (fail("parked->close", ret));
	if ((ret = db->close(db, 0)) != 0)
		return (fail("db->close", ret));
	if ((ret = env->close(env, 0)) != 0)
		return (fail("env->close", ret));

	printf("PASS curadj_dup_partition: off-page dup conversion adjusted a "
	    "parked cursor on a DB_THREAD handle (%lu dups reachable)\n",
	    (unsigned long)nreach);
	return (0);
}

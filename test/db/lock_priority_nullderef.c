/*-
 * Regression test for the NULL-dereference in DB_ENV->set_lk_priority() and
 * DB_ENV->get_lk_priority() (issue #148).
 *
 * __lock_getlocker(create = 0) reports "no such locker" by returning 0 with a
 * NULL locker rather than by returning an error, because callers such as
 * __lock_vec_pp() legitimately expect a locker that holds no locks.  The two
 * priority accessors adopted that idiom without the NULL check and dereferenced
 * it, so a plain public-API call with an unused locker id crashed the library.
 *
 * Cases:
 *   1. set_lk_priority() on an id with no live locker -> EINVAL, no crash.
 *   2. get_lk_priority() on an id with no live locker -> EINVAL, no crash.
 *   3. set/get on a LIVE locker still round-trips, so the fix did not simply
 *      disable the feature.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <db.h>

static int failures = 0;
static int checks = 0;

#define CHECK(cond, ...) do {						\
	checks++;							\
	if (!(cond)) {							\
		failures++;						\
		printf("  FAIL: ");					\
		printf(__VA_ARGS__);					\
		printf("\n");						\
	}								\
} while (0)

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	DB_TXN *txn;
	u_int32_t id, prio;
	const char *home = "LOCK_PRIORITY_TESTDIR";
	int ret;

	(void)argc; (void)argv;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	dbenv->set_errpfx(dbenv, "lock_priority_nullderef");
	if ((ret = dbenv->open(dbenv, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_INIT_LOG, 0600)) != 0) {
		fprintf(stderr, "env open %s: %s\n", home, db_strerror(ret));
		return (EXIT_FAILURE);
	}

	/*
	 * 1 + 2: an id with no live locker.  Before the fix these dereferenced
	 * NULL and raised SIGSEGV; the process died here rather than failing a
	 * check, so simply reaching the end of this test is part of the result.
	 */
	ret = dbenv->set_lk_priority(dbenv, 0, 100);
	printf("  set_lk_priority(unused id): ret=%d (%s)\n",
	    ret, ret == 0 ? "success" : db_strerror(ret));
	CHECK(ret == EINVAL,
	    "set_lk_priority on an unused id returned %d, expected EINVAL (%d)",
	    ret, EINVAL);

	prio = 0;
	ret = dbenv->get_lk_priority(dbenv, 0, &prio);
	printf("  get_lk_priority(unused id): ret=%d (%s)\n",
	    ret, ret == 0 ? "success" : db_strerror(ret));
	CHECK(ret == EINVAL,
	    "get_lk_priority on an unused id returned %d, expected EINVAL (%d)",
	    ret, EINVAL);

	/* 3: a live locker must still work. */
	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
		fprintf(stderr, "txn_begin: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	id = txn->id(txn);

	ret = dbenv->set_lk_priority(dbenv, id, 77);
	CHECK(ret == 0, "set_lk_priority on live locker %u returned %d (%s)",
	    id, ret, db_strerror(ret));

	prio = 0;
	ret = dbenv->get_lk_priority(dbenv, id, &prio);
	CHECK(ret == 0, "get_lk_priority on live locker %u returned %d (%s)",
	    id, ret, db_strerror(ret));
	CHECK(prio == 77, "priority round trip on live locker %u: set 77, got %u",
	    id, prio);
	printf("  live locker %u: set/get round trip prio=%u\n", id, prio);

	(void)txn->abort(txn);
	(void)dbenv->close(dbenv, 0);

	printf("lock_priority_nullderef: %d checks, %d failures\n",
	    checks, failures);
	printf("lock_priority_nullderef: %s\n", failures == 0 ? "PASS" : "FAIL");
	return (failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

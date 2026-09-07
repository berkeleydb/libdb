/*-
 * Regression test for the NULL-dereference pair in issue #149.
 *
 * 1. DB_ENV->rep_get_nsites() on a repmgr-configured but UNOPENED environment.
 *    __repmgr_get_nsites() dereferenced db_rep->region, which is only attached
 *    at env open.  The caller's ENV_NOT_CONFIGURED() guard does nothing before
 *    ENV_OPEN_CALLED is set, so the call reached the dereference and crashed.
 *
 * 2. __cdsgroup_begin() installed only 8 of DB_TXN's 12 methods, leaving
 *    get_priority, set_priority, set_commit_token and set_txn_lsnp as NULL
 *    function pointers.  Calling one made an indirect call through NULL.
 *
 * Both are reachable from documented public APIs with valid arguments -- only
 * the call order, or the handle type, is unusual.
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

/*
 * rep_nsites_preopen --
 *	Case 1.  Configure repmgr but never open the environment.
 */
static int
rep_nsites_preopen(void)
{
	DB_ENV *dbenv;
	u_int32_t nsites;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (1);
	}
	/* Quiet the expected BDB3672 diagnostic. */
	dbenv->set_errfile(dbenv, NULL);

	/*
	 * Touching any repmgr setting allocates the DB_REP handle without
	 * attaching its shared region -- the pre-open state that crashed.
	 */
	if ((ret = dbenv->repmgr_set_ack_policy(dbenv,
	    DB_REPMGR_ACKS_ALL)) != 0) {
		printf("  (repmgr unavailable in this build: %s -- skipping "
		    "case 1)\n", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (0);
	}

	nsites = 12345;
	ret = dbenv->rep_get_nsites(dbenv, &nsites);
	printf("  rep_get_nsites(unopened env): ret=%d (%s)\n",
	    ret, ret == 0 ? "success" : db_strerror(ret));
	CHECK(ret != 0,
	    "rep_get_nsites on an unopened env returned success; it cannot "
	    "know nsites yet");

	(void)dbenv->close(dbenv, 0);
	return (0);
}

/*
 * cds_method_slots --
 *	Case 2.  Every DB_TXN slot on a CDS group handle must be callable.
 */
static int
cds_method_slots(const char *home)
{
	DB_ENV *dbenv;
	DB_TXN *txn;
	DB_TXN_TOKEN token;
	DB_LSN *rlsnp, *lsnp;
	u_int32_t priority;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (1);
	}
	dbenv->set_errfile(dbenv, NULL);
	if ((ret = dbenv->open(dbenv, home,
	    DB_CREATE | DB_INIT_CDB | DB_INIT_MPOOL, 0600)) != 0) {
		fprintf(stderr, "env open %s: %s\n", home, db_strerror(ret));
		return (1);
	}
	if ((ret = dbenv->cdsgroup_begin(dbenv, &txn)) != 0) {
		fprintf(stderr, "cdsgroup_begin: %s\n", db_strerror(ret));
		return (1);
	}

	/*
	 * A CDS group cannot honor any of these, so an error is the correct
	 * answer -- the point is that the call returns at all.
	 */
	CHECK(txn->get_priority != NULL, "get_priority slot is NULL");
	CHECK(txn->set_priority != NULL, "set_priority slot is NULL");
	CHECK(txn->set_commit_token != NULL, "set_commit_token slot is NULL");
	CHECK(txn->set_txn_lsnp != NULL, "set_txn_lsnp slot is NULL");

	priority = 0;
	ret = txn->get_priority(txn, &priority);
	printf("  cdsgroup get_priority:     ret=%d\n", ret);
	CHECK(ret != 0, "get_priority on a CDS group returned success");

	ret = txn->set_priority(txn, 5);
	printf("  cdsgroup set_priority:     ret=%d\n", ret);
	CHECK(ret != 0, "set_priority on a CDS group returned success");

	memset(&token, 0, sizeof(token));
	ret = txn->set_commit_token(txn, &token);
	printf("  cdsgroup set_commit_token: ret=%d\n", ret);
	CHECK(ret != 0, "set_commit_token on a CDS group returned success");

	/* void method: it must null the out-parameters, not crash. */
	rlsnp = (DB_LSN *)(uintptr_t)1;
	lsnp = (DB_LSN *)(uintptr_t)1;
	txn->set_txn_lsnp(txn, &rlsnp, &lsnp);
	printf("  cdsgroup set_txn_lsnp:     rlsnp=%p lsnp=%p\n",
	    (void *)rlsnp, (void *)lsnp);
	CHECK(rlsnp == NULL && lsnp == NULL,
	    "set_txn_lsnp left non-NULL LSN pointers (%p, %p) for a CDS group",
	    (void *)rlsnp, (void *)lsnp);

	(void)txn->commit(txn, 0);
	(void)dbenv->close(dbenv, 0);
	return (0);
}

int
main(int argc, char *argv[])
{
	const char *home = "NULL_METHOD_TESTDIR";

	(void)argc; (void)argv;

	if (rep_nsites_preopen() != 0)
		return (EXIT_FAILURE);
	if (cds_method_slots(home) != 0)
		return (EXIT_FAILURE);

	printf("null_method_slots: %d checks, %d failures\n", checks, failures);
	printf("null_method_slots: %s\n", failures == 0 ? "PASS" : "FAIL");
	return (failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

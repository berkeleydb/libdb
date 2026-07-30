/*-
 * SQLite-style malloc-failure injection sweep for libdb.
 *
 * fi_sweep.c --
 *	THE driver: run a representative libdb workload once with injection
 *	OFF to measure the baseline allocation count M, then for K = 1..M
 *	run the SAME workload with "fail the Kth allocation" and assert at
 *	every failure point that the library:
 *	  - returns a non-zero error (ENOMEM / a DB error) rather than
 *	    crashing on the OOM return path;
 *	  - lets the env be torn down cleanly afterwards;
 *	  - holds NO mutex on the OOM path -- a hang here (a lock leaked on
 *	    the error return, the #47 class of bug) is caught by a per-run
 *	    watchdog timeout.
 *
 *	Each K runs in a forked child so a crash/abort/hang in one failure
 *	point can't take down the sweep; the parent classifies the child's
 *	exit (clean / expected-error / CRASH / HANG) and keeps going.  This
 *	is the dynamic complement to the Coccinelle malloc-leak /
 *	mutex-unbalanced static rules.
 *
 *	Build/run (from build_unix, after configure --enable-faultinject):
 *	    make fi_tests && ./fi_sweep [maxK]
 *	Reproduce one failure point:
 *	    DB_FI_FAIL_AT=<K> DB_FI_VERBOSE=1 ./fi_sweep --one
 *
 *	Env knobs:
 *	    FI_TIMEOUT_SEC   per-run watchdog (default 20s; hang -> HANG).
 *	    FI_MAXK          cap the sweep (also positional argv[1]).
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"
#include "fi_alloc.h"

#define HOME     "TESTDIR_fi_sweep"
#define BTFILE   "fi_bt.db"
#define HFILE    "fi_h.db"
#define SECFILE  "fi_sec.db"      /* secondary index DB          */
#define MULTIFILE "fi_multi.db"    /* subdatabase container       */
#define NREC     40
#define NBULK    24               /* records for the bulk put    */
#define GID_A    0x51             /* first byte of the 2PC gid   */

/* Child exit codes the parent interprets. */
#define EX_OK        0   /* workload completed with no injected failure   */
#define EX_ERR       1   /* an operation returned an error, torn down OK  */
#define EX_DIRTY     2   /* an error path could not tear the env down     */
#define EX_BADSTATE  3   /* post-failure sanity/consistency check failed  */

static int workload __P((int *));
static int reopen_ok __P((void));
static int run_child __P((long));
static void cleanup __P((void));
static int seckey __P((DB *, const DBT *, const DBT *, DBT *));

/*
 * seckey --
 *	Secondary-index key extractor: the secondary key is the first 4
 *	bytes of the primary value (deterministic, so associate/pget/
 *	get-by-secondary hit the same alloc sites every run).
 */
static int
seckey(sdb, pkey, pdata, skey)
	DB *sdb;
	const DBT *pkey, *pdata;
	DBT *skey;
{
	(void)sdb;
	(void)pkey;
	memset(skey, 0, sizeof(*skey));
	if (pdata->size < 4)
		return (DB_DONOTINDEX);
	skey->data = pdata->data;
	skey->size = 4;
	return (0);
}

/*
 * cleanup --
 *	Remove the env home so each run starts clean.  Best-effort.
 */
static void
cleanup()
{
	(void)system("rm -rf " HOME " >/dev/null 2>&1");
	(void)mkdir(HOME, 0755);
}

/*
 * workload --
 *	A representative single-process DB_PRIVATE workload touching the
 *	paths a real app hits: open a transactional env, a btree AND a
 *	hash DB, put/get, a cursor walk, a committed txn and an aborted
 *	txn, and a checkpoint.  It then broadens into the warmer error
 *	paths functional tests miss: a secondary index (associate +
 *	get-by-secondary + pget), a 2PC transaction (prepare + resolve),
 *	bulk put/get (DB_MULTIPLE / DB_MULTIPLE_KEY), an in-memory DB, a
 *	subdatabase open, a join cursor, DB->compact, and DB->stat.
 *	Returns the first non-zero DB error it sees (so an injected OOM
 *	surfaces as that op's error), or 0.
 *
 *	The point is NOT that every op succeeds under injection -- it is
 *	that whichever op the injected OOM lands in returns cleanly and
 *	everything can still be closed.  So we STOP at the first error and
 *	tear down; the teardown itself is part of what we're testing.
 *
 *	*heldp is set non-zero while a lock/cursor/txn is outstanding, so
 *	the caller can tell whether teardown had resources to release.
 */
static int
workload(heldp)
	int *heldp;
{
	DB_ENV *env = NULL;
	DB *bt = NULL, *h = NULL, *sec = NULL, *mem = NULL, *sub = NULL;
	DB_TXN *txn = NULL;
	DBC *dbc = NULL, *jc[2] = { NULL, NULL }, *jcur = NULL;
	DB_COMPACT c_data;
	DBT key, data, pkey, skey;
	void *bulk = NULL;
	void *ptr;
	u_int8_t gid[DB_GID_SIZE];
	int i, ret, t_ret;
	char kbuf[32], vbuf[64];
	DB_BTREE_STAT *bstat = NULL;

	*heldp = 0;

	/* Env: private, transactional, in-memory-ish home on disk. */
	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	env->set_errpfx(env, "fi_sweep");
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_PRIVATE, 0)) != 0)
		goto done;

	/* --- Btree DB, committed transaction --- */
	if ((ret = db_create(&bt, env, 0)) != 0)
		goto done;
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		goto done;
	*heldp = 1;
	if ((ret = bt->open(bt, txn, BTFILE, NULL, DB_BTREE,
	    DB_CREATE, 0664)) != 0)
		goto done;

	for (i = 0; i < NREC; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "bkey-%06d", i);
		(void)snprintf(vbuf, sizeof(vbuf), "bval-%06d-payload", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = bt->put(bt, txn, &key, &data, 0)) != 0)
			goto done;
	}
	ret = txn->commit(txn, 0);
	txn = NULL;
	*heldp = 0;
	if (ret != 0)
		goto done;

	/* Get back a few of the records we committed. */
	for (i = 0; i < 5; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "bkey-%06d", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if ((ret = bt->get(bt, NULL, &key, &data, 0)) != 0)
			goto done;
	}

	/* Cursor walk of the btree. */
	if ((ret = bt->cursor(bt, NULL, &dbc, 0)) != 0)
		goto done;
	*heldp = 1;
	while ((ret = dbc->get(dbc, &key, &data, DB_NEXT)) == 0)
		;
	if (ret == DB_NOTFOUND)
		ret = 0;
	{
		int cret = dbc->close(dbc);
		dbc = NULL;
		*heldp = 0;
		if (ret == 0)
			ret = cret;
	}
	if (ret != 0)
		goto done;

	/* --- Hash DB, aborted transaction (exercise the abort path) --- */
	if ((ret = db_create(&h, env, 0)) != 0)
		goto done;
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		goto done;
	*heldp = 1;
	if ((ret = h->open(h, txn, HFILE, NULL, DB_HASH,
	    DB_CREATE, 0664)) != 0)
		goto done;
	for (i = 0; i < NREC; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "hkey-%06d", i);
		(void)snprintf(vbuf, sizeof(vbuf), "hval-%06d", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = h->put(h, txn, &key, &data, 0)) != 0)
			goto done;
	}
	/* Abort -- the hash inserts must roll back cleanly even mid-OOM. */
	ret = txn->abort(txn);
	txn = NULL;
	*heldp = 0;
	if (ret != 0)
		goto done;

	/* Checkpoint the environment. */
	if ((ret = env->txn_checkpoint(env, 0, 0, 0)) != 0)
		goto done;

	/*
	 * --- Secondary index: associate a secondary DB with the btree,
	 * then read back through it (get-by-secondary + pget).  This
	 * reaches the associate/callback/secondary-cursor alloc sites.
	 */
	if ((ret = db_create(&sec, env, 0)) != 0)
		goto done;
	/* Secondary keys are the 4-byte value prefix -> duplicates. */
	if ((ret = sec->set_flags(sec, DB_DUPSORT)) != 0)
		goto done;
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		goto done;
	*heldp = 1;
	if ((ret = sec->open(sec, txn, SECFILE, NULL, DB_BTREE,
	    DB_CREATE, 0664)) != 0)
		goto done;
	if ((ret = bt->associate(bt, txn, sec, seckey, DB_CREATE)) != 0)
		goto done;
	ret = txn->commit(txn, 0);
	txn = NULL;
	*heldp = 0;
	if (ret != 0)
		goto done;
	/* get-by-secondary: look up a primary row via its secondary key. */
	memset(&skey, 0, sizeof(skey));
	memset(&data, 0, sizeof(data));
	skey.data = "bval"; skey.size = 4;
	if ((ret = sec->get(sec, NULL, &skey, &data, 0)) != 0 &&
	    ret != DB_NOTFOUND)
		goto done;
	/* pget: fetch secondary key + primary key + primary data. */
	memset(&skey, 0, sizeof(skey));
	memset(&pkey, 0, sizeof(pkey));
	memset(&data, 0, sizeof(data));
	skey.data = "bval"; skey.size = 4;
	if ((ret = sec->pget(sec, NULL, &skey, &pkey, &data, 0)) != 0 &&
	    ret != DB_NOTFOUND)
		goto done;

	/*
	 * --- Join cursor: build two btree cursors positioned on the same
	 * secondary key and join them (a single-DB self-join is enough to
	 * reach the join-cursor alloc sites).
	 */
	if ((ret = sec->cursor(sec, NULL, &jc[0], 0)) != 0)
		goto done;
	*heldp = 1;
	memset(&skey, 0, sizeof(skey));
	memset(&data, 0, sizeof(data));
	skey.data = "bval"; skey.size = 4;
	if ((ret = jc[0]->get(jc[0], &skey, &data, DB_SET)) != 0 &&
	    ret != DB_NOTFOUND)
		goto done;
	if (ret == 0) {
		if ((ret = bt->join(bt, jc, &jcur, 0)) != 0)
			goto done;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		while ((ret = jcur->get(jcur, &key, &data, 0)) == 0)
			;
		if (ret == DB_NOTFOUND)
			ret = 0;
		(void)jcur->close(jcur);
		jcur = NULL;
	}
	(void)jc[0]->close(jc[0]);
	jc[0] = NULL;
	*heldp = 0;
	if (ret != 0)
		goto done;

	/*
	 * --- Bulk put + bulk get: DB_MULTIPLE_KEY put into the btree and
	 * a DB_MULTIPLE_KEY cursor read back.  Reaches the bulk-buffer /
	 * DB_MULTIPLE alloc sites.
	 */
	if ((bulk = malloc(64 * 1024)) == NULL) {
		ret = ENOMEM;
		goto done;
	}
	memset(&key, 0, sizeof(key));
	key.data = bulk; key.ulen = 64 * 1024;
	key.flags = DB_DBT_USERMEM | DB_DBT_BULK;
	DB_MULTIPLE_WRITE_INIT(ptr, &key);
	for (i = 0; i < NBULK; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "mkey-%06d", i);
		(void)snprintf(vbuf, sizeof(vbuf), "mval-%06d", i);
		DB_MULTIPLE_KEY_WRITE_NEXT(ptr, &key,
		    kbuf, strlen(kbuf) + 1, vbuf, strlen(vbuf) + 1);
		if (ptr == NULL)
			break;
	}
	if ((ret = bt->put(bt, NULL, &key, NULL, DB_MULTIPLE_KEY)) != 0)
		goto done;
	/* Bulk get: DB_MULTIPLE_KEY cursor scan of the btree. */
	if ((ret = bt->cursor(bt, NULL, &dbc, 0)) != 0)
		goto done;
	*heldp = 1;
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	data.data = bulk; data.ulen = 64 * 1024;
	data.flags = DB_DBT_USERMEM;
	while ((ret = dbc->get(dbc, &key, &data,
	    DB_NEXT | DB_MULTIPLE_KEY)) == 0) {
		void *bp, *kp, *dp;
		u_int32_t klen, dlen;
		DB_MULTIPLE_INIT(bp, &data);
		for (;;) {
			DB_MULTIPLE_KEY_NEXT(bp, &data, kp, klen, dp, dlen);
			if (kp == NULL)
				break;
		}
	}
	if (ret == DB_NOTFOUND)
		ret = 0;
	{
		int cret = dbc->close(dbc);
		dbc = NULL;
		*heldp = 0;
		if (ret == 0)
			ret = cret;
	}
	if (ret != 0)
		goto done;

	/*
	 * --- In-memory DB (NULL filename): reaches the in-memory-named-DB
	 * open + mpool-backing alloc sites distinct from on-disk opens.
	 */
	if ((ret = db_create(&mem, env, 0)) != 0)
		goto done;
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		goto done;
	*heldp = 1;
	if ((ret = mem->open(mem, txn, NULL, NULL, DB_BTREE,
	    DB_CREATE, 0664)) != 0)
		goto done;
	for (i = 0; i < 8; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "ikey-%06d", i);
		(void)snprintf(vbuf, sizeof(vbuf), "ival-%06d", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = mem->put(mem, txn, &key, &data, 0)) != 0)
			goto done;
	}
	ret = txn->commit(txn, 0);
	txn = NULL;
	*heldp = 0;
	if (ret != 0)
		goto done;

	/*
	 * --- Subdatabase open: a named DB inside a container file reaches
	 * the multi-DB / master-DB open alloc sites.
	 */
	if ((ret = db_create(&sub, env, 0)) != 0)
		goto done;
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		goto done;
	*heldp = 1;
	if ((ret = sub->open(sub, txn, MULTIFILE, "sub1", DB_BTREE,
	    DB_CREATE, 0664)) != 0)
		goto done;
	for (i = 0; i < 8; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "skey-%06d", i);
		(void)snprintf(vbuf, sizeof(vbuf), "sval-%06d", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = sub->put(sub, txn, &key, &data, 0)) != 0)
			goto done;
	}
	ret = txn->commit(txn, 0);
	txn = NULL;
	*heldp = 0;
	if (ret != 0)
		goto done;

	/*
	 * --- Compaction: DB->compact on the btree exercises the compact
	 * alloc sites (compaction working buffers, page fetches).
	 */
	memset(&c_data, 0, sizeof(c_data));
	if ((ret = bt->compact(bt, NULL, NULL, NULL, &c_data,
	    DB_FREE_SPACE, NULL)) != 0)
		goto done;

	/*
	 * --- DB->stat: gather btree statistics (allocates the stat
	 * struct + walks metadata).
	 */
	if ((ret = bt->stat(bt, NULL, &bstat, 0)) != 0)
		goto done;
	free(bstat);
	bstat = NULL;

	/*
	 * --- 2PC: prepare a transaction, then resolve it.  prepare()
	 * reaches the log-flush / gid-write / prepare alloc sites that the
	 * plain commit/abort paths do not.  (txn_recover is a
	 * separate-process recovery operation; calling it on a still-live
	 * prepared txn in the same process double-resolves it, so we just
	 * commit through our own handle here.)
	 */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		goto done;
	*heldp = 1;
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "pkey-2pc"; key.size = 9;
	data.data = "pval-2pc"; data.size = 9;
	if ((ret = bt->put(bt, txn, &key, &data, 0)) != 0)
		goto done;
	memset(gid, 0, sizeof(gid));
	gid[0] = GID_A;
	if ((ret = txn->prepare(txn, gid)) != 0)
		goto done;
	ret = txn->commit(txn, 0);
	txn = NULL;
	*heldp = 0;
	if (ret != 0)
		goto done;
done:
	/*
	 * Teardown IS part of the test.  Release anything still held, in
	 * the right order, and remember the FIRST real error -- but keep
	 * going so a leaked handle can't wedge the process.  If teardown
	 * itself fails (e.g. a close hits a still-held mutex), the caller
	 * treats that as a dirty error path.
	 */
	if (dbc != NULL)
		(void)dbc->close(dbc);
	if (jcur != NULL)
		(void)jcur->close(jcur);
	if (jc[0] != NULL)
		(void)jc[0]->close(jc[0]);
	if (txn != NULL)
		(void)txn->abort(txn);
	free(bulk);
	free(bstat);
	if (sub != NULL) {
		if ((t_ret = sub->close(sub, 0)) != 0 && ret == 0)
			ret = t_ret;
	}
	if (mem != NULL) {
		if ((t_ret = mem->close(mem, 0)) != 0 && ret == 0)
			ret = t_ret;
	}
	if (sec != NULL) {
		/* Secondary must close before its primary. */
		if ((t_ret = sec->close(sec, 0)) != 0 && ret == 0)
			ret = t_ret;
	}
	if (h != NULL) {
		if ((t_ret = h->close(h, 0)) != 0 && ret == 0)
			ret = t_ret;
	}
	if (bt != NULL) {
		if ((t_ret = bt->close(bt, 0)) != 0 && ret == 0)
			ret = t_ret;
	}
	if (env != NULL) {
		if ((t_ret = env->close(env, 0)) != 0 && ret == 0)
			ret = t_ret;
	}
	return (ret);
}

/*
 * reopen_ok --
 *	With injection DISARMED, open a fresh env on the same home and the
 *	btree DB we created, do one get, and close.  Returns 0 iff the
 *	environment left behind by a failed injection run is still usable.
 *	A non-zero return means the OOM error path left the env in a state
 *	it cannot recover from without external recovery -- a dirty path.
 */
static int
reopen_ok()
{
	DB_ENV *env = NULL;
	int ret, t_ret;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	/* DB_RECOVER: a crashed/half-written env is expected to need it;
	 * requiring recovery to succeed is the real "still consistent"
	 * bar, not "opens with no recovery". */
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER |
	    DB_PRIVATE, 0)) != 0)
		goto done;
done:
	if (env != NULL) {
		if ((t_ret = env->close(env, 0)) != 0 && ret == 0)
			ret = t_ret;
	}
	return (ret);
}

/*
 * run_child --
 *	Arm "fail the Kth allocation" (K==0 => baseline, no injection), run
 *	the workload, and exit with a code the parent classifies.  Runs in
 *	a forked child; a crash here becomes a signal the parent sees, and
 *	the child's own alarm() turns a hung lock into SIGALRM.
 *
 *	Classification of a non-crash, non-hang injection run:
 *	  EX_OK    the injected OOM did not change the outcome (K beyond
 *	           this run's alloc count, or an allocation the workload
 *	           tolerates): workload returned 0 and tore down clean.
 *	  EX_ERR   the injected OOM surfaced as a clean non-zero error AND
 *	           the env still tore down cleanly -- the good OOM case.
 *	  EX_DIRTY workload reported an error but a consistency re-check
 *	           after teardown failed, i.e. the failure left the env
 *	           un-reopenable / a resource stuck.  This is where a
 *	           lock leaked on the OOM return path (the #47 class)
 *	           shows up if it makes close fail rather than hang.
 */
static int
run_child(k)
	long k;
{
	int ret, held, recheck;

	cleanup();
	if (k > 0)
		__db_fi_arm(k);
	else
		__db_fi_reset();

	ret = workload(&held);

	if (k == 0)
		return (ret == 0 ? EX_OK : EX_BADSTATE);

	if (ret == 0)
		return (EX_OK);		/* OOM tolerated / not reached. */

	/*
	 * The workload reported an error (from the injected OOM or its
	 * teardown).  Confirm the failure left a re-openable environment:
	 * with injection now DISARMED, a fresh env open+close on the same
	 * home must succeed.  If it can't (a stuck region, a leaked lock
	 * that blocks reopen, on-disk corruption), that is a dirty error
	 * path -- exactly what this sweep exists to catch.
	 */
	__db_fi_disarm();
	recheck = reopen_ok();
	return (recheck == 0 ? EX_ERR : EX_DIRTY);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	long k, maxk, M;
	int held, ret, status, one_shot;
	const char *p;
	int n_fired, n_error, n_ok, n_crash, n_hang, n_dirty;
	int timeout_sec;
	pid_t pid;

	one_shot = 0;
	if (argc > 1 && strcmp(argv[1], "--one") == 0)
		one_shot = 1;

	timeout_sec = (p = getenv("FI_TIMEOUT_SEC")) != NULL ?
	    atoi(p) : 20;
	if (timeout_sec <= 0)
		timeout_sec = 20;

	/*
	 * One-shot mode: honor DB_FI_FAIL_AT from the environment (armed
	 * lazily inside the library) and run a single workload in-process
	 * so it's easy to run under a debugger / ASan for a specific K.
	 */
	if (one_shot) {
		cleanup();
		ret = workload(&held);
		printf("fi: one-shot: DB_FI_FAIL_AT=%s -> ret=%d (%s), "
		    "allocs=%ld, fired=%d\n",
		    getenv("DB_FI_FAIL_AT") ? getenv("DB_FI_FAIL_AT") : "(unset)",
		    ret, ret == 0 ? "clean" : db_strerror(ret),
		    __db_fi_count(), __db_fi_fired());
		return (0);
	}

	/* --- Phase 1: baseline, injection OFF, measure M in-process. --- */
	__db_fi_disarm();
	__db_fi_reset();
	cleanup();
	ret = workload(&held);
	M = __db_fi_count();
	if (ret != 0) {
		fprintf(stderr,
		    "fi: FATAL: baseline workload failed with no injection: "
		    "ret=%d (%s)\n", ret, db_strerror(ret));
		return (2);
	}
	printf("fi: baseline allocation count M = %ld "
	    "(clean run, no injection)\n", M);

	maxk = M;
	if ((p = getenv("FI_MAXK")) != NULL && atol(p) > 0 && atol(p) < maxk)
		maxk = atol(p);
	if (argc > 1 && !one_shot && atol(argv[1]) > 0 && atol(argv[1]) < maxk)
		maxk = atol(argv[1]);

	printf("fi: sweeping K = 1..%ld (per-run watchdog %ds)\n",
	    maxk, timeout_sec);

	/* --- Phase 2: sweep K, one forked+watchdogged child per K. --- */
	n_fired = n_error = n_ok = n_crash = n_hang = n_dirty = 0;
	for (k = 1; k <= maxk; k++) {
		fflush(stdout);
		fflush(stderr);
		if ((pid = fork()) < 0) {
			perror("fork");
			return (2);
		}
		if (pid == 0) {
			/* Child: watchdog itself so a hung mutex -> SIGALRM. */
			alarm((unsigned)timeout_sec);
			_exit(run_child(k));
		}

		/* Parent: wait for the child; the child's own alarm() turns
		 * a hang into SIGALRM, which we see as a signal exit. */
		if (waitpid(pid, &status, 0) < 0) {
			perror("waitpid");
			return (2);
		}

		if (WIFSIGNALED(status)) {
			int sig = WTERMSIG(status);
			if (sig == SIGALRM) {
				n_hang++;
				printf("fi: K=%ld HANG (watchdog; likely a "
				    "lock held on the OOM return path)\n", k);
			} else {
				n_crash++;
				printf("fi: K=%ld CRASH (signal %d: %s)\n",
				    k, sig, strsignal(sig));
			}
			continue;
		}
		if (!WIFEXITED(status)) {
			n_crash++;
			printf("fi: K=%ld CRASH (abnormal exit)\n", k);
			continue;
		}
		switch (WEXITSTATUS(status)) {
		case EX_OK:      n_ok++;    break;
		case EX_ERR:     n_error++; break;
		case EX_DIRTY:
			n_dirty++;
			printf("fi: K=%ld DIRTY (error path could not tear "
			    "down the env cleanly)\n", k);
			break;
		case EX_BADSTATE:
			n_dirty++;
			printf("fi: K=%ld BADSTATE (post-failure consistency "
			    "check failed)\n", k);
			break;
		default:
			n_crash++;
			printf("fi: K=%ld CRASH (unexpected exit %d)\n",
			    k, WEXITSTATUS(status));
			break;
		}
	}

	printf("\nfi: sweep complete over K=1..%ld\n", maxk);
	printf("fi:   baseline allocations M   = %ld\n", M);
	printf("fi:   failure points exercised = %ld\n", maxk);
	printf("fi:   clean/tolerated (OK)     = %d\n", n_ok);
	printf("fi:   clean error return       = %d\n", n_error);
	printf("fi:   CRASH                    = %d\n", n_crash);
	printf("fi:   HANG (lock leak?)        = %d\n", n_hang);
	printf("fi:   DIRTY teardown/state     = %d\n", n_dirty);

	/* Fail the process iff any run crashed, hung, or left dirty. */
	if (n_crash != 0 || n_hang != 0 || n_dirty != 0) {
		printf("\nfi: RESULT: FAIL -- %d crash, %d hang, %d dirty. "
		    "Reproduce a point with: "
		    "DB_FI_FAIL_AT=<K> DB_FI_VERBOSE=1 ./fi_sweep --one\n",
		    n_crash, n_hang, n_dirty);
		return (1);
	}
	printf("\nfi: RESULT: PASS -- every failure point returned cleanly, "
	    "held no lock, tore down cleanly.\n");
	return (0);
}

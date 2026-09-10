/*-
 * test/repiso/test_rep_iso.c --
 *	Tier B4: two-process master+client replication isolation harness.
 *
 * WHAT THIS OBSERVES
 *
 * Issue #140 was a heap out-of-bounds write in __lock_vec (fixed in #145 and
 * gated by test/lockmatrix/).  It had a SECOND consequence that the reporter
 * derived from source and explicitly could not observe: a client-side
 * transaction-isolation violation.  This harness is that observation.
 *
 * The mechanism, end to end:
 *
 *  1. On the master, __txn_commit asks __lock_vec for the transaction's
 *     retained WRITE locks (DB_LOCK_PUT_READ with an objlist) and logs them in
 *     the __txn_regop record.  Before #145 the objlist was SIZED from
 *     sh_locker->nwrites -- which does not count DB_LOCK_SIREAD, the SSI read
 *     marker -- while the POPULATE loop wrote a descriptor for every retained
 *     lock of either kind.  __lock_fix_list then serialized only the first
 *     `nwrites' descriptors.
 *
 *  2. Newly granted locks go to the HEAD of the locker's heldby list.  So a
 *     transaction that WRITES page A and then READS page B holds
 *     [SIREAD(B), WRITE(A)] in that order, and a 1-entry truncation keeps
 *     SIREAD(B) and DROPS WRITE(A).  The commit record therefore names the
 *     page the transaction did NOT modify and omits the one it DID.
 *
 *  3. On the client, __rep_process_txn (src/rep/rep_record.c) calls
 *     __lock_get_list(..., DB_LOCK_WRITE, lock_dbt): apply reacquires every
 *     LISTED object as a write lock, and takes no other page locks (the apply
 *     cursor is DBC_RECOVER, which __db_lget short-circuits on a client).
 *     An omitted page is therefore NOT locked, so apply is free to modify it
 *     while an ordinary client transaction holds a read lock on it.
 *
 *  4. That client transaction can then re-read the same key and see a
 *     different value -- a non-repeatable read, which its isolation level
 *     forbids.
 *
 * WHY TWO REAL PROCESSES
 *
 * The rep0NN Tcl tests hand-carry replication messages between envs inside one
 * tclsh (test/tcl/reputils.tcl's replsend/process_msgs).  In that harness the
 * "client" apply and the "client" reader are the same thread, so the reader can
 * never be holding a lock while apply runs -- the anomaly is structurally
 * unobservable.  This harness runs a real master process and a real client
 * process over a real TCP socket using the Base Replication API
 * (rep_set_transport / rep_start / rep_process_message), modelled on
 * examples/c/ex_rep/base.
 *
 * WHY THE OBSERVATION IS DETERMINISTIC, NOT A RACE
 *
 * The client reader holds its read lock across the ENTIRE window: it reads,
 * signals the master, waits, re-reads, and only then commits.  So there is no
 * interleaving to win.  Either apply respects the lock (it must block, and the
 * second read agrees) or it does not (the second read changes).  Both outcomes
 * are decided by whether the modified page reached the commit lock list.
 *
 * THE ANTI-VACUOUS CHECK
 *
 * A harness that never delivered the update would report "both reads agree"
 * and look exactly like a pass.  So after committing, the reader polls until
 * the new value IS visible.  If it never becomes visible the verdict is
 * INCONCLUSIVE, not PASS.  A corroborating signal comes from a SENTINEL record
 * the master writes to a SEPARATE database immediately after the trigger
 * commit: apply is sequential in the client's single message thread, so the
 * sentinel cannot become visible until the trigger transaction has been
 * applied.  Seeing the sentinel while still holding the read lock is therefore
 * independent evidence that apply was NOT blocked.
 *
 * Usage:
 *	test_rep_iso --role=master --port=N --home=DIR --rendezvous=DIR
 *	test_rep_iso --role=client --port=N --home=DIR --rendezvous=DIR
 *
 * Exit status: 0 = the outcome matched expectation, 1 = it did not (an
 * isolation violation, or an inconclusive run), 2 = harness error.
 */
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "rep_iso.h"

#define	RISO_SENTINEL_DB	"sentinel.db"
#define	RISO_SENTINEL_KEY	"go"
#define	RISO_INITVAL		1
#define	RISO_NEWVAL		99

/*
 * How long the client keeps its read lock while polling for the sentinel.
 *
 * This is the ONE window in the harness, and with the fix present it is
 * EXPECTED to expire: apply is correctly blocked, so the sentinel cannot
 * arrive, and the whole budget is spent every run.  It therefore has to be
 * large enough to cover the real apply latency and small enough not to
 * dominate the tier.
 *
 * MEASURED (see test/repiso/README.md for the raw runs): on an idle 8-core
 * box the interval from the master committing the trigger to the client having
 * applied it and made the sentinel visible was 30-70ms across 10 runs
 * (min 31, median 44, max 68).  Under `make -j8` on the same box the same
 * interval spread to 90-320ms.  10s is >30x the worst observed value.  Do NOT
 * tighten this from a single measurement -- a too-tight budget here does not
 * fail the tier, it silently converts "apply was blocked" (correct) into
 * "apply had not got there yet" (proves nothing), which is worse.
 */
#define	RISO_SENTINEL_POLL_SECS	10

/*
 * Rendezvous flag files.  Two processes with no shared memory need SOME
 * channel; a file appearing in a shared directory is the smallest one that
 * works identically on every platform the tree builds on, and unlike a second
 * socket it cannot itself perturb the replication transport under test.
 */
#define	F_CLIENT_UP	"client_up"	/* client synced with master */
#define	F_READER_HOLDS	"reader_holds"	/* client holds its read lock */
#define	F_TRIGGER_DONE	"trigger_done"	/* master's trigger txn committed */
#define	F_CLIENT_DONE	"client_done"	/* client published its verdict */

static char	 rendezvous[512];
static int	 verbose;

/* Timeouts, in seconds.  Justified where they are used. */
static int	 wait_secs = 60;

static void
vlog(const char *fmt, ...)
{
	struct timeval tv;
	va_list ap;

	(void)gettimeofday(&tv, NULL);
	printf("[%ld.%03ld] ", (long)tv.tv_sec % 1000,
	    (long)tv.tv_usec / 1000);
	va_start(ap, fmt);
	(void)vprintf(fmt, ap);
	va_end(ap);
	printf("\n");
	(void)fflush(stdout);
}

static void
die(const char *what, int rc)
{
	fprintf(stderr, "harness error: %s: %s (%d)\n", what,
	    rc > 0 ? db_strerror(rc) : strerror(-rc), rc);
	exit(2);
}

/* Monotonic-enough elapsed milliseconds. */
static long
now_ms(void)
{
	struct timeval tv;

	(void)gettimeofday(&tv, NULL);
	return ((long)tv.tv_sec * 1000 + tv.tv_usec / 1000);
}

static void
flag_path(char *buf, size_t len, const char *name)
{
	(void)snprintf(buf, len, "%s/%s", rendezvous, name);
}

static void
flag_set(const char *name)
{
	char path[600];
	FILE *fp;

	flag_path(path, sizeof(path), name);
	if ((fp = fopen(path, "w")) == NULL)
		die(name, -errno);
	(void)fclose(fp);
	if (verbose)
		vlog("flag set: %s", name);
}

static int
flag_isset(const char *name)
{
	char path[600];
	struct stat sb;

	flag_path(path, sizeof(path), name);
	return (stat(path, &sb) == 0);
}

/*
 * flag_clear_all --
 *	Remove every rendezvous flag.  Called by the MASTER at startup, before
 *	it accepts the connection, so a rerun in a reused directory cannot see
 *	a previous run's flags.  A stale reader_holds would let the master
 *	commit the trigger before the client had taken its read lock, which
 *	produces a PASS that observed nothing -- the exact failure mode this
 *	tier is built to avoid.  One owner of the reset (the master) avoids a
 *	clear/set race between the two processes.
 */
static void
flag_clear_all(void)
{
	static const char *const all[] = {
		F_CLIENT_UP, F_READER_HOLDS, F_TRIGGER_DONE, F_CLIENT_DONE, NULL
	};
	char path[600];
	int i;

	for (i = 0; all[i] != NULL; i++) {
		flag_path(path, sizeof(path), all[i]);
		(void)unlink(path);
	}
}

/*
 * flag_wait --
 *	Poll for a flag.  Returns 1 if it appeared, 0 on timeout.  10ms polls:
 *	fine-grained enough that the poll interval is not what the harness
 *	measures, coarse enough not to burn a core.
 */
static int
flag_wait(const char *name, int secs)
{
	int i;

	for (i = 0; i < secs * 100; i++) {
		if (flag_isset(name))
			return (1);
		usleep(10000);
	}
	return (0);
}

/*
 * ---------------------------------------------------------------------------
 * DB helpers.  Records carry an int in the first bytes plus padding, so a
 * scenario can make records big enough to force the leaf split that puts the
 * write key and the read key on different pages.
 * ---------------------------------------------------------------------------
 */
static int
riso_put(DB *db, DB_TXN *txn, const char *key, int val)
{
	DBT k, d;
	u_int8_t buf[sizeof(int) + RISO_PAD];

	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	memset(buf, 0x5a, sizeof(buf));
	memcpy(buf, &val, sizeof(val));
	k.data = (void *)key;
	k.size = (u_int32_t)strlen(key);
	d.data = buf;
	d.size = sizeof(buf);
	return (db->put(db, txn, &k, &d, 0));
}

static int
riso_get(DB *db, DB_TXN *txn, const char *key, int *out)
{
	DBT k, d;
	u_int8_t buf[sizeof(int) + RISO_PAD + 64];
	int rc;

	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = (void *)key;
	k.size = (u_int32_t)strlen(key);
	d.data = buf;
	d.ulen = sizeof(buf);
	d.flags = DB_DBT_USERMEM;
	if ((rc = db->get(db, txn, &k, &d, 0)) != 0)
		return (rc);
	if (d.size < sizeof(int))
		die("short record", EINVAL);
	memcpy(out, buf, sizeof(int));
	return (0);
}

/*
 * env_open_common --
 *	Both sites want the same environment except for the replication role.
 */
static DB_ENV *
env_open_common(const char *home, int is_master)
{
	DB_ENV *env;
	int rc;

	if ((rc = db_env_create(&env, 0)) != 0)
		die("db_env_create", rc);
	(void)env->set_errfile(env, stderr);
	(void)env->set_errpfx(env, is_master ? "MASTER" : "CLIENT");
	(void)env->set_event_notify(env, riso_event);

	/*
	 * Plenty of MVCC room: too small a cache makes a snapshot transaction
	 * fail for cache reasons, which would mask the property under test.
	 */
	if ((rc = env->set_cachesize(env, 0, 16 * 1024 * 1024, 1)) != 0)
		die("set_cachesize", rc);

	/*
	 * A lock timeout, and deliberately NO deadlock detector.
	 *
	 * Apply on the client reacquires the committed transaction's write
	 * locks and WAITS.  If the modified page is in that list, apply
	 * correctly blocks on the reader's read lock -- and with no timeout it
	 * would block forever, turning "the engine held the line" into a hang.
	 * With a timeout, apply gets DB_LOCK_NOTGRANTED and __rep_process_rec
	 * retries it (its do/while loop retries exactly NOTGRANTED and
	 * DEADLOCK), so the correct outcome is self-bounding.
	 *
	 * The detector is left OFF on purpose: apply runs at
	 * DB_LOCK_MAXPRIORITY, so a detector would resolve the conflict by
	 * killing the READER -- a legitimate engine behaviour, but one that
	 * destroys the observation instead of making it.
	 *
	 * 500ms: long enough that apply is not spinning, short enough that a
	 * blocked apply resumes promptly once the reader commits.
	 */
	if ((rc = env->set_timeout(env, 500000, DB_SET_LOCK_TIMEOUT)) != 0)
		die("set_timeout", rc);

	/*
	 * DB_TXN_NOSYNC, as examples/c/ex_rep does (rep_common.c:512) and for
	 * the same reason the Reference Guide gives: a replication group's
	 * durability comes from the data existing at another site, so fsync per
	 * commit buys nothing here.  It matters a lot to this harness: the
	 * master writes 66 setup records, and with per-commit fsync that setup
	 * took ~30s of the run against ~1s without.  Both sites flush the log
	 * explicitly before shutdown so nothing is actually lost.
	 */
	if ((rc = env->set_flags(env, DB_TXN_NOSYNC, 1)) != 0)
		die("set_flags(DB_TXN_NOSYNC)", rc);

	if ((rc = env->rep_set_transport(env, RISO_SELF_EID, riso_send)) != 0)
		die("rep_set_transport", rc);

	if ((rc = env->open(env, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_INIT_REP |
	    DB_RECOVER | DB_THREAD, 0600)) != 0)
		die("DB_ENV->open", rc);
	return (env);
}

/*
 * ---------------------------------------------------------------------------
 * MASTER
 * ---------------------------------------------------------------------------
 */
static int
run_master(const char *home, int port)
{
	DB_ENV *env;
	DB *db, *sdb;
	DB_TXN *txn;
	DB_BTREE_STAT *bst;
	char fill[16];
	int f, leafpg, rc, v;

	env = env_open_common(home, 1);
	/*
	 * Clear stale rendezvous flags BEFORE accepting, so the client cannot
	 * observe a flag from a previous run in a reused directory.
	 */
	flag_clear_all();
	/*
	 * Accept the connection BEFORE rep_start: rep_start(MASTER) broadcasts,
	 * and with no socket yet riso_send can only answer DB_REP_UNAVAIL.
	 * Nothing is lost -- the client's sync-up drives the transfer anyway --
	 * but a harness whose first replication call fails is a harness whose
	 * later failures are hard to attribute.
	 */
	vlog("master: listening on %d", port);
	if (riso_listen(port) != 0)
		die("riso_listen", -EIO);
	vlog("master: client connected");
	if ((rc = env->rep_start(env, NULL, DB_REP_MASTER)) != 0)
		die("rep_start(MASTER)", rc);
	if ((rc = riso_start_reader(env)) != 0)
		die("riso_start_reader", rc);

	/*
	 * The replicated database.  DB_MULTIVERSION is required for the
	 * trigger: it is what makes a DB_TXN_SNAPSHOT read take a
	 * DB_LOCK_SIREAD marker (src/db/db_meta.c:__db_lget) instead of no
	 * lock at all, and the SIREAD marker is what truncated the list.
	 */
	if ((rc = db_create(&db, env, 0)) != 0)
		die("db_create", rc);
	if ((rc = db->set_pagesize(db, RISO_PAGESIZE)) != 0)
		die("set_pagesize", rc);
	if ((rc = db->open(db, NULL, RISO_DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_MULTIVERSION | DB_AUTO_COMMIT | DB_THREAD,
	    0600)) != 0)
		die("DB->open", rc);

	if ((rc = db_create(&sdb, env, 0)) != 0)
		die("db_create(sentinel)", rc);
	if ((rc = sdb->open(sdb, NULL, RISO_SENTINEL_DB, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0600)) != 0)
		die("DB->open(sentinel)", rc);

	/*
	 * The shape: RISO_WKEY and RISO_RKEY must land on DIFFERENT leaf
	 * pages, so filler keys sorting strictly between them split the leaf.
	 * The payload stays under the B-tree overflow threshold (pagesize/4)
	 * or records move off-page and the leaf never splits.
	 */
	if ((rc = riso_put(db, NULL, RISO_WKEY, RISO_INITVAL)) != 0)
		die("put wkey", rc);
	if ((rc = riso_put(db, NULL, RISO_RKEY, RISO_INITVAL)) != 0)
		die("put rkey", rc);
	for (f = 0; f < RISO_FILL; f++) {
		(void)snprintf(fill, sizeof(fill), "f%04d", f);
		if ((rc = riso_put(db, NULL, fill, f)) != 0)
			die("put filler", rc);
	}
	if ((rc = riso_put(sdb, NULL, RISO_SENTINEL_KEY, 0)) != 0)
		die("put sentinel", rc);

	if ((rc = db->stat(db, NULL, &bst, 0)) != 0)
		die("DB->stat", rc);
	leafpg = (int)bst->bt_leaf_pg;
	free(bst);
	printf("MASTER: btree leaf pages = %d\n", leafpg);
	vlog("master: setup writes done");
	if (leafpg < 2) {
		/*
		 * Not a pass and not a failure of the engine: the harness did
		 * not build the shape it claims to test.  Say so loudly.
		 */
		printf("MASTER: RESULT verdict=SHAPE_NOT_ESTABLISHED "
		    "leafpg=%d\n", leafpg);
		flag_set(F_TRIGGER_DONE);
		riso_stop_reader();
		(void)db->close(db, 0);
		(void)sdb->close(sdb, 0);
		(void)env->close(env, 0);
		return (2);
	}

	/*
	 * Push the setup records out so the trigger transaction's records are
	 * the last thing in the log.
	 *
	 * NO CHECKPOINT.  A checkpoint on a replication master sleeps for
	 * DB_REP_CHECKPOINT_DELAY (default 30s, src/rep/rep_method.c:54) after
	 * flushing the cache, deliberately, to let clients catch up -- so a
	 * checkpoint here cost 30.0s of every run (measured: 30.046s, and 0.024s
	 * once removed) and bought nothing the log_flush does not.  Setting the
	 * delay to 0 would work too, but not needing a checkpoint at all is
	 * simpler than configuring one away.
	 */
	if ((rc = env->log_flush(env, NULL)) != 0)
		die("log_flush(setup)", rc);
	vlog("master: setup log flushed");

	vlog("master: setup replicated; waiting for client to hold its lock");
	if (!flag_wait(F_READER_HOLDS, wait_secs)) {
		printf("MASTER: RESULT verdict=CLIENT_NEVER_READY\n");
		riso_stop_reader();
		(void)db->close(db, 0);
		(void)sdb->close(sdb, 0);
		(void)env->close(env, 0);
		return (1);
	}

	/*
	 * THE TRIGGER TRANSACTION.
	 *
	 * A logged, top-level DB_TXN_SNAPSHOT transaction on a
	 * DB_MULTIVERSION database that WRITES one key and then READS another
	 * on a different page.  At commit it retains WRITE(wkey page) and
	 * SIREAD(rkey page); the SIREAD, granted last, sits at the head of
	 * heldby.
	 *
	 * Write-then-read, not read-then-write: the truncation keeps the HEAD
	 * of heldby, so the SIREAD must be the newer lock for it to displace
	 * the write lock.  Reading first would put the write lock at the head
	 * and the list would (accidentally) still be correct.
	 */
	vlog("master: running trigger txn (put %s, get %s)",
	    RISO_WKEY, RISO_RKEY);
	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT)) != 0)
		die("txn_begin(SNAPSHOT)", rc);
	if ((rc = riso_put(db, txn, RISO_WKEY, RISO_NEWVAL)) != 0)
		die("trigger put", rc);
	if ((rc = riso_get(db, txn, RISO_RKEY, &v)) != 0)
		die("trigger get", rc);
	if ((rc = txn->commit(txn, 0)) != 0) {
		printf("MASTER: RESULT verdict=TRIGGER_COMMIT_FAILED rc=%d "
		    "(%s)\n", rc, db_strerror(rc));
		flag_set(F_TRIGGER_DONE);
		riso_stop_reader();
		(void)db->close(db, 0);
		(void)sdb->close(sdb, 0);
		(void)env->close(env, 0);
		return (1);
	}
	vlog("master: trigger committed (%s: %d -> %d, read %s = %d)",
	    RISO_WKEY, RISO_INITVAL, RISO_NEWVAL, RISO_RKEY, v);

	/*
	 * The sentinel, in a SEPARATE database so its own apply cannot
	 * conflict with the reader's lock (different fileid => different lock
	 * objects).  Apply is sequential in the client's single message
	 * thread, so the client seeing this sentinel proves the trigger
	 * transaction was already applied.
	 */
	if ((rc = riso_put(sdb, NULL, RISO_SENTINEL_KEY, 1)) != 0)
		die("put sentinel go", rc);
	if ((rc = env->log_flush(env, NULL)) != 0)
		die("log_flush", rc);
	flag_set(F_TRIGGER_DONE);
	vlog("master: sentinel written");

	/* Stay alive until the client has published its verdict. */
	if (!flag_wait(F_CLIENT_DONE, wait_secs))
		vlog("master: client verdict never arrived (continuing)");

	/*
	 * Push the log out before shutting down, so the client's copy is
	 * complete for the post-hoc log check in run.sh.
	 */
	if ((rc = env->log_flush(env, NULL)) != 0)
		die("log_flush", rc);
	sleep(1);			/* let the last messages drain */
	riso_stop_reader();
	if ((rc = db->close(db, 0)) != 0)
		die("DB->close", rc);
	if ((rc = sdb->close(sdb, 0)) != 0)
		die("DB->close(sentinel)", rc);
	if ((rc = env->close(env, 0)) != 0)
		die("DB_ENV->close", rc);
	printf("MASTER: RESULT verdict=OK msgs_in=%lu\n", riso_msgs_in);
	return (0);
}

/*
 * ---------------------------------------------------------------------------
 * CLIENT
 * ---------------------------------------------------------------------------
 */

/*
 * open_when_replicated --
 *	Retry DB->open until apply has created the file.
 *
 *	The open happens INSIDE a transaction on purpose.  __db_open sets
 *	DB_AM_TXN only when IS_REAL_TXN(txn) (src/db/db_open.c), and without
 *	DB_AM_TXN every later txn-scoped get is rejected with "Transaction
 *	specified for a non-transactional database" -- which would silently
 *	turn the reader into a non-transactional one holding no lock at all,
 *	i.e. exactly the vacuous harness this tier exists to avoid.
 *
 *	DB_AUTO_COMMIT would not do: it is a write and a replication client
 *	cannot write its own log.  A read-only txn opened with DB_TXN_SNAPSHOT
 *	is rejected on a client too, so this is an ordinary txn that we commit
 *	immediately after the open.
 */
static DB *
open_when_replicated(DB_ENV *env, const char *file, int secs)
{
	DB *db;
	DB_TXN *otxn;
	int i, rc;

	for (i = 0; i < secs * 10; i++) {
		if ((rc = db_create(&db, env, 0)) != 0)
			die("db_create", rc);
		if ((rc = env->txn_begin(env, NULL, &otxn, 0)) != 0)
			die("txn_begin(open)", rc);
		if ((rc = db->open(db, otxn, file, NULL, DB_BTREE,
		    DB_RDONLY | DB_THREAD, 0600)) == 0) {
			if ((rc = otxn->commit(otxn, 0)) != 0)
				die("commit(open)", rc);
			return (db);
		}
		(void)otxn->abort(otxn);
		(void)db->close(db, 0);
		usleep(100000);
	}
	return (NULL);
}

static int
run_client(const char *home, const char *host, int port)
{
	DB_ENV *env;
	DB *db, *sdb;
	DB_TXN *txn;
	long t_apply_ms, t_hold_ms, t_read1_ms;
	int rc, sentinel_before, sentinel_while_holding, v1, v2, v3, sv;
	int i, verdict_fail;

	env = env_open_common(home, 0);
	if (riso_connect(host, port, 30) != 0)
		die("riso_connect", -EIO);
	/*
	 * rep_start BEFORE the reader thread: rep_process_message returns
	 * EINVAL ("not configured as replication master or client") until the
	 * role is set, and the messages it would have rejected are the sync-up
	 * messages the client needs.
	 */
	if ((rc = env->rep_start(env, NULL, DB_REP_CLIENT)) != 0)
		die("rep_start(CLIENT)", rc);
	if ((rc = riso_start_reader(env)) != 0)
		die("riso_start_reader", rc);
	vlog("client: rep_start(CLIENT) done");

	for (i = 0; i < wait_secs * 10 && !riso_startupdone; i++)
		usleep(100000);
	if (!riso_startupdone) {
		printf("CLIENT: RESULT verdict=NO_STARTUPDONE msgs_in=%lu\n",
		    riso_msgs_in);
		goto shut;
	}
	vlog("client: STARTUPDONE (msgs_in=%lu)", riso_msgs_in);

	if ((db = open_when_replicated(env, RISO_DBFILE, 30)) == NULL) {
		printf("CLIENT: RESULT verdict=DB_NEVER_REPLICATED\n");
		goto shut;
	}
	if ((sdb = open_when_replicated(env, RISO_SENTINEL_DB, 30)) == NULL) {
		printf("CLIENT: RESULT verdict=SENTINEL_NEVER_REPLICATED\n");
		(void)db->close(db, 0);
		goto shut;
	}

	/* The initial value must be there, or we are testing nothing. */
	for (i = 0; i < wait_secs * 10; i++) {
		if (riso_get(db, NULL, RISO_WKEY, &v1) == 0)
			break;
		usleep(100000);
	}
	if (i == wait_secs * 10) {
		printf("CLIENT: RESULT verdict=WKEY_NEVER_REPLICATED\n");
		(void)db->close(db, 0);
		(void)sdb->close(sdb, 0);
		goto shut;
	}
	vlog("client: %s initially = %d", RISO_WKEY, v1);

	/*
	 * THE READER.
	 *
	 * Default isolation -- no DB_READ_COMMITTED, no DB_READ_UNCOMMITTED,
	 * and DB_TXN_SNAPSHOT is rejected outright on a replication client
	 * (src/txn/txn.c:236).  So this is ordinary two-phase locking: the
	 * read lock taken by the first get is held until commit, and the
	 * second get MUST return the same value.
	 */
	if ((rc = env->txn_begin(env, NULL, &txn, 0)) != 0)
		die("txn_begin(reader)", rc);

	sentinel_before = riso_get(sdb, NULL, RISO_SENTINEL_KEY, &sv) == 0 ?
	    sv : -1;

	t_read1_ms = now_ms();
	if ((rc = riso_get(db, txn, RISO_WKEY, &v1)) != 0)
		die("reader first get", rc);
	vlog("client: read #1 %s = %d (read lock now held)", RISO_WKEY, v1);

	/* Tell the master we are holding.  From here the lock never moves. */
	flag_set(F_READER_HOLDS);

	/*
	 * Wait for the master to say it committed, then give apply a chance.
	 *
	 * Both waits are bounded, and a timeout is NOT a pass: the verdict
	 * logic below requires the new value to become visible eventually.
	 */
	if (!flag_wait(F_TRIGGER_DONE, wait_secs))
		vlog("client: master never signalled trigger_done");

	/*
	 * Poll for the sentinel WHILE STILL HOLDING the read lock.  Apply is
	 * sequential, so the sentinel becoming visible here proves the trigger
	 * transaction was applied -- i.e. apply was NOT blocked by our lock.
	 *
	 * With the fix present this poll is EXPECTED to time out.  That is the
	 * correct behaviour, and the timeout is therefore part of the normal
	 * run cost, not a failure.
	 *
	 * apply_ms is measured from trigger_done, NOT from read #1: read #1
	 * happens before the master has even finished replicating the filler
	 * records, so timing from there would measure setup, not apply.  This
	 * is the number RISO_SENTINEL_POLL_SECS must cover.
	 */
	t_apply_ms = now_ms();
	sentinel_while_holding = 0;
	for (i = 0; i < RISO_SENTINEL_POLL_SECS * 100; i++) {
		if (riso_get(sdb, NULL, RISO_SENTINEL_KEY, &sv) == 0 &&
		    sv == 1) {
			sentinel_while_holding = 1;
			break;
		}
		usleep(10000);			/* 10ms x 100 = 1s per SEC */
	}
	t_apply_ms = now_ms() - t_apply_ms;
	t_hold_ms = now_ms() - t_read1_ms;
	vlog("client: sentinel_while_holding=%d apply_ms=%ld hold_ms=%ld",
	    sentinel_while_holding, t_apply_ms, t_hold_ms);

	/* THE ASSERTION: the second read must agree with the first. */
	if ((rc = riso_get(db, txn, RISO_WKEY, &v2)) != 0)
		die("reader second get", rc);
	vlog("client: read #2 %s = %d", RISO_WKEY, v2);

	if ((rc = txn->commit(txn, 0)) != 0)
		die("reader commit", rc);
	vlog("client: reader committed, read lock released");

	/*
	 * ANTI-VACUOUS CHECK.  Now that the lock is gone, apply must get
	 * through and the new value must appear.  If it never does, this run
	 * observed nothing and is INCONCLUSIVE -- not a pass.
	 */
	v3 = v2;
	for (i = 0; i < wait_secs * 10; i++) {
		if (riso_get(db, NULL, RISO_WKEY, &v3) == 0 &&
		    v3 == RISO_NEWVAL)
			break;
		usleep(100000);
	}
	vlog("client: after release %s = %d", RISO_WKEY, v3);

	/*
	 * VERDICT.
	 *
	 * ANOMALY	 second read changed under a held read lock: apply
	 *		 modified a page it did not lock (issue #140's client
	 *		 consequence).
	 * INCONCLUSIVE	 the update never became visible at all, so the decisive
	 *		 code path was not reached.  Never a pass.
	 * PASS		 the reads agreed AND the update landed once the lock
	 *		 was released -- apply respected the lock.
	 */
	verdict_fail = 0;
	if (v2 != v1) {
		printf("CLIENT: RESULT verdict=ANOMALY v1=%d v2=%d v3=%d "
		    "sentinel_before=%d sentinel_while_holding=%d "
		    "apply_ms=%ld hold_ms=%ld msgs_in=%lu\n",
		    v1, v2, v3, sentinel_before, sentinel_while_holding,
		    t_apply_ms, t_hold_ms, riso_msgs_in);
		printf("CLIENT: a repeated read inside one transaction "
		    "returned %d then %d -- apply modified a page the reader "
		    "held a read lock on (issue #140 client consequence).\n",
		    v1, v2);
		verdict_fail = 1;
	} else if (v3 != RISO_NEWVAL) {
		printf("CLIENT: RESULT verdict=INCONCLUSIVE v1=%d v2=%d "
		    "v3=%d sentinel_before=%d sentinel_while_holding=%d "
		    "apply_ms=%ld hold_ms=%ld msgs_in=%lu\n",
		    v1, v2, v3, sentinel_before, sentinel_while_holding,
		    t_apply_ms, t_hold_ms, riso_msgs_in);
		printf("CLIENT: the replicated update never became visible, "
		    "so the reads agreeing proves nothing.\n");
		verdict_fail = 1;
	} else {
		printf("CLIENT: RESULT verdict=PASS v1=%d v2=%d v3=%d "
		    "sentinel_before=%d sentinel_while_holding=%d "
		    "apply_ms=%ld hold_ms=%ld msgs_in=%lu\n",
		    v1, v2, v3, sentinel_before, sentinel_while_holding,
		    t_apply_ms, t_hold_ms, riso_msgs_in);
		if (sentinel_while_holding)
			printf("CLIENT: NOTE apply completed while the read "
			    "lock was held (sentinel visible) yet the value "
			    "did not change -- the omitted-page window "
			    "existed but did not alter this record.\n");
	}
	(void)db->close(db, 0);
	(void)sdb->close(sdb, 0);
	flag_set(F_CLIENT_DONE);
	riso_stop_reader();
	if ((rc = env->close(env, 0)) != 0)
		die("DB_ENV->close", rc);
	return (verdict_fail);

shut:
	flag_set(F_CLIENT_DONE);
	riso_stop_reader();
	(void)env->close(env, 0);
	return (1);
}

static void
usage(void)
{
	fprintf(stderr,
"usage: test_rep_iso --role=master|client --port=N --home=DIR\n"
"                    --rendezvous=DIR [--host=H] [--wait=SECS] [-v]\n");
	exit(2);
}

int
main(int argc, char **argv)
{
	const char *home, *host, *role;
	int i, port;

	home = role = NULL;
	host = "127.0.0.1";
	port = 0;

	for (i = 1; i < argc; i++) {
		if (strncmp(argv[i], "--role=", 7) == 0)
			role = argv[i] + 7;
		else if (strncmp(argv[i], "--home=", 7) == 0)
			home = argv[i] + 7;
		else if (strncmp(argv[i], "--host=", 7) == 0)
			host = argv[i] + 7;
		else if (strncmp(argv[i], "--port=", 7) == 0)
			port = atoi(argv[i] + 7);
		else if (strncmp(argv[i], "--wait=", 7) == 0)
			wait_secs = atoi(argv[i] + 7);
		else if (strncmp(argv[i], "--rendezvous=", 13) == 0)
			(void)snprintf(rendezvous, sizeof(rendezvous), "%s",
			    argv[i] + 13);
		else if (strcmp(argv[i], "-v") == 0)
			verbose = 1;
		else
			usage();
	}
	if (role == NULL || home == NULL || port == 0 || rendezvous[0] == '\0')
		usage();

	setvbuf(stdout, NULL, _IOLBF, 0);
	printf("%s (%s)\n", db_version(NULL, NULL, NULL), role);

	if (strcmp(role, "master") == 0)
		return (run_master(home, port));
	if (strcmp(role, "client") == 0)
		return (run_client(home, host, port));
	usage();
	return (2);
}

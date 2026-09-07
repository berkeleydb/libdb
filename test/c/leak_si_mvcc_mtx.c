/*-
 * See the file LICENSE for redistribution information.
 *
 * leak_si_mvcc_mtx.c -- resource-accounting regression test for the MVCC
 * mutex-slot leak in __txn_reap_si_details (GitHub issue #138).
 *
 * Drives the trigger sequence with public APIs only: each cycle runs a
 * DB_TXN_SNAPSHOT transaction that READS one multiversion database (creating
 * a SIREAD marker, so the committed detail's si_ref is nonzero) and WRITES
 * another (so the detail also has mvcc_ref > 0 and gets parked on the
 * mvcc_txn list by __txn_end).  Later the last MVCC buffer is evicted while
 * the marker is still live, so the detail is finally reclaimed by the SIREAD
 * reaper __txn_reap_si_details -- which freed the detail WITHOUT releasing
 * td->mvcc_mtx, leaking one mutex slot per reaped detail until the mutex
 * region was exhausted (ENOMEM from a later valid operation).
 *
 * Usage: leak_si_mvcc_mtx [read|no-read]   (default: read)
 *   read     -- the trigger: the snapshot txn reads accounts.db
 *   no-read  -- control: same cycle without the read, so no SIREAD marker
 *
 * Asserts via DB_ENV->mutex_stat()/mutex_stat_print() that st_mutex_inuse
 * and the MTX_TXN_MVCC ("txn mvcc") slot count stay bounded, and that no
 * operation returns ENOMEM.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	HOME		"TESTDIR_leak_si_mvcc_mtx"
#define	ACCOUNTS	512
#define	CYCLES		700
#define	VALUE_BYTES	256

/*
 * Pre-fix, "txn mvcc" grows ~1 per cycle (10 -> 600+) and in_use grows with
 * it until ENOMEM.  Post-fix both plateau: the sweep is best-effort and fires
 * when live SIREAD markers pass half the allocated lock objects, so there is
 * a legitimate sawtooth, but its height is a function of the lock region, not
 * of CYCLES.  The sharp test is therefore the plateau check below (late
 * sample must not exceed the early one), with these as loose backstops.
 */
#define	MAX_MVCC_MUTEXES	250
#define	MAX_INUSE_OVER_BASE	400

static DB_ENV *env;
static DB *accounts, *journal;
static u_long mvcc_mutexes;
static int saw_mvcc_type;

static void
fail(const char *op, int ret)
{
	fprintf(stderr, "FAIL %s: %s (%d)\n", op, db_strerror(ret), ret);
	exit(1);
}

/*
 * DB_ENV->mutex_stat_print(0) emits one "<count>\t<type>" line per mutex
 * type in use; keep the MTX_TXN_MVCC line.  Makes no libdb call.
 */
static void
message(const DB_ENV *unused, const char *text)
{
	(void)unused;
	if (strstr(text, "\ttxn mvcc") != NULL) {
		mvcc_mutexes = strtoul(text, NULL, 10);
		saw_mvcc_type = 1;
	}
}

static u_int32_t
in_use(void)
{
	DB_MUTEX_STAT *sp;
	u_int32_t n;
	int ret;

	if ((ret = env->mutex_stat(env, &sp, 0)) != 0)
		fail("DB_ENV->mutex_stat", ret);
	n = sp->st_mutex_inuse;
	free(sp);

	mvcc_mutexes = 0;
	saw_mvcc_type = 0;
	if ((ret = env->mutex_stat_print(env, 0)) != 0)
		fail("DB_ENV->mutex_stat_print", ret);
	return (n);
}

static void
pair(DBT *key, DBT *data, char *keybuf, char *databuf)
{
	memset(key, 0, sizeof(*key));
	memset(data, 0, sizeof(*data));
	key->data = keybuf;
	key->size = (u_int32_t)strlen(keybuf);
	key->ulen = 64;
	key->flags = DB_DBT_USERMEM;
	data->data = databuf;
	data->size = VALUE_BYTES;
	data->ulen = VALUE_BYTES;
	data->flags = DB_DBT_USERMEM;
}

static int
retryable(int ret)
{
	return (ret == DB_LOCK_DEADLOCK || ret == DB_SNAPSHOT_CONFLICT ||
	    ret == DB_SNAPSHOT_UNSAFE);
}

/* One snapshot transaction: optionally read accounts, then write journal. */
static int
cycle_txn(int with_read, int cycle, char *databuf)
{
	DB_TXN *txn;
	DBT key, data;
	char keybuf[64];
	int abort_ret, ret;

	for (;;) {
		txn = NULL;
		if ((ret = env->txn_begin(env,
		    NULL, &txn, DB_TXN_SNAPSHOT)) == ENOMEM)
			return (ret);
		if (retryable(ret))
			continue;
		if (ret != 0)
			fail("DB_ENV->txn_begin", ret);

		if (with_read) {
			snprintf(keybuf, sizeof(keybuf),
			    "account-%d", cycle % ACCOUNTS);
			pair(&key, &data, keybuf, databuf);
			if ((ret = accounts->get(accounts,
			    txn, &key, &data, 0)) != 0)
				goto undo;
		}
		snprintf(keybuf, sizeof(keybuf), "entry-%d", cycle);
		pair(&key, &data, keybuf, databuf);
		if ((ret = journal->put(journal, txn, &key, &data, 0)) != 0)
			goto undo;

		if ((ret = txn->commit(txn, 0)) == 0 || ret == ENOMEM)
			return (ret);
		if (!retryable(ret))
			fail("DB_TXN->commit", ret);
		continue;

undo:		if ((abort_ret = txn->abort(txn)) != 0)
			fail("DB_TXN->abort", abort_ret);
		if (ret == ENOMEM)
			return (ret);
		if (!retryable(ret))
			fail("txn body", ret);
	}
}

int
main(int argc, char *argv[])
{
	DBT key, data;
	char keybuf[64], databuf[VALUE_BYTES];
	const char *mode;
	u_long base_mvcc, high_mvcc, first_mvcc, second_mvcc;
	u_int32_t base, high, last, first_inuse, second_inuse;
	int cycle, enomem, j, ret, with_read;

	mode = argc > 1 ? argv[1] : "read";
	if (strcmp(mode, "read") != 0 && strcmp(mode, "no-read") != 0) {
		fprintf(stderr, "usage: %s [read|no-read]\n", argv[0]);
		return (2);
	}
	with_read = strcmp(mode, "read") == 0;

	(void)mkdir(HOME, 0755);

	if ((ret = db_env_create(&env, 0)) != 0)
		fail("db_env_create", ret);
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "leak_si_mvcc_mtx");
	env->set_msgcall(env, message);
	if ((ret = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		fail("DB_ENV->set_lk_detect", ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0600)) != 0)
		fail("DB_ENV->open", ret);

	if ((ret = db_create(&accounts, env, 0)) != 0)
		fail("db_create(accounts)", ret);
	if ((ret = accounts->open(accounts, NULL, "accounts.db", NULL,
	    DB_BTREE, DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION, 0600)) != 0)
		fail("DB->open(accounts)", ret);
	if ((ret = db_create(&journal, env, 0)) != 0)
		fail("db_create(journal)", ret);
	if ((ret = journal->open(journal, NULL, "journal.db", NULL,
	    DB_BTREE, DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION, 0600)) != 0)
		fail("DB->open(journal)", ret);

	memset(databuf, 'x', sizeof(databuf));
	for (j = 0; j < ACCOUNTS; ++j) {
		snprintf(keybuf, sizeof(keybuf), "account-%d", j);
		pair(&key, &data, keybuf, databuf);
		for (;;) {
			ret = accounts->put(accounts, NULL, &key, &data, 0);
			if (ret == 0)
				break;
			if (!retryable(ret))
				fail("DB->put(preload)", ret);
		}
	}
	high = last = base = in_use();
	if (!saw_mvcc_type)
		fail("mutex_stat_print: no \"txn mvcc\" line", EINVAL);
	high_mvcc = base_mvcc = mvcc_mutexes;
	first_mvcc = second_mvcc = 0;
	first_inuse = second_inuse = 0;
	printf("baseline mode=%s in_use=%lu txn_mvcc=%lu cycles=%d\n",
	    mode, (u_long)base, base_mvcc, CYCLES);

	enomem = 0;
	for (cycle = 0; cycle < CYCLES; ++cycle) {
		if (cycle_txn(with_read, cycle, databuf) == ENOMEM) {
			printf("ENOMEM in snapshot txn at cycle %d\n", cycle);
			enomem = 1;
			break;
		}

		/* Flush, then churn the cache so MVCC buffers get evicted. */
		if ((ret = env->memp_sync(env, NULL)) != 0)
			fail("DB_ENV->memp_sync", ret);
		for (j = 0; j < ACCOUNTS; ++j) {
			snprintf(keybuf, sizeof(keybuf), "account-%d",
			    (j + 97 * cycle) % ACCOUNTS);
			pair(&key, &data, keybuf, databuf);
			for (;;) {
				ret = accounts->get(accounts,
				    NULL, &key, &data, 0);
				if (ret == 0)
					break;
				if (ret == ENOMEM) {
					printf("ENOMEM in churn get at "
					    "cycle %d\n", cycle);
					enomem = 1;
					goto done;
				}
				if (!retryable(ret))
					fail("DB->get(churn)", ret);
			}
		}

		/* One autocommit write outside any snapshot transaction. */
		snprintf(keybuf, sizeof(keybuf), "tick-%d", cycle);
		pair(&key, &data, keybuf, databuf);
		for (;;) {
			ret = journal->put(journal, NULL, &key, &data, 0);
			if (ret == 0)
				break;
			if (ret == ENOMEM) {
				printf("ENOMEM in autocommit put at "
				    "cycle %d\n", cycle);
				enomem = 1;
				goto done;
			}
			if (!retryable(ret))
				fail("DB->put(autocommit)", ret);
		}
		if ((cycle + 1) % 100 == 0 &&
		    (ret = env->txn_checkpoint(env, 0, 0, 0)) != 0)
			fail("DB_ENV->txn_checkpoint", ret);

		last = in_use();
		if (last > high)
			high = last;
		if (mvcc_mutexes > high_mvcc)
			high_mvcc = mvcc_mutexes;
		/* Peak per half of the run; see the locker test for why. */
		if (cycle < CYCLES / 2) {
			if (mvcc_mutexes > first_mvcc)
				first_mvcc = mvcc_mutexes;
			if (last > first_inuse)
				first_inuse = last;
		} else {
			if (mvcc_mutexes > second_mvcc)
				second_mvcc = mvcc_mutexes;
			if (last > second_inuse)
				second_inuse = last;
		}
		if ((cycle + 1) % 100 == 0)
			printf("  after %4d cycles: in_use=%lu txn_mvcc=%lu\n",
			    cycle + 1, (u_long)last, mvcc_mutexes);
	}

done:	printf("final mode=%s cycles=%d enomem=%d in_use=%lu->%lu (peak %lu) "
	    "txn_mvcc=%lu->%lu (peak %lu) halfpeak in_use=%lu/%lu "
	    "txn_mvcc=%lu/%lu\n", mode, cycle, enomem,
	    (u_long)base, (u_long)last, (u_long)high,
	    base_mvcc, mvcc_mutexes, high_mvcc,
	    (u_long)first_inuse, (u_long)second_inuse,
	    first_mvcc, second_mvcc);

	if ((ret = accounts->close(accounts, 0)) != 0)
		fail("DB->close(accounts)", ret);
	if ((ret = journal->close(journal, 0)) != 0)
		fail("DB->close(journal)", ret);
	if ((ret = env->close(env, 0)) != 0)
		fail("DB_ENV->close", ret);

	ret = 0;
	if (enomem || cycle != CYCLES) {
		fprintf(stderr, "FAIL: %s mode did not complete %d cycles "
		    "(reached %d, enomem %d)\n", mode, CYCLES, cycle, enomem);
		ret = 1;
	}
	if (high_mvcc > MAX_MVCC_MUTEXES) {
		fprintf(stderr, "FAIL: \"txn mvcc\" mutexes grew to %lu "
		    "(limit %lu) -- reaped details leak their mvcc_mtx\n",
		    high_mvcc, (u_long)MAX_MVCC_MUTEXES);
		ret = 1;
	}
	if (high > base + MAX_INUSE_OVER_BASE) {
		fprintf(stderr, "FAIL: mutex slots in use grew to %lu from "
		    "base %lu (limit +%lu)\n", (u_long)high, (u_long)base,
		    (u_long)MAX_INUSE_OVER_BASE);
		ret = 1;
	}
	/*
	 * The leak signature: the counts track the cycle count.  A plateau or
	 * sawtooth has equal peaks in both halves of the run; a per-transaction
	 * leak makes the second half's peak strictly larger.
	 */
	if (first_mvcc != 0 && second_mvcc > first_mvcc) {
		fprintf(stderr, "FAIL: peak \"txn mvcc\" mutexes rose from %lu "
		    "(first half) to %lu (second half) -- reaped details still "
		    "leak their mvcc_mtx\n", first_mvcc, second_mvcc);
		ret = 1;
	}
	if (first_inuse != 0 && second_inuse > first_inuse) {
		fprintf(stderr, "FAIL: peak mutex slots in use rose from %lu "
		    "(first half) to %lu (second half)\n",
		    (u_long)first_inuse, (u_long)second_inuse);
		ret = 1;
	}
	printf("%s: mode=%s\n", ret == 0 ? "PASS" : "FAIL", mode);
	return (ret);
}

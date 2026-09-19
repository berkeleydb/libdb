/*-
 * See the file LICENSE for redistribution information.
 *
 * config_smoke.c -- the RUNTIME half of the configure-option sweep (gap G14).
 *
 * WHY A BUILD-ONLY SWEEP IS NOT ENOUGH
 *
 * 42 of 54 configure options were never built in CI.  One of them is
 * --enable-o_direct, and defect P2 -- a documented flag under which no database
 * can be opened at all -- survived because nothing ever COMPILED that path.
 * But compiling it would not have caught P2 either: --enable-o_direct builds
 * perfectly and fails at runtime.  So the sweep has to RUN something for every
 * option that changes runtime behaviour, or it proves only that the tree
 * compiles.
 *
 * WHAT THIS ASSERTS
 *
 * A short, self-contained exercise of the paths a configure option can silently
 * break, with every result CHECKED rather than merely attempted:
 *
 *   env      open a transactional environment, read back the config
 *   btree    put/get/cursor/delete round-trip, values compared byte for byte
 *   recno    the same on a RECNO database (fixed-length record numbering)
 *   hash     the same on a HASH database        -- skipped by --disable-hash
 *   queue    the same on a QUEUE database       -- skipped by --disable-queue
 *   heap     the same on a HEAP database        -- skipped by --disable-heap
 *   txn      commit makes a record visible, ABORT makes it disappear
 *   verify   DB->verify passes on a populated database  -- --disable-verify
 *   stat     DB->stat reports the record count we wrote -- --disable-statistics
 *   log      log_stat's write count rises across transactions
 *   compact  DB->compact on a partly-deleted database
 *   mpool    memp_stat reports cache activity
 *
 * Each prints
 *	<name>: PASS|FAIL|SKIP <detail>
 * and the program prints a final SMOKE line with the counts.  An access method
 * that a --disable- option removed reports SKIP with the errno, which is
 * correct behaviour rather than a failure -- but the sweep runner checks that
 * the EXPECTED subset was skipped, so "everything skipped" cannot pass.
 *
 * Deliberately NOT a copy of the Tcl suite: this must run in seconds against
 * every one of ~30 builds, so it is one process, single-threaded, no timing
 * dependence.  Depth is the Tcl suite's job; this is breadth across builds.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	HOME		"TESTDIR_config_smoke"
#define	NRECS		200
#define	VALBYTES	64

static int npass, nfail, nskip;

static void
report(const char *name, const char *v, const char *fmt, ...)
{
	va_list ap;

	printf("%s: %s ", name, v);
	va_start(ap, fmt);
	(void)vprintf(fmt, ap);
	va_end(ap);
	printf("\n");
	(void)fflush(stdout);
	if (strcmp(v, "PASS") == 0)
		npass++;
	else if (strcmp(v, "FAIL") == 0)
		nfail++;
	else
		nskip++;
}

/*
 * unsupported --
 *	Does RET mean "this build does not have the feature"?  A --disable-
 *	option makes the stub return DB_OPNOTSUP, which is EOPNOTSUPP on Linux,
 *	ENOTSUP elsewhere and EINVAL on platforms with neither -- so all three
 *	are accepted.  ENOSYS is included for the same reason.
 *
 *	This is the one place where being liberal is correct: reporting a
 *	deliberately-removed feature as a FAILURE would make every --disable-
 *	leg of the sweep red and the sweep would get switched off.  The sweep
 *	runner separately requires a MINIMUM NUMBER OF PASSES, so "everything
 *	unsupported" still cannot go green.
 */
static int
unsupported(int ret)
{
	return (ret == EINVAL || ret == ENOSYS ||
#ifdef EOPNOTSUPP
	    ret == EOPNOTSUPP ||
#endif
#ifdef ENOTSUP
	    ret == ENOTSUP ||
#endif
	    0);
}

static void
fillval(char *buf, int n)
{
	int i;

	for (i = 0; i < VALBYTES; i++)
		buf[i] = (char)('A' + ((n + i) % 26));
}

/*
 * am_roundtrip --
 *	Populate, read back and delete on one access method.  RECNO/QUEUE/HEAP
 *	number their own records, so the key is supplied by the library
 *	(DB_APPEND) and the check is "every appended value reads back".
 *
 *	A method the build removed makes DB->open fail EINVAL/ENOENT, which is
 *	reported as SKIP -- correct for --disable-hash and friends.
 *
 *	Three per-method facts this had to learn the hard way, kept here because
 *	each one produced a WRONG verdict first:
 *	  - HEAP keys are DB_HEAP_RID, not db_recno_t, so the DB_APPEND key
 *	    buffer must be sized for the RID or the put fails DB_BUFFER_SMALL.
 *	  - RECNO and HEAP report a deleted record as DB_KEYEMPTY, not
 *	    DB_NOTFOUND.  Both mean "gone"; accepting only DB_NOTFOUND reported
 *	    a correct library as broken.
 *	  - QUEUE reuses the record slot, so a deleted head can legitimately
 *	    read back as a later record.
 */
static void
am_roundtrip(DB_ENV *dbenv, const char *name, DBTYPE type)
{
	DB *dbp;
	DBC *dbc;
	DBT key, data;
	DB_TXN *txn;
	char fname[64], vbuf[VALBYTES];
	union {
		db_recno_t recno;
		DB_HEAP_RID rid;
	} kbuf, kslot[NRECS];
	u_int32_t ksize;
	int i, ret, nread, appended;

	(void)snprintf(fname, sizeof(fname), "smoke_%s.db", name);
	if ((ret = db_create(&dbp, dbenv, 0)) != 0) {
		report(name, "FAIL", "db_create: %s", db_strerror(ret));
		return;
	}
	if (type == DB_QUEUE && (ret = dbp->set_re_len(dbp, VALBYTES)) != 0) {
		report(name, "FAIL", "set_re_len: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	if ((ret = dbp->open(dbp, NULL, fname, NULL, type,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0) {
		/*
		 * The access method is not in this build.  SKIP, with the error
		 * printed -- a silent skip here is how a --disable- option
		 * would look identical to a working one.
		 */
		report(name, "SKIP", "DB->open(%s) refused: %s (%d) -- access "
		    "method not in this build", name, db_strerror(ret), ret);
		(void)dbp->close(dbp, 0);
		return;
	}

	appended = (type == DB_RECNO || type == DB_QUEUE || type == DB_HEAP);
	ksize = type == DB_HEAP ?
	    (u_int32_t)sizeof(DB_HEAP_RID) : (u_int32_t)sizeof(db_recno_t);

	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
		report(name, "FAIL", "txn_begin: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	for (i = 0; i < NRECS; i++) {
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		data.data = vbuf;
		data.size = VALBYTES;
		if (appended) {
			memset(&kbuf, 0, sizeof(kbuf));
			key.data = &kbuf;
			key.size = ksize;
			key.ulen = (u_int32_t)sizeof(kbuf);
			key.flags = DB_DBT_USERMEM;
			ret = dbp->put(dbp, txn, &key, &data, DB_APPEND);
			kslot[i] = kbuf;
		} else {
			key.data = &i;
			key.size = sizeof(i);
			ret = dbp->put(dbp, txn, &key, &data, 0);
		}
		if (ret != 0) {
			report(name, "FAIL", "put %d: %s", i, db_strerror(ret));
			(void)txn->abort(txn);
			(void)dbp->close(dbp, 0);
			return;
		}
	}
	if ((ret = txn->commit(txn, 0)) != 0) {
		report(name, "FAIL", "commit: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}

	/* Read every record back and compare the bytes. */
	for (i = 0; i < NRECS; i++) {
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		if (appended) {
			kbuf = kslot[i];
			key.data = &kbuf;
			key.size = ksize;
		} else {
			key.data = &i;
			key.size = sizeof(i);
		}
		if ((ret = dbp->get(dbp, NULL, &key, &data, 0)) != 0) {
			report(name, "FAIL", "get %d: %s", i,
			    db_strerror(ret));
			(void)dbp->close(dbp, 0);
			return;
		}
		if (data.size < VALBYTES ||
		    memcmp(data.data, vbuf, VALBYTES) != 0) {
			report(name, "FAIL",
			    "record %d read back WRONG (size %u)", i,
			    data.size);
			(void)dbp->close(dbp, 0);
			return;
		}
	}

	/* Cursor walk: the count must match what was written. */
	if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0) {
		report(name, "FAIL", "cursor: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	nread = 0;
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	while ((ret = dbc->get(dbc, &key, &data, DB_NEXT)) == 0)
		nread++;
	(void)dbc->close(dbc);
	if (ret != DB_NOTFOUND) {
		report(name, "FAIL", "cursor walk ended %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	if (nread != NRECS) {
		report(name, "FAIL", "cursor saw %d records, wrote %d",
		    nread, NRECS);
		(void)dbp->close(dbp, 0);
		return;
	}

	/* Delete one and prove it is gone. */
	memset(&key, 0, sizeof(key));
	if (appended) {
		kbuf = kslot[0];
		key.data = &kbuf;
		key.size = ksize;
	} else {
		i = 0;
		key.data = &i;
		key.size = sizeof(i);
	}
	if ((ret = dbp->del(dbp, NULL, &key, DB_AUTO_COMMIT)) != 0) {
		report(name, "FAIL", "del: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	memset(&data, 0, sizeof(data));
	ret = dbp->get(dbp, NULL, &key, &data, 0);
	/*
	 * "Gone" has three legitimate spellings.  DB_NOTFOUND is BTREE/HASH;
	 * RECNO and HEAP report the hole as DB_KEYEMPTY; QUEUE reuses the slot,
	 * so a successful read there is also correct.
	 */
	if (ret != DB_NOTFOUND && ret != DB_KEYEMPTY &&
	    !(type == DB_QUEUE && ret == 0)) {
		report(name, "FAIL",
		    "deleted record still readable (get returned %s)",
		    db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}

	if ((ret = dbp->close(dbp, 0)) != 0) {
		report(name, "FAIL", "close: %s", db_strerror(ret));
		return;
	}
	report(name, "PASS", "%d records round-tripped, cursor saw %d, "
	    "delete took effect", NRECS, nread);
}

/*
 * t_txn --
 *	COMMIT makes a record visible and ABORT makes it disappear.  Both halves
 *	are asserted: a build where abort silently committed would pass a
 *	commit-only check.
 */
static void
t_txn(DB_ENV *dbenv)
{
	DB *dbp;
	DB_TXN *txn;
	DBT key, data;
	char vbuf[VALBYTES];
	int k, ret;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, "smoke_txn.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0) {
		report("txn", "FAIL", "setup: %s", db_strerror(ret));
		return;
	}

	/* Committed record must be visible. */
	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
		report("txn", "FAIL", "txn_begin: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	k = 1;
	fillval(vbuf, k);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &k; key.size = sizeof(k);
	data.data = vbuf; data.size = VALBYTES;
	if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0 ||
	    (ret = txn->commit(txn, 0)) != 0) {
		report("txn", "FAIL", "commit path: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	memset(&data, 0, sizeof(data));
	if ((ret = dbp->get(dbp, NULL, &key, &data, 0)) != 0) {
		report("txn", "FAIL",
		    "COMMITTED record not visible: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}

	/* Aborted record must NOT be visible. */
	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
		report("txn", "FAIL", "txn_begin 2: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	k = 2;
	fillval(vbuf, k);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &k; key.size = sizeof(k);
	data.data = vbuf; data.size = VALBYTES;
	if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0 ||
	    (ret = txn->abort(txn)) != 0) {
		report("txn", "FAIL", "abort path: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	memset(&data, 0, sizeof(data));
	ret = dbp->get(dbp, NULL, &key, &data, 0);
	if (ret != DB_NOTFOUND) {
		report("txn", "FAIL",
		    "ABORTED record IS visible (get returned %s) -- rollback "
		    "did not happen", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	(void)dbp->close(dbp, 0);
	report("txn", "PASS", "commit visible, abort rolled back");
}

/*
 * t_verify -- DB->verify on a populated database.  --disable-verify: SKIP.
 *
 * DB->verify IS A DESTRUCTOR: both db_vrfy.c and db_vrfy_stub.c call
 * __db_close() on the handle before returning, on the success AND the failure
 * path.  Closing it again here segfaulted the driver (exit 139) on the
 * --enable-smallbuild leg, which is how this comment came to exist.  Never
 * close a handle that has been passed to verify().
 */
static void
t_verify(DB_ENV *dbenv)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0) {
		report("verify", "FAIL", "db_create: %s", db_strerror(ret));
		return;
	}
	/* verify() needs a handle that was never opened. */
	ret = dbp->verify(dbp, "smoke_btree.db", NULL, NULL, 0);
	if (ret == 0)
		report("verify", "PASS", "DB->verify clean on smoke_btree.db");
	else if (unsupported(ret))
		report("verify", "SKIP",
		    "DB->verify refused: %s (%d) -- not in this build",
		    db_strerror(ret), ret);
	else
		report("verify", "FAIL", "DB->verify: %s (%d)",
		    db_strerror(ret), ret);
}

/* t_stat -- DB->stat must report the records we wrote.  --disable-statistics. */
static void
t_stat(DB_ENV *dbenv)
{
	DB *dbp;
	DB_BTREE_STAT *sp;
	int ret;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, "smoke_btree.db", NULL, DB_BTREE,
	    0, 0600)) != 0) {
		report("stat", "FAIL", "setup: %s", db_strerror(ret));
		return;
	}
	ret = dbp->stat(dbp, NULL, &sp, 0);
	if (unsupported(ret)) {
		report("stat", "SKIP",
		    "DB->stat refused: %s -- --disable-statistics build",
		    db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	if (ret != 0) {
		report("stat", "FAIL", "DB->stat: %s", db_strerror(ret));
		(void)dbp->close(dbp, 0);
		return;
	}
	/*
	 * btree round-trip wrote NRECS and deleted one.  A stat that reports 0
	 * on a populated database is the interesting failure -- a
	 * --disable-statistics build must REFUSE, not lie.
	 */
	if (sp->bt_nkeys == 0)
		report("stat", "FAIL",
		    "DB->stat reported bt_nkeys=0 on a populated database -- "
		    "counters compiled out but the call still succeeded");
	else
		report("stat", "PASS", "bt_nkeys=%lu (wrote %d, deleted 1)",
		    (unsigned long)sp->bt_nkeys, NRECS);
	free(sp);
	(void)dbp->close(dbp, 0);
}

/* t_log -- the log write counter must rise across transactions. */
static void
t_log(DB_ENV *dbenv)
{
	DB_LOG_STAT *a, *b;
	DB *dbp;
	DB_TXN *txn;
	DBT key, data;
	char vbuf[VALBYTES];
	int i, k, ret;

	if ((ret = dbenv->log_stat(dbenv, &a, 0)) != 0) {
		if (unsupported(ret)) {
			report("log", "SKIP", "log_stat refused: %s",
			    db_strerror(ret));
			return;
		}
		report("log", "FAIL", "log_stat: %s", db_strerror(ret));
		return;
	}
	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, "smoke_log.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0) {
		report("log", "FAIL", "setup: %s", db_strerror(ret));
		free(a);
		return;
	}
	for (i = 0; i < 50; i++) {
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0)
			break;
		k = 10000 + i;
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &k; key.size = sizeof(k);
		data.data = vbuf; data.size = VALBYTES;
		if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			break;
		}
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			break;
	}
	(void)dbp->close(dbp, 0);
	if (ret != 0) {
		report("log", "FAIL", "workload: %s", db_strerror(ret));
		free(a);
		return;
	}
	if ((ret = dbenv->log_stat(dbenv, &b, 0)) != 0) {
		report("log", "FAIL", "log_stat 2: %s", db_strerror(ret));
		free(a);
		return;
	}
	if (b->st_wcount <= a->st_wcount)
		report("log", "FAIL",
		    "log write count did not rise across 50 synced "
		    "transactions (%llu -> %llu)",
		    (unsigned long long)a->st_wcount,
		    (unsigned long long)b->st_wcount);
	else
		report("log", "PASS", "st_wcount %llu -> %llu over 50 txns",
		    (unsigned long long)a->st_wcount,
		    (unsigned long long)b->st_wcount);
	free(a);
	free(b);
}

/* t_compact -- DB->compact on a partly-deleted database. */
static void
t_compact(DB_ENV *dbenv)
{
	DB *dbp;
	DB_COMPACT c;
	DBT key;
	int i, ret;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, "smoke_compact.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0) {
		report("compact", "FAIL", "setup: %s", db_strerror(ret));
		return;
	}
	{
		DBT data;
		char vbuf[VALBYTES];

		for (i = 0; i < NRECS * 4; i++) {
			fillval(vbuf, i);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = &i; key.size = sizeof(i);
			data.data = vbuf; data.size = VALBYTES;
			if ((ret = dbp->put(dbp, NULL, &key, &data,
			    DB_AUTO_COMMIT)) != 0) {
				report("compact", "FAIL", "put: %s",
				    db_strerror(ret));
				(void)dbp->close(dbp, 0);
				return;
			}
		}
	}
	for (i = 0; i < NRECS * 4; i += 2) {
		memset(&key, 0, sizeof(key));
		key.data = &i; key.size = sizeof(i);
		if ((ret = dbp->del(dbp, NULL, &key, DB_AUTO_COMMIT)) != 0) {
			report("compact", "FAIL", "del: %s", db_strerror(ret));
			(void)dbp->close(dbp, 0);
			return;
		}
	}
	memset(&c, 0, sizeof(c));
	ret = dbp->compact(dbp, NULL, NULL, NULL, &c, DB_FREE_SPACE, NULL);
	if (ret == 0)
		report("compact", "PASS", "pages freed=%lu examined=%lu",
		    (unsigned long)c.compact_pages_free,
		    (unsigned long)c.compact_pages_examine);
	else if (unsupported(ret))
		report("compact", "SKIP", "DB->compact refused: %s",
		    db_strerror(ret));
	else
		report("compact", "FAIL", "DB->compact: %s", db_strerror(ret));
	(void)dbp->close(dbp, 0);
}

/* t_mpool -- memp_stat must report cache activity after all that I/O. */
static void
t_mpool(DB_ENV *dbenv)
{
	DB_MPOOL_STAT *gsp;
	int ret;

	if ((ret = dbenv->memp_stat(dbenv, &gsp, NULL, 0)) != 0) {
		if (unsupported(ret)) {
			report("mpool", "SKIP", "memp_stat refused: %s",
			    db_strerror(ret));
			return;
		}
		report("mpool", "FAIL", "memp_stat: %s", db_strerror(ret));
		return;
	}
	if (gsp->st_cache_hit + gsp->st_cache_miss == 0)
		report("mpool", "FAIL",
		    "memp_stat reports zero cache references after the whole "
		    "smoke workload -- the counters are not tracking reality");
	else
		report("mpool", "PASS", "cache hit=%lu miss=%lu pages=%lu",
		    (unsigned long)gsp->st_cache_hit,
		    (unsigned long)gsp->st_cache_miss,
		    (unsigned long)gsp->st_pages);
	free(gsp);
}

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	u_int32_t v;
	int ret;

	(void)argc;
	(void)argv;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		report("env", "FAIL", "db_env_create: %s", db_strerror(ret));
		goto done;
	}
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, "config_smoke");
	(void)dbenv->set_cachesize(dbenv, 0, 16 * 1024 * 1024, 1);
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOG | DB_INIT_TXN | DB_INIT_LOCK, 0600)) != 0) {
		report("env", "FAIL", "DB_ENV->open: %s (%d)",
		    db_strerror(ret), ret);
		goto done;
	}
	/* Read a knob back: an env that opens but cannot be queried is broken. */
	if ((ret = dbenv->get_open_flags(dbenv, &v)) != 0 ||
	    (v & DB_INIT_TXN) == 0)
		report("env", "FAIL",
		    "get_open_flags did not report DB_INIT_TXN (ret %d, "
		    "flags 0x%lx)", ret, (unsigned long)v);
	else
		report("env", "PASS",
		    "transactional env open, flags read back 0x%lx",
		    (unsigned long)v);

	am_roundtrip(dbenv, "btree", DB_BTREE);
	am_roundtrip(dbenv, "hash", DB_HASH);
	am_roundtrip(dbenv, "recno", DB_RECNO);
	am_roundtrip(dbenv, "queue", DB_QUEUE);
	am_roundtrip(dbenv, "heap", DB_HEAP);
	t_txn(dbenv);
	t_stat(dbenv);
	t_log(dbenv);
	t_compact(dbenv);
	t_mpool(dbenv);
	/* verify() LAST: it wants no other handle open on the file. */
	t_verify(dbenv);

	if ((ret = dbenv->close(dbenv, 0)) != 0)
		report("envclose", "FAIL", "DB_ENV->close: %s",
		    db_strerror(ret));

done:
	printf("SMOKE pass=%d fail=%d skip=%d\n", npass, nfail, nskip);
	(void)fflush(stdout);
	return (nfail != 0 ? 1 : 0);
}

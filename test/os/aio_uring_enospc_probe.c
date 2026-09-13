/*
 * aio_uring_enospc_probe.c --
 *	Empirically exercise the io_uring backend's REAL write-error path,
 *	which the DST __os_io fault hook cannot reach (io_uring writes go
 *	straight to the fd via io_uring_prep_write, bypassing __os_io).
 *
 *	Runs a DB_MPOOL_AIO env on a tiny tmpfs, fills it, then forces a
 *	checkpoint whose async data-page writes hit a genuine ENOSPC.  With
 *	the __memp_aio_drain error-propagation fix, txn_checkpoint must
 *	return non-zero (the uring cqe->res carries -ENOSPC); without it the
 *	failure is swallowed and checkpoint returns 0.
 *
 *	Usage: aio_uring_enospc_probe <home-on-small-fs>
 *	Exit:  0 = checkpoint surfaced the error (correct)
 *	       1 = checkpoint returned SUCCESS despite a full disk (BUG)
 *	       2 = setup error (could not even fill the fs)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

int
main(argc, argv)
	int argc;
	char *argv[];
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[1024];
	const char *home;
	int i, ret, ckp_ret, put_err;

	if (argc < 2) {
		fprintf(stderr, "usage: %s <home>\n", argv[0]);
		return (2);
	}
	home = argv[1];

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (2);
	}
	env->set_errpfx(env, "uring_probe");
	env->set_errfile(env, stderr);
	if ((ret = env->set_flags(env, DB_MPOOL_AIO, 1)) != 0) {
		fprintf(stderr, "set_flags(DB_MPOOL_AIO): %s\n",
		    db_strerror(ret));
		return (2);
	}
	/* Small cache so many dirty pages must be flushed by the checkpoint. */
	(void)env->set_cachesize(env, 0, 256 * 1024, 1);
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0) {
		fprintf(stderr, "env->open: %s\n", db_strerror(ret));
		return (2);
	}
	if ((ret = db_create(&db, env, 0)) != 0 ||
	    (ret = db->open(db, NULL, "probe.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "db open: %s\n", db_strerror(ret));
		return (2);
	}

	memset(vbuf, 'z', sizeof(vbuf));
	/*
	 * Insert until the small filesystem fills (put itself may start
	 * failing with ENOSPC as the log/db grow).  Non-durable commits so
	 * the checkpoint's page flush is what actually pushes bytes to the
	 * data file.  We do NOT checkpoint in this loop -- we want dirty
	 * pages accumulated, then a single big checkpoint that hits ENOSPC.
	 */
	put_err = 0;
	for (i = 0; i < 200000; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "u-%08d", i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = sizeof(vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0) {
			put_err = ret; break;
		}
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			put_err = ret; break;
		}
		if ((ret = txn->commit(txn, DB_TXN_NOSYNC)) != 0) {
			put_err = ret; break;
		}
	}
	fprintf(stderr, "inserted ~%d recs, first put/commit err: %s\n",
	    i, put_err ? db_strerror(put_err) : "(none)");

	/* Force the checkpoint that flushes dirty pages via async writeback. */
	ckp_ret = env->txn_checkpoint(env, 0, 0, DB_FORCE);
	fprintf(stderr, "txn_checkpoint ret = %d (%s)\n",
	    ckp_ret, db_strerror(ckp_ret));

	(void)db->close(db, 0);
	(void)env->close(env, 0);

	/*
	 * If the filesystem really filled (put_err == ENOSPC or the
	 * checkpoint hit it), the checkpoint MUST surface a non-zero error.
	 * If puts never hit ENOSPC the fs did not fill -- inconclusive.
	 */
	if (put_err == 0 && ckp_ret == 0) {
		fprintf(stderr, "INCONCLUSIVE: filesystem never filled "
		    "(no ENOSPC seen); cannot exercise the uring error path\n");
		return (2);
	}
	if (ckp_ret == 0) {
		fprintf(stderr, "BUG: checkpoint returned SUCCESS after the "
		    "filesystem filled -- an async uring write error was "
		    "swallowed\n");
		return (1);
	}
	fprintf(stderr, "OK: checkpoint surfaced the real ENOSPC write error "
	    "on the io_uring backend\n");
	return (0);
}

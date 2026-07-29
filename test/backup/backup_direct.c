/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2024 Oracle and/or its affiliates.  All rights reserved.
 *
 * backup_direct.c --
 *	A standalone driver for Berkeley DB's hot-backup configuration and
 *	callback API (src/env/env_backup.c) and the environment backup engine
 *	it feeds (src/db/db_backup.c, DB_ENV->backup / DB_ENV->dbbackup).
 *
 *	The Tcl harness (test/tcl/backup.tcl) drives hot backup only through
 *	the db_hotbackup utility, which calls DB_ENV->backup() with a NULL
 *	backup_handle -- so env_backup.c's four config setters/getters and
 *	callback setter/getter are never reached, and the backup->open/write/
 *	close callback branches in db_backup.c stay cold.  This program calls
 *	those public entry points directly, as an application embedding BDB
 *	backup would:
 *
 *	  __env_get_backup_config (before alloc -> EINVAL branch, then all 4
 *	      config enums READ_COUNT/READ_SLEEP/SIZE/WRITE_DIRECT),
 *	  __env_set_backup_config (all 4 enums, WRITE_DIRECT on and off ->
 *	      F_SET/F_CLR branches, plus __env_backup_alloc first call),
 *	  __env_get_backup_callbacks (before alloc -> EINVAL branch),
 *	  __env_set_backup_callbacks (drives __env_backup_alloc's "already
 *	      allocated" early-return branch), then get again to read them back.
 *
 *	It then builds a small transactional environment with a btree db, and
 *	runs DB_ENV->backup() with the write callbacks installed so the
 *	backup->open/write/close paths in db_backup.c execute, and finally a
 *	DB_ENV->dbbackup() single-database backup.  A hard SIGALRM guard aborts
 *	the process if anything blocks; every handle is closed before exit.
 */

#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"BACKUP_TESTDIR"
#define	HOME_TGT	"BACKUP_TESTDIR/bak"
#define	TABLE		"backup_table.db"
#define	NRECS		200
#define	ALARM_SECS	60		/* hard self-timeout: never hang */

static int fails = 0;

#define	CHK(call, want) do {						\
	int _r = (call);						\
	if (_r != (want)) {						\
		fprintf(stderr,						\
		    "FAIL: %s:%d: %s => %d, expected %d\n",		\
		    __FILE__, __LINE__, #call, _r, (want));		\
		fails++;						\
	}								\
} while (0)

#define	CHK0(call)	CHK((call), 0)

/* Backup callbacks -- a minimal file-copy sink that mirrors the default. */
static int
bk_open(DB_ENV *dbenv, const char *dbname, const char *target, void **handle)
{
	char path[1024];
	FILE *fp;

	(void)dbenv;
	(void)snprintf(path, sizeof(path), "%s/%s", target, dbname);
	if ((fp = fopen(path, "wb")) == NULL)
		return (errno == 0 ? EIO : errno);
	*handle = fp;
	return (0);
}

static int
bk_write(DB_ENV *dbenv, u_int32_t off_gb, u_int32_t off, u_int32_t size,
    u_int8_t *buf, void *handle)
{
	FILE *fp = handle;

	(void)dbenv;
	(void)off_gb;
	if (fseek(fp, (long)off, SEEK_SET) != 0)
		return (EIO);
	if (fwrite(buf, 1, size, fp) != size)
		return (EIO);
	return (0);
}

static int
bk_close(DB_ENV *dbenv, const char *dbname, void *handle)
{
	FILE *fp = handle;

	(void)dbenv;
	(void)dbname;
	if (fp != NULL && fclose(fp) != 0)
		return (EIO);
	return (0);
}

/* Exercise env_backup.c's config + callback API in isolation. */
static void
test_config_api(DB_ENV *dbenv)
{
	u_int32_t v;
	int (*op)(DB_ENV *, const char *, const char *, void **);
	int (*wr)(DB_ENV *, u_int32_t, u_int32_t, u_int32_t, u_int8_t *, void *);
	int (*cl)(DB_ENV *, const char *, void *);

	/* Getters before any handle exists must fail with EINVAL. */
	CHK(dbenv->get_backup_config(dbenv, DB_BACKUP_SIZE, &v), EINVAL);
	CHK(dbenv->get_backup_callbacks(dbenv, &op, &wr, &cl), EINVAL);

	/* Set every config enum -- first set allocates the handle. */
	CHK0(dbenv->set_backup_config(dbenv, DB_BACKUP_READ_COUNT, 1024));
	CHK0(dbenv->set_backup_config(dbenv, DB_BACKUP_READ_SLEEP, 500));
	CHK0(dbenv->set_backup_config(dbenv, DB_BACKUP_SIZE, 1 << 20));
	/* WRITE_DIRECT on then off -> both F_SET and F_CLR branches. */
	CHK0(dbenv->set_backup_config(dbenv, DB_BACKUP_WRITE_DIRECT, 1));
	CHK0(dbenv->set_backup_config(dbenv, DB_BACKUP_WRITE_DIRECT, 0));

	/* Read each one back. */
	CHK0(dbenv->get_backup_config(dbenv, DB_BACKUP_READ_COUNT, &v));
	if (v != 1024) { fprintf(stderr, "FAIL: read_count=%u\n", v); fails++; }
	CHK0(dbenv->get_backup_config(dbenv, DB_BACKUP_READ_SLEEP, &v));
	if (v != 500) { fprintf(stderr, "FAIL: read_sleep=%u\n", v); fails++; }
	CHK0(dbenv->get_backup_config(dbenv, DB_BACKUP_SIZE, &v));
	if (v != (1 << 20)) { fprintf(stderr, "FAIL: size=%u\n", v); fails++; }
	CHK0(dbenv->get_backup_config(dbenv, DB_BACKUP_WRITE_DIRECT, &v));
	if (v != 0) { fprintf(stderr, "FAIL: write_direct=%u\n", v); fails++; }

	/* Install callbacks (handle already allocated -> alloc early return). */
	CHK0(dbenv->set_backup_callbacks(dbenv, bk_open, bk_write, bk_close));
	CHK0(dbenv->get_backup_callbacks(dbenv, &op, &wr, &cl));
	if (op != bk_open || wr != bk_write || cl != bk_close) {
		fprintf(stderr, "FAIL: callbacks not read back\n");
		fails++;
	}
}

int
main(void)
{
	DB_ENV *dbenv;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	u_int32_t i;
	int ret;
	char kbuf[32], vbuf[64];

	(void)signal(SIGALRM, SIG_DFL);
	(void)alarm(ALARM_SECS);

	/* Clean start (no rm -rf). */
	(void)system("rm -f " HOME "/__db.* " HOME "/log.* " HOME "/*.db "
	    HOME "/DB_CONFIG " HOME_TGT "/* 2>/dev/null");
	(void)mkdir(HOME, 0755);
	(void)mkdir(HOME_TGT, 0755);

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	dbenv->set_errpfx(dbenv, "backup_direct");

	/* Exercise the config/callback API before opening the env. */
	test_config_api(dbenv);

	CHK0(dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0644));

	CHK0(db_create(&db, dbenv, 0));
	CHK0(db->open(db, NULL, TABLE, NULL,
	    DB_BTREE, DB_CREATE | DB_AUTO_COMMIT, 0644));

	CHK0(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	for (i = 0; i < NRECS; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kbuf, sizeof(kbuf), "key%08u", i);
		(void)snprintf(vbuf, sizeof(vbuf), "value-%08u-payload", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		CHK0(db->put(db, txn, &key, &data, 0));
	}
	CHK0(txn->commit(txn, 0));
	CHK0(db->close(db, 0));

	/*
	 * DB_ENV->backup with the write callbacks installed: exercises the
	 * backup->open/write/close branches in db_backup.c.  DB_CREATE makes
	 * the target dir, DB_BACKUP_FILES copies db files.
	 */
	CHK0(dbenv->backup(dbenv, HOME_TGT,
	    DB_CREATE | DB_BACKUP_FILES | DB_BACKUP_CLEAN));

	/* Single-database backup -> DB_ENV->dbbackup on one file. */
	CHK0(dbenv->dbbackup(dbenv, TABLE, HOME_TGT, 0));

	CHK0(dbenv->close(dbenv, 0));

	if (fails != 0) {
		fprintf(stderr, "backup_direct: %d checks FAILED\n", fails);
		return (EXIT_FAILURE);
	}
	printf("backup_direct: PASS\n");
	return (EXIT_SUCCESS);
}

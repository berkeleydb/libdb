/*-
 * test/fuzz/fuzz_recover.c --
 *	Feed fuzz bytes as a transaction log file and run recovery.
 *
 *	Recovery (__db_apprec, reached via DB_ENV->open with DB_RECOVER)
 *	reads log records back and replays them.  A malformed log record --
 *	bad length, bogus type, truncated payload -- is parsed by the log
 *	cursor (__log_get) and dispatched to per-record recovery routines.
 *	This is the recovery-path analogue of dbsqlfuzz's WAL fuzzing and a
 *	classic high-yield surface (SQLite, TigerBeetle both fuzz replay).
 *
 *	Strategy: build a REAL, minimal txn environment once per input (so
 *	the env region + log header are structurally valid), then OVERWRITE
 *	the first log file (log.0000000001) with the fuzz bytes, then open
 *	the env with DB_RECOVER so the replay parses our mutated log.
 *
 *	Goal: a crash/ASan-fault while replaying a malformed record.  A
 *	clean error return (corrupt-log detected) is the healthy outcome.
 *
 *	Isolation: fresh scratch dir + fresh env per input, removed before
 *	returning.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#include "fuzz_util.h"
#include "fuzz_driver.h"

#define LOGNAME	"log.0000000001"
#define DBNAME	"rec.db"

static void
quiet_errcall(const DB_ENV *env, const char *pfx, const char *msg)
{
	(void)env; (void)pfx; (void)msg;
}

/* Populate a valid txn env with a little committed work, then close it,
 * so a real log file exists to be mutated.  Returns 0 on success. */
static int
seed_env(const char *dir)
{
	DB_ENV *env = NULL;
	DB *db = NULL;
	DBT key, val;
	int ret;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	env->set_errcall(env, quiet_errcall);
	if ((ret = env->open(env, dir, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_PRIVATE, 0600))
	    != 0) {
		(void)env->close(env, 0);
		return (ret);
	}
	if ((ret = db_create(&db, env, 0)) == 0 &&
	    db->open(db, NULL, DBNAME, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600) == 0) {
		memset(&key, 0, sizeof(key));
		memset(&val, 0, sizeof(val));
		key.data = (void *)"k"; key.size = 1;
		val.data = (void *)"v"; val.size = 1;
		(void)db->put(db, NULL, &key, &val, 0);
		(void)db->close(db, 0);
	} else if (db != NULL)
		(void)db->close(db, 0);
	(void)env->close(env, 0);
	return (0);
}

int
LLVMFuzzerTestOneInput(const unsigned char *data, unsigned long size)
{
	char dir[256];
	DB_ENV *env = NULL;

	if (size > (1u << 20))
		size = (1u << 20);

	if (fuzz_scratch_make(dir, sizeof(dir)) != 0)
		return (0);

	/*
	 * PRIVATE env avoids leaving a shared region behind; recovery on a
	 * PRIVATE env still exercises __db_apprec / the log cursor.  If we
	 * cannot even seed a valid env (e.g. no space), just bail cleanly.
	 */
	if (seed_env(dir) != 0) {
		fuzz_scratch_rm(dir);
		return (0);
	}

	/* Overwrite the first log file with the fuzz bytes. */
	if (fuzz_write_file(dir, LOGNAME, data, size) != 0) {
		fuzz_scratch_rm(dir);
		return (0);
	}

	/* Now run recovery over the mutated log. */
	if (db_env_create(&env, 0) == 0) {
		env->set_errcall(env, quiet_errcall);
		(void)env->open(env, dir, DB_CREATE | DB_INIT_LOCK |
		    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER |
		    DB_PRIVATE, 0600);
		(void)env->close(env, 0);
	}

	fuzz_scratch_rm(dir);
	return (0);
}

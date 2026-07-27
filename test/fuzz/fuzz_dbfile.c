/*-
 * test/fuzz/fuzz_dbfile.c --
 *	Feed fuzz bytes as a Berkeley DB .db file and run the page parser.
 *
 *	The bytes are written verbatim to a temp .db file, then that file is
 *	opened via DB->verify (the __db_verify page-walker) inside a private,
 *	transient DB_ENV, and separately opened read-only and cursor-scanned.
 *	Both paths parse untrusted on-disk page structures, which is the
 *	highest-bug-yield surface for a malformed-file fuzzer (SQLite's
 *	dbsqlfuzz targets the analogous SQLite pager).
 *
 *	Goal: an ASan/UBSan fault or crash on a malformed page.  We do NOT
 *	assert on the return code -- a clean DB_VERIFY_BAD / error return is
 *	the expected, healthy outcome.  A crash is the bug signal.
 *
 *	Isolation: fresh scratch dir + fresh env per input; everything is
 *	removed before returning, so no state leaks between inputs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#include "fuzz_util.h"
#include "fuzz_driver.h"

#define DBNAME	"fuzz.db"

/* Silence libdb's error stream; we only care about crashes, not messages. */
static void
quiet_errcall(const DB_ENV *env, const char *pfx, const char *msg)
{
	(void)env; (void)pfx; (void)msg;
}

int
LLVMFuzzerTestOneInput(const unsigned char *data, unsigned long size)
{
	char dir[256], dbpath[512];
	DB_ENV *env = NULL;
	DB *db = NULL;
	int ret;

	/* Cap input: a real page file is small; huge inputs just slow us. */
	if (size > (1u << 20))
		size = (1u << 20);

	if (fuzz_scratch_make(dir, sizeof(dir)) != 0)
		return (0);
	if (fuzz_write_file(dir, DBNAME, data, size) != 0) {
		fuzz_scratch_rm(dir);
		return (0);
	}
	(void)snprintf(dbpath, sizeof(dbpath), "%s/%s", dir, DBNAME);

	/*
	 * Path 1: DB->verify.  A verify handle must not be part of an env
	 * that has the DB open, so use a standalone private env (or none).
	 * verify() consumes/closes the handle itself, so we create a fresh
	 * one for it.
	 */
	if (db_env_create(&env, 0) == 0) {
		env->set_errcall(env, quiet_errcall);
		if (env->open(env, dir,
		    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0600) != 0) {
			(void)env->close(env, 0);
			env = NULL;
		}
	} else
		env = NULL;

	if (env != NULL && db_create(&db, env, 0) == 0) {
		/* verify() closes db on both success and failure. */
		(void)db->verify(db, DBNAME, NULL, NULL, 0);
		db = NULL;
	}

	/*
	 * Path 2: open read-only and walk every record with a cursor.  This
	 * drives the mpool page-load + btree/hash traversal on the same
	 * untrusted bytes (verify and the normal read path parse pages
	 * differently, so both are worth exercising).
	 */
	if (env != NULL && db_create(&db, env, 0) == 0) {
		if (db->open(db, NULL, DBNAME, NULL, DB_UNKNOWN,
		    DB_RDONLY, 0600) == 0) {
			DBC *dbc = NULL;
			if (db->cursor(db, NULL, &dbc, 0) == 0) {
				DBT key, val;
				/* USERMEM so libdb never malloc's return
				 * buffers the harness would have to free --
				 * keeps the harness itself leak-clean so
				 * LSan only flags genuine engine leaks. */
				static unsigned char kb[1 << 20];
				static unsigned char vb[1 << 20];
				int n = 0;
				memset(&key, 0, sizeof(key));
				memset(&val, 0, sizeof(val));
				key.data = kb; key.ulen = sizeof(kb);
				key.flags = DB_DBT_USERMEM;
				val.data = vb; val.ulen = sizeof(vb);
				val.flags = DB_DBT_USERMEM;
				/* Bound the walk so a cyclic/huge file can't
				 * spin forever. */
				while (n++ < 100000 &&
				    (ret = dbc->get(dbc, &key, &val,
				    DB_NEXT)) == 0)
					;
				(void)dbc->close(dbc);
			}
		}
		(void)db->close(db, 0);
		db = NULL;
	}

	if (env != NULL)
		(void)env->close(env, 0);
	fuzz_scratch_rm(dir);
	return (0);
}

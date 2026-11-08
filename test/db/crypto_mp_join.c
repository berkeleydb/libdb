/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * crypto_mp_join.c -- RFC 0019 multi-process join with the verifier.
 *
 * The creator makes an encrypted env + db and writes a record.  N child
 * processes then JOIN the running env concurrently, each recomputing the
 * verifier and comparing it in constant time; every joiner with the right key
 * reads the record back, and a joiner with the wrong key is rejected (EPERM).
 * Exercised for both the password provider and the key-file provider.
 *
 *   crypto_mp_join passwd  <nproc>
 *   crypto_mp_join keyfile <nproc> <keyfile>
 */
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "db.h"

#define	HOME	"TESTDIR_crypto_mp"
#define	PW	"multi-process-join-password-0019"

static int mode_keyfile;
static const char *keyfile;

static int
set_env_key(DB_ENV *dbenv, const char *pw_or_key)
{
	if (mode_keyfile)
		return (dbenv->set_encrypt_keyfile(dbenv, pw_or_key,
		    DB_ENCRYPT_AES));
	return (dbenv->set_encrypt(dbenv, pw_or_key, DB_ENCRYPT_AES));
}

static int
create_env(const char *keyarg)
{
	DB_ENV *dbenv;
	DB *db;
	DBT key, data;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) return (ret);
	if ((ret = set_env_key(dbenv, keyarg)) != 0) {
		(void)dbenv->close(dbenv, 0); return (ret);
	}
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN, 0600)) != 0) {
		(void)dbenv->close(dbenv, 0); return (ret);
	}
	if ((ret = db_create(&db, dbenv, 0)) != 0) goto done;
	if ((ret = db->set_flags(db, DB_ENCRYPT)) != 0) goto done;
	if ((ret = db->open(db, NULL, "mp.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0) goto done;
	memset(&key, 0, sizeof(key)); memset(&data, 0, sizeof(data));
	key.data = "mpk"; key.size = 3;
	data.data = "mp-value"; data.size = 8;
	ret = db->put(db, NULL, &key, &data, DB_AUTO_COMMIT);
	(void)db->close(db, 0);
done:
	(void)dbenv->close(dbenv, 0);
	return (ret);
}

/* Join and read.  Returns 0 if the record matches, else the error. */
static int
join_read(const char *keyarg)
{
	DB_ENV *dbenv;
	DB *db;
	DBT key, data;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) return (ret);
	if ((ret = set_env_key(dbenv, keyarg)) != 0) {
		(void)dbenv->close(dbenv, 0); return (ret);
	}
	if ((ret = dbenv->open(dbenv, HOME, DB_INIT_MPOOL | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_TXN, 0600)) != 0) {
		(void)dbenv->close(dbenv, 0); return (ret);
	}
	if ((ret = db_create(&db, dbenv, 0)) != 0) goto done;
	if ((ret = db->set_flags(db, DB_ENCRYPT)) != 0) goto done;
	if ((ret = db->open(db, NULL, "mp.db", NULL, DB_BTREE,
	    DB_AUTO_COMMIT, 0600)) != 0) goto done;
	memset(&key, 0, sizeof(key)); memset(&data, 0, sizeof(data));
	key.data = "mpk"; key.size = 3;
	if ((ret = db->get(db, NULL, &key, &data, 0)) == 0 &&
	    (data.size != 8 || memcmp(data.data, "mp-value", 8) != 0))
		ret = EINVAL;
	(void)db->close(db, 0);
done:
	(void)dbenv->close(dbenv, 0);
	return (ret);
}

int
main(int argc, char **argv)
{
	const char *keyarg;
	pid_t pids[64];
	int i, nproc, st, ret, fail = 0;

	if (argc < 3) {
		fprintf(stderr, "usage: %s passwd|keyfile <nproc> [keyfile]\n",
		    argv[0]);
		return (2);
	}
	mode_keyfile = (strcmp(argv[1], "keyfile") == 0);
	nproc = atoi(argv[2]);
	if (nproc < 1 || nproc > 64) nproc = 8;
	keyarg = mode_keyfile ? argv[3] : PW;
	if (mode_keyfile && argc < 4) { fprintf(stderr, "need keyfile\n"); return (2); }

	(void)system("rm -rf " HOME "; mkdir " HOME);

	if ((ret = create_env(keyarg)) != 0) {
		printf("VERDICT crypto_mp_join FAIL create ret=%d\n", ret);
		(void)system("rm -rf " HOME);
		return (1);
	}

	/* Fork N joiners with the correct key; all must succeed. */
	for (i = 0; i < nproc; i++) {
		if ((pids[i] = fork()) == 0)
			_exit(join_read(keyarg) == 0 ? 0 : 1);
	}
	for (i = 0; i < nproc; i++) {
		(void)waitpid(pids[i], &st, 0);
		if (!WIFEXITED(st) || WEXITSTATUS(st) != 0) {
			printf("  FAIL: joiner %d did not read back\n", i);
			fail = 1;
		}
	}
	if (!fail)
		printf("  ok: %d concurrent joiners read back the record\n",
		    nproc);

	/* One joiner with the wrong key must be rejected. */
	if (!mode_keyfile) {
		ret = join_read("wrong-password-xxxxxxxxxxxxxxxxx");
		if (ret == EPERM)
			printf("  ok: wrong-key joiner rejected (EPERM)\n");
		else {
			printf("  FAIL: wrong-key joiner ret=%d (want EPERM)\n",
			    ret);
			fail = 1;
		}
	}

	(void)system("rm -rf " HOME);
	printf("VERDICT crypto_mp_join %s\n", fail ? "FAIL" : "PASS");
	return (fail ? 1 : 0);
}

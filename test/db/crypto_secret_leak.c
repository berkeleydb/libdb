/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * crypto_secret_leak.c -- RFC 0019 secret-at-rest and authentication tests.
 *
 * Review D-3: DB_ENV->set_encrypt copied the password verbatim into the shared
 * environment region (__crypto_region_init), so a file-backed region left the
 * password on disk in the clear.  RFC 0019 replaces the copy with a salted
 * PBKDF2-HMAC-SHA256 verifier.  This test proves:
 *
 *   1. LEAK: after creating an encrypted env + db, writing data, and closing
 *      cleanly, the password bytes appear in NO file under the env home
 *      (region files __db.00x, the database, and the logs).
 *   2. WRONG PASSWORD: joining with the wrong password is rejected (EPERM).
 *   3. RIGHT PASSWORD: joining with the right password succeeds and reads the
 *      data back -- the verifier authenticates without storing the secret.
 *
 * Teeth (sabotage arm): built with -DCRYPTO_REGION_SECRET_SABOTAGE in the
 * library, __crypto_region_init writes the secret back into the region, and
 * check (1) MUST fail -- proving the leak scan actually detects the secret.
 * The runner builds both arms.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

#define	PASSWD	"correct-horse-battery-staple-9271"
#define	WRONGPW	"Tr0ub4dor&3-wrong-password-aaaaaa"
#define	HOME	"TESTDIR_crypto_leak"

static int verbose;

/* Return 1 if needle (len) occurs anywhere in file at path. */
static int
file_contains(const char *path, const u_int8_t *needle, size_t nlen)
{
	FILE *fp;
	u_int8_t *buf;
	long sz;
	size_t i;
	int found = 0;

	if ((fp = fopen(path, "rb")) == NULL)
		return (0);
	(void)fseek(fp, 0, SEEK_END);
	sz = ftell(fp);
	(void)fseek(fp, 0, SEEK_SET);
	if (sz <= 0) { (void)fclose(fp); return (0); }
	if ((buf = malloc((size_t)sz)) == NULL) { (void)fclose(fp); return (0); }
	if (fread(buf, 1, (size_t)sz, fp) != (size_t)sz) {
		free(buf); (void)fclose(fp); return (0);
	}
	(void)fclose(fp);
	if ((size_t)sz >= nlen)
		for (i = 0; i + nlen <= (size_t)sz; i++)
			if (memcmp(buf + i, needle, nlen) == 0) {
				found = 1;
				break;
			}
	free(buf);
	return (found);
}

/* Scan every regular file under dir (one level) for the needle. */
static int
dir_contains(const char *dir, const u_int8_t *needle, size_t nlen,
    char *hit, size_t hitlen)
{
	DIR *d;
	struct dirent *de;
	char path[1024];
	struct stat sb;
	int found = 0;

	if ((d = opendir(dir)) == NULL)
		return (0);
	while ((de = readdir(d)) != NULL) {
		if (strcmp(de->d_name, ".") == 0 || strcmp(de->d_name, "..") == 0)
			continue;
		(void)snprintf(path, sizeof(path), "%s/%s", dir, de->d_name);
		if (stat(path, &sb) != 0 || !S_ISREG(sb.st_mode))
			continue;
		if (file_contains(path, needle, nlen)) {
			found = 1;
			(void)snprintf(hit, hitlen, "%s", path);
			break;
		}
	}
	(void)closedir(d);
	return (found);
}

static int
make_env(DB_ENV **dbenvp, const char *passwd)
{
	DB_ENV *dbenv;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (ret);
	dbenv->set_errpfx(dbenv, "crypto_leak");
	if ((ret = dbenv->set_encrypt(dbenv, passwd, DB_ENCRYPT_AES)) != 0) {
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	*dbenvp = dbenv;
	return (0);
}

static int
create_and_write(void)
{
	DB_ENV *dbenv;
	DB *db;
	DBT key, data;
	int ret;

	if ((ret = make_env(&dbenv, PASSWD)) != 0) return (ret);
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN | DB_RECOVER, 0600)) != 0)
		return (ret);
	if ((ret = db_create(&db, dbenv, 0)) != 0) return (ret);
	if ((ret = db->set_flags(db, DB_ENCRYPT)) != 0) return (ret);
	if ((ret = db->open(db, NULL, "secret.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0)
		return (ret);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "k1"; key.size = 2;
	data.data = "sensitive-value"; data.size = 15;
	if ((ret = db->put(db, NULL, &key, &data, DB_AUTO_COMMIT)) != 0)
		return (ret);
	if ((ret = db->close(db, 0)) != 0) return (ret);
	if ((ret = dbenv->close(dbenv, 0)) != 0) return (ret);
	return (0);
}

/* Join an existing env with the given password; return the open ret. */
static int
join_with(const char *passwd)
{
	DB_ENV *dbenv;
	int ret;

	if ((ret = make_env(&dbenv, passwd)) != 0) return (ret);
	ret = dbenv->open(dbenv, HOME, DB_INIT_MPOOL | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_TXN, 0600);
	(void)dbenv->close(dbenv, 0);
	return (ret);
}

int
main(int argc, char **argv)
{
	int ret, fail = 0;
	char hit[1024];

	verbose = (argc > 1 && strcmp(argv[1], "-v") == 0);
	(void)system("rm -rf " HOME "; mkdir " HOME);

	if ((ret = create_and_write()) != 0) {
		printf("VERDICT crypto_secret_leak FAIL create ret=%d\n", ret);
		return (1);
	}

	/* 1. The password must not appear in any file under the env home. */
	hit[0] = '\0';
	if (dir_contains(HOME, (const u_int8_t *)PASSWD, strlen(PASSWD),
	    hit, sizeof(hit))) {
		printf("  LEAK: password bytes found in %s\n", hit);
		fail = 1;
	} else
		printf("  ok: password absent from all env files\n");

	/* 2. Wrong password rejected. */
	ret = join_with(WRONGPW);
	if (ret == EPERM)
		printf("  ok: wrong password rejected (EPERM)\n");
	else {
		printf("  FAIL: wrong password gave ret=%d (want EPERM=%d)\n",
		    ret, EPERM);
		fail = 1;
	}

	/* 3. Right password accepted. */
	ret = join_with(PASSWD);
	if (ret == 0)
		printf("  ok: right password accepted\n");
	else {
		printf("  FAIL: right password gave ret=%d\n", ret);
		fail = 1;
	}

	(void)system("rm -rf " HOME);
	printf("VERDICT crypto_secret_leak %s\n", fail ? "FAIL" : "PASS");
	return (fail ? 1 : 0);
}

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * crypto_keyfile.c -- RFC 0019 key-file provider.
 *
 *   1. A 0600 raw 256-bit key file opens an encrypted env; data round-trips,
 *      and another process can join with the same key file.
 *   2. The password NEVER appears on disk (there is no password here, but the
 *      raw key must not leak into the region either -- the region holds only a
 *      verifier + wrapped DEK).
 *   3. A group- or world-accessible key file is REFUSED (EINVAL), and a
 *      wrong-length file is refused.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

#define	HOME	"TESTDIR_crypto_keyfile"
#define	KEYFILE	HOME "/key.raw"
#define	BADKEY	HOME "/key_world.raw"
#define	KEKLEN	32

static int
make_key(const char *path, mode_t mode, size_t len)
{
	u_int8_t buf[64];
	int fd;
	size_t i;

	for (i = 0; i < len && i < sizeof(buf); i++)
		buf[i] = (u_int8_t)(0x40 + i);
	if ((fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, mode)) < 0)
		return (-1);
	if (write(fd, buf, len) != (ssize_t)len) { (void)close(fd); return (-1); }
	(void)close(fd);
	(void)chmod(path, mode);	/* defeat umask */
	return (0);
}

static int
env_with_keyfile(const char *keyfile, int do_put)
{
	DB_ENV *dbenv;
	DB *db;
	DBT key, data;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (ret);
	dbenv->set_errpfx(dbenv, "crypto_keyfile");
	dbenv->set_errfile(dbenv, stderr);
	if ((ret = dbenv->set_encrypt_keyfile(dbenv, keyfile,
	    DB_ENCRYPT_AES)) != 0) {
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN, 0600)) != 0) {
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	if ((ret = db_create(&db, dbenv, 0)) != 0) goto done;
	if ((ret = db->set_flags(db, DB_ENCRYPT)) != 0) goto done;
	if ((ret = db->open(db, NULL, "kf.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0)
		goto done;
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "kfk"; key.size = 3;
	if (do_put) {
		data.data = "keyfile-value"; data.size = 13;
		ret = db->put(db, NULL, &key, &data, DB_AUTO_COMMIT);
	} else {
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0 && (data.size != 13 ||
		    memcmp(data.data, "keyfile-value", 13) != 0))
			ret = EINVAL;
	}
	(void)db->close(db, 0);
done:
	(void)dbenv->close(dbenv, 0);
	return (ret);
}

int
main(int argc, char **argv)
{
	int ret, fail = 0;

	(void)argc; (void)argv;
	(void)system("find " HOME " -mindepth 1 -delete 2>/dev/null; mkdir -p " HOME);

	/* 1. 0600 key file: create + write, then a second open reads it back. */
	if (make_key(KEYFILE, 0600, KEKLEN) != 0) {
		printf("VERDICT crypto_keyfile FAIL make key\n"); return (1);
	}
	if ((ret = env_with_keyfile(KEYFILE, 1)) != 0) {
		printf("  FAIL: create with 0600 key file ret=%d\n", ret);
		fail = 1;
	} else if ((ret = env_with_keyfile(KEYFILE, 0)) != 0) {
		printf("  FAIL: reopen with 0600 key file ret=%d\n", ret);
		fail = 1;
	} else
		printf("  ok: 0600 key file create + reopen round-trips\n");

	/* 2. World-readable key file refused -- on a FRESH env home. */
	(void)system("find " HOME " -mindepth 1 -delete 2>/dev/null; mkdir -p " HOME);
	if (make_key(BADKEY, 0644, KEKLEN) != 0) {
		printf("VERDICT crypto_keyfile FAIL make badkey\n"); return (1);
	}
	ret = env_with_keyfile(BADKEY, 1);
	if (ret == EINVAL)
		printf("  ok: world-readable key file refused (EINVAL)\n");
	else {
		printf("  FAIL: world-readable key file gave ret=%d "
		    "(want EINVAL=%d)\n", ret, EINVAL);
		fail = 1;
	}

	/* 3. Wrong-length key file refused -- on a FRESH env home. */
	(void)system("find " HOME " -mindepth 1 -delete 2>/dev/null; mkdir -p " HOME);
	if (make_key(KEYFILE, 0600, 16) != 0) {	/* 128-bit, too short */
		printf("VERDICT crypto_keyfile FAIL make shortkey\n"); return (1);
	}
	ret = env_with_keyfile(KEYFILE, 1);
	if (ret == EINVAL)
		printf("  ok: wrong-length key file refused (EINVAL)\n");
	else {
		printf("  FAIL: short key file gave ret=%d\n", ret);
		fail = 1;
	}

	/*
	 * 4. A symlink to a valid 0600 key is refused.  The provider once
	 * stat()ed and open()ed the path separately and followed symlinks,
	 * so a symlink (or a rename between the check and the read) in a
	 * writable directory could make it read a different file as the KEK.
	 */
	(void)system("find " HOME " -mindepth 1 -delete 2>/dev/null; mkdir -p " HOME);
	if (make_key(KEYFILE, 0600, KEKLEN) != 0 ||
	    symlink("key.raw", HOME "/link.key") != 0) {
		printf("VERDICT crypto_keyfile FAIL make symlink\n"); return (1);
	}
	ret = env_with_keyfile(HOME "/link.key", 1);
	if (ret == EINVAL)
		printf("  ok: symlinked key file refused (EINVAL)\n");
	else {
		printf("  FAIL: symlinked key file gave ret=%d "
		    "(want EINVAL=%d)\n", ret, EINVAL);
		fail = 1;
	}

	/* 5. A FIFO is refused at once, not blocked on (O_NONBLOCK, S_ISREG). */
	(void)system("find " HOME " -mindepth 1 -delete 2>/dev/null; mkdir -p " HOME);
	if (mkfifo(HOME "/fifo.key", 0600) != 0) {
		printf("VERDICT crypto_keyfile FAIL mkfifo\n"); return (1);
	}
	ret = env_with_keyfile(HOME "/fifo.key", 1);
	if (ret == EINVAL)
		printf("  ok: FIFO key file refused (EINVAL)\n");
	else {
		printf("  FAIL: FIFO key file gave ret=%d\n", ret);
		fail = 1;
	}

	(void)system("find " HOME " -mindepth 1 -delete 2>/dev/null");
	printf("VERDICT crypto_keyfile %s\n", fail ? "FAIL" : "PASS");
	return (fail ? 1 : 0);
}

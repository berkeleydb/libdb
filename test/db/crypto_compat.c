/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * crypto_compat.c -- RFC 0019 backward-compatibility.
 *
 * The region verifier change must not alter the page/log encryption key
 * derivation, so a database created by a PRE-change master must still open
 * with the same password.  The page key still derives from the password via
 * __aes_derivekeys (legacy mode, dek_len == 0), unchanged.
 *
 * Usage:
 *   crypto_compat create <dbfile> <passwd>   -- create+write (run with BASE lib)
 *   crypto_compat read   <dbfile> <passwd>   -- open+verify  (run with NEW lib)
 *
 * The runner builds this against both libraries and checks that NEW reads what
 * BASE wrote.  A standalone (no-env) encrypted database is used so the fixture
 * is a single file with no region or log dependence.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

#define	NREC	64

static int
do_create(const char *dbfile, const char *passwd)
{
	DB *db;
	DBT key, data;
	char kb[32], vb[32];
	int i, ret;

	if ((ret = db_create(&db, NULL, 0)) != 0)
		return (ret);
	if ((ret = db->set_encrypt(db, passwd, DB_ENCRYPT_AES)) != 0)
		return (ret);
	if ((ret = db->set_flags(db, DB_ENCRYPT)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, dbfile, NULL, DB_BTREE,
	    DB_CREATE, 0600)) != 0)
		return (ret);
	for (i = 0; i < NREC; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kb, sizeof(kb), "key%08d", i);
		(void)snprintf(vb, sizeof(vb), "val-%08d-secret", i);
		key.data = kb; key.size = (u_int32_t)strlen(kb);
		data.data = vb; data.size = (u_int32_t)strlen(vb);
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			return (ret);
	}
	return (db->close(db, 0));
}

static int
do_read(const char *dbfile, const char *passwd)
{
	DB *db;
	DBT key, data;
	char kb[32], want[32];
	int i, ret, bad = 0;

	if ((ret = db_create(&db, NULL, 0)) != 0)
		return (ret);
	if ((ret = db->set_encrypt(db, passwd, DB_ENCRYPT_AES)) != 0)
		return (ret);
	if ((ret = db->set_flags(db, DB_ENCRYPT)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, dbfile, NULL, DB_BTREE,
	    DB_RDONLY, 0600)) != 0)
		return (ret);
	for (i = 0; i < NREC; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		(void)snprintf(kb, sizeof(kb), "key%08d", i);
		(void)snprintf(want, sizeof(want), "val-%08d-secret", i);
		key.data = kb; key.size = (u_int32_t)strlen(kb);
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			printf("  get key%08d failed ret=%d\n", i, ret);
			bad = 1;
			break;
		}
		if (data.size != strlen(want) ||
		    memcmp(data.data, want, data.size) != 0) {
			printf("  value mismatch at %d\n", i);
			bad = 1;
			break;
		}
	}
	(void)db->close(db, 0);
	return (bad ? EINVAL : 0);
}

int
main(int argc, char **argv)
{
	int ret;

	if (argc != 4) {
		fprintf(stderr, "usage: %s create|read <dbfile> <passwd>\n",
		    argv[0]);
		return (2);
	}
	if (strcmp(argv[1], "create") == 0)
		ret = do_create(argv[2], argv[3]);
	else if (strcmp(argv[1], "read") == 0)
		ret = do_read(argv[2], argv[3]);
	else {
		fprintf(stderr, "unknown mode %s\n", argv[1]);
		return (2);
	}
	if (ret != 0) {
		printf("VERDICT crypto_compat FAIL %s ret=%d\n", argv[1], ret);
		return (1);
	}
	printf("VERDICT crypto_compat OK %s\n", argv[1]);
	return (0);
}

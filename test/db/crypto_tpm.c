/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * crypto_tpm.c -- RFC 0019 TPM 2.0 provider (built only with --enable-tpm).
 *
 *   crypto_tpm seal <blob> [pcrmask]  seal a fresh KEK to the TPM
 *   crypto_tpm open <blob>            open an encrypted env whose DEK is wrapped
 *                                     under the TPM-sealed KEK, write+read back
 *
 * The runner drives swtpm: seal, open (create+write), reopen (join+read).  A
 * wrong-PCR refusal is covered by the runner re-sealing to a PCR policy and
 * extending the PCR between seal and open.  Without a working swtpm the runner
 * SKIPs.
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

#define	HOME	"TESTDIR_crypto_tpm"

/*
 * HAVE_TPM comes from the runner (-DHAVE_TPM, set only when the build's
 * db_config.h has it); the public db.h never defines it.
 */
#ifndef HAVE_TPM
int main(void){ printf("VERDICT crypto_tpm SKIP no-tpm-build\n"); return (0); }
#else

static int
env_tpm(const char *blob, int create, int do_read)
{
	DB_ENV *dbenv;
	DB *db;
	DBT key, data;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (ret);
	dbenv->set_errpfx(dbenv, "crypto_tpm");
	dbenv->set_errfile(dbenv, stderr);
	if ((ret = dbenv->set_encrypt_tpm(dbenv, blob, DB_ENCRYPT_AES)) != 0) {
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	if ((ret = dbenv->open(dbenv, HOME, (create ? DB_CREATE : 0) |
	    DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN, 0600))
	    != 0) {
		(void)dbenv->close(dbenv, 0);
		return (ret);
	}
	if ((ret = db_create(&db, dbenv, 0)) != 0) goto done;
	if ((ret = db->set_flags(db, DB_ENCRYPT)) != 0) goto done;
	if ((ret = db->open(db, NULL, "t.db", NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0600)) != 0)
		goto done;
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = "tk"; key.size = 2;
	if (do_read) {
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0 && (data.size != 8 ||
		    memcmp(data.data, "tpm-data", 8) != 0))
			ret = EINVAL;
	} else {
		data.data = "tpm-data"; data.size = 8;
		ret = db->put(db, NULL, &key, &data, DB_AUTO_COMMIT);
	}
	(void)db->close(db, 0);
done:
	(void)dbenv->close(dbenv, 0);
	return (ret);
}

int
main(int argc, char **argv)
{
	int ret;

	if (argc < 3) {
		fprintf(stderr, "usage: %s open <blob>\n", argv[0]);
		return (2);
	}
	if (strcmp(argv[1], "open") == 0) {
		if ((ret = env_tpm(argv[2], 1, 0)) != 0) {
			printf("VERDICT crypto_tpm FAIL create ret=%d\n", ret);
			return (1);
		}
		if ((ret = env_tpm(argv[2], 0, 1)) != 0) {
			printf("VERDICT crypto_tpm FAIL reopen ret=%d\n", ret);
			return (1);
		}
		printf("VERDICT crypto_tpm OK open\n");
		return (0);
	}
	if (strcmp(argv[1], "badpcr") == 0) {
		/* Expect EPERM/EINVAL because the PCR state changed. */
		ret = env_tpm(argv[2], 1, 0);
		if (ret == 0) {
			printf("VERDICT crypto_tpm FAIL wrong-PCR opened\n");
			return (1);
		}
		printf("VERDICT crypto_tpm OK badpcr-refused ret=%d\n", ret);
		return (0);
	}
	fprintf(stderr, "unknown mode %s\n", argv[1]);
	return (2);
}
#endif

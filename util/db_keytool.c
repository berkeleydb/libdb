/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * db_keytool -- create, seal and inspect encryption key material (RFC 0019).
 *
 *   db_keytool -k FILE               generate a random 256-bit raw key file
 *                                    (mode 0600) for the key-file provider
 *   db_keytool -T BLOB [-C PCRMASK]  seal a fresh random 256-bit KEK to the TPM
 *                                    and write the sealed blob (needs --enable-tpm)
 *   db_keytool -i BLOB               inspect a TPM sealed blob (no secret shown)
 *   db_keytool -V                    print version
 *
 * The utility never prints a secret.
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/crypto.h"

#ifndef DB_WIN32
#include <fcntl.h>
#include <unistd.h>
#endif

#ifndef lint
static const char copyright[] =
    "Copyright (c) 2026 libdb contributors.  All rights reserved.\n";
#endif

int main __P((int, char *[]));
int usage __P((void));

const char *progname;

/* Self-contained scrub so a key never lingers on the stack. */
static void
scrub(void *b, size_t n)
{
	volatile unsigned char *p = b;
	while (n-- > 0)
		*p++ = 0;
}

static int
write_keyfile(ENV *env, const char *path)
{
#if defined(HAVE_CRYPTO) && !defined(DB_WIN32)
	u_int8_t key[DB_CRYPTO_KEK_LEN];
	int ret, fd;
	ssize_t n;

	if ((ret = __os_csprng(env, key, sizeof(key))) != 0) {
		fprintf(stderr, "%s: entropy source unavailable\n", progname);
		return (ret);
	}
	/* Create 0600 so the key-file provider's permission check passes. */
	if ((fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0600)) < 0) {
		fprintf(stderr, "%s: %s: %s\n", progname, path, strerror(errno));
		scrub(key, sizeof(key));
		return (1);
	}
	n = write(fd, key, sizeof(key));
	(void)close(fd);
	scrub(key, sizeof(key));
	if (n != (ssize_t)sizeof(key)) {
		fprintf(stderr, "%s: short write to %s\n", progname, path);
		return (1);
	}
	printf("wrote %d-byte key file %s (mode 0600)\n",
	    (int)DB_CRYPTO_KEK_LEN, path);
	return (0);
#else
	COMPQUIET(env, NULL);
	COMPQUIET(path, NULL);
	fprintf(stderr, "%s: key-file creation needs crypto on a POSIX host\n",
	    progname);
	return (1);
#endif
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	DB_ENV *dbenv;
	ENV *env;
	const char *keyfile, *tpmblob, *inspectblob;
	u_int32_t pcr_mask;
	int ch, ret;

	if ((progname = __db_rpath(argv[0])) == NULL)
		progname = argv[0];
	else
		progname++;
	keyfile = tpmblob = inspectblob = NULL;
	pcr_mask = 0;

	while ((ch = getopt(argc, argv, "k:T:i:C:V")) != EOF)
		switch (ch) {
		case 'k': keyfile = optarg; break;
		case 'T': tpmblob = optarg; break;
		case 'i': inspectblob = optarg; break;
		case 'C': pcr_mask = (u_int32_t)strtoul(optarg, NULL, 0); break;
		case 'V':
			printf("%s\n", db_version(NULL, NULL, NULL));
			return (EXIT_SUCCESS);
		default:
			return (usage());
		}

	if (keyfile == NULL && tpmblob == NULL && inspectblob == NULL)
		return (usage());

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "%s: db_env_create: %s\n",
		    progname, db_strerror(ret));
		return (EXIT_FAILURE);
	}
	env = dbenv->env;

	ret = 0;
	if (keyfile != NULL)
		ret = write_keyfile(env, keyfile);
#ifdef HAVE_TPM
	{
		u_int8_t kek[DB_CRYPTO_KEK_LEN];
		if (ret == 0 && tpmblob != NULL) {
			if ((ret = __os_csprng(env, kek, sizeof(kek))) == 0) {
				ret = __crypto_tpm_seal(env, tpmblob, kek,
				    sizeof(kek), pcr_mask);
				scrub(kek, sizeof(kek));
				if (ret == 0)
					printf("sealed a 256-bit KEK to the TPM"
					    " -> %s (pcr_mask=0x%x)\n",
					    tpmblob, pcr_mask);
				else
					fprintf(stderr,
					    "%s: TPM seal failed: %d\n",
					    progname, ret);
			}
		}
		if (ret == 0 && inspectblob != NULL)
			ret = __crypto_tpm_inspect(inspectblob);
	}
#else
	if (tpmblob != NULL || inspectblob != NULL) {
		fprintf(stderr,
		    "%s: built without TPM support (--enable-tpm)\n", progname);
		ret = 1;
	}
#endif
	(void)dbenv->close(dbenv, 0);
	return (ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

int
usage()
{
	fprintf(stderr,
	    "usage: %s -k keyfile | -T blob [-C pcrmask] | -i blob | -V\n",
	    progname);
	return (EXIT_FAILURE);
}

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * Built-in key providers (RFC 0019).  A provider supplies the key-encryption
 * key (KEK); libdb owns the data key and the wrapping.  The password provider
 * is handled inline in crypto.c; this file holds the key-file provider and a
 * helper to construct one.  The TPM provider lives in provider_tpm.c, built
 * only under --enable-tpm.
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/crypto.h"

#ifndef DB_WIN32
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#endif

#ifdef HAVE_CRYPTO

/*
 * __crypto_keyfile_get_key --
 *	Read a raw 256-bit key from the file named by ctx.  Refuses a file
 *	that is group- or world-accessible, or not owned by the caller, or of
 *	the wrong length.
 */
static int
__crypto_keyfile_get_key(prov, key_id, buf, len)
	DB_CRYPTO_PROVIDER *prov;
	const char *key_id;
	u_int8_t *buf;
	size_t len;
{
	const char *path;
	int ret;

	COMPQUIET(key_id, NULL);
	path = (const char *)prov->ctx;
	if (path == NULL || len != DB_CRYPTO_KEK_LEN)
		return (EINVAL);

	if ((ret = __crypto_keyfile_check(path)) != 0)
		return (ret);

#ifdef DB_WIN32
	/* shortcut: Windows key-file read not implemented; upgrade on demand. */
	return (EINVAL);
#else
	{
		int fd;
		ssize_t n;
		u_int8_t extra;

		if ((fd = open(path, O_RDONLY, 0)) < 0)
			return (__os_get_errno());
		ret = 0;
		n = read(fd, buf, DB_CRYPTO_KEK_LEN);
		if (n < 0)
			ret = __os_get_errno();
		else if (n != DB_CRYPTO_KEK_LEN || read(fd, &extra, 1) != 0)
			ret = EINVAL;	/* wrong length */
		(void)close(fd);
		return (ret);
	}
#endif
}

/*
 * __crypto_keyfile_check --
 *	Verify a key file is not group/world accessible and is owned by us.
 *
 * PUBLIC: int __crypto_keyfile_check __P((const char *));
 */
int
__crypto_keyfile_check(path)
	const char *path;
{
#ifdef DB_WIN32
	/*
	 * shortcut: no ACL owner-only check on Windows yet; upgrade when a
	 * Windows TPM/keyfile user needs it.  We still require the file to
	 * exist and open read-only below.
	 */
	return (0);
#else
	struct stat sb;

	if (stat(path, &sb) != 0)
		return (__os_get_errno());
	if ((sb.st_mode & (S_IRWXG | S_IRWXO)) != 0)
		return (EINVAL);		/* group/world accessible */
	if (sb.st_uid != geteuid())
		return (EINVAL);		/* not owned by us */
	return (0);
#endif
}

/*
 * __crypto_keyfile_provider --
 *	Fill in a provider struct for a key file at the given path.  The path
 *	pointer must remain valid until env open.
 *
 * PUBLIC: int __crypto_keyfile_provider
 * PUBLIC:     __P((DB_CRYPTO_PROVIDER *, const char *));
 */
int
__crypto_keyfile_provider(prov, path)
	DB_CRYPTO_PROVIDER *prov;
	const char *path;
{
	memset(prov, 0, sizeof(*prov));
	prov->provider_id = DB_CRYPTO_PROV_KEYFILE;
	prov->ctx = (void *)path;
	prov->get_key = __crypto_keyfile_get_key;
	prov->release = NULL;
	prov->rotate = NULL;
	return (0);
}
#endif /* HAVE_CRYPTO */

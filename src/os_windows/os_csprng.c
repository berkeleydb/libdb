/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 The libdb contributors.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

/*
 * __os_csprng --
 *	Fill a buffer with cryptographically strong random bytes from the
 *	operating system (Windows).
 *
 *	This is the Windows counterpart of src/os/os_csprng.c and the entropy
 *	source for security-sensitive values (currently encryption
 *	initialization vectors).  RtlGenRandom (exposed as SystemFunction036)
 *	is used rather than BCryptGenRandom: it needs no provider handle, has
 *	been available since Windows XP, and avoids linking bcrypt.lib.
 *
 *	Returns 0 on success, or a non-zero error if the OS declines; callers
 *	MUST handle failure rather than silently falling back to a weak source.
 *
 * PUBLIC: int __os_csprng __P((ENV *, void *, size_t));
 */
int
__os_csprng(env, buf, len)
	ENV *env;
	void *buf;
	size_t len;
{
	/*
	 * RtlGenRandom is declared as SystemFunction036 in ntsecapi.h, which
	 * pulls in a large amount of unrelated interface; declare it directly
	 * the way Microsoft documents for this use.
	 */
	BOOLEAN (APIENTRY *pRtlGenRandom)(PVOID, ULONG);
	HMODULE advapi;
	int ret;

	if (len == 0)
		return (0);

	ret = 0;
	if ((advapi = LoadLibraryA("advapi32.dll")) == NULL)
		goto err;

	pRtlGenRandom = (BOOLEAN (APIENTRY *)(PVOID, ULONG))
	    GetProcAddress(advapi, "SystemFunction036");
	if (pRtlGenRandom == NULL ||
	    !pRtlGenRandom(buf, (ULONG)len))
		ret = EIO;

	(void)FreeLibrary(advapi);
	if (ret == 0)
		return (0);

err:	__db_errx(env, DB_STR("0214",
	    "Unable to obtain random bytes from the operating system"));
	return (ret == 0 ? EIO : ret);
}

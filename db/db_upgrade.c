/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)db_upgrade.c	11.3 (Sleepycat) 10/20/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_swap.h"
#include "btree.h"
#include "hash.h"
#include "qam.h"

/*
 * __db_upgrade --
 *	Upgrade an existing database.
 *
 * PUBLIC: int __db_upgrade __P((DB *, const char *, u_int32_t));
 */
int
__db_upgrade(dbp, fname, flags)
	DB *dbp;
	const char *fname;
	u_int32_t flags;
{
	DB_ENV *dbenv;
	DB_FH fh;
	ssize_t nr;
	int ret, swapped, t_ret;
	char *real_name, mbuf[256];

	dbenv = dbp->dbenv;

	/* Validate arguments. */
	if ((ret = __db_fchk(dbenv, "DB->upgrade", flags, 0)) != 0)
		return (ret);

	/* Get the real backing file name. */
	if ((ret = __db_appname(dbenv,
	    DB_APP_DATA, NULL, fname, 0, NULL, &real_name)) != 0)
		return (ret);

	/* Open the file. */
	if ((ret = __os_open(real_name, 0, 0, &fh)) != 0) {
		__db_err(dbenv, "%s: %s", fname, db_strerror(ret));
		return (ret);
	}

	/*
	 * Read the metadata page.  We read 256 bytes, which is larger than
	 * any access method's metadata page and smaller than any disk sector.
	 */
	if ((ret = __os_read(&fh, mbuf, sizeof(mbuf), &nr)) != 0)
		goto err;

	swapped = 0;
retry:	switch (((DBMETA *)mbuf)->magic) {
	case DB_BTREEMAGIC:
		if ((ret =
		    __bam_upgrade(dbp, swapped, real_name, &fh, mbuf)) != 0)
			goto err;
		break;
	case DB_HASHMAGIC:
		if ((ret =
		    __ham_upgrade(dbp, swapped, real_name, &fh, mbuf)) != 0)
			goto err;
		break;
	case DB_QAMMAGIC:
		break;
	default:
		if (swapped++) {
			__db_err(dbenv, "%s: unrecognized file type", fname);
			ret = EINVAL;
			goto err;
		}

		M_32_SWAP(((DBMETA *)mbuf)->magic);
		M_32_SWAP(((DBMETA *)mbuf)->version);
		goto retry;
	}

err:	if ((t_ret = __os_closehandle(&fh)) != 0 && ret == 0)
		ret = t_ret;
	__os_freestr(real_name);

	return (ret);
}

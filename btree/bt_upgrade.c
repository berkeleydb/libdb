/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)bt_upgrade.c	11.5 (Sleepycat) 10/20/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <limits.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_swap.h"
#include "btree.h"
#include "db_am.h"

static int __bam_upgrade6 __P((DB *, int, char *, DB_FH *));

/*
 * __bam_upgrade --
 *	Upgrade Btree databases.
 *
 * PUBLIC: int __bam_upgrade __P((DB *, int, char *, DB_FH *, char *));
 */
int
__bam_upgrade(dbp, swapped, real_name, fhp, mbuf)
	DB *dbp;
	int swapped;
	char *real_name, *mbuf;
	DB_FH *fhp;
{
	DB_ENV *dbenv;
	int ret;

	dbenv = dbp->dbenv;

	/* Check the version. */
	switch (((DBMETA *)mbuf)->version) {
	case 6:
		if ((ret = __bam_upgrade6(dbp, swapped, real_name, fhp)) != 0)
			return (ret);
		/* FALLTHROUGH */
	case 7:
		break;
	default:
		__db_err(dbenv, "%s: unsupported btree version: %lu",
		    real_name, (u_long)((DBMETA *)mbuf)->version);
		return (DB_OLD_VERSION);
	}
	return (0);
}

/*
 * __bam_upgrade6 --
 *	Upgrade the database from version 6 to version 7.
 */
static int
__bam_upgrade6(dbp, swapped, real_name, fhp)
	DB *dbp;
	int swapped;
	char *real_name;
	DB_FH *fhp;
{
	DB_ENV *dbenv;
	ssize_t n;
	u_int32_t tmp;
	int ret;
	u_int8_t buf[256], *p;

	dbenv = dbp->dbenv;

	if (dbp->db_feedback != NULL)
		dbp->db_feedback(dbp, DB_UPGRADE, 0);

	/*
	 * Seek to the beginning of the file and read the metadata page.  We
	 * read 256 bytes, which is larger than any access method's metadata
	 * page.
	 */
	if ((ret = __os_seek(fhp, 0, 0, 0, 0, DB_OS_SEEK_SET)) != 0)
		return (ret);
	if ((ret = __os_read(fhp, buf, sizeof(buf), &n)) != 0)
		return (ret);

	/*
	 * Upgrade a Btree meta-data page.
	 *	Version 6:	byte range:	Version 7:	byte range:
	 *	lsn		00-07		lsn		00-07
	 *	pgno		08-11		pgno		08-11
	 *	magic		12-15		magic		12-15
	 *	version		16-19		version		16-19
	 *	pagesize	20-23		pagesize	20-23
	 *	maxkey		24-27		unused		   24
	 *					type		   25
	 *					unused		26-27
	 *	minkey		28-31		free		28-31
	 *	free		32-35		flags		32-35
	 *	flags		36-39		uid		36-55
	 *	re_len		40-43		maxkey		56-59
	 *	re_pad		44-47		minkey		60-63
	 *	uid		48-63		re_len		64-67
	 *					re_pad		68-71
	 *					root		72-75
	 */

	/*
	 * We are going to create a new uid, so we can move the stuff
	 * at the end of the structure first, overwriting the uid.
	 */

	/* 64-71 done: Move re_len and re_pad */
	memmove(buf + 64, buf + 40, 8);

	/* 56-63 done: Move maxkey and minkey */
	memmove(buf + 56, buf + 24, 8);

	/* 16-19 done: Update the version. */
	tmp = 7;
	if (swapped)
		M_32_SWAP(tmp);
	memcpy(buf + 16, &tmp, sizeof(u_int32_t));

	/*  0-23 done: Bytes 0-24 are unchanged. */
	p = buf + 24;

	/* 24-27 done: Add type. */
	*p++ = '\0';
	*p++ = P_BTREEMETA;
	*p++ = '\0';
	*p = '\0';

	/* 28-35 done: Move free and flags */
	memmove(buf + 28, buf + 32, 8);

	/* 36-55 done: Replace the unique ID. */
	if ((ret = __os_fileid(dbenv, real_name, 1, buf + 36)) != 0)
		return (ret);

	/* 72-75 done: Set the root page. */
	tmp = 1;
	if (swapped)
		M_32_SWAP(tmp);
	memcpy(buf + 72, &tmp, sizeof(u_int32_t));

					/* Write the metadata page out. */
	if ((ret = __os_seek(fhp, 0, 0, 0, 1, DB_OS_SEEK_SET)) != 0)
		return (ret);
	if ((ret = __os_write(fhp, buf, 128, &n)) != 0)
		return (ret);
	if ((ret = __os_fsync(fhp)) != 0)
		return (ret);

	if (dbp->db_feedback != NULL)
		dbp->db_feedback(dbp, DB_UPGRADE, 100);

	return (0);
}

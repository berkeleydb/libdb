/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)hash_upgrade.c	11.7 (Sleepycat) 10/20/99";
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
#include "hash.h"

static int __ham_upgrade5 __P((DB *, int, char *, DB_FH *));

/*
 * __ham_upgrade --
 *	Upgrade Hash databases.
 *
 * PUBLIC: int __ham_upgrade __P((DB *, int, char *, DB_FH *, char *));
 */
int
__ham_upgrade(dbp, swapped, real_name, fhp, mbuf)
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
	case 4:
	case 5:
		if ((ret = __ham_upgrade5(dbp, swapped, real_name, fhp)) != 0)
			return (ret);
		/* FALLTHROUGH */
	case 6:
		break;
	default:
		__db_err(dbenv, "%s: unsupported hash version: %lu",
		    real_name, (u_long)((DBMETA *)mbuf)->version);
		return (DB_OLD_VERSION);
	}
	return (0);
}

/*
 * __ham_upgrade5 --
 *      Upgrade the database from version 4/5 to version 6.
 */
static int
__ham_upgrade5(dbp, swapped, real_name, fhp)
	DB *dbp;
	int swapped;
	char *real_name;
	DB_FH *fhp;
{
	DB_ENV *dbenv;
	ssize_t n;
	u_int32_t *o_spares, *n_spares, version;
	u_int32_t fillf, maxb, nelem;
	int i, non_zero, ret;
	u_int8_t nbuf[256], *new, obuf[256];

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
	if ((ret = __os_read(fhp, obuf, sizeof(obuf), &n)) != 0)
		return (ret);

	/*
	 * Upgrade a Hash meta-data page.
	 *	Version 5:	byte range:	Version 6:	byte range:
	 *	lsn		00-07		lsn		00-07
	 *	pgno		08-11		pgno		08-11
	 *	magic		12-15		magic		12-15
	 *	version		16-19		version		16-19
	 *	pagesize	20-23		pagesize	20-23
	 *	ovfl_point	24-27		unused		   24
	 *					type		   25
	 *					unused		26-27
	 *	last_freed	28-31		free		28-31
	 *	max_bucket	32-35		flags		32-35
	 *	high_mask	36-39		uid		36-55
	 *	low_mask	40-43		max_bucket	56-59
	 *	ffactor		44-47		high_mask	60-63
	 *	nelem		48-51		low_mask	64-67
	 *	h_charkey	52-55		ffactor		68-71
	 *	flags		56-59		nelem		72-75
	 *	spares		60-187		h_charkey	76-79
	 *	uid		188-207		spares		80-207
	 *
	 */

	/*
	 * The first 32 bytes are similar.  The only change is the version
	 * and that we removed the ovfl_point and have the page type now.
	 */
	memcpy(nbuf, obuf, 32);

	/* Update the version. */
	version = 6;
	if (swapped)
		M_32_SWAP(version);
	memcpy(nbuf + 16, &version, sizeof(u_int32_t));

	/* Assign unused and type fields. */
	new = nbuf + 24;
	*new++ = '\0';
	*new++ = P_HASHMETA;
	*new++ = '\0';
	*new = '\0';

	/* Move flags */
	memcpy(nbuf + 32, obuf + 56, 4);

	/* Copy: max_bucket, high_mask, low-mask, ffactor, nelem, h_charkey */
	memcpy(nbuf + 56, obuf + 32, 24);

	/*
	 * There was a bug in 2.X versions where the nelem could go negative.
	 * In general, this is considered "bad."  If it does go negative
	 * (that is, very large and positive), we'll die trying to dump and
	 * load this database.  So, let's see if we can fix it here.
	 */
	memcpy(&nelem, nbuf + 72, sizeof(u_int32_t));
	memcpy(&fillf, nbuf + 68, sizeof(u_int32_t));
	memcpy(&maxb, nbuf + 56, sizeof(u_int32_t));
	if (swapped) {
		M_32_SWAP(nelem);
		M_32_SWAP(fillf);
		M_32_SWAP(maxb);
	}

	if ((fillf != 0 && fillf * maxb < 2 * nelem) ||
	    (fillf == 0 && nelem > 0x8000000)) {
		nelem = 0;
		memcpy(nbuf + 72, &nelem, sizeof(u_int32_t));
	}

	/*
	 * We now have to convert the spares array.  The old spares array
	 * contained the total number of extra pages allocated prior to
	 * the bucket that begins the next doubling.  The new spares array
	 * contains the page number of the first bucket in the next doubling
	 * MINUS the bucket number of that bucket.
	 */
	o_spares = (u_int32_t *)(obuf + 60);
	n_spares = (u_int32_t *)(nbuf + 80);
	non_zero = 0;
	n_spares[0] = 1;
	for (i = 1; i < NCACHED; i++) {
		if (swapped)
			M_32_SWAP(o_spares[i -1]);
		non_zero = non_zero || o_spares[i - 1] != 0;
		if (o_spares[i - 1] == 0 && non_zero)
			n_spares[i] = 0;
		else
			n_spares[i] = 1 + o_spares[i - 1];
	}

	if (swapped)
		for (i = 0; i < NCACHED; i++)
			M_32_SWAP(n_spares[i]);

					/* Replace the unique ID. */
	if ((ret = __os_fileid(dbenv, real_name, 1, nbuf + 36)) != 0)
		return (ret);

	if ((ret = __os_seek(fhp, 0, 0, 0, 1, DB_OS_SEEK_SET)) != 0)
		return (ret);
	if ((ret = __os_write(fhp, nbuf, 256, &n)) != 0)
		return (ret);
	if ((ret = __os_fsync(fhp)) != 0)
		return (ret);

	if (dbp->db_feedback != NULL)
		dbp->db_feedback(dbp, DB_UPGRADE, 100);

	return (0);
}

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)qam_conv.c	11.2 (Sleepycat) 11/10/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "qam.h"
#include "db_swap.h"
#include "db_am.h"

/*
 * __qam_mswap --
 *	Swap the bytes on the queue metadata page.
 *
 * PUBLIC: int __qam_mswap __P((PAGE *));
 */
int
__qam_mswap(pg)
	PAGE *pg;
{
	u_int8_t *p;

	 __db_metaswap(pg);

	 p = (u_int8_t *)pg + sizeof(DBMETA);

	SWAP32(p);		/* start */
	SWAP32(p);		/* first_recno */
	SWAP32(p);		/* cur_recno */
	SWAP32(p);		/* re_len */
	SWAP32(p);		/* re_pad */
	SWAP32(p);		/* rec_page */

	return (0);
}

/*
 * __qam_pgin_out --
 *	Convert host-specific page layout to/from the host-independent format
 *	stored on disk.
 *  We only need to fix up a few fields in the header
 *
 * PUBLIC: int __qam_pgin_out __P((db_pgno_t, void *, DBT *));
 */
int
__qam_pgin_out(pg, pp, cookie)
	db_pgno_t pg;
	void *pp;
	DBT *cookie;
{
	DB_PGINFO *pginfo;
	QPAGE *h;

	COMPQUIET(pg, 0);
	pginfo = (DB_PGINFO *)cookie->data;
	if (!pginfo->needswap)
		return (0);

	h = pp;
	if (h->type == P_QAMMETA)
	    return (__qam_mswap(pp));

	M_32_SWAP(h->lsn.file);
	M_32_SWAP(h->lsn.offset);
	M_32_SWAP(h->pgno);

	return (0);
}

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)bt_reclaim.c	11.1 (Sleepycat) 7/24/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_shash.h"
#include "lock.h"
#include "btree.h"

/*
 * __bam_reclaim --
 *	Free a database.
 *
 * PUBLIC: int __bam_reclaim __P((DB *, DB_TXN *));
 */
int
__bam_reclaim(dbp, txn)
	DB *dbp;
	DB_TXN *txn;
{
	BTREE *t;
	DBC *dbc;
	int ret, t_ret;

	/* Acquire a cursor. */
	if ((ret = dbp->cursor(dbp, txn, &dbc, 0)) != 0)
		return (ret);

	/* Walk the tree, freeing pages. */
	t = dbp->bt_internal;
	ret = __bam_traverse(dbc,
	    DB_LOCK_WRITE, t->bt_root, __db_reclaim_callback, dbc);

	/* Discard the cursor. */
	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

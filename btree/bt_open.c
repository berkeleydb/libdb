/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
/*
 * Copyright (c) 1990, 1993, 1994, 1995, 1996
 *	Keith Bostic.  All rights reserved.
 */
/*
 * Copyright (c) 1990, 1993, 1994, 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)bt_open.c	11.13 (Sleepycat) 10/21/99";
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
#include "db_shash.h"
#include "lock.h"
#include "mp.h"

/*
 * __bam_open --
 *	Open a btree.
 *
 * PUBLIC: int __bam_open __P((DB *, const char *, db_pgno_t));
 */
int
__bam_open(dbp, name, base_pgno)
	DB *dbp;
	const char *name;
	db_pgno_t base_pgno;
{
	BTREE *t;

	t = dbp->bt_internal;

	/* Initialize the remaining fields/methods of the DB. */
	dbp->del = __bam_delete;
	dbp->stat = __bam_stat;

	/*
	 * We don't permit the user to specify a prefix routine if they didn't
	 * also specify a comparison routine, they can't know enough about our
	 * comparison routine to get it right.
	 */
	if (t->bt_compare == __bam_defcmp && t->bt_prefix != __bam_defpfx) {
		__db_err(dbp->dbenv,
"prefix comparison may not be specified for default comparison routine");
		return (EINVAL);
	}

	/* Set the overflow page size. */
	__bam_setovflsize(dbp);

	/* Start up the tree. */
	return (__bam_read_root(dbp, name, base_pgno));
}

/*
 * __bam_setovflsize --
 *
 * PUBLIC: void __bam_setovflsize __P((DB *));
 */
void
__bam_setovflsize(dbp)
	DB *dbp;
{
	BTREE *t;

	t = dbp->bt_internal;

	/*
	 * !!!
	 * Correction for recno, which doesn't know anything about minimum
	 * keys per page.
	 */
	if (t->bt_minkey == 0)
		t->bt_minkey = DEFMINKEYPAGE;

	/*
	 * The btree data structure requires that at least two key/data pairs
	 * can fit on a page, but other than that there's no fixed requirement.
	 * Translate the minimum number of items into the bytes a key/data pair
	 * can use before being placed on an overflow page.  We calculate for
	 * the worst possible alignment by assuming every item requires the
	 * maximum alignment for padding.
	 *
	 * Recno uses the btree bt_ovflsize value -- it's close enough.
	 */
	t->bt_ovflsize = (dbp->pgsize - P_OVERHEAD) / (t->bt_minkey * P_INDX)
	    - (BKEYDATA_PSIZE(0) + ALIGN(1, 4));
}

/*
 * __bam_metachk --
 *
 * PUBLIC: int __bam_metachk __P((DB *, const char *, BTMETA *));
 */
int
__bam_metachk(dbp, name, btm)
	DB *dbp;
	const char *name;
	BTMETA *btm;
{
	DB_ENV *dbenv;
	u_int32_t vers;
	int ret;

	dbenv = dbp->dbenv;

	/*
	 * At this point, all we know is that the magic number is for a Btree.
	 * Check the version, the database may be out of date.
	 */
	vers = btm->dbmeta.version;
	if (F_ISSET(dbp, DB_AM_SWAP))
		M_32_SWAP(vers);
	switch (vers) {
	case 6:
		__db_err(dbenv,
		    "%s: btree version %lu requires a version upgrade",
		    name, (u_long)vers);
		return (DB_OLD_VERSION);
	case 7:
		break;
	default:
		__db_err(dbenv,
		    "%s: unsupported btree version: %lu", name, (u_long)vers);
		return (EINVAL);
	}

	/* Swap the page if we need to. */
	if (F_ISSET(dbp, DB_AM_SWAP) && (ret = __bam_mswap((PAGE *)btm)) != 0)
		return (ret);

	/*
	 * Check application info against metadata info, and set info, flags,
	 * and type based on metadata info.
	 */
	if ((ret =
	    __db_fchk(dbenv, "DB->open", btm->dbmeta.flags, BTM_MASK)) != 0)
		return (ret);

	if (F_ISSET(&btm->dbmeta, BTM_RECNO)) {
		if (dbp->type == DB_BTREE)
			goto wrong_type;
		dbp->type = DB_RECNO;
		DB_ILLEGAL_METHOD(dbp, DB_OK_RECNO);
	} else {
		if (dbp->type == DB_RECNO)
			goto wrong_type;
		dbp->type = DB_BTREE;
		DB_ILLEGAL_METHOD(dbp, DB_OK_BTREE);
	}

	if (F_ISSET(&btm->dbmeta, BTM_DUP))
		F_SET(dbp, DB_AM_DUP);
	else
		if (F_ISSET(dbp, DB_AM_DUP)) {
			__db_err(dbenv,
		"%s: DB_DUP specified to open method but not set in database",
			    name);
			return (EINVAL);
		}

	if (F_ISSET(&btm->dbmeta, BTM_RECNUM)) {
		if (dbp->type != DB_BTREE)
			goto wrong_type;
		F_SET(dbp, DB_BT_RECNUM);

		if ((ret = __db_fcchk(dbenv,
		    "DB->open", dbp->flags, DB_AM_DUP, DB_BT_RECNUM)) != 0)
			return (ret);
	} else
		if (F_ISSET(dbp, DB_BT_RECNUM)) {
			__db_err(dbenv,
	    "%s: DB_RECNUM specified to open method but not set in database",
			    name);
			return (EINVAL);
		}

	if (F_ISSET(&btm->dbmeta, BTM_FIXEDLEN)) {
		if (dbp->type != DB_RECNO)
			goto wrong_type;
		F_SET(dbp, DB_RE_FIXEDLEN);
	} else
		if (F_ISSET(dbp, DB_RE_FIXEDLEN)) {
			__db_err(dbenv,
	"%s: DB_FIXEDLEN specified to open method but not set in database",
			    name);
			return (EINVAL);
		}

	if (F_ISSET(&btm->dbmeta, BTM_RENUMBER)) {
		if (dbp->type != DB_RECNO)
			goto wrong_type;
		F_SET(dbp, DB_RE_RENUMBER);
	} else
		if (F_ISSET(dbp, DB_RE_RENUMBER)) {
			__db_err(dbenv,
	    "%s: DB_RENUMBER specified to open method but not set in database",
			    name);
			return (EINVAL);
		}

	if (F_ISSET(&btm->dbmeta, BTM_SUBDB))
		F_SET(dbp, DB_AM_SUBDB);
	else
		if (F_ISSET(dbp, DB_AM_SUBDB)) {
			__db_err(dbenv,
		    "%s: subdatabase specified but not supported in database",
			    name);
			return (EINVAL);
		}

	/* Set the page size. */
	dbp->pgsize = btm->dbmeta.pagesize;
	F_CLR(dbp, DB_AM_PGDEF);

	/* Copy the file's ID. */
	memcpy(dbp->fileid, btm->dbmeta.uid, DB_FILE_ID_LEN);

	return (0);

wrong_type:
	if (dbp->type == DB_BTREE)
		__db_err(dbenv,
		    "open method type is Btree, database type is Recno");
	else
		__db_err(dbenv,
		    "open method type is Recno, database type is Btree");
	return (EINVAL);
}

/*
 * __bam_read_root --
 *	Check (and optionally create) a tree.
 *
 * PUBLIC: int __bam_read_root __P((DB *, const char *, db_pgno_t));
 */
int
__bam_read_root(dbp, name, base_pgno)
	DB *dbp;
	const char *name;
	db_pgno_t base_pgno;
{
	BTMETA *meta;
	BTREE *t;
	DBC *dbc;
	DB_LSN orig_lsn;
	DB_LOCK metalock;
	PAGE *root;
	int ret, t_ret;

	ret = 0;
	t = dbp->bt_internal;
	meta = NULL;
	root = NULL;

	metalock.off = LOCK_INVALID;

	/* Get a cursor. */
	if ((ret = dbp->cursor(dbp, dbp->open_txn, &dbc, 0)) != 0)
		return (ret);

	/* Get, and optionally create the metadata page. */
	if ((ret =
	    __db_lget(dbc, 0, base_pgno, DB_LOCK_WRITE, 0, &metalock)) != 0)
		goto err;
	if ((ret = memp_fget(
	    dbp->mpf, &base_pgno, DB_MPOOL_CREATE, (PAGE **)&meta)) != 0)
		goto err;

	/*
	 * If the magic number is correct, we're not creating the tree.
	 * Correct any fields that may not be right.  Note, all of the
	 * local flags were set by DB->open.
	 */
	if (meta->dbmeta.magic != 0) {
		t->bt_maxkey = meta->maxkey;
		t->bt_minkey = meta->minkey;
		t->re_pad = meta->re_pad;
		t->re_len = meta->re_len;

		t->bt_meta = base_pgno;
		t->bt_root = meta->root;

		(void)memp_fput(dbp->mpf, (PAGE *)meta, 0);
		meta = NULL;
		goto done;
	}

	/* Initialize the tree structure metadata information. */
	orig_lsn = meta->dbmeta.lsn;
	memset(meta, 0, sizeof(BTMETA));
	ZERO_LSN(meta->dbmeta.lsn);
	meta->dbmeta.pgno = base_pgno;
	meta->dbmeta.magic = DB_BTREEMAGIC;
	meta->dbmeta.version = DB_BTREEVERSION;
	meta->dbmeta.pagesize = dbp->pgsize;
	meta->dbmeta.type = P_BTREEMETA;
	meta->dbmeta.free = PGNO_INVALID;
	if (F_ISSET(dbp, DB_AM_DUP))
		F_SET(&meta->dbmeta, BTM_DUP);
	if (F_ISSET(dbp, DB_RE_FIXEDLEN))
		F_SET(&meta->dbmeta, BTM_FIXEDLEN);
	if (F_ISSET(dbp, DB_BT_RECNUM))
		F_SET(&meta->dbmeta, BTM_RECNUM);
	if (F_ISSET(dbp, DB_RE_RENUMBER))
		F_SET(&meta->dbmeta, BTM_RENUMBER);
	if (F_ISSET(dbp, DB_AM_SUBDB))
		F_SET(&meta->dbmeta, BTM_SUBDB);
	if (dbp->type == DB_RECNO)
		F_SET(&meta->dbmeta, BTM_RECNO);
	memcpy(meta->dbmeta.uid, dbp->fileid, DB_FILE_ID_LEN);

	meta->maxkey = t->bt_maxkey;
	meta->minkey = t->bt_minkey;
	meta->re_len = t->re_len;
	meta->re_pad = t->re_pad;

	/* If necessary, log the meta-data and root page creates.  */
	if ((ret = __db_log_page(dbp,
	    name, &orig_lsn, base_pgno, (PAGE *)meta)) != 0)
		goto err;

	/* Create and initialize a root page. */
	if ((ret = __db_new(dbc,
	    dbp->type == DB_RECNO ? P_LRECNO : P_LBTREE, &root)) != 0)
		goto err;
	root->level = LEAFLEVEL;

	if (dbp->open_txn != NULL && (ret = __bam_root_log(dbp->dbenv,
	    dbp->open_txn, &meta->dbmeta.lsn, 0, dbp->log_fileid,
	    meta->dbmeta.pgno, root->pgno, &meta->dbmeta.lsn)) != 0)
		goto err;

	meta->root = root->pgno;

	DB_TEST_RECOVERY(dbp, DB_TEST_POSTLOGMETA, ret, name);
	if ((ret = __db_log_page(dbp,
	    name, &root->lsn, root->pgno, root)) != 0)
		goto err;
	DB_TEST_RECOVERY(dbp, DB_TEST_POSTLOG, ret, name);

	t->bt_meta = base_pgno;
	t->bt_root = root->pgno;

	/* Release the metadata and root pages. */
	if ((ret = memp_fput(dbp->mpf, (PAGE *)meta, DB_MPOOL_DIRTY)) != 0)
		goto err;
	meta = NULL;
	if ((ret = memp_fput(dbp->mpf, root, DB_MPOOL_DIRTY)) != 0)
		goto err;
	root = NULL;

	/*
	 * Flush the metadata and root pages to disk.
	 *
	 * !!!
	 * It's not useful to return not-yet-flushed here -- convert it to
	 * an error.
	 */
	if ((ret = memp_fsync(dbp->mpf)) == DB_INCOMPLETE)
		ret = EINVAL;
	DB_TEST_RECOVERY(dbp, DB_TEST_POSTSYNC, ret, name);

done:	/*
	 * XXX
	 * We already did an insert and so the last-page-inserted has been
	 * set.  I'm not sure where the *right* place to clear this value
	 * is, it's not intuitively obvious that it belongs here.
	 */
	t->bt_lpgno = PGNO_INVALID;

err:
DB_TEST_RECOVERY_LABEL
	/* Put any remaining pages back. */
	if (meta != NULL)
		if ((t_ret = memp_fput(dbp->mpf, (PAGE *)meta, 0)) != 0 &&
		    ret == 0)
			ret = t_ret;
	if (root != NULL)
		if ((t_ret = memp_fput(dbp->mpf, (PAGE *)root, 0)) != 0 &&
		    ret == 0)
			ret = t_ret;

	/* We can release the metapage lock when we are done. */
	if (metalock.off != LOCK_INVALID)
		(void)__LPUT(dbc, metalock);

	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

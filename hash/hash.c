/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
/*
 * Copyright (c) 1990, 1993, 1994
 *	Margo Seltzer.  All rights reserved.
 */
/*
 * Copyright (c) 1990, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
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
static const char sccsid[] = "@(#)hash.c	11.29 (Sleepycat) 11/14/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_am.h"
#include "db_ext.h"
#include "db_shash.h"
#include "db_swap.h"
#include "hash.h"
#include "btree.h"
#include "log.h"
#include "lock.h"
#include "txn.h"

static int  __ham_c_close __P((DBC *));
static int  __ham_c_del __P((DBC *, u_int32_t));
static int  __ham_c_destroy __P((DBC *));
static int  __ham_c_get __P((DBC *, DBT *, DBT *, u_int32_t));
static int  __ham_c_put __P((DBC *, DBT *, DBT *, u_int32_t));
static int  __ham_delete __P((DB *, DB_TXN *, DBT *, u_int32_t));
static int  __ham_dup_return __P((DBC *, DBT *, u_int32_t));
static int  __ham_expand_table __P((DBC *));
static int  __ham_init_htab __P((DBC *,
		const char *, db_pgno_t, u_int32_t, u_int32_t));
static int  __ham_lookup __P((DBC *, const DBT *, u_int32_t, db_lockmode_t));
static int  __ham_overwrite __P((DBC *, DBT *));

/*
 * __ham_metachk --
 *
 * PUBLIC: int __ham_metachk __P((DB *, const char *, HMETA *));
 */
int
__ham_metachk(dbp, name, hashm)
	DB *dbp;
	const char *name;
	HMETA *hashm;
{
	DB_ENV *dbenv;
	u_int32_t vers;
	int ret;

	dbenv = dbp->dbenv;

	/*
	 * At this point, all we know is that the magic number is for a Hash.
	 * Check the version, the database may be out of date.
	 */
	vers = hashm->dbmeta.version;
	if (F_ISSET(dbp, DB_AM_SWAP))
		M_32_SWAP(vers);
	switch (vers) {
	case 4:
		/* FALLTHROUGH */
	case 5:
		__db_err(dbenv,
		    "%s: hash version %lu requires a version upgrade",
		    name, (u_long)vers);
		return (DB_OLD_VERSION);
	case 6:
		break;
	default:
		__db_err(dbenv,
		    "%s: unsupported hash version: %lu", name, (u_long)vers);
		return (EINVAL);
	}

	/* Swap the page if we need to. */
	if (F_ISSET(dbp, DB_AM_SWAP) && (ret = __ham_mswap((PAGE *)hashm)) != 0)
		return (ret);

	/* Check the type. */
	if (dbp->type != DB_HASH && dbp->type != DB_UNKNOWN)
		return (EINVAL);
	dbp->type = DB_HASH;
	DB_ILLEGAL_METHOD(dbp, DB_OK_HASH);

	/*
	 * Check application info against metadata info, and set info, flags,
	 * and type based on metadata info.
	 */
	if ((ret = __db_fchk(dbenv,
	    "DB->open", hashm->dbmeta.flags,
	    DB_HASH_DUP | DB_HASH_SUBDB)) != 0)
		return (ret);

	if (F_ISSET(&hashm->dbmeta, DB_HASH_DUP))
		F_SET(&hashm->dbmeta, DB_HASH_DUP);
	else
		if (F_ISSET(dbp, DB_AM_DUP)) {
			__db_err(dbenv,
		"%s: DB_DUP specified to open method but not set in database",
			    name);
			return (EINVAL);
		}

	if (F_ISSET(&hashm->dbmeta, DB_HASH_SUBDB))
		F_SET(dbp, DB_AM_SUBDB);
	else
		if (F_ISSET(dbp, DB_AM_SUBDB)) {
			__db_err(dbenv,
		    "%s: subdatabase specified but not supported in database",
			name);
			return (EINVAL);
		}

	/* Set the page size. */
	dbp->pgsize = hashm->dbmeta.pagesize;
	F_CLR(dbp, DB_AM_PGDEF);

	/* Copy the file's ID. */
	memcpy(dbp->fileid, hashm->dbmeta.uid, DB_FILE_ID_LEN);

	return (0);
}

/*
 * __ham_open --
 *
 * PUBLIC: int __ham_open __P((DB *, const char *, db_pgno_t));
 */
int
__ham_open(dbp, name, base_pgno)
	DB *dbp;
	const char *name;
	db_pgno_t base_pgno;
{
	DB_ENV *dbenv;
	DBC *dbc;
	HASH_CURSOR *hcp;
	HASH *hashp;
	int need_sync, ret, t_ret;

	dbc = NULL;
	dbenv = dbp->dbenv;
	need_sync = 0;

	/* Initialize the remaining fields/methods of the DB. */
	dbp->del = __ham_delete;
	dbp->stat = __ham_stat;

	/* Get a cursor we can use for the rest of this function. */
	if ((ret = dbp->cursor(dbp, dbp->open_txn, &dbc, 0)) != 0)
		return (ret);

	hcp = (HASH_CURSOR *)dbc->internal;
	hashp = dbp->h_internal;
	hashp->meta_pgno = base_pgno;
	if ((ret = __ham_get_meta(dbc)) != 0)
		goto err1;

	/*
	 * If this is a new file, initialize it, and put it back dirty.
	 *
	 * Initialize the hdr structure.
	 */
	if (hcp->hdr->dbmeta.magic == DB_HASHMAGIC) {
		/* File exists, verify the data in the header. */
		if (hashp->h_hash == NULL)
			hashp->h_hash = hcp->hdr->dbmeta.version < 5
			? __ham_func4 : __ham_func5;
		if (hashp->h_hash(CHARKEY, sizeof(CHARKEY)) !=
		    hcp->hdr->h_charkey) {
			__db_err(dbp->dbenv,
			    "hash: incompatible hash function");
			ret = EINVAL;
			goto err2;
		}
		if (F_ISSET(&hcp->hdr->dbmeta, DB_HASH_DUP))
			F_SET(dbp, DB_AM_DUP);
		if (F_ISSET(&hcp->hdr->dbmeta, DB_HASH_SUBDB))
			F_SET(dbp, DB_AM_SUBDB);
	} else {
		/*
		 * File does not exist, we must initialize the header.  If
		 * locking is enabled that means getting a write lock first.
		 */
		dbc->lock.pgno = base_pgno;

		if (F_ISSET(dbenv, DB_ENV_LOCKING) &&
		    ((ret = lock_put(dbenv, &hcp->hlock)) != 0 ||
		    (ret = lock_get(dbenv, dbc->locker,
		    DB_NONBLOCK(dbc) ? DB_LOCK_NOWAIT : 0,
		    &dbc->lock_dbt, DB_LOCK_WRITE, &hcp->hlock)) != 0))
			goto err2;

		if ((ret = __ham_init_htab(dbc, name,
		    base_pgno, hashp->h_nelem, hashp->h_ffactor)) != 0)
			goto err2;

		need_sync = 1;
	}

err2:	/* Release the meta data page */
	if ((t_ret = __ham_release_meta(dbc)) != 0 && ret == 0)
		ret = t_ret;
err1:	if ((t_ret  = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	/* Sync the file so that we know that the meta data goes to disk. */
	if (ret == 0 && need_sync)
		ret = dbp->sync(dbp, 0);
#if CONFIG_TEST
	if (ret == 0)
		DB_TEST_RECOVERY(dbp, DB_TEST_POSTSYNC, ret, name);

DB_TEST_RECOVERY_LABEL
#endif
	if (ret != 0)
		(void)__ham_db_close(dbp);

	return (ret);
}

/************************** LOCAL CREATION ROUTINES **********************/
/*
 * Returns 0 on No Error
 */
static int
__ham_init_htab(dbc, name, pgno, nelem, ffactor)
	DBC *dbc;
	const char *name;
	db_pgno_t pgno;
	u_int32_t nelem, ffactor;
{
	DB *dbp;
	DB_LOCK metalock;
	DB_LSN orig_lsn;
	DBMETA *mmeta;
	HASH_CURSOR *hcp;
	HASH *hashp;
	PAGE *h;
	db_pgno_t mpgno;
	int32_t l2, nbuckets;
	int dirty_mmeta, i, ret, t_ret;

	hcp = (HASH_CURSOR *)dbc->internal;
	dbp = dbc->dbp;
	hashp = dbp->h_internal;
	mmeta = NULL;
	dirty_mmeta = 0;
	metalock.off = LOCK_INVALID;

	if (hashp->h_hash == NULL)
		hashp->h_hash = DB_HASHVERSION < 5 ? __ham_func4 : __ham_func5;

	if (nelem != 0 && ffactor != 0) {
		nelem = (nelem - 1) / ffactor + 1;
		l2 = __db_log2(nelem > 2 ? nelem : 2);
	} else
		l2 = 1;
	nbuckets = 1 << l2;

	orig_lsn = hcp->hdr->dbmeta.lsn;
	memset(hcp->hdr, 0, sizeof(HMETA));
	ZERO_LSN(hcp->hdr->dbmeta.lsn);
	hcp->hdr->dbmeta.pgno = pgno;
	hcp->hdr->dbmeta.magic = DB_HASHMAGIC;
	hcp->hdr->dbmeta.version = DB_HASHVERSION;
	hcp->hdr->dbmeta.pagesize = dbp->pgsize;
	hcp->hdr->dbmeta.type = P_HASHMETA;
	hcp->hdr->dbmeta.free = PGNO_INVALID;
	hcp->hdr->max_bucket = hcp->hdr->high_mask = nbuckets - 1;
	hcp->hdr->low_mask = (nbuckets >> 1) - 1;
	hcp->hdr->ffactor = ffactor;
	hcp->hdr->h_charkey = hashp->h_hash(CHARKEY, sizeof(CHARKEY));
	memcpy(hcp->hdr->dbmeta.uid, dbp->fileid, DB_FILE_ID_LEN);

	if (F_ISSET(dbp, DB_AM_DUP))
		F_SET(&hcp->hdr->dbmeta, DB_HASH_DUP);
	if (F_ISSET(dbp, DB_AM_SUBDB)) {
		F_SET(&hcp->hdr->dbmeta, DB_HASH_SUBDB);

		/*
		 * If this is a subdatabase, then we need to get the LSN
		 * off the master meta data page because that's where free
		 * pages are linked and during recovery we need to access
		 * that page and roll it backward/forward correctly with
		 * respect to LSN.
		 */
		mpgno = PGNO_BASE_MD;
		if ((ret = __db_lget(dbc,
		   0, mpgno, DB_LOCK_WRITE, 0, &metalock)) != 0)
			return (ret);
		if ((ret = memp_fget(dbp->mpf,
		    &mpgno, 0, (PAGE **)&mmeta)) != 0)
			goto err;
	}
	if ((ret = __ham_dirty_page(dbp, (PAGE *)hcp->hdr)) != 0)
		goto err;

	/*
	 * Create the first and second buckets pages so that we have the
	 * page numbers for them and we can store that page number
	 * in the meta-data header (spares[0]).
	 */
	hcp->hdr->spares[0] = nbuckets;
	if ((ret = memp_fget(dbp->mpf,
	    &hcp->hdr->spares[0], DB_MPOOL_NEW_GROUP, &h)) != 0)
		goto err;

	P_INIT(h, dbp->pgsize, hcp->hdr->spares[0], PGNO_INVALID,
	    PGNO_INVALID, 0, P_HASH);

	/* Fill in the last fields of the meta data page. */
	hcp->hdr->spares[0] -= (nbuckets - 1);
	for (i = 1; i <= l2; i++)
		hcp->hdr->spares[i] = hcp->hdr->spares[0];
	for (; i < NCACHED; i++)
		hcp->hdr->spares[i] = PGNO_INVALID;

	/*
	 * Before we are about to put any dirty pages, we need to log
	 * the meta-data page create.
	 */
	ret = __db_log_page(dbp, name, &orig_lsn, pgno, (PAGE *)hcp->hdr);

	if (dbp->open_txn != NULL) {
		if ((t_ret = __ham_groupalloc_log(dbp->dbenv, dbp->open_txn,
		    &hcp->hdr->dbmeta.lsn, 0, dbp->log_fileid,
		    hcp->hdr->dbmeta.pgno, &hcp->hdr->dbmeta.lsn,
		    mmeta == NULL ? &hcp->hdr->dbmeta.lsn : &mmeta->lsn,
		    hcp->hdr->spares[0], hcp->hdr->max_bucket + 1)) != 0 &&
		    ret == 0)
			ret = t_ret;
		if (t_ret == 0 && mmeta != NULL) {
			mmeta->lsn = hcp->hdr->dbmeta.lsn;
			dirty_mmeta = 1;
		}
	}

	DB_TEST_RECOVERY(dbp, DB_TEST_POSTLOG, ret, name);

DB_TEST_RECOVERY_LABEL
	if ((t_ret = memp_fput(dbp->mpf, h, DB_MPOOL_DIRTY)) != 0 && ret == 0)
		ret = t_ret;

err:	if (mmeta != NULL)
		if ((t_ret = memp_fput(dbp->mpf, mmeta,
		    dirty_mmeta ? DB_MPOOL_DIRTY : 0)) != 0 && ret == 0)
			ret = t_ret;
	if (metalock.off != LOCK_INVALID)
		(void)__TLPUT(dbc, metalock);

	return (ret);
}

static int
__ham_delete(dbp, txn, key, flags)
	DB *dbp;
	DB_TXN *txn;
	DBT *key;
	u_int32_t flags;
{
	DBC *dbc;
	HASH_CURSOR *hcp;
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);
	DB_ILLEGAL_BEFORE_OPEN(dbp, "DB->del");

	if ((ret =
	    __db_delchk(dbp, key, flags, F_ISSET(dbp, DB_AM_RDONLY))) != 0)
		return (ret);

	if ((ret = dbp->cursor(dbp, txn, &dbc, DB_WRITELOCK)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, txn, "ham_delete", key, NULL, flags);

	hcp = (HASH_CURSOR *)dbc->internal;
	if ((ret = __ham_get_meta(dbc)) != 0)
		goto out;

	if ((ret = __ham_lookup(dbc, key, 0, DB_LOCK_WRITE)) == 0) {
		if (F_ISSET(hcp, H_OK))
			ret = __ham_del_pair(dbc, 1);
		else
			ret = DB_NOTFOUND;
	}

	if ((t_ret = __ham_release_meta(dbc)) != 0 && ret == 0)
		ret = t_ret;

out:	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/* ****************** CURSORS ********************************** */
/*
 * __ham_c_init --
 *	Initialize the hash-specific portion of a cursor.
 *
 * PUBLIC: int __ham_c_init __P((DBC *));
 */
int
__ham_c_init(dbc)
	DBC *dbc;
{
	HASH_CURSOR *new_curs;
	int ret;

	if ((ret = __os_calloc(1, sizeof(struct cursor_t), &new_curs)) != 0)
		return (ret);
	if ((ret =
	    __os_malloc(dbc->dbp->pgsize, NULL, &new_curs->split_buf)) != 0) {
		__os_free(new_curs, sizeof(*new_curs));
		return (ret);
	}

	new_curs->dbc = dbc;

	dbc->internal = new_curs;
	dbc->c_am_close = __ham_c_close;
	dbc->c_am_destroy = __ham_c_destroy;
	dbc->c_del = __ham_c_del;
	dbc->c_get = __ham_c_get;
	dbc->c_put = __ham_c_put;

	__ham_item_init(new_curs);

	return (0);
}

/*
 * __ham_c_close --
 *	Close down the cursor from a single use.
 */
static int
__ham_c_close(dbc)
	DBC *dbc;
{
	int ret;

	if ((ret = __ham_item_done(dbc, 0)) != 0)
		return (ret);

	__ham_item_init((HASH_CURSOR *)dbc->internal);
	return (0);
}

/*
 * __ham_c_destroy --
 *	Cleanup the access method private part of a cursor.
 */
static int
__ham_c_destroy(dbc)
	DBC *dbc;
{
	HASH_CURSOR *hcp;

	hcp = (HASH_CURSOR *)dbc->internal;
	if (hcp->split_buf != NULL)
		__os_free(hcp->split_buf, dbc->dbp->pgsize);
	__os_free(hcp, sizeof(HASH_CURSOR));

	return (0);
}

static int
__ham_c_del(dbc, flags)
	DBC *dbc;
	u_int32_t flags;
{
	DB *dbp;
	DBT repldbt;
	HASH_CURSOR *hcp;
	HASH_CURSOR save_curs;
	db_pgno_t ppgno, chg_pgno;
	int ret, t_ret;

	DEBUG_LWRITE(dbc, dbc->txn, "ham_c_del", NULL, NULL, flags);
	dbp = dbc->dbp;
	PANIC_CHECK(dbp->dbenv);
	hcp = (HASH_CURSOR *)dbc->internal;

	if ((ret = __db_cdelchk(dbc->dbp, flags,
	    F_ISSET(dbc->dbp, DB_AM_RDONLY), IS_VALID(hcp))) != 0)
		return (ret);

	if (F_ISSET(hcp, H_DELETED))
		return (DB_NOTFOUND);

	/*
	 * If we are in the concurrent DB product and this cursor
	 * is not a write cursor, then this request is invalid.
	 * If it is a simple write cursor, then we need to upgrade its
	 * lock.
	 */
	if (F_ISSET(dbp->dbenv, DB_ENV_CDB)) {
		/* Make sure it's a valid update cursor. */
		if (!F_ISSET(dbc, DBC_WRITECURSOR | DBC_WRITER))
			return (EPERM);

		if (F_ISSET(dbc, DBC_WRITECURSOR) &&
		    (ret = lock_get(dbp->dbenv, dbc->locker,
		    DB_LOCK_UPGRADE, &dbc->lock_dbt, DB_LOCK_WRITE,
		    &dbc->mylock)) != 0)
			return (ret);
	}

	SAVE_CURSOR(hcp, &save_curs);

	if ((ret = __ham_get_meta(dbc)) != 0)
		goto out;

	if ((ret = __ham_get_cpage(dbc, DB_LOCK_WRITE)) != 0)
		goto out;
	if (F_ISSET(hcp, H_ISDUP) && hcp->dpgno != PGNO_INVALID) {
		/*
		 * We are about to remove a duplicate from offpage.
		 *
		 * There are 4 cases.
		 * 1. We will remove an item on a page, but there are more
		 *    items on that page.
		 * 2. We will remove the last item on a page, but there is a
		 *    following page of duplicates.
		 * 3. We will remove the last item on a page, this page was the
		 *    last page in a duplicate set, but there were dups before
		 *    it.
		 * 4. We will remove the last item on a page, removing the last
		 *    duplicate.
		 * In case 1 hcp->dpagep is unchanged.
		 * In case 2 hcp->dpagep comes back pointing to the next dup
		 *     page.
		 * In case 3 hcp->dpagep comes back NULL.
		 * In case 4 hcp->dpagep comes back NULL.
		 *
		 * Case 4 results in deleting the pair off the master page.
		 * The normal code for doing this knows how to delete the
		 * duplicates, so we will handle this case in the normal code.
		 */
		ppgno = PREV_PGNO(hcp->dpagep);
		if (ppgno == PGNO_INVALID &&
		    NEXT_PGNO(hcp->dpagep) == PGNO_INVALID &&
		    NUM_ENT(hcp->dpagep) == 1)
			goto normal;

		/* Remove item from duplicate page. */
		chg_pgno = hcp->dpgno;
		if ((ret = __db_drem(dbc, &hcp->dpagep, hcp->dndx)) != 0)
			goto out;

		if (hcp->dpagep == NULL) {
			if (ppgno != PGNO_INVALID) {		/* Case 3 */
				hcp->dpgno = ppgno;
				if ((ret = __ham_get_cpage(dbc,
				    DB_LOCK_READ)) != 0)
					goto out;
				hcp->dndx = NUM_ENT(hcp->dpagep);
				F_SET(hcp, H_DELETED);
			} else {				/* Case 4 */
				ret = __ham_del_pair(dbc, 1);
				hcp->dpgno = PGNO_INVALID;
				/*
				 * Delpair updated the cursor queue, so we
				 * don't have to do that here.
				 */
				chg_pgno = PGNO_INVALID;
			}
		} else if (PGNO(hcp->dpagep) != hcp->dpgno) {
			hcp->dndx = 0;				/* Case 2 */
			hcp->dpgno = PGNO(hcp->dpagep);
			if (ppgno == PGNO_INVALID)
				memcpy(HOFFDUP_PGNO(P_ENTRY(hcp->pagep,
				    H_DATAINDEX(hcp->bndx))),
				    &hcp->dpgno, sizeof(db_pgno_t));
			/*
			 * We need to put the master page here, because
			 * although we have a duplicate page, the master
			 * page is dirty, and ham_item_done assumes that
			 * if you have a duplicate page, it's the only one
			 * that can be dirty.
			 */
			ret = __ham_put_page(dbp, hcp->pagep, 1);
			hcp->pagep = NULL;
			F_SET(hcp, H_DELETED);
		} else						/* Case 1 */
			F_SET(hcp, H_DELETED);
		if (chg_pgno != PGNO_INVALID)
			__ham_c_update(hcp, chg_pgno, 0, 0, 1);
	} else if (F_ISSET(hcp, H_ISDUP)) {			/* on page */
		if (hcp->dup_off == 0 && DUP_SIZE(hcp->dup_len) ==
		    LEN_HDATA(hcp->pagep, hcp->hdr->dbmeta.pagesize, hcp->bndx))
			ret = __ham_del_pair(dbc, 1);
		else {
			repldbt.flags = 0;
			F_SET(&repldbt, DB_DBT_PARTIAL);
			repldbt.doff = hcp->dup_off;
			repldbt.dlen = DUP_SIZE(hcp->dup_len);
			repldbt.size = 0;
			repldbt.data =
			    HKEYDATA_DATA(H_PAIRDATA(hcp->pagep, hcp->bndx));
			ret = __ham_replpair(dbc, &repldbt, 0);
			hcp->dup_tlen -= DUP_SIZE(hcp->dup_len);
			F_SET(hcp, H_DELETED);
			__ham_c_update(hcp, hcp->pgno,
			    DUP_SIZE(hcp->dup_len), 0, 1);
		}

	} else
		/* Not a duplicate */
normal:		ret = __ham_del_pair(dbc, 1);

out:	if ((t_ret = __ham_item_done(dbc, ret == 0)) != 0 && ret == 0)
		ret = t_ret;
	if ((t_ret = __ham_release_meta(dbc)) != 0 && ret == 0)
		ret = t_ret;
	RESTORE_CURSOR(dbp, hcp, &save_curs, ret);
	if (F_ISSET(dbc, DBC_WRITECURSOR))
		(void)__lock_downgrade(dbp->dbenv,
		    &dbc->mylock, DB_LOCK_IWRITE, 0);
	return (ret);
}

/*
 * __ham_c_dup --
 *	Duplicate a hash cursor, such that the new one holds appropriate
 *	locks for the position of the original.
 *
 * PUBLIC: int __ham_c_dup __P((DBC *, DBC *));
 */
int
__ham_c_dup(orig_dbc, new_dbc)
	DBC *orig_dbc, *new_dbc;
{
	HASH_CURSOR *orig, *new;

	orig = (HASH_CURSOR *)orig_dbc->internal;
	new = (HASH_CURSOR *)new_dbc->internal;

#ifdef DIAGNOSTIC
	memset(new, 0, sizeof(*new));
#endif
	new->dbc = orig->dbc;
	new->bucket = orig->bucket;
	new->lbucket = orig->lbucket;
	new->pgno = orig->pgno;
	new->bndx = orig->bndx;
	new->dpgno = orig->dpgno;
	new->dndx = orig->dndx;
	new->dup_off = orig->dup_off;
	new->dup_len = orig->dup_len;
	new->dup_tlen = orig->dup_tlen;

	if (F_ISSET(orig, H_DELETED))
		F_SET(new, H_DELETED);
	if (F_ISSET(orig, H_ISDUP))
		F_SET(new, H_ISDUP);

	/*
	 * If the old cursor held a lock and we're not in transactions, get one
	 * for the new one.   The reason that we don't need a new lock if we're
	 * in a transaction is because we already hold a lock and will continue
	 * to do so until commit, so there is no point in reaquiring it. We
	 * don't know if the old lock was a read or write lock, but it doesn't
	 * matter. We'll get a read lock.  We know that this locker already
	 * holds a lock of the correct type, so if we need a write lock and
	 * request it, we know that we'll get it.
	 */
	if (orig->lock.off == LOCK_INVALID || orig_dbc->txn != NULL) {
		new->lock.off = LOCK_INVALID;
		return (0);
	}

	return (__ham_lock_bucket(new_dbc, DB_LOCK_READ));
}

static int
__ham_c_get(dbc, key, data, flags)
	DBC *dbc;
	DBT *key;
	DBT *data;
	u_int32_t flags;
{
	DB *dbp;
	HASH_CURSOR *hcp, save_curs;
	db_lockmode_t lock_type;
	int get_key, ret, t_ret;

	DEBUG_LREAD(dbc, dbc->txn, "ham_c_get",
	    flags == DB_SET || flags == DB_SET_RANGE ? key : NULL,
	    NULL, flags);

	hcp = (HASH_CURSOR *)dbc->internal;
	dbp = dbc->dbp;
	PANIC_CHECK(dbp->dbenv);
	if ((ret = __db_cgetchk(dbp, key, data, flags, IS_VALID(hcp))) != 0)
		return (ret);

	/* Clear OR'd in additional bits so we can check for flag equality. */
	if (LF_ISSET(DB_RMW)) {
		lock_type = DB_LOCK_WRITE;
		LF_CLR(DB_RMW);
	} else
		lock_type = DB_LOCK_READ;

	SAVE_CURSOR(hcp, &save_curs);
	if ((ret = __ham_get_meta(dbc)) != 0)
		return (ret);
	hcp->seek_size = 0;

	ret = 0;
	get_key = 1;
	switch (flags) {
	case DB_PREV:
		if (hcp->bucket != BUCKET_INVALID) {
			ret = __ham_item_prev(dbc, lock_type);
			break;
		}
		/* FALLTHROUGH */
	case DB_LAST:
		ret = __ham_item_last(dbc, lock_type);
		break;
	case DB_NEXT:
		if (hcp->bucket != BUCKET_INVALID) {
			ret = __ham_item_next(dbc, lock_type);
			break;
		}
		/* FALLTHROUGH */
	case DB_FIRST:
		ret = __ham_item_first(dbc, lock_type);
		break;
	case DB_NEXT_DUP:
		/* cgetchk has already determined that the cursor is set. */
		F_SET(hcp, H_DUPONLY);
		ret = __ham_item_next(dbc, lock_type);
		break;
	case DB_SET:
	case DB_SET_RANGE:
	case DB_GET_BOTH:
		if (F_ISSET(dbc, DBC_CONTINUE)) {
			F_SET(hcp, H_DUPONLY);
			ret = __ham_item_next(dbc, lock_type);
		} else
			ret = __ham_lookup(dbc, key, 0, lock_type);
		get_key = 0;
		break;
	case DB_CURRENT:
		/* cgetchk has already determined that the cursor is set. */
		if (F_ISSET(hcp, H_DELETED)) {
			ret = DB_KEYEMPTY;
			goto err1;
		}

		ret = __ham_item(dbc, lock_type);
		break;
	}

	/*
	 * Must always enter this loop to do error handling and
	 * check for big key/data pair.
	 */
	while (1) {
		if (ret != 0 && ret != DB_NOTFOUND)
			goto err2;
		else if (F_ISSET(hcp, H_OK)) {
			/* Get the key. */
			if (get_key && (ret = __db_ret(dbp, hcp->pagep,
			    H_KEYINDEX(hcp->bndx), key, &dbc->rkey.data,
			    &dbc->rkey.size)) != 0)
				goto err2;

			ret = __ham_dup_return(dbc, data, flags);
			break;
		} else if (!F_ISSET(hcp, H_NOMORE)) {
			abort();
			break;
		}

		/*
		 * Ran out of entries in a bucket; change buckets.
		 */
		switch (flags) {
			case DB_LAST:
			case DB_PREV:
				ret = __ham_item_done(dbc, 0);
				if (hcp->bucket == 0) {
					ret = DB_NOTFOUND;
					goto err2;
				}
				hcp->bucket--;
				hcp->bndx = NDX_INVALID;
				if (ret == 0)
					ret = __ham_item_prev(dbc, lock_type);
				break;
			case DB_FIRST:
			case DB_NEXT:
				ret = __ham_item_done(dbc, 0);
				hcp->bndx = NDX_INVALID;
				hcp->bucket++;
				hcp->pgno = PGNO_INVALID;
				hcp->pagep = NULL;
				if (hcp->bucket > hcp->hdr->max_bucket) {
					ret = DB_NOTFOUND;
					goto err2;
				}
				if (ret == 0)
					ret = __ham_item_next(dbc, lock_type);
				break;
			case DB_GET_BOTH:
			case DB_NEXT_DUP:
			case DB_SET:
			case DB_SET_RANGE:
				/* Key not found. */
				ret = DB_NOTFOUND;
				goto err2;
			case DB_CURRENT:
				/*
				 * This should only happen if you are doing
				 * deletes and reading with concurrent threads
				 * and not doing proper locking.  We return
				 * the same error code as we would if the
				 * cursor were deleted.
				 */
				ret = DB_KEYEMPTY;
				goto err2;
		}
	}

err2:	if ((t_ret = __ham_item_done(dbc, 0)) != 0 && ret == 0)
		ret = t_ret;
err1:	if ((t_ret = __ham_release_meta(dbc)) != 0 && ret == 0)
		ret = t_ret;

	RESTORE_CURSOR(dbp, hcp, &save_curs, ret);

	F_CLR(hcp, H_DUPONLY);

	return (ret);
}

static int
__ham_c_put(dbc, key, data, flags)
	DBC *dbc;
	DBT *key;
	DBT *data;
	u_int32_t flags;
{
	DB *dbp;
	DBT tmp_val, *myval;
	HASH_CURSOR *hcp, save_curs;
	u_int32_t nbytes;
	int ret, t_ret;

	/*
	 * The compiler doesn't realize that we only use this when ret is
	 * equal to 0 and that if ret is equal to 0, that we must have set
	 * myval.  So, we initialize it here to shut the compiler up.
	 */
	COMPQUIET(myval, NULL);

	dbp = dbc->dbp;
	PANIC_CHECK(dbp->dbenv);
	DEBUG_LWRITE(dbc, dbc->txn, "ham_c_put",
	    flags == DB_KEYFIRST || flags == DB_KEYLAST ? key : NULL,
	    data, flags);
	hcp = (HASH_CURSOR *)dbc->internal;

	if ((ret = __db_cputchk(dbp, key, data, flags,
	    F_ISSET(dbp, DB_AM_RDONLY), IS_VALID(hcp))) != 0)
		return (ret);

	if (F_ISSET(hcp, H_DELETED) &&
	    flags != DB_KEYFIRST && flags != DB_KEYLAST)
		return (DB_NOTFOUND);

	/*
	 * If we are in the concurrent DB product and this cursor
	 * is not a write cursor, then this request is invalid.
	 * If it is a simple write cursor, then we need to upgrade its
	 * lock.
	 */
	if (F_ISSET(dbp->dbenv, DB_ENV_CDB)) {
		/* Make sure it's a valid update cursor. */
		if (!F_ISSET(dbc, DBC_WRITECURSOR | DBC_WRITER))
			return (EPERM);

		if (F_ISSET(dbc, DBC_WRITECURSOR) &&
		    (ret = lock_get(dbp->dbenv, dbc->locker,
		    DB_LOCK_UPGRADE, &dbc->lock_dbt, DB_LOCK_WRITE,
		    &dbc->mylock)) != 0)
			return (ret);
	}

	SAVE_CURSOR(hcp, &save_curs);

	if ((ret = __ham_get_meta(dbc)) != 0)
		goto err1;

	switch (flags) {
	case DB_KEYLAST:
	case DB_KEYFIRST:
		nbytes = (ISBIG(hcp, key->size) ? HOFFPAGE_PSIZE :
		    HKEYDATA_PSIZE(key->size)) +
		    (ISBIG(hcp, data->size) ? HOFFPAGE_PSIZE :
		    HKEYDATA_PSIZE(data->size));
		if ((ret = __ham_lookup(dbc,
		    key, nbytes, DB_LOCK_WRITE)) == DB_NOTFOUND) {
			ret = 0;
			if (hcp->seek_found_page != PGNO_INVALID &&
			    hcp->seek_found_page != hcp->pgno) {
				if ((ret = __ham_item_done(dbc, 0)) != 0)
					goto err2;
				hcp->pgno = hcp->seek_found_page;
				hcp->bndx = NDX_INVALID;
			}

			if (F_ISSET(data, DB_DBT_PARTIAL) && data->doff != 0) {
				/*
				 * A partial put, but the key does not exist
				 * and we are not beginning the write at 0.
				 * We must create a data item padded up to doff
				 * and then write the new bytes represented by
				 * val.
				 */
				if ((ret = __ham_init_dbt(&tmp_val,
				    data->size + data->doff,
				    &dbc->rdata.data, &dbc->rdata.size)) == 0) {
					memset(tmp_val.data, 0, data->doff);
					memcpy((u_int8_t *)tmp_val.data +
					    data->doff, data->data, data->size);
					myval = &tmp_val;
				}
			} else
				myval = (DBT *)data;

			if (ret == 0)
				ret = __ham_add_el(dbc, key, myval, H_KEYDATA);
			goto done;
		}
		break;
	case DB_BEFORE:
	case DB_AFTER:
	case DB_CURRENT:
		ret = __ham_item(dbc, DB_LOCK_WRITE);
		break;
	}

	if (ret == 0) {
		if (flags == DB_CURRENT ||
		    ((flags == DB_KEYFIRST || flags == DB_KEYLAST) &&
		    !F_ISSET(dbp, DB_AM_DUP)))
			ret = __ham_overwrite(dbc, data);
		else
			ret = __ham_add_dup(dbc, data, flags);
	}

done:	if (ret == 0 && F_ISSET(hcp, H_EXPAND)) {
		ret = __ham_expand_table(dbc);
		F_CLR(hcp, H_EXPAND);
	}

	if ((t_ret = __ham_item_done(dbc, ret == 0)) != 0 && ret == 0)
		ret = t_ret;

err2:	if ((t_ret = __ham_release_meta(dbc)) != 0 && ret == 0)
		ret = t_ret;

err1:	RESTORE_CURSOR(dbp, hcp, &save_curs, ret);


	if (F_ISSET(dbc, DBC_WRITECURSOR))
		(void)__lock_downgrade(dbp->dbenv,
		    &dbc->mylock, DB_LOCK_IWRITE, 0);

	return (ret);
}

/********************************* UTILITIES ************************/

/*
 * __ham_expand_table --
 */
static int
__ham_expand_table(dbc)
	DBC *dbc;
{
	DB *dbp;
	PAGE *h;
	HASH_CURSOR *hcp;
	db_pgno_t pgno;
	u_int32_t old_bucket, new_bucket;
	int ret;

	dbp = dbc->dbp;
	hcp = (HASH_CURSOR *)dbc->internal;
	if ((ret = __ham_dirty_meta(dbc)) != 0)
		return (ret);

	/*
	 * If the split point is about to increase, make sure that we
	 * have enough extra pages.  The calculation here is weird.
	 * We'd like to do this after we've upped max_bucket, but it's
	 * too late then because we've logged the meta-data split.  What
	 * we'll do between then and now is increment max bucket and then
	 * see what the log of one greater than that is; here we have to
	 * look at the log of max + 2.  VERY NASTY STUFF.
	 *
	 * It just got even nastier.  With subdatabases, we have to request
	 * a chunk of contiguous pages, so we do that here using an
	 * undocumented feature of mpool (the MPOOL_NEW_GROUP flag) to
	 * give us a number of contiguous pages.  Ouch.
	 */
	if (hcp->hdr->max_bucket == hcp->hdr->high_mask) {
		/*
		 * Ask mpool to give us a set of contiguous page numbers
		 * large enough to contain the next doubling.
		 *
		 * Figure out how many new pages we need.   This will return
		 * us the last page.  We calculate its page number, initialize
		 * the page and then write it back to reserve all the pages
		 * in between.  It is possible that the allocation of new pages
		 * has already been done, but the tranaction aborted.  Since
		 * we don't undo the allocation, check for a valid pgno before
		 * doing the allocation.
		 */
		pgno = hcp->hdr->max_bucket + 1;
		if (hcp->hdr->spares[__db_log2(pgno) + 1] == PGNO_INVALID)
			/* Allocate a group of pages. */
			ret = memp_fget(dbp->mpf,
			    &pgno, DB_MPOOL_NEW_GROUP, &h);
		else {
			/* Just read in the last page of the batch */
			pgno = hcp->hdr->spares[__db_log2(pgno) + 1] +
			    hcp->hdr->max_bucket + 1;
			ret = memp_fget(dbp->mpf,
			    &pgno, DB_MPOOL_CREATE, &h);
		}
		if (ret != 0)
			return (ret);

		P_INIT(h, dbp->pgsize, pgno,
		    PGNO_INVALID, PGNO_INVALID, 0, P_HASH);
		pgno -= hcp->hdr->max_bucket;
	} else {
		pgno = BUCKET_TO_PAGE(hcp, hcp->hdr->max_bucket + 1);
		if ((ret =
		    memp_fget(dbp->mpf, &pgno, DB_MPOOL_CREATE, &h)) != 0)
			return (ret);
	}

	/* Now we can log the meta-data split. */
	if (DB_LOGGING(dbc)) {
		if ((ret = __ham_metagroup_log(dbp->dbenv,
		    dbc->txn, &h->lsn, 0, dbp->log_fileid,
		    hcp->hdr->max_bucket, pgno, &hcp->hdr->dbmeta.lsn,
		    &h->lsn)) != 0)
			return (ret);

		hcp->hdr->dbmeta.lsn = h->lsn;
	}

	/* If we allocated some new pages, write out the last page. */
	if ((ret = memp_fput(dbp->mpf, h, DB_MPOOL_DIRTY)) != 0)
		return (ret);

	new_bucket = ++hcp->hdr->max_bucket;
	old_bucket = (hcp->hdr->max_bucket & hcp->hdr->low_mask);

	/*
	 * If we started a new doubling, fill in the spares array with
	 * the starting page number negatively offset by the bucket number.
	 */
	if (new_bucket > hcp->hdr->high_mask) {
		/* Starting a new doubling */
		hcp->hdr->low_mask = hcp->hdr->high_mask;
		hcp->hdr->high_mask = new_bucket | hcp->hdr->low_mask;
		if (hcp->hdr->spares[__db_log2(new_bucket) + 1] == PGNO_INVALID)
			hcp->hdr->spares[__db_log2(new_bucket) + 1] =
			    pgno - new_bucket;
	}

	/* Relocate records to the new bucket */
	return (__ham_split_page(dbc, old_bucket, new_bucket));
}

/*
 * PUBLIC: u_int32_t __ham_call_hash __P((HASH_CURSOR *, u_int8_t *, int32_t));
 */
u_int32_t
__ham_call_hash(hcp, k, len)
	HASH_CURSOR *hcp;
	u_int8_t *k;
	int32_t len;
{
	u_int32_t n, bucket;
	HASH *hashp;

	hashp = hcp->dbc->dbp->h_internal;
	n = (u_int32_t)(hashp->h_hash(k, len));

	bucket = n & hcp->hdr->high_mask;
	if (bucket > hcp->hdr->max_bucket)
		bucket = bucket & hcp->hdr->low_mask;
	return (bucket);
}

/*
 * Check for duplicates, and call __db_ret appropriately.  Release
 * everything held by the cursor.
 */
static int
__ham_dup_return(dbc, val, flags)
	DBC *dbc;
	DBT *val;
	u_int32_t flags;
{
	DB *dbp;
	HASH_CURSOR *hcp;
	PAGE *pp;
	DBT *myval, tmp_val;
	db_indx_t ndx;
	db_pgno_t pgno;
	u_int32_t off, tlen;
	u_int8_t *hk, type;
	int cmp, ret;
	db_indx_t len;

	/* Check for duplicate and return the first one. */
	dbp = dbc->dbp;
	hcp = (HASH_CURSOR *)dbc->internal;
	ndx = H_DATAINDEX(hcp->bndx);
	type = HPAGE_TYPE(hcp->pagep, ndx);
	pp = hcp->pagep;
	myval = val;

	/*
	 * There are 4 cases:
	 * 1. We are not in duplicate, simply call db_ret.
	 * 2. We are looking at keys and stumbled onto a duplicate.
	 * 3. We are in the middle of a duplicate set. (ISDUP set)
	 * 4. This is a duplicate and we need to return a specific item.
	 */

	/*
	 * Here we check for the case where we just stumbled onto a
	 * duplicate.  In this case, we do initialization and then
	 * let the normal duplicate code handle it.
	 */
	if (!F_ISSET(hcp, H_ISDUP)) {
		if (type == H_DUPLICATE) {
			F_SET(hcp, H_ISDUP);
			hcp->dup_tlen = LEN_HDATA(hcp->pagep,
			    hcp->hdr->dbmeta.pagesize, hcp->bndx);
			hk = H_PAIRDATA(hcp->pagep, hcp->bndx);
			if (flags == DB_LAST || flags == DB_PREV) {
				hcp->dndx = 0;
				hcp->dup_off = 0;
				do {
					memcpy(&len,
					    HKEYDATA_DATA(hk) + hcp->dup_off,
					    sizeof(db_indx_t));
					hcp->dup_off += DUP_SIZE(len);
					hcp->dndx++;
				} while (hcp->dup_off < hcp->dup_tlen);
				hcp->dup_off -= DUP_SIZE(len);
				hcp->dndx--;
			} else {
				memcpy(&len,
				    HKEYDATA_DATA(hk), sizeof(db_indx_t));
				hcp->dup_off = 0;
				hcp->dndx = 0;
			}
			hcp->dup_len = len;
		} else if (type == H_OFFDUP) {
			F_SET(hcp, H_ISDUP);
			if (flags == DB_CURRENT) {
				pgno = hcp->dpgno;
				ndx = hcp->dndx;
			} else
				memcpy(&pgno,
				    HOFFDUP_PGNO(P_ENTRY(hcp->pagep, ndx)),
				    sizeof(db_pgno_t));
			if (flags == DB_LAST || flags == DB_PREV) {
				if ((ret = __db_dend(dbc,
				    pgno, &hcp->dpagep)) != 0)
					return (ret);
				hcp->dpgno = PGNO(hcp->dpagep);
				hcp->dndx = NUM_ENT(hcp->dpagep) - 1;
			} else if ((ret = __ham_next_cpage(dbc,
			    pgno, 0, H_ISDUP)) != 0)
				return (ret);
			if (flags == DB_CURRENT)
				hcp->dndx = ndx;
		}
	}

	/*
	 * If we are retrieving a specific key/data pair, then we
	 * may need to adjust the cursor before returning data.
	 */
	if (flags == DB_GET_BOTH) {
		if (F_ISSET(hcp, H_ISDUP)) {
			if (hcp->dpgno != PGNO_INVALID) {
				if ((ret = __db_dsearch(dbc, 0, val,
				    hcp->dpgno, &hcp->dndx, &hcp->dpagep, &cmp))
				    != 0)
					return (ret);
				if (cmp == 0)
					hcp->dpgno = PGNO(hcp->dpagep);
			} else {
				__ham_dsearch(dbc, val, &off, &cmp);
				hcp->dup_off = off;
			}
		} else {
			hk = H_PAIRDATA(hcp->pagep, hcp->bndx);
			if (((HKEYDATA *)hk)->type == H_OFFPAGE) {
				memcpy(&tlen,
				    HOFFPAGE_TLEN(hk), sizeof(u_int32_t));
				memcpy(&pgno,
				    HOFFPAGE_PGNO(hk), sizeof(db_pgno_t));
				if ((ret = __db_moff(dbp, val,
				    pgno, tlen, dbp->dup_compare, &cmp)) != 0)
					return (ret);
			} else {
				/*
				 * We do not zero tmp_val since the comparison
				 * routines may only look at data and size.
				 */
				tmp_val.data = HKEYDATA_DATA(hk);
				tmp_val.size = LEN_HDATA(hcp->pagep,
				    dbp->pgsize, hcp->bndx);
				cmp = dbp->dup_compare == NULL ?
				    __bam_defcmp(&tmp_val, val) :
				    dbp->dup_compare(&tmp_val, val);
			}
		}

		if (cmp != 0)
			return (DB_NOTFOUND);
	}

	/*
	 * Now, everything is initialized, grab a duplicate if
	 * necessary.
	 */
	if (F_ISSET(hcp, H_ISDUP)) {
		if (hcp->dpgno != PGNO_INVALID) {
			pp = hcp->dpagep;
			ndx = hcp->dndx;
		} else {
			/*
			 * Copy the DBT in case we are retrieving into user
			 * memory and we need the parameters for it.  If the
			 * user requested a partial, then we need to adjust
			 * the user's parameters to get the partial of the
			 * duplicate which is itself a partial.
			 */
			memcpy(&tmp_val, val, sizeof(*val));
			if (F_ISSET(&tmp_val, DB_DBT_PARTIAL)) {
				/*
				 * Take the user's length unless it would go
				 * beyond the end of the duplicate.
				 */
				if (tmp_val.doff + hcp->dup_off > hcp->dup_len)
					tmp_val.dlen = 0;
				else if (tmp_val.dlen + tmp_val.doff >
				    hcp->dup_len)
					tmp_val.dlen =
					    hcp->dup_len - tmp_val.doff;

				/*
				 * Calculate the new offset.
				 */
				tmp_val.doff += hcp->dup_off;
			} else {
				F_SET(&tmp_val, DB_DBT_PARTIAL);
				tmp_val.dlen = hcp->dup_len;
				tmp_val.doff = hcp->dup_off + sizeof(db_indx_t);
			}
			myval = &tmp_val;
		}
	}


	/*
	 * Finally, if we had a duplicate, pp, ndx, and myval should be
	 * set appropriately.
	 */
	if ((ret = __db_ret(dbp, pp, ndx, myval, &dbc->rdata.data,
	    &dbc->rdata.size)) != 0)
		return (ret);

	/*
	 * In case we sent a temporary off to db_ret, set the real
	 * return values.
	 */
	val->data = myval->data;
	val->size = myval->size;

	return (0);
}

static int
__ham_overwrite(dbc, nval)
	DBC *dbc;
	DBT *nval;
{
	HASH_CURSOR *hcp;
	DBT *myval, tmp_val, tmp_val2;
	void *newrec;
	u_int8_t *hk, *p;
	u_int32_t len, nondup_size;
	db_pgno_t prev;
	db_indx_t newsize, dndx;
	int ret;

	hcp = (HASH_CURSOR *)dbc->internal;
	if (F_ISSET(hcp, H_ISDUP)) {
		/*
		 * This is an overwrite of a duplicate; check for
		 * onpage versus offpage and whether it's partial.
		 */
		if (hcp->dpagep != NULL) {
do_offpage:		if (F_ISSET(nval, DB_DBT_PARTIAL)) {
				/*
				 * We are using btree routines that are
				 * actually OK for hash to use.  Since all
				 * dbps have bt_internal initialized, this
				 * *should* just work.
				 */
				newsize = __bam_partsize(
				    DB_CURRENT, nval, hcp->dpagep, hcp->dndx);
				memcpy(&tmp_val, nval, sizeof(tmp_val));
				if ((ret =
				   __bam_build(dbc, DB_CURRENT, &tmp_val,
				   hcp->dpagep, hcp->dndx, newsize)) != 0)
					return (ret);
				myval = &tmp_val;
			} else
				myval = nval;

			/*
			 * Make sure that the caller isn't corrupting
			 * the sort order.
			 */
			if (dbc->dbp->dup_compare != NULL &&
			    __bam_cmp(dbc->dbp, myval, hcp->dpagep,
			    hcp->dndx, dbc->dbp->dup_compare) != 0)
				return (EINVAL);

			prev = PREV_PGNO(hcp->dpagep);
			if ((ret =
			    __db_drem(dbc, &hcp->dpagep, hcp->dndx)) != 0)
				return (ret);
			/*
			 * It's possible that hcp->dpagep is now NULL.  If
			 * we have a prev, we can deal pretty easily; if not
			 * this gets ugly.
			 */
			if (hcp->dpagep == NULL) {
				if (prev == PGNO_INVALID) {
					/*
					 * This was a duplicate page with
					 * a single item.  Pretend to reenter
					 * this routine simply overwriting the
					 * entry on the main page.
					 */
					F_CLR(hcp, H_ISDUP);
					goto doreplace;
				}
				if ((ret = __ham_next_cpage(dbc,
				    prev, 0, H_ISDUP)) != 0)
					return (ret);
				hcp->dndx = NUM_ENT(hcp->dpagep);
			}

			/*
			 * On page splits, the 4th parameter of db_dput returns
			 * the location the new item was put.  We cannot pass
			 * in permanent fields from the cursor, they may have
			 * been updated in cursor adjustment.
			 */
			dndx = hcp->dndx;
			ret = __db_dput(dbc, myval, &hcp->dpagep, &dndx);
			hcp->dpgno = PGNO(hcp->dpagep);
			hcp->dndx = dndx;
			return (ret);
		}

		/* On page dups */
		if (F_ISSET(nval, DB_DBT_PARTIAL)) {
			/*
			 * We're going to have to get the current item, then
			 * construct the record, do any padding and do a
			 * replace.
			 */
			memset(&tmp_val, 0, sizeof(tmp_val));
			if ((ret =
			    __ham_dup_return(dbc, &tmp_val, DB_CURRENT)) != 0)
				return (ret);

			/* Figure out new size. */
			nondup_size = tmp_val.size;
			newsize = nondup_size;

			/*
			 * Three cases:
			 * 1. strictly append (may need to allocate space
			 * 	for pad bytes; really gross).
			 * 2. overwrite some and append.
			 * 3. strictly overwrite.
			 */
			if (nval->doff > nondup_size)
				newsize +=
				    (nval->doff - nondup_size + nval->size);
			else if (nval->doff + nval->dlen > nondup_size)
				newsize += nval->size -
				    (nondup_size - nval->doff);
			else
				newsize += nval->size - nval->dlen;

			/*
			 * Make sure that the new size doesn't put us over
			 * the onpage duplicate size in which case we need
			 * to convert to off-page duplicates.
			 */
			if (ISBIG(hcp, hcp->dup_tlen - nondup_size + newsize)) {
				if ((ret = __ham_dup_convert(dbc)) != 0)
					return (ret);
				goto do_offpage;
			}

			if ((ret =
			    __os_malloc(DUP_SIZE(newsize), NULL, &newrec)) != 0)
				return (ret);
			memset(&tmp_val2, 0, sizeof(tmp_val2));
			F_SET(&tmp_val2, DB_DBT_PARTIAL);

			/* Construct the record. */
			p = newrec;
			/* Initial size. */
			memcpy(p, &newsize, sizeof(db_indx_t));
			p += sizeof(db_indx_t);

			/* First part of original record. */
			len = nval->doff > tmp_val.size
			    ? tmp_val.size : nval->doff;
			memcpy(p, tmp_val.data, len);
			p += len;

			if (nval->doff > tmp_val.size) {
				/* Padding */
				memset(p, 0, nval->doff - tmp_val.size);
				p += nval->doff - tmp_val.size;
			}

			/* New bytes */
			memcpy(p, nval->data, nval->size);
			p += nval->size;

			/* End of original record (if there is any) */
			if (nval->doff + nval->dlen < tmp_val.size) {
				len = tmp_val.size - nval->doff - nval->dlen;
				memcpy(p, (u_int8_t *)tmp_val.data +
				    nval->doff + nval->dlen, len);
				p += len;
			}

			/* Final size. */
			memcpy(p, &newsize, sizeof(db_indx_t));

			/*
			 * Make sure that the caller isn't corrupting
			 * the sort order.
			 */
			if (dbc->dbp->dup_compare != NULL) {
				tmp_val2.data =
				    (u_int8_t *)newrec + sizeof(db_indx_t);
				tmp_val2.size = newsize;
				if (dbc->dbp->dup_compare(&tmp_val, &tmp_val2)
				    != 0) {
					(void)__os_free(newrec,
					    DUP_SIZE(newsize));
					return (EINVAL);
				}
			}

			tmp_val2.data = newrec;
			tmp_val2.size = DUP_SIZE(newsize);
			tmp_val2.doff = hcp->dup_off;
			tmp_val2.dlen = DUP_SIZE(hcp->dup_len);

			ret = __ham_replpair(dbc, &tmp_val2, 0);
			(void)__os_free(newrec, DUP_SIZE(newsize));

			/* Update cursor */
			if (ret != 0)
				return (ret);

			if (newsize > nondup_size)
				hcp->dup_tlen += (newsize - nondup_size);
			else
				hcp->dup_tlen -= (nondup_size - newsize);
			hcp->dup_len = DUP_SIZE(newsize);
			return (0);
		} else {
			/* Check whether we need to convert to off page. */
			if (ISBIG(hcp,
			    hcp->dup_tlen - hcp->dup_len + nval->size)) {
				if ((ret = __ham_dup_convert(dbc)) != 0)
					return (ret);
				goto do_offpage;
			}

			/* Make sure we maintain sort order. */
			if (dbc->dbp->dup_compare != NULL) {
				tmp_val2.data =
				    HKEYDATA_DATA(H_PAIRDATA(hcp->pagep,
				    hcp->bndx)) + hcp->dup_off +
				    sizeof(db_indx_t);
				tmp_val2.size = hcp->dup_len;
				if (dbc->dbp->dup_compare(nval, &tmp_val2) != 0)
					return (EINVAL);
			}
			/* Overwriting a complete duplicate. */
			if ((ret = __ham_make_dup(nval, &tmp_val,
			    &dbc->rdata.data, &dbc->rdata.size)) != 0)
				return (ret);
			/* Now fix what we are replacing. */
			tmp_val.doff = hcp->dup_off;
			tmp_val.dlen = DUP_SIZE(hcp->dup_len);

			/* Update cursor */
			if (nval->size > hcp->dup_len)
				hcp->dup_tlen += (nval->size - hcp->dup_len);
			else
				hcp->dup_tlen -= (hcp->dup_len - nval->size);
			hcp->dup_len = DUP_SIZE(nval->size);
		}
		myval = &tmp_val;
	} else if (!F_ISSET(nval, DB_DBT_PARTIAL)) {
		/* Put/overwrite */
		memcpy(&tmp_val, nval, sizeof(*nval));
		F_SET(&tmp_val, DB_DBT_PARTIAL);
		tmp_val.doff = 0;
		hk = H_PAIRDATA(hcp->pagep, hcp->bndx);
		if (HPAGE_PTYPE(hk) == H_OFFPAGE)
			memcpy(&tmp_val.dlen,
			    HOFFPAGE_TLEN(hk), sizeof(u_int32_t));
		else
			tmp_val.dlen = LEN_HDATA(hcp->pagep,
			    hcp->hdr->dbmeta.pagesize,hcp->bndx);
		myval = &tmp_val;
	} else
		/* Regular partial put */
		myval = nval;
doreplace:
	return (__ham_replpair(dbc, myval, 0));
}

/*
 * Given a key and a cursor, sets the cursor to the page/ndx on which
 * the key resides.  If the key is found, the cursor H_OK flag is set
 * and the pagep, bndx, pgno (dpagep, dndx, dpgno) fields are set.
 * If the key is not found, the H_OK flag is not set.  If the sought
 * field is non-0, the pagep, bndx, pgno (dpagep, dndx, dpgno) fields
 * are set indicating where an add might take place.  If it is 0,
 * non of the cursor pointer field are valid.
 */
static int
__ham_lookup(dbc, key, sought, mode)
	DBC *dbc;
	const DBT *key;
	u_int32_t sought;
	db_lockmode_t mode;
{
	DB *dbp;
	HASH_CURSOR *hcp;
	db_pgno_t pgno;
	u_int32_t tlen;
	int match, ret, t_ret;
	u_int8_t *hk;

	dbp = dbc->dbp;
	hcp = (HASH_CURSOR *)dbc->internal;
	/*
	 * Set up cursor so that we're looking for space to add an item
	 * as we cycle through the pages looking for the key.
	 */
	if ((ret = __ham_item_reset(dbc)) != 0)
		return (ret);
	hcp->seek_size = sought;

	hcp->bucket = __ham_call_hash(hcp, (u_int8_t *)key->data, key->size);
	while (1) {
		if ((ret = __ham_item_next(dbc, mode)) != 0)
			return (ret);

		if (F_ISSET(hcp, H_NOMORE))
			break;

		hk = H_PAIRKEY(hcp->pagep, hcp->bndx);
		switch (HPAGE_PTYPE(hk)) {
		case H_OFFPAGE:
			memcpy(&tlen, HOFFPAGE_TLEN(hk), sizeof(u_int32_t));
			if (tlen == key->size) {
				memcpy(&pgno,
				    HOFFPAGE_PGNO(hk), sizeof(db_pgno_t));
				if ((ret = __db_moff(dbp,
				    key, pgno, tlen, NULL, &match)) != 0)
					return (ret);
				if (match == 0) {
					F_SET(hcp, H_OK);
					return (0);
				}
			}
			break;
		case H_KEYDATA:
			if (key->size ==
			    LEN_HKEY(hcp->pagep, dbp->pgsize, hcp->bndx) &&
			    memcmp(key->data,
			    HKEYDATA_DATA(hk), key->size) == 0) {
				F_SET(hcp, H_OK);
				return (0);
			}
			break;
		case H_DUPLICATE:
		case H_OFFDUP:
			/*
			 * These are errors because keys are never
			 * duplicated, only data items are.
			 */
			return (__db_pgfmt(dbp, PGNO(hcp->pagep)));
		}
	}

	/*
	 * Item was not found.
	 */

	if (sought != 0)
		return (ret);

	if ((t_ret = __ham_item_done(dbc, 0)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/*
 * __ham_init_dbt --
 *	Initialize a dbt using some possibly already allocated storage
 *	for items.
 *
 * PUBLIC: int __ham_init_dbt __P((DBT *, u_int32_t, void **, u_int32_t *));
 */
int
__ham_init_dbt(dbt, size, bufp, sizep)
	DBT *dbt;
	u_int32_t size;
	void **bufp;
	u_int32_t *sizep;
{
	int ret;

	memset(dbt, 0, sizeof(*dbt));
	if (*sizep < size) {
		if ((ret = __os_realloc(size, NULL, bufp)) != 0) {
			*sizep = 0;
			return (ret);
		}
		*sizep = size;
	}
	dbt->data = *bufp;
	dbt->size = size;
	return (0);
}

/*
 * Adjust the cursor after an insert or delete.  The cursor passed is
 * the one that was operated upon; we just need to check any of the
 * others.
 *
 * len indicates the length of the item added/deleted
 * add indicates if the item indicated by the cursor has just been
 * added (add == 1) or deleted (add == 0).
 * dup indicates if the addition occurred into a duplicate set.
 *
 * PUBLIC: void __ham_c_update
 * PUBLIC:    __P((HASH_CURSOR *, db_pgno_t, u_int32_t, int, int));
 */
void
__ham_c_update(hcp, chg_pgno, len, add, is_dup)
	HASH_CURSOR *hcp;
	db_pgno_t chg_pgno;
	u_int32_t len;
	int add, is_dup;
{
	DB *dbp;
	DBC *cp;
	HASH_CURSOR *lcp;
	int page_deleted;

	/*
	 * Regular adds are always at the end of a given page, so we never
	 * have to adjust anyone's cursor after a regular add.
	 */
	if (!is_dup && add)
		return;

	/*
	 * Determine if a page was deleted.    If this is a regular update
	 * (i.e., not is_dup) then the deleted page's number will be that in
	 * chg_pgno, and the pgno in the cursor will be different.  If this
	 * was an onpage-duplicate, then the same conditions apply.  If this
	 * was an off-page duplicate, then we need to verify if hcp->dpgno
	 * is the same (no delete) or different (delete) than chg_pgno.
	 */
	if (!is_dup || hcp->dpgno == PGNO_INVALID)
		page_deleted =
		    chg_pgno != PGNO_INVALID && chg_pgno != hcp->pgno;
	else
		page_deleted =
		    chg_pgno != PGNO_INVALID && chg_pgno != hcp->dpgno;

	dbp = hcp->dbc->dbp;
	MUTEX_THREAD_LOCK(dbp->mutexp);

	for (cp = TAILQ_FIRST(&dbp->active_queue); cp != NULL;
	    cp = TAILQ_NEXT(cp, links)) {
		if (cp->internal == hcp)
			continue;

		lcp = (HASH_CURSOR *)cp->internal;

		if (!is_dup && lcp->pgno != chg_pgno)
			continue;

		if (is_dup &&
		    ((lcp->dpgno == PGNO_INVALID && lcp->pgno != chg_pgno) ||
		    (lcp->dpgno != PGNO_INVALID && lcp->dpgno != chg_pgno)))
			continue;

		if (is_dup && F_ISSET(hcp, H_DELETED)) {
			if (lcp->dpgno == PGNO_INVALID) {
				if (lcp->pgno != chg_pgno)
					continue;
			} else if (lcp->dpgno != chg_pgno)
				continue;
		}

		if (page_deleted) {
			if (is_dup) {
				lcp->dpgno = hcp->dpgno;
				lcp->dndx = hcp->dndx;
			} else {
				lcp->pgno = hcp->pgno;
				lcp->bndx = hcp->bndx;
				lcp->bucket = hcp->bucket;
			}
			F_CLR(lcp, H_ISDUP);
			continue;
		}

		if (!is_dup && lcp->bndx > hcp->bndx)
			lcp->bndx--;
		else if (!is_dup && lcp->bndx == hcp->bndx)
			if (add)
				lcp->bndx++;
			else
				F_SET(lcp, H_DELETED);
		else if (is_dup && hcp->dpgno != PGNO_INVALID &&
		    hcp->dpgno == lcp->dpgno) {
			/* Off-page duplicate. */
			if (add && lcp->dndx >= hcp->dndx )
				lcp->dndx++;
			else if (!add && lcp->dndx > hcp->dndx)
				lcp->dndx--;
			else if (!add && lcp->dndx == hcp->dndx)
				F_SET(lcp, H_DELETED);

		} else if (is_dup && lcp->pgno == chg_pgno &&
		    lcp->bndx == hcp->bndx) {
			/* On-page duplicate. */
			if (add) {
				lcp->dup_tlen += len;
				if (lcp->dup_off > hcp->dup_off)
					lcp->dup_off += len;
				if (lcp->dup_off == hcp->dup_off)
					lcp->dup_len = len;
			} else {
				lcp->dup_tlen -= len;
				if (lcp->dup_off > hcp->dup_off)
					lcp->dup_off -= len;
				else if (lcp->dup_off == hcp->dup_off)
					F_SET(lcp, H_DELETED);
			}
		}
	}
	MUTEX_THREAD_UNLOCK(dbp->mutexp);
}

/*
 * __ham_get_clist --
 *
 * Get a list of cursors either on a particular bucket or on a particular
 * page and index combination.  The former is so that we can update
 * cursors on a split.  The latter is so we can update cursors when we
 * move items off page.
 *
 * PUBLIC: int __ham_get_clist __P((DB *,
 * PUBLIC:     db_pgno_t, u_int32_t, HASH_CURSOR ***));
 */
int
__ham_get_clist(dbp, bucket, indx, listp)
	DB *dbp;
	db_pgno_t bucket;
	u_int32_t indx;
	HASH_CURSOR ***listp;
{
	DBC *cp;
	int nalloc, nused, ret;

	/*
	 * Assume that finding anything is the exception, so optimize for
	 * the case where there aren't any.
	 */
	nalloc = nused = 0;
	*listp = NULL;

	MUTEX_THREAD_LOCK(dbp->mutexp);

	for (cp = TAILQ_FIRST(&dbp->active_queue); cp != NULL;
	    cp = TAILQ_NEXT(cp, links))
		if ((indx == NDX_INVALID &&
		    ((HASH_CURSOR *)(cp->internal))->bucket == bucket) ||
		    (indx != NDX_INVALID &&
		    ((HASH_CURSOR *)(cp->internal))->pgno == bucket &&
		    ((HASH_CURSOR *)(cp->internal))->bndx == indx)) {
			if (nused >= nalloc) {
				nalloc += 10;
				if ((ret =
				    __os_realloc(nalloc * sizeof(HASH_CURSOR *),
				    NULL, listp)) != 0)
					return (ret);
			}
			(*listp)[nused++] = (HASH_CURSOR *)cp->internal;
		}

	MUTEX_THREAD_UNLOCK(dbp->mutexp);
	if (listp != NULL) {
		if (nused >= nalloc) {
			nalloc++;
			if ((ret = __os_realloc(nalloc * sizeof(HASH_CURSOR *),
			    NULL, listp)) != 0)
				return (ret);
		}
		(*listp)[nused] = NULL;
	}
	return (0);
}

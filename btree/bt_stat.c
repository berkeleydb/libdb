/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)bt_stat.c	11.4 (Sleepycat) 10/18/99";
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

static int __bam_stat_callback __P((DB *, PAGE *, void *, int *));

/*
 * __bam_stat --
 *	Gather/print the btree statistics
 *
 * PUBLIC: int __bam_stat __P((DB *, void *, void *(*)(size_t), u_int32_t));
 */
int
__bam_stat(dbp, spp, db_malloc, flags)
	DB *dbp;
	void *spp;
	void *(*db_malloc) __P((size_t));
	u_int32_t flags;
{
	BTMETA *meta;
	BTREE *t;
	DBC *dbc;
	DB_BTREE_STAT *sp;
	DB_LOCK lock;
	PAGE *h;
	db_pgno_t pgno;
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);
	DB_ILLEGAL_BEFORE_OPEN(dbp, "DB->stat");

	meta = NULL;
	t = dbp->bt_internal;
	sp = NULL;
	lock.off = LOCK_INVALID;
	h = NULL;
	ret = 0;

	/* Check for invalid flags. */
	if ((ret = __db_statchk(dbp, flags)) != 0)
		return (ret);

	if (spp == NULL)
		return (0);

	/* Acquire a cursor. */
	if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, NULL, "bam_stat", NULL, NULL, flags);

	/* Allocate and clear the structure. */
	if ((ret = __os_malloc(sizeof(*sp), db_malloc, &sp)) != 0)
		goto err;
	memset(sp, 0, sizeof(*sp));

	/* If the app just wants the record count, make it fast. */
	if (flags == DB_RECORDCOUNT) {
		if ((ret =
		    __db_lget(dbc, 0, t->bt_root, DB_LOCK_READ, 0, &lock)) != 0)
			goto err;
		if ((ret =
		    memp_fget(dbp->mpf, &t->bt_root, 0, (PAGE **)&h)) != 0)
			goto err;

		sp->bt_nrecs = RE_NREC(h);

		goto done;
	}

	/* Get the metadata page for the entire database. */
	pgno = PGNO_BASE_MD;
	if ((ret = __db_lget(dbc, 0, pgno, DB_LOCK_READ, 0, &lock)) != 0)
		goto err;
	if ((ret = memp_fget(dbp->mpf, &pgno, 0, (PAGE **)&meta)) != 0)
		goto err;

	/* Walk the metadata free list, counting pages. */
	for (sp->bt_free = 0, pgno = meta->dbmeta.free; pgno != PGNO_INVALID;) {
		++sp->bt_free;

		if ((ret = memp_fget(dbp->mpf, &pgno, 0, &h)) != 0)
			goto err;

		pgno = h->next_pgno;
		if ((ret = memp_fput(dbp->mpf, h, 0)) != 0)
			goto err;
		h = NULL;
	}

	/*
	 * Get the subdatabase metadata page if it's not the same as the
	 * one we already have.
	 */
	if (t->bt_meta != PGNO_BASE_MD) {
		if ((ret = memp_fput(dbp->mpf, meta, 0)) != 0)
			goto err;
		meta = NULL;
		__LPUT(dbc, lock);

		if ((ret =
		    __db_lget(dbc, 0, t->bt_meta, DB_LOCK_READ, 0, &lock)) != 0)
			goto err;
		if ((ret =
		    memp_fget(dbp->mpf, &t->bt_meta, 0, (PAGE **)&meta)) != 0)
			goto err;
	}

	/* Get metadata page statistics. */
	sp->bt_metaflags = meta->dbmeta.flags;
	sp->bt_maxkey = meta->maxkey;
	sp->bt_minkey = meta->minkey;
	sp->bt_re_len = meta->re_len;
	sp->bt_re_pad = meta->re_pad;
	sp->bt_pagesize = meta->dbmeta.pagesize;
	sp->bt_magic = meta->dbmeta.magic;
	sp->bt_version = meta->dbmeta.version;

	/* Discard the metadata page. */
	if ((ret = memp_fput(dbp->mpf, meta, 0)) != 0)
		goto err;
	meta = NULL;
	__LPUT(dbc, lock);

	/* Get the root page. */
	pgno = t->bt_root;
	if ((ret = __db_lget(dbc, 0, pgno, DB_LOCK_READ, 0, &lock)) != 0)
		goto err;
	if ((ret = memp_fget(dbp->mpf, &pgno, 0, &h)) != 0)
		goto err;

	/* Get the levels from the root page. */
	sp->bt_levels = h->level;

	/* Discard the root page. */
	if ((ret = memp_fput(dbp->mpf, h, 0)) != 0)
		goto err;
	h = NULL;
	__LPUT(dbc, lock);

	/* Walk the tree. */
	if ((ret = __bam_traverse(dbc,
	    DB_LOCK_READ, t->bt_root, __bam_stat_callback, sp)) != 0)
		goto err;

done:	*(DB_BTREE_STAT **)spp = sp;

	if (0) {
err:		if (sp != NULL)
			__os_free(sp, sizeof(*sp));
	}

	if (h != NULL &&
	    (t_ret = memp_fput(dbp->mpf, h, 0)) != 0 && ret == 0)
		ret = t_ret;

	if (meta != NULL &&
	    (t_ret = memp_fput(dbp->mpf, meta, 0)) != 0 && ret == 0)
		ret = t_ret;

	if (lock.off != LOCK_INVALID)
		__LPUT(dbc, lock);

	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __bam_traverse --
 *	Walk a Btree database.
 *
 * PUBLIC: int __bam_traverse __P((DBC *, db_lockmode_t,
 * PUBLIC:     db_pgno_t, int (*)(DB *, PAGE *, void *, int *), void *));
 */
int
__bam_traverse(dbc, mode, root_pgno, callback, cookie)
	DBC *dbc;
	db_lockmode_t mode;
	db_pgno_t root_pgno;
	int (*callback)__P((DB *, PAGE *, void *, int *));
	void *cookie;
{
	BINTERNAL *bi;
	BOVERFLOW *bo;
	DB *dbp;
	DB_LOCK lock;
	PAGE *h;
	RINTERNAL *ri;
	int already_put, i, ret, t_ret;

	dbp = dbc->dbp;

	if ((ret = __db_lget(dbc, 0, root_pgno, mode, 0, &lock)) != 0)
		return (ret);
	if ((ret = memp_fget(dbp->mpf, &root_pgno, 0, &h)) != 0)
		goto err;

	switch (TYPE(h)) {
	case P_IBTREE:
		for (i = 0; i < NUM_ENT(h); i++) {
			bi = GET_BINTERNAL(h, i);
			if ((ret = __bam_traverse(dbc,
			    mode, bi->pgno, callback, cookie)) != 0)
				break;

			switch (B_TYPE(bi->type)) {
			case B_DUPLICATE:
				bo = (BOVERFLOW *)bi->data;
				if ((ret = __db_traverse_big(
				    dbp, bo->pgno, callback, cookie)) != 0)
					goto err;
				break;
			case B_OVERFLOW:
				bo = (BOVERFLOW *)bi->data;
				if ((ret = __db_traverse_dup(
				    dbp, bo->pgno, callback, cookie)) != 0)
					goto err;
				break;
			case B_KEYDATA:
				break;
			default:
				goto pgerr;
			}
		}
		break;
	case P_IRECNO:
		for (i = 0; i < NUM_ENT(h); i++) {
			ri = GET_RINTERNAL(h, i);
			if ((ret = __bam_traverse(
			    dbc, mode, ri->pgno, callback, cookie)) != 0)
				break;
		}
		break;
	case P_DUPLICATE:
	case P_LBTREE:
	case P_LRECNO:
	case P_OVERFLOW:
		break;
	default:
pgerr:		return (__db_pgfmt(dbc->dbp, h->pgno));
	}

	already_put = 0;
	if ((ret = callback(dbp, h, cookie, &already_put)) != 0)
		goto err;

err:	if (!already_put &&
	    (t_ret = memp_fput(dbp->mpf, h, 0)) != 0 && ret != 0)
		ret = t_ret;
	__LPUT(dbc, lock);

	return (ret);
}

/*
 * __bam_stat_callback --
 *	Statistics callback.
 */
static int
__bam_stat_callback(dbp, h, cookie, putp)
	DB *dbp;
	PAGE *h;
	void *cookie;
	int *putp;
{
	DB_BTREE_STAT *sp;

	*putp = 0;
	sp = cookie;

	switch (TYPE(h)) {
	case P_IBTREE:
	case P_IRECNO:
		++sp->bt_int_pg;
		sp->bt_int_pgfree += P_FREESPACE(h);
		break;
	case P_LBTREE:
		++sp->bt_leaf_pg;
		sp->bt_leaf_pgfree += P_FREESPACE(h);
		sp->bt_nrecs += NUM_ENT(h) / P_INDX;
		break;
	case P_LRECNO:
		++sp->bt_leaf_pg;
		sp->bt_leaf_pgfree += P_FREESPACE(h);
		sp->bt_nrecs += NUM_ENT(h);
		break;
	case P_DUPLICATE:
		++sp->bt_dup_pg;
		sp->bt_dup_pgfree += P_FREESPACE(h);
		break;
	case P_OVERFLOW:
		++sp->bt_over_pg;
		sp->bt_over_pgfree += P_OVFLSPACE(dbp->pgsize, h);
		break;
	default:
		return (__db_pgfmt(dbp, h->pgno));
	}
	return (0);
}

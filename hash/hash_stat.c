/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)hash_stat.c	11.5 (Sleepycat) 9/10/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_shash.h"
#include "hash.h"
#include "lock.h"

static int __ham_stat_callback __P((DB *, PAGE *, void *, int *));

/*
 * __ham_stat --
 *	Gather/print the hash statistics
 *
 * PUBLIC: int __ham_stat __P((DB *, void *, void *(*)(size_t), u_int32_t));
 */
int
__ham_stat(dbp, spp, db_malloc, flags)
	DB *dbp;
	void *spp, *(*db_malloc) __P((size_t));
	u_int32_t flags;
{
	DB_HASH_STAT *sp;
	HASH_CURSOR *hcp;
	DBC *dbc;
	PAGE *h;
	db_pgno_t pgno;
	int ret;

	PANIC_CHECK(dbp->dbenv);
	DB_ILLEGAL_BEFORE_OPEN(dbp, "DB->stat");

	sp = NULL;

	/* Check for invalid flags. */
	if ((ret = __db_statchk(dbp, flags)) != 0)
		return (ret);

	if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0)
		return (ret);
	hcp = (HASH_CURSOR *)dbc->internal;

	if ((ret = __ham_get_meta(dbc)) != 0)
		goto err;

	/* Allocate and clear the structure. */
	if ((ret = __os_malloc(sizeof(*sp), db_malloc, &sp)) != 0)
		goto err;
	memset(sp, 0, sizeof(*sp));

	/* Copy the fields that we have. */
	sp->hash_pagesize = dbp->pgsize;
	sp->hash_buckets = hcp->hdr->max_bucket + 1;
	sp->hash_magic = hcp->hdr->dbmeta.magic;
	sp->hash_version = hcp->hdr->dbmeta.version;
	sp->hash_metaflags = hcp->hdr->dbmeta.flags;
	sp->hash_nelem = hcp->hdr->nelem;
	sp->hash_ffactor = hcp->hdr->ffactor;

	/* Walk the free list, counting pages. */
	for (sp->hash_free = 0, pgno = hcp->hdr->dbmeta.free;
	    pgno != PGNO_INVALID;) {
		++sp->hash_free;

		if ((ret = memp_fget(dbp->mpf, &pgno, 0, &h)) != 0)
			goto err;

		pgno = h->next_pgno;
		(void)memp_fput(dbp->mpf, h, 0);
	}

	/* Now traverse the rest of the table. */
	if ((ret = __ham_traverse(dbp,
	    dbc, DB_LOCK_READ, __ham_stat_callback, sp)) != 0)
		goto err;
	if ((ret = dbc->c_close(dbc)) != 0)
		goto err;
	if ((ret = __ham_release_meta(dbc)) != 0)
		goto err;

	*(DB_HASH_STAT **)spp = sp;
	return (0);

err:	if (sp != NULL)
		__os_free(sp, sizeof(*sp));
	if (hcp->hdr != NULL)
		(void)__ham_release_meta(dbc);
	(void)dbc->c_close(dbc);
	return (ret);

}

/*
 * __ham_traverse
 *	 Traverse an entire hash table.  We use the callback so that we
 * can use this both for stat collection and for deallocation.
 *
 * PUBLIC:  int __ham_traverse __P((DB *, DBC *, db_lockmode_t,
 * PUBLIC:     int (*)(DB *, PAGE *, void *, int *), void *));
 */
int
__ham_traverse(dbp, dbc, mode, callback, cookie)
	DB *dbp;
	DBC *dbc;
	db_lockmode_t mode;
	int (*callback) __P((DB *, PAGE *, void *, int *));
	void *cookie;
{
	HASH_CURSOR *hcp;
	HKEYDATA *hk;
	db_pgno_t pgno, opgno;
	u_int32_t bucket;
	int did_put, i, ret;

	hcp = (HASH_CURSOR *)dbc->internal;

	/*
	 * In a perfect world, we could simply read each page in the file
	 * and look at its page type to tally the information necessary.
	 * Unfortunately, the bucket locking that hash tables do to make
	 * locking easy, makes this a pain in the butt.  We have to traverse
	 * duplicate, overflow and big pages from the bucket so that we
	 * don't access anything that isn't properly locked.
	 */
	for (bucket = 0; bucket <= hcp->hdr->max_bucket; bucket++) {
		hcp->bucket = bucket;
		pgno = __bucket_to_page(hcp, bucket);
		for (ret = __ham_get_cpage(dbc, mode); ret == 0;
		    ret = __ham_next_cpage(dbc, pgno, 0, 0)) {
			pgno = NEXT_PGNO(hcp->pagep);

			/*
			 * Go through each item on the page checking for
			 * duplicates (in which case we have to count the
			 * duplicate pages) or big key/data items (in which
			 * case we have to count those pages).
			 */
			for (i = 0; i < NUM_ENT(hcp->pagep); i++) {
				hk = (HKEYDATA *)P_ENTRY(hcp->pagep, i);
				switch (HPAGE_PTYPE(hk)) {
				case H_OFFDUP:
					memcpy(&opgno, HOFFDUP_PGNO(hk),
					    sizeof(db_pgno_t));
					if ((ret = __db_traverse_dup(dbp,
					    opgno, callback, cookie))
					    != 0)
						return (ret);
					break;
				case H_OFFPAGE:
					/*
					 * We are about to get a big page
					 * which will use the same spot that
					 * the current page uses, so we need
					 * to restore the current page before
					 * looking at it again.
					 */
					memcpy(&opgno, HOFFPAGE_PGNO(hk),
					    sizeof(db_pgno_t));
					ret = __db_traverse_big(dbp,
					    opgno, callback, cookie);
					if (ret != 0)
						return (ret);
					break;
				case H_DUPLICATE:
				case H_KEYDATA:
					break;
				}
			}

			/* Call the callback on main pages. */
			if ((ret = callback(dbp,
			    hcp->pagep, cookie, &did_put)) != 0)
				return (ret);

			if (did_put)
				hcp->pagep = NULL;
			if (pgno == PGNO_INVALID)
				break;
		}
		if (ret != 0)
			return (ret);

		if (F_ISSET(dbp->dbenv, DB_ENV_LOCKING))
			(void)lock_put(dbp->dbenv, &hcp->lock);
	}

	return (0);
}

static int
__ham_stat_callback(dbp, pagep, cookie, putp)
	DB *dbp;
	PAGE *pagep;
	void *cookie;
	int *putp;
{
	DB_HASH_STAT *sp;

	*putp = 0;
	sp = cookie;

	switch (pagep->type) {
	case P_DUPLICATE:
		sp->hash_dup++;
		sp->hash_dup_free += P_FREESPACE(pagep);
		break;
	case P_OVERFLOW:
		sp->hash_bigpages++;
		sp->hash_big_bfree += P_OVFLSPACE(dbp->pgsize, pagep);
		break;
	case P_HASH:
		/*
		 * We count the buckets and the overflow pages
		 * separately and tally their bytes separately
		 * as well.  We need to figure out if this page
		 * is a bucket.
		 */
		if (PREV_PGNO(pagep) == PGNO_INVALID)
			sp->hash_bfree += P_FREESPACE(pagep);
		else {
			sp->hash_overflows++;
			sp->hash_ovfl_free += P_FREESPACE(pagep);
		}
		sp->hash_nrecs += H_NUMPAIRS(pagep);
		break;
	default:
		return (EINVAL);
	}

	return (0);
}

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
static const char sccsid[] = "@(#)bt_put.c	11.20 (Sleepycat) 10/28/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "btree.h"

static int __bam_ndup __P((DBC *, PAGE *, u_int32_t));
static int __bam_ovput __P((DBC *, PAGE *, u_int32_t, DBT *));

/*
 * __bam_iitem --
 *	Insert an item into the tree.
 *
 * PUBLIC: int __bam_iitem __P((DBC *,
 * PUBLIC:    PAGE **, db_indx_t *, DBT *, DBT *, u_int32_t, u_int32_t));
 */
int
__bam_iitem(dbc, hp, indxp, key, data, op, flags)
	DBC *dbc;
	PAGE **hp;
	db_indx_t *indxp;
	DBT *key, *data;
	u_int32_t op, flags;
{
	BKEYDATA *bk;
	BTREE *t;
	BTREE_CURSOR *cp;
	DB *dbp;
	DBT tdbt;
	PAGE *h;
	db_indx_t indx;
	db_pgno_t pgno;
	u_int32_t data_size, have_bytes, need_bytes, needed;
	int bigkey, bigdata, dupadjust, padrec, replace, ret, was_deleted;

	COMPQUIET(bk, NULL);

	dbp = dbc->dbp;
	t = dbp->bt_internal;
	h = *hp;
	indx = *indxp;
	dupadjust = replace = was_deleted = 0;

	/*
	 * Fixed-length records with partial puts: it's an error to specify
	 * anything other simple overwrite.
	 */
	if (F_ISSET(dbp, DB_RE_FIXEDLEN) &&
	    F_ISSET(data, DB_DBT_PARTIAL) && data->dlen != data->size)
		return (EINVAL);

	/*
	 * Figure out how much space the data will take, including if it's a
	 * partial record.
	 *
	 * Fixed-length records: it's an error to specify a record that's
	 * longer than the fixed-length, and we never require less than
	 * the fixed-length record size.
	 */
	data_size = F_ISSET(data, DB_DBT_PARTIAL) ?
	    __bam_partsize(op, data, h, indx) : data->size;
	padrec = 0;
	if (F_ISSET(dbp, DB_RE_FIXEDLEN)) {
		if (data_size > t->re_len)
			return (EINVAL);
		if (data_size < t->re_len) {
			padrec = 1;
			data_size = t->re_len;
		}
	}

	/*
	 * Handle partial puts or short fixed-length records: build the
	 * real record.
	 *
	 * XXX
	 * I'd much rather wait until after we figure out if we need to do
	 * a split or not, but there are currently too many places that need
	 * the real record before we get there.  Revisit this decision after
	 * we move off-page duplicates into their own Btree.
	 */
	if (padrec || F_ISSET(data, DB_DBT_PARTIAL)) {
		tdbt = *data;
		if ((ret =
		    __bam_build(dbc, op, &tdbt, h, indx, data_size)) != 0)
			return (ret);
		data = &tdbt;
	}

	/*
	 * If the user has specified a duplicate comparison function, return
	 * an error if DB_CURRENT was specified and the replacement data
	 * doesn't compare equal to the current data.  This stops apps from
	 * screwing up the duplicate sort order.  We have to do this after
	 * we build the real record so that we're comparing the real items.
	 */
	if (op == DB_CURRENT && dbp->dup_compare != NULL &&
	    __bam_cmp(dbp, data, h,
	    indx + (TYPE(h) == P_LBTREE ? O_INDX : 0), dbp->dup_compare) != 0)
		return (EINVAL);

	/*
	 * If it's a page of duplicates, call the common code to do the work.
	 *
	 * !!!
	 * Here's where hp and indxp are important.  The duplicate code may
	 * decide to rework/rearrange the pages and indices we're using, so
	 * the caller must understand that the page stack may change.
	 */
	if (TYPE(h) == P_DUPLICATE) {
		/* If appending a new entry adjust the index for the item. */
		if (op == DB_AFTER || op == DB_CURRENT)
			++*indxp;

		/*
		 * Put the new/replacement item onto the page.
		 *
		 * !!!
		 * *hp and *indxp may be changed after the return.
		 */
		if ((ret = __db_dput(dbc, data, hp, indxp)) != 0)
			return (ret);

		/*
		 * XXX
		 * If this is CURRENT, we do an append followed by a delete,
		 * because the underlying duplicate code doesn't support the
		 * replace operation.  The tricky part is to make sure we
		 * delete the proper row.  The append may have caused the row
		 * to move, in which case, the cursor will be updated to point
		 * at it.  This code ASSUMES that the cursor passed in is
		 * pointing at the current record.
		 */
		cp = dbc->internal;
		if (op == DB_CURRENT) {
			/*
			 * The append may have allocated a new page, in which
			 * case it discarded the page we held -- re-acquire
			 * that page.
			 */
			if (PGNO(*hp) != cp->dpgno) {
				if ((ret = memp_fget(
				    dbp->mpf, &cp->dpgno, 0,  &h)) != 0)
					return (ret);
			} else
				h = *hp;

			/* Delete the original item. */
			if ((ret = __db_drem(dbc, &h, cp->dindx)) != 0)
				return (ret);

			/*
			 * Clear the deleted flag on any cursors referencing
			 * the item.
			 */
			(void)__bam_ca_delete(dbp, cp->dpgno, cp->dindx, 0);

			/*
			 * If the insert and delete are on different pages, we
			 * have to adjust cursors on both pages.
			 */
			if (PGNO(*hp) != cp->dpgno) {
				indx = cp->dindx;
				pgno = cp->dpgno;
				__bam_ca_di(dbp, PGNO(*hp), *indxp, 1);
				__bam_ca_repl(dbp,
				    pgno, indx, PGNO(*hp), *indxp);
				__bam_ca_di(dbp, pgno, indx + 1, -1);

				if ((ret = memp_fput(
				    dbp->mpf, h, DB_MPOOL_DIRTY)) != 0)
					return (ret);
			}
		} else {
			h = *hp;
			indx = *indxp;
			__bam_ca_di(dbp, PGNO(h), indx, 1);
			cp->dindx = indx;
			cp->dpgno = PGNO(h);
		}
		goto done;
	}

	/*
	 * If the key or data item won't fit on a page, we'll have to store
	 * them on overflow pages.
	 */
	needed = 0;
	bigdata = data_size > t->bt_ovflsize;
	switch (op) {
	case DB_KEYFIRST:
		/* We're adding a new key and data pair. */
		bigkey = key->size > t->bt_ovflsize;
		if (bigkey)
			needed += BOVERFLOW_PSIZE;
		else
			needed += BKEYDATA_PSIZE(key->size);
		if (bigdata)
			needed += BOVERFLOW_PSIZE;
		else
			needed += BKEYDATA_PSIZE(data_size);
		break;
	case DB_AFTER:
	case DB_BEFORE:
	case DB_CURRENT:
		/*
		 * We're either overwriting the data item of a key/data pair
		 * or we're adding the data item only, i.e. a new duplicate.
		 */
		bigkey = 0;
		if (op == DB_CURRENT) {
			bk = GET_BKEYDATA(h,
			    indx + (TYPE(h) == P_LBTREE ? O_INDX : 0));
			if (B_TYPE(bk->type) == B_KEYDATA)
				have_bytes = BKEYDATA_PSIZE(bk->len);
			else
				have_bytes = BOVERFLOW_PSIZE;
			need_bytes = 0;
		} else {
			have_bytes = 0;
			need_bytes = sizeof(db_indx_t);
		}
		if (bigdata)
			need_bytes += BOVERFLOW_PSIZE;
		else
			need_bytes += BKEYDATA_PSIZE(data_size);

		if (have_bytes < need_bytes)
			needed += need_bytes - have_bytes;
		break;
	default:
		return (EINVAL);
	}

	/*
	 * If there's not enough room, or the user has put a ceiling on the
	 * number of keys permitted in the page, split the page.
	 *
	 * XXX
	 * The t->bt_maxkey test here may be insufficient -- do we have to
	 * check in the btree split code, so we don't undo it there!?!?
	 */
	if (P_FREESPACE(h) < needed ||
	    (t->bt_maxkey != 0 && NUM_ENT(h) > t->bt_maxkey))
		return (DB_NEEDSPLIT);

	/*
	 * The code breaks it up into five cases:
	 *
	 * 1. Insert a new key/data pair.
	 * 2. Append a new data item (a new duplicate).
	 * 3. Insert a new data item (a new duplicate).
	 * 4. Delete and re-add the data item (overflow item).
	 * 5. Overwrite the data item.
	 */
	switch (op) {
	case DB_KEYFIRST:		/* 1. Insert a new key/data pair. */
		if (bigkey) {
			if ((ret = __bam_ovput(dbc, h, indx, key)) != 0)
				return (ret);
		} else
			if ((ret = __db_pitem(dbc, h, indx,
			    BKEYDATA_SIZE(key->size), NULL, key)) != 0)
				return (ret);

		__bam_ca_di(dbp, PGNO(h), indx, 1);
		++indx;
		break;
	case DB_AFTER:			/* 2. Append a new data item. */
		if (TYPE(h) == P_LBTREE) {
			/*
			 * Adjust the cursor and copy in the key for the
			 * duplicate.
			 */
			if ((ret =
			    __bam_adjindx(dbc, h, indx + P_INDX, indx, 1)) != 0)
				return (ret);

			indx += 3;
			dupadjust = 1;

			*indxp += 2;
		} else {
			++indx;
			__bam_ca_di(dbp, PGNO(h), indx, 1);

			*indxp += 1;
		}
		break;
	case DB_BEFORE:			/* 3. Insert a new data item. */
		if (TYPE(h) == P_LBTREE) {
			/*
			 * Adjust the cursor and copy in the key for the
			 * duplicate.
			 */
			if ((ret = __bam_adjindx(dbc, h, indx, indx, 1)) != 0)
				return (ret);

			++indx;
			dupadjust = 1;
		} else
			__bam_ca_di(dbp, PGNO(h), indx, 1);
		break;
	case DB_CURRENT:
		if (TYPE(h) == P_LBTREE) {
			++indx;
			dupadjust = 1;

			/*
			 * In a Btree deleted records aren't counted (deleted
			 * records are counted in a Recno because all accesses
			 * are based on record number).  If it's a Btree and
			 * it's a DB_CURRENT operation overwriting a previously
			 * deleted record, increment the record count.
			 */
			was_deleted = B_DISSET(bk->type);
		}

		/*
		 * 4. Delete and re-add the data item.
		 *
		 * If we're dealing with offpage items, we have to delete and
		 * then re-add the item.
		 */
		if (bigdata || B_TYPE(bk->type) != B_KEYDATA) {
			if ((ret = __bam_ditem(dbc, h, indx)) != 0)
				return (ret);
			break;
		}

		/* 5. Overwrite the data item. */
		replace = 1;
		break;
	default:
		return (EINVAL);
	}

	/* Add the data. */
	if (bigdata) {
		if ((ret = __bam_ovput(dbc, h, indx, data)) != 0)
			return (ret);
	} else {
		BKEYDATA __bk;
		DBT __hdr;

		if (LF_ISSET(BI_DELETED)) {
			B_TSET(__bk.type, B_KEYDATA, 1);
			__bk.len = data->size;
			__hdr.data = &__bk;
			__hdr.size = SSZA(BKEYDATA, data);
			ret = __db_pitem(dbc, h, indx,
			    BKEYDATA_SIZE(data->size), &__hdr, data);
		} else if (replace)
			ret = __bam_ritem(dbc, h, indx, data);
		else
			ret = __db_pitem(dbc, h, indx,
			    BKEYDATA_SIZE(data->size), NULL, data);
		if (ret != 0)
			return (ret);
	}
	if ((ret = memp_fset(dbp->mpf, h, DB_MPOOL_DIRTY)) != 0)
		return (ret);

	/*
	 * Adjust the cursors in general.  After that's done, reset the current
	 * cursor to point to the new item.
	 */
	if (op == DB_CURRENT)
		(void)__bam_ca_delete(dbp, PGNO(h),
		    TYPE(h) == P_LBTREE ? indx - O_INDX : indx, 0);
	else {
		__bam_ca_di(dbp, PGNO(h), indx, 1);
		((BTREE_CURSOR *)dbc->internal)->indx =
		    TYPE(h) == P_LBTREE ? indx - O_INDX : indx;
	}

	/*
	 * If we've changed the record count, update the tree.  Record counts
	 * need to be updated in Recno databases and in Btree databases where
	 * we are supporting records.  In both cases, adjust the count if the
	 * operation wasn't performed on the current record or when the record
	 * was previously deleted.
	 */
	if ((op != DB_CURRENT || was_deleted) &&
	    (F_ISSET(dbp, DB_BT_RECNUM) || dbp->type == DB_RECNO))
		if ((ret = __bam_adjust(dbc, 1)) != 0)
			return (ret);

	/*
	 * If a Btree leaf page is at least 50% full and we may have added or
	 * modified a duplicate data item, see if the set of duplicates takes
	 * up at least 25% of the space on the page.  If it does, move it off
	 * int its own page.
	 */
	if (dupadjust && P_FREESPACE(h) <= dbp->pgsize / 2) {
		--indx;
		if ((ret = __bam_ndup(dbc, h, indx)) != 0)
			return (ret);
	}

	/* If we've modified a recno file, set the flag. */
done:	if (dbp->type == DB_RECNO)
		F_SET(t, RECNO_MODIFIED);

	return (ret);
}

/*
 * __bam_partsize --
 *	Figure out how much space a partial data item is in total.
 *
 * PUBLIC: u_int32_t __bam_partsize __P((u_int32_t, DBT *, PAGE *, u_int32_t));
 */
u_int32_t
__bam_partsize(op, data, h, indx)
	u_int32_t op, indx;
	DBT *data;
	PAGE *h;
{
	BKEYDATA *bk;
	u_int32_t nbytes;

	/*
	 * If the record doesn't already exist, it's simply the data we're
	 * provided.
	 */
	if (op != DB_CURRENT)
		return (data->doff + data->size);

	/*
	 * Otherwise, it's the data provided plus any already existing data
	 * that we're not replacing.
	 */
	bk = GET_BKEYDATA(h, indx + (TYPE(h) == P_LBTREE ? O_INDX : 0));
	nbytes =
	    B_TYPE(bk->type) == B_OVERFLOW ? ((BOVERFLOW *)bk)->tlen : bk->len;

	/*
	 * There are really two cases here:
	 *
	 * Case 1: We are replacing some bytes that do not exist (i.e., they
	 * are past the end of the record).  In this case the number of bytes
	 * we are replacing is irrelevant and all we care about is how many
	 * bytes we are going to add from offset.  So, the new record length
	 * is going to be the size of the new bytes (size) plus wherever those
	 * new bytes begin (doff).
	 *
	 * Case 2: All the bytes we are replacing exist.  Therefore, the new
	 * size is the oldsize (nbytes) minus the bytes we are replacing (dlen)
	 * plus the bytes we are adding (size).
	 */
	if (nbytes < data->doff + data->dlen)		/* Case 1 */
		return (data->doff + data->size);

	return (nbytes + data->size - data->dlen);	/* Case 2 */
}

/*
 * __bam_build --
 *	Build the real record for a partial put, or short fixed-length record.
 *
 * PUBLIC: int __bam_build __P((DBC *, u_int32_t,
 * PUBLIC:     DBT *, PAGE *, u_int32_t, u_int32_t));
 */
int
__bam_build(dbc, op, dbt, h, indx, nbytes)
	DBC *dbc;
	u_int32_t op, indx, nbytes;
	DBT *dbt;
	PAGE *h;
{
	BKEYDATA *bk, tbk;
	BOVERFLOW *bo;
	BTREE *t;
	DB *dbp;
	DBT copy;
	u_int32_t len, tlen;
	u_int8_t *p;
	int ret;

	COMPQUIET(bo, NULL);

	dbp = dbc->dbp;
	t = dbp->bt_internal;

	/* We use the record data return memory, it's only a short-term use. */
	if (dbc->rdata.ulen < nbytes) {
		 if ((ret =
		     __os_realloc(nbytes, NULL, &dbc->rdata.data)) != 0) {
			dbc->rdata.ulen = 0;
			dbc->rdata.data = NULL;
			return (ret);
		}
		dbc->rdata.ulen = nbytes;
	}

	/*
	 * We use nul or pad bytes for any part of the record that isn't
	 * specified; get it over with.
	 */
	memset(dbc->rdata.data,
	   F_ISSET(dbp, DB_RE_FIXEDLEN) ? t->re_pad : 0, nbytes);

	/*
	 * In the next clauses, we need to do three things: a) set p to point
	 * to the place at which to copy the user's data, b) set tlen to the
	 * total length of the record, not including the bytes contributed by
	 * the user, and c) copy any valid data from an existing record.  If
	 * it's not a partial put (this code is called for both partial puts
	 * and fixed-length record padding) or it's a new key, we can cut to
	 * the chase.
	 */
	if (!F_ISSET(dbt, DB_DBT_PARTIAL) || op != DB_CURRENT) {
		p = (u_int8_t *)dbc->rdata.data + dbt->doff;
		tlen = dbt->doff;
		goto user_copy;
	}

	/* Find the current record. */
	if (indx < NUM_ENT(h)) {
		bk = GET_BKEYDATA(h, indx + (TYPE(h) == P_LBTREE ? O_INDX : 0));
		bo = (BOVERFLOW *)bk;
	} else {
		bk = &tbk;
		B_TSET(bk->type, B_KEYDATA, 0);
		bk->len = 0;
	}
	if (B_TYPE(bk->type) == B_OVERFLOW) {
		/*
		 * In the case of an overflow record, we shift things around
		 * in the current record rather than allocate a separate copy.
		 */
		memset(&copy, 0, sizeof(copy));
		if ((ret = __db_goff(dbp, &copy, bo->tlen,
		    bo->pgno, &dbc->rdata.data, &dbc->rdata.ulen)) != 0)
			return (ret);

		/* Skip any leading data from the original record. */
		tlen = dbt->doff;
		p = (u_int8_t *)dbc->rdata.data + dbt->doff;

		/*
		 * Copy in any trailing data from the original record.
		 *
		 * If the original record was larger than the original offset
		 * plus the bytes being deleted, there is trailing data in the
		 * original record we need to preserve.  If we aren't deleting
		 * the same number of bytes as we're inserting, copy it up or
		 * down, into place.
		 *
		 * Use memmove(), the regions may overlap.
		 */
		if (bo->tlen > dbt->doff + dbt->dlen) {
			len = bo->tlen - (dbt->doff + dbt->dlen);
			if (dbt->dlen != dbt->size)
				memmove(p + dbt->size, p + dbt->dlen, len);
			tlen += len;
		}
	} else {
		/* Copy in any leading data from the original record. */
		memcpy(dbc->rdata.data,
		    bk->data, dbt->doff > bk->len ? bk->len : dbt->doff);
		tlen = dbt->doff;
		p = (u_int8_t *)dbc->rdata.data + dbt->doff;

		/* Copy in any trailing data from the original record. */
		len = dbt->doff + dbt->dlen;
		if (bk->len > len) {
			memcpy(p + dbt->size, bk->data + len, bk->len - len);
			tlen += bk->len - len;
		}
	}

user_copy:
	/*
	 * Copy in the application provided data -- p and tlen must have been
	 * initialized above.
	 */
	memcpy(p, dbt->data, dbt->size);
	tlen += dbt->size;

	/* Set the DBT to reference our new record. */
	dbc->rdata.size = F_ISSET(dbp, DB_RE_FIXEDLEN) ? t->re_len : tlen;
	dbc->rdata.dlen = 0;
	dbc->rdata.doff = 0;
	dbc->rdata.flags = 0;
	*dbt = dbc->rdata;
	return (0);
}

/*
 * OVPUT --
 *	Copy an overflow item onto a page.
 */
#undef	OVPUT
#define	OVPUT(h, indx, bo) do {						\
	DBT __hdr;							\
	memset(&__hdr, 0, sizeof(__hdr));				\
	__hdr.data = &bo;						\
	__hdr.size = BOVERFLOW_SIZE;					\
	if ((ret = __db_pitem(dbc,					\
	    h, indx, BOVERFLOW_SIZE, &__hdr, NULL)) != 0)		\
		return (ret);						\
} while (0)

/*
 * __bam_ovput --
 *	Build an overflow item and put it on the page.
 */
static int
__bam_ovput(dbc, h, indx, item)
	DBC *dbc;
	PAGE *h;
	u_int32_t indx;
	DBT *item;
{
	BOVERFLOW bo;
	int ret;

	UMRW(bo.unused1);
	B_TSET(bo.type, B_OVERFLOW, 0);
	UMRW(bo.unused2);
	if ((ret = __db_poff(dbc, item, &bo.pgno)) != 0)
		return (ret);
	bo.tlen = item->size;

	OVPUT(h, indx, bo);

	return (0);
}

/*
 * __bam_ritem --
 *	Replace an item on a page.
 *
 * PUBLIC: int __bam_ritem __P((DBC *, PAGE *, u_int32_t, DBT *));
 */
int
__bam_ritem(dbc, h, indx, data)
	DBC *dbc;
	PAGE *h;
	u_int32_t indx;
	DBT *data;
{
	BKEYDATA *bk;
	DB *dbp;
	DBT orig, repl;
	db_indx_t cnt, lo, ln, min, off, prefix, suffix;
	int32_t nbytes;
	int ret;
	u_int8_t *p, *t;

	dbp = dbc->dbp;

	/*
	 * Replace a single item onto a page.  The logic figuring out where
	 * to insert and whether it fits is handled in the caller.  All we do
	 * here is manage the page shuffling.
	 */
	bk = GET_BKEYDATA(h, indx);

	/* Log the change. */
	if (DB_LOGGING(dbc)) {
		/*
		 * We might as well check to see if the two data items share
		 * a common prefix and suffix -- it can save us a lot of log
		 * message if they're large.
		 */
		min = data->size < bk->len ? data->size : bk->len;
		for (prefix = 0,
		    p = bk->data, t = data->data;
		    prefix < min && *p == *t; ++prefix, ++p, ++t)
			;

		min -= prefix;
		for (suffix = 0,
		    p = (u_int8_t *)bk->data + bk->len - 1,
		    t = (u_int8_t *)data->data + data->size - 1;
		    suffix < min && *p == *t; ++suffix, --p, --t)
			;

		/* We only log the parts of the keys that have changed. */
		orig.data = (u_int8_t *)bk->data + prefix;
		orig.size = bk->len - (prefix + suffix);
		repl.data = (u_int8_t *)data->data + prefix;
		repl.size = data->size - (prefix + suffix);
		if ((ret = __bam_repl_log(dbp->dbenv, dbc->txn,
		    &LSN(h), 0, dbp->log_fileid, PGNO(h), &LSN(h),
		    (u_int32_t)indx, (u_int32_t)B_DISSET(bk->type),
		    &orig, &repl, (u_int32_t)prefix, (u_int32_t)suffix)) != 0)
			return (ret);
	}

	/*
	 * Set references to the first in-use byte on the page and the
	 * first byte of the item being replaced.
	 */
	p = (u_int8_t *)h + HOFFSET(h);
	t = (u_int8_t *)bk;

	/*
	 * If the entry is growing in size, shift the beginning of the data
	 * part of the page down.  If the entry is shrinking in size, shift
	 * the beginning of the data part of the page up.  Use memmove(3),
	 * the regions overlap.
	 */
	lo = BKEYDATA_SIZE(bk->len);
	ln = BKEYDATA_SIZE(data->size);
	if (lo != ln) {
		nbytes = lo - ln;		/* Signed difference. */
		if (p == t)			/* First index is fast. */
			h->inp[indx] += nbytes;
		else {				/* Else, shift the page. */
			memmove(p + nbytes, p, t - p);

			/* Adjust the indices' offsets. */
			off = h->inp[indx];
			for (cnt = 0; cnt < NUM_ENT(h); ++cnt)
				if (h->inp[cnt] <= off)
					h->inp[cnt] += nbytes;
		}

		/* Clean up the page and adjust the item's reference. */
		HOFFSET(h) += nbytes;
		t += nbytes;
	}

	/* Copy the new item onto the page. */
	bk = (BKEYDATA *)t;
	B_TSET(bk->type, B_KEYDATA, 0);
	bk->len = data->size;
	memcpy(bk->data, data->data, data->size);

	return (0);
}

/*
 * __bam_ndup --
 *	Check to see if the duplicate set at indx should have its own page.
 *	If it should, create it.
 */
static int
__bam_ndup(dbc, h, indx)
	DBC *dbc;
	PAGE *h;
	u_int32_t indx;
{
	BKEYDATA *bk;
	BOVERFLOW bo;
	DB *dbp;
	DBT hdr;
	PAGE *cp;
	db_indx_t cnt, cpindx, first, sz;
	int ret;

	dbp = dbc->dbp;

	/*
	 * Count the duplicate records and calculate how much room they're
	 * using on the page.
	 */
	while (indx > 0 && h->inp[indx] == h->inp[indx - P_INDX])
		indx -= P_INDX;
	for (cnt = 0, sz = 0, first = indx;; ++cnt, indx += P_INDX) {
		if (indx >= NUM_ENT(h) || h->inp[first] != h->inp[indx])
			break;
		bk = GET_BKEYDATA(h, indx);
		sz += B_TYPE(bk->type) == B_KEYDATA ?
		    BKEYDATA_PSIZE(bk->len) : BOVERFLOW_PSIZE;
		bk = GET_BKEYDATA(h, indx + O_INDX);
		sz += B_TYPE(bk->type) == B_KEYDATA ?
		    BKEYDATA_PSIZE(bk->len) : BOVERFLOW_PSIZE;
	}

	/*
	 * We have to do these checks when the user is replacing the cursor's
	 * data item -- if the application replaces a duplicate item with a
	 * larger data item, it can increase the amount of space used by the
	 * duplicates, requiring this check.  But that means it may not be a
	 * duplicate after all.
	 */
	if (cnt == 1)
		return (0);

	/*
	 * If this set of duplicates is using more than 25% of the page, move
	 * them off.  The choice of 25% is a WAG, but it has to be small enough
	 * that we can always split regardless of the presence of duplicates.
	 */
	if (sz < dbp->pgsize / 4)
		return (0);

	/* Get a new page. */
	if ((ret = __db_new(dbc, P_DUPLICATE, &cp)) != 0)
		return (ret);

	/*
	 * Move this set of duplicates off the page.  First points to the first
	 * key of the first duplicate key/data pair, cnt is the number of pairs
	 * we're dealing with.
	 */
	memset(&hdr, 0, sizeof(hdr));
	for (indx = first + O_INDX, cpindx = 0;; ++cpindx) {
		/* Copy the entry to the new page. */
		bk = GET_BKEYDATA(h, indx);
		hdr.data = bk;
		hdr.size = B_TYPE(bk->type) == B_KEYDATA ?
		    BKEYDATA_SIZE(bk->len) : BOVERFLOW_SIZE;
		if ((ret =
		    __db_pitem(dbc, cp, cpindx, hdr.size, &hdr, NULL)) != 0)
			goto err;

		/* Move cursors referencing the old entry to the new entry. */
		__bam_ca_dup(dbp,
		    PGNO(h), first, indx - O_INDX, PGNO(cp), cpindx);

		/* Delete the data item. */
		if ((ret = __db_ditem(dbc, h, indx, hdr.size)) != 0)
			goto err;

		__bam_ca_di(dbp, PGNO(h), indx, -1);

		/* Delete all but the first reference to the key. */
		if (--cnt == 0)
			break;
		if ((ret = __bam_adjindx(dbc, h, indx, first, 0)) != 0)
			goto err;
	}

	/* Put in a new data item that points to the duplicates page. */
	UMRW(bo.unused1);
	B_TSET(bo.type, B_DUPLICATE, 0);
	UMRW(bo.unused2);
	bo.pgno = cp->pgno;
	bo.tlen = 0;

	OVPUT(h, indx, bo);

	return (memp_fput(dbp->mpf, cp, DB_MPOOL_DIRTY));

err:	(void)__db_free(dbc, cp);
	return (ret);
}

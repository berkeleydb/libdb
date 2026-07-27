/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/db_am.h"
#include "dbinc/heap.h"

/*
 * __db_ret_okitem --
 *	Bounds-check the on-page leaf item at (h, indx) against the page size
 *	before __db_ret dereferences its length/offset fields.
 *
 *	The page comes from an untrusted file: a corrupt inp[] offset or item
 *	length field must not be trusted, or a copy-out (__db_retcopy /
 *	__db_goff) reads past the end of the page (a heap OOB read).  This is
 *	the lean, silent, hot-path mirror of __db_vrfy_inpitem (db_vrfy.c) --
 *	it gates every access-method cursor return that routes through
 *	__db_ret (btree, hash, heap), so all of them are covered in one place.
 *
 *	Returns 0 if the item is safe to read; DB_PAGE_NOTFOUND otherwise.
 */
static int
__db_ret_okitem(dbp, h, indx)
	DB *dbp;
	PAGE *h;
	u_int32_t indx;
{
	BKEYDATA *bk;
	HEAPHDR *hdr;
	db_indx_t *inp, off, prev;
	size_t pgsize;

	pgsize = dbp->pgsize;
	inp = P_INP(dbp, h);

	/* The index must be one of the entries the page claims to hold. */
	if (indx >= NUM_ENT(h))
		return (DB_PAGE_NOTFOUND);

	/*
	 * The item offset must fall after the inp[] array (which grows down
	 * from the header) and strictly before the end of the page.
	 */
	off = inp[indx];
	if ((size_t)((u_int8_t *)(inp + indx + 1) - (u_int8_t *)h) > pgsize)
		return (DB_PAGE_NOTFOUND);
	if ((size_t)off < (size_t)((u_int8_t *)(inp + NUM_ENT(h)) -
	    (u_int8_t *)h) || (size_t)off >= pgsize)
		return (DB_PAGE_NOTFOUND);

	switch (TYPE(h)) {
	case P_HASH_UNSORTED:
	case P_HASH:
		/*
		 * The one-byte type selector must be on-page, and the item
		 * length is derived as (prev_off - off) via LEN_HITEM, where
		 * prev_off is pgsize for indx 0 else inp[indx-1].  Guard the
		 * subtraction against underflow (prev_off <= off) and against
		 * a prev_off that itself runs past the page.
		 */
		if ((size_t)off + 1 > pgsize)
			return (DB_PAGE_NOTFOUND);
		prev = indx == 0 ? (db_indx_t)pgsize : inp[indx - 1];
		if ((size_t)prev > pgsize || off >= prev)
			return (DB_PAGE_NOTFOUND);
		if (HPAGE_PTYPE((u_int8_t *)h + off) == H_OFFPAGE &&
		    (size_t)off + HOFFPAGE_SIZE > pgsize)
			return (DB_PAGE_NOTFOUND);
		/* On-page H_KEYDATA/H_DUPLICATE: item must hold its header. */
		if (HPAGE_PTYPE((u_int8_t *)h + off) != H_OFFPAGE &&
		    (size_t)(prev - off) < HKEYDATA_SIZE(0))
			return (DB_PAGE_NOTFOUND);
		break;
	case P_HEAP:
		if ((size_t)off + sizeof(HEAPHDR) > pgsize)
			return (DB_PAGE_NOTFOUND);
		hdr = (HEAPHDR *)((u_int8_t *)h + off);
		/* Split records are copied out separately; skip the size check. */
		if (!F_ISSET(hdr, (HEAP_RECSPLIT | HEAP_RECFIRST)) &&
		    (size_t)off + sizeof(HEAPHDR) + hdr->size > pgsize)
			return (DB_PAGE_NOTFOUND);
		break;
	case P_LBTREE:
	case P_LDUP:
	case P_LRECNO:
		/* The BKEYDATA/BOVERFLOW header (len,type) must be on-page. */
		if ((size_t)off + SSZA(BKEYDATA, data) > pgsize)
			return (DB_PAGE_NOTFOUND);
		bk = GET_BKEYDATA(dbp, h, indx);
		if (B_TYPE(bk->type) == B_OVERFLOW ||
		    B_TYPE(bk->type) == B_DUPLICATE) {
			if ((size_t)off + BOVERFLOW_SIZE > pgsize)
				return (DB_PAGE_NOTFOUND);
		} else if (B_TYPE(bk->type) == B_KEYDATA) {
			/* data[bk->len] must not run past the page end. */
			if ((size_t)off + SSZA(BKEYDATA, data) + bk->len > pgsize)
				return (DB_PAGE_NOTFOUND);
		} else
			return (DB_PAGE_NOTFOUND);
		break;
	default:
		break;
	}
	return (0);
}

/*
 * __db_ret --
 *	Build return DBT.
 *
 * PUBLIC: int __db_ret __P((DBC *,
 * PUBLIC:    PAGE *, u_int32_t, DBT *, void **, u_int32_t *));
 */
int
__db_ret(dbc, h, indx, dbt, memp, memsize)
	DBC *dbc;
	PAGE *h;
	u_int32_t indx;
	DBT *dbt;
	void **memp;
	u_int32_t *memsize;
{
	BKEYDATA *bk;
	BOVERFLOW *bo;
	DB *dbp;
	HEAPHDR *hdr;
	HOFFPAGE ho;
	u_int32_t len;
	u_int8_t *hk;
	void *data;

	if (F_ISSET(dbt, DB_DBT_READONLY))
		return (0);
	dbp = dbc->dbp;

	/*
	 * Validate the on-page item against the (possibly corrupt) page
	 * before trusting any of its length/offset fields.  Only the leaf
	 * item types that __db_ret reads below are checked.
	 */
	if ((TYPE(h) == P_HASH_UNSORTED || TYPE(h) == P_HASH ||
	    TYPE(h) == P_HEAP || TYPE(h) == P_LBTREE ||
	    TYPE(h) == P_LDUP || TYPE(h) == P_LRECNO) &&
	    __db_ret_okitem(dbp, h, indx) != 0)
		return (DB_PAGE_NOTFOUND);

	switch (TYPE(h)) {
	case P_HASH_UNSORTED:
	case P_HASH:
		hk = P_ENTRY(dbp, h, indx);
		if (HPAGE_PTYPE(hk) == H_OFFPAGE) {
			memcpy(&ho, hk, sizeof(HOFFPAGE));
			return (__db_goff(dbc, dbt,
			    ho.tlen, ho.pgno, memp, memsize));
		}
		len = LEN_HKEYDATA(dbp, h, dbp->pgsize, indx);
		data = HKEYDATA_DATA(hk);
		break;
	case P_HEAP:
		hdr = (HEAPHDR *)P_ENTRY(dbp, h, indx);
		if (F_ISSET(hdr,(HEAP_RECSPLIT | HEAP_RECFIRST)))
			return (__heapc_gsplit(dbc, dbt, memp, memsize));
		len = hdr->size;
		data = (u_int8_t *)hdr + sizeof(HEAPHDR);
		break;
	case P_LBTREE:
	case P_LDUP:
	case P_LRECNO:
		bk = GET_BKEYDATA(dbp, h, indx);
		if (B_TYPE(bk->type) == B_OVERFLOW) {
			bo = (BOVERFLOW *)bk;
			return (__db_goff(dbc, dbt,
			    bo->tlen, bo->pgno, memp, memsize));
		}
		len = bk->len;
		data = bk->data;
		break;
	default:
		return (__db_pgfmt(dbp->env, h->pgno));
	}

	return (__db_retcopy(dbp->env, dbt, data, len, memp, memsize));
}

/*
 * __db_retcopy --
 *	Copy the returned data into the user's DBT, handling special flags.
 *
 * PUBLIC: int __db_retcopy __P((ENV *, DBT *,
 * PUBLIC:    void *, u_int32_t, void **, u_int32_t *));
 */
int
__db_retcopy(env, dbt, data, len, memp, memsize)
	ENV *env;
	DBT *dbt;
	void *data;
	u_int32_t len;
	void **memp;
	u_int32_t *memsize;
{
	int ret;

	if (F_ISSET(dbt, DB_DBT_READONLY))
		return (0);
	ret = 0;

	/* If returning a partial record, reset the length. */
	if (F_ISSET(dbt, DB_DBT_PARTIAL)) {
		data = (u_int8_t *)data + dbt->doff;
		if (len > dbt->doff) {
			len -= dbt->doff;
			if (len > dbt->dlen)
				len = dbt->dlen;
		} else
			len = 0;
	}

	/*
	 * Allocate memory to be owned by the application: DB_DBT_MALLOC,
	 * DB_DBT_REALLOC.
	 *
	 * !!!
	 * We always allocate memory, even if we're copying out 0 bytes. This
	 * guarantees consistency, i.e., the application can always free memory
	 * without concern as to how many bytes of the record were requested.
	 *
	 * Use the memory specified by the application: DB_DBT_USERMEM.
	 *
	 * !!!
	 * If the length we're going to copy is 0, the application-supplied
	 * memory pointer is allowed to be NULL.
	 */
	if (F_ISSET(dbt, DB_DBT_USERCOPY)) {
		dbt->size = len;
		return (len == 0 ? 0 : env->dbt_usercopy(dbt, 0, data,
		    len, DB_USERCOPY_SETDATA));

	} else if (F_ISSET(dbt, DB_DBT_MALLOC))
		ret = __os_umalloc(env, len, &dbt->data);
	else if (F_ISSET(dbt, DB_DBT_REALLOC)) {
		if (dbt->data == NULL || dbt->size == 0 || dbt->size < len)
			ret = __os_urealloc(env, len, &dbt->data);
	} else if (F_ISSET(dbt, DB_DBT_USERMEM)) {
		if (len != 0 && (dbt->data == NULL || dbt->ulen < len))
			ret = DB_BUFFER_SMALL;
	} else if (memp == NULL || memsize == NULL)
		ret = EINVAL;
	else {
		if (len != 0 && (*memsize == 0 || *memsize < len)) {
			if ((ret = __os_realloc(env, len, memp)) == 0)
				*memsize = len;
			else
				*memsize = 0;
		}
		if (ret == 0)
			dbt->data = *memp;
	}

	if (ret == 0 && len != 0)
		memcpy(dbt->data, data, len);

	/*
	 * Return the length of the returned record in the DBT size field.
	 * This satisfies the requirement that if we're using user memory
	 * and insufficient memory was provided, return the amount necessary
	 * in the size field.
	 */
	dbt->size = len;

	return (ret);
}

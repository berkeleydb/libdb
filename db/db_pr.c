/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)db_pr.c	11.9 (Sleepycat) 11/10/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "btree.h"
#include "hash.h"
#include "qam.h"
#include "db_am.h"

static int	 __db_bmeta __P((DB *, FILE *, BTMETA *, u_int32_t));
static int	 __db_hmeta __P((DB *, FILE *, HMETA *, u_int32_t));
static void	 __db_meta __P((DB *, DBMETA *, FILE *, FN const *, u_int32_t));
static const char	*__db_name __P((DB *));
static void	 __db_prdb __P((DB *, FILE *, u_int32_t));
static FILE	*__db_prinit __P((FILE *));
static void	 __db_proff __P((void *));
static int	 __db_prtree __P((DB *, u_int32_t));
static void	 __db_psize __P((DB *));
static int	 __db_qmeta __P((DB *, FILE *, QMETA *, u_int32_t));

/*
 * 64K is the maximum page size, so by default we check for offsets larger
 * than that, and, where possible, we refine the test.
 */
#define	PSIZE_BOUNDARY	(64 * 1024 + 1)
static size_t set_psize = PSIZE_BOUNDARY;

static FILE *set_fp;				/* Output file descriptor. */

#define	DB_PR_PAGE	0x01			/* Show page contents. */
#define	DB_PR_RECOVERY	0x02			/* Recovery test. */

/*
 * __db_loadme --
 *	A nice place to put a breakpoint.
 *
 * PUBLIC: void __db_loadme __P((void));
 */
void
__db_loadme()
{
	getpid();
}

/*
 * __db_dump --
 *	Dump the tree to a file.
 *
 * PUBLIC: int __db_dump __P((DB *, char *, char *));
 */
int
__db_dump(dbp, op, name)
	DB *dbp;
	char *op, *name;
{
	FILE *fp, *save_fp;
	u_int32_t flags;

	COMPQUIET(save_fp, NULL);

	if (set_psize == PSIZE_BOUNDARY)
		__db_psize(dbp);

	if (name != NULL) {
		if ((fp = fopen(name, "w")) == NULL)
			return (__os_get_errno());
		save_fp = set_fp;
		set_fp = fp;
	} else
		fp = __db_prinit(NULL);

	for (flags = 0; *op != '\0'; ++op)
		switch (*op) {
		case 'a':
			LF_SET(DB_PR_PAGE);
			break;
		case 'h':
			break;
		case 'r':
			LF_SET(DB_PR_RECOVERY);
			break;
		default:
			return (EINVAL);
		}

	__db_prdb(dbp, fp, flags);

	fprintf(fp, "%s\n", DB_LINE);

	(void)__db_prtree(dbp, flags);

	fflush(fp);

	if (name != NULL) {
		fclose(fp);
		set_fp = save_fp;
	}
	return (0);
}

/*
 * __db_prdb --
 *	Print out the DB structure information.
 */
static void
__db_prdb(dbp, fp, flags)
	DB *dbp;
	FILE *fp;
	u_int32_t flags;
{
	static const FN fn[] = {
		{ DB_AM_DISCARD,	"discard cached pages" },
		{ DB_AM_DUP,		"duplicates" },
		{ DB_AM_INMEM,		"in-memory" },
		{ DB_AM_PGDEF,		"default page size" },
		{ DB_AM_RDONLY,		"read-only" },
		{ DB_AM_SUBDB,		"subdatabases" },
		{ DB_AM_SWAP,		"needswap" },
		{ DB_BT_RECNUM,		"btree:recnum" },
		{ DB_BT_REVSPLIT,	"btree:no reverse split" },
		{ DB_DBM_ERROR,		"dbm/ndbm error" },
		{ DB_OPEN_CALLED,	"DB->open called" },
		{ DB_RE_DELIMITER,	"recno:delimiter" },
		{ DB_RE_FIXEDLEN,	"recno:fixed-length" },
		{ DB_RE_PAD,		"recno:pad" },
		{ DB_RE_RENUMBER,	"recno:renumber" },
		{ DB_RE_SNAPSHOT,	"recno:snapshot" },
		{ 0,			NULL }
	};
	static const FN bfn[] = {
		{ RECNO_EOF,		"recno:eof" },
		{ RECNO_MODIFIED,	"recno:modified" },
		{ 0,			NULL }
	};
	BTREE *bt;
	HASH *h;
	QUEUE *q;

	COMPQUIET(flags, 0);

	fprintf(fp,
	    "In-memory DB structure:\n%s: %#lx",
	    __db_name(dbp), (u_long)dbp->flags);
	__db_prflags(dbp->flags, fn, fp);
	fprintf(fp, "\n");

	switch (dbp->type) {
	case DB_BTREE:
	case DB_RECNO:
		bt = dbp->bt_internal;
		fprintf(fp, "bt_lpgno: %lu\n", (u_long)bt->bt_lpgno);
		fprintf(fp, "bt_ovflsize: %lu\n", (u_long)bt->bt_ovflsize);
		fprintf(fp, "bt_meta: %lu: bt_root: %lu\n",
		    (u_long)bt->bt_meta, (u_long)bt->bt_root);
		fprintf(fp, "bt_maxkey: %lu bt_minkey: %lu\n",
		    (u_long)bt->bt_maxkey, (u_long)bt->bt_minkey);
		fprintf(fp, "bt_compare: %#lx bt_prefix: %#lx\n",
		    (u_long)bt->bt_compare, (u_long)bt->bt_prefix);
		if (dbp->type == DB_RECNO) {
			fprintf(fp,
		    "re_pad: %#lx re_delim: %#lx re_len: %lu re_source: %s\n",
			    (u_long)bt->re_pad, (u_long)bt->re_delim,
			    (u_long)bt->re_len,
			    bt->re_source == NULL ? "" : bt->re_source);
			fprintf(fp, "re_last: %lu\n", (u_long)bt->re_last);
			fprintf(fp,
			    "cmap: %#lx smap: %#lx emap: %#lx msize: %lu\n",
			    (u_long)bt->re_cmap, (u_long)bt->re_smap,
			    (u_long)bt->re_emap, (u_long)bt->re_msize);
			fprintf(fp, "re_irec: %#lx\n", (u_long)bt->re_irec);
		}
		fprintf(fp, "flags: %#lx", (u_long)bt->flags);
		__db_prflags(bt->flags, bfn, fp);
		fprintf(fp, "\n");
		break;
	case DB_HASH:
		h = dbp->h_internal;
		fprintf(fp, "meta_pgno: %lu\n", (u_long)h->meta_pgno);
		fprintf(fp, "h_ffactor: %lu\n", (u_long)h->h_ffactor);
		fprintf(fp, "h_nelem: %lu\n", (u_long)h->h_nelem);
		fprintf(fp, "h_hash: %#lx\n", (u_long)h->h_hash);
		break;
	case DB_QUEUE:
		q = dbp->q_internal;
		fprintf(fp, "q_meta: %lu\n", (u_long)q->q_meta);
		fprintf(fp, "q_root: %lu\n", (u_long)q->q_root);
		fprintf(fp, "re_pad: %#lx re_len: %lu\n",
		    (u_long)q->re_pad, (u_long)q->re_len);
		fprintf(fp, "rec_page: %lu\n", (u_long)q->rec_page);
		break;
	default:
		break;
	}
}

/*
 * __db_prtree --
 *	Print out the entire tree.
 */
static int
__db_prtree(dbp, flags)
	DB *dbp;
	u_int32_t flags;
{
	PAGE *h;
	db_pgno_t i, last;
	int ret;

	if (set_psize == PSIZE_BOUNDARY)
		__db_psize(dbp);

	/* Find out the page number of the last page in the database. */
	if ((ret = memp_fget(dbp->mpf, &last, DB_MPOOL_LAST, &h)) != 0)
		return (ret);
	if ((ret = memp_fput(dbp->mpf, h, 0)) != 0)
		return (ret);

	/* Dump each page. */
	for (i = 0; i <= last; ++i) {
		if ((ret = memp_fget(dbp->mpf, &i, 0, &h)) != 0)
			return (ret);
		(void)__db_prpage(dbp, h, flags);
		if ((ret = memp_fput(dbp->mpf, h, 0)) != 0)
			return (ret);
	}

	(void)fflush(__db_prinit(NULL));
	return (0);
}

/*
 * __db_meta --
 *	Print out common metadata information.
 */
static void
__db_meta(dbp, dbmeta, fp, fn, flags)
	DB *dbp;
	DBMETA *dbmeta;
	FILE *fp;
	FN const *fn;
	u_int32_t flags;
{
	PAGE *h;
	int cnt;
	db_pgno_t pgno;
	u_int8_t *p;
	int ret;
	const char *sep;

	fprintf(fp, "\tmagic: %#lx\n", (u_long)dbmeta->magic);
	fprintf(fp, "\tversion: %lu\n", (u_long)dbmeta->version);
	fprintf(fp, "\tpagesize: %lu\n", (u_long)dbmeta->pagesize);
	fprintf(fp, "\ttype: %lu\n", (u_long)dbmeta->type);

	if (!LF_ISSET(DB_PR_RECOVERY)) {
		/*
		 * If we're doing recovery testing, don't display the free
		 * list, it may have changed and that makes the dump diff
		 * not work.
		 */
		fprintf(fp, "\tfree list: %lu", (u_long)dbmeta->free);
		for (pgno = dbmeta->free,
		    cnt = 0, sep = ", "; pgno != PGNO_INVALID;) {
			if ((ret = memp_fget(dbp->mpf, &pgno, 0, &h)) != 0) {
				fprintf(fp,
			    "Unable to retrieve free-list page: %lu: %s\n",
				    (u_long)pgno, db_strerror(ret));
				break;
			}
			pgno = h->next_pgno;
			(void)memp_fput(dbp->mpf, h, 0);
			fprintf(fp, "%s%lu", sep, (u_long)pgno);
			if (++cnt % 10 == 0) {
				fprintf(fp, "\n");
				cnt = 0;
				sep = "\t";
			} else
				sep = ", ";
		}
		fprintf(fp, "\n");
	}

	if (fn != NULL) {
		fprintf(fp, "\tflags: %#lx", (u_long)dbmeta->flags);
		__db_prflags(dbmeta->flags, fn, fp);
		fprintf(fp, "\n");
	}

	fprintf(fp, "\tuid: ");
	for (p = (u_int8_t *)dbmeta->uid,
	    cnt = 0; cnt < DB_FILE_ID_LEN; ++cnt) {
		fprintf(fp, "%x", *p++);
		if (cnt < DB_FILE_ID_LEN - 1)
			fprintf(fp, " ");
	}
	fprintf(fp, "\n");
}

/*
 * __db_bmeta --
 *	Print out the btree meta-data page.
 */
static int
__db_bmeta(dbp, fp, h, flags)
	DB *dbp;
	FILE *fp;
	BTMETA *h;
	u_int32_t flags;
{
	static const FN mfn[] = {
		{ BTM_DUP,	"duplicates" },
		{ BTM_RECNO,	"recno" },
		{ BTM_RECNUM,	"btree:recnum" },
		{ BTM_FIXEDLEN,	"recno:fixed-length" },
		{ BTM_RENUMBER,	"recno:renumber" },
		{ BTM_SUBDB,	"subdatabases" },
		{ 0,		NULL }
	};

	__db_meta(dbp, (DBMETA *)h, fp, mfn, flags);

	fprintf(fp, "\tmaxkey: %lu minkey: %lu\n",
	    (u_long)h->maxkey, (u_long)h->minkey);
	if (dbp->type == DB_RECNO)
		fprintf(fp, "\tre_len: %#lx re_pad: %lu\n",
		    (u_long)h->re_len, (u_long)h->re_pad);
	fprintf(fp, "\troot: %lu\n", (u_long)h->root);

	return (0);
}

/*
 * __db_hmeta --
 *	Print out the hash meta-data page.
 */
static int
__db_hmeta(dbp, fp, h, flags)
	DB *dbp;
	FILE *fp;
	HMETA *h;
	u_int32_t flags;
{
	static const FN mfn[] = {
		{ DB_HASH_DUP,	 "duplicates" },
		{ DB_HASH_SUBDB, "subdatabases" },
		{ 0,		 NULL }
	};
	int i;

	__db_meta(dbp, (DBMETA *)h, fp, mfn, flags);

	fprintf(fp, "\tmax_bucket: %lu\n", (u_long)h->max_bucket);
	fprintf(fp, "\thigh_mask: %#lx\n", (u_long)h->high_mask);
	fprintf(fp, "\tlow_mask:  %#lx\n", (u_long)h->low_mask);
	fprintf(fp, "\tffactor: %lu\n", (u_long)h->ffactor);
	fprintf(fp, "\tnelem: %lu\n", (u_long)h->nelem);
	fprintf(fp, "\th_charkey: %#lx\n", (u_long)h->h_charkey);
	fprintf(fp, "\tspare points: ");
	for (i = 0; i < NCACHED; i++)
		fprintf(fp, "%lu ", (u_long)h->spares[i]);
	fprintf(fp, "\n");

	return (0);
}

/*
 * __db_qmeta --
 *	Print out the queue meta-data page.
 */
static int
__db_qmeta(dbp, fp, h, flags)
	DB *dbp;
	FILE *fp;
	QMETA *h;
	u_int32_t flags;
{
	__db_meta(dbp, (DBMETA *)h, fp, NULL, flags);

	fprintf(fp, "\tstart: %lu\n", (u_long)h->start);
	fprintf(fp, "\tfirst_recno: %lu\n", (u_long)h->first_recno);
	fprintf(fp, "\tcur_recno: %lu\n", (u_long)h->cur_recno);
	fprintf(fp, "\tre_len: %#lx re_pad: %lu\n",
	    (u_long)h->re_len, (u_long)h->re_pad);
	fprintf(fp, "\trec_page: %lu\n", (u_long)h->rec_page);

	return (0);
}

/*
 * __db_prnpage
 *	-- Print out a specific page.
 *
 * PUBLIC: int __db_prnpage __P((DB *, db_pgno_t));
 */
int
__db_prnpage(dbp, pgno)
	DB *dbp;
	db_pgno_t pgno;
{
	PAGE *h;
	int ret;

	if (set_psize == PSIZE_BOUNDARY)
		__db_psize(dbp);

	if ((ret = memp_fget(dbp->mpf, &pgno, 0, &h)) != 0)
		return (ret);

	ret = __db_prpage(dbp, h, DB_PR_PAGE);
	(void)fflush(__db_prinit(NULL));

	(void)memp_fput(dbp->mpf, h, 0);
	return (ret);
}

/*
 * __db_prpage
 *	-- Print out a page.
 *
 * PUBLIC: int __db_prpage __P((DB *, PAGE *, u_int32_t));
 */
int
__db_prpage(dbp, h, flags)
	DB *dbp;
	PAGE *h;
	u_int32_t flags;
{
	BINTERNAL *bi;
	BKEYDATA *bk;
	BTREE *t;
	FILE *fp;
	HOFFPAGE a_hkd;
	QAMDATA *qp, *qep;
	RINTERNAL *ri;
	db_indx_t dlen, len, i;
	db_pgno_t pgno;
	db_recno_t recno;
	int deleted, ret;
	const char *s;
	u_int32_t qlen;
	u_int8_t *ep, *hk, *p;
	void *sp;

	fp = __db_prinit(NULL);

	switch (TYPE(h)) {
	case P_BTREEMETA:
		s = "btree metadata";
		break;
	case P_DUPLICATE:
		s = "duplicate";
		break;
	case P_HASH:
		s = "hash";
		break;
	case P_HASHMETA:
		s = "hash metadata";
		break;
	case P_IBTREE:
		s = "btree internal";
		break;
	case P_INVALID:
		/*
		 * If we're doing recovery testing, assume this is a page
		 * that's on the free list, and don't display it.
		 */
		if (LF_ISSET(DB_PR_RECOVERY))
			return (0);
		s = "invalid";
		break;
	case P_IRECNO:
		s = "recno internal";
		break;
	case P_LBTREE:
		s = "btree leaf";
		break;
	case P_LRECNO:
		s = "recno leaf";
		break;
	case P_OVERFLOW:
		s = "overflow";
		break;
	case P_QAMMETA:
		s = "queue metadata";
		break;
	case P_QAMDATA:
		s = "queue";
		break;
	default:
		fprintf(fp, "ILLEGAL PAGE TYPE: page: %lu type: %lu\n",
		    (u_long)h->pgno, (u_long)TYPE(h));
			return (1);
	}

	/* Every page has a type, a page number, and an LSN. */
	fprintf(fp, "page %lu: %s", (u_long)h->pgno, s);
	fprintf(fp, " (lsn.file: %lu lsn.offset: %lu)\n",
	    (u_long)LSN(h).file, (u_long)LSN(h).offset);

	switch (TYPE(h)) {
	case P_BTREEMETA:
		return (__db_bmeta(dbp, fp, (BTMETA *)h, flags));
	case P_HASHMETA:
		return (__db_hmeta(dbp, fp, (HMETA *)h, flags));
	case P_QAMMETA:
		return (__db_qmeta(dbp, fp, (QMETA *)h, flags));
	case P_QAMDATA:				/* Should be meta->start. */
		if (!LF_ISSET(DB_PR_PAGE))
			return (0);

		qlen = ((QUEUE *)dbp->q_internal)->re_len;
		recno = (h->pgno - 1) * QAM_RECNO_PER_PAGE(dbp) + 1;
		i = 0;
		qep = (QAMDATA *)((u_long) h + set_psize - qlen);
		for (qp = QAM_GET_RECORD(dbp, h, i); qp < qep;
		    recno++, i++, qp = QAM_GET_RECORD(dbp, h, i)) {
			if (!F_ISSET(qp, QAM_SET))
				continue;

			fprintf(fp, "%s",
			    F_ISSET(qp, QAM_VALID) ? "\t" : "       D");
			fprintf(fp, "[%03lu] %4lu ",
			    (u_long)recno, (u_long)qp - (u_long)h);
			__db_pr(qp->data, qlen);
		}
		return (0);
	}

	t = dbp->bt_internal;

	if (TYPE(h) == P_IBTREE || TYPE(h) == P_IRECNO ||
	    (TYPE(h) == P_LRECNO &&
	    h->pgno == ((BTREE *)dbp->bt_internal)->bt_root))
		fprintf(fp, "\ttotal records: %4lu\n", (u_long)RE_NREC(h));
	if (TYPE(h) != P_IBTREE && TYPE(h) != P_IRECNO)
		fprintf(fp, "\tprev: %4lu next: %4lu",
		    (u_long)PREV_PGNO(h), (u_long)NEXT_PGNO(h));
	if (TYPE(h) == P_IBTREE || TYPE(h) == P_LBTREE)
		fprintf(fp, " level: %2lu", (u_long)h->level);
	if (TYPE(h) == P_OVERFLOW) {
		fprintf(fp, " ref cnt: %4lu ", (u_long)OV_REF(h));
		__db_pr((u_int8_t *)h + P_OVERHEAD, OV_LEN(h));
		return (0);
	}
	fprintf(fp, " entries: %4lu", (u_long)NUM_ENT(h));
	fprintf(fp, " offset: %4lu\n", (u_long)HOFFSET(h));

	if (TYPE(h) == P_INVALID || !LF_ISSET(DB_PR_PAGE))
		return (0);

	ret = 0;
	for (i = 0; i < NUM_ENT(h); i++) {
		if (P_ENTRY(h, i) - (u_int8_t *)h < P_OVERHEAD ||
		    (size_t)(P_ENTRY(h, i) - (u_int8_t *)h) >= set_psize) {
			fprintf(fp,
			    "ILLEGAL PAGE OFFSET: indx: %lu of %lu\n",
			    (u_long)i, (u_long)h->inp[i]);
			ret = EINVAL;
			continue;
		}
		deleted = 0;
		switch (TYPE(h)) {
		case P_HASH:
		case P_IBTREE:
		case P_IRECNO:
			sp = P_ENTRY(h, i);
			break;
		case P_LBTREE:
			sp = P_ENTRY(h, i);
			deleted = i % 2 == 0 &&
			    B_DISSET(GET_BKEYDATA(h, i + O_INDX)->type);
			break;
		case P_LRECNO:
		case P_DUPLICATE:
			sp = P_ENTRY(h, i);
			deleted = B_DISSET(GET_BKEYDATA(h, i)->type);
			break;
		default:
			fprintf(fp,
			    "ILLEGAL PAGE ITEM: %lu\n", (u_long)TYPE(h));
			ret = EINVAL;
			continue;
		}
		fprintf(fp, "%s", deleted ? "       D" : "\t");
		fprintf(fp, "[%03lu] %4lu ", (u_long)i, (u_long)h->inp[i]);
		switch (TYPE(h)) {
		case P_HASH:
			hk = sp;
			switch (HPAGE_PTYPE(hk)) {
			case H_OFFDUP:
				memcpy(&pgno,
				    HOFFDUP_PGNO(hk), sizeof(db_pgno_t));
				fprintf(fp,
				    "%4lu [offpage dups]\n", (u_long)pgno);
				break;
			case H_DUPLICATE:
				/*
				 * If this is the first item on a page, then
				 * we cannot figure out how long it is, so
				 * we only print the first one in the duplicate
				 * set.
				 */
				if (i != 0)
					len = LEN_HKEYDATA(h, 0, i);
				else
					len = 1;

				fprintf(fp, "Duplicates:\n");
				for (p = HKEYDATA_DATA(hk),
				    ep = p + len; p < ep;) {
					memcpy(&dlen, p, sizeof(db_indx_t));
					p += sizeof(db_indx_t);
					fprintf(fp, "\t\t");
					__db_pr(p, dlen);
					p += sizeof(db_indx_t) + dlen;
				}
				break;
			case H_KEYDATA:
				__db_pr(HKEYDATA_DATA(hk),
				    LEN_HKEYDATA(h, i == 0 ? set_psize : 0, i));
				break;
			case H_OFFPAGE:
				memcpy(&a_hkd, hk, HOFFPAGE_SIZE);
				fprintf(fp,
				    "overflow: total len: %4lu page: %4lu\n",
				    (u_long)a_hkd.tlen, (u_long)a_hkd.pgno);
				break;
			}
			break;
		case P_IBTREE:
			bi = sp;
			fprintf(fp, "count: %4lu pgno: %4lu ",
			    (u_long)bi->nrecs, (u_long)bi->pgno);
			switch (B_TYPE(bi->type)) {
			case B_KEYDATA:
				__db_pr(bi->data, bi->len);
				break;
			case B_DUPLICATE:
			case B_OVERFLOW:
				__db_proff(bi->data);
				break;
			default:
				fprintf(fp, "ILLEGAL BINTERNAL TYPE: %lu\n",
				    (u_long)B_TYPE(bi->type));
				ret = EINVAL;
				break;
			}
			break;
		case P_IRECNO:
			ri = sp;
			fprintf(fp, "entries %4lu pgno %4lu\n",
			    (u_long)ri->nrecs, (u_long)ri->pgno);
			break;
		case P_LBTREE:
		case P_LRECNO:
		case P_DUPLICATE:
			bk = sp;
			switch (B_TYPE(bk->type)) {
			case B_KEYDATA:
				__db_pr(bk->data, bk->len);
				break;
			case B_DUPLICATE:
			case B_OVERFLOW:
				__db_proff(bk);
				break;
			default:
				fprintf(fp,
			    "ILLEGAL DUPLICATE/LBTREE/LRECNO TYPE: %lu\n",
				    (u_long)B_TYPE(bk->type));
				ret = EINVAL;
				break;
			}
			break;
		}
	}
	(void)fflush(fp);
	return (ret);
}

/*
 * __db_isbad
 *	-- Decide if a page is corrupted.
 *
 * PUBLIC: int __db_isbad __P((PAGE *, int));
 */
int
__db_isbad(h, die)
	PAGE *h;
	int die;
{
	BINTERNAL *bi;
	BKEYDATA *bk;
	FILE *fp;
	db_indx_t i;
	u_int type;

	fp = __db_prinit(NULL);

	switch (TYPE(h)) {
	case P_DUPLICATE:
	case P_HASH:
	case P_IBTREE:
	case P_INVALID:
	case P_IRECNO:
	case P_LBTREE:
	case P_LRECNO:
	case P_OVERFLOW:
		break;
	case P_BTREEMETA:
	case P_HASHMETA:
	case P_QAMDATA:
	case P_QAMMETA:
		return (0);
	default:
		fprintf(fp, "ILLEGAL PAGE TYPE: page: %lu type: %lu\n",
		    (u_long)h->pgno, (u_long)TYPE(h));
		goto bad;
	}

	for (i = 0; i < NUM_ENT(h); i++) {
		if (P_ENTRY(h, i) - (u_int8_t *)h < P_OVERHEAD ||
		    (size_t)(P_ENTRY(h, i) - (u_int8_t *)h) >= set_psize) {
			fprintf(fp,
			    "ILLEGAL PAGE OFFSET: indx: %lu of %lu\n",
			    (u_long)i, (u_long)h->inp[i]);
			goto bad;
		}
		switch (TYPE(h)) {
		case P_HASH:
			type = HPAGE_TYPE(h, i);
			if (type != H_OFFDUP &&
			    type != H_DUPLICATE &&
			    type != H_KEYDATA &&
			    type != H_OFFPAGE) {
				fprintf(fp, "ILLEGAL HASH TYPE: %lu\n",
				    (u_long)type);
				goto bad;
			}
			break;
		case P_IBTREE:
			bi = GET_BINTERNAL(h, i);
			if (B_TYPE(bi->type) != B_KEYDATA &&
			    B_TYPE(bi->type) != B_DUPLICATE &&
			    B_TYPE(bi->type) != B_OVERFLOW) {
				fprintf(fp, "ILLEGAL BINTERNAL TYPE: %lu\n",
				    (u_long)B_TYPE(bi->type));
				goto bad;
			}
			break;
		case P_IRECNO:
		case P_LBTREE:
		case P_LRECNO:
			break;
		case P_DUPLICATE:
			bk = GET_BKEYDATA(h, i);
			if (B_TYPE(bk->type) != B_KEYDATA &&
			    B_TYPE(bk->type) != B_DUPLICATE &&
			    B_TYPE(bk->type) != B_OVERFLOW) {
				fprintf(fp,
			    "ILLEGAL DUPLICATE/LBTREE/LRECNO TYPE: %lu\n",
				    (u_long)B_TYPE(bk->type));
				goto bad;
			}
			break;
		default:
			fprintf(fp,
			    "ILLEGAL PAGE ITEM: %lu\n", (u_long)TYPE(h));
			goto bad;
		}
	}
	return (0);

bad:	if (die) {
		abort();
		/* NOTREACHED */
	}
	return (1);
}

/*
 * __db_pr --
 *	Print out a data element.
 *
 * PUBLIC: void __db_pr __P((u_int8_t *, u_int32_t));
 */
void
__db_pr(p, len)
	u_int8_t *p;
	u_int32_t len;
{
	FILE *fp;
	u_int lastch;
	int i;

	fp = __db_prinit(NULL);

	fprintf(fp, "len: %3lu", (u_long)len);
	lastch = '.';
	if (len != 0) {
		fprintf(fp, " data: ");
		for (i = len <= 20 ? len : 20; i > 0; --i, ++p) {
			lastch = *p;
			if (isprint(*p) || *p == '\n')
				fprintf(fp, "%c", *p);
			else
				fprintf(fp, "0x%.2x", (u_int)*p);
		}
		if (len > 20) {
			fprintf(fp, "...");
			lastch = '.';
		}
	}
	if (lastch != '\n')
		fprintf(fp, "\n");
}

/*
 * __db_prdbt --
 *	Print out a DBT data element.
 *
 * PUBLIC: int __db_prdbt __P((DBT *, int, const char *, FILE *, int));
 */
int
__db_prdbt(dbtp, checkprint, prefix, fp, is_recno)
	DBT *dbtp;
	int checkprint;
	const char *prefix;
	FILE *fp;
	int is_recno;
{
	static const char hex[] = "0123456789abcdef";
	db_recno_t recno;
	u_int32_t len;
	u_int8_t *p;

	/*
	 * !!!
	 * This routine is the routine that dumps out items in the format
	 * used by db_dump(1) and db_load(1).  This means that the format
	 * cannot change.
	 */
	if (prefix != NULL && fprintf(fp, "%s", prefix) != (int)strlen(prefix))
		return (EIO);
	if (is_recno) {
		/* 
		 * We're printing a record number, and this has to be done
		 * in a platform-independent way.  So we use the numeral in
		 * straight ASCII.
		 */    
		__ua_memcpy(&recno, dbtp->data, sizeof(recno));
		if (fprintf(fp, "%lu", (u_long)recno) == 0)
			return (EIO);
	} else if (checkprint) {
		for (len = dbtp->size, p = dbtp->data; len--; ++p)
			if (isprint(*p)) {
				if (*p == '\\' && fprintf(fp, "\\") != 1)
					return (EIO);
				if (fprintf(fp, "%c", *p) != 1)
					return (EIO);
			} else
				if (fprintf(fp, "\\%c%c",
				    hex[(u_int8_t)(*p & 0xf0) >> 4],
				    hex[*p & 0x0f]) != 3)
					return (EIO);
	} else
		for (len = dbtp->size, p = dbtp->data; len--; ++p)
			if (fprintf(fp, "%c%c",
			    hex[(u_int8_t)(*p & 0xf0) >> 4],
			    hex[*p & 0x0f]) != 2)
				return (EIO);

	return (fprintf(fp, "\n") == 1 ? 0 : EIO);
}

/*
 * __db_proff --
 *	Print out an off-page element.
 */
static void
__db_proff(vp)
	void *vp;
{
	FILE *fp;
	BOVERFLOW *bo;

	fp = __db_prinit(NULL);

	bo = vp;
	switch (B_TYPE(bo->type)) {
	case B_OVERFLOW:
		fprintf(fp, "overflow: total len: %4lu page: %4lu\n",
		    (u_long)bo->tlen, (u_long)bo->pgno);
		break;
	case B_DUPLICATE:
		fprintf(fp, "duplicate: page: %4lu\n", (u_long)bo->pgno);
		break;
	}
}

/*
 * __db_prflags --
 *	Print out flags values.
 *
 * PUBLIC: void __db_prflags __P((u_int32_t, const FN *, FILE *));
 */
void
__db_prflags(flags, fn, fp)
	u_int32_t flags;
	FN const *fn;
	FILE *fp;
{
	const FN *fnp;
	int found;
	const char *sep;

	sep = " (";
	for (found = 0, fnp = fn; fnp->mask != 0; ++fnp)
		if (LF_ISSET(fnp->mask)) {
			fprintf(fp, "%s%s", sep, fnp->name);
			sep = ", ";
			found = 1;
		}
	if (found)
		fprintf(fp, ")");
}

/*
 * __db_prinit --
 *	Initialize tree printing routines.
 */
static FILE *
__db_prinit(fp)
	FILE *fp;
{
	if (set_fp == NULL)
		set_fp = fp == NULL ? stdout : fp;
	return (set_fp);
}

/*
 * __db_psize --
 *	Get the page size.
 */
static void
__db_psize(dbp)
	DB *dbp;
{
	DBMETA *mp;
	db_pgno_t pgno;

	set_psize = PSIZE_BOUNDARY - 1;

	pgno = PGNO_BASE_MD;
	if (memp_fget(dbp->mpf, &pgno, 0, &mp) != 0)
		return;

	switch (mp->magic) {
	case DB_BTREEMAGIC:
	case DB_HASHMAGIC:
	case DB_QAMMAGIC:
		set_psize = mp->pagesize;
		break;
	}
	(void)memp_fput(dbp->mpf, mp, 0);
}

/*
 * __db_name --
 *	Return the name of the database type.
 */
static const char *
__db_name(dbp)
	DB *dbp;
{
	switch (dbp->type) {
	case DB_BTREE:
		return ("btree");
	case DB_HASH:
		return ("hash");
		break;
	case DB_RECNO:
		return ("recno");
		break;
	case DB_QUEUE:
		return ("queue");
	default:
		return ("UNKNOWN TYPE");
	}
	/* NOTREACHED */
}

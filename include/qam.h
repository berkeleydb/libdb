/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)qam.h	11.7 (Sleepycat) 9/10/99
 */

/*
 * QAM data elements: a status field and the data.
 */
typedef struct _qamdata {
	u_int8_t  flags;	/* 00: delete bit. */
#define QAM_VALID	0x01
#define QAM_SET		0x02
	u_int8_t  data[1];	/* Record. */
} QAMDATA;

struct __queue;		typedef struct __queue QUEUE;
struct __qcursor;	typedef struct __qcursor QUEUE_CURSOR;

struct __qcursor {
	/* Per-thread information: queue private. */
	PAGE		*page;		/* Cursor page. */
	db_recno_t	 start;		/* start record number. */
	db_recno_t	 recno;		/* Current record number. */

	db_pgno_t	 pgno;		/* Page. */
	db_indx_t	 indx;		/* Page item ref'd by the cursor. */

	DB_LOCK		 lock;		/* Cursor lock. */
	db_lockmode_t	 lock_mode;	/* Lock mode. */

	u_int32_t	 flags;
};

/*
 * The in-memory, per-tree queue data structure.
 */
struct __queue {
	db_pgno_t q_meta;		/* Database meta-data page. */
	db_pgno_t q_root;		/* Database root page. */

	int	  re_pad;		/* Fixed-length padding byte. */
	u_int32_t re_len;		/* Length for fixed-length records. */
	u_int32_t rec_page;		/* records per page */
};

/*
 * Caculate the page number of a recno
 *
 * Number of records per page =
 * 	Divide the available space on the page by the record len + header.
 *
 * Page number for record =
 *	divide the physical record number by the records per page
 *	add the root page number
 *      For now the root page will always be 1, but we might want to change
 *	in the future (e.g. multiple fixed len queues per file).
 *
 * Index of record on page =
 * 	physical record number, less the logical pno times records/page
 */
#define CALC_QAM_RECNO_PER_PAGE(dbp) 				\
	(((dbp)->pgsize - ALIGN(sizeof(QPAGE), sizeof(u_int32_t))) / \
	ALIGN(((QUEUE *)(dbp)->q_internal)->re_len +		\
	sizeof(QAMDATA) - sizeof ((QAMDATA *)0)->data, sizeof(u_int32_t)))

#define QAM_RECNO_PER_PAGE(dbp)	(((QUEUE*)(dbp)->q_internal)->rec_page)

#define QAM_RECNO_PAGE(dbp, start, recno)			\
	(((QUEUE *)(dbp)->q_internal)->q_root			\
	      + (((recno) - (start)) / QAM_RECNO_PER_PAGE(dbp)))

#define QAM_RECNO_INDEX(dbp, pgno, start, recno) 		\
		(((recno) - (start)) -				\
			(QAM_RECNO_PER_PAGE(dbp) *		\
		(pgno - ((QUEUE *)(dbp)->q_internal)->q_root)))

#define QAM_GET_RECORD(dbp, page, index)			\
	((QAMDATA *) ((char *)(page) +				\
		ALIGN(sizeof(QPAGE), sizeof(u_int32_t)) +	\
	(ALIGN(sizeof(QAMDATA) - sizeof ((QAMDATA *)0)->data + 	\
	((QUEUE *)(dbp)->q_internal)->re_len, sizeof (u_int32_t)) * index)))

/*
 * Log opcodes for the mvptr routine.
 */
#define	QAM_SETFIRST		0x01
#define	QAM_SETCUR		0x02

#include "qam_auto.h"
#include "qam_ext.h"

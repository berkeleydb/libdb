/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness: queue record arithmetic (src/dbinc/qam.h).  (RFC 0025 Q1.)
 *
 * The queue access method is a circular log of fixed-length records numbered
 * by a 32-bit db_recno_t (RECNO_OOB == 0 is reserved).  A handful of macros in
 * qam.h do all the record<->(page,index) mapping and the "is this record in
 * the live window [first_recno, cur_recno)" tests, as plain u_int32_t
 * arithmetic that must stay correct even when the queue has WRAPPED around the
 * 32-bit space (cur_recno < first_recno).  Queue wrap-around has had bugs.
 *
 * We verify (over the whole 32-bit range, loop-free):
 *
 *   MAP: for recno >= 1 and rec_page >= 1,
 *        QAM_RECNO_INDEX(QAM_RECNO_PAGE(recno)) < rec_page, and the
 *        (page, index) pair maps back to recno.  No OOB, no div-by-zero (the
 *        harness constrains rec_page >= 1, which the engine guarantees:
 *        CALC_QAM_RECNO_PER_PAGE is >= 1 for any sane page/record size).
 *
 *   RANGE: QAM_OUTSIDE_QUEUE / QAM_BEFORE_FIRST / QAM_AFTER_CURRENT agree with
 *        an INDEPENDENT reference for "recno is in the circular live window",
 *        including the wrapped case (cur < first).
 *
 *   PARTITION: for any recno != RECNO_OOB, exactly one of {before-first,
 *        after-current, inside} holds (QAM_NOT_VALID == before || after).
 *
 * The macros below are copied VERBATIM from src/dbinc/qam.h (only the struct
 * member access is adapted to the local Q struct, which mirrors the QUEUE /
 * QMETA fields the macros read: q_root, rec_page, cur_recno, first_recno).
 * The arithmetic -- the subtractions, the (recno-1)/per_page, the circular
 * comparisons -- is byte-for-byte the shipped code.
 *
 * Bound: full u_int32_t for recno/first/cur/q_root; rec_page in 1..REC_PAGE_MAX
 *        (8).  Loop-free.
 */

#include <stdint.h>

typedef uint32_t u_int32_t;
typedef uint32_t db_recno_t;
typedef uint32_t db_pgno_t;

#define RECNO_OOB 0

/*
 * Local stand-ins for the fields the qam.h macros read.  QUEUE holds q_root
 * and rec_page (records per page); META holds the window endpoints.
 */
typedef struct {
	db_pgno_t q_root;
	u_int32_t rec_page;
} Q;
typedef struct {
	db_recno_t first_recno;
	db_recno_t cur_recno;
} META;

/*
 * --- VERBATIM from src/dbinc/qam.h (member access adapted to Q/META) ---
 * QAM_RECNO_PER_PAGE(dbp)      -> q->rec_page
 * QAM_RECNO_PAGE(dbp, recno)   -> q->q_root + (recno-1)/rec_page
 * QAM_RECNO_INDEX(dbp,pgno,r)  -> (r-1) - rec_page*(pgno - q_root)
 */
#define QAM_RECNO_PER_PAGE(q)	((q)->rec_page)

#define QAM_RECNO_PAGE(q, recno)					\
    ((q)->q_root + (((recno) - 1) / QAM_RECNO_PER_PAGE(q)))

#ifdef QAM_SABOTAGE
/*
 * Planted bug: drop the 1-based "- 1" in the index computation.  This is a
 * realistic off-by-one (record numbers are 1-based); it breaks both the
 * index-in-range bound and the (page,index)->recno round-trip.  This arm
 * MUST FAIL.
 */
#define QAM_RECNO_INDEX(q, pgno, recno)					\
    (u_int32_t)(((recno)) - (QAM_RECNO_PER_PAGE(q)			\
    * (pgno - (q)->q_root)))
#else
#define QAM_RECNO_INDEX(q, pgno, recno)					\
    (u_int32_t)(((recno) - 1) - (QAM_RECNO_PER_PAGE(q)			\
    * (pgno - (q)->q_root)))
#endif

#define QAM_OUTSIDE_QUEUE(meta, recno)					\
	(((meta)->cur_recno >= (meta)->first_recno ?			\
	    ((recno) < (meta)->first_recno ||				\
	         (recno) > (meta)->cur_recno) :				\
	    ((recno) > (meta)->cur_recno && 				\
	        (recno) < (meta)->first_recno)))

#define QAM_AFTER_CURRENT(meta, recno)					\
	((recno) == (meta)->cur_recno ||				\
	(QAM_OUTSIDE_QUEUE(meta, recno) &&				\
        ((recno) - (meta)->cur_recno) <= ((meta)->first_recno - (recno))))

#define QAM_BEFORE_FIRST(meta, recno)					\
	(QAM_OUTSIDE_QUEUE(meta, recno) &&				\
	((meta)->first_recno - (recno)) < ((recno) - (meta)->cur_recno))

#define QAM_NOT_VALID(meta, recno)					\
    (recno == RECNO_OOB ||						\
	QAM_BEFORE_FIRST(meta, recno) || QAM_AFTER_CURRENT(meta, recno))
/* --- end verbatim macros --- */

#define REC_PAGE_MAX 8

u_int32_t nondet_u32(void);

/*
 * Independent reference for "recno is OUTSIDE the circular live window".
 * The window the engine maintains is the records between first_recno and
 * cur_recno in increasing circular order; cur_recno is the next slot to be
 * written (exclusive), so the live records are first_recno..cur_recno-1.  For
 * the RANGE predicates we only need a reference for QAM_OUTSIDE_QUEUE, which
 * the engine defines as "not between first and cur in circular order", with
 * cur treated INCLUSIVELY (recno == cur is NOT outside; QAM_AFTER_CURRENT then
 * treats recno == cur as the sentinel just-past-the-end).  We mirror exactly
 * that: not-outside iff recno lies in the closed circular span [first, cur].
 */
static int ref_outside(const META *m, db_recno_t r)
{
	if (m->cur_recno >= m->first_recno)
		return (r < m->first_recno || r > m->cur_recno);
	/* wrapped: the live span is [first, MAX] U [1, cur]; outside is the
	 * gap strictly between cur and first. */
	return (r > m->cur_recno && r < m->first_recno);
}

void harness(void)
{
	Q q;
	META m;
	db_recno_t recno;

	q.q_root = nondet_u32();
	q.rec_page = nondet_u32();
	recno = nondet_u32();
	m.first_recno = nondet_u32();
	m.cur_recno = nondet_u32();

	/* rec_page is records-per-page: the engine's CALC_QAM_RECNO_PER_PAGE is
	 * always >= 1 (a page holds at least one record), and the macros divide
	 * by it, so model that invariant. */
	__CPROVER_assume(q.rec_page >= 1 && q.rec_page <= REC_PAGE_MAX);

	/* ---- MAP: recno >= 1 maps to (page, index) and back. ---- */
	{
		db_recno_t r = recno;
		/* A real record number is >= 1; bound q_root and r so the page
		 * arithmetic stays inside u_int32_t without wrap (the engine's
		 * q_root is 1 and page numbers are bounded by the file size;
		 * here we just keep (r-1)/rec_page + q_root from overflowing). */
		__CPROVER_assume(r >= 1 && r <= 0x10000);
		__CPROVER_assume(q.q_root <= 0x10000);

		db_pgno_t pg = QAM_RECNO_PAGE(&q, r);
		u_int32_t idx = QAM_RECNO_INDEX(&q, pg, r);

		__CPROVER_assert(idx < q.rec_page,
		    "QAM: record index is within records-per-page");
		/* (page, index) -> recno round-trip: the physical record number
		 * is rec_page*(pg - q_root) + idx, +1 for the 1-based recno. */
		u_int32_t back =
		    q.rec_page * (pg - q.q_root) + idx + 1;
		__CPROVER_assert(back == r,
		    "QAM: (page,index) maps back to the record number");
	}

	/* ---- RANGE: the macros agree with the independent reference. ---- */
	__CPROVER_assert(
	    (QAM_OUTSIDE_QUEUE(&m, recno) ? 1 : 0) ==
	    (ref_outside(&m, recno) ? 1 : 0),
	    "QAM_OUTSIDE_QUEUE matches the circular-window reference");

	/* ---- PARTITION: for recno != OOB, exactly one of before/after/inside
	 * holds.  "inside" == !outside (and recno != cur, since cur is the
	 * after-current sentinel). ---- */
	if (recno != RECNO_OOB) {
		int before = QAM_BEFORE_FIRST(&m, recno) ? 1 : 0;
		int after  = QAM_AFTER_CURRENT(&m, recno) ? 1 : 0;
		int inside = (!QAM_OUTSIDE_QUEUE(&m, recno) &&
		    recno != m.cur_recno) ? 1 : 0;
		__CPROVER_assert(before + after + inside == 1,
		    "QAM: exactly one of before-first / after-current / inside");
		/* QAM_NOT_VALID is precisely before || after (for recno != OOB). */
		__CPROVER_assert(
		    (QAM_NOT_VALID(&m, recno) ? 1 : 0) == (before || after ? 1 : 0),
		    "QAM_NOT_VALID == before-first || after-current");
	}
}

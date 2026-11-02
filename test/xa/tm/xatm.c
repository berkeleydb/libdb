/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * xatm.c --
 *	Implementation of the minimal test transaction manager.  See xatm.h.
 */

#include <sys/types.h>

#include <stdio.h>
#include <string.h>

#include "xatm.h"

/*
 * xatm_mkxid --
 *	XA spec §2.2 "The Transaction Identifier XID".  formatID identifies the
 *	naming authority; a value of -1 means a NULL XID (spec: "A value of -1
 *	in formatID means that the XID is null").  We use the printable
 *	constant 0x4442 ("DB") as every real TM uses its own non-negative id.
 *	gtrid and bqual are packed into data[] as [gtrid][bqual]; gtrid_length
 *	and bqual_length record their sizes (XA spec §2.2: 1..64 bytes each,
 *	data at most XIDDATASIZE = 128 bytes total).
 */
void
xatm_mkxid(xid, gtrid_no, branch_no)
	XID *xid;
	int gtrid_no, branch_no;
{
	memset(xid, 0, sizeof(*xid));
	xid->formatID = 0x4442;			/* "DB" */
	xid->gtrid_length = 8;
	xid->bqual_length = 4;
	/* gtrid: "GTxxxxxx" carrying gtrid_no; distinct global transactions
	 * get distinct gtrids (the XA "global transaction" identity). */
	(void)snprintf((char *)&xid->data[0], 9, "GT%06d", gtrid_no);
	/* bqual: "Bxxx" carrying branch_no; branches of the same global
	 * transaction (same gtrid) differ only here (XA §2.2 branch qualifier). */
	(void)snprintf((char *)&xid->data[8], 5, "B%03d", branch_no);
}

int
xatm_xid_eq(a, b)
	const XID *a, *b;
{
	if (a->formatID != b->formatID ||
	    a->gtrid_length != b->gtrid_length ||
	    a->bqual_length != b->bqual_length)
		return (0);
	return (memcmp(a->data, b->data,
	    (size_t)(a->gtrid_length + a->bqual_length)) == 0);
}

const char *
xatm_xaerr(code)
	int code;
{
	switch (code) {
	case XA_OK:		return ("XA_OK");
	case XA_RDONLY:		return ("XA_RDONLY");
	case XA_RETRY:		return ("XA_RETRY");
	case XA_HEURHAZ:	return ("XA_HEURHAZ");
	case XA_HEURCOM:	return ("XA_HEURCOM");
	case XA_HEURRB:		return ("XA_HEURRB");
	case XA_HEURMIX:	return ("XA_HEURMIX");
	case XA_RBROLLBACK:	return ("XA_RBROLLBACK");
	case XA_RBDEADLOCK:	return ("XA_RBDEADLOCK");
	case XA_RBOTHER:	return ("XA_RBOTHER");
	case XA_RBPROTO:	return ("XA_RBPROTO");
	case XA_RBTIMEOUT:	return ("XA_RBTIMEOUT");
	case XAER_ASYNC:	return ("XAER_ASYNC");
	case XAER_RMERR:	return ("XAER_RMERR");
	case XAER_NOTA:		return ("XAER_NOTA");
	case XAER_INVAL:	return ("XAER_INVAL");
	case XAER_PROTO:	return ("XAER_PROTO");
	case XAER_RMFAIL:	return ("XAER_RMFAIL");
	case XAER_DUPID:	return ("XAER_DUPID");
	case XAER_OUTSIDE:	return ("XAER_OUTSIDE");
	default:		return ("<unknown>");
	}
}

void
xatm_begin(h, name)
	xatm_h *h;
	const char *name;
{
	h->name = name;
	h->fails = 0;
	h->checks = 0;
	fprintf(stderr, "== %s ==\n", name);
}

int
xatm_chk_at(h, file, line, expr, got, want)
	xatm_h *h;
	const char *file, *expr;
	int line, got, want;
{
	h->checks++;
	if (got == want)
		return (1);
	h->fails++;
	fprintf(stderr, "FAIL: %s:%d: %s => %s (%d), expected %s (%d)\n",
	    file, line, expr, xatm_xaerr(got), got, xatm_xaerr(want), want);
	return (0);
}

int
xatm_chk_true_at(h, file, line, expr, cond)
	xatm_h *h;
	const char *file, *expr;
	int line, cond;
{
	h->checks++;
	if (cond)
		return (1);
	h->fails++;
	fprintf(stderr, "FAIL: %s:%d: %s is false\n", file, line, expr);
	return (0);
}

int
xatm_end(h)
	xatm_h *h;
{
	if (h->fails == 0) {
		fprintf(stderr, "%s: PASS (%d checks)\n", h->name, h->checks);
		return (0);
	}
	fprintf(stderr, "%s: FAIL (%d of %d checks failed)\n",
	    h->name, h->fails, h->checks);
	return (1);
}

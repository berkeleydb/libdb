/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * xatm.h --
 *	A minimal test transaction manager (TM) for libdb's X/Open XA
 *	resource-manager switch (db_xa_switch, src/xa/xa.c + xa_map.c).
 *
 *	This exists so the XA layer can be tested with NO Oracle Tuxedo
 *	install.  It drives the db_xa_switch entry points exactly as a real TM
 *	does: it builds a global transaction identifier (XID), calls the switch
 *	functions with an rmid, and drives the recovery scan.  Spec references
 *	in the .c files are to the X/Open CAE Specification, Distributed
 *	Transaction Processing: The XA Specification (X/Open Document C193,
 *	1991).
 */

#ifndef	XATM_H
#define	XATM_H

#include "db.h"
#include "xa.h"

#if defined(__cplusplus)
extern "C" {
#endif

/* db_xa_switch is exported by libdb but not declared in the public db.h. */
extern const struct xa_switch_t db_xa_switch;

/*
 * xatm_mkxid --
 *	Build a well-formed global transaction identifier (XID).
 *
 *	XA spec §2.2 "The Transaction Identifier XID": an XID carries a
 *	format identifier, a global-transaction id (gtrid) and a branch
 *	qualifier (bqual), packed into one `data` buffer as
 *	[gtrid bytes][bqual bytes].  A real TM makes gtrid unique per global
 *	transaction and bqual unique per branch (per RM) within it; two
 *	branches of the SAME global transaction share the gtrid and differ in
 *	bqual.  We encode gtrid from `gtrid_no` and bqual from `branch_no`.
 */
void xatm_mkxid __P((XID *, int gtrid_no, int branch_no));

/*
 * xatm_xid_eq --
 *	Compare two XIDs for equality (format + gtrid + bqual + data).
 */
int xatm_xid_eq __P((const XID *, const XID *));

/*
 * xatm_xaerr --
 *	Return the symbolic name of an XA return code (XA_OK, XAER_NOTA, ...)
 *	for readable assertion output.
 */
const char *xatm_xaerr __P((int));

/*
 * The test harness: a tiny assert facility that records the first failure's
 * line and keeps a running count, so a whole matrix runs to completion and
 * prints every failure rather than stopping at the first.
 */
typedef struct {
	const char *name;	/* program name, for the PASS/FAIL banner */
	int fails;		/* number of failed checks */
	int checks;		/* number of checks run */
} xatm_h;

void xatm_begin __P((xatm_h *, const char *name));

/*
 * xatm_chk --
 *	Record that `got` should equal `want` (an XA return code).  Returns
 *	non-zero when they matched, so a caller can branch on success without
 *	a second comparison.
 */
int xatm_chk_at __P((xatm_h *, const char *file, int line,
    const char *expr, int got, int want));

/*
 * xatm_chk_true --
 *	Record a boolean condition (used for data checks).
 */
int xatm_chk_true_at __P((xatm_h *, const char *file, int line,
    const char *expr, int cond));

/*
 * xatm_end --
 *	Print the PASS/FAIL banner and return the process exit code
 *	(0 == all checks passed).
 */
int xatm_end __P((xatm_h *));

#define	XCHK(h, call, want)						\
	xatm_chk_at((h), __FILE__, __LINE__, #call, (call), (want))
#define	XCHK_TRUE(h, cond)						\
	xatm_chk_true_at((h), __FILE__, __LINE__, #cond, (cond))

#if defined(__cplusplus)
}
#endif
#endif /* !XATM_H */

/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness: __logc_hdrchk (src/log/log_get.c).  (RFC 0025 L1.)
 *
 * __logc_hdrchk is the guard recovery and DB_LOGC->get run over a log record
 * header read from disk BEFORE trusting hdr->len to allocate a buffer and copy
 * a record out.  A header that passes must be internally consistent, or
 * recovery over-allocates / over-copies on a corrupt log.
 *
 *   PROPERTY: if __logc_hdrchk returns 0 and reports NOT end-of-file
 *   (*eofp == 0), then
 *     (1) hdr->len > hdr->size            (the record is larger than its own
 *                                          header -- there is a body to read);
 *     (2) when bp_maxrec != 0 (we are reading from a log file),
 *         hdr->len <= bp_maxrec           (not an impossibly large record).
 *
 * The function body below is copied VERBATIM from src/log/log_get.c between
 * the BEGIN/END markers (guarded by check_copies.sh).  What is stubbed (named
 * in each stub's comment):
 *   - __logc_set_maxrec: the real one re-reads the max-record size from a
 *     growing last log file.  The brief stubs it; here it returns 0 and leaves
 *     bp_maxrec unchanged, so the post-condition is checked against the value
 *     the function actually decided with.  To keep the proof honest about the
 *     re-read, the harness ALSO runs a variant where the stub lowers bp_maxrec
 *     (it can only shrink what passes), which still must uphold the property.
 *   - __db_err / __db_errx: error reporting; no-ops (the err: path returns EIO,
 *     not 0, so it never reaches the success post-condition).
 *   - F_ISSET: the real bit test, over a nondet flags word.
 *
 * Bound: all field values nondet; loop-free.
 */

#include <stdint.h>
#include <stddef.h>

typedef uint8_t  u_int8_t;
typedef uint32_t u_int32_t;
typedef unsigned long u_long;

#define EIO 5
#define DB_MAC_KEY 20
#define DB_IV_BYTES 16
#define DB_LOG_SILENT_ERR 0x04

#define F_ISSET(p, f) ((p)->flags & (f))
#define DB_STR_A(id, msg, fmt) (msg)
#define COMPQUIET(x, v) do { (x) = (v); } while (0)

typedef struct { u_int32_t file, offset; } DB_LSN;

/* HDR: mirrors src/dbinc/log.h struct __hdr (the fields hdrchk reads). */
typedef struct __hdr {
	u_int32_t prev;
	u_int32_t len;
	u_int8_t  chksum[DB_MAC_KEY];
	u_int8_t  iv[DB_IV_BYTES];
	u_int32_t orig_size;
	size_t    size;
} HDR;

typedef struct { int unused; } ENV;

/* DB_LOGC: only the fields hdrchk reads (src/dbinc/db.in struct
 * __db_log_cursor): env, bp_maxrec, flags. */
typedef struct __db_log_cursor {
	ENV      *env;
	u_int32_t bp_maxrec;
	u_int32_t flags;
} DB_LOGC;

/* __logc_set_maxrec stub: see header comment.  SET_MAXREC_SHRINK lowers
 * bp_maxrec (models the re-read finding a smaller max); otherwise it is a
 * no-op.  Either way it returns success. */
u_int32_t nondet_u32(void);
static int
__logc_set_maxrec(DB_LOGC *logc, char *name)
{
	COMPQUIET(name, (char *)0);
#ifdef L1_SET_MAXREC_SHRINK
	{
		u_int32_t nv = nondet_u32();
		__CPROVER_assume(nv <= logc->bp_maxrec);
		logc->bp_maxrec = nv;
	}
#endif
	return (0);
}

/* Error reporting stubs: no-ops.  The err: label returns EIO, so these are
 * never on a path that returns 0. */
static void __db_err(ENV *env, int ret, const char *fmt, ...) { (void)env; (void)ret; (void)fmt; }
static void __db_errx(ENV *env, const char *fmt, ...) { (void)env; (void)fmt; }

/* --- BEGIN verbatim copy of __logc_hdrchk from src/log/log_get.c --- */
static int
__logc_hdrchk(logc, lsn, hdr, eofp)
	DB_LOGC *logc;
	DB_LSN *lsn;
	HDR *hdr;
	int *eofp;
{
	ENV *env;
	int ret;

	env = logc->env;

	/*
	 * Check EOF before we do any other processing.
	 */
	if (eofp != NULL) {
		if (hdr->prev == 0 && hdr->chksum[0] == 0 && hdr->len == 0) {
			*eofp = 1;
			return (0);
		}
		*eofp = 0;
	}

	/*
	 * Sanity check the log record's size.
	 * We must check it after "virtual" EOF above.
	 */
	if (hdr->len <= hdr->size)
		goto err;

	/*
	 * If the cursor's max-record value isn't yet set, it means we aren't
	 * reading these records from a log file and no check is necessary.
	 */
	if (logc->bp_maxrec != 0 && hdr->len > logc->bp_maxrec) {
		/*
		 * If we fail the check, there's the pathological case that
		 * we're reading the last file, it's growing, and our initial
		 * check information was wrong.  Get it again, to be sure.
		 */
		if ((ret = __logc_set_maxrec(logc, NULL)) != 0) {
			__db_err(env, ret, "DB_LOGC->get");
			return (ret);
		}
		if (logc->bp_maxrec != 0 && hdr->len > logc->bp_maxrec)
			goto err;
	}
	return (0);

err:	if (!F_ISSET(logc, DB_LOG_SILENT_ERR))
		__db_errx(env, DB_STR_A("2580",
		    "DB_LOGC->get: LSN %lu/%lu: invalid log record header",
		    "%lu %lu"), (u_long)lsn->file, (u_long)lsn->offset);
	return (EIO);
}
/* --- END verbatim copy --- */

/*
 * SABOTAGE variant: the brief's planted bug -- `<` for `<=` in the record-size
 * sanity check.  With `<`, a header whose len EQUALS its size (a record with no
 * body) passes, so property (1) -- hdr->len > hdr->size on a non-EOF OK -- must
 * FAIL.  The non-sabotage build uses the verbatim __logc_hdrchk above; this
 * hand-written copy exists only so the sabotage has teeth, and is not guarded
 * by check_copies.sh (it is deliberately different from src/).
 */
#ifdef L1_SABOTAGE
static int
__logc_hdrchk_sab(logc, lsn, hdr, eofp)
	DB_LOGC *logc;
	DB_LSN *lsn;
	HDR *hdr;
	int *eofp;
{
	ENV *env;
	int ret;

	env = logc->env;

	if (eofp != NULL) {
		if (hdr->prev == 0 && hdr->chksum[0] == 0 && hdr->len == 0) {
			*eofp = 1;
			return (0);
		}
		*eofp = 0;
	}

	if (hdr->len < hdr->size)		/* SABOTAGE: was <= */
		goto err;

	if (logc->bp_maxrec != 0 && hdr->len > logc->bp_maxrec) {
		if ((ret = __logc_set_maxrec(logc, NULL)) != 0) {
			__db_err(env, ret, "DB_LOGC->get");
			return (ret);
		}
		if (logc->bp_maxrec != 0 && hdr->len > logc->bp_maxrec)
			goto err;
	}
	return (0);

err:	if (!F_ISSET(logc, DB_LOG_SILENT_ERR))
		__db_errx(env, DB_STR_A("2580",
		    "DB_LOGC->get: LSN %lu/%lu: invalid log record header",
		    "%lu %lu"), (u_long)lsn->file, (u_long)lsn->offset);
	return (EIO);
}
#define HDRCHK(l, s, h, e) __logc_hdrchk_sab(l, s, h, e)
#else
#define HDRCHK(l, s, h, e) __logc_hdrchk(l, s, h, e)
#endif

u_int8_t nondet_u8(void);
size_t nondet_size(void);

void harness(void)
{
	DB_LOGC logc;
	DB_LSN lsn;
	HDR hdr;
	int eof, rc;
	unsigned i;

	logc.env = (ENV *)0;
	logc.bp_maxrec = nondet_u32();
	logc.flags = nondet_u32();

	lsn.file = nondet_u32();
	lsn.offset = nondet_u32();

	hdr.prev = nondet_u32();
	hdr.len = nondet_u32();
	for (i = 0; i < DB_MAC_KEY; i++)
		hdr.chksum[i] = nondet_u8();
	hdr.orig_size = nondet_u32();
	hdr.size = nondet_size();

	eof = -1;
	rc = HDRCHK(&logc, &lsn, &hdr, &eof);

	if (rc == 0 && eof == 0) {
		__CPROVER_assert(hdr.len > hdr.size,
		    "hdrchk-OK non-EOF: record length exceeds its header size");
		if (logc.bp_maxrec != 0)
			__CPROVER_assert(hdr.len <= logc.bp_maxrec,
			    "hdrchk-OK non-EOF: record length within bp_maxrec");
	}
}

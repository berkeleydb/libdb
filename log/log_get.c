/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)log_get.c	11.4 (Sleepycat) 9/16/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "log.h"
#include "hash.h"

/*
 * log_get --
 *	Get a log record.
 */
int
log_get(dbenv, alsn, dbt, flags)
	DB_ENV *dbenv;
	DB_LSN *alsn;
	DBT *dbt;
	u_int32_t flags;
{
	DB_LOG *dblp;
	int ret;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv, dbenv->lg_handle, DB_INIT_LOG);

	/* Validate arguments. */
	if (flags != DB_CHECKPOINT && flags != DB_CURRENT &&
	    flags != DB_FIRST && flags != DB_LAST &&
	    flags != DB_NEXT && flags != DB_PREV && flags != DB_SET)
		return (__db_ferr(dbenv, "log_get", 1));

	if (F_ISSET(dbenv, DB_ENV_THREAD)) {
		if (flags == DB_NEXT || flags == DB_PREV || flags == DB_CURRENT)
			return (__db_ferr(dbenv, "log_get", 1));
		if (!F_ISSET(dbt,
		    DB_DBT_MALLOC | DB_DBT_REALLOC | DB_DBT_USERMEM))
			return (__db_ferr(dbenv, "threaded data", 1));
	}

	dblp = dbenv->lg_handle;
	R_LOCK(dbenv, &dblp->reginfo);

	/*
	 * If we get one of the log's header records, repeat the operation.
	 * This assumes that applications don't ever request the log header
	 * records by LSN, but that seems reasonable to me.
	 */
	ret = __log_get(dblp, alsn, dbt, flags, 0);
	if (ret == 0 && alsn->offset == 0) {
		switch (flags) {
		case DB_FIRST:
			flags = DB_NEXT;
			break;
		case DB_LAST:
			flags = DB_PREV;
			break;
		}
		ret = __log_get(dblp, alsn, dbt, flags, 0);
	}

	R_UNLOCK(dbenv, &dblp->reginfo);

	return (ret);
}

/*
 * __log_get --
 *	Get a log record; internal version.
 *
 * PUBLIC: int __log_get __P((DB_LOG *, DB_LSN *, DBT *, u_int32_t, int));
 */
int
__log_get(dblp, alsn, dbt, flags, silent)
	DB_LOG *dblp;
	DB_LSN *alsn;
	DBT *dbt;
	u_int32_t flags;
	int silent;
{
	DB_LSN nlsn;
	HDR hdr;
	LOG *lp;
	size_t len;
	ssize_t nr;
	int cnt, ret;
	char *np, *tbuf;
	const char *fail;
	void *shortp;
	u_int8_t *p;

	lp = dblp->reginfo.primary;
	fail = np = tbuf = NULL;

	nlsn = dblp->c_lsn;
	switch (flags) {
	case DB_CHECKPOINT:
		nlsn = lp->chkpt_lsn;
		if (IS_ZERO_LSN(nlsn)) {
			ret = ENOENT;
			goto err2;
		}
		break;
	case DB_NEXT:				/* Next log record. */
		if (!IS_ZERO_LSN(nlsn)) {
			/* Increment the cursor by the cursor record size. */
			nlsn.offset += dblp->c_len;
			break;
		}
		/* FALLTHROUGH */
	case DB_FIRST:				/* Find the first log record. */
		/* Find the first log file. */
		if ((ret = __log_find(dblp, 1, &cnt)) != 0)
			goto err2;

		/*
		 * We may have only entered records in the buffer, and not
		 * yet written a log file.  If no log files were found and
		 * there's anything in the buffer, it belongs to file 1.
		 */
		if (cnt == 0)
			cnt = 1;

		nlsn.file = cnt;
		nlsn.offset = 0;
		break;
	case DB_CURRENT:			/* Current log record. */
		break;
	case DB_PREV:				/* Previous log record. */
		if (!IS_ZERO_LSN(nlsn)) {
			/* If at start-of-file, move to the previous file. */
			if (nlsn.offset == 0) {
				if (nlsn.file == 1 ||
				    __log_valid(dblp, nlsn.file - 1, 0) != 0)
					return (DB_NOTFOUND);

				--nlsn.file;
				nlsn.offset = dblp->c_off;
			} else
				nlsn.offset = dblp->c_off;
			break;
		}
		/* FALLTHROUGH */
	case DB_LAST:				/* Last log record. */
		nlsn.file = lp->lsn.file;
		nlsn.offset = lp->lsn.offset - lp->len;
		break;
	case DB_SET:				/* Set log record. */
		nlsn = *alsn;
		break;
	}

	if (0) {				/* Move to the next file. */
next_file:	++nlsn.file;
		nlsn.offset = 0;
	}

	/* Return 1 if the request is past the end of the log. */
	if (nlsn.file > lp->lsn.file ||
	    (nlsn.file == lp->lsn.file && nlsn.offset >= lp->lsn.offset))
		return (DB_NOTFOUND);

	/* If we've switched files, discard the current file handle. */
	if (dblp->c_lsn.file != nlsn.file &&
	    F_ISSET(&dblp->c_fh, DB_FH_VALID))
		(void)__os_closehandle(&dblp->c_fh);

	/* If the entire record is in the in-memory buffer, copy it out. */
	if (nlsn.file == lp->lsn.file && nlsn.offset >= lp->w_off) {
		/* Copy the header. */
		p = dblp->bufp + (nlsn.offset - lp->w_off);
		memcpy(&hdr, p, sizeof(HDR));

		/* Copy the record. */
		len = hdr.len - sizeof(HDR);
		if ((ret = __db_retcopy(NULL, dbt, p + sizeof(HDR),
		    len, &dblp->c_dbt.data, &dblp->c_dbt.ulen)) != 0)
			goto err1;
		goto cksum;
	}

	/* Acquire a file descriptor. */
	if (!F_ISSET(&dblp->c_fh, DB_FH_VALID)) {
		if ((ret = __log_name(dblp, nlsn.file,
		    &np, &dblp->c_fh, DB_OSO_RDONLY | DB_OSO_SEQ)) != 0) {
			fail = np;
			goto err1;
		}
		__os_freestr(np);
		np = NULL;
	}

	/*
	 * Seek to the header offset and read the header.  Because the file
	 * may be pre-allocated, we have to make sure that we're not reading
	 * past the information in the start of the in-memory buffer.
	 */
	if ((ret = __os_seek(
	    &dblp->c_fh, 0, 0, nlsn.offset, 0, DB_OS_SEEK_SET)) != 0) {
		fail = "seek";
		goto err1;
	}
	if (nlsn.file == lp->lsn.file && nlsn.offset + sizeof(HDR) > lp->w_off)
		nr = lp->w_off - nlsn.offset;
	else
		nr = sizeof(HDR);
	if ((ret = __os_read(&dblp->c_fh, &hdr, nr, &nr)) != 0) {
		fail = "read";
		goto err1;
	}
	if (nr == sizeof(HDR))
		shortp = NULL;
	else {
		/* If read returns EOF, try the next file. */
		if (nr == 0) {
			if (flags != DB_NEXT || nlsn.file == lp->lsn.file)
				goto corrupt;
			goto next_file;
		}

		/*
		 * If read returns a short count the rest of the record has
		 * to be in the in-memory buffer.
		 */
		if (lp->b_off < sizeof(HDR) - nr)
			goto corrupt;

		/* Get the rest of the header from the in-memory buffer. */
		memcpy((u_int8_t *)&hdr + nr, dblp->bufp, sizeof(HDR) - nr);
		shortp = dblp->bufp + (sizeof(HDR) - nr);
	}

	/*
	 * Check for buffers of 0's, that's what we usually see during recovery,
	 * although it's certainly not something on which we can depend.  Check
	 * for impossibly large records.  The malloc should fail later, but we
	 * have customers that run mallocs that handle allocation failure as a
	 * fatal error.
	 */
	if (hdr.len == 0)
		goto next_file;
	if (hdr.len <= sizeof(HDR) || hdr.len > lp->persist.lg_max)
		goto corrupt;
	len = hdr.len - sizeof(HDR);

	/* If we've already moved to the in-memory buffer, fill from there. */
	if (shortp != NULL) {
		if (lp->b_off < ((u_int8_t *)shortp - dblp->bufp) + len)
			goto corrupt;
		if ((ret = __db_retcopy(NULL, dbt, shortp, len,
		    &dblp->c_dbt.data, &dblp->c_dbt.ulen)) != 0)
			goto err1;
		goto cksum;
	}

	/*
	 * Allocate temporary memory to hold the record.
	 *
	 * XXX
	 * We're calling malloc(3) with a region locked.  This isn't
	 * a good idea.
	 */
	if ((ret = __os_malloc(len, NULL, &tbuf)) != 0)
		goto err1;

	/*
	 * Read the record into the buffer.  If read returns a short count,
	 * there was an error or the rest of the record is in the in-memory
	 * buffer.  Note, the information may be garbage if we're in recovery,
	 * so don't read past the end of the buffer's memory.
	 *
	 * Because the file may be pre-allocated, we have to make sure that
	 * we're not reading past the information in the start of the in-memory
	 * buffer.
	 */
	if (nlsn.file == lp->lsn.file &&
	    nlsn.offset + sizeof(HDR) + len > lp->w_off)
		nr = lp->w_off - (nlsn.offset + sizeof(HDR));
	else
		nr = len;
	if ((ret = __os_read(&dblp->c_fh, tbuf, nr, &nr)) != 0) {
		fail = "read";
		goto err1;
	}
	if (len - nr > lp->buffer_size)
		goto corrupt;
	if (nr != (ssize_t)len) {
		if (lp->b_off < len - nr)
			goto corrupt;

		/* Get the rest of the record from the in-memory buffer. */
		memcpy((u_int8_t *)tbuf + nr, dblp->bufp, len - nr);
	}

	/* Copy the record into the user's DBT. */
	if ((ret = __db_retcopy(NULL, dbt, tbuf, len,
	    &dblp->c_dbt.data, &dblp->c_dbt.ulen)) != 0)
		goto err1;
	__os_free(tbuf, 0);
	tbuf = NULL;

cksum:	if (hdr.cksum != __ham_func4(dbt->data, dbt->size)) {
		if (!silent)
			__db_err(dblp->dbenv, "log_get: checksum mismatch");
		goto corrupt;
	}

	/* Update the cursor and the return lsn. */
	dblp->c_off = hdr.prev;
	dblp->c_len = hdr.len;
	dblp->c_lsn = *alsn = nlsn;

	return (0);

corrupt:/*
	 * This is the catchall -- for some reason we didn't find enough
	 * information or it wasn't reasonable information, and it wasn't
	 * because a system call failed.
	 */
	ret = EIO;
	fail = "read";

err1:	if (!silent) {
		if (fail == NULL)
			__db_err(dblp->dbenv, "log_get: %s", db_strerror(ret));
		else
			__db_err(dblp->dbenv,
			    "log_get: %s: %s", fail, db_strerror(ret));
	}
err2:	if (np != NULL)
		__os_freestr(np);
	if (tbuf != NULL)
		__os_free(tbuf, 0);
	return (ret);
}

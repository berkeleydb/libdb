/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_rw.c	11.2 (Sleepycat) 9/20/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <unistd.h>
#endif

#include "db_int.h"
#include "os_jump.h"

/*
 * __os_io --
 *	Do an I/O.
 *
 * PUBLIC: int __os_io __P((DB_IO *, int, ssize_t *));
 */
int
__os_io(db_iop, op, niop)
	DB_IO *db_iop;
	int op;
	ssize_t *niop;
{
	int ret;

#ifdef HAVE_PREAD
	switch (op) {
	case DB_IO_READ:
		if (__db_jump.j_read != NULL)
			goto slow;
		*niop = pread(db_iop->fhp->fd, db_iop->buf,
		    db_iop->bytes, (off_t)db_iop->pgno * db_iop->pagesize);
		break;
	case DB_IO_WRITE:
		if (__db_jump.j_write != NULL)
			goto slow;
		*niop = pwrite(db_iop->fhp->fd, db_iop->buf,
		    db_iop->bytes, (off_t)db_iop->pgno * db_iop->pagesize);
		break;
	}
	if (*niop == (ssize_t)db_iop->bytes)
		return (0);
slow:
#endif
	MUTEX_THREAD_LOCK(db_iop->mutexp);

	if ((ret = __os_seek(db_iop->fhp,
	    db_iop->pagesize, db_iop->pgno, 0, 0, DB_OS_SEEK_SET)) != 0)
		goto err;
	switch (op) {
	case DB_IO_READ:
		ret =
		    __os_read(db_iop->fhp, db_iop->buf, db_iop->bytes, niop);
		break;
	case DB_IO_WRITE:
		ret =
		    __os_write(db_iop->fhp, db_iop->buf, db_iop->bytes, niop);
		break;
	}

err:	MUTEX_THREAD_UNLOCK(db_iop->mutexp);

	return (ret);

}

/*
 * __os_read --
 *	Read from a file handle.
 *
 * PUBLIC: int __os_read __P((DB_FH *, void *, size_t, ssize_t *));
 */
int
__os_read(fhp, addr, len, nrp)
	DB_FH *fhp;
	void *addr;
	size_t len;
	ssize_t *nrp;
{
	size_t offset;
	ssize_t nr;
	u_int8_t *taddr;

	for (taddr = addr,
	    offset = 0; offset < len; taddr += nr, offset += nr) {
		if ((nr = __db_jump.j_read != NULL ?
		    __db_jump.j_read(fhp->fd, taddr, len - offset) :
		    read(fhp->fd, taddr, len - offset)) < 0)
			return (__os_get_errno());
		if (nr == 0)
			break;
	}
	*nrp = taddr - (u_int8_t *)addr;
	return (0);
}

/*
 * __os_write --
 *	Write to a file handle.
 *
 * PUBLIC: int __os_write __P((DB_FH *, void *, size_t, ssize_t *));
 */
int
__os_write(fhp, addr, len, nwp)
	DB_FH *fhp;
	void *addr;
	size_t len;
	ssize_t *nwp;
{
	size_t offset;
	ssize_t nw;
	u_int8_t *taddr;

	for (taddr = addr,
	    offset = 0; offset < len; taddr += nw, offset += nw)
		if ((nw = __db_jump.j_write != NULL ?
		    __db_jump.j_write(fhp->fd, taddr, len - offset) :
		    write(fhp->fd, taddr, len - offset)) < 0)
			return (__os_get_errno());
	*nwp = len;
	return (0);
}

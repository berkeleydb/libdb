/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_fid.c	11.1 (Sleepycat) 7/25/99";
#endif /* not lint */

#include "db_int.h"

/*
 * __os_fileid --
 *	Return a unique identifier for a file.
 */
int
__os_fileid(dbenv, fname, timestamp, fidp)
	DB_ENV *dbenv;
	const char *fname;
	int timestamp;
	u_int8_t *fidp;
{
	size_t i;
	u_int32_t tmp;
	u_int8_t *p;
	int ret;

	/*
	 * The documentation for GetFileInformationByHandle() states that the
	 * inode-type numbers are not constant between processes.  Actually,
	 * they are, they're the NTFS MFT indexes.  So, this works on NTFS,
	 * but perhaps not on other platforms, and perhaps not over a network.
	 * Can't think of a better solution right now.
	 */
	DB_FH fh;
	HANDLE handle;
	BY_HANDLE_FILE_INFORMATION fi;
	BOOL retval = FALSE;

	/* Clear the buffer. */
	memset(fidp, 0, DB_FILE_ID_LEN);

	/*
	 * First we open the file, because we're not given a handle to it.
	 * If we can't open it, we're in trouble.
	 */
	if ((ret = __os_open(fname, DB_OSO_RDONLY, _S_IREAD, &fh)) != 0)
		return (ret);

	/* File open, get its info */
	handle = (HANDLE)_get_osfhandle(fh.fd);
	if (handle == INVALID_HANDLE_VALUE)
		ret = __os_win32_errno();
	else
		if ((retval = GetFileInformationByHandle(handle, &fi)) == FALSE)
			ret = __os_win32_errno();
	__os_closehandle(&fh);

	if (handle == INVALID_HANDLE_VALUE || retval == FALSE)
		return (ret);

	/*
	 * We want the three 32-bit words which tell us the volume ID and
	 * the file ID.  We make a crude attempt to copy the bytes over to
	 * the callers buffer.
	 *
	 * We don't worry about byte sexing or the actual variable sizes.
	 *
	 * When this routine is called from the DB access methods, it's only
	 * called once -- whatever ID is generated when a database is created
	 * is stored in the database file's metadata, and that is what is
	 * saved in the mpool region's information to uniquely identify the
	 * file.
	 *
	 * When called from the mpool layer this routine will be called each
	 * time a new thread of control wants to share the file, which makes
	 * things tougher.  As far as byte sexing goes, since the mpool region
	 * lives on a single host, there's no issue of that -- the entire
	 * region is byte sex dependent.  As far as variable sizes go, we make
	 * the simplifying assumption that 32-bit and 64-bit processes will
	 * get the same 32-bit values if we truncate any returned 64-bit value
	 * to a 32-bit value.
	 */
	memcpy(fidp, &fi.nFileIndexLow, sizeof(u_int32_t));
	fidp += sizeof(u_int32_t);
	memcpy(fidp, &fi.nFileIndexHigh, sizeof(u_int32_t));
	fidp += sizeof(u_int32_t);
	memcpy(fidp, &fi.dwVolumeSerialNumber, sizeof(u_int32_t));
	fidp += sizeof(u_int32_t);

	if (timestamp) {
		/*
		 * We want the number of seconds, not the high-order 0 bits,
		 * so convert the returned time_t to a (potentially) smaller
		 * fixed-size type.
		 */
		tmp = (u_int32_t)time(NULL);
		for (p = (u_int8_t *)&tmp, i = sizeof(u_int32_t); i > 0; --i)
			*fidp++ = *p++;
	}
	return (0);
}

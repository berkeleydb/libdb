/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_map.c	10.7 (Sleepycat) 10/25/97";
#endif /* not lint */

#include "db_int.h"

/*
 * __os_map --
 *	Map in some shared memory backed by a file descriptor.
 */
int
__os_map(fd, len, is_private, is_rdonly, addr)
	int fd, is_private, is_rdonly;
	size_t len;
	void **addr;
{
	/* We have not implemented copy-on-write here */
	void * pMemory = 0;
	HANDLE hFile = (HANDLE)_get_osfhandle(fd);
	HANDLE hMemory = CreateFileMapping(
	      hFile,
	      0,
	      (is_rdonly ? PAGE_READONLY : PAGE_READWRITE),
	      0,
	      len, /* This code fails if the library is ever compiled on a 64-bit machine */
	      0
	      );
	if (NULL == hMemory)
	{
	      return errno;
	}
	pMemory = MapViewOfFile(
	      hMemory,
	      (is_rdonly ? FILE_MAP_READ : FILE_MAP_ALL_ACCESS),
	      0,
	      0,
	      len
	      );
	CloseHandle(hMemory);
	*addr = pMemory;
	return 0;
}

/*
 * __os_unmap --
 *	Release the specified shared memory.
 *
 * PUBLIC: int __os_unmap __P((void *, size_t));
 */
int
__os_unmap(addr, len)
	void *addr;
	size_t len;
{
	/*
	 * !!!
	 * The argument len is always the same length as was mapped.
	 */
	return (!UnmapViewOfFile(addr) ? errno : 0);
}

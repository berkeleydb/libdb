/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

/*
 * bsearch --
 *
 * PUBLIC: #ifndef HAVE_BSEARCH
 * PUBLIC: void *bsearch __P((const void *, const void *, size_t,
 * PUBLIC:	size_t, int (*)(const void *, const void *)));
 * PUBLIC: #endif
 */

void *bsearch(key, base, nmemb, size, cmp)
	const void *key;
	const void *base;
	size_t nmemb;
	size_t size;
	int (*cmp) __P((const void *, const void *));
{
	size_t i;

	/* not doing a binary search, but searching linearly */
	for (i=0; i < nmemb; i++) {
		if (*(pid_t*)key - *((pid_t*)base + i) == 0)
			return ((pid_t*)base + i);
	}

	return (NULL);
}

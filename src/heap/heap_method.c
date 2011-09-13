/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/heap.h"

/*
 * __heap_db_create --
 *	Heap specific initialization of the DB structure.
 *
 * PUBLIC: int __heap_db_create __P((DB *));
 */
int
__heap_db_create(dbp)
	DB *dbp;
{
	HEAP *h;
	int ret;

	if ((ret = __os_calloc(dbp->env, 1, sizeof(HEAP), &h)) != 0)
		return (ret);
	dbp->heap_internal = h;
	h->region_size = HEAP_DEFAULT_REGION_MAX;

	dbp->get_heapsize = __heap_get_heapsize;
	dbp->set_heapsize = __heap_set_heapsize;

	return (0);
}

/*
 * __heap_db_close --
 *      Heap specific discard of the DB structure.
 *
 * PUBLIC: int __heap_db_close __P((DB *));
 */
int
__heap_db_close(dbp)
	DB *dbp;
{
	HEAP *h;
	int ret;
	
	ret = 0;
	if ((h = dbp->heap_internal) == NULL)
		return (0);

	__os_free(dbp->env, h);
	dbp->heap_internal = NULL;

	return (0);
}

/*
 * __heap_get_heapsize --
 *	Get the initial size of the heap.
 *
 * PUBLIC: int __heap_get_heapsize __P((DB *, u_int32_t *, u_int32_t *));
 */
int
__heap_get_heapsize(dbp, gbytes, bytes)
	DB *dbp;
	u_int32_t *gbytes, *bytes;
{
	HEAP *h;

	DB_ILLEGAL_METHOD(dbp, DB_OK_HEAP);

	h = dbp->heap_internal;
	*gbytes = h->gbytes;
	*bytes = h->bytes;

	return (0);
}

/*
 * __heap_set_heapsize --
 *	Set the initial size of the heap.
 *
 * PUBLIC: int __heap_set_heapsize __P((DB *, u_int32_t, u_int32_t, u_int32_t));
 */
int
__heap_set_heapsize(dbp, gbytes, bytes, flags)
	DB *dbp;
	u_int32_t gbytes, bytes, flags;
{
	HEAP *h;

	DB_ILLEGAL_AFTER_OPEN(dbp, "DB->set_heapsize");
	DB_ILLEGAL_METHOD(dbp, DB_OK_HEAP);

	COMPQUIET(flags, 0);
	h = dbp->heap_internal;
	h->gbytes = gbytes;
	h->bytes = bytes;

	return (0);
}

/*
 * __heap_exist --
 * 	Test to see if heap exists or not, used in Perl interface
 *
 * PUBLIC: int __heap_exist __P((void));
 */
int
__heap_exist()
{
	return (1);
}

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

DB_GLOBALS __db_global_values = {
#ifdef HAVE_VXWORKS
	0,				/* VxWorks: initialized */
	NULL,				/* VxWorks: global semaphore */
#endif

	"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=",
	{ 0 },
	0,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL
};

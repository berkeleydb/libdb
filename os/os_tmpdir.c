/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_tmpdir.c	11.1 (Sleepycat) 7/25/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#endif

#include "db_int.h"

#ifdef macintosh
#include <TFileSpec.h>
#endif

/*
 * __os_tmpdir --
 *	Set the temporary directory path.
 *
 * The order of items in the list structure and the order of checks in
 * the environment are documented.
 *
 * PUBLIC: int __os_tmpdir __P((DB_ENV *, u_int32_t));
 */
int
__os_tmpdir(dbenv, flags)
	DB_ENV *dbenv;
	u_int32_t flags;
{
	/*
	 * !!!
	 * Don't change this to:
	 *
	 *	static const char * const list[]
	 *
	 * because it creates a text relocation in position independent code.
	 */
	static const char * list[] = {
		"/var/tmp",
		"/usr/tmp",
		"/temp",		/* Windows. */
		"/tmp",
		"C:/temp",		/* Windows. */
		"C:/tmp",		/* Windows. */
		NULL
	};
	const char * const *lp, *p;

	/* Use the environment if it's permitted and initialized. */
	p = NULL;
	if (LF_ISSET(DB_USE_ENVIRON) ||
	    (LF_ISSET(DB_USE_ENVIRON_ROOT) && __os_isroot() == 0)) {
		if ((p = getenv("TMPDIR")) != NULL && p[0] == '\0') {
			__db_err(dbenv, "illegal TMPDIR environment variable");
			return (EINVAL);
		}
		/* Windows */
		if (p == NULL && (p = getenv("TEMP")) != NULL && p[0] == '\0') {
			__db_err(dbenv, "illegal TEMP environment variable");
			return (EINVAL);
		}
		/* Windows */
		if (p == NULL && (p = getenv("TMP")) != NULL && p[0] == '\0') {
			__db_err(dbenv, "illegal TMP environment variable");
			return (EINVAL);
		}
		/* Macintosh */
		if (p == NULL &&
		    (p = getenv("TempFolder")) != NULL && p[0] == '\0') {
			__db_err(dbenv,
			    "illegal TempFolder environment variable");
			return (EINVAL);
		}
	}

#ifdef macintosh
	/* Get the path to the temporary folder. */
	if (p == NULL) {
		FSSpec spec;

		if (!Special2FSSpec(kTemporaryFolderType,
		    kOnSystemDisk, 0, &spec))
			(void)__os_strdup(FSp2FullPath(&spec), &p);
	}
#endif

	/* Step through the list looking for a possibility. */
	if (p == NULL)
		for (lp = list; *lp != NULL; ++lp)
			if (__os_exists(p = *lp, NULL) == 0)
				break;
	if (p == NULL)
		return (0);

	return (__os_strdup(p, &dbenv->db_tmp_dir));
}

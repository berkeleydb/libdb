/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)env_method.c	11.5 (Sleepycat) 9/16/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_shash.h"
#include "db_page.h"
#include "db_am.h"
#include "lock.h"
#include "log.h"
#include "mp.h"
#include "txn.h"

DB_GLOBALS __db_global_values = {
	1,					/* db_set_mutexlocks */
	0,					/* db_set_pageyield */
	1,					/* db_set_panic */
	0,					/* db_set_region_init */
	0,					/* db_set_tas_spins */
						/* XA environment queue */
        {NULL, &__db_global_values.db_envq.tqh_first}
};

static void __dbenv_err __P((const DB_ENV *, int, const char *, ...));
static void __dbenv_errx __P((const DB_ENV *, const char *, ...));
static void __dbenv_set_errcall __P((DB_ENV *, void (*)(const char *, char *)));
static void __dbenv_set_errfile __P((DB_ENV *, FILE *));
static void __dbenv_set_errpfx __P((DB_ENV *, const char *));
static void __dbenv_set_feedback __P((DB_ENV *, void (*)(DB_ENV *, int, int)));
static int  __dbenv_set_mutexlocks __P((DB_ENV *, int));
static int  __dbenv_set_pageyield __P((DB_ENV *, int));
static int  __dbenv_set_panic __P((DB_ENV *, int));
static void __dbenv_set_paniccall __P((DB_ENV *, void (*)(DB_ENV *, int)));
static int  __dbenv_set_recovery_init __P((DB_ENV *, int (*)(DB_ENV *)));
static int  __dbenv_set_region_init __P((DB_ENV *, int));
static int  __dbenv_set_tas_spins __P((DB_ENV *, u_int32_t));
static int  __dbenv_set_verbose __P((DB_ENV *, u_int32_t, int));

/*
 * db_env_create --
 *	DB_ENV constructor.
 */
int
db_env_create(dbenvpp, flags)
	DB_ENV **dbenvpp;
	u_int32_t flags;
{
	DB_ENV *dbenv;
	int ret;

	/*
	 * !!!
	 * We can't call the flags-checking routines, we don't have an
	 * environment yet.
	 */
	if (flags != 0)
		return (EINVAL);

	if ((ret = __os_calloc(1, sizeof(*dbenv), &dbenv)) != 0)
		return (ret);

	if ((ret = __dbenv_init(dbenv)) != 0) {
		__os_free(dbenv, sizeof(*dbenv));
		return (ret);
	}

	*dbenvpp = dbenv;
	return (0);
}

/*
 * __dbenv_init --
 *	Initialize a DB_ENV structure.
 *
 * PUBLIC: int  __dbenv_init __P((DB_ENV *));
 */
int
__dbenv_init(dbenv)
	DB_ENV *dbenv;
{
	dbenv->close = __dbenv_close;
	dbenv->err = __dbenv_err;
	dbenv->errx = __dbenv_errx;
	dbenv->open = __dbenv_open;
	dbenv->remove = __dbenv_remove;
	dbenv->set_errcall = __dbenv_set_errcall;
	dbenv->set_errfile = __dbenv_set_errfile;
	dbenv->set_errpfx = __dbenv_set_errpfx;
	dbenv->set_feedback = __dbenv_set_feedback;
	dbenv->set_mutexlocks = __dbenv_set_mutexlocks;
	dbenv->set_pageyield = __dbenv_set_pageyield;
	dbenv->set_panic = __dbenv_set_panic;
	dbenv->set_paniccall = __dbenv_set_paniccall;
	dbenv->set_recovery_init = __dbenv_set_recovery_init;
	dbenv->set_region_init = __dbenv_set_region_init;
	dbenv->set_tas_spins = __dbenv_set_tas_spins;
	dbenv->set_verbose = __dbenv_set_verbose;

	__log_dbenv_create(dbenv);		/* Subsystem specific. */
	__lock_dbenv_create(dbenv);
	__memp_dbenv_create(dbenv);
	__txn_dbenv_create(dbenv);
	__os_dbenv_create(dbenv);

	return (0);
}

/*
 * __dbenv_err --
 *	Error message, including the standard error string.
 */
static void
#ifdef __STDC__
__dbenv_err(const DB_ENV *dbenv, int error, const char *fmt, ...)
#else
__dbenv_err(dbenv, error, fmt, va_alist)
	const DB_ENV *dbenv;
	int error;
	const char *fmt;
	va_dcl
#endif
{
	va_list ap;

#ifdef __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	__db_real_err(dbenv, error, 1, 1, fmt, ap);

	va_end(ap);
}

/*
 * __dbenv_errx --
 *	Error message.
 */
static void
#ifdef __STDC__
__dbenv_errx(const DB_ENV *dbenv, const char *fmt, ...)
#else
__dbenv_errx(dbenv, fmt, va_alist)
	const DB_ENV *dbenv;
	const char *fmt;
	va_dcl
#endif
{
	va_list ap;

#ifdef __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	__db_real_err(dbenv, 0, 0, 1, fmt, ap);

	va_end(ap);
}

static void
__dbenv_set_errcall(dbenv, errcall)
	DB_ENV *dbenv;
	void (*errcall) __P((const char *, char *));
{
	dbenv->db_errcall = errcall;
}

static void
__dbenv_set_errfile(dbenv, errfile)
	DB_ENV *dbenv;
	FILE *errfile;
{
	dbenv->db_errfile = errfile;
}

static void
__dbenv_set_errpfx(dbenv, errpfx)
	DB_ENV *dbenv;
	const char *errpfx;
{
	dbenv->db_errpfx = errpfx;
}

static void
__dbenv_set_feedback(dbenv, feedback)
	DB_ENV *dbenv;
	void (*feedback) __P((DB_ENV *, int, int));
{
	dbenv->db_feedback = feedback;
}

static int
__dbenv_set_mutexlocks(dbenv, onoff)
	DB_ENV *dbenv;
	int onoff;
{
	COMPQUIET(dbenv, NULL);

	DB_GLOBAL(db_mutexlocks) = onoff;
	return (0);
}

static int
__dbenv_set_pageyield(dbenv, onoff)
	DB_ENV *dbenv;
	int onoff;
{
	COMPQUIET(dbenv, NULL);

	DB_GLOBAL(db_pageyield) = onoff;
	return (0);
}

static int					/* !!!: Undocumented. */
__dbenv_set_panic(dbenv, onoff)
	DB_ENV *dbenv;
	int onoff;
{
	COMPQUIET(dbenv, NULL);

	DB_GLOBAL(db_panic) = onoff;
	return (0);
}

static void
__dbenv_set_paniccall(dbenv, paniccall)
	DB_ENV *dbenv;
	void (*paniccall) __P((DB_ENV *, int));
{
	dbenv->db_paniccall = paniccall;
}

static int
__dbenv_set_recovery_init(dbenv, recovery_init)
	DB_ENV *dbenv;
	int (*recovery_init) __P((DB_ENV *));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_recovery_init");

	dbenv->db_recovery_init = recovery_init;

	return (0);
}

static int
__dbenv_set_region_init(dbenv, onoff)
	DB_ENV *dbenv;
	int onoff;
{
	COMPQUIET(dbenv, NULL);

	DB_GLOBAL(db_region_init) = onoff;
	return (0);
}

static int
__dbenv_set_tas_spins(dbenv, tas_spins)
	DB_ENV *dbenv;
	u_int32_t tas_spins;
{
	COMPQUIET(dbenv, NULL);

	DB_GLOBAL(db_tas_spins) = tas_spins;
	return (0);
}

static int
__dbenv_set_verbose(dbenv, which, onoff)
	DB_ENV *dbenv;
	u_int32_t which;
	int onoff;
{
	switch (which) {
	case DB_VERB_CHKPOINT:
	case DB_VERB_DEADLOCK:
	case DB_VERB_RECOVERY:
	case DB_VERB_WAITSFOR:
		if (onoff)
			FLD_SET(dbenv->verbose, which);
		else
			FLD_CLR(dbenv->verbose, which);
		break;
	default:
		return (EINVAL);
	}
	return (0);
}

/*
 * __db_mi_env --
 *	Method illegally called with public environment.
 *
 * PUBLIC: int __db_mi_env __P((DB_ENV *, const char *));
 */
int
__db_mi_env(dbenv, name)
	DB_ENV *dbenv;
	const char *name;
{
	__db_err(dbenv, "%s: method meaningless in shared environment", name);
	return (EINVAL);
}

/*
 * __db_mi_open --
 *	Method illegally called after open.
 *
 * PUBLIC: int __db_mi_open __P((DB_ENV *, const char *, int));
 */
int
__db_mi_open(dbenv, name, after)
	DB_ENV *dbenv;
	const char *name;
	int after;
{
	__db_err(dbenv,
	    "%s: method meaningless %s open", name, after ? "after" : "before");
	return (EINVAL);
}

/*
 * __db_env_config --
 *	Method or function called without subsystem being configured.
 *
 * PUBLIC: int __db_env_config __P((DB_ENV *, int));
 */
int
__db_env_config(dbenv, subsystem)
	DB_ENV *dbenv;
	int subsystem;
{
	const char *name;

	switch (subsystem) {
	case DB_INIT_LOCK:
		name = "lock";
		break;
	case DB_INIT_LOG:
		name = "log";
		break;
	case DB_INIT_MPOOL:
		name = "mpool";
		break;
	case DB_INIT_TXN:
		name = "txn";
		break;
	default:
		name = "unknown";
		break;
	}
	__db_err(dbenv,
    "%s interface called with environment not configured for that subsystem",
	    name);
	return (EINVAL);
}

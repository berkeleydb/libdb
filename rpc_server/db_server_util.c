/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000
 *      Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: db_server_util.c,v 1.23 2000/05/18 17:43:20 sue Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#include <rpc/rpc.h>

#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#endif
#include "db_server.h"

#include "db_int.h"
#include "clib_ext.h"
#include "db_server_int.h"
#include "rpc_server_ext.h"
#include "common_ext.h"

extern int __dbsrv_main	 __P((void));
static int add_home __P((char *));
static int env_recover __P((char *));

static LIST_HEAD(cthead, ct_entry) __dbsrv_head;
static LIST_HEAD(homehead, home_entry) __dbsrv_home;
static long __dbsrv_defto = DB_SERVER_TIMEOUT;
static long __dbsrv_maxto = DB_SERVER_MAXTIMEOUT;
static long __dbsrv_idleto = DB_SERVER_IDLETIMEOUT;
static char *logfile = NULL;
static char *prog;

static void usage __P((char *));

int __dbsrv_verbose = 0;

int
main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	CLIENT *cl;
	int ch, ret;

	LIST_INIT(&__dbsrv_home);
	prog = argv[0];
	/*
	 * Check whether another server is running or not.  There
	 * is a race condition where two servers could be racing to
	 * register with the portmapper.  The goal of this check is to
	 * forbid running additional servers (like those started from
	 * the test suite) if the user is already running one.
	 *
	 * XXX
	 * This does not solve nor prevent two servers from being
	 * started at the same time and running recovery at the same
	 * time on the same environments.
	 */
	if ((cl = clnt_create("localhost", DB_SERVERPROG, DB_SERVERVERS, "tcp"))
	    != NULL) {
		fprintf(stderr, "DB server already running.\n");
		clnt_destroy(cl);
		exit(1);
	}
	while ((ch = getopt(argc, argv, "h:I:L:t:T:Vv")) != EOF)
		switch (ch) {
		case 'h':
			(void) add_home(optarg);
			break;
		case 'I':
			(void)__db_getlong(NULL, prog, optarg, 1,
			    LONG_MAX, &__dbsrv_idleto);
			break;
		case 'L':
			logfile = optarg;
			break;
		case 't':
			(void)__db_getlong(NULL, prog, optarg, 1,
			    LONG_MAX, &__dbsrv_defto);
			break;
		case 'T':
			(void)__db_getlong(NULL, prog, optarg, 1,
			    LONG_MAX, &__dbsrv_maxto);
			break;
		case 'V':
			printf("%s\n", db_version(NULL, NULL, NULL));
			exit(0);
		case 'v':
			__dbsrv_verbose = 1;
			break;
		default:
			usage(prog);
		}
	/*
	 * Check default timeout against maximum timeout
	 */
	if (__dbsrv_defto > __dbsrv_maxto)
		__dbsrv_defto = __dbsrv_maxto;

	LIST_INIT(&__dbsrv_head);

	/*
	 * If a client crashes during an RPC, our reply to it
	 * generates a SIGPIPE.  Ignore SIGPIPE so we don't exit unnecessarily.
	 */
#ifdef SIGPIPE
	signal(SIGPIPE, SIG_IGN);
#endif

	if (logfile != NULL && __db_util_logset("berkeley_db_svc", logfile))
		exit(1);

	/*
	 * Now that we are ready to start, run recovery on all the
	 * environments specified.
	 */
	if ((ret = env_recover(prog)) != 0)
		exit(1);

	/*
	 * We've done our setup, now call the generated server loop
	 */
	if (__dbsrv_verbose)
		printf("%s:  Ready to receive requests\n", prog);
	__dbsrv_main();

	/* NOTREACHED */
	abort();
}

static void
usage(prog)
	char *prog;
{
	fprintf(stderr, "usage: %s %s\n\t%s\n", prog,
	    "[-Vv] [-h home]",
	    "[-I idletimeout] [-L logfile] [-t def_timeout] [-T maxtimeout]");
	exit(1);
}

/*
 * PUBLIC: void __dbsrv_settimeout __P((ct_entry *, u_int32_t));
 */
void
__dbsrv_settimeout(ctp, to)
	ct_entry *ctp;
	u_int32_t to;
{
	if (to > (u_int32_t)__dbsrv_maxto)
		ctp->ct_timeout = __dbsrv_maxto;
	else if (to <= 0)
		ctp->ct_timeout = __dbsrv_defto;
	else
		ctp->ct_timeout = to;
}

/*
 * PUBLIC: void __dbsrv_timeout __P((int));
 */
void
__dbsrv_timeout(force)
	int force;
{
	static long to_hint = -1;
	DBC *dbcp;
	time_t t;
	long to;
	ct_entry *ctp, *nextctp;

	if ((t = time(NULL)) == -1)
		return;

	/*
	 * Check hint.  If hint is further in the future
	 * than now, no work to do.
	 */
	if (!force && to_hint > 0 && t < to_hint)
		return;
	to_hint = -1;
	/*
	 * First timeout idle handles.
	 * If we are forcing a timeout, we'll close all env handles.
	 */
	for (ctp = LIST_FIRST(&__dbsrv_head); ctp != NULL; ctp = nextctp) {
		nextctp = LIST_NEXT(ctp, entries);
		if (ctp->ct_type != H_ENV)
			continue;
		to = ctp->ct_active + ctp->ct_idle;
		/* TIMEOUT */
		if (to < t || force) {
			if (__dbsrv_verbose)
				printf("Timing out env id %ld\n", ctp->ct_id);
			(void)__dbenv_close_int(ctp->ct_id, 0);
			/*
			 * If we timed out an env, we may have closed
			 * all sorts of ctp's (maybe even all of them.
			 * So start over with a guaranteed good ctp.
			 */
			nextctp = LIST_FIRST(&__dbsrv_head);
		}
	}
	/*
	 * Next timeout transactions or cursors holding DB resources.
	 */
	for (ctp = LIST_FIRST(&__dbsrv_head); ctp != NULL; ctp = nextctp) {
		nextctp = LIST_NEXT(ctp, entries);
		switch (ctp->ct_type) {
		case H_TXN:
			to = ctp->ct_active + ctp->ct_timeout;
			/* TIMEOUT */
			if (to < t) {
				if (__dbsrv_verbose)
					printf("Timing out txn id %ld\n",
					    ctp->ct_id);
				(void) txn_abort((DB_TXN *)ctp->ct_anyp);
				__dbdel_ctp(ctp);
				/*
				 * If we timed out an txn, we may have closed
				 * all sorts of ctp's.
				 * So start over with a guaranteed good ctp.
				 */
				nextctp = LIST_FIRST(&__dbsrv_head);
			} else if ((to_hint > 0 && to_hint > to) ||
			    to_hint == -1)
				to_hint = to;
			break;
		case H_CURSOR:
			to = ctp->ct_active + ctp->ct_timeout;
			/* TIMEOUT */
			if (to < t) {
				if (__dbsrv_verbose)
					printf("Timing out cursor %ld\n",
					    ctp->ct_id);
				dbcp = (DBC *)ctp->ct_anyp;
				(void) dbcp->c_close(dbcp);
				__dbclear_ctp(ctp);
				/*
				 * Start over with a guaranteed good ctp.
				 */
				nextctp = LIST_FIRST(&__dbsrv_head);
			} else if ((to_hint > 0 && to_hint > to) ||
			    to_hint == -1)
				to_hint = to;
			break;
		default:
			break;
		}
	}
}

/*
 * RECURSIVE FUNCTION.  We need to abort any number of levels of nested
 * layers.
 */
/*
 * PUBLIC: void __dbclear_child __P((ct_entry *));
 */
void
__dbclear_child(parent)
	ct_entry *parent;
{
	ct_entry *ctp, *nextctp;

	for (ctp = LIST_FIRST(&__dbsrv_head); ctp != NULL;
	    ctp = nextctp) {
		nextctp = LIST_NEXT(ctp, entries);
		if (ctp->ct_type == 0)
			continue;
		if (ctp->ct_parent == parent) {
			__dbclear_child(ctp);
			/*
			 * Need to do this here because le_next may
			 * have changed with the recursive call and we
			 * don't want to point to a removed entry.
			 */
			nextctp = LIST_NEXT(ctp, entries);
			__dbclear_ctp(ctp);
		}
	}
}

/*
 * PUBLIC: void __dbclear_ctp __P((ct_entry *));
 */
void
__dbclear_ctp(ctp)
	ct_entry *ctp;
{
	LIST_REMOVE(ctp, entries);
	__os_free(ctp, sizeof(ct_entry));
}

/*
 * PUBLIC: void __dbdel_ctp __P((ct_entry *));
 */
void
__dbdel_ctp(parent)
	ct_entry *parent;
{
	__dbclear_child(parent);
	__dbclear_ctp(parent);
}

/*
 * PUBLIC: ct_entry *new_ct_ent __P((u_int32_t *));
 */
ct_entry *
new_ct_ent(errp)
	u_int32_t *errp;
{
	time_t t;
	ct_entry *ctp, *octp;
	int ret;

	if ((ret = __os_malloc(NULL, sizeof(ct_entry), NULL, &ctp)) != 0) {
		*errp = ret;
		return (NULL);
	}
	/*
	 * Get the time as ID.  We may service more than one request per
	 * second however.  If we are, then increment id value until we
	 * find an unused one.  We insert entries in LRU fashion at the
	 * head of the list.  So, if the first entry doesn't match, then
	 * we know for certain that we can use our entry.
	 */
	if ((t = time(NULL)) == -1) {
		*errp = t;
		__os_free(ctp, sizeof(ct_entry));
		return (NULL);
	}
	octp = LIST_FIRST(&__dbsrv_head);
	if (octp != NULL && octp->ct_id >= t)
		t = octp->ct_id + 1;
	ctp->ct_id = t;
	ctp->ct_idle = __dbsrv_idleto;

	LIST_INSERT_HEAD(&__dbsrv_head, ctp, entries);
	return (ctp);
}

/*
 * PUBLIC: ct_entry *get_tableent __P((long));
 */
ct_entry *
get_tableent(id)
	long id;
{
	ct_entry *ctp;

	for (ctp = LIST_FIRST(&__dbsrv_head); ctp != NULL;
	    ctp = LIST_NEXT(ctp, entries))
		if (ctp->ct_id == id)
			return (ctp);
	return (NULL);
}

/*
 * PUBLIC: void __dbsrv_active __P((ct_entry *));
 */
void
__dbsrv_active(ctp)
	ct_entry *ctp;
{
	time_t t;
	ct_entry *envctp;

	if (ctp == NULL)
		return;
	if ((t = time(NULL)) == -1)
		return;
	ctp->ct_active = t;
	if ((envctp = ctp->ct_envparent) == NULL)
		return;
	envctp->ct_active = t;
	return;
}

/*
 * PUBLIC: int __dbenv_close_int __P((long, int));
 */
int
__dbenv_close_int(id, flags)
	long id;
	int flags;
{
	DB_ENV *dbenv;
	int ret;
	ct_entry *ctp;

	ctp = get_tableent(id);
	if (ctp == NULL)
		return (DB_NOSERVER_ID);
	DB_ASSERT(ctp->ct_type == H_ENV);
	dbenv = ctp->ct_envp;

	ret = dbenv->close(dbenv, flags);
	__dbdel_ctp(ctp);
	return (ret);
}

static int
add_home(home)
	char *home;
{
	home_entry *hp, *homep;
	int ret;

	if ((ret = __os_malloc(NULL, sizeof(home_entry), NULL, &hp)) != 0)
		return (ret);
	if ((ret = __os_malloc(NULL, strlen(home)+1, NULL, &hp->home)) != 0)
		return (ret);
	memcpy(hp->home, home, strlen(home)+1);
	hp->dir = home;
	/*
	 * This loop is to remove any trailing path separators,
	 * to assure hp->name points to the last component.
	 */
	hp->name = __db_rpath(home);
	*(hp->name) = '\0';
	hp->name++;
	while (*(hp->name) == '\0') {
		hp->name = __db_rpath(home);
		*(hp->name) = '\0';
		hp->name++;
	}
	/*
	 * Now we have successfully added it.  Make sure there are no
	 * identical names.
	 */
	for (homep = LIST_FIRST(&__dbsrv_home); homep != NULL;
	    homep = LIST_NEXT(homep, entries))
		if (strcmp(homep->name, hp->name) == 0) {
			printf("Already added home name %s, at directory %s\n",
			    hp->name, homep->dir);
			return (-1);
		}
	LIST_INSERT_HEAD(&__dbsrv_home, hp, entries);
	if (__dbsrv_verbose)
		printf("Added home %s in dir %s\n", hp->name, hp->dir);
	return (0);
}

/*
 * PUBLIC: char *get_home __P((char *));
 */
char *
get_home(name)
	char *name;
{
	home_entry *hp;

	for (hp = LIST_FIRST(&__dbsrv_home); hp != NULL;
	    hp = LIST_NEXT(hp, entries))
		if (strcmp(name, hp->name) == 0)
			return (hp->home);
	return (NULL);
}

static int
env_recover(progname)
	char *progname;
{
	DB_ENV *dbenv;
	home_entry *hp;
	u_int32_t flags;
	int exitval, ret;

	for (hp = LIST_FIRST(&__dbsrv_home); hp != NULL;
	    hp = LIST_NEXT(hp, entries)) {
		exitval = 0;
		if ((ret = db_env_create(&dbenv, 0)) != 0) {
			fprintf(stderr, "%s: db_env_create: %s\n",
			    progname, db_strerror(ret));
			exit(1);
		}
		if (__dbsrv_verbose == 1) {
			(void)dbenv->set_verbose(dbenv, DB_VERB_RECOVERY, 1);
			(void)dbenv->set_verbose(dbenv, DB_VERB_CHKPOINT, 1);
		}
		dbenv->set_errfile(dbenv, stderr);
		dbenv->set_errpfx(dbenv, progname);

		/*
		 * Initialize the env with DB_RECOVER.  That is all we
		 * have to do to run recovery.
		 */
		if (__dbsrv_verbose)
			printf("Running recovery on %s\n", hp->home);
		flags = DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
		    DB_INIT_TXN | DB_PRIVATE | DB_USE_ENVIRON | DB_RECOVER;
		if ((ret = dbenv->open(dbenv, hp->home, flags, 0)) != 0) {
			dbenv->err(dbenv, ret, "DBENV->open");
			goto error;
		}

		if (0) {
error:			exitval = 1;
		}
		if ((ret = dbenv->close(dbenv, 0)) != 0) {
			exitval = 1;
			fprintf(stderr, "%s: dbenv->close: %s\n",
			    progname, db_strerror(ret));
		}
		if (exitval)
			return (exitval);
	}
	return (0);
}

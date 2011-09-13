/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/log.h"
#include "dbinc/db_page.h"
#include "dbinc/qam.h"
#include "dbinc/partition.h"

#ifndef lint
static const char copyright[] =
    "Copyright (c) 1996, 2011 Oracle and/or its affiliates.  All rights reserved.\n";
#endif

enum which_open { OPEN_ORIGINAL, OPEN_HOT_BACKUP };

int db_hotbackup_backup_dir_clean __P((DB_ENV *, char *, char *, int *, int, int));
int db_hotbackup_data_copy __P((DB_ENV *, char *, char *, char *, int, int));
int db_hotbackup_env_data_copy __P((DB_ENV *, char *, char *, int, const char *));
int db_hotbackup_env_init __P((DB_ENV **,
     char *, char **, char ***, char *, enum which_open, int));
int db_hotbackup_main __P((int, char *[]));
int db_hotbackup_read_log_dir __P((DB_ENV *, char *, int, char *, char *, int *, int, int));
int db_hotbackup_read_data_dir __P((DB_ENV *,
    char *, char *, char *, int, int, int, int, const char *));
void save_error __P((const DB_ENV *, const char *, const char *));
int db_hotbackup_usage __P((void));
int db_hotbackup_version_check __P((void));

const char *progname;

/*
 * For backporting.
 */
#if (DB_VERSION_MAJOR < 5 || (DB_VERSION_MAJOR == 5 && DB_VERSION_MINOR < 2))
#define DB_STR_A(code, string, fmt)	string
#define	PART_PREFIX	"__dbp."
#ifndef DB_WIN32
#include "db_copy.c"
#endif
#endif

int
db_hotbackup(args)
	char *args;
{
	int argc;
	char **argv;

	__db_util_arg("db_hotbackup", args, &argc, &argv);
	return (db_hotbackup_main(argc, argv) ? EXIT_FAILURE : EXIT_SUCCESS);
}

#include <stdio.h>
#define	ERROR_RETURN	ERROR

int
db_hotbackup_main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind, __db_getopt_reset;
	time_t now;
	DB_ENV *dbenv;
	u_int data_cnt, data_next;
	int ch, checkpoint, copy_min, db_config, debug, env_copy, exitval;
	int remove_max, ret, update, verbose, hotbackup_flag_set, relative;
	char *backup_dir, **data_dir, **dir, *home, *log_dir, *passwd;
	char home_buf[DB_MAXPATHLEN], time_buf[CTIME_BUFLEN];
	u_int32_t flags;

	/*
	 * Make sure all verbose message are output before any error messages
	 * in the case where the output is being logged into a file.  This
	 * call has to be done before any operation is performed on the stream.
	 *
	 * Use unbuffered I/O because line-buffered I/O requires a buffer, and
	 * some operating systems have buffer alignment and size constraints we
	 * don't want to care about.  There isn't enough output for the calls
	 * to matter.
	 */
	setbuf(stdout, NULL);

	if ((progname = __db_rpath(argv[0])) == NULL)
		progname = argv[0];
	else
		++progname;

	if ((ret = db_hotbackup_version_check()) != 0)
		return (ret);

	/* We default to the safe environment copy. */
	env_copy = 1;

	checkpoint = db_config = data_cnt = data_next = debug = 
	    exitval = update = verbose = hotbackup_flag_set = 0;
	data_dir = NULL;
	backup_dir = home = passwd = NULL;
	log_dir = NULL;
	copy_min = remove_max = 0;
	__db_getopt_reset = 1;
	while ((ch = getopt(argc, argv, "b:cDd:Fgh:l:P:uVv")) != EOF)
		switch (ch) {
		case 'b':
			backup_dir = optarg;
			break;
		case 'c':
			checkpoint = 1;
			break;
		case 'D':
			db_config = 1;
			break;
		case 'd':
			/*
			 * User can specify a list of directories -- keep an
			 * array, leaving room for the trailing NULL.
			 */
			if (data_dir == NULL || data_next >= data_cnt - 2) {
				data_cnt = data_cnt == 0 ? 20 : data_cnt * 2;
				if ((data_dir = realloc(data_dir,
				    data_cnt * sizeof(*data_dir))) == NULL) {
					fprintf(stderr, "%s: %s\n",
					    progname, strerror(errno));
					return (EXIT_FAILURE);
				}
			}
			data_dir[data_next++] = optarg;
			break;
		case 'F':
			/* The default is to use environment copy. */
			env_copy = 0;
			break;
		case 'g':
			debug = 1;
			break;
		case 'h':
			home = optarg;
			break;
		case 'l':
			log_dir = optarg;
			break;
		case 'P':
			passwd = strdup(optarg);
			memset(optarg, 0, strlen(optarg));
			if (passwd == NULL) {
				fprintf(stderr, DB_STR_A("5026",
				    "%s: strdup: %s\n", "%s %s\n"),
				    progname, strerror(errno));
				return (EXIT_FAILURE);
			}
			break;
		case 'u':
			update = 1;
			break;
		case 'V':
			printf("%s\n", db_version(NULL, NULL, NULL));
			return (EXIT_SUCCESS);
		case 'v':
			verbose = 1;
			break;
		case '?':
		default:
			return (db_hotbackup_usage());
		}
	argc -= optind;
	argv += optind;

	if (argc != 0)
		return (db_hotbackup_usage());

	/* NULL-terminate any list of data directories. */
	if (data_dir != NULL) {
		data_dir[data_next] = NULL;
		/*
		 * -d is relative to the current directory, to run a checkpoint
		 * we must have directories relative to the environment.
		 */
		if (checkpoint == 1) {
			fprintf(stderr, DB_STR_A("5027",
			    "%s: cannot specify -d and -c\n", "%s\n"),
			    progname);
			return (db_hotbackup_usage());
		}
	}

	if (db_config && (data_dir != NULL || log_dir != NULL)) {
		fprintf(stderr, DB_STR_A("5028",
		    "%s: cannot specify -D and -d or -l\n", "%s\n"), progname);
		return (db_hotbackup_usage());
	}

	/* Handle possible interruptions. */
	__db_util_siginit();

	/*
	 * The home directory defaults to the environment variable DB_HOME.
	 * The log directory defaults to the home directory.
	 *
	 * We require a source database environment directory and a target
	 * backup directory.
	 */
	if (home == NULL) {
		home = home_buf;
		if ((ret = __os_getenv(
		    NULL, "DB_HOME", &home, sizeof(home_buf))) != 0) {
			fprintf(stderr, DB_STR_A("5029",
		    "%s failed to get environment variable DB_HOME: %s\n",
			    "%s %s\n"), progname, db_strerror(ret));
			return (EXIT_FAILURE);
		}
		/*
		 * home set to NULL if __os_getenv failed to find DB_HOME.
		 */
	}
	if (home == NULL) {
		fprintf(stderr, DB_STR_A("5030",
		    "%s: no source database environment specified\n",
		    "%s\n"), progname);
		return (db_hotbackup_usage());
	}
	if (backup_dir == NULL) {
		fprintf(stderr, DB_STR_A("5031",
		    "%s: no target backup directory specified\n", "%s\n"),
		    progname);
		return (db_hotbackup_usage());
	}

	if (verbose) {
		(void)time(&now);
		printf(DB_STR_A("5032", "%s: hot backup started at %s",
		    "%s %s"), progname, __os_ctime(&now, time_buf));
	}

	/* Open the source environment. */
	if (db_hotbackup_env_init(&dbenv, home,
	     (db_config || log_dir != NULL) ? &log_dir : NULL,
	     db_config ? &data_dir : NULL,
	     passwd, OPEN_ORIGINAL, verbose) != 0)
		goto err;
	
	if (env_copy) {
		if ((ret = dbenv->get_open_flags(dbenv, &flags)) != 0)
			goto err;
		if (flags & DB_PRIVATE) {
			fprintf(stderr, DB_STR_A("5129",
			    "%s: Cannot copy data from a PRIVATE environment\n",
			    "%s"), progname);
			goto err;
		}
	}

	if (log_dir != NULL) {
		if (db_config && __os_abspath(log_dir)) {
			fprintf(stderr, DB_STR_A("5033",
			    "%s: DB_CONFIG must not contain an absolute "
			    "path for the log directory\n", "%s\n"), progname);
			goto err;
		}
	}

#if (DB_VERSION_MAJOR > 5 || (DB_VERSION_MAJOR == 5 && DB_VERSION_MINOR > 0))
	/*
	 * Record in the environment that a hot backup is in progress.
	 * This disables the bulk loading optimization for the
	 * duration of the backup.  This increments a persistent
	 * counter in the database's environment, so it is essential
	 * that the corresponding decrement takes place upon
	 * completion of the backup.
	 */
	if ((ret = dbenv->set_flags(dbenv, DB_HOTBACKUP_IN_PROGRESS, 1))) {
		fprintf(stderr, DB_STR_A("5034",
		    "%s: dbenv->set_flags(DB_HOTBACKUP_IN_PROGRESS, 1): %s\n",
		    "%s %s\n"), progname, db_strerror(ret));
		goto err;
	}
	hotbackup_flag_set = 1;
#endif

	/*
	 * If the -c option is specified, checkpoint the source home
	 * database environment, and remove any unnecessary log files.
	 */
	if (checkpoint) {
		if (verbose)
			printf(DB_STR_A("5035", "%s: %s: force checkpoint\n",
			    "%s %s\n"), progname, home);
		if ((ret =
		    dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE)) != 0) {
			dbenv->err(dbenv, ret, "DB_ENV->txn_checkpoint");
			goto err;
		}
		if (!update) {
			if (verbose)
				printf(DB_STR_A("5036",
				    "%s: %s: remove unnecessary log files\n",
				    "%s %s\n"), progname, home);
			if ((ret = dbenv->log_archive(dbenv,
			     NULL, DB_ARCH_REMOVE)) != 0) {
				dbenv->err(dbenv, ret, "DB_ENV->log_archive");
				goto err;
			}
		}
	}

	/*
	 * If the target directory for the backup does not exist, create it
	 * with mode read-write-execute for the owner.  Ignore errors here,
	 * it's simpler and more portable to just always try the create.  If
	 * there's a problem, we'll fail with reasonable errors later.
	 */
	(void)__os_mkdir(NULL, backup_dir, DB_MODE_700);

	/*
	 * If -u was specified, remove all log files; if -u was not specified,
	 * remove all files.
	 *
	 * Potentially there are two directories to clean, the log directory
	 * and the target directory.  First, clean up the log directory if
	 * it's different from the target directory, then clean up the target
	 * directory.
	 */
	if (db_config && log_dir != NULL &&
	    db_hotbackup_backup_dir_clean(
	    dbenv, backup_dir, log_dir, &remove_max, update, verbose) != 0)
		goto err;
	if (db_hotbackup_backup_dir_clean(dbenv,
	    backup_dir, NULL, &remove_max, update, verbose) != 0)
		goto err;

	/*
	 * If the -u option was not specified, copy all database files found in
	 * the database environment home directory, or any directory specified
	 * using the -d option, into the target directory for the backup.
	 */
	if (!update) {
		if (db_hotbackup_read_data_dir(dbenv, home, backup_dir,
		     home, verbose, db_config, env_copy, 0, passwd) != 0)
			goto err;
		/*
		 * Use a DB_CONFIG if it exists and neither -D/-d was set.
		 */
		relative = 0;
		if (!db_config && data_dir == NULL) {
			(void)dbenv->get_data_dirs(
			    dbenv, (const char ***)&data_dir);
			relative = 1;
		}

		if (data_dir != NULL)
			for (dir = data_dir; *dir != NULL; ++dir) {
				/*
				 * Don't allow absolute path names taken from
				 * the DB_CONFIG file -- running recovery with
				 * them would corrupt the source files.
				 */
				if (db_config && __os_abspath(*dir)) {
					fprintf(stderr, DB_STR_A("5037",
"%s: data directory '%s' is absolute path, not permitted with -D option\n",
					    "%s %s\n"), progname, *dir);
					goto err;
				}
				if (db_hotbackup_read_data_dir(dbenv, home, backup_dir,
				     *dir, verbose, db_config,
				     env_copy, relative, passwd) != 0)
					goto err;
			}
	}

	/*
	 * Copy all log files found in the directory specified by the -l option
	 * (or in the database environment home directory, if no -l option was
	 * specified), into the target directory for the backup.
	 *
	 * The log directory defaults to the home directory.
	 */
	if (db_hotbackup_read_log_dir(dbenv, home, db_config, backup_dir,
	     log_dir, &copy_min, update, verbose) != 0)
		goto err;
	/*
	 * If we're updating a snapshot, the lowest-numbered log file copied
	 * into the backup directory should be less than, or equal to, the
	 * highest-numbered log file removed from the backup directory during
	 * cleanup.
	 */
	if (update && remove_max < copy_min &&
	     !(remove_max == 0 && copy_min == 1)) {
		fprintf(stderr, DB_STR_A("5038",
		    "%s: the largest log file removed (%d) must be greater\n",
		    "%s %d\n"), progname, remove_max);
		fprintf(stderr, DB_STR_A("5039",
		    "%s: than or equal the smallest log file copied (%d)\n",
		    "%s %d\n"), progname, copy_min);
		goto err;
	}

#if (DB_VERSION_MAJOR > 5 || (DB_VERSION_MAJOR == 5 && DB_VERSION_MINOR > 0))
	/* Turn off the hotbackup flag in the environment now. */
	if ((ret = dbenv->set_flags(dbenv, DB_HOTBACKUP_IN_PROGRESS, 0))) {
		fprintf(stderr,
		    "%s: dbenv->set_flags(DB_HOTBACKUP_IN_PROGRESS, 0): %s\n",
		    progname, db_strerror(ret));
		goto err;
	}
	hotbackup_flag_set = 0;
#endif

	/* Close the source environment. */
	if ((ret = dbenv->close(dbenv, 0)) != 0) {
		fprintf(stderr,
		    "%s: dbenv->close: %s\n", progname, db_strerror(ret));
		dbenv = NULL;
		goto err;
	}
	/* Perform catastrophic recovery on the hot backup. */
	if (verbose)
		printf(DB_STR_A("5040", "%s: %s: run catastrophic recovery\n",
		    "%s %s\n"), progname, backup_dir);
	if (db_hotbackup_env_init(&dbenv,
	    backup_dir, NULL, NULL, passwd, OPEN_HOT_BACKUP, verbose) != 0)
		goto err;

	/*
	 * Remove any unnecessary log files from the hot backup.
	 * For debugging purposes, leave them around.
	 */
	if (debug == 0) {
		if (verbose)
			printf(DB_STR_A("5041",
			    "%s: %s: remove unnecessary log files\n",
			    "%s %s\n"), progname, backup_dir);
		if ((ret =
		    dbenv->log_archive(dbenv, NULL, DB_ARCH_REMOVE)) != 0) {
			dbenv->err(dbenv, ret, "DB_ENV->log_archive");
			goto err;
		}
	}

	if (0) {
err:		exitval = 1;
	}
#if (DB_VERSION_MAJOR > 5 || (DB_VERSION_MAJOR == 5 && DB_VERSION_MINOR > 0))
	if (hotbackup_flag_set &&
	    (ret = dbenv->set_flags(dbenv, DB_HOTBACKUP_IN_PROGRESS, 0))) {
		exitval = 1;
		fprintf(stderr,
		    "%s: dbenv->set_flags(DB_HOTBACKUP_IN_PROGRESS, 0): %s\n",
		    progname, db_strerror(ret));
	}
#endif
	if (dbenv != NULL && (ret = dbenv->close(dbenv, 0)) != 0) {
		exitval = 1;
		fprintf(stderr,
		    "%s: dbenv->close: %s\n", progname, db_strerror(ret));
	}

	if (exitval == 0) {
		if (verbose) {
			(void)time(&now);
			printf(DB_STR_A("5042",
			    "%s: hot backup completed at %s",
			    "%s %s"), progname, __os_ctime(&now, time_buf));
		}
	} else {
		fprintf(stderr, DB_STR_A("5043", "%s: HOT BACKUP FAILED!\n",
		    "%s\n"), progname);
	}

	/* Resend any caught signal. */
	__db_util_sigresend();

	return (exitval == 0 ? EXIT_SUCCESS : EXIT_FAILURE);

}

/*
 * env_init --
 *	Open a database environment.
 */
int
db_hotbackup_env_init(dbenvp, home, log_dirp, data_dirp, passwd, which, verbose)
	DB_ENV **dbenvp;
	char *home, **log_dirp, ***data_dirp, *passwd;
	enum which_open which;
	int verbose;
{
	DB_ENV *dbenv;
	int ret;

	*dbenvp = NULL;

	/*
	 * Create an environment object and initialize it for error reporting.
	 */
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr,
		    "%s: db_env_create: %s\n", progname, db_strerror(ret));
		return (1);
	}

	dbenv->set_errfile(dbenv, stderr);
	setbuf(stderr, NULL);
	dbenv->set_errpfx(dbenv, progname);

	/* Any created intermediate directories are created private. */
	if ((ret = dbenv->set_intermediate_dir_mode(dbenv, "rwx------")) != 0) {
		dbenv->err(dbenv, ret, "DB_ENV->set_intermediate_dir_mode");
		return (1);
	}

	/*
	 * If a log directory has been specified, and it's not the same as the
	 * home directory, set it for the environment.
	 */
	if (log_dirp != NULL && *log_dirp != NULL &&
	    (ret = dbenv->set_lg_dir(dbenv, *log_dirp)) != 0) {
		dbenv->err(dbenv, ret, "DB_ENV->set_lg_dir: %s", *log_dirp);
		return (1);
	}

	/* Optionally set the password. */
	if (passwd != NULL &&
	    (ret = dbenv->set_encrypt(dbenv, passwd, DB_ENCRYPT_AES)) != 0) {
		dbenv->err(dbenv, ret, "DB_ENV->set_encrypt");
		return (1);
	}

	switch (which) {
	case OPEN_ORIGINAL:
		/*
		 * Opening the database environment we're trying to back up.
		 * We try to attach to a pre-existing environment; if that
		 * fails, we create a private environment and try again.
		 */
		if ((ret = dbenv->open(dbenv, home, DB_USE_ENVIRON, 0)) != 0 &&
		    (ret == DB_VERSION_MISMATCH ||
		    (ret = dbenv->open(dbenv, home, DB_CREATE |
		    DB_INIT_LOG | DB_INIT_TXN | DB_PRIVATE | DB_USE_ENVIRON,
		    0)) != 0)) {
			dbenv->err(dbenv, ret, "DB_ENV->open: %s", home);
			return (1);
		}
		if (log_dirp != NULL && *log_dirp == NULL)
			(void)dbenv->get_lg_dir(dbenv, (const char **)log_dirp);
		if (data_dirp != NULL && *data_dirp == NULL)
			(void)dbenv->get_data_dirs(
			    dbenv, (const char ***)data_dirp);
		break;
	case OPEN_HOT_BACKUP:
		/*
		 * Opening the backup copy of the database environment.  We
		 * better be the only user, we're running recovery.
		 * Ensure that there at least minimal cache for worst
		 * case page size.
		 */
		if ((ret =
		    dbenv->set_cachesize(dbenv, 0, 64 * 1024 * 10, 0)) != 0) {
			dbenv->err(dbenv,
			     ret, "DB_ENV->set_cachesize: %s", home);
			return (1);
		}
		if (verbose == 1)
			(void)dbenv->set_verbose(dbenv, DB_VERB_RECOVERY, 1);
		if ((ret = dbenv->open(dbenv, home, DB_CREATE |
		    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_PRIVATE |
		    DB_RECOVER_FATAL | DB_USE_ENVIRON, 0)) != 0) {
			dbenv->err(dbenv, ret, "DB_ENV->open: %s", home);
			return (1);
		}
		break;
	}

	*dbenvp = dbenv;
	return (0);
}

/*
 * backup_dir_clean --
 *	Clean out the backup directory.
 */
int
db_hotbackup_backup_dir_clean(dbenv, backup_dir, log_dir, remove_maxp, update, verbose)
	DB_ENV *dbenv;
	char *backup_dir, *log_dir;
	int *remove_maxp, update, verbose;
{
	ENV *env;
	int cnt, fcnt, ret, v;
	char **names, *dir, buf[DB_MAXPATHLEN], path[DB_MAXPATHLEN];

	env = dbenv->env;

	/* We may be cleaning a log directory separate from the target. */
	if (log_dir != NULL) {
		if ((size_t)snprintf(buf, sizeof(buf), "%s%c%s",
		    backup_dir, PATH_SEPARATOR[0] ,log_dir) >= sizeof(buf)) {
			dbenv->errx(dbenv, DB_STR_A("5044",
			    "%s%c%s: path too long", "%s %c %s"),
			    backup_dir, PATH_SEPARATOR[0] ,log_dir);
			return (1);
		}
		dir = buf;
	} else
		dir = backup_dir;

	/* Get a list of file names. */
	if ((ret = __os_dirlist(env, dir, 0, &names, &fcnt)) != 0) {
		if (log_dir != NULL && !update)
			return (0);
		dbenv->err(dbenv, ret, DB_STR_A("5045", "%s: directory read",
		    "%s"), dir);
		return (1);
	}
	for (cnt = fcnt; --cnt >= 0;) {
		/*
		 * Skip non-log files (if update was specified).
		 */
		if (strncmp(names[cnt], LFPREFIX, sizeof(LFPREFIX) - 1)) {
			if (update)
				continue;
		} else {
			/* Track the highest-numbered log file removed. */
			v = atoi(names[cnt] + sizeof(LFPREFIX) - 1);
			if (*remove_maxp < v)
				*remove_maxp = v;
		}
		if ((size_t)snprintf(path, sizeof(path), "%s%c%s",
		    dir, PATH_SEPARATOR[0], names[cnt]) >= sizeof(path)) {
			dbenv->errx(dbenv, DB_STR_A("5046",
			    "%s%c%s: path too long", "%s %c %s"),
			    dir, PATH_SEPARATOR[0], names[cnt]);
			return (1);
		}
		if (verbose)
			printf(DB_STR_A("5047", "%s: removing %s\n",
			    "%s %s\n"),  progname, path);
		if (__os_unlink(env, path, 0) != 0)
			return (1);
	}

	__os_dirfree(env, names, fcnt);

	if (verbose && *remove_maxp != 0)
		printf(DB_STR_A("5048",
		    "%s: highest numbered log file removed: %d\n", "%s %d\n"),
		    progname, *remove_maxp);

	return (0);
}

/*
 * read_data_dir --
 *	Read a directory looking for databases to copy.
 */
int
db_hotbackup_read_data_dir(dbenv,
    home, backup_dir, dir, verbose, db_config, env_copy, relative, passwd)
	DB_ENV *dbenv;
	char *home, *backup_dir, *dir;
	int verbose, db_config, env_copy, relative;
	const char *passwd;
{
	ENV *env;
	int cnt, fcnt, ret;
	char *bd, **names;
	char buf[DB_MAXPATHLEN], bbuf[DB_MAXPATHLEN];

	env = dbenv->env;

	bd = backup_dir;
	if (db_config && dir != home) {
		/* Build a path name to the destination. */
		if ((size_t)(cnt = snprintf(bbuf, sizeof(bbuf), "%s%c%s%c",
		      backup_dir, PATH_SEPARATOR[0],
		      dir, PATH_SEPARATOR[0])) >= sizeof(buf)) {
			dbenv->errx(dbenv, DB_STR_A("5049",
			    "%s%c%s: path too long", "%s %c %s"),
			    backup_dir, PATH_SEPARATOR[0], dir);
			return (1);
		}
		bd = bbuf;

		/* Create the path. */
		if ((ret = __db_mkpath(env, bd)) != 0) {
			dbenv->err(dbenv, ret, DB_STR_A("5050",
			    "%s: cannot create", "%s"), bd);
			return (1);
		}
		/* step on the trailing '/' */
		bd[cnt - 1] = '\0';

	}
	if ((relative && !__os_abspath(dir)) || (db_config && dir != home)) {
		/* Build a path name to the source. */
		if ((size_t)snprintf(buf, sizeof(buf),
		    "%s%c%s", home, PATH_SEPARATOR[0], dir) >= sizeof(buf)) {
			dbenv->errx(dbenv, DB_STR_A("5051",
			    "%s%c%s: path too long", "%s %c %s"),
			    home, PATH_SEPARATOR[0], dir);
			return (1);
		}
		dir = buf;
	}
	/* Get a list of file names. */
	if ((ret = __os_dirlist(env, dir, 0, &names, &fcnt)) != 0) {
		dbenv->err(dbenv, ret, DB_STR_A("5052", "%s: directory read",
		    "%s"), dir);
		return (1);
	}
	for (cnt = fcnt; --cnt >= 0;) {
		/*
		 * Skip files in DB's name space (but not Queue extent files or
		 * the replicated system database, we need them).
		 */
		if (!strncmp(names[cnt], LFPREFIX, sizeof(LFPREFIX) - 1))
			continue;
		if (!strncmp(names[cnt],
		    DB_REGION_PREFIX, sizeof(DB_REGION_PREFIX) - 1) &&
		    (env_copy || strncmp(names[cnt],
		    QUEUE_EXTENT_PREFIX, sizeof(QUEUE_EXTENT_PREFIX) - 1))
#ifdef HAVE_PARTITION
		    &&
		    strncmp(names[cnt],
		    PART_PREFIX, sizeof(PART_PREFIX) - 1)
#endif
#if (DB_VERSION_MAJOR >= 5)
		    &&
		    strncmp(names[cnt],
		    REPSYSDBNAME, sizeof(REPSYSDBNAME) - 1)
#endif
		    )
			continue;

		/*
		 * Skip DB_CONFIG.
		 */
		if (!db_config &&
		     !strncmp(names[cnt], "DB_CONFIG", sizeof("DB_CONFIG")))
			continue;

		/*
		 * Copy the file.
		 * By default we copy through the environment, this works
		 * in all cases.  If the OS or Filesystem supports atomic
		 * reads then we can read directly from the filesystem.
		 * This is known to be true only for UNIX based operating
		 * systems and not LINUX or Windows based operating systems.
		 */
		if (env_copy)
			ret = db_hotbackup_env_data_copy(dbenv,
			    names[cnt], bd, verbose, passwd);
		/* The file might not be a database. */
		if (!env_copy || ret == ENOENT || ret == EINVAL)
			ret = db_hotbackup_data_copy(dbenv, names[cnt], dir, bd, 0, verbose);
		if (ret != 0)
			return (1);
	}

	__os_dirfree(env, names, fcnt);

	return (0);
}

/*
 * read_log_dir --
 *	Read a directory looking for log files to copy.  If home
 * is passed then we are possibly using a log dir in the destination,
 * following DB_CONFIG configuration.
 */
int
db_hotbackup_read_log_dir(dbenv, home, db_config, backup_dir, log_dir, copy_minp,
	update, verbose)
	DB_ENV *dbenv;
	char *home, *backup_dir, *log_dir;
	int *copy_minp, db_config, update, verbose;
{
	ENV *env;
	u_int32_t aflag;
	int cnt, ret, v;
	char **begin, **names, *backupd, *logd;
	char from[DB_MAXPATHLEN], to[DB_MAXPATHLEN];
	char cfpath[DB_MAXPATHLEN];

	env = dbenv->env;

	/*
	 * If the log directory is specified from DB_CONFIG then it is
	 * located relative to the the log file source dircectory.  If
	 * the log directory is set from the -l argument and it is not an
	 * absolute path it is also located relative to the log file source
	 * directory.  Otherwise the log file source directory is home.
	 */
	if (db_config && log_dir != NULL) {
		if ((size_t)snprintf(from, sizeof(from), "%s%c%s",
		    home, PATH_SEPARATOR[0], log_dir) >= sizeof(from)) {
			dbenv->errx(dbenv, DB_STR_A("5053",
			    "%s%c%s: path too long", "%s %c %s"),
			    home, PATH_SEPARATOR[0], log_dir);
			return (1);
		}
		logd = strdup(from);

		/*
		 * With a DB_CONFIG file, logs are copied to the specified
		 * location relative to the backup_dir.  Otherwise the logs are
		 * copied to the backup_dir.
		 */
		if ((size_t)(cnt = snprintf(to, sizeof(to),
		    "%s%c%s%c", backup_dir, PATH_SEPARATOR[0],
		    log_dir, PATH_SEPARATOR[0])) >= sizeof(to)) {
			dbenv->errx(dbenv, DB_STR_A("5054",
			    "%s%c%s: path too long", "%s %c %s"),
			    backup_dir, PATH_SEPARATOR[0], log_dir);
			return (1);
		}
		backupd = strdup(to);

		/* Create the backup log directory. */
		if ((ret = __db_mkpath(env, backupd)) != 0) {
			dbenv->err(dbenv, ret, DB_STR_A("5055",
			    "%s: cannot create", "%s"), backupd);
			return (1);
		}
		/* Step on the trailing '/'. */
		backupd[cnt - 1] = '\0';
	} else {
		/*
		 * Use a DB_CONFIG if it exists and neither -D/-l was set.
		 */
		if (log_dir == NULL) {
			(void)dbenv->get_lg_dir(dbenv, (const char **)&log_dir);

			if (log_dir)
				logd = log_dir;
			else
				logd = log_dir = home;
		} else {
			logd = log_dir;
			/*
			 * Do we have -l and an existing DB_CONFIG?  That is a
			 * usage problem, but for backward compatibility, keep
			 * going if log_dir happens to be the same as the
			 * DB_CONFIG path.
			 */
			if ((size_t)snprintf(cfpath, sizeof(cfpath),
			    "%s%c%s", home, PATH_SEPARATOR[0], "DB_CONFIG") >=
			    sizeof(cfpath)) {
				dbenv->errx(dbenv,DB_STR_A("5056",
				    "%s%c%s: path too long", "%s %c %s"),
				    home, PATH_SEPARATOR[0], "DB_CONFIG");
				return (1);
			}
			if (__os_exists(NULL, cfpath, NULL) == 0) {
				if (strcmp(log_dir,dbenv->db_log_dir)) {
					fprintf(stderr, DB_STR_A("5057",
		    "%s: cannot specify -l with conflicting DB_CONFIG file\n",
					    "%s\n"), progname);
					return (db_hotbackup_usage());
				} else
					fprintf(stderr, DB_STR_A("5058",
		    "%s: use of -l with DB_CONFIG file is deprecated\n",
					    "%s\n"), progname);
			}
		}

		if (logd != home && !__os_abspath(logd)) {
			if ((size_t)snprintf(from, sizeof(from),
			    "%s%c%s", home, PATH_SEPARATOR[0], log_dir)
			    >= sizeof(from)) {
				dbenv->errx(dbenv, DB_STR_A("5059",
				    "%s%c%s: path too long",
				    "%s %c %s"), home,
				    PATH_SEPARATOR[0], log_dir);
				return 1;
			}
			logd = strdup(from);
		}
		backupd = backup_dir;
	}

again:	aflag = DB_ARCH_LOG;

	/*
	 * If this is an update and we are deleting files, first process
	 * those files that can be removed, then repeat with the rest.
	 */
	if (update)
		aflag = 0;

	/* Flush the log to get latest info. */
	if ((ret = dbenv->log_flush(dbenv, NULL)) != 0) {
		dbenv->err(dbenv, ret, "DB_ENV->log_flush");
		return (1);
	}
		
	/* Get a list of file names to be copied. */
	if ((ret = dbenv->log_archive(dbenv, &names, aflag)) != 0) {
		dbenv->err(dbenv, ret, "DB_ENV->log_archive");
		return (1);
	}
	if (names == NULL)
		goto done;
	begin = names;
	for (; *names != NULL; names++) {
		/* Track the lowest-numbered log file copied. */
		v = atoi(*names + sizeof(LFPREFIX) - 1);
		if (*copy_minp == 0 || *copy_minp > v)
			*copy_minp = v;

		if ((size_t)snprintf(from, sizeof(from), "%s%c%s",
		    logd, PATH_SEPARATOR[0], *names) >= sizeof(from)) {
			dbenv->errx(dbenv, DB_STR_A("5060",
			    "%s%c%s: path too long", "%s %c %s"),
			    logd, PATH_SEPARATOR[0], *names);
			return (1);
		}

		/*
		 * If we're going to remove the file, attempt to rename the
		 * instead of copying and then removing.  The likely failure
		 * is EXDEV (source and destination are on different volumes).
		 * Fall back to a copy, regardless of the error.  We don't
		 * worry about partial contents, the copy truncates the file
		 * on open.
		 */
		if (update) {
			if ((size_t)snprintf(to, sizeof(to), "%s%c%s",
			    backupd, PATH_SEPARATOR[0], *names) >= sizeof(to)) {
				dbenv->errx(dbenv, DB_STR_A("5061",
				    "%s%c%s: path too long", "%s %c %s"),
				    backupd, PATH_SEPARATOR[0], *names);
				return (1);
			}
			if (__os_rename(env, from, to, 1) == 0) {
				if (verbose)
					printf(DB_STR_A("5062",
					    "%s: moving %s to %s\n",
					    "%s %s %s\n"),
					   progname, from, to);
				continue;
			}
		}

		/* Copy the file. */
		if (db_hotbackup_data_copy(dbenv, *names, logd, backupd, 1, verbose) != 0)
			return (1);

		if (update) {
			if (verbose)
				printf(DB_STR_A("5063", "%s: removing %s\n",
				    "%s %s\n"), progname, from);
			if ((ret = __os_unlink(env, from, 0)) != 0) {
				dbenv->err(dbenv, ret, DB_STR_A("5064",
				    "unlink of %s failed", "%s"), from);
				return (1);
			}
		}

	}

	free(begin);
done:	if (update) {
		update = 0;
		goto again;
	}

	if (verbose && *copy_minp != 0)
		printf(DB_STR_A("5065",
		    "%s: lowest numbered log file copied: %d\n", "%s %d\n"),
		    progname, *copy_minp);
	if (logd != log_dir)
		free(logd);
	if (backupd != backup_dir)
		free(backupd);

	return (0);
}

/*
 * data_copy --
 *	Copy a file into the backup directory.
 */
int
db_hotbackup_data_copy(dbenv, file, from_dir, to_dir, log, verbose)
	DB_ENV *dbenv;
	char *file, *from_dir, *to_dir;
	int log, verbose;
{
	DB_FH *rfhp, *wfhp;
	ENV *env;
	size_t nr, nw;
	int ret;
	char *buf;

	rfhp = wfhp = NULL;
	env = dbenv->env;
	ret = 0;

	if (verbose)
		printf(DB_STR_A("5066", "%s: copying %s%c%s to %s%c%s\n",
		    "%s %s %c %s %s %c %s\n"), progname, from_dir,
		    PATH_SEPARATOR[0], file, to_dir, PATH_SEPARATOR[0], file);

	/*
	 * We MUST copy multiples of the page size, atomically, to ensure a
	 * database page is not updated by another thread of control during
	 * the copy.
	 *
	 * !!!
	 * The current maximum page size for Berkeley DB is 64KB; we will have
	 * to increase this value if the maximum page size is ever more than a
	 * megabyte
	 */
	if ((buf = malloc(MEGABYTE)) == NULL) {
		dbenv->err(dbenv, errno, DB_STR_A("5067",
		    "%lu buffer allocation", "%lu"), (u_long)MEGABYTE);
		return (1);
	}

	/* Open the input file. */
	if (snprintf(buf, MEGABYTE, "%s%c%s",
	    from_dir, PATH_SEPARATOR[0], file) >= MEGABYTE) {
		dbenv->errx(dbenv, DB_STR_A("5068", "%s%c%s: path too long",
		    "%s %c %s"), from_dir, PATH_SEPARATOR[0], file);
		goto err;
	}
	if ((ret = __os_open(env, buf, 0, DB_OSO_RDONLY, 0, &rfhp)) != 0) {
		if (ret == ENOENT && !log) {
			ret = 0;
			if (verbose)
				printf(DB_STR_A("5069",
				    "%s: %s%c%s not present\n", "%s %s %c %s\n"),
				    progname, from_dir, PATH_SEPARATOR[0], file);
			goto done;
		}
		dbenv->err(dbenv, ret, "%s", buf);
		goto err;
	}

	/* Open the output file. */
	if (snprintf(buf, MEGABYTE, "%s%c%s",
	    to_dir, PATH_SEPARATOR[0], file) >= MEGABYTE) {
		dbenv->errx(dbenv, DB_STR_A("5070", "%s%c%s: path too long",
		    "%s %c %s"), to_dir, PATH_SEPARATOR[0], file);
		goto err;
	}
	if ((ret = __os_open(env, buf, 0,
	    DB_OSO_CREATE | DB_OSO_TRUNC, DB_MODE_600, &wfhp)) != 0) {
		dbenv->err(dbenv, ret, "%s", buf);
		goto err;
	}

	/* Copy the data. */
	while ((ret = __os_read(env, rfhp, buf, MEGABYTE, &nr)) == 0 &&
	    nr > 0)
		if ((ret = __os_write(env, wfhp, buf, nr, &nw)) != 0)
			break;

	if (0) {
err:		ret = 1;
	}
done:	if (buf != NULL)
		free(buf);

	if (rfhp != NULL && __os_closehandle(env, rfhp) != 0)
		ret = 1;

	/* We may be running on a remote filesystem; force the flush. */
	if (wfhp != NULL) {
		if (__os_fsync(env, wfhp) != 0)
			ret = 1;
		if (__os_closehandle(env, wfhp) != 0)
			ret = 1;
	}
	return (ret);
}

char *saved_prefix;
char *saved_errstr;
void save_error(dbenv, prefix, errstr)
	const DB_ENV *dbenv;
	const char *prefix;
	const char *errstr;
{
	COMPQUIET(dbenv, NULL);
	saved_prefix = strdup(prefix);
	saved_errstr = strdup(errstr);
}

/*
 * env_data_copy --
 *	Copy a file into the backup directory through the environment.
 */
int
db_hotbackup_env_data_copy(dbenv, file, to_dir, verbose, passwd)
	DB_ENV *dbenv;
	char *file, *to_dir;
	int verbose;
	const char *passwd;
{
	FILE *savefile;
	int ret;

	ret = 0;

	if (verbose)
		printf(DB_STR_A("5066", "%s: copying database %s to %s%c%s",
		    "%s %s %s %c %s"), progname, 
		    file, to_dir, PATH_SEPARATOR[0], file);

	dbenv->set_errcall(dbenv, save_error);
	dbenv->get_errfile(dbenv, &savefile);
	dbenv->set_errfile(dbenv, NULL);
	if ((ret = db_copy(dbenv, file, to_dir, passwd)) != 0) {
	    	if (ret != ENOENT && ret != EINVAL) {
			if (saved_errstr != NULL)
				fprintf(stderr, "%s: %s\n",
				    saved_prefix, saved_errstr);
			else
				fprintf(stderr, " %s\n", db_strerror(ret));
		} else if (verbose)
			printf(": not a database\n");

		dbenv->set_errcall(dbenv, NULL);
		dbenv->set_errfile(dbenv, savefile);
		goto err;
	}
	dbenv->set_errcall(dbenv, NULL);
	dbenv->set_errfile(dbenv, savefile);

	if (verbose)
		printf("\n");

err:	if (saved_prefix != NULL)
		free(saved_prefix);
	if (saved_errstr != NULL)
		free(saved_errstr);
	saved_prefix = saved_errstr = NULL;
	return (ret);
}

int
db_hotbackup_usage()
{
	(void)fprintf(stderr, "usage: %s [-cDuVv]\n\t%s\n", progname,
    "[-d data_dir ...] [-h home] [-l log_dir] [-P password] -b backup_dir");
	return (EXIT_FAILURE);
}

int
db_hotbackup_version_check()
{
	int v_major, v_minor, v_patch;

	/* Make sure we're loaded with the right version of the DB library. */
	(void)db_version(&v_major, &v_minor, &v_patch);
	if (v_major != DB_VERSION_MAJOR || v_minor != DB_VERSION_MINOR) {
		fprintf(stderr, DB_STR_A("5071",
		    "%s: version %d.%d doesn't match library version %d.%d\n",
		    "%s %d %d %d %d\n"), progname,
		    DB_VERSION_MAJOR, DB_VERSION_MINOR,
		    v_major, v_minor);
		return (EXIT_FAILURE);
	}
	return (0);
}

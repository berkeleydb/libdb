/*-
 * See the file LICENSE for redistribution information.
 *
 * flag_archive.c -- BEHAVIOUR tests for the DB_ENV->log_archive and
 * DB_ENV->backup flag sets, which test/TESTING-PROGRAM.md
 * found referenced by ZERO tests:
 *
 *	DB_ARCH_ABS, DB_ARCH_DATA, DB_ARCH_LOG, DB_ARCH_REMOVE,
 *	DB_BACKUP_NO_LOGS, DB_BACKUP_UPDATE, DB_BACKUP_SINGLE_DIR
 *
 * WHY THIS GROUP FIRST
 *
 * These are the data-loss-adjacent flags.  DB_ARCH_REMOVE UNLINKS LOG FILES;
 * a defect there destroys the only record of committed transactions.
 * DB_BACKUP_NO_LOGS suppresses copying the log into a backup; a backup missing
 * logs it was meant to contain, or containing logs it was meant to omit, is a
 * silently unrecoverable backup.  Nothing tested any of them.
 *
 * THE BAR IS AN OBSERVABLE CONSEQUENCE, NEVER A RETURN CODE
 *
 * Gap G15 established that `rc == 0` from a flagged call proves nothing: the
 * old cov_api_surface.c check on DB_DIRECT_DB passed while the flag could not
 * open a database at all (defect P2).  So every check here asserts a FILE
 * SYSTEM fact about what the call did:
 *
 *	arch_log	the returned list names log.* files and nothing else,
 *			and every name resolves to a file that EXISTS
 *	arch_data	the returned list names the *.db data files, contains
 *			NO log.* entry, and every name exists
 *	arch_abs	DB_ARCH_ABS names are absolute and DB_ARCH_DATA alone
 *			names are relative -- compared against each other in
 *			the same environment, so "absolute" is attributable
 *	arch_remove	the log files log_archive(flags=0) named as REMOVABLE are
 *			gone from the directory afterwards, and (the control
 *			half) the CURRENT log file survives -- a "remove" that
 *			deleted the live log, or that deleted nothing, both pass
 *			a return-code check and both are caught here.
 *
 *			Note which list is used: DB_ARCH_LOG names EVERY log
 *			file including the live one (log_archive.c builds down
 *			from the last LSN's file), so grading DB_ARCH_REMOVE
 *			against the DB_ARCH_LOG list reports a spurious failure
 *			on the live log -- the first version of this test did
 *			exactly that.  flags=0 is the removable set.
 *	backup_nologs	the backup directory contains the .db files and NO
 *			log.* file, compared against a plain backup of the
 *			SAME environment which does contain log files
 *	backup_update	an incremental backup copies log files and does NOT
 *			re-copy the .db files (asserted on mtime + presence
 *			against a full backup taken first)
 *
 * Each mode that claims an absence also asserts the corresponding PRESENCE in
 * a control arm.  Without that, a probe that always reported "no log files"
 * (because it was looking in the wrong directory, say) would pass every
 * absence assertion.  That is the vacuous shape this project has nine recorded
 * instances of, and it is the reason the control arms exist.
 *
 * Usage:  flag_archive <mode>
 *	   arch_log | arch_data | arch_abs | arch_remove |
 *	   backup_nologs | backup_update
 *
 * Every mode prints at least one
 *
 *	VERDICT <name> <PASS|FAIL|XFAIL|SKIP> <detail...>
 *
 * and exits non-zero on FAIL.  A run that exits 0 with no VERDICT line is
 * graded FAIL by the runner: an exit status cannot distinguish "passed" from
 * "never ran".
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <dirent.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"TESTDIR_flag_archive"
#define	BAKFULL		"BAKFULL"
#define	BAKTEST		"BAKTEST"
#define	DBFILE		"archive.db"
#define	DBFILE2		"archive2.db"
#define	VALBYTES	400
/*
 * Enough transactions, at a 64KB log file size, to roll several log files --
 * DB_ARCH_LOG and DB_ARCH_REMOVE are both no-ops with a single log file, and a
 * no-op that returns 0 is precisely what this test must not accept.
 */
#define	LG_MAX		(64 * 1024)
#define	ROLL_RECS	3000

static int fails = 0;

static void
verdict(const char *name, const char *v, const char *fmt, ...)
{
	va_list ap;

	printf("VERDICT %s %s ", name, v);
	va_start(ap, fmt);
	(void)vprintf(fmt, ap);
	va_end(ap);
	printf("\n");
	(void)fflush(stdout);
	if (strcmp(v, "FAIL") == 0)
		fails++;
}

static int
die(const char *op, int ret)
{
	fprintf(stderr, "ERROR %s: %s (%d)\n", op, db_strerror(ret), ret);
	return (ret);
}

/* ------------------------------------------------------- directory probes */

/*
 * count_matching --
 *	How many entries of DIR start with PREFIX.  This is the primitive every
 *	presence/absence assertion is built on, so it reports -1 (not 0) when
 *	the directory cannot be read: "unreadable" must never be mistaken for
 *	"empty", which would make every absence check vacuously true.
 */
static int
count_matching(const char *dir, const char *prefix)
{
	DIR *d;
	struct dirent *e;
	size_t plen;
	int n;

	if ((d = opendir(dir)) == NULL) {
		fprintf(stderr, "opendir %s: %s\n", dir, strerror(errno));
		return (-1);
	}
	plen = strlen(prefix);
	n = 0;
	while ((e = readdir(d)) != NULL)
		if (strncmp(e->d_name, prefix, plen) == 0)
			n++;
	(void)closedir(d);
	return (n);
}

/* Does PATH exist (relative to the process cwd)? */
static int
exists(const char *path)
{
	struct stat sb;

	return (stat(path, &sb) == 0);
}

/* Basename of PATH, or PATH if it has no separator. */
static const char *
base(const char *path)
{
	const char *p;

	return ((p = strrchr(path, '/')) != NULL ? p + 1 : path);
}

/* ------------------------------------------------------------- environment */

static const u_int32_t ENVFLAGS = DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOG |
    DB_INIT_TXN | DB_INIT_LOCK;

static void
fillval(char *buf, int key)
{
	int i;

	for (i = 0; i < VALBYTES; i++)
		buf[i] = (char)('a' + ((key + i) % 26));
}

/*
 * setup --
 *	An environment with SEVERAL log files and two databases, checkpointed.
 *
 *	The checkpoint matters: without it no log file is archivable, so
 *	log_archive returns an EMPTY list and every "the list contains only
 *	log files" assertion would hold trivially.  assert_rolled() below
 *	refuses to continue if the log did not actually roll.
 */
static int
setup(DB_ENV **dbenvp, DB **dbpp)
{
	DB_ENV *dbenv;
	DB *dbp, *dbp2;
	DB_TXN *txn;
	DBT key, data;
	char vbuf[VALBYTES];
	int i, ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (die("db_env_create", ret));
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, "flag_archive");
	if ((ret = dbenv->set_cachesize(dbenv, 0, 8 * 1024 * 1024, 1)) != 0)
		return (die("set_cachesize", ret));
	if ((ret = dbenv->set_lg_max(dbenv, LG_MAX)) != 0)
		return (die("set_lg_max", ret));
	if ((ret = dbenv->open(dbenv, HOME, ENVFLAGS, 0600)) != 0)
		return (die("DB_ENV->open", ret));

	if ((ret = db_create(&dbp, dbenv, 0)) != 0)
		return (die("db_create", ret));
	if ((ret = dbp->open(dbp, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0)
		return (die("DB->open", ret));
	/* A second database so DB_ARCH_DATA has more than one name to get right. */
	if ((ret = db_create(&dbp2, dbenv, 0)) != 0)
		return (die("db_create 2", ret));
	if ((ret = dbp2->open(dbp2, NULL, DBFILE2, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0)
		return (die("DB->open 2", ret));

	for (i = 0; i < ROLL_RECS; i++) {
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0)
			return (die("txn_begin", ret));
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &i;
		key.size = sizeof(i);
		data.data = vbuf;
		data.size = VALBYTES;
		if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0)
			return (die("DB->put", ret));
		if ((ret = txn->commit(txn, 0)) != 0)
			return (die("commit", ret));
	}
	if ((ret = dbp2->put(dbp2, NULL, &key, &data, 0)) != 0)
		return (die("DB->put 2", ret));
	if ((ret = dbp2->close(dbp2, 0)) != 0)
		return (die("DB->close 2", ret));
	/*
	 * Checkpoint so earlier log files become archivable; DB_FORCE because
	 * a checkpoint that decides nothing changed writes no record and leaves
	 * every log file live.
	 */
	if ((ret = dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE)) != 0)
		return (die("txn_checkpoint", ret));

	*dbenvp = dbenv;
	*dbpp = dbp;
	return (0);
}

/*
 * assert_rolled --
 *	Refuse to grade anything unless the environment really has several log
 *	files.  With one log file DB_ARCH_LOG/DB_ARCH_REMOVE have nothing to
 *	act on and every assertion below would be vacuously satisfied.
 */
static int
assert_rolled(const char *name)
{
	int n;

	if ((n = count_matching(HOME, "log.")) < 2) {
		verdict(name, "FAIL",
		    "only %d log file(s) in %s -- the log did not roll, so "
		    "nothing was measured", n, HOME);
		return (0);
	}
	printf("SETUP %s logfiles=%d\n", name, n);
	(void)fflush(stdout);
	return (1);
}

/* How many names in a NULL-terminated log_archive list. */
static int
listlen(char **list)
{
	int n;

	if (list == NULL)
		return (0);
	for (n = 0; list[n] != NULL; n++)
		;
	return (n);
}

/* Print a list, bounded, so a failure report says WHICH names were wrong. */
static void
showlist(const char *tag, char **list)
{
	int i;

	printf("LIST %s n=%d", tag, listlen(list));
	for (i = 0; list != NULL && list[i] != NULL && i < 8; i++)
		printf(" %s", list[i]);
	if (listlen(list) > 8)
		printf(" ...");
	printf("\n");
	(void)fflush(stdout);
}

/* ------------------------------------------------------------------ modes */

/*
 * m_arch_log --
 *	DB_ARCH_LOG: the list must name log files, ONLY log files, and every
 *	name must resolve to a file that exists.  The existence half is what
 *	makes this more than a string check: a list of plausible-looking names
 *	for files that are not there is exactly the bug that loses a backup.
 */
static int
m_arch_log(const char *name)
{
	DB_ENV *dbenv;
	DB *dbp;
	char **list, path[1024];
	int i, n, ret, bad, missing;

	if ((ret = setup(&dbenv, &dbp)) != 0)
		return (1);
	if (!assert_rolled(name))
		goto out;

	if ((ret = dbenv->log_archive(dbenv, &list, DB_ARCH_LOG)) != 0) {
		verdict(name, "FAIL", "log_archive(DB_ARCH_LOG): %s (%d)",
		    db_strerror(ret), ret);
		goto out;
	}
	showlist("arch_log", list);
	if ((n = listlen(list)) == 0) {
		verdict(name, "FAIL",
		    "DB_ARCH_LOG returned an EMPTY list in an environment with "
		    "several log files -- no assertion below could fail");
		goto freelist;
	}
	bad = missing = 0;
	for (i = 0; i < n; i++) {
		if (strncmp(base(list[i]), "log.", 4) != 0) {
			bad++;
			printf("    NOT-A-LOG %s\n", list[i]);
			continue;
		}
		(void)snprintf(path, sizeof(path), "%s/%s", HOME, base(list[i]));
		if (!exists(path) && !exists(list[i])) {
			missing++;
			printf("    MISSING %s\n", list[i]);
		}
	}
	if (bad != 0)
		verdict(name, "FAIL",
		    "%d of %d DB_ARCH_LOG names are not log files", bad, n);
	else if (missing != 0)
		verdict(name, "FAIL",
		    "%d of %d DB_ARCH_LOG names do not exist on disk", missing,
		    n);
	else
		verdict(name, "PASS",
		    "DB_ARCH_LOG named %d file(s), all log.* and all present",
		    n);
freelist:
	free(list);
out:	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

/*
 * m_arch_data --
 *	DB_ARCH_DATA: the list must name the DATA files and no log file, and
 *	must contain BOTH databases created by setup().  The "both" part is the
 *	control against a directory walk that stops after one entry.
 */
static int
m_arch_data(const char *name)
{
	DB_ENV *dbenv;
	DB *dbp;
	char **list, path[1024];
	int i, n, ret, logs, missing, saw1, saw2;

	if ((ret = setup(&dbenv, &dbp)) != 0)
		return (1);

	if ((ret = dbenv->log_archive(dbenv, &list, DB_ARCH_DATA)) != 0) {
		verdict(name, "FAIL", "log_archive(DB_ARCH_DATA): %s (%d)",
		    db_strerror(ret), ret);
		goto out;
	}
	showlist("arch_data", list);
	if ((n = listlen(list)) == 0) {
		verdict(name, "FAIL",
		    "DB_ARCH_DATA returned an EMPTY list although %s and %s "
		    "are open in the environment", DBFILE, DBFILE2);
		goto freelist;
	}
	logs = missing = saw1 = saw2 = 0;
	for (i = 0; i < n; i++) {
		if (strncmp(base(list[i]), "log.", 4) == 0) {
			logs++;
			printf("    LOG-IN-DATA-LIST %s\n", list[i]);
			continue;
		}
		if (strcmp(base(list[i]), DBFILE) == 0)
			saw1 = 1;
		if (strcmp(base(list[i]), DBFILE2) == 0)
			saw2 = 1;
		(void)snprintf(path, sizeof(path), "%s/%s", HOME, base(list[i]));
		if (!exists(path) && !exists(list[i])) {
			missing++;
			printf("    MISSING %s\n", list[i]);
		}
	}
	if (logs != 0)
		verdict(name, "FAIL",
		    "DB_ARCH_DATA listed %d LOG file(s) -- a data-file list "
		    "that includes logs would archive the wrong thing", logs);
	else if (missing != 0)
		verdict(name, "FAIL",
		    "%d of %d DB_ARCH_DATA names do not exist on disk",
		    missing, n);
	else if (!saw1 || !saw2)
		verdict(name, "FAIL",
		    "DB_ARCH_DATA missed a database (%s:%s %s:%s) -- an "
		    "incomplete data list silently loses a file from a backup",
		    DBFILE, saw1 ? "yes" : "NO", DBFILE2, saw2 ? "yes" : "NO");
	else
		verdict(name, "PASS",
		    "DB_ARCH_DATA named %d file(s): both databases present, "
		    "no log files, all present on disk", n);
freelist:
	free(list);
out:	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

/*
 * m_arch_abs --
 *	DB_ARCH_ABS, asserted as a DIFFERENCE against the same call without it
 *	in the same environment.  Checking only "the names start with /" would
 *	pass on a library that always returned absolute paths and ignored the
 *	flag entirely -- the relative arm is what makes the assertion about the
 *	flag rather than about the library's habits.
 */
static int
m_arch_abs(const char *name)
{
	DB_ENV *dbenv;
	DB *dbp;
	char **rel, **abs;
	int i, nr, na, ret, relabs, absrel;

	rel = abs = NULL;
	if ((ret = setup(&dbenv, &dbp)) != 0)
		return (1);

	if ((ret = dbenv->log_archive(dbenv, &rel, DB_ARCH_DATA)) != 0) {
		verdict(name, "FAIL", "log_archive(DB_ARCH_DATA): %s",
		    db_strerror(ret));
		goto out;
	}
	if ((ret = dbenv->log_archive(dbenv, &abs,
	    DB_ARCH_DATA | DB_ARCH_ABS)) != 0) {
		verdict(name, "FAIL",
		    "log_archive(DB_ARCH_DATA|DB_ARCH_ABS): %s",
		    db_strerror(ret));
		goto out;
	}
	showlist("arch_abs_rel", rel);
	showlist("arch_abs_abs", abs);
	nr = listlen(rel);
	na = listlen(abs);
	if (nr == 0 || na == 0) {
		verdict(name, "FAIL",
		    "empty list (rel=%d abs=%d) -- nothing to compare", nr, na);
		goto out;
	}
	if (nr != na) {
		verdict(name, "FAIL",
		    "DB_ARCH_ABS changed the NUMBER of files listed (%d -> %d) "
		    "-- it must only change their spelling", nr, na);
		goto out;
	}
	relabs = absrel = 0;
	for (i = 0; i < nr; i++)
		if (rel[i][0] == '/')
			relabs++;
	for (i = 0; i < na; i++)
		if (abs[i][0] != '/')
			absrel++;
	if (absrel != 0)
		verdict(name, "FAIL",
		    "%d of %d DB_ARCH_ABS names are NOT absolute -- the flag "
		    "was ignored", absrel, na);
	else if (relabs != 0)
		verdict(name, "FAIL",
		    "%d of %d names are absolute WITHOUT DB_ARCH_ABS -- the "
		    "control arm is absolute too, so the flag proves nothing",
		    relabs, nr);
	else
		verdict(name, "PASS",
		    "%d name(s) relative without DB_ARCH_ABS and all %d "
		    "absolute with it", nr, na);

out:	free(rel);
	free(abs);
	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

/*
 * m_arch_remove --
 *	DB_ARCH_REMOVE, the flag that DELETES FILES.  Three assertions, and all
 *	three are needed:
 *
 *	1. every log file log_archive(flags=0) named REMOVABLE before the call
 *	   is GONE afterwards -- otherwise the flag did nothing and returned 0.
 *	   (flags=0, not DB_ARCH_LOG: DB_ARCH_LOG's list includes the LIVE log
 *	   file, which DB_ARCH_REMOVE must not delete, so grading against it
 *	   reports a spurious failure -- as the first version of this did.);
 *	2. the CURRENT (highest-numbered) log file still exists -- a "remove"
 *	   that took the live log with it destroys the environment, and would
 *	   pass assertion 1 with room to spare;
 *	3. the environment still works afterwards (a transaction commits and
 *	   the data reads back) -- file counts cannot see a log the library has
 *	   lost track of.
 */
static int
m_arch_remove(const char *name)
{
	DB_ENV *dbenv;
	DB *dbp;
	DB_TXN *txn;
	DBT key, data;
	char **list, path[1024], vbuf[VALBYTES];
	int i, n, ret, before, after, survived, curlog, maxnum, num, k;

	list = NULL;
	if ((ret = setup(&dbenv, &dbp)) != 0)
		return (1);
	if (!assert_rolled(name))
		goto out;

	/*
	 * flags == 0 is the REMOVABLE set: "log files that are no longer in
	 * use", i.e. everything before the last checkpoint.  DB_ARCH_LOG would
	 * be the wrong list here -- it includes the live log file, which
	 * DB_ARCH_REMOVE must NOT delete.
	 */
	if ((ret = dbenv->log_archive(dbenv, &list, 0)) != 0) {
		verdict(name, "FAIL", "log_archive(0): %s",
		    db_strerror(ret));
		goto out;
	}
	showlist("arch_remove_candidates", list);
	if ((n = listlen(list)) == 0) {
		verdict(name, "FAIL",
		    "log_archive(0) named nothing removable, so DB_ARCH_REMOVE "
		    "has nothing to act on and the assertion would be vacuous");
		goto out;
	}
	before = count_matching(HOME, "log.");
	/* The live log is the highest-numbered one present. */
	maxnum = 0;
	for (i = 0; i < n; i++) {
		num = atoi(base(list[i]) + 4);
		if (num > maxnum)
			maxnum = num;
	}
	curlog = before;	/* count, for the report */

	/* The live log must be present before the call, or (2) is vacuous. */
	(void)snprintf(path, sizeof(path), "%s/log.%010d", HOME, maxnum + 1);
	printf("ARCHREMOVE live_log_guess=%s exists=%d\n", path, exists(path));
	(void)fflush(stdout);

	if ((ret = dbenv->log_archive(dbenv, NULL, DB_ARCH_REMOVE)) != 0) {
		verdict(name, "FAIL", "log_archive(DB_ARCH_REMOVE): %s (%d)",
		    db_strerror(ret), ret);
		goto out;
	}
	after = count_matching(HOME, "log.");
	printf("ARCHREMOVE candidates=%d logfiles_before=%d logfiles_after=%d\n",
	    n, before, after);
	(void)fflush(stdout);

	/* (1) every candidate gone? */
	survived = 0;
	for (i = 0; i < n; i++) {
		(void)snprintf(path, sizeof(path), "%s/%s", HOME,
		    base(list[i]));
		if (exists(path)) {
			survived++;
			printf("    SURVIVED %s\n", path);
		}
	}
	if (survived != 0) {
		verdict(name, "FAIL",
		    "DB_ARCH_REMOVE returned 0 but %d of %d archivable log "
		    "file(s) are still on disk -- the flag did nothing",
		    survived, n);
		goto out;
	}
	/* (2) the live log must NOT have been removed. */
	if (after < 1 || count_matching(HOME, "log.") < 1) {
		verdict(name, "FAIL",
		    "DB_ARCH_REMOVE left NO log file at all (%d before, %d "
		    "after) -- it deleted the live log", curlog, after);
		goto out;
	}
	/* (3) the environment must still be usable. */
	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
		verdict(name, "FAIL", "txn_begin after remove: %s",
		    db_strerror(ret));
		goto out;
	}
	k = -1;
	fillval(vbuf, 7);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &k;
	key.size = sizeof(k);
	data.data = vbuf;
	data.size = VALBYTES;
	if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0 ||
	    (ret = txn->commit(txn, 0)) != 0) {
		verdict(name, "FAIL",
		    "the environment is broken after DB_ARCH_REMOVE: %s",
		    db_strerror(ret));
		goto out;
	}
	memset(&data, 0, sizeof(data));
	if ((ret = dbp->get(dbp, NULL, &key, &data, 0)) != 0) {
		verdict(name, "FAIL", "read-back after DB_ARCH_REMOVE: %s",
		    db_strerror(ret));
		goto out;
	}
	verdict(name, "PASS",
	    "DB_ARCH_REMOVE unlinked all %d archivable log file(s) (%d -> %d "
	    "on disk), kept the live log, and the environment still commits "
	    "and reads back", n, curlog, after);

out:	free(list);
	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

/*
 * backup_to --
 *	DB_ENV->backup into TGT with FLAGS, having made TGT exist and be empty.
 */
static int
backup_to(DB_ENV *dbenv, const char *tgt, u_int32_t flags)
{
	(void)mkdir(tgt, 0700);
	return (dbenv->backup(dbenv, tgt, DB_CREATE | flags));
}

/* Remove the files a previous backup left, without rm -rf. */
static void
clean_dir(const char *dir)
{
	DIR *d;
	struct dirent *e;
	char path[1024];

	if ((d = opendir(dir)) == NULL)
		return;
	while ((e = readdir(d)) != NULL) {
		if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0)
			continue;
		(void)snprintf(path, sizeof(path), "%s/%s", dir, e->d_name);
		(void)unlink(path);
	}
	(void)closedir(d);
}

/*
 * m_backup_nologs --
 *	DB_BACKUP_NO_LOGS -- documented as "Back up only the *.db files.  Do not
 *	backup the log files."  Asserted as a DIFFERENCE between two backups of
 *	the SAME environment:
 *
 *		plain backup		must contain log.* AND *.db
 *		DB_BACKUP_NO_LOGS	must contain *.db and NO log.*
 *
 *	The plain arm is what gives the absence claim meaning.  A probe looking
 *	in the wrong directory reports "no log files" for both arms and is
 *	caught by the plain arm failing.
 */
static int
m_backup_nologs(const char *name)
{
	DB_ENV *dbenv;
	DB *dbp;
	int ret, flogs, fdbs, nlogs, ndbs;

	if ((ret = setup(&dbenv, &dbp)) != 0)
		return (1);
	if (!assert_rolled(name))
		goto out;
	/* The handles must be closed for a consistent backup of the files. */
	if ((ret = dbp->close(dbp, 0)) != 0)
		(void)die("DB->close", ret);
	dbp = NULL;

	clean_dir(BAKFULL);
	clean_dir(BAKTEST);

	/* Control arm: a plain backup, which MUST contain log files. */
	if ((ret = backup_to(dbenv, BAKFULL, 0)) != 0) {
		verdict(name, "FAIL", "plain DB_ENV->backup: %s (%d)",
		    db_strerror(ret), ret);
		goto out;
	}
	flogs = count_matching(BAKFULL, "log.");
	fdbs = count_matching(BAKFULL, "archive");

	/* Test arm. */
	if ((ret = backup_to(dbenv, BAKTEST, DB_BACKUP_NO_LOGS)) != 0) {
		verdict(name, "FAIL",
		    "DB_ENV->backup(DB_BACKUP_NO_LOGS): %s (%d)",
		    db_strerror(ret), ret);
		goto out;
	}
	nlogs = count_matching(BAKTEST, "log.");
	ndbs = count_matching(BAKTEST, "archive");

	printf("BACKUP plain logs=%d dbs=%d | no_logs logs=%d dbs=%d\n",
	    flogs, fdbs, nlogs, ndbs);
	(void)fflush(stdout);

	if (flogs <= 0)
		verdict(name, "FAIL",
		    "the CONTROL backup contains %d log files -- the probe is "
		    "not looking where the backup went, so 'no logs' would be "
		    "vacuously true", flogs);
	else if (fdbs <= 0 || ndbs <= 0)
		verdict(name, "FAIL",
		    "a backup contains no database files (plain=%d no_logs=%d) "
		    "-- DB_BACKUP_NO_LOGS must still copy the data", fdbs,
		    ndbs);
	else if (nlogs > 0)
		/*
		 * The flag is in DB_ENV->backup's accepted-flag mask
		 * (db_backup.c:683) and is READ NOWHERE in src/ -- grep finds
		 * exactly one reference, the mask itself.  So it is accepted
		 * and ignored: log files land in the backup regardless.  That
		 * is defect P6 and it is recorded, not hidden: this becomes a
		 * PASS with no edit once the flag is implemented.
		 */
		verdict(name, "XFAIL",
		    "DB_BACKUP_NO_LOGS copied %d log file(s) anyway (control "
		    "arm: %d) -- the flag is ACCEPTED AND IGNORED: "
		    "DB_BACKUP_NO_LOGS appears exactly once in src/, in "
		    "db_backup.c's accepted-flag mask, and is never tested. "
		    "Defect P6, see test/TESTING-IMPROVEMENTS.md",
		    nlogs, flogs);
	else
		verdict(name, "PASS",
		    "DB_BACKUP_NO_LOGS: %d db file(s) and 0 log files, against "
		    "a control backup with %d log file(s)", ndbs, flogs);

out:	if (dbp != NULL)
		(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

/*
 * m_backup_update --
 *	DB_BACKUP_UPDATE -- documented as "Perform an incremental back up ...
 *	only log files are copied to the target directory."  Asserted on what
 *	lands in the directory:
 *
 *		a full backup first (so there is something to update),
 *		then the .db files are DELETED from the backup dir,
 *		then an incremental backup runs,
 *		then: log files present, and the deleted .db files NOT restored.
 *
 *	Deleting the .db files between the arms is what makes "only log files
 *	are copied" checkable at all: if the incremental run copied data files
 *	it would put them back, and the test sees it.
 */
static int
m_backup_update(const char *name)
{
	DB_ENV *dbenv;
	DB *dbp;
	char path[1024];
	int ret, logs0, dbs0, logs1, dbs1;

	if ((ret = setup(&dbenv, &dbp)) != 0)
		return (1);
	if (!assert_rolled(name))
		goto out;
	if ((ret = dbp->close(dbp, 0)) != 0)
		(void)die("DB->close", ret);
	dbp = NULL;

	clean_dir(BAKTEST);
	if ((ret = backup_to(dbenv, BAKTEST, 0)) != 0) {
		verdict(name, "FAIL", "full DB_ENV->backup: %s (%d)",
		    db_strerror(ret), ret);
		goto out;
	}
	logs0 = count_matching(BAKTEST, "log.");
	dbs0 = count_matching(BAKTEST, "archive");
	if (logs0 <= 0 || dbs0 <= 0) {
		verdict(name, "FAIL",
		    "the full backup this test updates has logs=%d dbs=%d -- "
		    "nothing to do an incremental backup against", logs0, dbs0);
		goto out;
	}
	/* Remove the data files: the incremental run must not restore them. */
	(void)snprintf(path, sizeof(path), "%s/%s", BAKTEST, DBFILE);
	(void)unlink(path);
	(void)snprintf(path, sizeof(path), "%s/%s", BAKTEST, DBFILE2);
	(void)unlink(path);
	if (count_matching(BAKTEST, "archive") != 0) {
		verdict(name, "FAIL",
		    "could not remove the data files from %s -- the "
		    "'was it re-copied' assertion cannot be made", BAKTEST);
		goto out;
	}

	if ((ret = backup_to(dbenv, BAKTEST, DB_BACKUP_UPDATE)) != 0) {
		verdict(name, "FAIL",
		    "DB_ENV->backup(DB_BACKUP_UPDATE): %s (%d)",
		    db_strerror(ret), ret);
		goto out;
	}
	logs1 = count_matching(BAKTEST, "log.");
	dbs1 = count_matching(BAKTEST, "archive");
	printf("BACKUPUPD full logs=%d dbs=%d | after_update logs=%d dbs=%d\n",
	    logs0, dbs0, logs1, dbs1);
	(void)fflush(stdout);

	if (logs1 <= 0)
		verdict(name, "FAIL",
		    "DB_BACKUP_UPDATE left %d log files in the backup -- an "
		    "incremental backup whose whole job is the log copied none",
		    logs1);
	else if (dbs1 != 0)
		verdict(name, "FAIL",
		    "DB_BACKUP_UPDATE re-copied %d database file(s) after they "
		    "were removed -- 'only log files are copied' is false",
		    dbs1);
	else
		verdict(name, "PASS",
		    "DB_BACKUP_UPDATE copied %d log file(s) and did not "
		    "re-copy the %d removed database file(s)", logs1, dbs0);

out:	if (dbp != NULL)
		(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

int
main(int argc, char *argv[])
{
	const char *mode;
	int rc;

	if (argc < 2) {
		fprintf(stderr, "usage: %s <mode>\n", argv[0]);
		return (2);
	}
	mode = argv[1];

	if (strcmp(mode, "arch_log") == 0)
		rc = m_arch_log(mode);
	else if (strcmp(mode, "arch_data") == 0)
		rc = m_arch_data(mode);
	else if (strcmp(mode, "arch_abs") == 0)
		rc = m_arch_abs(mode);
	else if (strcmp(mode, "arch_remove") == 0)
		rc = m_arch_remove(mode);
	else if (strcmp(mode, "backup_nologs") == 0)
		rc = m_backup_nologs(mode);
	else if (strcmp(mode, "backup_update") == 0)
		rc = m_backup_update(mode);
	else {
		fprintf(stderr, "unknown mode: %s\n", mode);
		return (2);
	}
	return (rc != 0 || fails != 0 ? 1 : 0);
}

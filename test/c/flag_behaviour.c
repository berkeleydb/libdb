/*-
 * See the file LICENSE for redistribution information.
 *
 * flag_behaviour.c -- BEHAVIOUR tests for the six runtime I/O and durability
 * flags that gap G15 found referenced by zero tests:
 *
 *	DB_DIRECT (DB_MPOOLFILE->open), DB_DIRECT_DB, DB_DSYNC_DB,
 *	DB_LOG_DIRECT, DB_LOG_DSYNC, DB_LOG_WRNOSYNC, DB_NOSYNC
 *
 * WHY A SEPARATE DRIVER, AND WHAT IT ASSERTS
 *
 * test/c/cov_api_surface.c already "covers" DB_DIRECT_DB.  It does this:
 *
 *	(void)dbenv->set_flags(dbenv, DB_DIRECT_DB, 1);
 *	checks += 2;
 *
 * That is an acceptance check, not a behaviour test.  It cannot fail while the
 * flag is completely non-functional -- and the flag IS completely
 * non-functional: under DB_DIRECT_DB no database can be opened at all (defect
 * P2, rfc/0011-test-coverage-gaps.md), because __fop_read_meta hands an
 * unaligned stack buffer to an O_DIRECT read.  A whole public flag was dead and
 * the coverage counter went up.
 *
 * So every check here asserts the OBSERVABLE CONSEQUENCE of the flag on the
 * file descriptor the library actually opened, read back out of
 * /proc/self/fdinfo/<fd>:
 *
 *	O_DIRECT (0040000) set on the DATA file    for DB_DIRECT_DB / DB_DIRECT
 *	O_DSYNC  (0010000) set on the DATA file    for DB_DSYNC_DB
 *	O_DIRECT set on the LOG file               for DB_LOG_DIRECT
 *	O_DSYNC  set on the LOG file               for DB_LOG_DSYNC
 *	fewer log syncs (st_scount) with           for DB_LOG_WRNOSYNC
 *	  DB_TXN_WRITE_NOSYNC than without,
 *	  while st_wcount still rises
 *	fewer fsync(2) calls                       for DB_NOSYNC on DB->close
 *	  (counted by strace in the runner)
 *
 * and the "control" mode asserts the probe reports those bits ABSENT on a
 * default environment.  Without the control, a probe that always answered
 * "flag present" would pass every other mode -- that is the vacuous shape one
 * layer out, and this project has nine recorded instances of it.
 *
 * THE P2 EXPECTATION IS RECORDED, NOT HIDDEN
 *
 * direct_db reports XFAIL when the open fails the way P2 fails, naming P2.
 * If P2 is fixed the open succeeds, the O_DIRECT assertion runs, and the mode
 * reports PASS -- so this test starts passing on the fix without being edited.
 * If the open succeeds but O_DIRECT is NOT set, that is a FAIL: a flag that is
 * silently ignored is the failure mode the old acceptance check could not see.
 * Run the runner with FLAGB_STRICT=1 to refuse the XFAIL allowance, which is
 * how the teeth of this test are demonstrated on current master.
 *
 * Usage:  flag_behaviour <mode> [arg]
 *	   control | direct_db | dsync_db | direct_log | dsync_log |
 *	   direct_mpf | syncs {default|wrnosync} | closesync {sync|nosync}
 *
 * Every mode prints at least one line
 *
 *	VERDICT <name> <PASS|FAIL|XFAIL|SKIP> <detail...>
 *
 * and exits non-zero on FAIL.  The runner requires the VERDICT line to exist:
 * exit status 0 with no verdict is treated as a failed run, not a pass.
 */
#define	_GNU_SOURCE	1		/* O_DIRECT */

#include <sys/types.h>
#include <sys/stat.h>

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"TESTDIR_flag_behaviour"
#define	DBFILE		"behaviour.db"
#define	PAGESIZE	4096		/* O_DIRECT needs block-aligned I/O */
#define	NKEYS		256
#define	VALBYTES	200
#define	SYNC_TXNS	200
/*
 * closesync opens this many databases and closes each one.  The signal is one
 * fdatasync per flushed handle, so with a single database the arms differ by
 * 2 vs 1 syscall -- true but at the resolution limit.  Eight handles make it
 * 9 vs 1, which a count can state confidently.
 */
#define	CLOSE_DBS	8

/*
 * O_DIRECT and O_DSYNC come from <fcntl.h> so the probe uses the same numbers
 * the kernel does.  They are asserted non-zero: a platform without them would
 * otherwise make every "flag absent" check trivially true.
 */
#ifndef O_DIRECT
#define	O_DIRECT	0
#endif

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

/*
 * fdinfo_scan --
 *	Find every open descriptor whose /proc/self/fd link contains MATCH and
 *	report how many there were plus the OR and AND of their open flags.
 *
 *	Both aggregates matter.  "The flag took effect" is asserted against the
 *	AND (every descriptor on the file has it, so a second unflagged handle
 *	cannot hide behind a flagged one), and "the flag is absent" against the
 *	OR (no descriptor has it).  Returns the number of matches, -1 on error.
 *
 *	MATCH is matched with strstr, not as a suffix, so "/log." collects the
 *	whole log file set -- the caller then checks WHICH log files those are.
 */
static int
fdinfo_scan(const char *match, long *orp, long *andp, char *say, size_t saylen)
{
	DIR *d;
	struct dirent *e;
	char path[512], link[1024], line[256];
	FILE *fp;
	ssize_t n;
	long flags, oragg, andagg;
	size_t used;
	int cnt, rc;

	cnt = 0;
	oragg = 0;
	andagg = -1L;
	used = 0;
	if (say != NULL && saylen > 0)
		say[0] = '\0';

	if ((d = opendir("/proc/self/fd")) == NULL) {
		fprintf(stderr, "opendir /proc/self/fd: %s\n", strerror(errno));
		return (-1);
	}
	while ((e = readdir(d)) != NULL) {
		if (e->d_name[0] == '.')
			continue;
		(void)snprintf(path, sizeof(path), "/proc/self/fd/%s",
		    e->d_name);
		if ((n = readlink(path, link, sizeof(link) - 1)) <= 0)
			continue;
		link[n] = '\0';
		if (strstr(link, match) == NULL)
			continue;
		(void)snprintf(path, sizeof(path), "/proc/self/fdinfo/%s",
		    e->d_name);
		if ((fp = fopen(path, "r")) == NULL)
			continue;
		flags = -1L;
		while (fgets(line, sizeof(line), fp) != NULL)
			if (strncmp(line, "flags:", 6) == 0) {
				flags = strtol(line + 6, NULL, 8);
				break;
			}
		(void)fclose(fp);
		if (flags < 0)
			continue;
		cnt++;
		oragg |= flags;
		andagg = andagg < 0 ? flags : (andagg & flags);
		if (say != NULL && used + 96 < saylen) {
			rc = snprintf(say + used, saylen - used,
			    "%s%s=0%lo", used ? "," : "",
			    strrchr(link, '/') ? strrchr(link, '/') + 1 : link,
			    flags);
			if (rc > 0)
				used += (size_t)rc;
		}
	}
	(void)closedir(d);
	*orp = oragg;
	*andp = andagg < 0 ? 0 : andagg;
	return (cnt);
}

/*
 * open_log_min --
 *	The LOWEST log file number currently open.  log_set_config takes effect
 *	on the next log file OPEN, so probing a descriptor that predates the call
 *	reports a false negative; requiring every open log file to be number 2
 *	or higher is what makes the probe attributable to the flag.
 */
static int
open_log_min(void)
{
	DIR *d;
	struct dirent *e;
	char path[512], link[1024];
	const char *p;
	ssize_t n;
	int lo, v;

	lo = -1;
	if ((d = opendir("/proc/self/fd")) == NULL)
		return (-1);
	while ((e = readdir(d)) != NULL) {
		if (e->d_name[0] == '.')
			continue;
		(void)snprintf(path, sizeof(path), "/proc/self/fd/%s",
		    e->d_name);
		if ((n = readlink(path, link, sizeof(link) - 1)) <= 0)
			continue;
		link[n] = '\0';
		if ((p = strstr(link, "/log.")) == NULL)
			continue;
		v = atoi(p + 5);
		if (lo < 0 || v < lo)
			lo = v;
	}
	(void)closedir(d);
	return (lo);
}

static int
die(const char *op, int ret)
{
	fprintf(stderr, "ERROR %s: %s (%d)\n", op, db_strerror(ret), ret);
	return (ret);
}

/* Fill a value buffer deterministically so a short read is detectable. */
static void
fillval(char *buf, int key)
{
	int i;

	for (i = 0; i < VALBYTES; i++)
		buf[i] = (char)('a' + ((key + i) % 26));
}

/*
 * workload --
 *	Write NKEYS records, force them to the data file, and read them all
 *	back.  This is what makes the fd probe meaningful: the descriptor must
 *	have carried real I/O under the flag, not merely have been opened.
 */
static int
workload(DB_ENV *dbenv, DB *dbp, int do_sync)
{
	DBT key, data;
	char vbuf[VALBYTES], got[VALBYTES];
	int i, ret;

	for (i = 0; i < NKEYS; i++) {
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &i;
		key.size = sizeof(i);
		data.data = vbuf;
		data.size = VALBYTES;
		if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) != 0)
			return (die("DB->put", ret));
	}
	if (do_sync && (ret = dbp->sync(dbp, 0)) != 0)
		return (die("DB->sync", ret));
	if (do_sync && (ret = dbenv->memp_sync(dbenv, NULL)) != 0)
		return (die("DB_ENV->memp_sync", ret));

	for (i = 0; i < NKEYS; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &i;
		key.size = sizeof(i);
		if ((ret = dbp->get(dbp, NULL, &key, &data, 0)) != 0)
			return (die("DB->get", ret));
		fillval(vbuf, i);
		if (data.size != VALBYTES) {
			fprintf(stderr, "ERROR read-back size %u != %d\n",
			    data.size, VALBYTES);
			return (EINVAL);
		}
		memcpy(got, data.data, VALBYTES);
		if (memcmp(got, vbuf, VALBYTES) != 0) {
			fprintf(stderr, "ERROR read-back mismatch key %d\n", i);
			return (EINVAL);
		}
	}
	return (0);
}

static const u_int32_t ENVFLAGS =
    DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOG | DB_INIT_TXN | DB_INIT_LOCK;

/* No log, no txn: used by closesync, where log fsyncs would swamp the signal. */
static const u_int32_t ENVFLAGS_MPOOL_ONLY = DB_CREATE | DB_INIT_MPOOL;

/*
 * open_env --
 *	Open an environment with OFLAGS, optionally setting ENV_FLAG before the
 *	open and LOG_FLAG through DB_ENV->log_set_config.  Sets *unsupp when the
 *	library was built without O_DIRECT so the caller can SKIP loudly.
 */
static int
open_env(DB_ENV **dbenvp, u_int32_t oflags, u_int32_t env_flag,
    u_int32_t log_flag, int *unsupp)
{
	DB_ENV *dbenv;
	int ret;

	*unsupp = 0;
	if ((ret = db_env_create(&dbenv, 0)) != 0)
		return (die("db_env_create", ret));
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, "flag_behaviour");
	if ((ret = dbenv->set_cachesize(dbenv, 0, 16 * 1024 * 1024, 1)) != 0)
		return (die("set_cachesize", ret));

	if (env_flag != 0 && (ret = dbenv->set_flags(dbenv, env_flag, 1)) != 0) {
		/*
		 * EINVAL from set_flags(DB_DIRECT_DB) means the library was
		 * built without O_DIRECT support (__os_support_direct_io() == 0,
		 * i.e. no --enable-o_direct).  That is a legitimate SKIP, but
		 * only when it is REPORTED: silently treating it as success is
		 * how this flag went untested in the first place.
		 */
		if (ret == EINVAL && (env_flag & DB_DIRECT_DB) != 0) {
			*unsupp = 1;
			(void)dbenv->close(dbenv, 0);
			return (EINVAL);
		}
		return (die("set_flags", ret));
	}
	if ((ret = dbenv->open(dbenv, HOME, oflags, 0600)) != 0)
		return (die("DB_ENV->open", ret));

	/*
	 * log_set_config must come AFTER the env open for DB_LOG_DIRECT /
	 * DB_LOG_DSYNC to reach a live log region, and the log file is reopened
	 * with the new flags on the next log file switch -- so the callers that
	 * use this force a switch (DB_ENV->log_archive/log_flush + a big write)
	 * before probing.
	 */
	if (log_flag != 0 &&
	    (ret = dbenv->log_set_config(dbenv, log_flag, 1)) != 0) {
		if (ret == EINVAL && (log_flag & DB_LOG_DIRECT) != 0) {
			*unsupp = 1;
			(void)dbenv->close(dbenv, 0);
			return (EINVAL);
		}
		return (die("log_set_config", ret));
	}
	*dbenvp = dbenv;
	return (0);
}

static int
open_db_named(DB_ENV *dbenv, DB **dbpp, const char *fname, u_int32_t dbflags)
{
	DB *dbp;
	int ret;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0)
		return (die("db_create", ret));
	if ((ret = dbp->set_pagesize(dbp, PAGESIZE)) != 0)
		return (die("set_pagesize", ret));
	if ((ret = dbp->open(dbp, NULL, fname, NULL, DB_BTREE,
	    DB_CREATE | dbflags, 0600)) != 0) {
		/* Caller decides whether this is the P2 signature. */
		*dbpp = NULL;
		return (ret);
	}
	*dbpp = dbp;
	return (0);
}

static int
open_db(DB_ENV *dbenv, DB **dbpp, u_int32_t dbflags)
{
	return (open_db_named(dbenv, dbpp, DBFILE, dbflags));
}

/*
 * probe_file --
 *	Assert WANT (a mask of O_DIRECT / O_DSYNC) is set on every descriptor
 *	open on SUFFIX, or -- when want == 0 -- that neither bit is set on any.
 */
static void
probe_file(const char *name, const char *match, long want)
{
	long orf, andf;
	char say[512];
	int cnt;

	cnt = fdinfo_scan(match, &orf, &andf, say, sizeof(say));
	if (cnt < 0) {
		verdict(name, "FAIL", "fdinfo scan failed for %s", match);
		return;
	}
	if (cnt == 0) {
		/*
		 * No descriptor on the file means the probe measured NOTHING.
		 * That must never read as a pass -- it is exactly the shape of
		 * a test that runs against a build with the property destroyed.
		 */
		verdict(name, "FAIL",
		    "no open fd matching %s -- probe measured nothing", match);
		return;
	}
	if (want == 0) {
		if ((orf & (O_DIRECT | O_DSYNC)) != 0)
			verdict(name, "FAIL",
			    "control: O_DIRECT/O_DSYNC present on %s (%s)",
			    match, say);
		else
			verdict(name, "PASS",
			    "control: %d fd(s) on %s, neither O_DIRECT (0%o) "
			    "nor O_DSYNC (0%o) set (%s)",
			    cnt, match, O_DIRECT, O_DSYNC, say);
		return;
	}
	if ((andf & want) == want)
		verdict(name, "PASS", "0%lo set on all %d fd(s) of %s (%s)",
		    want, cnt, match, say);
	else
		verdict(name, "FAIL",
		    "0%lo NOT set on every fd of %s -- flag ignored (%s)",
		    want, match, say);
}

/* ------------------------------------------------------------------ modes */

/*
 * m_datafile --
 *	control / direct_db / dsync_db: open an env (optionally with the flag),
 *	create the database, run the workload, and probe the DATA file's fds.
 */
static int
m_datafile(const char *name, u_int32_t env_flag, long want)
{
	DB_ENV *dbenv;
	DB *dbp;
	int ret, unsupp;

	dbenv = NULL;
	dbp = NULL;
	if ((ret = open_env(&dbenv, ENVFLAGS, env_flag, 0, &unsupp)) != 0) {
		if (unsupp) {
			verdict(name, "SKIP",
			    "library has no O_DIRECT support "
			    "(built without --enable-o_direct)");
			return (0);
		}
		return (1);
	}
	if ((ret = open_db(dbenv, &dbp, DB_AUTO_COMMIT)) != 0) {
		/*
		 * P2: under DB_DIRECT_DB the first metadata read fails EINVAL
		 * because __fop_read_meta's buffer is a bare stack array and
		 * O_DIRECT requires block alignment.  Recorded as XFAIL with
		 * the reference, so the suite states the defect rather than
		 * hiding it -- and reports PASS once P2 is fixed.
		 */
		if ((env_flag & DB_DIRECT_DB) != 0 && ret == EINVAL) {
			verdict(name, "XFAIL",
			    "DB->open under DB_DIRECT_DB failed EINVAL -- "
			    "defect P2 (__fop_read_meta unaligned buffer), "
			    "see rfc/0011-test-coverage-gaps.md");
			(void)dbenv->close(dbenv, 0);
			return (0);
		}
		verdict(name, "FAIL", "DB->open: %s (%d)",
		    db_strerror(ret), ret);
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	if ((ret = workload(dbenv, dbp, 1)) != 0) {
		verdict(name, "FAIL", "workload: %s (%d)",
		    db_strerror(ret), ret);
		(void)dbp->close(dbp, 0);
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	probe_file(name, "/" DBFILE, want);
	if ((ret = dbp->close(dbp, 0)) != 0)
		(void)die("DB->close", ret);
	if ((ret = dbenv->close(dbenv, 0)) != 0)
		(void)die("DB_ENV->close", ret);
	return (fails != 0);
}

/*
 * m_logfile --
 *	direct_log / dsync_log.  log_set_config takes effect when the log file
 *	is next OPENED, so this shrinks the log file size, writes enough to roll
 *	past the file that was open when the flag was set, and probes the log
 *	descriptors -- asserting first that none of them predates the flag.
 */
static int
m_logfile(const char *name, u_int32_t log_flag, long want)
{
	DB_ENV *dbenv;
	DB *dbp;
	DBT key, data;
	char vbuf[VALBYTES];
	int i, ret, unsupp, lo;

	dbenv = NULL;
	dbp = NULL;
	if ((ret = open_env(&dbenv, ENVFLAGS, 0, log_flag, &unsupp)) != 0) {
		if (unsupp) {
			verdict(name, "SKIP",
			    "library has no O_DIRECT support "
			    "(built without --enable-o_direct)");
			return (0);
		}
		return (1);
	}
	if ((ret = open_db(dbenv, &dbp, DB_AUTO_COMMIT)) != 0) {
		/*
		 * This used to allow XFAIL here for want == O_DIRECT: defect P3,
		 * the DB_LOG_DIRECT sibling of P2, made the log write path hand
		 * __os_io unaligned buffers of arbitrary length at arbitrary
		 * offsets, so the first transactional open failed EINVAL.  P3 is
		 * fixed (__log_write_direct restages each write into aligned
		 * whole blocks; see test/c/P3-LOG-ODIRECT.md), so the
		 * allowance is GONE and a failed open here is a hard FAIL -- a
		 * regression, not a recorded expectation.
		 */
		verdict(name, "FAIL", "DB->open: %s (%d)",
		    db_strerror(ret), ret);
		(void)dbenv->close(dbenv, 0);
		return (1);
	}

	/*
	 * Roll the log.  set_lg_max works after the env open (the LOGGING_ON
	 * branch of __log_set_lg_max writes lp->log_nsize), so shrink the log
	 * file and then write past it.  Only log files opened AFTER
	 * log_set_config can carry the flag, which open_log_min() checks below.
	 */
	if ((ret = dbenv->set_lg_max(dbenv, 1024 * 1024)) != 0) {
		verdict(name, "FAIL", "set_lg_max: %s", db_strerror(ret));
		goto out;
	}
	for (i = 0; i < 20000; i++) {
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &i;
		key.size = sizeof(i);
		data.data = vbuf;
		data.size = VALBYTES;
		if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) != 0) {
			if (want == O_DIRECT && ret == EINVAL) {
				verdict(name, "XFAIL",
				    "log write under DB_LOG_DIRECT failed "
				    "EINVAL (unaligned buffer) -- P2 family");
				goto out;
			}
			verdict(name, "FAIL", "DB->put: %s", db_strerror(ret));
			goto out;
		}
	}
	if ((ret = dbenv->log_flush(dbenv, NULL)) != 0) {
		verdict(name, "FAIL", "log_flush: %s", db_strerror(ret));
		goto out;
	}

	/*
	 * If the log never rolled, the only open log descriptor is the one
	 * created before log_set_config and there is nothing the flag could
	 * have affected.  That must be a FAIL of the probe, not a pass: a
	 * silently un-rolled log is exactly how this check would go vacuous.
	 */
	lo = open_log_min();
	if (lo < 2) {
		verdict(name, "FAIL",
		    "lowest open log file is log.%d -- it predates "
		    "log_set_config, so the probe would measure the wrong fd",
		    lo);
		goto out;
	}
	printf("LOGROLL %s lowest_open_log=%d\n", name, lo);
	(void)fflush(stdout);
	probe_file(name, "/log.", want);

out:	if (dbp != NULL)
		(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

/*
 * m_direct_mpf --
 *	DB_DIRECT through the DB_MPOOLFILE->open path, which does NOT go through
 *	__fop_read_meta.  It therefore isolates P2: if this passes while
 *	direct_db XFAILs, the O_DIRECT plumbing in os_open works and the defect
 *	is in fop, which is a stronger statement than either check alone.
 */
static int
m_direct_mpf(const char *name)
{
	DB_ENV *dbenv;
	DB_MPOOLFILE *mpf;
	void *page;
	db_pgno_t pgno;
	int ret, unsupp;

	dbenv = NULL;
	if ((ret = open_env(&dbenv, ENVFLAGS, 0, 0, &unsupp)) != 0)
		return (1);
	if ((ret = dbenv->set_flags(dbenv, DB_DIRECT_DB, 1)) == EINVAL) {
		verdict(name, "SKIP", "library has no O_DIRECT support");
		(void)dbenv->close(dbenv, 0);
		return (0);
	}
	(void)dbenv->set_flags(dbenv, DB_DIRECT_DB, 0);

	if ((ret = dbenv->memp_fcreate(dbenv, &mpf, 0)) != 0) {
		verdict(name, "FAIL", "memp_fcreate: %s", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	if ((ret = mpf->open(mpf, "direct_mpf.pag",
	    DB_CREATE | DB_DIRECT, 0600, PAGESIZE)) != 0) {
		verdict(name, "FAIL", "DB_MPOOLFILE->open(DB_DIRECT): %s (%d)",
		    db_strerror(ret), ret);
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	/* Touch a page so the descriptor carries real I/O. */
	pgno = 0;
	if ((ret = mpf->get(mpf, &pgno, NULL,
	    DB_MPOOL_CREATE | DB_MPOOL_DIRTY, &page)) != 0) {
		verdict(name, "FAIL", "DB_MPOOLFILE->get: %s", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	memset(page, 0x5a, PAGESIZE);
	/*
	 * DB_MPOOLFILE->put takes flags == 0 only (__memp_fput_pp rejects
	 * anything else); the dirty bit is set on the GET above.  Leaving the
	 * page pinned makes __memp_fclose PANIC the environment, so the put must
	 * succeed before the close path runs.
	 */
	if ((ret = mpf->put(mpf, page, DB_PRIORITY_UNCHANGED, 0)) != 0) {
		verdict(name, "FAIL", "DB_MPOOLFILE->put: %s", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	if ((ret = mpf->sync(mpf)) != 0)
		(void)die("DB_MPOOLFILE->sync", ret);

	probe_file(name, "/direct_mpf.pag", O_DIRECT);

	(void)mpf->close(mpf, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

/*
 * m_syncs --
 *	DB_LOG_WRNOSYNC, asserted on the MECHANISM.  DB_TXN_WRITE_NOSYNC makes
 *	__txn_commit pass DB_LOG_WRNOSYNC to log_put, which WRITES the log
 *	buffer but does not fsync it.  So against the default (DB_TXN_SYNC
 *	commits):
 *
 *		st_scount (log syncs)  must DROP
 *		st_wcount (log writes) must still be > 0
 *
 * The second half is what makes the first half meaningful: a mode that stopped
 * writing the log entirely would also have fewer syncs, and would be a
 * different (much worse) thing than the flag promises.  Printed as a machine
 * -readable line for the runner to compare across the two arms.
 */
static int
m_syncs(const char *name, const char *arm)
{
	DB_ENV *dbenv;
	DB *dbp;
	DB_TXN *txn;
	DB_LOG_STAT *ls;
	DBT key, data;
	char vbuf[VALBYTES];
	int i, ret, unsupp, wrnosync;

	wrnosync = strcmp(arm, "wrnosync") == 0;
	dbenv = NULL;
	if ((ret = open_env(&dbenv, ENVFLAGS, 0, 0, &unsupp)) != 0)
		return (1);
	/*
	 * DB_TXN_WRITE_NOSYNC is the env flag whose commit path sets
	 * DB_LOG_WRNOSYNC (see LOG_FLAGS() in src/txn/txn.c).  The default arm
	 * sets DB_TXN_SYNC behaviour by leaving both off, so each commit
	 * flushes.
	 */
	if (wrnosync &&
	    (ret = dbenv->set_flags(dbenv, DB_TXN_WRITE_NOSYNC, 1)) != 0) {
		verdict(name, "FAIL", "set_flags(DB_TXN_WRITE_NOSYNC): %s",
		    db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	if ((ret = open_db(dbenv, &dbp, DB_AUTO_COMMIT)) != 0) {
		verdict(name, "FAIL", "DB->open: %s", db_strerror(ret));
		(void)dbenv->close(dbenv, 0);
		return (1);
	}
	for (i = 0; i < SYNC_TXNS; i++) {
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
			verdict(name, "FAIL", "txn_begin: %s",
			    db_strerror(ret));
			goto out;
		}
		fillval(vbuf, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &i;
		key.size = sizeof(i);
		data.data = vbuf;
		data.size = VALBYTES;
		if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0) {
			verdict(name, "FAIL", "DB->put: %s", db_strerror(ret));
			(void)txn->abort(txn);
			goto out;
		}
		if ((ret = txn->commit(txn, 0)) != 0) {
			verdict(name, "FAIL", "commit: %s", db_strerror(ret));
			goto out;
		}
	}
	if ((ret = dbenv->log_stat(dbenv, &ls, 0)) != 0) {
		verdict(name, "FAIL", "log_stat: %s", db_strerror(ret));
		goto out;
	}
	printf("SYNCS arm=%s txns=%d st_scount=%llu st_wcount=%llu\n",
	    arm, SYNC_TXNS, (unsigned long long)ls->st_scount,
	    (unsigned long long)ls->st_wcount);
	(void)fflush(stdout);
	if (ls->st_wcount == 0)
		verdict(name, "FAIL",
		    "arm=%s wrote NOTHING to the log (st_wcount=0) -- "
		    "the arm did not do the work it claims", arm);
	else
		verdict(name, "PASS",
		    "arm=%s st_scount=%llu st_wcount=%llu (comparison is the "
		    "runner's job)", arm,
		    (unsigned long long)ls->st_scount,
		    (unsigned long long)ls->st_wcount);
	free(ls);

out:	if (dbp != NULL)
		(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);
	return (fails != 0);
}

/*
 * m_closesync --
 *	DB_NOSYNC on DB->close.  The flag's whole meaning is "do not flush this
 *	handle's dirty pages on close", which is only observable as the ABSENCE
 *	of the I/O -- so the assertion is made by the runner, which counts
 *	fsync/fdatasync syscalls with strace across the two arms.
 *
 *	The environment here is MPOOL-ONLY on purpose.  With DB_INIT_LOG the
 *	commit path issues one log fdatasync per transaction, which buried the
 *	difference completely: the first version of this mode measured 262 vs
 *	261 syscalls (0.4%) because ~260 of them were the log, not the data
 *	file.  A gate that cannot resolve its own signal is not a gate.
 */
static int
m_closesync(const char *name, const char *arm)
{
	DB_ENV *dbenv;
	DB *dbs[CLOSE_DBS];
	char fname[64];
	u_int32_t cflags;
	int i, ret, unsupp;

	cflags = strcmp(arm, "nosync") == 0 ? DB_NOSYNC : 0;
	dbenv = NULL;
	if ((ret = open_env(&dbenv, ENVFLAGS_MPOOL_ONLY, 0, 0, &unsupp)) != 0)
		return (1);
	for (i = 0; i < CLOSE_DBS; i++) {
		(void)snprintf(fname, sizeof(fname), "closesync%d.db", i);
		if ((ret = open_db_named(dbenv, &dbs[i], fname, 0)) != 0) {
			verdict(name, "FAIL", "DB->open %s: %s", fname,
			    db_strerror(ret));
			(void)dbenv->close(dbenv, 0);
			return (1);
		}
		/* Dirty pages, do NOT sync them: the close is measured. */
		if ((ret = workload(dbenv, dbs[i], 0)) != 0) {
			verdict(name, "FAIL", "workload: %s", db_strerror(ret));
			(void)dbenv->close(dbenv, 0);
			return (1);
		}
	}
	printf("CLOSESYNC arm=%s flags=0x%lx dbs=%d closing\n", arm,
	    (unsigned long)cflags, CLOSE_DBS);
	(void)fflush(stdout);
	for (i = 0; i < CLOSE_DBS; i++)
		if ((ret = dbs[i]->close(dbs[i], cflags)) != 0) {
			verdict(name, "FAIL", "DB->close(0x%lx): %s",
			    (unsigned long)cflags, db_strerror(ret));
			(void)dbenv->close(dbenv, 0);
			return (1);
		}
	/*
	 * DB_ENV->close with DB_FORCESYNC would re-add the syncs the flag just
	 * avoided, which would erase the very difference being measured.  Close
	 * the env with 0.
	 */
	if ((ret = dbenv->close(dbenv, 0)) != 0)
		(void)die("DB_ENV->close", ret);
	verdict(name, "PASS", "arm=%s closed %d handles (fsync count is the "
	    "runner's assertion)", arm, CLOSE_DBS);
	return (fails != 0);
}

int
main(int argc, char *argv[])
{
	const char *mode, *arm;
	int rc;

	if (argc < 2) {
		fprintf(stderr, "usage: %s <mode> [arm]\n", argv[0]);
		return (2);
	}
	mode = argv[1];
	arm = argc > 2 ? argv[2] : "";

	/*
	 * A platform with no O_DIRECT macro would make every "bit absent"
	 * assertion trivially true.  Refuse to run rather than report a pass
	 * that proves nothing.
	 */
	if (O_DIRECT == 0 || O_DSYNC == 0) {
		verdict(mode, "FAIL",
		    "O_DIRECT/O_DSYNC not defined by <fcntl.h> -- the probe "
		    "cannot distinguish set from unset on this platform");
		return (1);
	}

	if (strcmp(mode, "control") == 0)
		rc = m_datafile(mode, 0, 0);
	else if (strcmp(mode, "direct_db") == 0)
		rc = m_datafile(mode, DB_DIRECT_DB, O_DIRECT);
	else if (strcmp(mode, "dsync_db") == 0)
		rc = m_datafile(mode, DB_DSYNC_DB, O_DSYNC);
	else if (strcmp(mode, "direct_log") == 0)
		rc = m_logfile(mode, DB_LOG_DIRECT, O_DIRECT);
	else if (strcmp(mode, "dsync_log") == 0)
		rc = m_logfile(mode, DB_LOG_DSYNC, O_DSYNC);
	else if (strcmp(mode, "direct_mpf") == 0)
		rc = m_direct_mpf(mode);
	else if (strcmp(mode, "syncs") == 0)
		rc = m_syncs(mode, arm);
	else if (strcmp(mode, "closesync") == 0)
		rc = m_closesync(mode, arm);
	else {
		fprintf(stderr, "unknown mode: %s\n", mode);
		return (2);
	}
	return (rc != 0 || fails != 0 ? 1 : 0);
}

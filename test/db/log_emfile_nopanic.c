/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 *
 * $Id$
 *
 * log_emfile_nopanic -- regression test for the environment panic when a
 * log-file open fails with EMFILE/ENFILE (the process is out of file
 * descriptors).  __log_name (src/log/log_put.c) treated any non-ENOENT open
 * error as fatal and panicked the whole environment; in diagnostic builds
 * that aborts the process.  A transient, process-local descriptor shortage
 * must instead be returned to the caller so it can free descriptors and
 * retry, leaving the shared environment usable.
 *
 * The condition is injected deterministically through
 * db_env_set_func_open: the installed open returns EMFILE for log files
 * only.  No real descriptor exhaustion (and so no interference with the
 * test harness) is needed.
 */
#include <sys/types.h>

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "db.h"

#define	VALSZ	4096
#define	NWRITES	200

static int fails;
static volatile int inject;		/* fail log-file opens while set */
static volatile int injected;		/* count of injected failures */
static volatile int inject_region;	/* fail region (__db.NNN) opens after N */
static volatile int region_seen;	/* count of region opens observed */

#define	CHECK(cond, ...)						\
	do {								\
		if (!(cond)) {						\
			printf("  FAIL: ");				\
			printf(__VA_ARGS__);				\
			printf("\n");					\
			fails++;					\
		}							\
	} while (0)

static int
my_open(const char *name, int flags, ...)
{
	va_list ap;
	int mode;
	const char *base;

	va_start(ap, flags);
	mode = va_arg(ap, int);
	va_end(ap);

	base = strrchr(name, '/');
	base = base ? base + 1 : name;
	if (inject_region > 0 && strncmp(base, "__db.", 5) == 0) {
		/*
		 * Fail a region-file open during env create, after the first
		 * REGION_CREATE region exists -- the point where
		 * __env_attach_regions used to panic (abort, in diagnostic
		 * builds) on a transient EMFILE instead of returning it.
		 */
		if (++region_seen > inject_region) {
			injected++;
			errno = EMFILE;
			return (-1);
		}
	}
	if (inject && strncmp(base, "log.", 4) == 0) {
		injected++;
		errno = EMFILE;
		return (-1);
	}
	return (open(name, flags, mode));
}

/*
 * run -- write enough to roll the log so a new log file must be opened.
 * With do_inject set, that open gets EMFILE.  Afterwards the environment
 * must still be alive (a read returns something other than DB_RUNRECOVERY).
 */
static void
run(const char *home, int do_inject)
{
	static char big[VALSZ];
	char kb[16];
	DB_ENV *e;
	DB *d;
	DBT k, v;
	int i, r, panicked;

	fails = 0;
	injected = 0;
	if ((r = db_env_create(&e, 0)) != 0) {
		CHECK(0, "db_env_create: %s", db_strerror(r));
		return;
	}
	e->set_errpfx(e, "log_emfile_nopanic");
	(void)e->set_lg_max(e, 64 * 1024);	/* small logs: roll quickly */
	if ((r = e->open(e, home, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0644)) != 0) {
		CHECK(0, "env open: %s", db_strerror(r));
		(void)e->close(e, 0);
		return;
	}
	if ((r = db_create(&d, e, 0)) != 0 ||
	    (r = d->open(d, NULL, "f.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
		CHECK(0, "db open: %s", db_strerror(r));
		(void)e->close(e, 0);
		return;
	}

	memset(&k, 0, sizeof(k));
	memset(&v, 0, sizeof(v));

	inject = do_inject;
	r = 0;
	for (i = 0; i < NWRITES && r == 0; i++) {
		snprintf(kb, sizeof(kb), "k%06d", i);
		k.data = kb;
		k.size = (u_int32_t)strlen(kb);
		v.data = big;
		v.size = sizeof(big);
		r = d->put(d, NULL, &k, &v, DB_AUTO_COMMIT);
	}
	inject = 0;

	if (do_inject) {
		CHECK(injected > 0,
		    "injection never forced a log open (inconclusive)");
		CHECK(r == EMFILE || r == ENFILE,
		    "write under EMFILE returned %d (%s), want EMFILE",
		    r, db_strerror(r));
	} else
		CHECK(r == 0, "control write failed: %s", db_strerror(r));

	/* Probe: is the environment panicked? */
	{
		DBT pk, pv;
		memset(&pk, 0, sizeof(pk));
		memset(&pv, 0, sizeof(pv));
		pk.data = "k000000";
		pk.size = 7;
		pv.flags = DB_DBT_MALLOC;
		r = d->get(d, NULL, &pk, &pv, 0);
		panicked = (r == DB_RUNRECOVERY);
		if (r == 0)
			free(pv.data);
	}
	CHECK(!panicked, "environment was panicked by a transient EMFILE");

	(void)d->close(d, 0);
	(void)e->close(e, 0);
}

/*
 * region_run -- open a fresh environment while a region-file (__db.NNN) open
 * fails with EMFILE after the first region exists.  This exercises the
 * sibling site to __log_name: __env_attach_regions used to call __env_panic
 * on any post-REGION_CREATE error (abort, in diagnostic builds) rather than
 * returning a transient resource error.  The open must return EMFILE and must
 * not abort.
 */
static void
region_run(const char *home)
{
	DB_ENV *e;
	int r;

	if ((r = db_env_create(&e, 0)) != 0) {
		CHECK(0, "db_env_create: %s", db_strerror(r));
		return;
	}
	e->set_errpfx(e, "log_emfile_nopanic");
	region_seen = 0;
	injected = 0;
	inject_region = 2;	/* fail region opens after the first two */
	r = e->open(e, home, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN, 0644);
	inject_region = 0;
	/*
	 * If injection never fired (fewer region opens than expected on this
	 * platform) the arm is inconclusive, not a failure -- but it must
	 * never have aborted, which reaching this line proves.
	 */
	if (injected > 0)
		CHECK(r == EMFILE || r == ENFILE,
		    "env open under region EMFILE returned %d (%s), want EMFILE",
		    r, db_strerror(r));
	if (r == 0)
		(void)e->close(e, 0);
}

int
main(int argc, char *argv[])
{
	char home[] = "/tmp/LOG_EMFILE_NOPANIC.XXXXXX";
	int total;

	(void)argc;
	(void)argv;

	printf("log_emfile_nopanic: %s\n", db_version(NULL, NULL, NULL));

	(void)db_env_set_func_open(my_open);

	total = 0;

	/* Control: no injection, writes and reads succeed. */
	if (mkdtemp(home) == NULL)
		return (printf("mkdtemp: control\n"), 99);
	printf(" control (no injection):\n");
	run(home, 0);
	total += fails;

	/* The defect: EMFILE on a log open must not panic the environment. */
	strcpy(home, "/tmp/LOG_EMFILE_NOPANIC.XXXXXX");
	if (mkdtemp(home) == NULL)
		return (printf("mkdtemp: inject\n"), 99);
	printf(" EMFILE injected on log open:\n");
	run(home, 1);
	total += fails;

	/*
	 * The sibling site: EMFILE on a region-file open during env create
	 * must not panic/abort either -- the half-created env is removed and
	 * the error is returned.
	 */
	fails = 0;
	strcpy(home, "/tmp/LOG_EMFILE_NOPANIC.XXXXXX");
	if (mkdtemp(home) == NULL)
		return (printf("mkdtemp: region\n"), 99);
	printf(" EMFILE injected on region open at env create:\n");
	region_run(home);
	total += fails;

	printf("log_emfile_nopanic: %d failure%s\n",
	    total, total == 1 ? "" : "s");
	return (total == 0 ? 0 : 1);
}

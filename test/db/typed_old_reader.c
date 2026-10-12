/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_old_reader.c --
 *	RFC 0014 commit 4 test (spec §7.2 typed_old_reader): a reader that
 *	does not know BTM_TYPED / DB_HASH_TYPED must refuse a typed
 *	subdatabase, read-only or not, with BDB0055 (illegal meta flag).
 *	Built against WHATEVER libdb the runner points it at: in the db tier
 *	that is the DEFAULT build, which must refuse; the typed tier uses the
 *	regen companion (run_typed_old_reader.sh) instead.
 *
 * Usage: typed_old_reader <homedir> <file>
 *	opens <file>/typedsub RDONLY then read-write; both must fail EINVAL,
 *	and the error text must contain BDB0055.  One VERDICT line.
 */
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

static char errbuf[4096];
static int errpos;

static void
errcb(const DB_ENV *dbenv, const char *pfx, const char *msg)
{
	(void)dbenv; (void)pfx;
	if (msg != NULL && errpos < (int)sizeof(errbuf) - 2) {
		(void)snprintf(errbuf + errpos, sizeof(errbuf) - errpos,
		    "%s\n", msg);
		errpos += (int)strlen(msg) + 1;
	}
}

static int nfail, ncase;

static void
try_open(DB_ENV *dbenv, const char *file, int rdonly)
{
	DB *dbp;
	int ret;
	const char *mode = rdonly ? "RDONLY" : "RW";

	ncase++;
	errbuf[0] = '\0'; errpos = 0;
	if (db_create(&dbp, dbenv, 0) != 0) {
		printf("  FAIL [%d]: %s db_create\n", ncase, mode);
		nfail++;
		return;
	}
	ret = dbp->open(dbp, NULL, file, "typedsub", DB_UNKNOWN,
	    rdonly ? DB_RDONLY : 0, 0);
	(void)dbp->close(dbp, 0);
#ifdef HAVE_TYPED
	/*
	 * Linked against a typed build: it KNOWS BTM_TYPED, so a schema-less
	 * open of a typed subdatabase succeeds in commit 4 (the read-only rule
	 * is commit 5).  The point of this tier is the DEFAULT build's
	 * refusal; here the only wrong outcome is a BDB0055 refusal, which
	 * would mean the typed build does not recognise its own bit.
	 */
	if (ret == 0)
		printf("  ok   [%d]: %s opened (typed build knows the bit)\n",
		    ncase, mode);
	else {
		printf("  FAIL [%d]: %s typed build refused ret=%d (%s) <%s>\n",
		    ncase, mode, ret, db_strerror(ret), errbuf);
		nfail++;
	}
#else
	if (ret == EINVAL && strstr(errbuf, "BDB0055") != NULL)
		printf("  ok   [%d]: %s refused EINVAL/BDB0055\n", ncase, mode);
	else {
		printf("  FAIL [%d]: %s ret=%d (%s) err=<%s>\n", ncase, mode,
		    ret, db_strerror(ret), errbuf);
		nfail++;
	}
#endif
}

int
main(int argc, char *argv[])
{
	DB_ENV *dbenv;
	const char *home, *file;

	if (argc < 3) {
		printf("VERDICT typed_old_reader FAIL usage\n");
		return (1);
	}
	home = argv[1];
	file = argv[2];

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, home, DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("VERDICT typed_old_reader FAIL env setup\n");
		return (1);
	}
	dbenv->set_errcall(dbenv, errcb);

	try_open(dbenv, file, 1);	/* RDONLY */
	try_open(dbenv, file, 0);	/* read-write */

	(void)dbenv->close(dbenv, 0);

	if (nfail == 0)
		printf("VERDICT typed_old_reader PASS %s %d/%d as expected\n",
		    file, ncase, ncase);
	else
		printf("VERDICT typed_old_reader FAIL %s %d of %d wrong\n",
		    file, nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

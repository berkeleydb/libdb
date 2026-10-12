/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_optread.c --
 *	RFC 0014 commit 8 test (spec §7.2 typed_optread): the DB_OPTREAD gate
 *	(§6).  When DB_OPTREAD is on, a non-MEMCMP typed Btree key whose record
 *	or field codecs lack DB_CODEC_TOTAL is refused at open with EINVAL and a
 *	message naming the codec.
 *
 *	Each case runs in its OWN forked child, because __bam_opt_enabled caches
 *	the getenv result per process; the child sets the environment before any
 *	libdb call.  Cases:
 *	  1. DB_OPTREAD=1, key field codec without TOTAL      -> EINVAL, names codec
 *	  2. DB_OPTREAD=1, record codec without TOTAL         -> EINVAL, names codec
 *	  3. DB_OPTREAD=1, all codecs TOTAL                    -> opens
 *	  4. DB_OPTREAD=1, MEMCMP key without TOTAL            -> opens (not gated)
 *	  5. DB_NO_OPTREAD=1 DB_OPTREAD=1 (off), no TOTAL      -> opens
 *	  6. DB_OPTREAD=1, Hash schema without TOTAL           -> opens (not gated)
 *	One VERDICT line in the parent.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "db.h"

static int
field1(const DB_RECORD_CODEC *rc, const DBT *rec, u_int32_t i,
    const u_int8_t **pp, u_int32_t *lenp, int *isnull)
{
	(void)rc;
	if (i != 0) return (DB_CODEC_MALFORMED);
	*pp = rec->data; *lenp = rec->size; *isnull = 0;
	return (0);
}
static int
anycmp(const DB_CODEC *c, const u_int8_t *a, u_int32_t al,
    const u_int8_t *b, u_int32_t bl)
{
	u_int32_t m = al < bl ? al : bl;
	int r;
	(void)c;
	if (m && (r = memcmp(a, b, m)) != 0) return (r);
	return (al < bl ? -1 : al > bl ? 1 : 0);
}
static u_int32_t
anyhash(const DB_CODEC *c, const u_int8_t *p, u_int32_t len)
{
	u_int32_t h = 2166136261u, i;
	(void)c;
	for (i = 0; i < len; i++) { h ^= p[i]; h *= 16777619u; }
	return (h);
}
/* Golden vectors in ascending memcmp order (anycmp is bytewise). */
static const unsigned char ov0[1] = { 0x01 };
static const unsigned char ov1[1] = { 0x02 };
static DBT ovec[2];

/*
 * Build a schema for the given case into *sp (and the codec/rc statics it
 * points at).  Returns the expected: want_open (1 open, 0 refuse) and, for a
 * refusal, the codec name that must appear in the error.
 */
static DB_CODEC fc;
static DB_RECORD_CODEC rc;
static const DB_CODEC *types1[1];

/* case index -> (type, key flags, rc flags, want_open, named-codec) */
static int
one_case(int cs, const char **named, int *want_open)
{
	u_int32_t cflags = 0, rcflags = 0;
	DBTYPE t = DB_BTREE;

	*named = NULL; *want_open = 0;
	switch (cs) {
	case 1:	/* key field codec not TOTAL */
		cflags = 0; rcflags = DB_CODEC_TOTAL;
		*named = "fnt"; *want_open = 0; break;
	case 2:	/* record codec not TOTAL */
		cflags = DB_CODEC_TOTAL; rcflags = 0;
		*named = "rnt"; *want_open = 0; break;
	case 3:	/* everything TOTAL */
		cflags = DB_CODEC_TOTAL; rcflags = DB_CODEC_TOTAL;
		*want_open = 1; break;
	case 4:	/* MEMCMP key, no TOTAL -> not gated, opens */
		cflags = DB_CODEC_MEMCMP | DB_CODEC_EQUALIMAGE;
		rcflags = 0; *want_open = 1; break;
	case 5:	/* OPTREAD off via DB_NO_OPTREAD, no TOTAL -> opens */
		cflags = 0; rcflags = 0; *want_open = 1; break;
	case 6:	/* Hash, no TOTAL -> not gated, opens */
		t = DB_HASH; cflags = 0; rcflags = 0; *want_open = 1; break;
	}
	memset(&fc, 0, sizeof(fc));
	fc.name = (cs == 1) ? "fnt" : "fok";
	fc.version = 1; fc.flags = cflags; fc.compare = anycmp;
	ovec[0].data = (void *)ov0; ovec[0].size = 1;
	ovec[1].data = (void *)ov1; ovec[1].size = 1;
	if ((cflags & DB_CODEC_MEMCMP) != 0) {
		/* MEMCMP codec needs no hash or vectors. */
		fc.hash = NULL; fc.vectors = NULL; fc.nvectors = 0;
	} else {
		fc.hash = anyhash; fc.vectors = ovec; fc.nvectors = 2;
	}
	types1[0] = &fc;
	memset(&rc, 0, sizeof(rc));
	rc.name = (cs == 2) ? "rnt" : "rok";
	rc.version = 1; rc.flags = rcflags; rc.nfields = 1;
	rc.field = field1; rc.types = types1;
	return ((int)t);
}

/* The child: set env, open, print RESULT line, exit. */
static char g_errbuf[1024];
static void
errcb(const DB_ENV *dbenv, const char *pfx, const char *msg)
{
	(void)dbenv; (void)pfx;
	/* keep the LAST error line (the open refusal). */
	if (msg != NULL) {
		strncpy(g_errbuf, msg, sizeof(g_errbuf) - 1);
		g_errbuf[sizeof(g_errbuf) - 1] = '\0';
	}
}

static int
child(int cs)
{
	DB_ENV *dbenv;
	DB *dbp;
	DB_SCHEMA s;
	DBTYPE t;
	const char *named;
	int want_open, ret;

	/* Environment is set BEFORE any libdb call (the getenv is cached). */
	if (cs == 5) {
		setenv("DB_OPTREAD", "1", 1);
		setenv("DB_NO_OPTREAD", "1", 1);
	} else {
		setenv("DB_OPTREAD", "1", 1);
		unsetenv("DB_NO_OPTREAD");
	}

	t = (DBTYPE)one_case(cs, &named, &want_open);

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, ".", DB_CREATE | DB_INIT_MPOOL, 0) != 0) {
		printf("RESULT %d FAIL env\n", cs); return (2);
	}
	g_errbuf[0] = '\0';
	dbenv->set_errcall(dbenv, errcb);
	if (db_create(&dbp, dbenv, 0) != 0) {
		printf("RESULT %d FAIL create\n", cs); return (2); }
	memset(&s, 0, sizeof(s));
	s.type = t; s.key = &rc;
	if (db_set_schema(dbp, &s, 0) != 0) {
		printf("RESULT %d FAIL set_schema\n", cs);
		return (2);
	}
	{
		char fname[32];
		sprintf(fname, "or%d.db", cs);
		ret = dbp->open(dbp, NULL, fname, "sub", t, DB_CREATE, 0600);
	}
	if (want_open) {
		if (ret == 0) {
			printf("RESULT %d PASS opened\n", cs);
			(void)dbp->close(dbp, 0);
			ret = 0;
		} else {
			printf("RESULT %d FAIL expected-open got %d\n", cs, ret);
			ret = 1;
		}
	} else {
		if (ret == EINVAL && named != NULL &&
		    strstr(g_errbuf, named) != NULL) {
			printf("RESULT %d PASS refused-EINVAL names '%s'\n",
			    cs, named);
			ret = 0;
		} else {
			printf("RESULT %d FAIL expected-EINVAL(%s) got %d err='%s'\n",
			    cs, named ? named : "?", ret, g_errbuf);
			if (ret == 0) (void)dbp->close(dbp, 0);
			ret = 1;
		}
	}
	(void)dbenv->close(dbenv, 0);
	return (ret);
}

int
main(int argc, char *argv[])
{
	int cs, nfail = 0, ncase = 0, status;
	pid_t pid;
	const char *home = (argc > 1) ? argv[1] : ".";

	/* A single hidden sub-invocation runs one case (used by fork+exec-free
	 * fork below).  argv[2] = case number means "be the child". */
	if (argc > 2) {
		if (chdir(home) != 0) { perror("chdir"); return (2); }
		return (child(atoi(argv[2])));
	}

	for (cs = 1; cs <= 6; cs++) {
		char cwd[1024];
		char sub[1200];
		/* per-case subdir so envs do not share a file */
		sprintf(sub, "%s/c%d", home, cs);
		if (mkdir(sub, 0700) != 0 && errno != EEXIST) {
			printf("  FAIL [%d]: mkdir\n", cs); nfail++; ncase++;
			continue;
		}
		if (getcwd(cwd, sizeof(cwd)) == NULL) cwd[0] = '\0';
		pid = fork();
		if (pid < 0) { printf("  FAIL [%d]: fork\n", cs); nfail++; ncase++;
			continue; }
		if (pid == 0) {
			/* fresh process: uncached getenv.  Re-exec ourselves so
			 * the environment is read cleanly in the child. */
			char n[8];
			sprintf(n, "%d", cs);
			execl(argv[0], argv[0], sub, n, (char *)NULL);
			perror("execl"); _exit(3);
		}
		(void)waitpid(pid, &status, 0);
		ncase++;
		if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
			printf("  FAIL [%d]: child exit %d (sig %d)\n", cs,
			    WIFEXITED(status) ? WEXITSTATUS(status) : -1,
			    WIFSIGNALED(status) ? WTERMSIG(status) : 0);
			nfail++;
		} else
			printf("  ok   [%d]: case passed\n", cs);
	}

	if (nfail == 0)
		printf("VERDICT typed_optread PASS all %d cases\n", ncase);
	else
		printf("VERDICT typed_optread FAIL %d of %d cases\n",
		    nfail, ncase);
	return (nfail == 0 ? 0 : 1);
}

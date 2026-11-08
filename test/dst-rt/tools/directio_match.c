/*
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * test/dst-rt/tools/directio_match.c -- a test-only LD_PRELOAD shim that sets
 * O_DIRECT on open()/openat() ONLY for paths matching $DIRECTIO_MATCH (a
 * colon-separated list of fnmatch patterns, default "*.db").
 *
 * WHY (RFC 0024 S1 preload arm, finding F1-001).  libdirectio forces O_DIRECT
 * on EVERY regular file, including the region files (__db.00N) libdb keeps
 * buffered on purpose -- whose small management writes are not block-aligned,
 * so env-open fails EINVAL.  The log file has the SAME problem unless libdb
 * KNOWS it is direct (then DB_LOG_DIRECT aligns the header); so the shim
 * defaults to the DATA files (*.db) only, where libdb's page I/O is already
 * aligned.  To also direct the log, run the loader with --direct (native
 * DB_LOG_DIRECT) alongside the shim.  libfiu's/libdirectio's preload has no
 * path filter; this shim gives one, so the preload arm exercises the uncached
 * crash path for the data file AND the utilities (db_verify/db_recover/
 * db_backup, which never take DB_DIRECT_DB), without tripping F1-001.
 *
 * Build:  cc -shared -fPIC -D_GNU_SOURCE directio_match.c -ldl -o directio_match.so
 * Use:    LD_PRELOAD=.../directio_match.so DIRECTIO_MATCH='*.db' ./app
 */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <fnmatch.h>

static int
want_direct(const char *path)
{
	const char *pats = getenv("DIRECTIO_MATCH");
	const char *base;
	char buf[256], *tok, *save;
	if (path == NULL)
		return (0);
	if (pats == NULL || *pats == '\0')
		pats = "*.db";
	base = strrchr(path, '/');
	base = base ? base + 1 : path;
	if (strlen(pats) >= sizeof(buf))
		return (0);
	strcpy(buf, pats);
	for (tok = strtok_r(buf, ":", &save); tok != NULL;
	    tok = strtok_r(NULL, ":", &save))
		if (fnmatch(tok, base, 0) == 0)
			return (1);
	return (0);
}

int
open(const char *path, int flags, ...)
{
	static int (*real)(const char *, int, ...);
	mode_t mode = 0;
	if (real == NULL)
		real = dlsym(RTLD_NEXT, "open");
	if (flags & O_CREAT) {
		va_list ap; va_start(ap, flags);
		mode = (mode_t)va_arg(ap, int); va_end(ap);
	}
	if (want_direct(path))
		flags |= O_DIRECT;
	return (real(path, flags, mode));
}

int
openat(int dirfd, const char *path, int flags, ...)
{
	static int (*real)(int, const char *, int, ...);
	mode_t mode = 0;
	if (real == NULL)
		real = dlsym(RTLD_NEXT, "openat");
	if (flags & O_CREAT) {
		va_list ap; va_start(ap, flags);
		mode = (mode_t)va_arg(ap, int); va_end(ap);
	}
	if (want_direct(path))
		flags |= O_DIRECT;
	return (real(dirfd, path, flags, mode));
}

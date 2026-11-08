/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * handle_sizes.c -- pin the size of every public handle struct.
 *
 * Defect A1: sizeof(DB) grew 1456 -> 1744 bytes (+288) under an UNCHANGED
 * soname.  Commit f9937fab5 (cursor-shard) replaced struct __db's two cursor
 * queue heads with cq_parts[8] of {db_mutex_t + 2 heads}, and struct __dbc
 * gained a u_int32_t before links.  Nothing caught it, because the abidiff job
 * compares a PR head against the PREVIOUS RELEASE TAG -- a struct that grows
 * once, in one commit, between two releases is invisible to every later
 * comparison, and nothing compares across the whole fork.
 *
 * The decision taken (rfc/0020, review decision Q6) is option (a): a public
 * handle layout change moves the compatibility level, and so the soname
 * (libdb-MAJOR.MINOR).  The recorded sizes below therefore belong to ONE level,
 * RECORDED_MAJOR.RECORDED_MINOR.  A size change in a build still at that level
 * is an ABI break shipped under an unchanged soname, and FAILs.  The fix is to
 * bump DB_VERSION_MINOR in dist/RELEASE (ABI-BREAK-INTENDED) and record the new
 * sizes and level here in the same commit.
 *
 * Sizes are per-ABI, so they are recorded for LP64 only and the test SKIPs
 * elsewhere rather than asserting numbers it cannot know.  A 32-bit or Windows
 * ABI would need its own recorded set; see the three-gate note in
 * rfc/0010-global-invariants.md.
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

struct rec {
	const char	*name;
	size_t		 actual;
	size_t		 expect;
};

/* The compatibility level the sizes below were recorded at (rfc/0020). */
#define	RECORDED_MAJOR	2026
#define	RECORDED_MINOR	1

int
main(int argc, char **argv)
{
	struct rec r[] = {
		{ "DB",		sizeof(DB),	1744 },
		{ "DBC",	sizeof(DBC),	 552 },
		{ "DB_ENV",	sizeof(DB_ENV),	2120 },
		{ "DB_TXN",	sizeof(DB_TXN),	 336 },
		{ "DBT",	sizeof(DBT),	  40 },
		{ "DB_LSN",	sizeof(DB_LSN),	   8 },
	};
	size_t i, n = sizeof(r) / sizeof(r[0]);
	int bad = 0;

	(void)argc; (void)argv;

	/*
	 * The recorded numbers are LP64.  Do not pretend to check an ABI whose
	 * values were never measured -- that is how a gate ends up asserting a
	 * wrong constant and then being "fixed" by loosening it.
	 */
	if (sizeof(void *) != 8 || sizeof(long) != 8) {
		printf("VERDICT handle_sizes SKIP not LP64 "
		    "(sizeof(void*)=%zu sizeof(long)=%zu); sizes are "
		    "recorded for LP64 only\n",
		    sizeof(void *), sizeof(long));
		return (0);
	}

	if (DB_VERSION_MAJOR != RECORDED_MAJOR ||
	    DB_VERSION_MINOR != RECORDED_MINOR) {
		printf("VERDICT handle_sizes FAIL db.h is level %d.%d but the "
		    "sizes were recorded at %d.%d: re-record them for the new "
		    "level in this file\n", DB_VERSION_MAJOR, DB_VERSION_MINOR,
		    RECORDED_MAJOR, RECORDED_MINOR);
		return (1);
	}

	for (i = 0; i < n; i++) {
		printf("  %-8s actual=%4zu expect=%4zu %s\n",
		    r[i].name, r[i].actual, r[i].expect,
		    r[i].actual == r[i].expect ? "ok" : "CHANGED");
		if (r[i].actual != r[i].expect)
			bad++;
	}

	if (bad == 0) {
		printf("VERDICT handle_sizes PASS all %zu handle sizes "
		    "match the recorded values\n", n);
		return (0);
	}

	printf("VERDICT handle_sizes FAIL %d handle size(s) changed\n", bad);
	printf("\n");
	printf("A public handle struct changed size at compatibility level\n");
	printf("%d.%d, so the soname would not change.  That is the A1 defect.\n",
	    RECORDED_MAJOR, RECORDED_MINOR);
	printf("Bump DB_VERSION_MINOR in dist/RELEASE (ABI-BREAK-INTENDED,\n");
	printf("rfc/0020), regenerate (dist/s_config etc.), and record the new\n");
	printf("sizes and RECORDED_MINOR here in the same commit.\n");
	return (1);
}

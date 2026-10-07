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
 * The decision taken (A1 option (b)) is that handle structs are INTERNAL: they
 * are allocated inside the library (db_create does
 * __os_calloc(env, 1, sizeof(*dbp))) and db.in has zero by-value DB/DBC members
 * in any public struct, so no application allocates or embeds one and the soname
 * tracks only the exported-function ABI.  That decision is only safe if a size
 * change is DELIBERATE, which is what this asserts: the numbers below are the
 * recorded sizes, and changing one requires editing this file in the same commit
 * that changes the layout.  That puts the decision in the diff instead of in a
 * release note six weeks later.
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

int
main(int argc, char **argv)
{
	struct rec r[] = {
		{ "DB",		sizeof(DB),	1744 },
		{ "DBC",	sizeof(DBC),	 552 },
		{ "DB_ENV",	sizeof(DB_ENV),	2088 },
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
	printf("A public handle struct changed size.  This is not automatically\n");
	printf("wrong -- these structs are library-allocated and opaque (A1) --\n");
	printf("but it IS a decision, so make it explicitly:\n");
	printf("\n");
	printf("  1. Confirm nothing embeds or allocates the struct outside the\n");
	printf("     library:  grep the tree for by-value members in db.in, and\n");
	printf("     check the struct is still only reached through a pointer.\n");
	printf("  2. Confirm the region layout is unaffected, or bump\n");
	printf("     DB_REGION_MAJOR/MINOR -- __env_struct_sig() is a separate\n");
	printf("     gate from this one.\n");
	printf("  3. Update the expected value in test/db/handle_sizes.c IN THIS\n");
	printf("     COMMIT, so the size change appears in the diff that caused\n");
	printf("     it rather than in a later release note.\n");
	return (1);
}

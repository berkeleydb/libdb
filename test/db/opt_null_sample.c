/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * opt_null_sample.c -- M1 regression.
 *
 * __memp_fget_opt_valid() dereferenced sample->bhp with no NULL check, while
 * its partner __memp_fget_opt_release() has been NULL-tolerant from the start.
 * The two are documented as a pair the caller must use together, so the
 * asymmetry invited a caller to validate a sample before knowing whether its
 * fetch had succeeded -- and that faults rather than answering "not valid".
 *
 * "I hold no sample" and "my sample is stale" mean the same thing to a caller:
 * do not trust what you read.  So the predicate must answer 0, not crash.
 *
 * This does NOT need a race to run: the hazard is a missing branch, so it is
 * checkable directly.  Without the guard this file SIGSEGVs (verified: exit
 * 139); with it, exit 0.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/mp.h"

int
main(int argc, char *argv[])
{
	BH_SAMPLE s;

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);

	/*
	 * A sample as __memp_fget_opt leaves it when the fetch fails: bhp NULL,
	 * the other fields whatever they were.  They are set to non-zero values
	 * deliberately -- if the implementation ever compares those first, the
	 * mismatch must not be what saves it.
	 */
	s.bhp = NULL;
	s.pgno = 7;
	s.mf_offset = 3;
	s.gen = 2;

	if (__memp_fget_opt_valid(&s) != 0) {
		printf("VERDICT opt_null_sample FAIL __memp_fget_opt_valid "
		    "returned TRUE for a sample with bhp == NULL\n");
		return (1);
	}

	/* Idempotent, and must also tolerate the NULL. */
	if (__memp_fget_opt_valid(&s) != 0) {
		printf("VERDICT opt_null_sample FAIL not idempotent\n");
		return (1);
	}

	printf("VERDICT opt_null_sample PASS __memp_fget_opt_valid(bhp=NULL) "
	    "== 0 without faulting\n");
	return (0);
}

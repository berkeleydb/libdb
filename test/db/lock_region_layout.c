/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * A3 (REVIEW-2026-11 Sec 1.5): pin the lock-manager region layout so it stops
 * being re-broken release over release (Design Lesson 11).  See the normative
 * note test/audit/LOCK-REGION-LAYOUT-2026-11.md.
 *
 * This asserts the structural constants of the committed layout: the fixed
 * stripe count, the one-cache-line stripe padding, and the sizeof of the key
 * region structs.  A field added to DB_LOCKREGION, a stripe-count edit, or a
 * pad miscomputation changes one of these and fails the build in the commit
 * that causes it.  A deliberate region change (carrying REGION-SIG-CHANGE-
 * INTENDED) updates the recorded values in the same commit.
 *
 * The region signature itself is platform-specific; the runner
 * (run_lock_region_layout.sh) checks it against the recorded LP64 x86_64 value
 * and SKIPs elsewhere.  This C program pins only the CPU-/endian-independent
 * structural facts, so it is meaningful on every platform.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/lock.h"

#include <stdio.h>
#include <stdlib.h>

/*
 * The committed layout (LP64).  These are the numbers in
 * test/audit/LOCK-REGION-LAYOUT-2026-11.md.  On a deliberate region change,
 * bump these and the note together, in the REGION-SIG-CHANGE-INTENDED commit.
 */
#define	EXPECT_STRIPES		64
#define	EXPECT_CACHE_LINE	64
#define	EXPECT_SZ_LOCKERSTRIPE	64
#if defined(__LP64__) || defined(_LP64)
#define	EXPECT_SZ_LOCKREGION	5064
#define	EXPECT_SZ_LOCKPART	88
#define	EXPECT_SZ_DB_LOCK	80
#define	EXPECT_SZ_LOCKOBJ	136
#define	EXPECT_SZ_LOCKER	176
#define	HAVE_SIZE_EXPECTATIONS	1
#else
/* ILP32 and other models have their own sizes; pin only the invariants. */
#define	HAVE_SIZE_EXPECTATIONS	0
#endif

static int failures = 0;

#define	EQ(label, got, want) do {					\
	unsigned long __g = (unsigned long)(got);			\
	unsigned long __w = (unsigned long)(want);			\
	printf("  %-24s %lu", label, __g);				\
	if (__g != __w) {						\
		printf("  FAIL (expected %lu)", __w);			\
		failures++;						\
	}								\
	printf("\n");							\
} while (0)

int
main(int argc, char *argv[])
{
	(void)argc; (void)argv;

	EQ("LOCK_LOCKER_STRIPES",	LOCK_LOCKER_STRIPES,	EXPECT_STRIPES);
	EQ("DB_CACHE_LINE_BYTES",	DB_CACHE_LINE_BYTES,	EXPECT_CACHE_LINE);
	EQ("sizeof(DB_LOCKERSTRIPE)",	sizeof(DB_LOCKERSTRIPE),
	    EXPECT_SZ_LOCKERSTRIPE);

	/* The one-cache-line invariant, checked at runtime too (covers a
	 * non-DIAGNOSTIC build where the build-time bitfield assert still
	 * holds, but belt-and-suspenders against a future refactor). */
	if (sizeof(DB_LOCKERSTRIPE) != DB_CACHE_LINE_BYTES) {
		printf("  FAIL: DB_LOCKERSTRIPE is not exactly one cache line\n");
		failures++;
	}

#if HAVE_SIZE_EXPECTATIONS
	EQ("sizeof(DB_LOCKREGION)",	sizeof(DB_LOCKREGION),	EXPECT_SZ_LOCKREGION);
	EQ("sizeof(DB_LOCKPART)",	sizeof(DB_LOCKPART),	EXPECT_SZ_LOCKPART);
	EQ("sizeof(struct __db_lock)",	sizeof(struct __db_lock), EXPECT_SZ_DB_LOCK);
	EQ("sizeof(DB_LOCKOBJ)",	sizeof(DB_LOCKOBJ),	EXPECT_SZ_LOCKOBJ);
	EQ("sizeof(DB_LOCKER)",		sizeof(DB_LOCKER),	EXPECT_SZ_LOCKER);
#else
	printf("  (non-LP64: sizeof expectations skipped; invariants still "
	    "checked)\n");
	printf("  sizeof(DB_LOCKREGION)    %lu\n",
	    (unsigned long)sizeof(DB_LOCKREGION));
#endif

	printf("VERDICT lock_region_layout %s\n", failures == 0 ? "PASS" : "FAIL");
	printf("lock_region_layout: %s\n", failures == 0 ? "PASS" : "FAIL");
	return (failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

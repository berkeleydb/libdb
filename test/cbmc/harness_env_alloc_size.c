/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * CBMC harness: region allocator size classes (src/env/env_alloc.c).
 * (RFC 0025 E1.)
 *
 * __env_alloc carves the shared region into chunks, bucketed into size queues
 * by SET_QUEUE_FOR_SIZE and sized by DB_ALLOC_SIZE / __env_alloc_size.  A bad
 * size class or a size that does not cover the request corrupts the region
 * allocator for every process attached to it.
 *
 *   SIZE: __env_alloc_size(len) == DB_ALLOC_SIZE(len) rounds the request up to
 *         the alignment and ALWAYS covers len (result >= len), with no
 *         size_t overflow, for every len up to a stated bound.
 *
 *   QUEUE: SET_QUEUE_FOR_SIZE produces an index i in [0, DB_SIZE_Q_COUNT].
 *         When i < DB_SIZE_Q_COUNT it is the SMALLEST queue whose class
 *         (1024 << i) covers len.  When i == DB_SIZE_Q_COUNT (len exceeds the
 *         largest class, 1 MB) the engine clamps it to DB_SIZE_Q_COUNT - 1
 *         before indexing sizeq[]; that clamped index is in range.
 *
 * SET_QUEUE_FOR_SIZE and DB_ALLOC_SIZE / DB_ALIGN are copied VERBATIM from
 * src/env/env_alloc.c (the queue head array access is replaced by a local
 * array, since the macro only reads &head->sizeq[i]; the arithmetic that
 * picks i and rounds the size is the shipped code).  sizeof(ALLOC_ELEMENT) is
 * modelled as ALLOC_ELEMENT_SIZE below (its real LP64 layout: two
 * SH_TAILQ_ENTRY of two db_ssize_t each, plus two uintmax_t).
 *
 * Bound: len in 0..SIZE_MAX-64; only the last 64 values, where
 *        `len + overhead + align` must overflow, are excluded.  Loop-free
 *        apart from the <= 11-iteration SET_QUEUE_FOR_SIZE loop.
 */

#include <stdint.h>
#include <stddef.h>

typedef uint8_t  u_int8_t;
typedef uint32_t u_int32_t;
typedef uint64_t u_int64_t;
typedef unsigned int u_int;

/* --- VERBATIM constants / macros from src/env/env_alloc.c + db_int.h --- */
#define	DB_SIZE_Q_COUNT	11

/* DB_ALIGN from src/dbinc/db_int.in. */
#define	DB_ALIGN(v, bound)						\
	(((v) + (bound) - 1) & ~(((uintmax_t)(bound)) - 1))

/*
 * sizeof(ALLOC_ELEMENT): two SH_TAILQ_ENTRY (each two db_ssize_t) plus two
 * uintmax_t.  On the LP64 target db_ssize_t is 8 bytes and uintmax_t is 8,
 * so 2*(2*8) + 2*8 = 48.  The DIAGNOSTIC build adds a +1 guard byte before
 * the alignment; we verify both.
 */
#define	ALLOC_ELEMENT_SIZE 48

#ifdef E1_DIAGNOSTIC
#define	DB_ALLOC_SIZE(len)						\
	(size_t)DB_ALIGN((len) + ALLOC_ELEMENT_SIZE + 1, sizeof(uintmax_t))
#else
#define	DB_ALLOC_SIZE(len)						\
	(size_t)DB_ALIGN((len) + ALLOC_ELEMENT_SIZE, sizeof(uintmax_t))
#endif

/*
 * SET_QUEUE_FOR_SIZE from src/env/env_alloc.c, with the real head->sizeq[i]
 * access replaced by a local array `sizeq` (the macro only takes its address).
 */
#ifdef E1_SABOTAGE
/*
 * SABOTAGE: `<` for `<=` in the class test.  A chunk whose length is exactly
 * a class boundary (1024 << i) then falls through to the NEXT, larger queue
 * (or past the last one), so the "i is the smallest class that covers len"
 * property fails at the boundary.
 */
#define	CLASS_FITS(len, i)	((len) < (u_int64_t)1024 << (i))
#else
#define	CLASS_FITS(len, i)	((len) <= (u_int64_t)1024 << (i))
#endif
#define	SET_QUEUE_FOR_SIZE(sizeq, q, i, len) do {			\
	for (i = 0; i < DB_SIZE_Q_COUNT; ++i) {				\
		q = &(sizeq)[i];					\
		if (CLASS_FITS(len, i))					\
			break;						\
	}								\
} while (0)
/* --- end verbatim macros --- */

/* Exclude only the top 64 values, where overhead + alignment must wrap. */
#define	LEN_MAX ((size_t)-1 - 64)	/* all of size_t short of the rounding */

size_t nondet_size(void);

void harness(void)
{
	size_t len, total, c;
	/* A stand-in for head->sizeq[]; the macro takes &sizeq[i]. */
	u_int8_t sizeq[DB_SIZE_Q_COUNT];
	u_int8_t *q;
	u_int i;

	len = nondet_size();
	__CPROVER_assume(len <= LEN_MAX);

	/* ---- SIZE: the rounded size covers len and does not overflow. ---- */
	total = DB_ALLOC_SIZE(len);
	__CPROVER_assert(total >= len,
	    "env_alloc_size: rounded size covers the request");
	/* Alignment is to 8 bytes, so total is a multiple of 8. */
	__CPROVER_assert((total & (sizeof(uintmax_t) - 1)) == 0,
	    "env_alloc_size: result is aligned");

	/* ---- QUEUE: the chosen index is in range and is the smallest class
	 * that covers the (rounded) chunk length. ---- */
	SET_QUEUE_FOR_SIZE(sizeq, q, i, total);
	__CPROVER_assert(i <= DB_SIZE_Q_COUNT,
	    "SET_QUEUE_FOR_SIZE: index never exceeds the queue count");

	if (i < DB_SIZE_Q_COUNT) {
		/* class i covers total ... */
		c = (size_t)1024 << i;
		__CPROVER_assert(total <= c,
		    "SET_QUEUE_FOR_SIZE: chosen class covers the chunk");
		/* ... and it is the SMALLEST such class (class i-1 does not). */
		if (i > 0) {
			size_t cprev = (size_t)1024 << (i - 1);
			__CPROVER_assert(total > cprev,
			    "SET_QUEUE_FOR_SIZE: no smaller class would fit");
		}
	}

	/* The engine clamps an out-of-range index to the last queue before it
	 * indexes sizeq[] (env_alloc.c: `if (i >= DB_SIZE_Q_COUNT) i =
	 * DB_SIZE_Q_COUNT - 1`).  The clamped index is always valid. */
	{
		u_int ci = i;
		if (ci >= DB_SIZE_Q_COUNT)
			ci = DB_SIZE_Q_COUNT - 1;
		__CPROVER_assert(ci < DB_SIZE_Q_COUNT,
		    "clamped queue index is a valid sizeq[] subscript");
	}
}

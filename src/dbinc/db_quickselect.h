/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 */

/*
 * db_quickselect -- place a set of order statistics without a full sort.
 *
 * The percentile reporters in this tree (log_handoff_trace.c and the
 * test/bench drivers) copy a latency array, qsort it, then read a handful of
 * fixed positions: p50, p90, p99, p999 and the extremes.  A full O(n log n)
 * sort is wasteful when only a few order statistics are wanted.  This helper
 * uses quickselect (Hoare partition) to place exactly the requested positions,
 * each in O(n) expected time, and leaves base[ks[i]] holding the SAME element
 * a full qsort would have put there -- so a caller can swap qsort+index reads
 * for this call and get byte-identical output.
 *
 * It is header-only and self-contained (no libdb dependency) so the engine's
 * diagnostic trace file and the standalone bench programs can all include it.
 *
 * Contract:
 *   base, nmemb, size, compar  -- as for qsort(3).
 *   ks[]   -- indices (0..nmemb-1) whose order statistic the caller wants,
 *             sorted strictly ascending.
 *   nk     -- number of entries in ks[].
 * On return, for every i, base[ks[i]] is the element that would occupy
 * position ks[i] if base were fully sorted by compar.  Other positions are
 * partitioned but otherwise unspecified, exactly like after partial selection.
 *
 * Equivalence to qsort at the requested positions is proved by
 * test/c/quickselect_equiv.c against random, sorted, reverse-sorted, all-equal
 * and duplicate-heavy inputs.
 */
#ifndef	_DB_QUICKSELECT_H_
#define	_DB_QUICKSELECT_H_

#include <stddef.h>

static void
db_qs_swap(unsigned char *a, unsigned char *b, size_t size)
{
	unsigned char t;

	if (a == b)
		return;
	while (size-- > 0) {
		t = *a;
		*a = *b;
		*b = t;
		a++;
		b++;
	}
}

/*
 * Partition base[lo..hi] (inclusive) around a median-of-three pivot and place
 * the target position k.  Returns the final resting index of the pivot.
 */
static size_t
db_qs_partition(unsigned char *base, size_t lo, size_t hi, size_t size,
    int (*compar)(const void *, const void *))
{
	size_t mid, i, j, p;
	unsigned char *pv;

	mid = lo + (hi - lo) / 2;
	/* Median of three: order lo, mid, hi, then use mid as pivot. */
	if (compar(base + mid * size, base + lo * size) < 0)
		db_qs_swap(base + mid * size, base + lo * size, size);
	if (compar(base + hi * size, base + lo * size) < 0)
		db_qs_swap(base + hi * size, base + lo * size, size);
	if (compar(base + hi * size, base + mid * size) < 0)
		db_qs_swap(base + hi * size, base + mid * size, size);
	/* Move pivot (mid) to hi-1; lo and hi are now sentinels. */
	p = hi - 1;
	db_qs_swap(base + mid * size, base + p * size, size);
	pv = base + p * size;

	i = lo;
	j = p;
	for (;;) {
		do { i++; } while (compar(base + i * size, pv) < 0);
		do { j--; } while (compar(pv, base + j * size) < 0);
		if (i >= j)
			break;
		db_qs_swap(base + i * size, base + j * size, size);
	}
	db_qs_swap(base + i * size, base + p * size, size);
	return (i);
}

/*
 * Select the single order statistic k within base[lo..hi].
 */
static void
db_qs_select1(unsigned char *base, size_t lo, size_t hi, size_t k, size_t size,
    int (*compar)(const void *, const void *))
{
	size_t p;

	while (lo < hi) {
		/* Small ranges: insertion sort is exact and avoids pivot edge
		 * cases (needs lo, hi-1, hi distinct for median-of-three). */
		if (hi - lo < 3) {
			size_t a, b;
			for (a = lo + 1; a <= hi; a++)
				for (b = a; b > lo && compar(base + b * size,
				    base + (b - 1) * size) < 0; b--)
					db_qs_swap(base + b * size,
					    base + (b - 1) * size, size);
			return;
		}
		p = db_qs_partition(base, lo, hi, size, compar);
		if (p == k)
			return;
		if (k < p)
			hi = p - 1;
		else
			lo = p + 1;
	}
}

/*
 * Place every requested order statistic.  ks[] must be strictly ascending.
 * Once position ks[i] is placed, everything at indices < ks[i] is <= it, so
 * the search for the next (larger) target can start just past ks[i].
 */
static void
db_quickselect(void *base, size_t nmemb, size_t size,
    int (*compar)(const void *, const void *), const size_t *ks, size_t nk)
{
	unsigned char *b = (unsigned char *)base;
	size_t i, lo;

	if (nmemb == 0 || nk == 0)
		return;
	lo = 0;
	for (i = 0; i < nk; i++) {
		if (ks[i] >= nmemb)
			continue;
		db_qs_select1(b, lo, nmemb - 1, ks[i], size, compar);
		lo = ks[i];	/* next target is >= this one */
	}
}

#endif /* !_DB_QUICKSELECT_H_ */

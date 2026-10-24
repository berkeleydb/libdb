/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 */

/*
 * quickselect_equiv.c --
 *	Prove db_quickselect() (src/dbinc/db_quickselect.h) returns, at every
 *	requested position, the SAME element a full qsort would, across the
 *	input shapes that break naive selection: random, already sorted,
 *	reverse sorted, all-equal, and duplicate-heavy.  This is the
 *	equivalence guarantee the percentile reporters rely on when they swap
 *	qsort+index for db_quickselect.
 *
 *	Self-contained: no libdb dependency.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db_quickselect.h"

static int
cmp_ul(const void *a, const void *b)
{
	unsigned long x = *(const unsigned long *)a;
	unsigned long y = *(const unsigned long *)b;

	return (x < y ? -1 : x > y ? 1 : 0);
}

static int
cmp_dbl(const void *a, const void *b)
{
	double x = *(const double *)a, y = *(const double *)b;

	return (x < y ? -1 : x > y ? 1 : 0);
}

/* The percentile positions the reporters actually read. */
static void
fill_ks(size_t n, size_t *ks, size_t *nk)
{
	size_t k = 0;

	ks[k++] = 0;			/* min */
	ks[k++] = n * 50 / 100;		/* p50 */
	ks[k++] = n * 90 / 100;		/* p90 */
	ks[k++] = n * 99 / 100;		/* p99 */
	ks[k++] = (size_t)((unsigned long)n * 999 / 1000); /* p999 */
	ks[k++] = n - 1;		/* max */
	*nk = k;
	/* ks must be ascending & de-duplicated for db_quickselect's contract. */
	{
		size_t i, j;
		for (i = 1; i < *nk; i++)
			for (j = i; j > 0 && ks[j] < ks[j - 1]; j--) {
				size_t t = ks[j];
				ks[j] = ks[j - 1];
				ks[j - 1] = t;
			}
		for (i = 1, j = 1; i < *nk; i++)
			if (ks[i] != ks[j - 1])
				ks[j++] = ks[i];
		*nk = j;
	}
}

static int failures;

static void
check_ul(const char *shape, unsigned long *data, size_t n)
{
	unsigned long *a, *b;
	size_t ks[8], nk, i;

	if (n == 0)
		return;
	a = malloc(n * sizeof(*a));
	b = malloc(n * sizeof(*b));
	memcpy(a, data, n * sizeof(*a));
	memcpy(b, data, n * sizeof(*b));

	qsort(a, n, sizeof(*a), cmp_ul);		/* reference */
	fill_ks(n, ks, &nk);
	db_quickselect(b, n, sizeof(*b), cmp_ul, ks, nk);

	for (i = 0; i < nk; i++)
		if (b[ks[i]] != a[ks[i]]) {
			printf("FAIL ul/%s n=%zu k=%zu select=%lu sorted=%lu\n",
			    shape, n, ks[i], b[ks[i]], a[ks[i]]);
			failures++;
		}
	free(a);
	free(b);
}

static void
check_dbl(const char *shape, double *data, size_t n)
{
	double *a, *b;
	size_t ks[8], nk, i;

	if (n == 0)
		return;
	a = malloc(n * sizeof(*a));
	b = malloc(n * sizeof(*b));
	memcpy(a, data, n * sizeof(*a));
	memcpy(b, data, n * sizeof(*b));

	qsort(a, n, sizeof(*a), cmp_dbl);
	fill_ks(n, ks, &nk);
	db_quickselect(b, n, sizeof(*b), cmp_dbl, ks, nk);

	for (i = 0; i < nk; i++)
		if (b[ks[i]] != a[ks[i]]) {
			printf("FAIL dbl/%s n=%zu k=%zu select=%g sorted=%g\n",
			    shape, n, ks[i], b[ks[i]], a[ks[i]]);
			failures++;
		}
	free(a);
	free(b);
}

int
main(void)
{
	static const size_t sizes[] = { 1, 2, 3, 4, 5, 7, 8, 15, 16, 17, 100,
	    999, 1000, 1001, 4096, 10000 };
	size_t s, i, n;
	unsigned long *u;
	double *d;
	unsigned iter;

	srand(12345);

	for (s = 0; s < sizeof(sizes) / sizeof(sizes[0]); s++) {
		n = sizes[s];
		u = malloc(n * sizeof(*u));
		d = malloc(n * sizeof(*d));

		/* sorted */
		for (i = 0; i < n; i++) { u[i] = i; d[i] = (double)i; }
		check_ul("sorted", u, n);
		check_dbl("sorted", d, n);

		/* reverse sorted */
		for (i = 0; i < n; i++) { u[i] = n - i; d[i] = (double)(n - i); }
		check_ul("reverse", u, n);
		check_dbl("reverse", d, n);

		/* all equal */
		for (i = 0; i < n; i++) { u[i] = 42; d[i] = 42.0; }
		check_ul("allequal", u, n);
		check_dbl("allequal", d, n);

		/* duplicate-heavy: only a few distinct values */
		for (iter = 0; iter < 20; iter++) {
			for (i = 0; i < n; i++) {
				u[i] = (unsigned long)(rand() % 5);
				d[i] = (double)(rand() % 5);
			}
			check_ul("dupheavy", u, n);
			check_dbl("dupheavy", d, n);
		}

		/* random */
		for (iter = 0; iter < 50; iter++) {
			for (i = 0; i < n; i++) {
				u[i] = (unsigned long)rand();
				d[i] = (double)rand() / (RAND_MAX / 1000.0);
			}
			check_ul("random", u, n);
			check_dbl("random", d, n);
		}

		free(u);
		free(d);
	}

	if (failures != 0) {
		printf("quickselect_equiv: %d FAILURES\n", failures);
		return (1);
	}
	printf("quickselect_equiv: PASS (db_quickselect == qsort at every "
	    "percentile position, all shapes)\n");
	return (0);
}

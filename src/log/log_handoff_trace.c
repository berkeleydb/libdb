/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 Greg Burd.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/log.h"
#include "dbinc/log_handoff_trace.h"

#ifdef HAVE_HANDOFF_TRACE

/*
 * Group-commit handoff instrumentation.  See src/dbinc/log_handoff_trace.h for
 * what is measured and why.  Everything in this file is compiled out unless
 * the tree was configured --enable-handoff-trace.
 *
 * Samples are per-process, not per-region: recording into the LOG region would
 * change its layout and therefore src/env/env_sig.c's signature for every
 * build, and the measured workload (test/bench/commit_bench) is multithreaded
 * within one process, so process scope loses nothing that matters here.
 */
static struct __db_hoff_stats hoff;
static int hoff_ready;

/*
 * __db_hoff_init --
 *	Allocate the sample rings.  Idempotent; called from the first flush.
 *
 */
void
__db_hoff_init()
{
	size_t n;

	if (hoff_ready)
		return;
	n = DB_HOFF_MAX * sizeof(u_int32_t);
	hoff.rounds = malloc(n);
	hoff.wait_us = malloc(n);
	hoff.block_us = malloc(n);
	hoff.reacq_us = malloc(n);
	hoff.hold_us = malloc(n);
	hoff.fsync_us = malloc(n);
	hoff.woke = malloc(n);
	hoff.baton = malloc(n);
	if (hoff.rounds == NULL || hoff.wait_us == NULL ||
	    hoff.block_us == NULL || hoff.reacq_us == NULL ||
	    hoff.hold_us == NULL || hoff.fsync_us == NULL ||
	    hoff.woke == NULL || hoff.baton == NULL) {
		/*
		 * Do not silently degrade into a partial histogram: an
		 * instrument that quietly measures less than it claims is
		 * worse than one that is absent.
		 */
		fprintf(stderr, "handoff trace: out of memory, aborting\n");
		abort();
	}
	hoff_ready = 1;
}

/*
 * __db_hoff_wait --
 *	Record one waiter's experience.  Called by the waiter itself.
 *
 */
void
__db_hoff_wait(rounds, wait_us, block_us, reacq_us)
	u_int32_t rounds, wait_us, block_us, reacq_us;
{
	u_int32_t i;

	if (!hoff_ready)
		return;
	/*
	 * Racy increment: the caller does NOT hold a lock we can rely on, and
	 * taking one would perturb exactly the handoff being measured.  A lost
	 * sample biases nothing in particular (it is not correlated with the
	 * value being recorded) and the loss rate at these thread counts is
	 * negligible next to the effect sizes involved.  It is recorded rather
	 * than hidden: the total sample count is printed with the histogram.
	 */
	i = hoff.nwait;
	if (i >= DB_HOFF_MAX) {
		hoff.wait_over++;
		return;
	}
	hoff.nwait = i + 1;
	hoff.rounds[i] = rounds;
	hoff.wait_us[i] = wait_us;
	hoff.block_us[i] = block_us;
	hoff.reacq_us[i] = reacq_us;
}

/*
 * __db_hoff_round --
 *	Record one flush round.  Called by the leader under the region lock.
 *
 */
void
__db_hoff_round(hold_us, fsync_us, woke, baton)
	u_int32_t hold_us, fsync_us, woke, baton;
{
	u_int32_t i;

	if (!hoff_ready)
		return;
	i = hoff.nround;
	if (i >= DB_HOFF_MAX) {
		hoff.round_over++;
		return;
	}
	hoff.nround = i + 1;
	hoff.hold_us[i] = hold_us;
	hoff.fsync_us[i] = fsync_us;
	hoff.woke[i] = woke;
	hoff.baton[i] = baton;
}

static int
hoff_cmp(a, b)
	const void *a, *b;
{
	u_int32_t x = *(const u_int32_t *)a, y = *(const u_int32_t *)b;

	return (x < y ? -1 : x > y ? 1 : 0);
}

static void
hoff_pct(tag, v, n, out)
	const char *tag;
	u_int32_t *v, n;
	FILE *out;
{
	u_int32_t *c;
	double mean;
	u_int32_t i;

	if (n == 0) {
		fprintf(out, "%s n=0\n", tag);
		return;
	}
	if ((c = malloc(n * sizeof(*c))) == NULL)
		return;
	memcpy(c, v, n * sizeof(*c));
	qsort(c, n, sizeof(*c), hoff_cmp);
	for (mean = 0, i = 0; i < n; i++)
		mean += c[i];
	fprintf(out,
    "%s n=%lu mean=%.2f p50=%lu p90=%lu p99=%lu p999=%lu max=%lu\n",
	    tag, (u_long)n, mean / n, (u_long)c[n * 50 / 100],
	    (u_long)c[n * 90 / 100], (u_long)c[n * 99 / 100],
	    (u_long)c[(u_long)n * 999 / 1000], (u_long)c[n - 1]);
	free(c);
}

/*
 * __db_hoff_dump --
 *	Print the histograms.  Called from the benchmark driver, not from the
 *	library's own paths.
 *
 */
void
__db_hoff_dump(path)
	const char *path;
{
	FILE *out;
	u_int32_t i, nb, nw;
	u_int64_t sum_hold, sum_fsync;
	u_int32_t hist[12];

	out = (path == NULL || strcmp(path, "-") == 0) ?
	    stderr : fopen(path, "w");
	if (out == NULL)
		out = stderr;

	if (!hoff_ready) {
		fprintf(out, "HOFF not-initialised\n");
		goto done;
	}
	fprintf(out, "HOFF waiters=%lu wait_dropped=%lu rounds=%lu "
	    "round_dropped=%lu\n", (u_long)hoff.nwait,
	    (u_long)hoff.wait_over, (u_long)hoff.nround,
	    (u_long)hoff.round_over);

	/*
	 * THE FAIRNESS NUMBER.  rounds_waited is how many flush rounds
	 * completed between a waiter enqueueing and being released.  A FIFO
	 * queue would put essentially every waiter at 1-2; a spread here is
	 * starvation, and the p99 of this distribution is what the p99 of
	 * commit latency is made of.
	 */
	hoff_pct("HOFF rounds_waited", hoff.rounds, hoff.nwait, out);
	hoff_pct("HOFF wait_us", hoff.wait_us, hoff.nwait, out);
	hoff_pct("HOFF block_us", hoff.block_us, hoff.nwait, out);
	hoff_pct("HOFF reacq_us", hoff.reacq_us, hoff.nwait, out);
	hoff_pct("HOFF hold_us", hoff.hold_us, hoff.nround, out);
	hoff_pct("HOFF fsync_us", hoff.fsync_us, hoff.nround, out);
	hoff_pct("HOFF woke", hoff.woke, hoff.nround, out);

	/* Explicit histogram of rounds waited -- the shape, not just quantiles. */
	memset(hist, 0, sizeof(hist));
	for (i = 0; i < hoff.nwait; i++) {
		u_int32_t r = hoff.rounds[i], b = 0;
		while (r > 0 && b < 11) { r >>= 1; b++; }
		hist[b]++;
	}
	fprintf(out, "HOFF rounds_hist");
	for (i = 0; i < 12; i++)
		fprintf(out, " %lu:%lu", (u_long)i, (u_long)hist[i]);
	fprintf(out, "\n");

	/*
	 * Software handoff cost per round, measured in the leader's own
	 * thread: hold minus the fsync it contains.  This replaces subtracting
	 * a separate benchmark's device fsync number, which measures a
	 * different operation (appending to a growing file) and cannot be
	 * differenced against this one.
	 */
	for (sum_hold = sum_fsync = 0, i = 0; i < hoff.nround; i++) {
		sum_hold += hoff.hold_us[i];
		sum_fsync += hoff.fsync_us[i];
	}
	if (hoff.nround != 0)
		fprintf(out,
	    "HOFF per_round_mean_us hold=%.1f fsync=%.1f software=%.1f\n",
		    (double)sum_hold / hoff.nround,
		    (double)sum_fsync / hoff.nround,
		    (double)(sum_hold - sum_fsync) / hoff.nround);

	/* How often a round handed the DB_COMMIT_FLUSH baton onward. */
	for (nb = 0, i = 0; i < hoff.nround; i++)
		if (hoff.baton[i])
			nb++;
	/* How often a round woke nobody at all. */
	for (nw = 0, i = 0; i < hoff.nround; i++)
		if (hoff.woke[i] == 0)
			nw++;
	fprintf(out, "HOFF baton_rounds=%lu empty_rounds=%lu\n",
	    (u_long)nb, (u_long)nw);

done:
	fflush(out);
	if (out != stderr)
		(void)fclose(out);
}

#endif /* HAVE_HANDOFF_TRACE */

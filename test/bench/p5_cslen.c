/*-
 * P5: how long is the log-region critical section, and what is it made of?
 *
 * The A/B measurements say shortening the critical section does not help (an
 * 8 MB log buffer removes 170x of the writes inside it for no throughput
 * change).  Before accepting that, measure the section directly: if the hold
 * time is dominated by the memcpy then reserve-then-copy should have worked and
 * something else is wrong with the experiment; if it is dominated by fixed
 * per-acquisition overhead then reserve-then-copy cannot help and D0 is right.
 *
 * This is a standalone model, not libdb: it reproduces __log_put's critical
 * section shape (bump two counters, memcpy `reclen` bytes into a shared ring)
 * under the same BDB test-and-set mutex discipline, and times it.  Using a
 * model rather than instrumenting the library keeps the measurement out of the
 * thing being measured -- an rdtsc pair inside the real latch would itself cost
 * a shared-cacheline write.
 *
 *	cc -O2 -pthread p5_cslen.c -o p5_cslen
 *	./p5_cslen <threads> <reclen> <secs> [spins] [mode]
 *
 * mode=0 (default) copies INSIDE the latch, as libdb does today.
 * mode=1 copies OUTSIDE the latch: the latch is held only to bump the two
 *        counters (the reservation), exactly the PostgreSQL/InnoDB shape.  The
 *        A/B of these two modes is the cheapest honest estimate of the ceiling
 *        on design D1, and it costs no risk to the library to measure.
 *
 * Reports, per thread count: acquisitions/sec, mean/p50/p99 hold time in ns.
 */
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define	BUFSZ	(32 * 1000)		/* LG_BSIZE_DEFAULT */
#define	MAXT	256

static volatile int stop, go;
static int nthreads, reclen, spins, mode;

/*
 * A test-and-set latch with BDB's acquisition discipline: spin up to `spins`
 * times on a plain read before attempting the atomic, pause between attempts,
 * and -- the part that matters -- write owner identity into the same cacheline
 * on success, as mut_tas.c:205-206 does.
 */
typedef struct {
	volatile uint32_t tas;
	volatile uint32_t pid;		/* mutexp->pid  */
	volatile uint64_t tid;		/* mutexp->tid  */
} latch_t;

static latch_t latch __attribute__((aligned(64)));
static uint8_t sharedbuf[BUFSZ];
static uint32_t b_off;			/* lp->b_off  */
static uint64_t lsn;			/* lp->lsn     */

static inline uint64_t
rdtsc_ns(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return ((uint64_t)ts.tv_sec * 1000000000ull + ts.tv_nsec);
}

static void
latch_lock(uint32_t id)
{
	for (;;) {
		int n;
		for (n = spins; n > 0; --n) {
			if (latch.tas == 0 &&
			    __sync_bool_compare_and_swap(&latch.tas, 0, 1)) {
				latch.pid = id;	  /* shared-line write, as BDB */
				latch.tid = id;
				return;
			}
			__asm__ __volatile__("rep; nop");
		}
		sched_yield();
	}
}

static void
latch_unlock(void)
{
	__sync_synchronize();
	latch.tas = 0;
}

typedef struct {
	uint64_t n, hold_ns, copy_ns;
	uint32_t *hist;			/* hold-time histogram, ns-resolution */
	int tid;
} targ_t;

#define	HISTN	200000

static void *
worker(void *a)
{
	targ_t *t = a;
	uint8_t *rec = malloc(reclen);

	memset(rec, 'x', reclen);
	t->hist = calloc(HISTN, sizeof(*t->hist));
	while (!go) { }
	while (!stop) {
		uint64_t t0, t1, t2;

		t0 = rdtsc_ns();
		latch_lock((uint32_t)t->tid);
		t1 = rdtsc_ns();
		if (mode == 0) {
			/* --- what __log_putr/__log_fill do under the latch --- */
			if (b_off + reclen > BUFSZ)
				b_off = 0;		  /* buffer wrap   */
			memcpy(sharedbuf + b_off, rec, reclen);	  /* :1382 */
			b_off += reclen;		  /* :1385         */
			lsn += reclen;			  /* :898          */
			t2 = rdtsc_ns();
			latch_unlock();
		} else {
			/* --- reserve only: the D1 critical section --- */
			uint32_t myoff;
			if (b_off + reclen > BUFSZ)
				b_off = 0;
			myoff = b_off;
			b_off += reclen;
			lsn += reclen;
			t2 = rdtsc_ns();
			latch_unlock();
			/* copy in parallel, into the privately reserved range */
			memcpy(sharedbuf + myoff, rec, reclen);
		}

		t->hold_ns += t2 - t1;
		t->copy_ns += t2 - t1;		/* the section IS the copy here */
		t->n++;
		if (t2 - t1 < HISTN)
			t->hist[t2 - t1]++;
		(void)t0;
	}
	free(rec);
	return (NULL);
}

int
main(int argc, char **argv)
{
	pthread_t th[MAXT];
	targ_t ta[MAXT];
	uint64_t tot = 0, hold = 0;
	uint32_t *agg;
	double t0, el;
	int i, secs;
	struct timespec ts;

	nthreads = argc > 1 ? atoi(argv[1]) : 8;
	reclen = argc > 2 ? atoi(argv[2]) : 157;  /* measured mean record size */
	secs = argc > 3 ? atoi(argv[3]) : 5;
	spins = argc > 4 ? atoi(argv[4]) : 4800;  /* 96 cpu * 50 */
	mode = argc > 5 ? atoi(argv[5]) : 0;

	if (nthreads > MAXT)
		return (2);
	memset(ta, 0, sizeof(ta));
	agg = calloc(HISTN, sizeof(*agg));

	clock_gettime(CLOCK_MONOTONIC, &ts);
	t0 = ts.tv_sec + ts.tv_nsec / 1e9;
	for (i = 0; i < nthreads; i++) {
		ta[i].tid = i + 1;
		pthread_create(&th[i], NULL, worker, &ta[i]);
	}
	go = 1;
	sleep(secs);
	stop = 1;
	for (i = 0; i < nthreads; i++)
		pthread_join(th[i], NULL);
	clock_gettime(CLOCK_MONOTONIC, &ts);
	el = (ts.tv_sec + ts.tv_nsec / 1e9) - t0;

	for (i = 0; i < nthreads; i++) {
		uint32_t j;
		tot += ta[i].n;
		hold += ta[i].hold_ns;
		for (j = 0; j < HISTN; j++)
			agg[j] += ta[i].hist[j];
	}

	/* p50 / p99 of the hold time. */
	{
		uint64_t seen = 0, p50 = 0, p99 = 0;
		uint32_t j;
		for (j = 0; j < HISTN; j++) {
			seen += agg[j];
			if (p50 == 0 && seen * 2 >= tot)
				p50 = j;
			if (p99 == 0 && seen * 100 >= tot * 99) {
				p99 = j;
				break;
			}
		}
		printf("threads=%d reclen=%d spins=%d mode=%d "
		    "acq_per_sec=%.0f mean_hold_ns=%.1f p50=%llu p99=%llu\n",
		    nthreads, reclen, spins, mode, tot / el,
		    tot == 0 ? 0.0 : (double)hold / tot,
		    (unsigned long long)p50, (unsigned long long)p99);
	}
	free(agg);
	return (0);
}

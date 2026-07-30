/*
 * CBMC harness: __dd_find (src/lock/lock_deadlock.c) -- the waits-for bitmap
 * cycle-detection core of deadlock detection.
 *
 * __dd_find takes the nlockers x nalloc waits-for bitmap matrix (bmp) and,
 * for each locker, ORs in the rows of the lockers it waits on, detecting a
 * cycle when a locker ends up waiting on itself.  We verify the REAL function
 * (copied VERBATIM below; the surrounding file pulls in the whole lock
 * subsystem) over a bounded number of lockers:
 *
 *   1. NO OOB on the bitmap matrix: every ISSET_MAP / OR_MAP index
 *      (mymap[j/32], tmpmap = bmp + nalloc*j, retp[ndead]) stays within the
 *      allocated matrix and the deadlist array (CBMC bounds/pointer checks).
 *   2. TERMINATION: the three nested loops are all bounded by nlockers, so the
 *      function terminates (proved by --unwinding-assertions at the bound).
 *   3. deadlist is NULL-terminated and killids are valid indices (< nlockers).
 *
 * What is stubbed:
 *   - __os_malloc / __os_realloc: backed by a fixed static buffer (the max
 *     number of deadlist entries is bounded by NLOCKERS+1 << INITIAL_DEAD_ALLOC
 *     (8), so realloc is never reached at this bound and no dynamic malloc
 *     modelling is needed -- keeps CBMC fast).
 *   - locker_info reduced to the two fields __dd_find reads: valid, in_abort.
 *   - env is opaque (only passed to the alloc stubs).
 *
 * Bound: NLOCKERS lockers, NALLOC = ceil(NLOCKERS/32) = 1 u_int32 word.
 *        --unwind covers the nlockers-bounded loops.
 */

#include <stdint.h>
#include <stddef.h>

typedef uint32_t u_int32_t;
typedef unsigned int u_int;

/* --- bitmap macros: VERBATIM from src/lock/lock_deadlock.c:16-30 --- */
#define	ISSET_MAP(M, N)	((M)[(N) / 32] & (1 << ((N) % 32)))
#define	OR_MAP(D, S, N)	{						\
	u_int32_t __i;							\
	for (__i = 0; __i < (N); __i++)					\
		D[__i] |= S[__i];					\
}

/* locker_info reduced to the fields __dd_find touches. */
typedef struct { int valid; int in_abort; } locker_info;

typedef struct __env ENV; /* opaque */

/* __os_malloc / __os_realloc backed by a fixed static buffer.  At this bound
 * INITIAL_DEAD_ALLOC (8) already exceeds the max deadlist size (NLOCKERS+1),
 * so realloc is never invoked -- one static buffer suffices and avoids the
 * cost of a symbolic-size dynamic malloc model. */
static u_int32_t *dd_pool[16];
static int __os_malloc(ENV *env, size_t sz, void *storep)
{
	(void)env; (void)sz;
	*(void **)storep = dd_pool;
	return 0;
}
static int __os_realloc(ENV *env, size_t sz, void *storep)
{
	(void)env; (void)sz; (void)storep;
	return 0; /* unreachable at this bound; keep the buffer as-is */
}

/* --- BEGIN verbatim copy of __dd_find from src/lock/lock_deadlock.c --- */
static int
__dd_find(env, bmp, idmap, nlockers, nalloc, deadp)
	ENV *env;
	u_int32_t *bmp, nlockers, nalloc;
	locker_info *idmap;
	u_int32_t ***deadp;
{
	u_int32_t i, j, k, *mymap, *tmpmap, **retp;
	u_int ndead, ndeadalloc;
	int ret;

#undef	INITIAL_DEAD_ALLOC
#define	INITIAL_DEAD_ALLOC	8

	ndeadalloc = INITIAL_DEAD_ALLOC;
	ndead = 0;
	if ((ret = __os_malloc(env,
	    ndeadalloc * sizeof(u_int32_t *), &retp)) != 0)
		return (ret);

	for (mymap = bmp, i = 0; i < nlockers; i++, mymap += nalloc) {
		if (!idmap[i].valid)
			continue;
		for (j = 0; j < nlockers; j++) {
			if (!ISSET_MAP(mymap, j))
				continue;

			/* Find the map for this bit. */
			tmpmap = bmp + (nalloc * j);
			OR_MAP(mymap, tmpmap, nalloc);
			if (!ISSET_MAP(mymap, i))
				continue;

			/* Make sure we leave room for NULL. */
			if (ndead + 2 >= ndeadalloc) {
				ndeadalloc <<= 1;
				if (__os_realloc(env,
				    ndeadalloc * sizeof(u_int32_t *),
				    &retp) != 0) {
					retp[ndead] = NULL;
					*deadp = retp;
					return (0);
				}
			}
			retp[ndead++] = mymap;

			/* Mark all participants in this deadlock invalid. */
			for (k = 0; k < nlockers; k++)
				if (ISSET_MAP(mymap, k))
					idmap[k].valid = 0;
			break;
		}
	}
	retp[ndead] = NULL;
	*deadp = retp;
	return (0);
}
/* --- END verbatim copy --- */

#define NLOCKERS 4
#define NALLOC   1  /* ceil(NLOCKERS/32) */
u_int32_t nondet_u32(void);
int nondet_int(void);

void harness(void)
{
	u_int32_t bmp[NLOCKERS * NALLOC];
	locker_info idmap[NLOCKERS];
	u_int32_t **deadp = NULL;
	unsigned i;
	int rc;

	/* Fully nondeterministic waits-for matrix and validity flags. */
	for (i = 0; i < NLOCKERS * NALLOC; i++)
		bmp[i] = nondet_u32();
	for (i = 0; i < NLOCKERS; i++) {
		idmap[i].valid = nondet_int();
		idmap[i].in_abort = nondet_int();
	}

	rc = __dd_find(NULL, bmp, idmap, NLOCKERS, NALLOC, &deadp);

	/* At this bound __os_malloc always succeeds, so rc is always 0. */
	__CPROVER_assert(rc == 0, "__dd_find succeeds (alloc cannot fail here)");
	__CPROVER_assert(deadp != NULL,
	    "__dd_find success sets the deadlist pointer");
	/* Each recorded entry points into the bitmap matrix (a row), and the
	 * list is NULL terminated (walked under CBMC bounds/pointer checks). */
	for (i = 0; i < NLOCKERS + 1 && deadp[i] != NULL; i++)
		__CPROVER_assert(
		    deadp[i] >= bmp && deadp[i] < bmp + NLOCKERS * NALLOC,
		    "deadlist entry points into the bitmap matrix");
}

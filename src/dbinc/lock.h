/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#ifndef	_DB_LOCK_H_
#define	_DB_LOCK_H_

#if defined(__cplusplus)
extern "C" {
#endif

#define	DB_LOCK_DEFAULT_N	1000	/* Default # of locks in region. */

/*
 * SI_CLEANUP_TRIGGER_DIV --
 *	The committed-reader SIREAD marker sweep (__lock_sicleanup) fires from
 *	txn_begin when live markers (region->nsireaders) exceed
 *	st_objects / SI_CLEANUP_TRIGGER_DIV.  This makes the marker/locker/detail
 *	footprint a bounded sawtooth whose ceiling is that fraction of the
 *	lock-object table, independent of the transaction count -- the mechanism
 *	that keeps a long-lived, no-checkpoint read-only SSI workload from
 *	exhausting the lock region (issue #137).
 *	8 was chosen over 2 empirically: it lowers the steady state ~4x and also
 *	speeds the common path (shorter sireaders lists, smaller sweeps).
 *
 *	Defined here rather than in txn.c because it is a property of the lock
 *	region, and DB_ENV->lock_stat_print reports the ceiling it implies as the
 *	operator-facing bound on the SIREAD marker population.  One definition,
 *	so the documented bound cannot drift from the trigger that enforces it.
 */
#define	SI_CLEANUP_TRIGGER_DIV	8

/*
 * Number of striped shared latches guarding the locker hash table.  A power
 * of two so the stripe index is a mask of the bucket index.  Readers (the
 * lock-get hot path) take one stripe shared; a writer takes one stripe
 * exclusive plus stripe 0, which doubles as the locker allocation latch (see
 * the LOCK_LOCKERS block below).  Sized to comfortably exceed common core
 * counts so the per-operation reader-count atomics rarely share a cache line.
 */
#define	LOCK_LOCKER_STRIPES	64
#define	LOCK_LOCKER_STRIPE(id)	((id) & (LOCK_LOCKER_STRIPES - 1))

/*
 * The locker id space is divided between the transaction manager and the lock
 * manager.  Lock IDs start at 1 and go to DB_LOCK_MAXID.  Txn IDs start at
 * DB_LOCK_MAXID + 1 and go up to TXN_MAXIMUM.
 */
#define	DB_LOCK_INVALIDID	0
#define	DB_LOCK_MAXID		0x7fffffff

/*
 * A locker's deadlock resolution priority is stored as a 32 bit unsigned
 * integer.  The maximum priority is DB_LOCK_MAXPRIORITY and the default
 * priority is DB_LOCK_DEFPRIORITY.
 */
#define	DB_LOCK_DEFPRIORITY	100
#define DB_LOCK_MAXPRIORITY	UINT32_MAX

/*
 * Out of band value for a lock.  Locks contain an offset into a lock region,
 * so we use an invalid region offset to indicate an invalid or unset lock.
 */
#define	LOCK_INVALID		INVALID_ROFF
#define	LOCK_ISSET(lock)	((lock).off != LOCK_INVALID)
#define	LOCK_INIT(lock)		((lock).off = LOCK_INVALID)

/*
 * Macro to identify a write lock for the purpose of counting locks
 * for the NUMWRITES option to deadlock detection.
 */
#define	IS_WRITELOCK(m) \
	((m) == DB_LOCK_WRITE || (m) == DB_LOCK_WWRITE || \
	    (m) == DB_LOCK_IWRITE || (m) == DB_LOCK_IWR)

/*
 * Macros to lock/unlock the lock region as a whole. Mostly used for
 * initialization.
 */
#define	LOCK_REGION_LOCK(env)						\
	MUTEX_LOCK(env, ((DB_LOCKREGION *)				\
	    (env)->lk_handle->reginfo.primary)->mtx_region)
#define	LOCK_REGION_UNLOCK(env)						\
	MUTEX_UNLOCK(env, ((DB_LOCKREGION *)				\
	    (env)->lk_handle->reginfo.primary)->mtx_region)

/*
 * Cache-line size assumed for PADDING ONLY.  64 bytes on x86-64 and arm64.
 *
 * This is a PERFORMANCE hint, never a correctness requirement: if the real
 * line size differs, the padding is merely the wrong size and nothing
 * misbehaves.  It is the first such constant in the tree, so it is named
 * rather than written inline at the one use site below.
 *
 * It exists because P12's per-stripe locker state is written by whichever core
 * owns that stripe, so two stripes sharing a line make two unrelated cores
 * contend one line -- the classic false-sharing trade where splitting one hot
 * line into N yields N contended lines instead of none.
 */
#define	DB_CACHE_LINE_BYTES	64

/*
 * DB_LOCKERSTRIPE --
 *	Per-stripe locker allocation state (P12).
 *
 * free_lockers, the in-use "lockers" (ulinks) list and the in-use count used
 * to be ONE EACH, region-wide, mutated on every locker create and every
 * locker free under mtx_locker_stripe[0].  That made stripe 0 a global
 * serialization point on the txn_begin path: measured on 64 vCPU at scale 96,
 * 100% of stripe-0 acquisitions were creates (1.563 per commit), and the wait
 * grew 5.2x from t=8 to t=64 -- the fastest-growing latch wait in the
 * subsystem.  The hot cost was the free-list POP, not the __env_alloc refill.
 *
 * Sharding them per stripe makes create and free touch only the one stripe
 * they already hold for the locker's bucket chain.  The index is
 * LOCK_LOCKER_STRIPE(bucket index) -- the SAME stripe that owns that chain --
 * so the stripe's owner owns all four pieces of state outright, and a locker
 * always returns to the stripe it came from (both directions are derived from
 * its id).
 *
 * nlockers/maxnlockers are summed on demand (see __lock_nlockers): they are
 * read only for statistics and the st_maxnlockers watermark.
 *
 * PADDED to a full cache line so adjacent stripes do not share one.  At its
 * natural 40 bytes, 1.6 stripes fell in each 64-byte line, so cores working
 * neighbouring stripes wrote the same line.
 */
/*
 * The UNPADDED field list, declared separately only so its size can be
 * measured to compute DB_LOCKERSTRIPE's pad. It must stay field-for-field
 * identical to the head of DB_LOCKERSTRIPE; the size check below fails the
 * build if the padded struct is not exactly one cache line, which is what
 * a divergence between the two would cause. Never instantiated.
 */
struct __db_lockerstripe_fields {
	SH_TAILQ_HEAD(__flocker_f) free_lockers;
	SH_TAILQ_HEAD(__lkrs_f) lockers;
	u_int32_t	nlockers;
	u_int32_t	maxnlockers;
};
typedef struct __db_lockerstripe { /* SHARED */
	SH_TAILQ_HEAD(__flocker) free_lockers;	/* Free locker list. */
	SH_TAILQ_HEAD(__lkrs) lockers;	/* In-use list (ulinks). */
	u_int32_t	nlockers;	/* Lockers in use on this stripe. */
	u_int32_t	maxnlockers;	/* Watermark of the above. */
	/*
	 * Explicit tail padding rather than __attribute__((aligned)): this
	 * struct lives in a shared region whose layout must be identical for
	 * every attaching process and is hashed into the build signature, so
	 * its size must be a plain function of the declaration, not of
	 * compiler attribute support.
	 *
	 * The pad is COMPUTED from the fields, not hand-written. A first
	 * version wrote pad[24] on the stated assumption that the fields are
	 * "40 bytes on every supported platform". They are not: SH_TAILQ_HEAD
	 * is two db_ssize_t, which is 4 bytes on ILP32, so the fields are 24
	 * bytes there, the struct was 48, and the size check below broke the
	 * 32-bit build (CI "ubuntu 32-bit", which is continue-on-error and so
	 * did not fail the workflow). The assertion did its job; the pad was
	 * wrong. DB_LOCKERSTRIPE_FIELDS is the field list, used once for the
	 * real struct and once to measure it.
	 */
	u_int8_t	pad[DB_CACHE_LINE_BYTES -
	    sizeof(struct __db_lockerstripe_fields)];
} DB_LOCKERSTRIPE;

/*
 * Fail the build if DB_LOCKERSTRIPE stops being exactly one cache line.  The
 * padding above is hand-computed, so without this an added field would halve
 * the padding's effect and the only symptom would be a performance
 * regression -- the hardest kind to attribute.
 *
 * A negative-width bitfield rather than _Static_assert: this header predates
 * C11 by convention and the tree has no static-assert precedent.  It must be
 * instantiated to fire, so __lock_region_init declares one (a STRUCT MEMBER is
 * required -- the first attempt used a negative array bound in a function
 * PROTOTYPE, which gcc accepts silently because an array parameter decays to a
 * pointer before the bound is checked, so the assertion had no teeth).
 * Verified to fail the build by growing pad[] until sizeof moved; note pad[23]
 * is NOT a valid perturbation, since 8-byte struct alignment rounds it back
 * to 64.
 */
typedef struct {
	unsigned int db_lockerstripe_is_one_cache_line :
	    ((sizeof(DB_LOCKERSTRIPE) == DB_CACHE_LINE_BYTES) ? 1 : -1);
} DB_LOCKERSTRIPE_SIZE_CHECK;

/*
 * DB_LOCKREGION --
 *	The lock shared region.
 */

typedef struct __db_lockregion { /* SHARED */
	db_mutex_t	mtx_region;	/* Region mutex. */

	u_int32_t	need_dd;	/* flag for deadlock detector */
	u_int32_t	detect;		/* run dd on every conflict */
	db_timespec	next_timeout;	/* next time to expire a lock */
	db_mutex_t	mtx_dd;		/* mutex for lock object dd list. */
	/*
	 * Striped shared latches over the locker hash table.  The lock-get hot
	 * path read-locks one stripe (chosen by the locker's BUCKET index) so
	 * the per-operation reader-count atomics spread across
	 * LOCK_LOCKER_STRIPES cache lines instead of contending on a single
	 * rwlock counter.  A locker create or free takes its one bucket stripe
	 * exclusive and nothing else -- that stripe owns both the bucket chain
	 * and (since P12) the free list, ulinks list and counters the create
	 * mutates, so there is no region-wide allocation latch on that path.
	 * Stripe 0 remains the latch for the lock_id/cur_maxid counters and the
	 * representative mutex for wait-time statistics.  Only the whole-table
	 * walks (deadlock detector, failchk, stat, free-list exhaustion) still
	 * lock all stripes exclusive.
	 */
	db_mutex_t	mtx_locker_stripe[LOCK_LOCKER_STRIPES];
	SH_TAILQ_HEAD(__dobj) dd_objs;	/* objects with waiters */
					/* free locker header */
        roff_t          locker_mem_off; /* block memory for lockers */
	DB_LOCKERSTRIPE	locker_stripe[LOCK_LOCKER_STRIPES];

	db_timeout_t	lk_timeout;	/* timeout for locks. */
	db_timeout_t	tx_timeout;	/* timeout for txns. */

	u_int32_t	locker_t_size;	/* size of locker hash table */
	u_int32_t	object_t_size;	/* size of object hash table */
	u_int32_t	part_t_size;	/* number of partitions */

	roff_t		conf_off;	/* offset of conflicts array */
	roff_t		obj_off;	/* offset of object hash table */
	roff_t		part_off;	/* offset of partition array */
	roff_t		stat_off;	/* offset to object hash stats */
	roff_t		locker_off;	/* offset of locker hash table */

	u_int32_t	lock_id;	/* Current lock(er) id to allocate. */
	u_int32_t	cur_maxid;	/* Current max lock(er) id. */
	/*
	 * P12 A/B SWITCH.  1 = per-stripe locker allocation (the fix); 0 = all
	 * locker allocation state on locker_stripe[0], guarded by stripe 0,
	 * which is the pre-P12 behaviour.  Read ONCE from the environment
	 * (DB_NO_LOCKER_SHARD) when the region is created and stored here, not
	 * consulted per call, for two reasons:
	 *
	 *  - Correctness.  Two processes sharing a region MUST agree: a locker
	 *    created on stripe 7's free list and freed by a process that thinks
	 *    every locker lives on stripe 0 would corrupt both lists.  Storing
	 *    the creator's choice in the region makes every attacher inherit it.
	 *  - Measurement.  ONE binary with a runtime switch is the only
	 *    unconfounded way to A/B this subsystem -- see the DB_PRIVATE layout
	 *    warning atop test/bench/run_bench.sh, where per-arm builds produced
	 *    a 1.65x "win" that was memory layout, not code.  Both arms here run
	 *    the same binary over the same region layout.
	 */
	u_int32_t	locker_shard;
	db_atomic_t	nsireaders;	/* SSI: approximate count of live SIREAD
					   markers held by committed readers,
					   used only to trigger best-effort GC
					   (__lock_sicleanup) before the count
					   grows unbounded between checkpoints.
					   Atomic: inc/dec happen under different
					   partition mutexes.  Not required to be
					   exact. */
	int32_t		nmodes;		/* Number of modes in conflict table. */
	DB_LOCK_STAT	stat;		/* stats about locking. */
} DB_LOCKREGION;

/*
 * Since we will store DBTs in shared memory, we need the equivalent of a
 * DBT that will work in shared memory.
 */
typedef struct __sh_dbt { /* SHARED */
	u_int32_t size;			/* Byte length. */
	roff_t    off;			/* Region offset. */
} SH_DBT;

#define	SH_DBT_PTR(p)	((void *)(((u_int8_t *)(p)) + (p)->off))

/*
 * Object structures;  these live in the object hash table.
 */
typedef struct __db_lockobj { /* SHARED */
	u_int32_t	indx;		/* Hash index of this object. */
	u_int32_t	generation;	/* Generation of this object. */
	SH_DBT	lockobj;		/* Identifies object locked. */
	SH_TAILQ_ENTRY links;		/* Links for free list or hash list. */
	SH_TAILQ_ENTRY dd_links;	/* Links for dd list. */
	SH_TAILQ_HEAD(__waitl) waiters;	/* List of waiting locks. */
	SH_TAILQ_HEAD(__holdl) holders;	/* List of held locks. */
	SH_TAILQ_HEAD(__sil) sireaders;	/* List of SSI snapshot readers. */
					/* Declare room in the object to hold
					 * typical DB lock structures so that
					 * we do not have to allocate them from
					 * shalloc at run-time. */
	u_int8_t objdata[sizeof(struct __db_ilock)];
} DB_LOCKOBJ;

/*
 * Locker structures; these live in the locker hash table.
 */
struct __db_locker { /* SHARED */
	u_int32_t id;			/* Locker id. */

	pid_t pid;			/* Process owning locker ID */
	db_threadid_t tid;		/* Thread owning locker ID */
	db_mutex_t mtx_locker;		/* Mutex to block on. */

	roff_t	td_off;			/* SSI: TXN_DETAIL offset of locker. */

	u_int32_t dd_id;		/* Deadlock detector id. */

	u_int32_t nlocks;		/* Number of locks held. */
	u_int32_t nwrites;		/* Number of write locks held. */
	u_int32_t priority;		/* Deadlock resolution priority. */
	u_int32_t nrequest;             /* number of requests. */
	
	roff_t  master_locker;		/* Locker of master transaction. */
	roff_t  parent_locker;		/* Parent of this child. */
	SH_LIST_HEAD(_child) child_locker;	/* List of descendant txns;
						   only used in a "master"
						   txn. */
	SH_LIST_ENTRY child_link;	/* Links transactions in the family;
					   elements of the child_locker
					   list. */
	SH_TAILQ_ENTRY links;		/* Links for free and hash list. */
	SH_TAILQ_ENTRY ulinks;		/* Links in-use list. */
	SH_LIST_HEAD(_held) heldby;	/* Locks held by this locker. */
	db_timespec	lk_expire;	/* When current lock expires. */
	db_timespec	tx_expire;	/* When this txn expires. */
	db_timeout_t	lk_timeout;	/* How long do we let locks live. */

#define	DB_LOCKER_DIRTY		0x0001	/* Has write locks. */
#define	DB_LOCKER_INABORT	0x0002	/* Is aborting, don't abort again. */
#define	DB_LOCKER_TIMEOUT	0x0004	/* Has timeout set. */
#define	DB_LOCKER_FAMILY_LOCKER 0x0008	/* Part of a family of lockers. */
#define	DB_LOCKER_HANDLE_LOCKER 0x0010	/* Not associated with a thread. */
#define	DB_LOCKER_FREED		0x0020	/* SSI: freed, kept for SIREAD locks. */
	u_int32_t flags;
};

/*
 * Map a hash index into a partition.
 */
#define	LOCK_PART(reg, ndx)  (ndx % (reg)->part_t_size)

/*
 * Structure that contains information about a lock table partition.
 */
typedef struct __db_lockpart{ /* SHARED */
	db_mutex_t	mtx_part;	/* mutex for partition*/
					/* free lock header */
	SH_TAILQ_HEAD(__flock) free_locks;
					/* free obj header */
	SH_TAILQ_HEAD(__fobj) free_objs;
        roff_t          lock_mem_off;   /* block memory for locks */
        roff_t          lockobj_mem_off;/* block memory for lockobjs */
#ifdef HAVE_STATISTICS
	DB_LOCK_PSTAT	part_stat;	/* Partition stats. */
#endif
} DB_LOCKPART;

#define	FREE_LOCKS(lt, part)	((lt)->part_array[part].free_locks)
#define	FREE_OBJS(lt, part)	((lt)->part_array[part].free_objs)

/*
 * DB_LOCKTAB --
 *	The primary library lock data structure (i.e., the one referenced
 * by the environment, as opposed to the internal one laid out in the region.)
 */
struct __db_locktab {
	ENV		*env;		/* Environment. */
	REGINFO		 reginfo;	/* Region information. */
	u_int8_t	*conflicts;	/* Pointer to conflict matrix. */
	DB_LOCKPART	*part_array;	/* Beginning of partition array. */
#ifdef HAVE_STATISTICS
	DB_LOCK_HSTAT	*obj_stat;	/* Object hash stats array. */
#endif
	DB_HASHTAB	*obj_tab;	/* Beginning of object hash table. */
	DB_HASHTAB	*locker_tab;	/* Beginning of locker hash table. */
};

/*
 * Test for conflicts.
 *
 * Cast HELD and WANTED to ints, they are usually db_lockmode_t enums.
 */
#define	CONFLICTS(T, R, HELD, WANTED) \
	(T)->conflicts[((int)HELD) * (R)->nmodes + ((int)WANTED)]

#define	OBJ_LINKS_VALID(L) ((L)->links.stqe_prev != -1)

struct __db_lock { /* SHARED */
	/*
	 * Wait on mutex to wait on lock.  You reference your own mutex with
	 * ID 0 and others reference your mutex with ID 1.
	 */
	db_mutex_t	mtx_lock;

	roff_t		holder;		/* Who holds this lock. */
	u_int32_t	gen;		/* Generation count. */
	SH_TAILQ_ENTRY	links;		/* Free or holder/waiter list. */
	SH_LIST_ENTRY	locker_links;	/* List of locks held by a locker. */
	u_int32_t	refcount;	/* Reference count the lock. */
	db_lockmode_t	mode;		/* What sort of lock. */
	roff_t		obj;		/* Relative offset of object struct. */
	u_int32_t	indx;		/* Hash index of this object. */
	db_status_t	status;		/* Status of this lock. */
};

/*
 * Serializable Snapshot Isolation (SSI) helpers.
 *
 * A locker is linked to its transaction detail via td_off so the lock layer
 * can read MVCC snapshot LSNs (read_lsn / visible_lsn) when reasoning about
 * rw-antidependencies recorded by SIREAD locks.
 */
#define	LOCKER_TD(env, lockerp)						\
    ((TXN_DETAIL *)R_ADDR(&(env)->tx_handle->reginfo, (lockerp)->td_off))

#define	LOCK_HOLDER(env, lp)						\
    ((DB_LOCKER *)R_ADDR(&(env)->lk_handle->reginfo, (lp)->holder))

#define	LOCK_OWNER(env, lp)		LOCKER_TD(env, LOCK_HOLDER(env, lp))

#define	LOCK_READLSN(env, lp)		(LOCK_OWNER(env, lp)->read_lsn)

#define	LOCK_COMMITLSN(env, lp)		(LOCK_OWNER(env, lp)->visible_lsn)

/*
 * Flag values for __lock_put_internal:
 * DB_LOCK_DOALL:     Unlock all references in this lock (instead of only 1).
 * DB_LOCK_FREE:      Free the lock (used in checklocker).
 * DB_LOCK_NOPROMOTE: Don't bother running promotion when releasing locks
 *		      (used by __lock_put_internal).
 * DB_LOCK_UNLINK:    Remove from the locker links (used in checklocker).
 * Make sure that these do not conflict with the interface flags because
 * we pass some of those around.
 */
#define	DB_LOCK_DOALL		0x010000
#define	DB_LOCK_SNAPSHOT_SAFE	0x020000	/* SSI: snapshot-safe acquire. */
#define	DB_LOCK_FREE		0x040000
#define	DB_LOCK_NOPROMOTE	0x080000
#define	DB_LOCK_UNLINK		0x100000
#define	DB_LOCK_ONEWAITER	0x400000

/*
 * Macros to get/release different types of mutexes.
 */
/*
 * Operations on lock objects must be protected by a mutex, either on their
 * partition or on the lock region.  Lock structures associated with that
 * object are protected as well.  Each partition has a free list of objects
 * and lock structures protected by that mutex.  We want to avoid getting
 * multiple mutexes, particularly in __lock_vec, when there is only a
 * single partition.  If there is only one partition, then all the calls
 * to LOCK_SYSTEM_LOCK(UNLOCK) actually acquire(release) a lock system
 * wide mutex and MUTEX_LOCK(UNLOCK)_PARTITION are no-ops.  If the number
 * of partitions is greater than one, then LOCK_SYSTEM_LOCK(UNLOCK) is a
 * no-op, and MUTEX_LOCK(UNLOCK)_PARTITION acquire a mutex on a particular
 * partition of the lock table.
 */
#define	LOCK_SYSTEM_LOCK(lt, reg) do {					\
	if ((reg)->part_t_size == 1)					\
		MUTEX_LOCK((lt)->env, (reg)->mtx_region);		\
} while (0)
#define	LOCK_SYSTEM_UNLOCK(lt, reg) do {				\
	if ((reg)->part_t_size == 1)					\
		MUTEX_UNLOCK((lt)->env, (reg)->mtx_region);		\
} while (0)
#define	MUTEX_LOCK_PARTITION(lt, reg, p) do {				\
	if ((reg)->part_t_size != 1)					\
		MUTEX_LOCK((lt)->env, (lt)->part_array[p].mtx_part);	\
} while (0)
#define	MUTEX_UNLOCK_PARTITION(lt, reg, p) do {				\
	if ((reg)->part_t_size != 1)					\
		MUTEX_UNLOCK((lt)->env, (lt)->part_array[p].mtx_part);	\
} while (0)

#define	OBJECT_LOCK(lt, reg, obj, ndx) do {				\
	ndx = __lock_ohash(obj) % (reg)->object_t_size;			\
	MUTEX_LOCK_PARTITION(lt, reg, LOCK_PART(reg, ndx));		\
} while (0)

#define	OBJECT_LOCK_NDX(lt, reg, ndx)					\
	MUTEX_LOCK_PARTITION(lt, reg, LOCK_PART(reg, ndx));

#define	OBJECT_UNLOCK(lt, reg, ndx)					\
	MUTEX_UNLOCK_PARTITION(lt, reg, LOCK_PART(reg, ndx));

/*
 * Protect the object deadlock detector queue and the locker allocation
 * and active queues
 */
#define	LOCK_DD(env, region)						\
	MUTEX_LOCK(env, (region)->mtx_dd)
#define	UNLOCK_DD(env, region)						\
	MUTEX_UNLOCK(env, (region)->mtx_dd)
/*
 * The locker hash table is guarded by a set of striped shared latches
 * (mtx_locker_stripe[]).  Each stripe owns, outright:
 *
 *  1. the locker_tab[indx] bucket chains that map onto it
 *     (LOCK_LOCKER_STRIPE(indx) == the stripe).  Readers take it shared,
 *     writers exclusive.
 *  2. its DB_LOCKERSTRIPE: free_lockers, the in-use ulinks list, and the
 *     nlockers / maxnlockers counters.  A locker create pops from, and the
 *     matching free pushes back to, the stripe that owns its bucket chain --
 *     the SAME stripe, derived from the same id, so a locker can never be
 *     returned to a stripe other than the one it came from.
 *
 * Stripe 0 additionally guards the region-wide lock_id / cur_maxid id
 * counters (LOCK_LOCKER_ID_ALLOC), which have no per-stripe home because an
 * id's stripe is not known until the id is chosen.  That is the ONLY remaining
 * role for stripe 0 beyond its own bucket, and it is paid once per __lock_id
 * call -- not on the txn_begin create path, which is the P12 fix.
 *
 * The stripe is keyed on the BUCKET INDEX, not on the locker id.  That is not
 * cosmetic: locker_t_size is a prime (__db_tablesize), so ids that differ by
 * locker_t_size share a bucket while landing in different id-keyed stripes
 * (id 1 and id 1 + 1031 with the default table).  An id-keyed stripe therefore
 * does not own the chain its holder walks, and two writers could corrupt one
 * chain concurrently.  Keying on the bucket makes a stripe the sole owner of
 * every chain it covers.
 *
 * LOCK ORDER.  No path holds two different bucket stripes at once, so the
 * single-stripe paths are trivially ordered.  Where more than one stripe is
 * needed the stripes are taken ASCENDING FROM 0 -- LOCK_LOCKERS takes all of
 * them that way, and LOCK_LOCKERS_REST completes that same prefix-ordered set
 * from a caller already holding stripe 0.  Ascending acquisition is what makes
 * the order total and deadlock-free.
 *
 * BEFORE P12 this comment said "stripe 0 is always acquired FIRST and released
 * LAST" and the create/free paths really did take it that way, because stripe 0
 * was the allocation latch for region-wide free/ulinks/nlockers state.  Those
 * three are now per-stripe (see DB_LOCKERSTRIPE), so the create and free paths
 * take ONE stripe -- their bucket's -- and stripe 0 is no longer on them at
 * all.  Nothing nests inside a bucket stripe, so there is nothing left for a
 * stripe-0-first rule to order.
 *
 * A consequence of the previous design is also gone: a READER whose bucket
 * mapped onto stripe 0 used to block behind any writer anywhere, because every
 * writer held stripe 0.  Stripe 0 is now just a stripe for bucket purposes.
 *
 * LOCK_LOCKERS/UNLOCK_LOCKERS keep their original meaning: mutual exclusion of
 * the whole locker table, for paths that walk every bucket, walk every
 * stripe's ulinks list, or touch more than one locker's chain.  RDLOCK_LOCKER
 * is the shared single-bucket read path; LOCK_LOCKER_WRITE is the exclusive
 * single-locker write path, which is what locker create and locker free need.
 */
/*
 * Bucket index and bucket-keyed stripe index for a locker id.  (Expands
 * __lock_locker_hash, defined below -- macro bodies are substituted at the use
 * site, all of which are in .c files.)
 */
#define	LOCK_LOCKER_NDX(region, id)					\
	(__lock_locker_hash(id) % (region)->locker_t_size)
#define	LOCK_LOCKER_ID_STRIPE(region, id)				\
	LOCK_LOCKER_STRIPE(LOCK_LOCKER_NDX(region, id))

/*
 * The stripe whose DB_LOCKERSTRIPE holds a given locker's allocation state.
 * Normally the stripe that owns its bucket chain; with the P12 switch off,
 * always stripe 0 (the pre-P12 region-wide lists, which stripe 0 guarded).
 */
#define	LOCK_LOCKER_ALLOC_STRIPE(region, s)				\
	((region)->locker_shard ? (s) : 0)

/*
 * The per-stripe allocation state for a locker id: free list, in-use ulinks
 * list and counters.  Valid to touch only while that id's stripe is held
 * exclusive (LOCK_LOCKER_WRITE, or LOCK_LOCKERS which covers all of them).
 */
#define	LOCK_LOCKER_ID_LS(region, id)					\
	(&(region)->locker_stripe[LOCK_LOCKER_ALLOC_STRIPE(region,	\
	    LOCK_LOCKER_ID_STRIPE(region, id))])

#define	RDLOCK_LOCKER(env, region, id)					\
	MUTEX_READLOCK(env,						\
	    (region)->mtx_locker_stripe[LOCK_LOCKER_ID_STRIPE(region, id)])
#define	RDUNLOCK_LOCKER(env, region, id)				\
	MUTEX_UNLOCK(env,						\
	    (region)->mtx_locker_stripe[LOCK_LOCKER_ID_STRIPE(region, id)])

/*
 * Stripe 0 on its own, for the region-wide locker ID COUNTERS (lock_id,
 * cur_maxid) only.  An id's own stripe cannot guard them: the stripe is not
 * known until the id has been chosen.  This is NOT the old allocation latch --
 * the free list, ulinks list and counters it used to cover are now per-stripe,
 * and the create path does not take this at all.
 */
#define	LOCK_LOCKER_ID_ALLOC(env, region)				\
	MUTEX_LOCK(env, (region)->mtx_locker_stripe[0])
#define	UNLOCK_LOCKER_ID_ALLOC(env, region)				\
	MUTEX_UNLOCK(env, (region)->mtx_locker_stripe[0])

/*
 * As above, but skipping the acquire when the bucket maps onto stripe 0, which
 * the caller is already holding (the latches are not recursive).  Used by
 * __lock_id, which must hold stripe 0 across the id choice and the create that
 * follows it, and by the switched-off arm of LOCK_LOCKER_WRITE.
 */
#define	LOCK_LOCKER_BUCKET(env, region, id) do {			\
	u_int32_t __bs = LOCK_LOCKER_ID_STRIPE(region, id);		\
	if (__bs != 0)							\
		MUTEX_LOCK(env, (region)->mtx_locker_stripe[__bs]);	\
} while (0)
#define	UNLOCK_LOCKER_BUCKET(env, region, id) do {			\
	u_int32_t __bs = LOCK_LOCKER_ID_STRIPE(region, id);		\
	if (__bs != 0)							\
		MUTEX_UNLOCK(env, (region)->mtx_locker_stripe[__bs]);	\
} while (0)

/*
 * THE SINGLE-LOCKER WRITE PATH -- locker create and locker free.
 *
 * With the P12 sharding on (the default) this is ONE latch: the stripe that
 * owns both the bucket chain and that stripe's free/ulinks/counters, taken
 * exclusive.  With it off it is the pre-P12 pair -- stripe 0 for the (then
 * region-wide) allocation state, plus the bucket stripe nested inside it,
 * skipped when they coincide.  Both arms acquire ascending from 0, so the lock
 * order is the same either way.
 */
#define	LOCK_LOCKER_WRITE(env, region, id) do {				\
	if ((region)->locker_shard)					\
		MUTEX_LOCK(env, (region)->mtx_locker_stripe[		\
		    LOCK_LOCKER_ID_STRIPE(region, id)]);		\
	else {								\
		MUTEX_LOCK(env, (region)->mtx_locker_stripe[0]);	\
		LOCK_LOCKER_BUCKET(env, region, id);			\
	}								\
} while (0)
#define	UNLOCK_LOCKER_WRITE(env, region, id) do {			\
	if ((region)->locker_shard)					\
		MUTEX_UNLOCK(env, (region)->mtx_locker_stripe[		\
		    LOCK_LOCKER_ID_STRIPE(region, id)]);		\
	else {								\
		UNLOCK_LOCKER_BUCKET(env, region, id);			\
		MUTEX_UNLOCK(env, (region)->mtx_locker_stripe[0]);	\
	}								\
} while (0)

#define	LOCK_LOCKERS(env, region) do {					\
	u_int32_t __s;							\
	for (__s = 0; __s < LOCK_LOCKER_STRIPES; __s++)			\
		MUTEX_LOCK(env, (region)->mtx_locker_stripe[__s]);	\
} while (0)
#define	UNLOCK_LOCKERS(env, region) do {				\
	u_int32_t __s;							\
	for (__s = LOCK_LOCKER_STRIPES; __s-- > 0; )			\
		MUTEX_UNLOCK(env, (region)->mtx_locker_stripe[__s]);	\
} while (0)

/*
 * Escalation from stripe 0 to the whole table: stripes 1..N-1, taken ascending
 * while stripe 0 is already held, so the resulting hold is exactly
 * LOCK_LOCKERS and acquired in the same ascending order.  Used by __lock_id
 * for the id-wraparound rebuild, which must walk every stripe's ulinks list
 * while holding the id counters.
 */
#define	LOCK_LOCKERS_REST(env, region) do {				\
	u_int32_t __s;							\
	for (__s = 1; __s < LOCK_LOCKER_STRIPES; __s++)			\
		MUTEX_LOCK(env, (region)->mtx_locker_stripe[__s]);	\
} while (0)
#define	UNLOCK_LOCKERS_REST(env, region) do {				\
	u_int32_t __s;							\
	for (__s = LOCK_LOCKER_STRIPES; __s-- > 1; )			\
		MUTEX_UNLOCK(env, (region)->mtx_locker_stripe[__s]);	\
} while (0)

/*
 * __lock_locker_hash --
 *	Hash function for entering lockers into the locker hash table.
 *	Since these are simply 32-bit unsigned integers at the moment,
 *	just return the locker value.
 */
#define	__lock_locker_hash(locker)	(locker)
#define	LOCKER_HASH(lt, reg, locker, ndx)				\
	ndx = __lock_locker_hash(locker) % (reg)->locker_t_size;

#if defined(__cplusplus)
}
#endif

#include "dbinc_auto/lock_ext.h"
#endif /* !_DB_LOCK_H_ */

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)region.h	11.2 (Sleepycat) 8/24/99
 */

/*
 * The DB environment consists of some number of "regions", which are described
 * by the following four structures:
 *
 *	REGENV	   -- shared information about the environment
 *	REGENV_REF -- file describing system memory version of REGENV
 *	REGION	   -- shared information about a single region
 *	REGINFO	   -- per-process information about a REGION
 *
 * There are three types of memory that hold regions:
 *	per-process heap (malloc)
 *	file mapped into memory (mmap, MapViewOfFile)
 *	system memory (shmget, CreateFileMapping)
 *
 * If the regions are private to a process, they're in malloc.  If they're
 * public, they're in file mapped memory, or, optionally, in system memory.
 * Regions in the filesystem are named "__db.001", "__db.002" and so on.  If
 * we're not using a private environment allocated using malloc(3), the file
 * "__db.001" will always exist, as we use it to synchronize on the regions,
 * whether they exist in file mapped memory or system memory.
 *
 * The file "__db.001" contains a REGENV structure and a linked list of some
 * number of REGION structures.  Each of the REGION structures describes and
 * locks one of the underlying shared regions used by DB.
 *
 *	__db.001
 *	+---------+
 *	|REGENV  |
 *	+---------+   +----------+
 *	|REGION   |-> | __db.002 |
 *	|	  |   +----------+
 *	+---------+   +----------+
 *	|REGION   |-> | __db.003 |
 *	|	  |   +----------+
 *	+---------+   +----------+
 *	|REGION   |-> | __db.004 |
 *	|	  |   +----------+
 *	+---------+
 *
 * The only tricky part about manipulating the regions is correctly creating
 * or joining the REGENV file, i.e., __db.001.  We have to be absolutely sure
 * that only one process creates it, and that everyone else joins it without
 * seeing inconsistent data.  Once that region is created, we can use normal
 * shared locking procedures to do mutal exclusion for all other regions.
 *
 * One of the REGION structures in the main environment region describes the
 * environment region itself.
 *
 * To lock a region, locate the REGION structure that describes it and acquire
 * the region's mutex.  There is one exception to this rule -- the lock for the
 * environment region itself is in the REGENV structure, and not in the REGION
 * that describes the environment region.  That's so that we can acquire a lock
 * without walking linked lists that could potentially change underneath us.
 * The REGION will not be moved or removed during the life of the region, and
 * so long-lived references to it can be held by the process.
 *
 * All requests to create or join a region return a REGINFO structure, which
 * is held by the caller and used to open and subsequently close the reference
 * to the region.  The REGINFO structure contains the per-process information
 * that we need to access the region.
 *
 * The one remaining complication.  If the regions (including the environment
 * region) really live in system memory, we need some way of finding it.  We
 * do this by writing the REGENV_REF structure into the "__db.001" file.  When
 * we first open that file, and realize it holds a REGENV_REF instead of a
 * REGENV structure, we simply use that information to redirect to the real
 * "__db.001" file.  Currently, this only happens when the REGENV file is in
 * shared system memory returned by the UNIX shmget(2) call.
 *
 * Although DB does not currently grow regions when they run out of memory, it
 * would be possible to do so.  To grow a region, allocate a new region of the
 * appropriate size, then copy the old region over it and insert the additional
 * space into the already existing shalloc arena.  Callers may have to fix up
 * local references, but that should be easy to do.  This failed in historic
 * versions of DB because the region lock lived in the mapped memory, and when
 * it was unmapped and remapped (or copied), threads could lose track of it.
 * Once we moved that lock into a region that is never unmapped, growing should
 * work.  That all said, current versions of DB don't implement region grow
 * because some systems don't support mutex copying, e.g., from OSF1 V4.0:
 *
 *      The address of an msemaphore structure may be significant.  If the
 *	msemaphore structure contains any value copied from an msemaphore
 *	structure at a different address, the result is undefined.
 */
#define	DB_REGION_FMT	"__db.%03d"	/* Region file name format. */
#define	DB_REGION_NAME_NUM	5	/* First digit offset in file names. */
#define	DB_REGION_NAME_LENGTH	8	/* Length of file names. */

#define	DB_REGION_ENV	"__db.001"	/* Primary environment name. */

#define	INVALID_REGION_SEGID	-1	/* Segment IDs are either shmget(2) or
					 * Win16 segment identifiers.  They are
					 * both stored in an "int", and we need
					 * an out-of-band value.
					 */
/*
 * Currently, region offsets are limited to 32-bits.  I expect that's going
 * to have to be fixed in the not-too-distant future, since we won't want to
 * split 100Gb memory pools into that many different regions.  It's typedef'd
 * so it won't be too painful to upgrade.
 */
typedef u_int32_t roff_t;

/*
 * Nothing can live at region offset 0, because, in all cases, that's where
 * we store *something*.  Lots of code needs an out-of-band value for region
 * offsets, so we use 0.
 */
#define	INVALID_ROFF		0

/* Reference describing system memory version of REGENV. */
typedef struct __db_reg_env_ref {
	roff_t	   size;		/* Region size. */
	int	   segid;		/* shmget(2) ID. */
} REGENV_REF;

/* Per-environment region information. */
typedef struct __db_reg_env {
	/*
	 * !!!
	 * The mutex must be the first entry in the structure to guarantee
	 * correct alignment.
	 */
	MUTEX      mutex;		/* Environment mutex. */

	/*
	 * !!!
	 * Note, the magic and panic fields are NOT protected by the mutex,
	 * and for this reason cannot be anything more complicated than a
	 * zero/non-zero value.
	 *
	 * !!!
	 * Some 64-bit architectures (e.g., the OSF/1 Alpha processor) do not
	 * support 32-bit atomic reads and writes, and so have an interesting
	 * bug where sequential 32-bit values can be accidentally overwritten,
	 * i.e., a variable protected by a lock gets overwritten by a thread
	 * that doesn't hold the lock, simply because the variable sequentially
	 * followed a variable that didn't need the lock for protection. We do
	 * not want setting the panic value to be overwritten by another thread
	 * unlocking the region, or vice-versa, for that matter.  As the magic
	 * variable is written only during region creation, list it first to
	 * ensure this cannot happen.
	 *
	 * !!!
	 * The valid region magic number must appear at the same byte offset
	 * in both the environment and each shared region, as Windows/95 uses
	 * it to determine if the memory has been zeroed since it was last used.
	 */
#define	DB_REGION_MAGIC	0x120897
	u_int32_t  magic;		/* Valid region magic number. */

	int	   panic;		/* Environment is dead. */

	int	   majver;		/* Major DB version number. */
	int	   minver;		/* Minor DB version number. */
	int	   patch;		/* Patch DB version number. */

					/* List of regions. */
	SH_LIST_HEAD(__db_regionh) regionq;

	u_int32_t  refcnt;		/* References to the environment. */

	size_t	   pad;			/* Guarantee that following memory is
					 * size_t aligned.  This is necessary
					 * because we're going to store the
					 * allocation region information there.
					 */
} REGENV;

/* Per-region shared region information. */
typedef struct __db_region {
	/*
	 * !!!
	 * The mutex must be the first entry in the structure to guarantee
	 * correct alignment.
	 */
	MUTEX	   mutex;		/* Region mutex. */

	/*
	 * !!!
	 * The valid region magic number must appear at the same byte offset
	 * in both the environment and each shared region, as Windows/95 uses
	 * it to determine if the memory has been zeroed since it was last used.
	 */
	u_int32_t  magic;

	SH_LIST_ENTRY q;		/* Linked list of REGIONs. */

	roff_t	   size;		/* Region size in bytes. */

	roff_t	   primary;		/* Primary data structure offset. */

	int	   segid;		/* UNIX shmget(2), Win16 segment ID. */

#define	REG_ID_INVALID	0		/* Invalid. */
#define	REG_ID_ENV	1		/* Environment. */
#define	REG_ID_LOCK	2		/* Lock region. */
#define	REG_ID_LOG	3		/* Log region. */
#define	REG_ID_MPOOL	4		/* Mpool region. */
#define	REG_ID_TXN	5		/* Txn region. */
#define	REG_ID_ASSIGN	(REG_ID_TXN + 1)/* First assignable region number. */
	int	   id;			/* Region id. */

#define	REG_DEAD	0x01		/* Region may be corrupted. */
	u_int32_t  flags;
} REGION;

/*
 * Per-process/per-attachment information about a single region.
 */
struct __db_reginfo_t {		/* __db_r_attach IN parameters. */
	int	    id;			/* Region id: used for naming. */
	int	    mode;		/* File creation mode. */

				/* __db_r_attach OUT parameters. */
	REGION	   *rp;			/* Shared region. */

	char	   *name;		/* Region file name. */

	void	   *addr;		/* Region allocation address. */
	void	   *primary;		/* Primary data structure address. */

	void	   *wnt_handle;		/* Win/NT HANDLE. */

#define	REGION_CREATE		0x01	/* Caller created region. */
#define	REGION_CREATE_OK	0x02	/* Caller willing to create region. */
	u_int32_t   flags;
};

/*
 * R_ADDR	Return a per-process address for a shared region offset.
 * R_OFFSET	Return a shared region offset for a per-process address.
 *
 * !!!
 * R_OFFSET should really be returning a ptrdiff_t, but that's not yet
 * portable.  We use u_int32_t, which restricts regions to 4Gb in size.
 */
#define	R_ADDR(base, offset)						\
	((void *)((u_int8_t *)((base)->addr) + offset))
#define	R_OFFSET(base, p)						\
	((u_int32_t)((u_int8_t *)(p) - (u_int8_t *)(base)->addr))

/*
 * R_LOCK	Lock/unlock a region.
 * R_UNLOCK
 */
#define	R_LOCK(dbenv, reginfo)						\
	MUTEX_LOCK(&(reginfo)->rp->mutex, (dbenv)->lockfhp)
#define	R_UNLOCK(dbenv, reginfo)					\
	MUTEX_UNLOCK(&(reginfo)->rp->mutex)

/* PANIC_CHECK:	Check to see if the DB environment is dead. */
#define	PANIC_CHECK(dbenv)						\
	if (DB_GLOBAL(db_panic) &&					\
	    (dbenv)->reginfo != NULL && ((REGENV *)			\
	    ((REGINFO *)(dbenv)->reginfo)->primary)->panic != 0)	\
		return (DB_RUNRECOVERY);

/*
 * All regions are created on 8K boundaries out of sheer paranoia, so that
 * we don't make some underlying VM unhappy.
 */
#define	OS_ROUNDOFF(i, s) {						\
	(i) += (s) - 1;							\
	(i) -= (i) % (s);						\
}
#define	OS_VMPAGESIZE		(8 * 1024)
#define	OS_VMROUNDOFF(i)	OS_ROUNDOFF(i, OS_VMPAGESIZE)

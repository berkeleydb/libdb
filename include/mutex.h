/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)mutex.h	11.6 (Sleepycat) 10/15/99
 */

/*********************************************************************
 * POSIX.1 pthreads interface.
 *********************************************************************/
#ifdef HAVE_MUTEX_PTHREADS
#include <pthread.h>

#define	MUTEX_FIELDS							\
	pthread_mutex_t mutex;		/* Mutex. */			\
	pthread_cond_t  cond;		/* Condition variable. */
#endif

/*********************************************************************
 * Solaris lwp threads interface.
 *
 * !!!
 * We use LWP mutexes on Solaris instead of UI or POSIX mutexes (both of
 * which are available), for two reasons.  First, the Solaris C library
 * includes versions of the both UI and POSIX thread mutex interfaces, but
 * they are broken in that they don't support inter-process locking, and
 * there's no way to detect it, e.g., calls to configure the mutexes for
 * inter-process locking succeed without error.  So, we use LWP mutexes so
 * that we don't fail in fairly undetectable ways because the application
 * wasn't linked with the appropriate threads library.  Second, there were
 * bugs in SunOS 5.7 (Solaris 7) where if an application loaded the C library
 * before loading the libthread/libpthread threads libraries (e.g., by using
 * dlopen to load the DB library), the pwrite64 interface would be translated
 * into a call to pwrite and DB would drop core.
 *********************************************************************/
#ifdef HAVE_MUTEX_SOLARIS_LWP
#include <synch.h>

#define	MUTEX_FIELDS							\
	lwp_mutex_t mutex;		/* Mutex. */			\
	lwp_cond_t cond;		/* Condition variable. */
#endif

/*********************************************************************
 * Solaris/Unixware threads interface.
 *********************************************************************/
#ifdef HAVE_MUTEX_UI_THREADS
#include <thread.h>
#include <synch.h>

#define	MUTEX_FIELDS							\
	mutex_t mutex;			/* Mutex. */			\
	cond_t  cond;			/* Condition variable. */
#endif

/*********************************************************************
 * AIX C library functions.
 *********************************************************************/
#ifdef HAVE_MUTEX_AIX_CHECK_LOCK
#include <sys/atomic_op.h>
typedef int tsl_t;

#define	MUTEX_ALIGN	sizeof(int)
#define	MUTEX_SET(x)	(!_check_lock(x, 0, 1))
#define	MUTEX_UNSET(x)	_clear_lock(x, 0)
#endif

/*********************************************************************
 * General C library functions (msemaphore).
 *
 * !!!
 * Check for HPPA as a special case, because it requires unusual alignment,
 * and doesn't support semaphores in malloc(3) or shmget(2) memory.
 *
 * !!!
 * Do not remove the MSEM_IF_NOWAIT flag.  The problem is that if a single
 * process makes two msem_lock() calls in a row, the second one returns an
 * error.  We depend on the fact that we can lock against ourselves in the
 * locking subsystem, where we set up a mutex so that we can block ourselves.
 * Tested on OSF1 v4.0.
 *********************************************************************/
#ifdef HAVE_MUTEX_HPPA_MSEM_INIT
#define	MUTEX_NO_MALLOC_LOCKS
#define	MUTEX_NO_SHMGET_LOCKS

#define	MUTEX_ALIGN	16
#endif

#if defined(HAVE_MUTEX_MSEM_INIT) || defined(HAVE_MUTEX_HPPA_MSEM_INIT)
#include <sys/mman.h>
typedef msemaphore tsl_t;

#ifndef MUTEX_ALIGN
#define	MUTEX_ALIGN	sizeof(int)
#endif
#define	MUTEX_INIT(x)	(msem_init(x, MSEM_UNLOCKED) == NULL)
#define	MUTEX_SET(x)	(!msem_lock(x, MSEM_IF_NOWAIT))
#define	MUTEX_UNSET(x)	msem_unlock(x, 0)
#endif

/*********************************************************************
 * MacOS.
 *
 * !!!
 * We should simplify this by always returning a no-need-to-lock lock
 * when we initialize the mutex.
 *********************************************************************/
#ifdef HAVE_MUTEX_MACOS
typedef unsigned char tsl_t;

#define	MUTEX_INIT(x)	0
#endif

/*********************************************************************
 * Reliant UNIX C library functions.
 *********************************************************************/
#ifdef HAVE_MUTEX_RELIANTUNIX_INITSPIN
#include <ulocks.h>
typedef spinlock_t tsl_t;

#define	MUTEX_INIT(x)	initspin(x, 1)
#define	MUTEX_SET(x)	(cspinlock(x) == 0)
#define	MUTEX_UNSET(x)	spinunlock(x)
#endif

/*********************************************************************
 * General C library functions (POSIX 1003.1 sema_XXX).
 *
 * !!!
 * Never selected by autoconfig in this release (semaphore calls are known
 * to not work in Solaris 5.5).
 *********************************************************************/
#ifdef HAVE_MUTEX_SEMA_INIT
#include <synch.h>
typedef sema_t tsl_t;

#define	MUTEX_ALIGN	sizeof(int)
#define	MUTEX_INIT(x)	(sema_init(x, 1, USYNC_PROCESS, NULL) != 0)
#define	MUTEX_SET(x)	(sema_wait(x) == 0)
#define	MUTEX_UNSET(x)	sema_post(x)
#endif

/*********************************************************************
 * SGI C library functions.
 *********************************************************************/
#ifdef HAVE_MUTEX_SGI_INIT_LOCK
#include <abi_mutex.h>
typedef abilock_t tsl_t;

#define	MUTEX_ALIGN	sizeof(int)
#define	MUTEX_INIT(x)	(init_lock(x) != 0)
#define	MUTEX_SET(x)	(!acquire_lock(x))
#define	MUTEX_UNSET(x)	release_lock(x)
#endif

/*********************************************************************
 * Solaris C library functions.
 *
 * !!!
 * These are undocumented functions, but they're the only ones that work
 * correctly as far as we know.
 *********************************************************************/
#ifdef HAVE_MUTEX_SOLARIS_LOCK_TRY
#include <sys/machlock.h>
typedef lock_t tsl_t;

#define	MUTEX_ALIGN	sizeof(int)
#define	MUTEX_INIT(x)	0
#define	MUTEX_SET(x)	_lock_try(x)
#define	MUTEX_UNSET(x)	_lock_clear(x)
#endif

/*********************************************************************
 * VMS.
 *********************************************************************/
#ifdef HAVE_MUTEX_VMS
#include <sys/mman.h>;
#include <builtins.h>
typedef unsigned char tsl_t;

#define	MUTEX_ALIGN		sizeof(unsigned int)
#ifdef __ALPHA
#define	MUTEX_SET(tsl)		(!__TESTBITSSI(tsl, 0))
#else /* __VAX */
#define	MUTEX_SET(tsl)		(!(int)_BBSSI(0, tsl))
#endif
#define	MUTEX_UNSET(tsl) 	(*(tsl) = 0)
#define	MUTEX_INIT(tsl)		MUTEX_UNSET(tsl)
#endif

/*********************************************************************
 * Win16
 *
 * Win16 spinlocks are simple because we cannot possibly be preempted.
 *
 * !!!
 * We should simplify this by always returning a no-need-to-lock lock
 * when we initialize the mutex.
 *********************************************************************/
#ifdef HAVE_MUTEX_WIN16
typedef unsigned int tsl_t;

#define	MUTEX_ALIGN		sizeof(unsigned int)
#define	MUTEX_INIT(x)		0
#define	MUTEX_SET(tsl)		(*(tsl) = 1)
#define	MUTEX_UNSET(tsl)	(*(tsl) = 0)
#endif

/*********************************************************************
 * Win32
 *
 * XXX
 * DBDB this needs to be byte-aligned!!
 *********************************************************************/
#ifdef HAVE_MUTEX_WIN32
typedef unsigned int tsl_t;

#define	MUTEX_ALIGN		sizeof(unsigned int)
#define	MUTEX_INIT(x)		0
#define	MUTEX_SET(tsl)		(!InterlockedExchange((PLONG)tsl, 1))
#define	MUTEX_UNSET(tsl)	(*(tsl) = 0)
#endif

/*********************************************************************
 * 68K/gcc assembly.
 *********************************************************************/
#ifdef HAVE_MUTEX_68K_GCC_ASSEMBLY
typedef unsigned char tsl_t;
#endif

/*********************************************************************
 * ALPHA/gcc assembly.
 *********************************************************************/
#ifdef HAVE_MUTEX_ALPHA_GCC_ASSEMBLY
typedef u_int32_t tsl_t;

#define	MUTEX_ALIGN	4
#endif

/*********************************************************************
 * HPPA/gcc assembly.
 *********************************************************************/
#ifdef HAVE_MUTEX_HPPA_GCC_ASSEMBLY
typedef u_int32_t tsl_t;

#define	MUTEX_ALIGN	16
#endif

/*********************************************************************
 * SCO/cc assembly.
 *********************************************************************/
#ifdef HAVE_MUTEX_SCO_X86_CC_ASSEMBLY
typedef unsigned char tsl_t;
#endif

/*********************************************************************
 * Sparc/gcc assembly.
 *********************************************************************/
#ifdef HAVE_MUTEX_SPARC_GCC_ASSEMBLY
typedef unsigned char tsl_t;
#endif

/*********************************************************************
 * UTS/cc assembly.
 *********************************************************************/
#ifdef HAVE_MUTEX_UTS_CC_ASSEMBLY
typedef int tsl_t;

#define	MUTEX_ALIGN	sizeof(int)
#define	MUTEX_INIT(x)	0
#define	MUTEX_SET(x)	(!uts_lock(x, 1))
#define	MUTEX_UNSET(x)	(*(x) = 0)
#endif

/*********************************************************************
 * x86/gcc assembly.
 *********************************************************************/
#ifdef HAVE_MUTEX_X86_GCC_ASSEMBLY
typedef unsigned char tsl_t;
#endif

/*
 * Mutex alignment defaults to one byte.
 *
 * !!!
 * Various systems require different alignments for mutexes (the worst we've
 * seen so far is 16-bytes on some HP architectures).  Malloc(3) is assumed
 * to return reasonable alignment, all other mutex users must ensure proper
 * alignment locally.
 */
#ifndef	MUTEX_ALIGN
#define	MUTEX_ALIGN	1
#endif

#define	MUTEX_IGNORE		0x001	/* Ignore, no lock required. */
#define	MUTEX_SELF_BLOCK	0x002	/* Must block self. */
#define	MUTEX_THREAD		0x004	/* Thread-only mutex. */

/* Mutex. */
struct __mutex_t {
#ifdef	HAVE_MUTEX_THREADS
#ifdef	MUTEX_FIELDS
	MUTEX_FIELDS
#else
	tsl_t	tas;			/* Test and set. */
#endif
	u_int32_t spins;		/* Spins before block. */
	u_int32_t locked;		/* !0 if locked. */
#else
	u_int32_t off;			/* Byte offset to lock. */
	u_int32_t pid;			/* Lock holder: 0 or process pid. */
#endif
	u_int32_t mutex_set_wait;	/* Granted after wait. */
	u_int32_t mutex_set_nowait;	/* Granted without waiting. */

	u_int8_t  flags;		/* MUTEX_XXX */
};

/* Redirect calls to the correct functions. */
#ifdef HAVE_MUTEX_THREADS
#if defined(HAVE_MUTEX_PTHREADS) || defined(HAVE_MUTEX_SOLARIS_LWP) || defined(HAVE_MUTEX_UI_THREADS)
#define	__db_mutex_init(a, b, c, d)	__db_pthread_mutex_init(a, b, d)
#define	__db_mutex_lock(a, b)		__db_pthread_mutex_lock(a)
#define	__db_mutex_unlock(a)		__db_pthread_mutex_unlock(a)
#else
#define	__db_mutex_init(a, b, c, d)	__db_tas_mutex_init(a, b, d)
#define	__db_mutex_lock(a, b)		__db_tas_mutex_lock(a)
#define	__db_mutex_unlock(a)		__db_tas_mutex_unlock(a)
#endif
#else
#define	__db_mutex_init(a, b, c, d)	__db_fcntl_mutex_init(a, b, c)
#define	__db_mutex_lock(a, b)		__db_fcntl_mutex_lock(a, b)
#define	__db_mutex_unlock(a)		__db_fcntl_mutex_unlock(a)
#endif

/*
 * Lock/unlock a mutex.  If the mutex was marked as uninteresting, the thread
 * of control can proceed without it.
 *
 * If the lock is for threads-only, then it was optionally not allocated and
 * file handles aren't necessary, as threaded applications aren't supported by
 * fcntl(2) locking.
 */
#define	MUTEX_LOCK(mp, fh)						\
	if (!F_ISSET((MUTEX *)(mp), MUTEX_IGNORE))			\
		(void)__db_mutex_lock(mp, fh);
#define	MUTEX_UNLOCK(mp)						\
	if (!F_ISSET((MUTEX *)(mp), MUTEX_IGNORE))			\
		(void)__db_mutex_unlock(mp);
#define	MUTEX_THREAD_LOCK(mp)						\
	if (mp != NULL)							\
		MUTEX_LOCK(mp, NULL)
#define	MUTEX_THREAD_UNLOCK(mp)						\
	if (mp != NULL)							\
		MUTEX_UNLOCK(mp)

/*
 * We use a single file descriptor for fcntl(2) locking, and (generally) the
 * object's offset in a shared region as the byte that we're locking.  So,
 * there's a (remote) possibility that two objects might have the same offsets
 * such that the locks could conflict, resulting in deadlock.  To avoid this
 * possibility, we offset the region offset by a small integer value, using a
 * different offset for each subsystem's locks.  Since all region objects are
 * suitably aligned, the offset guarantees that we don't collide with another
 * region's objects.
 */
#define	DB_FCNTL_OFF_GEN	0		/* Everything else. */
#define	DB_FCNTL_OFF_LOCK	1		/* Lock subsystem offset. */
#define	DB_FCNTL_OFF_MPOOL	2		/* Mpool subsystem offset. */

/*
 *	@(#)acconfig.h	8.31 (Sleepycat) 12/14/98
 */

/* Define to `int' if <sys/types.h> doesn't define.  */
#undef ssize_t

/* Define if you want a debugging version. */
#undef DEBUG

/* Define if you want a version with run-time diagnostic checking. */
#undef DIAGNOSTIC

/* Define if you have sigfillset (and sigprocmask). */
#undef HAVE_SIGFILLSET

/* Define if building big-file environment (e.g., Solaris, HP/UX). */
#undef HAVE_FILE_OFFSET_BITS

/* Define if you have spinlocks. */
#undef HAVE_SPINLOCKS

/* Define if you want to use mc68020/gcc assembly spinlocks. */
#undef HAVE_ASSEM_MC68020_GCC

/* Define if you want to use parisc/gcc assembly spinlocks. */
#undef HAVE_ASSEM_PARISC_GCC

/* Define if you want to use sco/cc assembly spinlocks. */
#undef HAVE_ASSEM_SCO_CC

/* Define if you want to use sparc/gcc assembly spinlocks. */
#undef HAVE_ASSEM_SPARC_GCC

/* Define if you want to use uts4/cc assembly spinlocks. */
#undef HAVE_ASSEM_UTS4_CC

/* Define if you want to use x86/gcc assembly spinlocks. */
#undef HAVE_ASSEM_X86_GCC

/* Define if you have the AIX _check_lock spinlocks. */
#undef HAVE_FUNC_AIX

/* Define if you have the OSF1 or HPPA msemaphore spinlocks. */
#undef HAVE_FUNC_MSEM

/* Define if you have the SGI abilock_t spinlocks. */
#undef HAVE_FUNC_SGI

/* Define if you have the ReliantUNIX spinlock_t spinlocks. */
#undef HAVE_FUNC_RELIANT

/* Define if you have the Solaris mutex_t spinlocks. */
#undef HAVE_FUNC_SOLARIS

/* Define if your sprintf returns a pointer, not a length. */
#undef SPRINTF_RET_CHARPNT

@BOTTOM@

/*
 * Don't step on the namespace.  Also, other libraries have real snprintf(3)
 * implementations, don't want to override them just because they're loaded
 * after us.
 */
#ifndef HAVE_SNPRINTF
#define	snprintf	__db_snprintf
#endif
#ifndef HAVE_VSNPRINTF
#define	vsnprintf	__db_vsnprintf
#endif

/*
 * Big-file configuration.
 */
#ifdef	HAVE_FILE_OFFSET_BITS
#define	_LARGE_FILES				/* AIX specific. */
#define	_FILE_OFFSET_BITS	64
#endif

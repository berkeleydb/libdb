/*
 * @(#)db_config.h	11.11 (Sleepycat) 11/9/99
 */

/* Define to empty if the keyword does not work.  */
/* #undef const */

/* Define if your struct stat has st_blksize.  */
/* #undef HAVE_ST_BLKSIZE */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef mode_t */

/* Define to `long' if <sys/types.h> doesn't define.  */
/* #undef off_t */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* Define if the `S_IS*' macros in <sys/stat.h> do not work properly.  */
/* #undef STAT_MACROS_BROKEN */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
/* #undef TIME_WITH_SYS_TIME */

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
/* #undef WORDS_BIGENDIAN */

/* Define if you are building a version for running the test suite. */
/* #undef CONFIG_TEST */

/* Define if you want a debugging version. */
/* #undef DEBUG */

/* Define if you want a version that logs read operations. */
/* #undef DEBUG_ROP */

/* Define if you want a version that logs write operations. */
/* #undef DEBUG_WOP */

/* Define if you want a version with run-time diagnostic checking. */
/* #undef DIAGNOSTIC */

/* Define if fcntl/F_SETFD denies child access to file descriptors. */
/* #undef HAVE_FCNTL_F_SETFD */

/* Define if building big-file environment (e.g., Solaris, HP/UX). */
/* #undef HAVE_FILE_OFFSET_BITS */

/* Mutex possibilities. */
/* #undef HAVE_MUTEX_68K_GCC_ASSEMBLY */
/* #undef HAVE_MUTEX_AIX_CHECK_LOCK */
/* #undef HAVE_MUTEX_HPPA_GCC_ASSEMBLY */
/* #undef HAVE_MUTEX_HPPA_MSEM_INIT */
/* #undef HAVE_MUTEX_MACOS */
/* #undef HAVE_MUTEX_MSEM_INIT */
/* #undef HAVE_MUTEX_PTHREADS */
/* #undef HAVE_MUTEX_RELIANTUNIX_INITSPIN */
/* #undef HAVE_MUTEX_SCO_X86_CC_ASSEMBLY */
/* #undef HAVE_MUTEX_SEMA_INIT */
/* #undef HAVE_MUTEX_SGI_INIT_LOCK */
/* #undef HAVE_MUTEX_SOLARIS_LOCK_TRY */
/* #undef HAVE_MUTEX_SOLARIS_LWP */
/* #undef HAVE_MUTEX_SPARC_GCC_ASSEMBLY */
#define HAVE_MUTEX_THREADS 1
/* #undef HAVE_MUTEX_UI_THREADS */
/* #undef HAVE_MUTEX_UTS_CC_ASSEMBLY */
#define HAVE_MUTEX_VMS 1
/* #undef HAVE_MUTEX_WIN16 */
/* #undef HAVE_MUTEX_WIN32 */
/* #undef HAVE_MUTEX_X86_GCC_ASSEMBLY */

/* Define if you have the sigfillset function.  */
#define HAVE_SIGFILLSET 1

/* Define if your sprintf returns a pointer, not a length. */
/* #undef SPRINTF_RET_CHARPNT */

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getopt function.  */
#define HAVE_GETOPT 1

/* Define if you have the getuid function.  */
#define HAVE_GETUID 1

/* Define if you have the memcmp function.  */
#define HAVE_MEMCMP 1

/* Define if you have the memcpy function.  */
#define HAVE_MEMCPY 1

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the mmap function.  */
#define HAVE_MMAP 1

/* Define if you have the munmap function.  */
#define HAVE_MUNMAP 1

/* Define if you have the pread function.  */
/* #undef HAVE_PREAD */

/* Define if you have the pstat_getdynamic function.  */
/* #undef HAVE_PSTAT_GETDYNAMIC */

/* Define if you have the qsort function.  */
#define HAVE_QSORT 1

/* Define if you have the raise function.  */
#define HAVE_RAISE 1

/* Define if you have the sched_yield function.  */
#define HAVE_SCHED_YIELD 1

/* Define if you have the select function.  */
#define HAVE_SELECT 1

/* Define if you have the shmget function.  */
/* #undef HAVE_SHMGET */

/* Define if you have the snprintf function.  */
/* #undef HAVE_SNPRINTF */

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

/* Define if you have the sysconf function.  */
#define HAVE_SYSCONF 1

/* Define if you have the vsnprintf function.  */
/* #undef HAVE_VSNPRINTF */

/* Define if you have the yield function.  */
/* #undef HAVE_YIELD */

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/select.h> header file.  */
/* #undef HAVE_SYS_SELECT_H */

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

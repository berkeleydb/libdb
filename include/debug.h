/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)debug.h	11.8 (Sleepycat) 11/8/99
 */

/*
 * When running with #DIAGNOSTIC defined, we smash memory and do memory
 * guarding with a special byte value.
 */
#define	CLEAR_BYTE	0xdb
#define	GUARD_BYTE	0xdc

/*
 * DB assertions.
 */
#ifdef DIAGNOSTIC
#define	DB_ASSERT(e)	((e) ? (void)0 : __db_assert(#e, __FILE__, __LINE__))
#else
#define	DB_ASSERT(e)	((void)0)
#endif

/*
 * Debugging macro to log operations.
 *	If DEBUG_WOP is defined, log operations that modify the database.
 *	If DEBUG_ROP is defined, log operations that read the database.
 *
 * D dbp
 * T txn
 * O operation (string)
 * K key
 * A data
 * F flags
 */
#define	LOG_OP(C, T, O, K, A, F) {					\
	DB_LSN __lsn;							\
	DBT __op;							\
	if (DB_LOGGING((C))) {						\
		memset(&__op, 0, sizeof(__op));				\
		__op.data = O;						\
		__op.size = strlen(O) + 1;				\
		(void)__db_debug_log((C)->dbp->dbenv,			\
		    T, &__lsn, 0, &__op, (C)->dbp->log_fileid, K, A, F);\
	}								\
}
#ifdef	DEBUG_ROP
#define	DEBUG_LREAD(C, T, O, K, A, F)	LOG_OP(C, T, O, K, A, F)
#else
#define	DEBUG_LREAD(C, T, O, K, A, F)
#endif
#ifdef	DEBUG_WOP
#define	DEBUG_LWRITE(C, T, O, K, A, F)	LOG_OP(C, T, O, K, A, F)
#else
#define	DEBUG_LWRITE(C, T, O, K, A, F)
#endif

/*
 * Hook for testing recovery at various places in the create/delete paths.
 */
#if CONFIG_TEST
#define DB_TEST_RECOVERY(dbp, val, ret, name)				\
do {									\
	int __ret;							\
	PANIC_CHECK((dbp)->dbenv);					\
	if ((dbp)->dbenv->test_copy == (val)) {				\
		/* COPY the FILE */					\
		(void)(dbp)->sync((dbp), 0);				\
		if ((__ret = __db_testcopy((dbp), (name))) != 0)	\
			(ret) = __db_panic((dbp)->dbenv, __ret);	\
	}								\
	if ((dbp)->dbenv->test_abort == (val)) {			\
		/* ABORT the TXN */					\
		(ret) = EINVAL;						\
		goto db_tr_err;						\
	}								\
} while (0)
#define DB_TEST_RECOVERY_LABEL	db_tr_err:
#else
#define DB_TEST_RECOVERY(dbp, val, ret, name)
#define DB_TEST_RECOVERY_LABEL
#endif

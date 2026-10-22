#!/usr/sbin/dtrace -qs
/*
 * Copyright (c) 2011, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * mutex.d - Display DB mutex wait times.
 *
 * Specify the target application with -p <pid> or -c "<program> [<args>..]"
 *
 * The optional integer maxcount parameter directs the script to exit once that
 * many mutex times have been accumulated.
 *
 * usage: mutex.d { -p <pid> | -c "<program> [<args]" } [limit]
 *
 * Static probes: 
 *   mutex__suspend(unsigned mutex, unsigned exclusive, unsigned alloc_id);
 *   mutex__resume(unsigned mutex, unsigned exclusive, unsigned alloc_id);
 *
 *	mutex:
 *		is the integer mutex id returned by mutex_alloc()
 *	exclusive:
 *		is set to 1 except when obtaining a read-write latch with
 *		shared access (allocated with DB_MUTEX_SHARED), then it is 0.
 *	alloc_id:
 *		is one of dbinc/mutex.h's MTX_XXX definitions (i.e., the class)
 */
#pragma D option defaultargs

string idnames[int];

dtrace:::BEGIN
{
	maxcount = $1 != 0 ? $1 : -1;
	mutexcount = 0;
	printf("DB Mutex wait times for process %d; interrupt to display summary\n", $target);
	/* Must match the MTX_* values in src/dbinc/mutex.h. */
	idnames[1] = "APPLICATION";
	idnames[2] = "ATOMIC_EMULATION";
	idnames[3] = "DB_HANDLE";
	idnames[4] = "ENV_DBLIST";
	idnames[5] = "ENV_EXCLDBLIST";
	idnames[6] = "ENV_HANDLE";
	idnames[7] = "ENV_REGION";
	idnames[8] = "LOCK_REGION";
	idnames[9] = "LOGICAL_LOCK";
	idnames[10] = "LOG_FILENAME";
	idnames[11] = "LOG_FLUSH";
	idnames[12] = "LOG_HANDLE";
	idnames[13] = "LOG_REGION";
	idnames[14] = "MPOOLFILE_HANDLE";
	idnames[15] = "MPOOL_BH";
	idnames[16] = "MPOOL_FH";
	idnames[17] = "MPOOL_FILE_BUCKET";
	idnames[18] = "MPOOL_HANDLE";
	idnames[19] = "MPOOL_HASH_BUCKET";
	idnames[20] = "MPOOL_REGION";
	idnames[21] = "MUTEX_REGION";
	idnames[22] = "MUTEX_TEST";
	idnames[23] = "REP_CHKPT";
	idnames[24] = "REP_DATABASE";
	idnames[25] = "REP_DIAG";
	idnames[26] = "REP_EVENT";
	idnames[27] = "REP_REGION";
	idnames[28] = "REP_START";
	idnames[29] = "REP_WAITER";
	idnames[30] = "REPMGR";
	idnames[31] = "SEQUENCE";
	idnames[32] = "TWISTER";
	idnames[33] = "TCL_EVENTS";
	idnames[34] = "TXN_ACTIVE";
	idnames[35] = "TXN_CHKPT";
	idnames[36] = "TXN_COMMIT";
	idnames[37] = "TXN_MVCC";
	idnames[38] = "TXN_REGION";
	idnames[39] = "MPOOL_AIO";
}

bdb$target:::mutex-suspend
{
	self->suspend = timestamp;
}

/* mutex-resume(unsigned mutex, boolean exclusive, unsigned alloc_id, struct __db_mutex_t *mutexp) */
bdb$target:::mutex-resume
/self->suspend/
{
	this->duration = timestamp - self->suspend;
	self->suspend = 0;
	@mutexwaits[arg0, arg1, idnames[arg2], tid] = quantize(this->duration);
	@classwaits[idnames[arg2], arg1] = quantize(this->duration);
	mutexcount++;
}

bdb$target:::mutex-resume
/mutexcount == maxcount/
{
	exit(0);
}

dtrace:::END
{
	printf("Mutex wait times grouped by (mutex, mode, thread)\n");
	printa("Mutex %d exclusive %d %s thread %p wait times in nanoseconds %@d\n", @mutexwaits);
	printf("\nAggregate mutex wait times grouped by (alloc_id, mode)\n");
	printa("Mutex class %s exclusive %d wait times in nanoseconds %@d\n", @classwaits);
}

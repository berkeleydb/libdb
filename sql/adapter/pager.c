/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2010 Oracle and/or its affiliates.  All rights reserved.
 */

#include "sqliteInt.h"
#include "btreeInt.h"

static sqlite3_file nullfile;

int sqlite3PagerIsMemdb(Pager *pPager) { return 0; }
int sqlite3PagerJournalMode(Pager *pPager, int mode) { return 0; }
i64 sqlite3PagerJournalSizeLimit(Pager *pPager, i64 limit) 
{ 
	Btree *p = (Btree *)pPager;
	BtShared *pBt = p->pBt;
	u_int32_t oldSize = pBt->logFileSize;
	int ret;
	/* Choose an 8k min, since it's twice the common default page size. */
	if( limit == -1 || limit >= 8192){
		if (limit == -1) {
			pBt->logFileSize = 
					SQLITE_DEFAULT_JOURNAL_SIZE_LIMIT;
		} else 
			pBt->logFileSize = (u_int32_t)limit;
		/* Set the value to the previous log size if it cannot be set. */
		if (pBt->transactional) {
			ret = pBt->dbenv->set_lg_max(pBt->dbenv, 
				pBt->logFileSize);
			if (ret != 0) {
				pBt->logFileSize = oldSize;
				pBt->dbenv->set_lg_max(pBt->dbenv, 
						pBt->logFileSize);
			}
		}
	}
	return pBt->logFileSize;
}

int sqlite3PagerLockingMode(Pager *pPager, int mode) { return 0; }

int sqlite3PagerPagecount(Pager *pPager, int *pCount) {
	pCount = 0;
	return SQLITE_OK;
}

sqlite3_file *sqlite3PagerFile(Pager *pPager) {
	return &nullfile;
}

#ifdef SQLITE_TEST
int sqlite3_pager_readdb_count = 0;    /* Number of full pages read from DB */
int sqlite3_pager_writedb_count = 0;   /* Number of full pages written to DB */
int sqlite3_pager_writej_count = 0;    /* Number of pages written to journal */
int sqlite3_opentemp_count = 0;
/*
** This routine is used for testing and analysis only.
** Some cheesy manipulation of the values in a is done so
** that the incrblob 2.* tests pass, even though auto_vacuum
** is not implemented for DB SQLITE.
*/
int *sqlite3PagerStats(Pager *pPager) {
	static int a[11];
	static int count = 0;
	if (count > 3) {
		a[9] = 4;
	} else {
		memset(&a[0], 0, sizeof(a));
		a[9] = 30;
		a[10] = 2;
	}
	count++;
	return a;
}
#endif

#ifdef ANDROID
// Begin Android add 
SQLITE_API void sqlite3_get_pager_stats(sqlite3_int64 * totalBytesOut,
					sqlite3_int64 * referencedBytesOut,
					sqlite3_int64 * dbBytesOut,
					int * numPagersOut)
{
  *totalBytesOut = 0;
  *referencedBytesOut = 0;
  *dbBytesOut = 0;
  *numPagersOut = 0;
}
#endif


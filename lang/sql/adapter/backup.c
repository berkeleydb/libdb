/*-
* See the file LICENSE for redistribution information.
*
* Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
*/
/*
** This file contains the implementation of the sqlite3_backup_XXX()
** API functions and the related features.
**
*/
#include "sqliteInt.h"
#include "btreeInt.h"
#include <db.h>

/* Forward declarations. */
static int btreeCopyPages(sqlite3_backup *p, int *pages);
static int btreeCleanDatabase(Btree *p);

extern int __os_open(ENV *, const char *, u_int32_t, u_int32_t, int, DB_FH **);
extern int __os_closehandle(ENV *, DB_FH *);

/*
** Structure allocated for each backup operation.
*/
struct sqlite3_backup {
	sqlite3* pDestDb;     /* Destination database handle */
	Btree *pDest;         /* Destination b-tree file */
	BtCursor destCur;     /* Destination cursor. */
	char *destName;       /* Name destination db. */
	int inDestTxn;        /* 1 for Transaction, 2 for Savepoint.*/
	int openDest;         /* Destination btree needs closing. */
	int iDb;              /* Id of destination database. */

	sqlite3* pSrcDb;      /* Source database handle */
	Btree *pSrc;          /* Source b-tree file */
	DBC *srcCur;		  /* Source cursor. */
	int *tables;          /* Tables to copy. */
	int currentTable;     /* Table currently being copied */
	char *srcName;        /* Name source db. */
	DB_TXN *srcTxn;

	int rc;               /* Backup process error code */
	int cleaned;          /* Whether the destination environment
						   * has been cleaned. */
	u32 lastUpdate;       /* The last update made while backup was
						   * in progress.*/

	/* These two variables are read by calls to backup_remaining()
	** and backup_pagecount().
	*/
	u32 nRemaining;        /* Number of pages left to copy */
	u32 nPagecount;        /* Total number of pages to copy */
};

/*
** THREAD SAFETY NOTES:
**
**   Once it has been created using backup_init(), a single sqlite3_backup
**   structure may be accessed via two groups of thread-safe entry points:
**
**     * Via the sqlite3_backup_XXX() API function backup_step() and
**       backup_finish(). Both these functions obtain the source database
**       handle mutex.
**
**   The other sqlite3_backup_XXX() API functions, backup_remaining() and
**   backup_pagecount() are not thread-safe functions. If they are called
**   while some other thread is calling backup_step() or backup_finish(),
**   the values returned may be invalid.
**
**   Depending on the SQLite configuration, the database handles and/or
**   the Btree objects may have their own mutexes that require locking.
**   Non-sharable Btrees (in-memory databases for example), do not have
**   associated mutexes.
*/

/*
** Return a pointer corresponding to database zDb (i.e. "main", "temp")
** in connection handle pDb. If such a database cannot be found, return
** a NULL pointer and write an error message to pErrorDb.
**
** If the "temp" database is requested, it may need to be opened by this
** function. If an error occurs while doing so, return 0 and write an
** error message to pErrorDb.
*/
static Btree *findBtree(sqlite3 *pErrorDb, sqlite3 *pDb, const char *zDb)
{
	int i, rc;
	Parse *pParse;

	rc = 0;

	i = sqlite3FindDbName(pDb, zDb);

	if (i == 1) {
		pParse = sqlite3StackAllocZero(pErrorDb, sizeof(*pParse));
		if (pParse == 0) {
			sqlite3Error(pErrorDb, SQLITE_NOMEM, "out of memory");
			rc = SQLITE_NOMEM;
		} else {
			pParse->db = pDb;
			if (sqlite3OpenTempDatabase(pParse)) {
				sqlite3Error(pErrorDb, pParse->rc, "%s",
				    pParse->zErrMsg);
				rc = SQLITE_ERROR;
				sqlite3DbFree(pDb, pParse->zErrMsg);
			}
			sqlite3StackFree(pErrorDb, pParse);
		}
		if (rc)
			return 0;
	}

	if (i < 0) {
		sqlite3Error(pErrorDb, SQLITE_ERROR, "unknown database %s", zDb);
		return 0;
	}

	return pDb->aDb[i].pBt;
}

/*
** Create an sqlite3_backup process to copy the contents of zSrcDb from
** connection handle pSrcDb to zDestDb in pDestDb. If successful, return
** a pointer to the new sqlite3_backup object.
**
** If an error occurs, NULL is returned and an error code and error message
** stored in database handle pDestDb.
** pDestDb  Database to write to
** zDestDb  Name of database within pDestDb
** pSrcDb   Database connection to read from
** zSrcDb   Name of database within pSrcDb
*/
sqlite3_backup *sqlite3_backup_init(sqlite3* pDestDb, const char *zDestDb,
    sqlite3* pSrcDb, const char *zSrcDb)
{
	sqlite3_backup *p;                    /* Value to return */
	Parse parse;
	DB_ENV *dbenv;
	DB_FH *fhp;
	int ret;

	p = NULL;
	ret = 0;
	fhp = NULL;

	if (!pDestDb || !pSrcDb)
		return 0;

	sqlite3_mutex_enter(pSrcDb->mutex);
	sqlite3_mutex_enter(pDestDb->mutex);
	if (pSrcDb == pDestDb) {
		sqlite3Error(pDestDb, SQLITE_ERROR,
		    "source and destination must be distinct");
		goto err;
	}

	/* Allocate space for a new sqlite3_backup object */
	p = (sqlite3_backup *)sqlite3_malloc(sizeof(sqlite3_backup));
	if (!p) {
		sqlite3Error(pDestDb, SQLITE_NOMEM, 0);
		goto err;
	}

	memset(p, 0, sizeof(sqlite3_backup));
	p->pSrc = findBtree(pDestDb, pSrcDb, zSrcDb);
	p->pDest = findBtree(pDestDb, pDestDb, zDestDb);
	p->pDestDb = pDestDb;
	p->pSrcDb = pSrcDb;

	if (0 == p->pSrc) {
		p->rc = p->pSrcDb->errCode;
		goto err;
	}
	if (0 == p->pDest) {
		p->rc = p->pDestDb->errCode;
		goto err;
	}

	p->iDb = sqlite3FindDbName(pDestDb, zDestDb);

	/* Test that the data file(s) is not read-only, editing the
	 * data would also reveal this, but SQLite expects this error
	 * before checking for other errors, such as reading an
	 * encrypted database without a key.
	 */
	if (p->pDest->pBt->dbStorage == DB_STORE_NAMED) {
		char *filename;

#ifdef BDBSQL_FILE_PER_TABLE
		p->rc = getMetaDataFileName(p->pDest->pBt->full_name, &filename);
		if (p->rc != SQLITE_OK)
			goto err;
#else
		filename = p->pDest->pBt->full_name;
#endif
		ret = __os_open(0, filename, 0, 0, 0, &fhp);
#ifdef BDBSQL_FILE_PER_TABLE
		sqlite3_free(filename);
#endif
		if (ret == EPERM) {
			p->rc = SQLITE_READONLY;
			goto err;
		}
		if (ret == 0)
			(void)__os_closehandle(0, fhp);
	}

	p->srcName = sqlite3_malloc(strlen(zSrcDb) + 1);
	p->destName = sqlite3_malloc(strlen(zDestDb) + 1);
	if (0 == p->srcName || 0 == p->destName) {
		p->rc = SQLITE_NOMEM;
		goto err;
	}
	strncpy(p->srcName, zSrcDb, strlen(zSrcDb) + 1);
	strncpy(p->destName, zDestDb, strlen(zDestDb) + 1);

	/*
	 * Make sure the schema has been read in, so the keyInfo
	 * can be retrieved for the indexes.  No-op if already read.
	 */
	memset(&parse, 0, sizeof parse);
	parse.db = p->pSrcDb;
	p->rc = sqlite3ReadSchema(&parse);
	if (p->rc != SQLITE_OK) {
		if (parse.zErrMsg != NULL)
			sqlite3DbFree(p->pSrcDb, parse.zErrMsg);
		goto err;
	}

	/* Begin a transaction on the source. */
	if (!p->pSrc->connected) {
		if ((p->rc = btreeOpenEnvironment(p->pSrc, 1)) != SQLITE_OK)
			goto err;
	}
	dbenv = p->pSrc->pBt->dbenv;
	p->rc = dberr2sqlite(dbenv->txn_begin(dbenv, p->pSrc->family_txn,
	    &p->srcTxn, 0));
	if (p->rc != SQLITE_OK) {
		sqlite3Error(pSrcDb, p->rc, 0);
		goto err;
	}

	/*
	 * Get the page count and list of tables to copy. This will
	 * result in a read lock on the schema table, held in the
	 * read transaction.
	 */
	if ((p->rc = btreeGetPageCount(p->pSrc,
	    &p->tables, &p->nPagecount, p->srcTxn)) != SQLITE_OK) {
		sqlite3Error(pSrcDb, p->rc, 0);
		goto err;
	}

	p->nRemaining = p->nPagecount;
	p->pSrc->nBackup++;
	p->pDest->nBackup++;
	p->lastUpdate = p->pSrc->updateDuringBackup;

	goto done;

err:	if (p != 0) {
		if (pDestDb->errCode == SQLITE_OK)
			sqlite3Error(pDestDb, p->rc, 0);
		if (p->srcTxn)
			p->srcTxn->abort(p->srcTxn);
		if (p->srcName != 0)
			sqlite3_free(p->srcName);
		if (p->destName != 0)
			sqlite3_free(p->destName);
		sqlite3_free(p);
		p = NULL;
	}
done:	sqlite3_mutex_leave(pDestDb->mutex);
	sqlite3_mutex_leave(pSrcDb->mutex);
	return p;
}

/* Close or free all handles and commit or rollback the transaction. */
static int backupCleanup(sqlite3_backup *p)
{
	int rc, rc2, ret;
	int op;
	void *app;
	DB *db;

	rc = rc2 = SQLITE_OK;

	if (!p || p->rc == SQLITE_OK)
		return rc;

	rc2 = sqlite3BtreeCloseCursor(&p->destCur);
	if (rc2 != SQLITE_OK)
		rc = rc2;
	if (p->srcCur) {
		db = p->srcCur->dbp;
		app = db->app_private;
		if ((ret = p->srcCur->close(p->srcCur)) == 0)
			ret = db->close(db, DB_NOSYNC);
		rc2 = dberr2sqlite(ret);
		/*
		 * The KeyInfo was allocated in btreeSetupIndex,
		 * so have to deallocate it here.
		 */
		if (app)
			sqlite3DbFree(p->pSrcDb, app);
	}
	if (rc2 != SQLITE_OK)
		rc = rc2;
	p->srcCur = 0;

	/*
	 * May retry on a locked or busy error, so keep
	 * these values.
	 */
	if (p->rc != SQLITE_LOCKED && p->rc != SQLITE_BUSY) {
		if (p->srcName)
			sqlite3_free(p->srcName);
		if (p->destName != 0)
			sqlite3_free(p->destName);
		p->srcName = p->destName = NULL;
	}

	if (p->tables != 0)
			sqlite3_free(p->tables);
	p->tables = NULL;

	p->pSrc->nBackup--;
	if (p->pDest != NULL)
		p->pDest->nBackup--;
	if (p->srcTxn) {
		if (p->rc == SQLITE_DONE)
			ret = p->srcTxn->commit(p->srcTxn, 0);
		else
			ret = p->srcTxn->abort(p->srcTxn);
		rc2 = dberr2sqlite(ret);
	}
	p->srcTxn = 0;
	if (rc2 != SQLITE_OK)
		rc = rc2;

	if (p->inDestTxn) {
		if (p->inDestTxn == 2) {
			if (p->rc == SQLITE_DONE)
				rc2 = sqlite3BtreeCommit(p->pDest);
			else
				rc2 = sqlite3BtreeRollback(p->pDest);
		} else {
			if (p->rc == SQLITE_DONE)
				op = SAVEPOINT_RELEASE;
			else
				op = SAVEPOINT_ROLLBACK;
			rc2 = sqlite3BtreeSavepoint(p->pDest, op,
			    p->pDest->nSavepoint - 2);
		}
		if (rc2 != SQLITE_OK)
			rc = rc2;
	}

	if (p->openDest && p->pDest &&
	    (p->rc != SQLITE_LOCKED && p->rc != SQLITE_BUSY)) {
		Schema *schema;
		char *home;

		if ((home = sqlite3Malloc(
		    strlen(p->pDest->pBt->orig_name) + 1)) != NULL)
			strcpy(home, p->pDest->pBt->orig_name);
		else
			rc = SQLITE_NOMEM;
		schema = p->pDestDb->aDb[p->iDb].pSchema;
		p->pDest->schema = NULL;
		rc2 = sqlite3BtreeClose(p->pDest);
		if (rc == SQLITE_OK)
			rc = rc2;
		if (rc == SQLITE_OK) {
			p->pDest = NULL;
			p->pDestDb->aDb[p->iDb].pBt = NULL;
			p->pDestDb->aDb[p->iDb].pSchema = schema;
			p->openDest = 0;
			rc = sqlite3BtreeFactory(p->pDestDb, home, 0,
			    SQLITE_DEFAULT_CACHE_SIZE,
			    p->pDestDb->openFlags, &p->pDest);
			p->pDestDb->aDb[p->iDb].pBt = p->pDest;
			p->pDest->pBt->db_oflags |= DB_CREATE;
#ifdef SQLITE_HAS_CODEC
			if (rc == SQLITE_OK) {
				if (p->iDb == 0)
					rc = sqlite3_key(p->pDestDb,
					    p->pSrc->pBt->encrypt_pwd,
					    p->pSrc->pBt->encrypt_pwd_len);
				else
					rc = sqlite3CodecAttach(p->pDestDb,
					    p->iDb, p->pSrc->pBt->encrypt_pwd,
					    p->pSrc->pBt->encrypt_pwd_len);
			}
#endif
			sqlite3_free(home);
		}
	}
	p->inDestTxn = 0;
	p->lastUpdate = p->pSrc->updateDuringBackup;
	return rc;
}

/*
 * If the source database is updated by the backup thread or
 * a locked or busy error occurs, reset the backup process.
 */
static void backupReset(sqlite3_backup *p)
{
	p->pSrc->nBackup++;
	p->pDest->nBackup++;
	p->cleaned = 0;
	p->currentTable = 0;
	p->nRemaining = p->nPagecount;
	p->rc = SQLITE_OK;
}

#ifdef SQLITE_HAS_CODEC
/*
 * Deletes all data and environment files of the given Btree.  Requires
 * that there are no other handles using the BtShared when this function
 * is called.
 */
int btreeDeleteEnvironment(Btree *p, char **home)
{
	BtShared *pBt;
	int rc, ret, iDb;
	DB_ENV *pEnv;
	char *filename, *tablename, *tmp, *databaseName;
	sqlite3 *db;
#ifdef BDBSQL_FILE_PER_TABLE
	DB_BTREE_STAT *stats;
	DBC *dbc;
	int numDb, i;
	DBT key, data;
	char *current;
	char **dataDir;

	dbc = NULL;
	numDb = i = 0;
	current = NULL;
	memset(&key, 0, sizeof key);
	memset(&data, 0, sizeof data);
#endif
	pBt = p->pBt;
	rc = SQLITE_OK;
	ret = 0;
	filename = tablename = databaseName = NULL;
	*home = NULL;

	if (pBt->dbStorage != DB_STORE_NAMED)
		return rc;

	if (!p->connected) {
		if ((rc = btreeOpenEnvironment(p, 1)) != SQLITE_OK)
			return rc;
	}
	pBt = p->pBt;
	pEnv = pBt->dbenv;

	/*
	 * Cannot delete the environment if there are other references
	 * to it.
	 */
	if (pBt->nRef > 1)
		return SQLITE_BUSY;

#ifdef BDBSQL_FILE_PER_TABLE
	/* Get the name to re-open the environment with. */
	pEnv->get_data_dirs(pEnv, (const char ***)&dataDir);
	/* Ignore the ../ at the beginning of the string. */
	databaseName = sqlite3Malloc(strlen(&(dataDir[0][3])));
	if (databaseName == NULL) {
		rc = SQLITE_NOMEM;
		goto err;
	}
	strcpy(databaseName, &(dataDir[0][3]));

	if ((ret = pBt->tablesdb->stat(pBt->tablesdb,
	    p->savepoint_txn, &stats, 0)) != 0)
		goto err;

	numDb = stats->bt_nkeys;
#ifdef BDBSQL_OMIT_LEAKCHECK
	free(stats);
#else
	sqlite3_free(stats);
#endif
	if ((filename = sqlite3Malloc(numDb * DBNAME_SIZE)) == 0) {
		rc = SQLITE_NOMEM;
		goto err;
	}

	current = filename;
	if ((ret = pBt->tablesdb->cursor(pBt->tablesdb, p->savepoint_txn,
	    &dbc, 0)) != 0)
		goto err;
	while ((ret = dbc->get(dbc, &key, &data, DB_NEXT)) == 0) {
		memcpy(current, key.data, key.size);
		current[key.size] = '\0';
		current += DBNAME_SIZE;
	}
	if (ret != DB_NOTFOUND)
		goto err;
	dbc->close(dbc);
	dbc = NULL;
	ret = 0;

	/*
	 * Close the Btree and BtShared, but keep the environment
	 * around.
	 */
	pBt->dbenv = NULL;
	if ((rc = sqlite3BtreeClose(p)) != SQLITE_OK)
		goto err;
	pBt = NULL;
	p = NULL;

	current = filename;
	for (i = 0; i < numDb; i++) {
		if (strcmp(current, "metadb") == 0)
			ret = pEnv->dbremove(pEnv, NULL, BDBSQL_META_DATA_TABLE,
			    NULL, DB_FORCE);
		else
			ret = pEnv->dbremove(pEnv, NULL, current,
			    NULL, DB_FORCE);
		if (ret != 0)
			goto err;
		current += DBNAME_SIZE;
	}

#else
	ret = pBt->metadb->get_dbname(pBt->metadb, (const char **)&tmp,
	    (const char **)&tablename);
	if (ret != 0)
		goto err;
	if ((filename = sqlite3Malloc(strlen(tmp) + 1)) == NULL) {
		rc = SQLITE_NOMEM;
		goto err;
	}
	strcpy(filename, tmp);
	databaseName = filename;

	/*
	 * Close the Btree and BtShared, but keep the environment
	 * around.
	 */
	pBt->dbenv = NULL;
	db = p->db;
	for (iDb = 0; iDb < db->nDb; iDb++) {
		if (db->aDb[iDb].pBt == p)
			break;
	}
	if ((rc = sqlite3BtreeClose(p)) != SQLITE_OK)
		goto err;
	pBt = NULL;
	p = NULL;
	db->aDb[iDb].pBt = NULL;

	ret = pEnv->dbremove(pEnv, NULL, filename, NULL, DB_FORCE);
	if (ret != 0)
		goto err;

#endif

	if ((ret = pEnv->get_home(pEnv, (const char **)&tmp)) != 0)
		goto err;
	*home = sqlite3Malloc(strlen(tmp) + 1);
	if (*home == NULL) {
		rc = SQLITE_NOMEM;
		goto err;
	}
	strcpy(*home, tmp);

	if ((ret = pEnv->close(pEnv, 0)) != 0)
		goto err;
	pEnv = NULL;

	if ((rc = btreeCleanupEnv(*home)) != SQLITE_OK)
		goto err;

	sqlite3_free(*home);
	*home = databaseName;
	databaseName = NULL;
	goto done;

err:	if (*home != NULL) {
		sqlite3_free(*home);
		*home = NULL;
	}
done:	if (databaseName != NULL)
		sqlite3_free(databaseName);
	if (pEnv != NULL && pBt == NULL)
		pEnv->close(pEnv, 0);
 #ifdef BDBSQL_FILE_PER_TABLE
	 if (filename != NULL)
		 sqlite3_free(filename);
	 if (dbc != NULL)
		 dbc->close(dbc);
#endif

	return MAP_ERR(rc, ret);
}
#endif /* SQLITE_HAS_CODEC */

/*
** Copy nPage pages from the source b-tree to the destination.
*/
int sqlite3_backup_step(sqlite3_backup *p, int nPage) {
	int returnCode, pages;
	Parse parse;
	DB_ENV *dbenv;
	BtShared *pBtDest, *pBtSrc;

	pBtDest = pBtSrc = NULL;

	if (p->rc != SQLITE_OK || nPage == 0)
		return p->rc;

	sqlite3_mutex_enter(p->pSrcDb->mutex);
	sqlite3_mutex_enter(p->pDestDb->mutex);

	/*
	 * Make sure the schema has been read in, so the keyInfo
	 * can be retrieved for the indexes.  No-op if already read.
	 * If the schema has not been read then an update must have
	 * changed it, so backup will restart.
	 */
	memset(&parse, 0, sizeof parse);
	parse.db = p->pSrcDb;
	p->rc = sqlite3ReadSchema(&parse);
	if (p->rc != SQLITE_OK)
		goto err;

	/*
	 * This process updated the source database, so
	 * the backup process has to restart.
	 */
	if (p->pSrc->updateDuringBackup > p->lastUpdate) {
		p->rc = SQLITE_LOCKED;
		if ((p->rc = backupCleanup(p)) != SQLITE_OK)
			goto err;
		else
			backupReset(p);
	}

	pages = nPage;

#ifdef SQLITE_HAS_CODEC
	/*
	 * The destination database must be encrypted with the
	 * same key as the source database.
	 */
	pBtDest = p->pDest->pBt;
	/*
	 * In the case of a temporary source database, use the
	 * encryption of the main database.
	 */
	if (strcmp(p->srcName, "temp") == 0) {
		int iDb = sqlite3FindDbName(p->pSrcDb, "main");
		pBtSrc = p->pSrcDb->aDb[iDb].pBt->pBt;
	} else
		pBtSrc = p->pSrc->pBt;
#ifdef BDBSQL_FILE_PER_TABLE
	if (pBtDest->dbStorage == DB_STORE_NAMED &&
	    (pBtSrc->encrypted || pBtDest->encrypted) && !p->cleaned) {
#else
	if (pBtDest->dbStorage == DB_STORE_NAMED &&
	    pBtSrc->encrypted &&
	    (!pBtDest->encrypted ||
	    (strcmp(pBtSrc->encrypt_pwd, pBtDest->encrypt_pwd) != 0))) {
#endif
		char *home;
		Schema *schema;

		/* Close the btree in cleanup if it was opened here. */
		p->openDest = 1;
		/* Deleting the schema causes problems. */
		schema = p->pDestDb->aDb[p->iDb].pSchema;
		p->pDest->schema = NULL;
		p->rc = btreeDeleteEnvironment(p->pDest, &home);
		p->pDest = p->pDestDb->aDb[p->iDb].pBt;
		p->pDestDb->aDb[p->iDb].pSchema = schema;
		if (p->rc != SQLITE_OK)
			goto err;
		p->rc = sqlite3BtreeFactory(p->pDestDb, home, 0,
		    SQLITE_DEFAULT_CACHE_SIZE,
		    p->pDestDb->openFlags,
		    &p->pDest);
		sqlite3_free(home);
		p->pDestDb->aDb[p->iDb].pBt = p->pDest;
		if (p->rc == SQLITE_OK) {
			if (p->iDb == 0)
				p->rc = sqlite3_key(p->pDestDb,
				    pBtSrc->encrypt_pwd,
				    pBtSrc->encrypt_pwd_len);
			else
				p->rc = sqlite3CodecAttach(p->pDestDb, p->iDb,
				    pBtSrc->encrypt_pwd,
				    pBtSrc->encrypt_pwd_len);
		}
		if (p->rc != SQLITE_OK)
			goto err;
		p->cleaned = 1;
	}
#endif

	/*
	 * Begin a transaction or savepoint, unfortuantely the lock on
	 * the schema has to be released to allow the sqlite_master
	 * table to be cleared, which could allow another thread to
	 * alter it, however accessing the backup database during
	 * backup is already an illegal condition with undefined
	 * results.
	 */
	if (!p->inDestTxn) {
		if (!sqlite3BtreeIsInTrans(p->pDest)) {
			if (!p->pDest->connected) {
				p->rc = btreeOpenEnvironment(p->pDest, 1);
				if (p->rc != SQLITE_OK)
					goto err;
			}
			if ((p->rc = sqlite3BtreeBeginTrans(p->pDest, 2))
				!= SQLITE_OK)
				goto err;
			p->inDestTxn++;
		}
		if ((p->rc = sqlite3BtreeBeginStmt(p->pDest,
		    p->pDest->nSavepoint)) != SQLITE_OK)
			goto err;
		p->inDestTxn++;
	}
	/* Only this process should be accessing the backup environment. */
	if (p->pDest->pBt->nRef > 1) {
		p->rc = SQLITE_BUSY;
		goto err;
	}

	/*
	 * Begin a transaction, a lock error or update could have caused
	 * it to be released in a previous call to step.
	 */
	if (!p->srcTxn) {
		dbenv = p->pSrc->pBt->dbenv;
		if ((p->rc = dberr2sqlite(dbenv->txn_begin(dbenv,
		    p->pSrc->family_txn, &p->srcTxn, 0))) != SQLITE_OK)
			goto err;
	}

	/*
	 * An update could have dropped or created a table, so recalculate
	 * the list of tables.
	 */
	if (!p->tables) {
		if ((p->rc = btreeGetPageCount(p->pSrc,
		    &p->tables, &p->nPagecount, p->srcTxn)) != SQLITE_OK) {
				sqlite3Error(p->pSrcDb, p->rc, 0);
				goto err;
		}
		p->nRemaining = p->nPagecount;
	}

	/* Delete the old contents of the backup environment. */
	if (!p->cleaned) {
		if ((p->rc = btreeCleanDatabase(p->pDest))
			!= SQLITE_OK)
			goto err;
		p->cleaned = 1;
	}

	/* Copy the pages. */
	p->rc = btreeCopyPages(p, &pages);
	if (p->rc == SQLITE_DONE) {
		p->nRemaining = 0;
		sqlite3ResetInternalSchema(p->pDestDb, p->iDb);
		memset(&parse, 0, sizeof parse);
		parse.db = p->pDestDb;
		p->rc = sqlite3ReadSchema(&parse);
		if (p->rc == SQLITE_OK)
			p->rc = SQLITE_DONE;
	} else if (p->rc != SQLITE_OK)
		goto err;

	/*
	 * The number of pages left to copy is an estimate, so
	 * do not let the number go to zero unless we are really
	 * done.
	 */
	if (p->rc != SQLITE_DONE) {
		if ((u32)pages >= p->nRemaining)
			p->nRemaining = 1;
		else
			p->nRemaining -= pages;
	}

err:	/*
	 * This process updated the source database, so
	 * the backup process has to restart.
	 */
	if (p->pSrc->updateDuringBackup > p->lastUpdate &&
	    (p->rc == SQLITE_OK || p->rc == SQLITE_DONE)) {
		int cleanCode;
		returnCode = p->rc;
		p->rc = SQLITE_LOCKED;
		if ((cleanCode = backupCleanup(p)) != SQLITE_OK)
			returnCode = p->rc = cleanCode;
		else
			backupReset(p);
	} else {
		returnCode = backupCleanup(p);
		if (returnCode == SQLITE_OK ||
		    (p->rc != SQLITE_OK && p->rc != SQLITE_DONE))
			returnCode = p->rc;
		else
			p->rc = returnCode;
	}
	/*
	 * On a locked or busy error the backup process is rolled back,
	 * but can be restarted by the user.
	 */
	if ( returnCode == SQLITE_LOCKED || returnCode == SQLITE_BUSY )
		backupReset(p);
	else if ( returnCode != SQLITE_OK && returnCode != SQLITE_DONE ) {
		sqlite3Error(p->pDestDb, p->rc, 0);
	}
	sqlite3_mutex_leave(p->pDestDb->mutex);
	sqlite3_mutex_leave(p->pSrcDb->mutex);
	return (returnCode);
}

/*
** Release all resources associated with an sqlite3_backup* handle.
*/
int sqlite3_backup_finish(sqlite3_backup *p) {
	sqlite3_mutex *mutex;

	if (!p || !p->pSrcDb || !p->pDestDb)
		return SQLITE_OK;
	sqlite3_mutex_enter(p->pSrcDb->mutex);
	mutex = p->pSrcDb->mutex;
	/*
	 * Cleanup was already done on error or SQLITE_DONE, only
	 * cleanup if quiting before finishing backup.
	 */
	if (p->rc == SQLITE_OK) {
		p->rc = SQLITE_ERROR;
		backupCleanup(p);
	}
	sqlite3_free(p);
	sqlite3_mutex_leave(mutex);
	return SQLITE_OK;
}

/*
** Return the number of pages still to be backed up as of the most recent
** call to sqlite3_backup_step().
*/
int sqlite3_backup_remaining(sqlite3_backup *p) {
	return p->nRemaining;
}

/*
** Return the total number of pages in the source database as of the most
** recent call to sqlite3_backup_step().
*/
int sqlite3_backup_pagecount(sqlite3_backup *p) {
	return (int)p->nPagecount;
}

/*
 * Deletes all the user created tables and clears the sqlite_master.
 */
static int btreeCleanDatabase(Btree *p)
{
	int rc, rc2, iTable, tmp, i;
	char *sql;
	BtShared *pBt;
	int *tables;

	rc = rc2 = SQLITE_OK;
	tables = NULL;
	sql = NULL;
	pBt = p->pBt;

	if (!p->connected) {
		if ((rc = btreeOpenEnvironment(p, 1)) != SQLITE_OK)
			goto err;
		/* The btreeOpenEnvironment call might have updated pBt. */
		pBt = p->pBt;
	}

	if ((rc = btreeGetTables(p, &tables, p->savepoint_txn))
		!= SQLITE_OK)
		goto err;

	/* Delete all data tables. */
	i = 0;
	while (tables[i] > -1) {
		iTable = tables[i];
		if (iTable > MASTER_ROOT) {
			if (p->pBt->dbStorage != DB_STORE_NAMED) {
				/* We do not want to remove the table
				 * from the environment, which is what
				 * drop table does for in memory db, we
				 * just want to clear it of data.
				 */
				if ((rc = sqlite3BtreeClearTable(p, iTable, 0)
					!= SQLITE_OK))
					goto err;
			} else {
				if ((rc = sqlite3BtreeDropTable(p, iTable, &tmp)
					!= SQLITE_OK))
					goto err;
			}
		}
		i++;
	}
	/* Clear the master root table, remove the lock on it first. */
	tmp = p->schemaLockMode;
	if ((rc = btreeLockSchema(p, LOCKMODE_NONE) != SQLITE_OK))
		goto err;
	rc = sqlite3BtreeClearTable(p, MASTER_ROOT, 0);
	rc2 = btreeLockSchema(p, tmp);

	if (rc == SQLITE_OK && rc2 != SQLITE_OK)
		rc = rc2;

err:	if (tables)
		sqlite3_free(tables);

	return rc;
}

/*
 * Use Bulk Get/Put to copy the given number of pages worth of
 * records from the source database to the destination database,
 * this function should be called until all tables are copied, at
 * which point it will return SQLITE_DONE.  Both Btrees need to
 * have transactions before calling this function.
 * p->pSrc - Source Btree
 * p->tables - Contains a list of iTables to copy, gotten using
 *          btreeGetTables().
 * p->currentTable - Index in tables of the current table being copied.
 * p->srcCur -  Cursor on the current source table being copied.
 * p->pDest - Destiniation Btree.
 * p->destCur - BtCursor on the destination table being copied into.
 * pages - Number of pages worth of data to copy.
 */
static int btreeCopyPages(sqlite3_backup *p, int *pages)
{
	DB *dbp;
	DBT dataOut, dataIn;
	char bufOut[MULTI_BUFSIZE], bufIn[MULTI_BUFSIZE];
	int ret, rc, copied, srcIsDupIndex;
	void *in, *out, *app;

	ret = 0;
	rc = SQLITE_OK;
	dbp = NULL;
	copied = 0;
	memset(&dataOut, 0, sizeof dataOut);
	memset(&dataIn, 0, sizeof dataIn);
	dataOut.flags = DB_DBT_USERMEM;
	dataIn.flags = DB_DBT_USERMEM;
	dataOut.data = bufOut;
	dataOut.ulen = sizeof bufOut;
	dataIn.data = bufIn;
	dataIn.ulen = sizeof bufIn;

	while (*pages < 0 || *pages > copied) {
		/* No tables left to copy */
		if (p->tables[p->currentTable] == -1) {
			u32 val;
			/*
			 * Update the schema file format and largest rootpage
			 * in the meta data.  Other meta data values should
			 * not be changed.
			 */
			sqlite3BtreeGetMeta(p->pSrc, 1, &val);
			if (p->pSrc->db->errCode == SQLITE_BUSY) {
				rc = SQLITE_BUSY;
				goto err;
			}
			rc = sqlite3BtreeUpdateMeta(p->pDest, 1, val);
			if (rc != SQLITE_OK)
				goto err;
			sqlite3BtreeGetMeta(p->pSrc, 3, &val);
                       if (p->pSrc->db->errCode == SQLITE_BUSY) {
				rc = SQLITE_BUSY;
				goto err;
			}
			rc = sqlite3BtreeUpdateMeta(p->pDest, 3, val);
			if (rc != SQLITE_OK)
				goto err;
			ret = SQLITE_DONE;
			goto err;
		}
		/* If not currently copying a table, get the next table. */
		if (!p->srcCur) {
			rc = btreeGetUserTable(p->pSrc, p->srcTxn, &dbp,
			    p->tables[p->currentTable]);
			if (rc != SQLITE_OK)
				goto err;
			assert(dbp);
			memset(&p->destCur, 0, sizeof p->destCur);
			/*
			 * Open a cursor on the destination table, this will
			 * create the table and allow the Btree to manage the
			 * DB object.
			 */
			sqlite3BtreeCursor(p->pDest, p->tables[p->currentTable],
			    1, dbp->app_private, &p->destCur);
			if ((rc = p->destCur.error) != SQLITE_OK) {
				app = dbp->app_private;
				dbp->close(dbp, DB_NOSYNC);
				if (app)
					sqlite3DbFree(p->pSrcDb, app);
				goto err;
			}
			/* Open a cursor on the source table. */
			if ((ret = dbp->cursor(dbp,
			    p->srcTxn, &p->srcCur, 0)) != 0)
				goto err;
			dbp = 0;
		}
		srcIsDupIndex = isDupIndex((p->tables[p->currentTable] & 1) ?
		    BTREE_INTKEY : 0, p->pSrc->pBt->dbStorage,
		    p->srcCur->dbp->app_private, p->srcCur->dbp);
		/*
		 * Copy the current table until the given number of
		 * pages is copied, or the entire table has been copied.
		 */
		while (*pages < 0 || *pages > copied) {
			DBT key, data;
			memset(&key, 0, sizeof key);
			memset(&data, 0, sizeof data);
			/* Do a Bulk Get from the source table. */
			ret = p->srcCur->get(p->srcCur, &key, &dataOut,
			    DB_NEXT | DB_MULTIPLE_KEY);
			if (ret == DB_NOTFOUND)
				break;
			if (ret != 0)
				goto err;
			/* Copy the records into the Bulk buffer. */
			DB_MULTIPLE_INIT(out, &dataOut);
			DB_MULTIPLE_WRITE_INIT(in, &dataIn);
			DB_MULTIPLE_KEY_NEXT(out, &dataOut, key.data,
			    key.size, data.data, data.size);
			while (out) {
				/*
				 * Have to translate the index formats if they
				 * are not the same.
				 */
				if (p->destCur.isDupIndex != srcIsDupIndex) {
					if (srcIsDupIndex) {
						p->destCur.key = key;
						p->destCur.data = data;
						if (!btreeCreateIndexKey(
						    &p->destCur)) {
							rc = SQLITE_NOMEM;
							goto err;
						}
						DB_MULTIPLE_KEY_WRITE_NEXT(in,
						    &dataIn,
						    p->destCur.index.data,
						    p->destCur.index.size,
						    p->destCur.data.data, 0);
					} else {
						/* Copy the key into the cursor
						 * index since spliting the key
						 * requires changing the
						 * internal memory.
						 */
						if (!allocateCursorIndex(
						    &p->destCur, key.size)) {
							rc = SQLITE_NOMEM;
							goto err;
						}
						memcpy(p->destCur.index.data,
						    key.data, key.size);
						p->destCur.index.size =
						    key.size;
						p->destCur.key.data =
						    p->destCur.index.data;
						p->destCur.key.size =
						    p->destCur.index.size;
						splitIndexKey(&p->destCur);
						DB_MULTIPLE_KEY_WRITE_NEXT(
						    in, &dataIn,
						    p->destCur.key.data,
						    p->destCur.key.size,
						    p->destCur.data.data,
						    p->destCur.data.size);
					}
				} else
					DB_MULTIPLE_KEY_WRITE_NEXT(in, &dataIn,
					    key.data, key.size,
					    data.data, data.size);
				DB_MULTIPLE_KEY_NEXT(out, &dataOut,
				    key.data, key.size,
				    data.data, data.size);
			}
			/* Insert into the destination table. */
			dbp = p->destCur.cached_db->dbp;
			if ((ret = dbp->put(dbp, p->pDest->savepoint_txn,
			    &dataIn, 0, DB_MULTIPLE_KEY)) != 0)
				goto err;
			dbp = NULL;
			copied += MULTI_BUFSIZE/SQLITE_DEFAULT_PAGE_SIZE;
		}
		/*
		 * Done copying the current table, time to look for a new
		 * table to copy.
		 */
		if (ret == DB_NOTFOUND) {
			ret = 0;
			rc = sqlite3BtreeCloseCursor(&p->destCur);
			if (p->srcCur) {
				app = p->srcCur->dbp->app_private;
				dbp = p->srcCur->dbp;
				p->srcCur->close(p->srcCur);
				ret = dbp->close(dbp, DB_NOSYNC);
				if (app)
					sqlite3DbFree(p->pSrcDb, app);
			}
			p->srcCur = NULL;
			if (ret != 0 || rc != SQLITE_OK)
				goto err;
			p->currentTable += 1;
		}
	}
	goto done;
err:	if (ret == SQLITE_DONE)
		return ret;
done:	return MAP_ERR(rc, ret);
}

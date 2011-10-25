/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 */

/*
** This file implements the Berkeley DB specific pragmas.
 */
#include "sqliteInt.h"
#include "btreeInt.h"

extern void returnSingleInt(Parse *, const char *, i64);
extern u8 getBoolean(const char *);

/*
 * Parse all Berkeley DB specific pragma options.
 *
 * Return non zero to indicate the pragma parsing should continue, 0 if the
 * pragma has been fully processed.
 */
int bdbsqlPragma(Parse *pParse, char *zLeft, char *zRight, int iDb)
{
	Btree *pBt;
	Db *pDb;
	sqlite3 *db;
	int parsed;

	db = pParse->db;
	pDb = &db->aDb[iDb];
	if (pDb != NULL)
		pBt = pDb->pBt;
	else
		pBt = NULL;

	parsed = 0;
	/* Deal with the Berkeley DB specific autodetect page_size setting. */
	if (sqlite3StrNICmp(zLeft, "page_size", 9) == 0 && zRight != 0) {
		int n = sqlite3Strlen30(zRight);
		if (pBt != NULL &&
		    sqlite3StrNICmp(zRight, "autodetect", n) == 0) {
			if (SQLITE_NOMEM ==
			    sqlite3BtreeSetPageSize(pBt, 0, -1, 0))
				db->mallocFailed = 1;
			parsed = 1;
		}
	} else if (sqlite3StrNICmp(zLeft, "txn_bulk", 8) == 0) {
		if (zRight)
			pBt->txn_bulk = getBoolean(zRight);

		returnSingleInt(pParse, "txn_bulk", (i64)pBt->txn_bulk);
		parsed = 1;
	/* 
	 * This pragma is used internally for testing deadlock.  It sets
	 * the priority of a transaction so it will not loose its locks 
	 * when deadlock occurs with a non-exclusive transaction.
	 * txn_priority=#       sets the transaction priority of all 
	 * non-exclusive transactions in this session to #
	 */
	} else if (sqlite3StrNICmp(zLeft, "txn_priority", 12) == 0) {
		u_int32_t priority, excl_priority;

		priority = 0;
		excl_priority = -1;
		if (pDb->pBt->savepoint_txn != NULL) {
			if (zRight) {
				/* 
				 * Txn priority must be less than 
				 * the priority given to exclusive
				 * transactions.
				 */
				if (sqlite3GetInt32(zRight, &priority) &&
				    priority < excl_priority) 
					pDb->pBt->txn_priority = priority;
				else 
					sqlite3ErrorMsg(pParse,
					    "Invalid transaction priority %s,"
					    " must be a number.", zRight);
			}
		}
		returnSingleInt(pParse, "txn_priority", 
		    pDb->pBt->txn_priority);
		parsed = 1;
	}
	/* Return semantics to match strcmp. */
	return (!parsed);
}


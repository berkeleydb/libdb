/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 */

/*
** This file contains code used to implement the VACUUM command.
*/
#include "sqliteInt.h"
#include "btreeInt.h"
#include "vdbeInt.h"

#if !defined(SQLITE_OMIT_VACUUM)
/*
** The non-standard VACUUM command is used to clean up the database,
** collapse free space, etc.  It is modelled after the VACUUM command
** in PostgreSQL.
*/
void sqlite3Vacuum(Parse *pParse) {
	Vdbe *v = sqlite3GetVdbe(pParse);

	if (v)
		sqlite3VdbeAddOp2(v, OP_Vacuum, 0, 0);
}

int btreeVacuum(Btree *p, char **pzErrMsg) {
	sqlite3 *db;
	int rc;
	u_int32_t truncatedPages;

	db = p->db;

	/* Return directly if vacuum is on progress */
	if (p->inVacuum)
		return SQLITE_OK;

	/*
	 * We're going to do updates in this transaction at the Berkeley DB
	 * Core level (i.e., call DB->compact), but we start it read-only at
	 * the SQL level to avoid overhead from checkpoint-on-commit.
	 */
	if ((rc = sqlite3BtreeBeginTrans(p, 0)) != SQLITE_OK) {
		sqlite3SetString(pzErrMsg, db,
		    "failed to begin a vacuum transaction");
		return rc;
	}

	p->inVacuum = 1;

	truncatedPages = 0;
	/* Go through all tables */
	do {
		rc = btreeIncrVacuum(p, &truncatedPages);
	} while (rc == SQLITE_OK);
	p->needVacuum = 0;

	if (rc != SQLITE_DONE) {
		sqlite3SetString(pzErrMsg, db,
		    "error during vacuum, rolled back");
		(void)sqlite3BtreeRollback(p);
	} else if ((rc = sqlite3BtreeCommit(p)) != SQLITE_OK) {
		sqlite3SetString(pzErrMsg, db,
		    "failed to commit the vacuum transaction");
	}

	p->inVacuum = 0;

	return rc;
}

/*
** This routine implements the OP_Vacuum opcode of the VDBE.
*/
int sqlite3RunVacuum(char **pzErrMsg, sqlite3 *db) {
	int rc;
	Btree *p;

	p = db->aDb[0].pBt;
	rc = SQLITE_OK;

	if (p->pBt->dbStorage != DB_STORE_NAMED)
		return SQLITE_OK;

	if ((rc = sqlite3Init(db, pzErrMsg)) != SQLITE_OK)
		return rc;

	if (!db->autoCommit) {
		sqlite3SetString(pzErrMsg, db,
		    "cannot VACUUM from within a transaction");
		return SQLITE_ERROR;
	}

	assert(sqlite3_mutex_held(db->mutex));
	rc = btreeVacuum(p, pzErrMsg);

	return rc;
}
#endif  /* SQLITE_OMIT_VACUUM */

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2010 Oracle and/or its affiliates.  All rights reserved.
 */

/*
** This file contains code used to implement / stub the VACUUM command.
*/
#include "sqliteInt.h"
#include "vdbeInt.h"

#if !defined(SQLITE_OMIT_VACUUM) && !defined(SQLITE_OMIT_ATTACH)
/*
** The non-standard VACUUM command is used to clean up the database,
** collapse free space, etc.  It is modelled after the VACUUM command
** in PostgreSQL.
*/
void sqlite3Vacuum(Parse *pParse) {
  Vdbe *v = sqlite3GetVdbe(pParse);
  if (v) {
    sqlite3VdbeAddOp2(v, OP_Vacuum, 0, 0);
  }
  return;
}

/*
** This routine implements the OP_Vacuum opcode of the VDBE.
*/
int sqlite3RunVacuum(char **pzErrMsg, sqlite3 *db) {
  int rc = SQLITE_OK;     /* Return code from service routines */

  if (!db->autoCommit) {
    sqlite3SetString(pzErrMsg, db, "cannot VACUUM from within a transaction");
    return SQLITE_ERROR;
  }

  return rc;
}
#endif  /* SQLITE_OMIT_VACUUM && SQLITE_OMIT_ATTACH */

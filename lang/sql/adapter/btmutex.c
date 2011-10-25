/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 */

/*
** This file contains code used to implement mutexes on Btree objects.
** This code really belongs in btree.c.  But btree.c is getting too
** big and we want to break it down some.  This packaged seemed like
** a good breakout.
*/
#include "btreeInt.h"
#ifndef SQLITE_OMIT_SHARED_CACHE
/*
 * Berkeley DB does fine grained locking - disable the SQLite table level
 * locking.
 */
void sqlite3BtreeEnter(Btree *p)
{
}

void sqlite3BtreeLeave(Btree *p)
{
}

void sqlite3BtreeEnterCursor(BtCursor *pCur)
{
}

void sqlite3BtreeLeaveCursor(BtCursor *pCur)
{
}

void sqlite3BtreeEnterAll(sqlite3 *db)
{
}

void sqlite3BtreeLeaveAll(sqlite3 *db)
{
}

int sqlite3BtreeHoldsMutex(Btree *db)
{
	log_msg(LOG_VERBOSE, "sqlite3BtreeHoldsMutex(%p)", db);
	return 1;
}

void sqlite3BtreeMutexArrayEnter(BtreeMutexArray *pArray)
{
}

void sqlite3BtreeMutexArrayLeave(BtreeMutexArray *pArray)
{
}

int sqlite3BtreeHoldsAllMutexes(sqlite3 *db)
{
	log_msg(LOG_VERBOSE, "sqlite3BtreeHoldsAllMutexes(%p)", db);
	return 1;
}

void sqlite3BtreeMutexArrayInsert(BtreeMutexArray *pArray, Btree *pBtree)
{
}
#endif /* ifndef SQLITE_OMIT_SHARED_CACHE */

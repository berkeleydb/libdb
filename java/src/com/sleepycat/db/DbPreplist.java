/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999-2001
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: DbPreplist.java,v 11.2 2001/05/16 13:09:30 bostic Exp $
 */

package com.sleepycat.db;

/*
 * This is filled in and returned by the
 * DbEnv.txn_recover() method.
 */
public class DbPreplist
{
    public DbTxn txn;
    public byte gid[];
}

// end of DbPreplist.java

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999-2001
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: DbDeadlockException.java,v 11.5 2001/05/16 13:09:27 bostic Exp $
 */

package com.sleepycat.db;

public class DbDeadlockException extends DbException
{
    // methods
    //

    public DbDeadlockException(String s)
    {
        super(s);
    }

    public DbDeadlockException(String s, int errno)
    {
        super(s, errno);
    }
}

// end of DbDeadlockException.java

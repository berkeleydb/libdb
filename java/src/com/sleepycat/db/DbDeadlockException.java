/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbDeadlockException.java	11.1 (Sleepycat) 7/25/99
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

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2001
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id: DbRunRecoveryException.java,v 11.5 2001/05/16 13:09:31 bostic Exp $
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public class DbRunRecoveryException extends DbException
{
    // methods
    //

    public DbRunRecoveryException(String s)
    {
        super(s);
    }

    public DbRunRecoveryException(String s, int errno)
    {
        super(s, errno);
    }
}

// end of DbRunRecoveryException.java

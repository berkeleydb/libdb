/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 *
 *	$Id: DbLsn.java,v 11.4 2000/05/25 04:18:13 dda Exp $
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public class DbLsn
{
    // methods
    //
    DbLsn()
    {
        init_lsn();
    }

    protected native void finalize()
         throws Throwable;

    private native void init_lsn();

    // get/set methods
    //

    // private data
    //
    private long private_dbobj_ = 0;

    static {
        Db.load_db();
    }
}

// end of DbLsn.java

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbLsn.java	11.1 (Sleepycat) 7/25/99
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
    private long private_info_ = 0;

    static {
        Db.load_db();
    }
}

// end of DbLsn.java

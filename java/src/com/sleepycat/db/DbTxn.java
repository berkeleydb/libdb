/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbTxn.java	11.2 (Sleepycat) 7/30/99
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public class DbTxn
{
    // methods
    //
    public native void abort()
         throws DbException;

    public native void commit(int flags)
         throws DbException;

    public native /*u_int32_t*/ int id()
         throws DbException;

    public native void prepare()
         throws DbException;

    protected native void finalize()
         throws Throwable;

    // get/set methods
    //

    // private data
    //
    private long private_info_ = 0;

    static {
        Db.load_db();
    }
}

// end of DbTxn.java

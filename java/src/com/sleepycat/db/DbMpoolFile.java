/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbMpoolFile.java	10.1 (Sleepycat) 11/10/97
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public class DbMpoolFile
{
    // methods
    //
    public native void close()
         throws DbException;

    // Note: The semantics of Mpool files are a little hard
    //       to reconcile with Java, and they are not often
    //       used directly.  For now, we're leaving out these
    //       critical calls.
    //
    // public native void get(/*db_pgno_t* */ long pgnoaddr,
    //                       /*unsigned*/ int flags, byte[] data)
    //      throws DbException;

    // public native void put(/*void* */long pgaddr, /*unsigned*/ int flags)
    //      throws DbException;

    // public native void set(/*void* */long pgaddr, /*unsigned*/ int flags)
    //      throws DbException;

    public native void sync()
         throws DbException;

    public native static DbMpoolFile open(DbMpool mp, String file,
                                          int ftype, int flags, int mode,
                                          /*size_t*/ long pagesize,
                                          int lsn_offset,
                                          Dbt pgcookie,
                                          byte[] uid)
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

// end of DbMpoolFile.java

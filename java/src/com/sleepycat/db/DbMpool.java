/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbMpool.java	10.2 (Sleepycat) 11/25/97
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public class DbMpool
{
    // methods
    //
    public native void close()
         throws DbException;

    // Note: this callback is not implemented
    // public native void db_register(int ftype, DbPage pgin, DbPage pgout)
    //      throws DbException;

    // Note: DbMpool.stat and DbMpool.fstat are handled by a
    // single method call in the C/C++ APIs, but that is
    // awkward in Java.
    //
    public native DbMpoolStat stat()
         throws DbException;

    public native DbMpoolFStat[] fstat()
         throws DbException;

    public native void sync(DbLsn lsn)
         throws DbException;

    public native int trickle(int pct)
         throws DbException;

    // Create or remove new mpool files
    //
    public native static DbMpool open(String dir, int flags, int mode,
                                      DbEnv dbenv)
         throws DbException;

    public native static void unlink(String dir, int force, DbEnv dbenv)
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

// end of DbMpool.java

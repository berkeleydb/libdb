/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbEnv.java	10.6 (Sleepycat) 10/27/98
 */

package com.sleepycat.db;

import java.io.OutputStream;

/**
 *
 * @author Donald D. Anderson
 */
public class DbEnv
{
    // methods
    //

    // This constructor can be used to immediately initialize the
    // application with these arguments.  Do not use it if you
    // need to set other parameters via the access methods.
    //
    public DbEnv(String homeDir, String[] db_config, int flags)
         throws DbException
    {
        init();
        appinit(homeDir, db_config, flags);
    }

    // Use this constructor if you wish to *delay* the initialization
    // of the db library.  This is useful if you need to set
    // any particular parameters via the access methods below.
    // Then call appinit() to complete the initialization.
    //
    public DbEnv()
    {
        init();
    }

    // Used in conjunction with the default constructor to
    // complete the initialization of the db library.
    //
    public native void appinit(String homeDir, String[] db_config, int flags)
         throws DbException;

    // Can be called at any time to shut down Db.
    // Called automatically when DbEnv is GC-ed,
    // but don't rely on GC unless you turn on
    // System.runFinalizersOnExit()!
    //
    public native void appexit()
         throws DbException;

    protected native void finalize()
         throws Throwable;

    ////////////////////////////////////////////////////////////////
    // simple get/set access methods
    //
    // If you are calling set_ methods, you need to
    // use the default constructor along with appinit().

    // Byte order.
    public native void set_lorder(int lorder);

    // Error message callback.
    public native void set_errcall(DbErrcall errcall);

    // Error message file stream.
    // Note: there is no access to the underlying "errfile" field, since
    // it is a C FILE* that makes little sense in the Java world.
    // Consider using set_errcall() instead.

    // Error message prefix.
    public native void set_errpfx(String errpfx);

    // Generate debugging messages.
    public native void set_verbose(int verbose);


    ////////////////////////////////////////////////////////////////
    // Locking.

    // Return from lock_open().
    public DbLockTab get_lk_info()
    {
        return lk_info_;
    }

    // Two dimensional conflict matrix.
    // Note: set_lk_conflicts() sets the underlying array to a copy.
    // You should call set_lk_modes() when calling set_lk_conflicts().
    //
    public native void set_lk_conflicts(byte[][] lk_conflicts);

    // Number of lock modes in table.
    public native void set_lk_modes(int lk_modes);

    // Maximum number of locks.
    public native void set_lk_max(/*unsigned*/ int lk_max);

    // Deadlock detect on every conflict.
    public native void set_lk_detect(/*u_int32_t*/ int lk_detect);

    ////////////////////////////////////////////////////////////////
    // Logging.

    // Return from log_open().
    public DbLog get_lg_info()
    {
        return lg_info_;
    }

    // Maximum file size.
    public native void set_lg_max(/*u_int32_t*/ int lg_max);


    ////////////////////////////////////////////////////////////////
    // Memory pool.

    // Return from memp_open().
    public DbMpool get_mp_info()
    {
        return mp_info_;
    }

    // Maximum file size for mmap.
    public native void set_mp_mmapsize(/*size_t*/ long mmapsize);

    // Bytes in the mpool cache.
    public native void set_mp_size(/*size_t*/ long mp_size);


    ////////////////////////////////////////////////////////////////
    // Transactions.

    // Return from txn_open().
    public DbTxnMgr get_tx_info()
    {
        return tx_info_;
    }

    // Maximum number of transactions.
    public native void set_tx_max(/*unsigned*/ int tx_max);

    // Versioning information
    public native static int get_version_major();
    public native static int get_version_minor();
    public native static int get_version_patch();
    public native static String get_version_string();

    public void set_error_stream(OutputStream s)
    {
        DbOutputStreamErrcall errcall = new DbOutputStreamErrcall(s);
        set_errcall(errcall);
    }

    // private data
    //
    private long private_info_ = 0;
    private DbLockTab lk_info_ = null;
    private DbLog lg_info_ = null;
    private DbMpool mp_info_ = null;
    private DbTxnMgr tx_info_ = null;

    private native void init();

    static {
        Db.load_db();
    }
}

// end of DbEnv.java

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbEnv.java	11.5 (Sleepycat) 9/30/99
 */

package com.sleepycat.db;

import java.io.OutputStream;
import java.io.FileNotFoundException;

/**
 *
 * @author Donald D. Anderson
 */
public class DbEnv
{
    // methods
    //

    //
    // After using this constructor, set any parameters via
    // the set_* access methods below, and finally open
    // the environment by calling open().
    //
    public DbEnv(int flags)
    {
        _init(new DbOutputStreamErrcall(System.err), flags);
    }

    //
    // This constructor is purposely not public.
    // It is used internally to create a DbEnv wrapper
    // when an underlying environment already exists.
    //
    /*package*/ DbEnv(Db db)
    {
        _init_using_db(new DbOutputStreamErrcall(System.err), db);
    }

    protected native void finalize()
         throws Throwable;

    // close discards any internal memory.
    // After using close, the DbEnv can no longer be used;
    // create another one if needed.
    //
    public native void close(int flags)
         throws DbException;

    public native void err(int errcode, String message);

    public native void errx(String message);

    public native void open(String db_home, String[] db_config,
                            int flags, int mode)
         throws DbException, FileNotFoundException;

    // remove removes any files and discards any internal memory.
    // (i.e. implicitly it does a close, if the environment is open).
    // After using close, the DbEnv can no longer be used;
    // create another one if needed.
    //
    public native void remove(String db_home, String[] db_config, int flags)
         throws DbException;

    ////////////////////////////////////////////////////////////////
    // simple get/set access methods
    //
    // If you are calling set_ methods, you need to
    // use the constructor with one argument along with open().

    public native void set_cachesize(int gbytes, int bytes, int ncaches)
         throws DbException;

    // Error message callback.
    public native void set_errcall(DbErrcall errcall);

    // Error stream.
    public void set_error_stream(OutputStream s)
    {
        DbOutputStreamErrcall errcall = new DbOutputStreamErrcall(s);
        set_errcall(errcall);
    }

    // Error message prefix.
    public native void set_errpfx(String errpfx);

    // Feedback
    public void set_feedback(DbFeedback feedback)
    {
        feedback_ = feedback;
        feedback_changed(feedback);
    }

    // (Internal)
    private native void feedback_changed(DbFeedback feedback);

    // Generate debugging messages.
    public native void set_verbose(int which, int onoff)
         throws DbException;

    // Log buffer size.
    public native void set_lg_bsize(/*u_int32_t*/ int lg_max)
         throws DbException;

    // Maximum log file size.
    public native void set_lg_max(/*u_int32_t*/ int lg_max)
         throws DbException;

    // Maximum number of locks.
    public native void set_lk_conflicts(byte[][] lk_conflicts)
         throws DbException;

    // Deadlock detect on every conflict.
    public native void set_lk_detect(/*u_int32_t*/ int lk_detect)
         throws DbException;

    // Maximum number of locks.
    public native void set_lk_max(/*unsigned*/ int lk_max)
         throws DbException;

    // Maximum file size for mmap.
    public native void set_mp_mmapsize(/*size_t*/ long mmapsize)
         throws DbException;

    public native void set_mutexlocks(int mutexlocks)
         throws DbException;

    public native void set_pageyield(int pageyield)
         throws DbException;

    public void set_recovery_init(DbRecoveryInit recovery_init)
    {
        recovery_init_ = recovery_init;
        recovery_init_changed(recovery_init);
    }

    // (Internal)
    private native void recovery_init_changed(DbRecoveryInit recovery_init);

    public native void set_region_init(int region_init)
         throws DbException;

    public native void set_tas_spins(int tas_spins)
         throws DbException;

    // Maximum number of transactions.
    public native void set_tx_max(/*unsigned*/ int tx_max)
         throws DbException;

    // Versioning information
    public native static int get_version_major();
    public native static int get_version_minor();
    public native static int get_version_patch();
    public native static String get_version_string();

    // Convert DB error codes to strings
    public native static String strerror(int errcode);

    public native int lock_detect(int flags, int atype)
         throws DbException;

    public native DbLock lock_get(/*u_int32_t*/ int locker,
                                  int flags,
                                  Dbt obj,
                                  /*db_lockmode_t*/ int lock_mode)
         throws DbException;

    public native /*u_int32_t*/ int lock_id()
         throws DbException;

    public native DbLockStat lock_stat()
         throws DbException;

    public native String[] log_archive(int flags)
         throws DbException;

    public native static int log_compare(DbLsn lsn0, DbLsn lsn1);

    public native String log_file(DbLsn lsn)
         throws DbException;

    public native void log_flush(DbLsn lsn)
         throws DbException;

    public native void log_get(DbLsn lsn, Dbt data, int flags)
         throws DbException;

    public native void log_put(DbLsn lsn, Dbt data, int flags)
         throws DbException;

    public native DbLogStat log_stat()
         throws DbException;

    public native /*u_int32_t fidp*/ int log_register(Db dbp, String name)
         throws DbException;

    public native DbMpoolStat memp_stat()
         throws DbException;

    public native DbMpoolFStat[] memp_fstat()
         throws DbException;

    public native int memp_trickle(int pct)
         throws DbException;

    public native void log_unregister(/*u_int32_t*/ int fid)
         throws DbException;

    public native DbTxn txn_begin(DbTxn pid, int flags)
         throws DbException;

    public native void txn_checkpoint(int kbyte, int min)
         throws DbException;

    public native DbTxnStat txn_stat()
         throws DbException;

    ////////////////////////////////////////////////////////////////
    //
    // private data
    //
    private long private_info_ = 0;
    private DbFeedback feedback_ = null;
    private DbRecoveryInit recovery_init_ = null;

    private native void _init(DbErrcall errcall, int flags);
    private native void _init_using_db(DbErrcall errcall, Db db);

    static {
        Db.load_db();
    }

}

// end of DbEnv.java

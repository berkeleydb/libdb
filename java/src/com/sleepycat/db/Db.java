/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)Db.java	11.8 (Sleepycat) 11/4/99
 */

package com.sleepycat.db;

import java.io.OutputStream;
import java.io.FileNotFoundException;

/**
 *
 * @author Donald D. Anderson
 */
public class Db
{
    // All constant and flag values used with Db* classes are defined here.

    // Collectively, these constants are known by the name
    // "DBTYPE" in the documentation.
    //
    public static final int DB_BTREE = 1; // B+tree
    public static final int DB_HASH  = 2; // Extended Linear Hashing.
    public static final int DB_RECNO = 3; // Fixed and variable-length records.
    public static final int DB_QUEUE = 4; // Queue
    public static final int DB_UNKNOWN = 5; // Figure it out on open.

    // Flags understood by DbEnv()
    //
    // Note: DB_CXX_NO_EXCEPTIONS will have no effect in Java.
    //
    public static final int DB_CXX_NO_EXCEPTIONS; // C++: return error values

    // Flags understood by Db()
    //
    public static final int DB_XA_CREATE;  // Open in an XA environment.

    // Flags understood by Db.open(), DbEnv.open().
    //
    public static final int DB_CREATE; // O_CREAT: create file as necessary.
    public static final int DB_NOMMAP; // Don't mmap underlying file.
    public static final int DB_THREAD; // Free-thread DB package handles.

    // Flags understood by only DbEnv.open().
    //
    public static final int DB_LOCKDOWN; // Lock memory into physical core.
    public static final int DB_PRIVATE;  // DB_ENV is process local.

    //
    // Flags understood by DbEnv.txn_begin().
    //
    public static final int DB_TXN_SYNC;   // Always sync log on commit.
    public static final int DB_TXN_NOWAIT; // Do not wait for locks in this TXN.

    //
    // Flags understood by Db.open().
    //
    public static final int DB_EXCL;     // Exclusive open (O_EXCL).
    public static final int DB_RDONLY;   // Read-only (O_RDONLY).
    public static final int DB_TRUNCATE; // Discard existing DB.
    public static final int DB_UPGRADE;  // Upgrade if necessary.


    //
    // DB (user visible) error return codes.
    //
    public static final int DB_INCOMPLETE = -30999; // Sync didn't finish.
    public static final int DB_KEYEMPTY = -30998;   // The key/data pair was deleted or
                                                // was never created by the user.
    public static final int DB_KEYEXIST = -30997;   // The key/data pair already exists.
    public static final int DB_LOCK_DEADLOCK = -30996; // Locker killed to resolve deadlock.
    public static final int DB_LOCK_NOTGRANTED = -30995; // Lock unavailable, no-wait set.
    public static final int DB_NOTFOUND = -30994;   // Key/data pair not found (EOF).
    public static final int DB_OLD_VERSION = -30993;   // Out-of-date version.
    public static final int DB_RUNRECOVERY = -30992;   // Panic return.

    //
    // Flags used by DbEnv.open and DbEnv.remove.
    //
    public static final int DB_FORCE;            // Force (anything).
    public static final int DB_INIT_CDB;         // Concurrent Access Methods.
    public static final int DB_INIT_LOCK;        // Initialize locking.
    public static final int DB_INIT_LOG;         // Initialize logging.
    public static final int DB_INIT_MPOOL;       // Initialize mpool.
    public static final int DB_INIT_TXN;         // Initialize transactions.
    public static final int DB_RECOVER;          // Run normal recovery.
    public static final int DB_RECOVER_FATAL;    // Run catastrophic recovery.
    public static final int DB_SYSTEM_MEM;       // Use system-backed memory.
    public static final int DB_TXN_NOSYNC;       // Do not sync log on commit.
    public static final int DB_USE_ENVIRON;      // Use the environment.
    public static final int DB_USE_ENVIRON_ROOT; // Use the environment if root.

    //
    // Verbose flags; used for DbEnv.set_verbose
    //
    public static final int DB_VERB_CHKPOINT;  // List checkpoints.
    public static final int DB_VERB_DEADLOCK;  // Deadlock detection information.
    public static final int DB_VERB_RECOVERY;  // Recovery information.
    public static final int DB_VERB_WAITSFOR;  // Dump waits-for table.

    //
    // Deadlock detector modes; used in the DBENV structure to configure the
    // locking subsystem.
    //
    public static final int DB_LOCK_NORUN;
    public static final int DB_LOCK_DEFAULT;
    public static final int DB_LOCK_OLDEST;
    public static final int DB_LOCK_RANDOM;
    public static final int DB_LOCK_YOUNGEST;

    //
    // Values for DbInfo flags
    //
    public static final int DB_DUP;        // Btree, Hash: duplicate keys.
    public static final int DB_DUPSORT;    // Btree, Hash: duplicate keys.
    public static final int DB_RECNUM;     // Btree: record numbers.
    public static final int DB_RENUMBER;   // Recno: renumber on insert/delete.
    public static final int DB_REVSPLITOFF;// Btree: turn off reverse splits.
    public static final int DB_SNAPSHOT;   // Recno: snapshot the input.

    // Collectively, these constants are known by the name
    // "db_lockmode_t" in the documentation.
    //
    public static final int DB_LOCK_NG = 0;	// Not granted.
    public static final int DB_LOCK_READ = 1;	// Shared/read.
    public static final int DB_LOCK_WRITE = 2;	// Exclusive/write.
    public static final int DB_LOCK_IWRITE = 3;	// Intent exclusive/write.
    public static final int DB_LOCK_IREAD = 4;	// Intent to share/read.
    public static final int DB_LOCK_IWR = 5;	// Intent to read and write.

    // Collectively, these constants are known by the name
    // "db_lockop_t" in the documentation.
    //
    public static final int DB_LOCK_DUMP = 0;	// Display held locks.
    public static final int DB_LOCK_GET = 1;	// Get the lock.
    /* Not visible to API:  DB_LOCK_INHERIT = 2 // Pass locks to parent. */
    public static final int DB_LOCK_PUT = 3;	// Release the lock.
    public static final int DB_LOCK_PUT_ALL = 4;// Release locker's locks.
    public static final int DB_LOCK_PUT_OBJ = 5;// Release locker's locks on obj.

    // Flag values for DbLock.vec()
    public static final int DB_LOCK_NOWAIT; // Don't wait on unavailable lock.

    // Flag values for DbLock.detect()
    public static final int DB_LOCK_CONFLICT; // Run on any conflict.

    // Size of commonly used conflict matrices.
    //
    //
    public static final int DB_LOCK_RW_N; // standard R/W (or exclusive/shared)

    //
    // Flag values for DbLog.archive()
    //
    public static final int DB_ARCH_ABS;      // Absolute pathnames.
    public static final int DB_ARCH_DATA;     // Data files.
    public static final int DB_ARCH_LOG;      // Log files.

    //
    // DB access method and cursor operation values.
    // Each value is an operation code to which
    // additional bit flags are added.
    //
    public static final int DB_AFTER;      // Dbc.put()
    public static final int DB_APPEND;     // Db.put()
    public static final int DB_BEFORE;     // Dbc.put()
    public static final int DB_CHECKPOINT; // DbLog.put(), DbLog.get()
    public static final int DB_CONSUME;    // Dbc.get()
    public static final int DB_CURLSN;     // DbLog.put()
    public static final int DB_CURRENT;    // Dbc.get(), Dbc.put(), DbLog.get()
    public static final int DB_FIRST;      // Dbc.get(), DbLog.get()
    public static final int DB_FLUSH;      // DbLog.put()
    public static final int DB_GET_BOTH;   // Db.get(), Dbc.get()
    public static final int DB_GET_RECNO;  // Dbc.get()
    public static final int DB_JOIN_ITEM;  // Dbc.get()
    public static final int DB_KEYFIRST;   // Dbc.put()
    public static final int DB_KEYLAST;    // Dbc.put()
    public static final int DB_LAST;       // Dbc.get(), DbLog.get()
    public static final int DB_NEXT;       // Dbc.get(), DbLog.get()
    public static final int DB_NEXT_DUP;   // Dbc.get()
    public static final int DB_NEXT_NODUP; // Dbc.get()
    public static final int DB_NOOVERWRITE;// Db.put()
    public static final int DB_NOSYNC;     // Db.close()
    public static final int DB_POSITION;   // Dbc.dup()
    public static final int DB_PREV;       // Dbc.get(), DbLog.get()
    public static final int DB_RECORDCOUNT;// Db.stat()
    public static final int DB_SET;        // Dbc.get(), DbLog.get()
    public static final int DB_SET_RANGE;  // Dbc.get()
    public static final int DB_SET_RECNO;  // Dbc.get()
    public static final int DB_WRITECURSOR;// Db.cursor()

    // Other flags that can be added to an operation codes above.
    //
    public static final int DB_RMW;        // Acquire write flag immediately.

    // Collectively, these values are used for Dbt flags
    //
    // Return in allocated memory.
    public static final int DB_DBT_MALLOC;

    // Partial put/get.
    public static final int DB_DBT_PARTIAL;

    // Return in realloc'd memory.
    public static final int DB_DBT_REALLOC;

    // Return in user's memory.
    public static final int DB_DBT_USERMEM;



    // Note: the env can be null
    //
    public Db(DbEnv env, int flags)
         throws DbException
    {
        dbenv_ = env;
        _init(env, flags);
        if (env == null) {
            dbenv_ = new DbEnv(this);
        }
    }

    private native void _init(DbEnv env, int flags)
         throws DbException;

    // methods
    //

    public native void close(int flags)
         throws DbException;

    public native Dbc cursor(DbTxn txnid, int flags)
         throws DbException;

    public native void del(DbTxn txnid, Dbt key, int flags)
         throws DbException;

    public native void err(int errcode, String message);

    public native void errx(String message);

    public native int fd()
         throws DbException;

    // returns: 0, DB_NOTFOUND, or throws error
    public native int get(DbTxn txnid, Dbt key, Dbt data, int flags)
         throws DbException;

    public native boolean get_byteswapped();

    public native /*DBTYPE*/ int get_type();

    public native Dbc join(Dbc curslist[], int flags)
         throws DbException;

    public native void open(String name,  String subname,
                            /*DBTYPE*/ int type,
                            int flags, int mode)
         throws DbException, FileNotFoundException;

    // returns: 0, DB_KEYEXIST, or throws error
    public native int put(DbTxn txnid, Dbt key, Dbt data, int flags)
         throws DbException;

    public native void remove(String name, String subname, int flags)
         throws DbException;

    // Note: this callback is not implemented
    // Comparison function.
    // public native void set_bt_compare(DbBtreeCompare bt_compare);

    // Maximum keys per page.
    public native void set_bt_maxkey(int maxkey)
        throws DbException;

    // Minimum keys per page.
    public native void set_bt_minkey(int minkey)
        throws DbException;

    // Note: this callback is not implemented
    // Prefix function.
    // public native void set_bt_prefix(DbBtreePrefix bt_prefix);

    // Set cache size
    public native void set_cachesize(int gbytes, int bytes, int ncaches)
        throws DbException;

    // Note: this callback is not implemented
    // Duplication resolution
    // public native void set_dup_compare(DbDupCompare dup_compare);

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

    // Flags.
    public native void set_flags(/*u_int32_t*/ int flags);

    // Fill factor.
    public native void set_h_ffactor(/*unsigned*/ int h_ffactor);

    // Note: this callback is not implemented
    // Hash function.
    // public native void set_h_hash(DbHash h_hash);

    // Number of elements.
    public native void set_h_nelem(/*unsigned*/ int h_nelem);

    // Byte order.
    public native void set_lorder(int lorder);

    // Underlying page size.
    public native void set_pagesize(/*size_t*/ long pagesize);

    // Variable-length delimiting byte.
    public native void set_re_delim(int re_delim);

    // Length for fixed-length records.
    public native void set_re_len(/*u_int32_t*/ int re_len);

    // Fixed-length padding byte.
    public native void set_re_pad(int re_pad);

    // Source file name.
    public native void set_re_source(String re_source);

    // returns a DbBtreeStat or DbHashStat
    public native Object stat(int flags)
         throws DbException;

    public native void sync(int flags)
         throws DbException;

    public native void upgrade(String name, int flags)
         throws DbException;

    protected native void finalize()
         throws Throwable;

    ////////////////////////////////////////////////////////////////
    //
    // private data
    //
    private long private_info_ = 0;
    private DbEnv dbenv_ = null;
    private DbFeedback feedback_ = null;

    private static boolean already_loaded_ = false;

    public static void load_db()
    {
        if (already_loaded_)
            return;

        // An alternate library name can be specified via a property.
        //
        String overrideLibname = System.getProperty("sleepycat.db.libname");
        if (overrideLibname != null) {
            System.loadLibrary(overrideLibname);
        }
        else {
            String os = System.getProperty("os.name");
            if (os != null && os.startsWith("Windows")) {
                // called libdb_java30.dll on Win/*
                System.loadLibrary("libdb_java" +
                                   DbConstants.DB_VERSION_MAJOR +
                                   DbConstants.DB_VERSION_MINOR);
            }
            else {
                // called libdb_java.so on UNIX
                System.loadLibrary("db_java");
            }
        }

        already_loaded_ = true;
    }

    static private void check_constant(int c1, int c2)
    {
        if (c1 != c2) {
            System.err.println("Db: constant mismatch");
            System.exit(1);
        }
    }

    static {
        Db.load_db();

        // Note: constant values are stored in DbConstants, which
        // is automatically generated.  Initializing constants in
        // static code insulates users from the possibility of
        // changing constants.
        //
        DB_CXX_NO_EXCEPTIONS = DbConstants.DB_CXX_NO_EXCEPTIONS;
        DB_XA_CREATE = DbConstants.DB_XA_CREATE;

        DB_CREATE = DbConstants.DB_CREATE;
        DB_NOMMAP = DbConstants.DB_NOMMAP;
        DB_THREAD = DbConstants.DB_THREAD;

        DB_LOCKDOWN = DbConstants.DB_LOCKDOWN;
        DB_PRIVATE = DbConstants.DB_PRIVATE;
        DB_TXN_SYNC = DbConstants.DB_TXN_SYNC;
        DB_TXN_NOWAIT = DbConstants.DB_TXN_NOWAIT;

        DB_EXCL = DbConstants.DB_EXCL;
        DB_RDONLY = DbConstants.DB_RDONLY;
        DB_TRUNCATE = DbConstants.DB_TRUNCATE;
        DB_UPGRADE = DbConstants.DB_UPGRADE;

        // These constants are not assigned, but rather checked.
        // Having initialized constants for these values allows
        // them to be used as case values in switch statements.
        //
        check_constant(DB_INCOMPLETE, DbConstants.DB_INCOMPLETE);
        check_constant(DB_KEYEMPTY, DbConstants.DB_KEYEMPTY);
        check_constant(DB_KEYEXIST, DbConstants.DB_KEYEXIST);
        check_constant(DB_LOCK_DEADLOCK, DbConstants.DB_LOCK_DEADLOCK);
        check_constant(DB_LOCK_NOTGRANTED, DbConstants.DB_LOCK_NOTGRANTED);
        check_constant(DB_NOTFOUND, DbConstants.DB_NOTFOUND);
        check_constant(DB_OLD_VERSION, DbConstants.DB_OLD_VERSION);
        check_constant(DB_RUNRECOVERY, DbConstants.DB_RUNRECOVERY);

        DB_FORCE = DbConstants.DB_FORCE;
        DB_INIT_CDB = DbConstants.DB_INIT_CDB;
        DB_INIT_LOCK = DbConstants.DB_INIT_LOCK;
        DB_INIT_LOG = DbConstants.DB_INIT_LOG;
        DB_INIT_MPOOL = DbConstants.DB_INIT_MPOOL;
        DB_INIT_TXN = DbConstants.DB_INIT_TXN;
        DB_RECOVER = DbConstants.DB_RECOVER;
        DB_RECOVER_FATAL = DbConstants.DB_RECOVER_FATAL;
        DB_SYSTEM_MEM = DbConstants.DB_SYSTEM_MEM;
        DB_TXN_NOSYNC = DbConstants.DB_TXN_NOSYNC;
        DB_USE_ENVIRON = DbConstants.DB_USE_ENVIRON;
        DB_USE_ENVIRON_ROOT = DbConstants.DB_USE_ENVIRON_ROOT;

        DB_VERB_CHKPOINT = DbConstants.DB_VERB_CHKPOINT;
        DB_VERB_DEADLOCK = DbConstants.DB_VERB_DEADLOCK;
        DB_VERB_RECOVERY = DbConstants.DB_VERB_RECOVERY;
        DB_VERB_WAITSFOR = DbConstants.DB_VERB_WAITSFOR;

        DB_LOCK_NORUN = DbConstants.DB_LOCK_NORUN;
        DB_LOCK_DEFAULT = DbConstants.DB_LOCK_DEFAULT;
        DB_LOCK_OLDEST = DbConstants.DB_LOCK_OLDEST;
        DB_LOCK_RANDOM = DbConstants.DB_LOCK_RANDOM;
        DB_LOCK_YOUNGEST = DbConstants.DB_LOCK_YOUNGEST;

        DB_DUP = DbConstants.DB_DUP;
        DB_DUPSORT = DbConstants.DB_DUPSORT;
        DB_RECNUM = DbConstants.DB_RECNUM;
        DB_RENUMBER = DbConstants.DB_RENUMBER;
        DB_REVSPLITOFF = DbConstants.DB_REVSPLITOFF;
        DB_SNAPSHOT = DbConstants.DB_SNAPSHOT;

        DB_LOCK_NOWAIT = DbConstants.DB_LOCK_NOWAIT;
        DB_LOCK_CONFLICT = DbConstants.DB_LOCK_CONFLICT;

        DB_LOCK_RW_N = DbConstants.DB_LOCK_RW_N;

        DB_ARCH_ABS = DbConstants.DB_ARCH_ABS;
        DB_ARCH_DATA = DbConstants.DB_ARCH_DATA;
        DB_ARCH_LOG = DbConstants.DB_ARCH_LOG;

        DB_AFTER = DbConstants.DB_AFTER;
        DB_APPEND = DbConstants.DB_APPEND;
        DB_BEFORE = DbConstants.DB_BEFORE;
        DB_CHECKPOINT = DbConstants.DB_CHECKPOINT;
        DB_CONSUME = DbConstants.DB_CONSUME;
        DB_CURLSN = DbConstants.DB_CURLSN;
        DB_CURRENT = DbConstants.DB_CURRENT;
        DB_FIRST = DbConstants.DB_FIRST;
        DB_FLUSH = DbConstants.DB_FLUSH;
        DB_GET_BOTH = DbConstants.DB_GET_BOTH;
        DB_GET_RECNO = DbConstants.DB_GET_RECNO;
        DB_JOIN_ITEM = DbConstants.DB_JOIN_ITEM;
        DB_KEYFIRST = DbConstants.DB_KEYFIRST;
        DB_KEYLAST = DbConstants.DB_KEYLAST;
        DB_LAST = DbConstants.DB_LAST;
        DB_NEXT = DbConstants.DB_NEXT;
        DB_NEXT_DUP = DbConstants.DB_NEXT_DUP;
        DB_NEXT_NODUP = DbConstants.DB_NEXT_NODUP;
        DB_NOOVERWRITE = DbConstants.DB_NOOVERWRITE;
        DB_NOSYNC = DbConstants.DB_NOSYNC;
        DB_POSITION = DbConstants.DB_POSITION;
        DB_PREV = DbConstants.DB_PREV;
        DB_RECORDCOUNT = DbConstants.DB_RECORDCOUNT;
        DB_SET = DbConstants.DB_SET;
        DB_SET_RANGE = DbConstants.DB_SET_RANGE;
        DB_SET_RECNO = DbConstants.DB_SET_RECNO;
        DB_WRITECURSOR = DbConstants.DB_WRITECURSOR;
        DB_RMW = DbConstants.DB_RMW;

        DB_DBT_MALLOC = DbConstants.DB_DBT_MALLOC;
        DB_DBT_PARTIAL = DbConstants.DB_DBT_PARTIAL;
        DB_DBT_REALLOC = DbConstants.DB_DBT_REALLOC;
        DB_DBT_USERMEM = DbConstants.DB_DBT_USERMEM;
    }
}

// end of Db.java

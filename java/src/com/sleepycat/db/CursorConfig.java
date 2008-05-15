/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: CursorConfig.java,v 12.9 2008/01/17 05:04:53 mjc Exp $
 */

package com.sleepycat.db;

import com.sleepycat.db.internal.DbConstants;
import com.sleepycat.db.internal.Db;
import com.sleepycat.db.internal.Dbc;
import com.sleepycat.db.internal.DbTxn;

/**
Specify the attributes of database cursor.  An instance created with the
default constructor is initialized with the system's default settings.
*/
public class CursorConfig implements Cloneable {
    /**
    Default configuration used if null is passed to methods that create a
    cursor.
    */
    public static final CursorConfig DEFAULT = new CursorConfig();

    /**
        A convenience instance to configure read operations performed by the
    cursor to return modified but not yet committed data.
    */
    public static final CursorConfig READ_UNCOMMITTED = new CursorConfig();
    static { READ_UNCOMMITTED.setReadUncommitted(true); }

    /**
        A convenience instance to configure a cursor for read committed isolation.
    <p>
    This ensures the stability of the current data item read by the
    cursor but permits data read by this cursor to be modified or
    deleted prior to the commit of the transaction.
    */
    public static final CursorConfig READ_COMMITTED = new CursorConfig();
    static { READ_COMMITTED.setReadCommitted(true); }

    /**
    A convenience instance to specify the Concurrent Data Store environment
    cursor will be used to update the database.
    <p>
    The underlying Berkeley DB database environment must have been
    configured as a Concurrent Data Store environment.
    */
    public static final CursorConfig WRITECURSOR = new CursorConfig();
    static { WRITECURSOR.setWriteCursor(true); }

    /**
        A convenience instance to configure read operations performed by the
    cursor to return modified but not yet committed data.
        <p>
    @deprecated This has been replaced by {@link #READ_UNCOMMITTED} to conform to ANSI
    database isolation terminology.
    */
    public static final CursorConfig DIRTY_READ = READ_UNCOMMITTED;
    /**
        A convenience instance to configure a cursor for read committed isolation.
    <p>
    This ensures the stability of the current data item read by the
    cursor but permits data read by this cursor to be modified or
    deleted prior to the commit of the transaction.
        <p>
    @deprecated This has been replaced by {@link #READ_COMMITTED} to conform to ANSI
    database isolation terminology.
    */
    public static final CursorConfig DEGREE_2 = READ_COMMITTED;

    private boolean readUncommitted = false;
    private boolean readCommitted = false;
    private boolean writeCursor = false;

    /**
    An instance created using the default constructor is initialized with
    the system's default settings.
    */
    public CursorConfig() {
    }

    /* package */
    static CursorConfig checkNull(CursorConfig config) {
        return (config == null) ? DEFAULT : config;
    }

    /**
        Configure the cursor for read committed isolation.
    <p>
    This ensures the stability of the current data item read by the
    cursor but permits data read by this cursor to be modified or
    deleted prior to the commit of the transaction.
    <p>
    @param readCommitted
    If true, configure the cursor for read committed isolation.
    */
    public void setReadCommitted(final boolean readCommitted) {
        this.readCommitted = readCommitted;
    }

    /**
        Return if the cursor is configured for read committed isolation.
    <p>
    @return
    If the cursor is configured for read committed isolation.
    */
    public boolean getReadCommitted() {
        return readCommitted;
    }

    /**
        Configure the cursor for read committed isolation.
    <p>
    This ensures the stability of the current data item read by the
    cursor but permits data read by this cursor to be modified or
    deleted prior to the commit of the transaction.
    <p>
    @param degree2
    If true, configure the cursor for read committed isolation.
        <p>
    @deprecated This has been replaced by {@link #setReadCommitted} to conform to ANSI
    database isolation terminology.
    */
    public void setDegree2(final boolean degree2) {
        setReadCommitted(degree2);
    }

    /**
        Return if the cursor is configured for read committed isolation.
    <p>
    @return
    If the cursor is configured for read committed isolation.
        <p>
    @deprecated This has been replaced by {@link #getReadCommitted} to conform to ANSI
    database isolation terminology.
    */
    public boolean getDegree2() {
        return getReadCommitted();
    }

    /**
        Configure read operations performed by the cursor to return modified
    but not yet committed data.
    <p>
    @param readUncommitted
    If true, configure read operations performed by the cursor to return
    modified but not yet committed data.
    */
    public void setReadUncommitted(final boolean readUncommitted) {
        this.readUncommitted = readUncommitted;
    }

    /**
        Return if read operations performed by the cursor are configured to
    return modified but not yet committed data.
    <p>
    @return
    If read operations performed by the cursor are configured to return
    modified but not yet committed data.
    */
    public boolean getReadUncommitted() {
        return readUncommitted;
    }

    /**
        Configure read operations performed by the cursor to return modified
    but not yet committed data.
    <p>
    @param dirtyRead
    If true, configure read operations performed by the cursor to return
    modified but not yet committed data.
        <p>
    @deprecated This has been replaced by {@link #setReadUncommitted} to conform to ANSI
    database isolation terminology.
    */
    public void setDirtyRead(final boolean dirtyRead) {
        setReadUncommitted(dirtyRead);
    }

    /**
        Return if read operations performed by the cursor are configured to
    return modified but not yet committed data.
    <p>
    @return
    If read operations performed by the cursor are configured to return
    modified but not yet committed data.
        <p>
    @deprecated This has been replaced by {@link #getReadUncommitted} to conform to ANSI
    database isolation terminology.
    */
    public boolean getDirtyRead() {
        return getReadUncommitted();
    }

    /**
    Specify the Concurrent Data Store environment cursor will be used to
    update the database.
    <p>
    @param writeCursor
    If true, specify the Concurrent Data Store environment cursor will be
    used to update the database.
    */
    public void setWriteCursor(final boolean writeCursor) {
        this.writeCursor = writeCursor;
    }

    /**
    Return if the Concurrent Data Store environment cursor will be used to
    update the database.
    <p>
    @return
    If the Concurrent Data Store environment cursor will be used to update
    the database.
    */
    public boolean getWriteCursor() {
        return writeCursor;
    }

    /* package */
    Dbc openCursor(final Db db, final DbTxn txn)
        throws DatabaseException {

        int flags = 0;
        flags |= readUncommitted ? DbConstants.DB_READ_UNCOMMITTED : 0;
        flags |= readCommitted ? DbConstants.DB_READ_COMMITTED : 0;
        flags |= writeCursor ? DbConstants.DB_WRITECURSOR : 0;
        return db.cursor(txn, flags);
    }
}

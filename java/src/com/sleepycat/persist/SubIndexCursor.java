/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: SubIndexCursor.java,v 1.1 2008/02/07 17:12:26 mark Exp $
 */

package com.sleepycat.persist;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.LockMode;
import com.sleepycat.util.keyrange.RangeCursor;

/**
 * The cursor for a SubIndex treats Dup and NoDup operations specially because
 * the SubIndex never has duplicates -- the keys are primary keys.  So a
 * next/prevDup operation always returns null, and a next/prevNoDup operation
 * actually does next/prev.
 *
 * @author Mark Hayes
 */
class SubIndexCursor<V> extends BasicCursor<V> {

    SubIndexCursor(RangeCursor cursor, ValueAdapter<V> adapter) {
        super(cursor, adapter, false/*updateAllowed*/);
    }

    public EntityCursor<V> dup()
        throws DatabaseException {

        return new SubIndexCursor<V>(cursor.dup(true), adapter);
    }

    public V nextDup(LockMode lockMode)
        throws DatabaseException {

        checkInitialized();
        return null;
    }

    public V nextNoDup(LockMode lockMode)
        throws DatabaseException {

        return returnValue(cursor.getNext(key, pkey, data, lockMode));
    }

    public V prevDup(LockMode lockMode)
        throws DatabaseException {

        checkInitialized();
        return null;
    }

    public V prevNoDup(LockMode lockMode)
        throws DatabaseException {

        return returnValue(cursor.getPrev(key, pkey, data, lockMode));
    }
}

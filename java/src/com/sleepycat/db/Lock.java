/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2001-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: Lock.java,v 12.1 2005/06/16 20:23:01 bostic Exp $
 */

package com.sleepycat.db;

import com.sleepycat.db.internal.DbLock;

public final class Lock {
    private DbLock dbLock;

    private Lock(final DbLock dblock) {
        this.dbLock = dbLock;
        dbLock.wrapper = this;
    }

    /* package */
    static Lock wrap(final DbLock dblock) {
        return (dblock == null) ? null : new Lock(dblock);
    }

    /* package */
    DbLock unwrap() {
        return dbLock;
    }
}

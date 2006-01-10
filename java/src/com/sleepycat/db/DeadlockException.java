/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: DeadlockException.java,v 12.1 2005/06/16 20:23:00 bostic Exp $
 */
package com.sleepycat.db;

import com.sleepycat.db.internal.DbEnv;

public class DeadlockException extends DatabaseException {
    protected DeadlockException(final String s,
                                final int errno,
                                final DbEnv dbenv) {
        super(s, errno, dbenv);
    }
}

/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997,2008 Oracle.  All rights reserved.
 *
 * $Id: ReplicationLockoutException.java,v 12.8 2008/01/17 05:04:53 mjc Exp $
 */
package com.sleepycat.db;

import com.sleepycat.db.internal.DbEnv;

/**
Thrown when an operation was blocked by client/master synchronization.
*/
public class ReplicationLockoutException extends DatabaseException {
    /* package */ ReplicationLockoutException(final String s,
                                   final int errno,
                                   final DbEnv dbenv) {
        super(s, errno, dbenv);
    }
}

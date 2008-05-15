/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997,2008 Oracle.  All rights reserved.
 *
 * $Id: ReplicationSiteUnavailableException.java,v 12.9 2008/01/17 05:04:53 mjc Exp $
 */
package com.sleepycat.db;

import com.sleepycat.db.internal.DbEnv;

/**
Thrown if replication group was unable to elect a master, or was unable to
complete the election in the specified timeout period.
**/
public class ReplicationSiteUnavailableException extends DatabaseException {
    /* package */ ReplicationSiteUnavailableException(final String s,
                                   final int errno,
                                   final DbEnv dbenv) {
        super(s, errno, dbenv);
    }
}

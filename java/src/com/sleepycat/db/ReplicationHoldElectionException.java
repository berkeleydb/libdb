/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997,2008 Oracle.  All rights reserved.
 *
 * $Id: ReplicationHoldElectionException.java,v 12.9 2008/01/17 05:04:53 mjc Exp $
 */
package com.sleepycat.db;

import com.sleepycat.db.internal.DbEnv;

/**
An election is needed.  The application should call for an election by calling
{@link com.sleepycat.db.Environment#electReplicationMaster Environment.electReplicationMaster}.
*/
public class ReplicationHoldElectionException extends DatabaseException {
    /* package */ ReplicationHoldElectionException(final String s,
                                   final int errno,
                                   final DbEnv dbenv) {
        super(s, errno, dbenv);
    }
}

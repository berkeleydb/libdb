/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997,2008 Oracle.  All rights reserved.
 *
 * $Id: ReplicationDuplicateMasterException.java,v 12.9 2008/01/17 05:04:53 mjc Exp $
 */
package com.sleepycat.db;

import com.sleepycat.db.internal.DbEnv;

/**
The replication group has more than one master.  The application should
reconfigure itself as a client by calling the
{@link com.sleepycat.db.Environment#startReplication Environment.startReplication} method, and then call for an election by
calling {@link com.sleepycat.db.Environment#electReplicationMaster Environment.electReplicationMaster}.
*/
public class ReplicationDuplicateMasterException extends DatabaseException {
    /* package */ ReplicationDuplicateMasterException(final String s,
                                   final int errno,
                                   final DbEnv dbenv) {
        super(s, errno, dbenv);
    }
}

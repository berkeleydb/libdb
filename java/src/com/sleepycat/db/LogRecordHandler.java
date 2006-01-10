/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: LogRecordHandler.java,v 12.1 2005/06/16 20:23:02 bostic Exp $
 */
package com.sleepycat.db;

public interface LogRecordHandler {
    int handleLogRecord(Environment dbenv,
                        DatabaseEntry logRecord,
                        LogSequenceNumber lsn,
                        RecoveryOperation operation);
}

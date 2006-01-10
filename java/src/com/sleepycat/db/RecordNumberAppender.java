/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: RecordNumberAppender.java,v 12.1 2005/06/16 20:23:03 bostic Exp $
 */
package com.sleepycat.db;

public interface RecordNumberAppender {
    void appendRecordNumber(Database db, DatabaseEntry data, int recno)
        throws DatabaseException;
}

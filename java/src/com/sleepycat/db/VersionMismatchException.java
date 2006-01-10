/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: VersionMismatchException.java,v 1.2 2005/06/16 20:23:04 bostic Exp $
 */
package com.sleepycat.db;

import com.sleepycat.db.internal.DbEnv;

public class VersionMismatchException extends DatabaseException {
    protected VersionMismatchException(final String s,
                                   final int errno,
                                   final DbEnv dbenv) {
        super(s, errno, dbenv);
    }
}

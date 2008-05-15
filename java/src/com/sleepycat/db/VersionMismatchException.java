/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997,2008 Oracle.  All rights reserved.
 *
 * $Id: VersionMismatchException.java,v 1.9 2008/01/17 05:04:53 mjc Exp $
 */
package com.sleepycat.db;

import com.sleepycat.db.internal.DbEnv;

/**
Thrown if the version of the Berkeley DB library doesn't match the version that created
the database environment.
*/
public class VersionMismatchException extends DatabaseException {
    /* package */ VersionMismatchException(final String s,
                                   final int errno,
                                   final DbEnv dbenv) {
        super(s, errno, dbenv);
    }
}

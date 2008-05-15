/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2001,2008 Oracle.  All rights reserved.
 *
 * $Id: BtreePrefixCalculator.java,v 12.7 2008/01/17 05:04:53 mjc Exp $
 */

package com.sleepycat.db;

/**
An interface specifying how Btree prefixes should be calculated.
*/
public interface BtreePrefixCalculator {
    /**
    The application-specific Btree prefix callback.
    <p>
    @param db
    The enclosing database handle.
    @param dbt1
    A database entry representing a database key.
    @param dbt2
    A database entry representing a database key.
    */
    int prefix(Database db, DatabaseEntry dbt1, DatabaseEntry dbt2);
}

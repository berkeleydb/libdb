/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000,2008 Oracle.  All rights reserved.
 *
 * $Id: PersistTestUtils.java,v 1.3 2008/02/18 14:48:11 mark Exp $
 */
package com.sleepycat.persist.test;

import java.io.FileNotFoundException;

import junit.framework.TestCase;

import com.sleepycat.compat.DbCompat;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.Environment;

class PersistTestUtils {

    /**
     * Asserts than a database expectExists or does not exist. If keyName is
     * null, checks an entity database.  If keyName is non-null, checks a
     * secondary database.
     */
    static void assertDbExists(boolean expectExists,
                               Environment env,
                               String storeName,
                               String entityClassName,
                               String keyName) {
        String fileName;
        String dbName;
        if (DbCompat.SEPARATE_DATABASE_FILES) {
            fileName = storeName + '-' + entityClassName;
            if (keyName != null) {
                fileName += "-" + keyName;
            }
            dbName = null;
        } else {
            fileName = null;
            dbName = "persist#" + storeName + '#' + entityClassName;
            if (keyName != null) {
                dbName += "#" + keyName;
            }
        }
        boolean exists;
        try {
            DatabaseConfig config = new DatabaseConfig();
            config.setReadOnly(true);
            Database db = DbCompat.openDatabase
                (env, null/*txn*/, fileName, dbName, config);
            db.close();
            exists = true;
        } catch (FileNotFoundException e) {
            exists = false;
        } catch (Exception e) {
            /* Any other exception means the DB does exist. */
            exists = true;
        }
        if (expectExists != exists) {
            TestCase.fail
                ((expectExists ? "Does not exist: " : "Does exist: ") +
                 dbName);
        }
    }
}
